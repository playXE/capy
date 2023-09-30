use mmtk::{util::ObjectReference, AllocationSemantics, MutatorContext};
use std::mem::{size_of, transmute};
use std::ptr::null_mut;

use crate::compiler::tree_il::IForm;
use crate::runtime::hashtable::{lookup_hashtable_size, WeakHashTableRec, WeakHashtable};
use crate::runtime::object::{ScmTuple, ScmWeakMapping, scm_bytevector_set_mapping};
use crate::{
    compiler::{expand::define_syntax, P},
    gc_protect,
    runtime::{
        environment::ScmEnvironment,
        object::{Header, ScmCellHeader, ScmCellRef, ScmSyntaxExpander, TypeId},
    },
    runtime::{
        hashtable::{
            dummy_equiv, dummy_hash, equal_hash, equal_hash_equiv, eqv_hash, eqv_hash_equiv,
            string_hash, string_hash_equiv, CompareProc, HashProc, HashTableRec, HashTableType,
            ScmHashTable,
        },
        object::{
            ScmBox, ScmBytevector, ScmGloc, ScmPair, ScmProgram, ScmString, ScmSymbol, ScmVector,
        },
        value::Value,
    },
    utils::round_up,
};

use super::{
    sync::mutex::{Mutex, RawMutex},
    thread::Thread,
};

impl Thread {
    pub fn make_cons<const IMMORTAL: bool>(&mut self, car: Value, cdr: Value) -> Value {
        unsafe {
            let semantics = if IMMORTAL {
                AllocationSemantics::Immortal
            } else {
                AllocationSemantics::Default
            };
            let mutator = self.mutator.assume_init_mut();
            let mem = mutator.alloc(size_of::<ScmPair>(), size_of::<usize>(), 0, semantics);

            mem.store::<ScmPair>(ScmPair {
                header: ScmCellHeader {
                    as_header: Header {
                        type_id: TypeId::Pair,
                        pad: [0; 4],
                        flags: 0,
                    },
                },
                car,
                cdr,
            });
            let reference = transmute::<_, ObjectReference>(mem);
            mutator.post_alloc(reference, size_of::<ScmPair>(), semantics);

            ScmCellRef(transmute(reference)).into()
        }
    }

    pub fn make_vector<const IMMORTAL: bool>(&mut self, len: usize, fill: Value) -> Value {
        let size = round_up(size_of::<ScmVector>() + len * size_of::<Value>(), 8, 0);

        unsafe {
            let semantics = if IMMORTAL {
                AllocationSemantics::Immortal
            } else {
                AllocationSemantics::Default
            };
            let mutator = self.mutator();
            let mem = mutator.alloc(size, size_of::<usize>(), 0, semantics);

            mem.store(ScmVector {
                header: ScmCellHeader {
                    as_header: Header {
                        type_id: TypeId::Vector,
                        pad: [0; 4],
                        flags: 0,
                    },
                },
                length: len,
                values: [],
            });
            let mut cursor = (*mem.to_mut_ptr::<ScmVector>()).values.as_mut_ptr();
            let end = cursor.add(len);

            while cursor < end {
                cursor.write(fill);
                cursor = cursor.add(1);
            }

            let reference = transmute::<_, ObjectReference>(mem);
            mutator.post_alloc(reference, size, semantics);

            Value::encode_object_value(ScmCellRef(transmute(reference)))
        }
    }

    pub fn make_tuple<const IMMORTAL: bool>(&mut self, len: usize, fill: Value) -> Value {
        let size = round_up(size_of::<ScmTuple>() + len * size_of::<Value>(), 8, 0);

        unsafe {
            let semantics = if IMMORTAL {
                AllocationSemantics::Immortal
            } else {
                AllocationSemantics::Default
            };
            let mutator = self.mutator();
            let mem = mutator.alloc(size, size_of::<usize>(), 0, semantics);

            mem.store(ScmTuple {
                header: ScmCellHeader {
                    as_header: Header {
                        type_id: TypeId::Tuple,
                        pad: [0; 4],
                        flags: 0,
                    },
                },
                length: len,
                values: [],
            });
            let mut cursor = (*mem.to_mut_ptr::<ScmTuple>()).values.as_mut_ptr();
            let end = cursor.add(len);

            while cursor < end {
                cursor.write(fill);
                cursor = cursor.add(1);
            }

            let reference = transmute::<_, ObjectReference>(mem);
            mutator.post_alloc(reference, size, semantics);

            Value::encode_object_value(ScmCellRef(transmute(reference)))
        }
    }

    pub fn make_bytevector<const IMMORTAL: bool>(&mut self, n: usize, init: u8) -> Value {
        let size = round_up(size_of::<ScmBytevector>() + n, 8, 0);
        let semantics = if IMMORTAL {
            AllocationSemantics::Immortal
        } else {
            AllocationSemantics::Default
        };
        unsafe {
            let mutator = self.mutator.assume_init_mut();
            let mem = mutator.alloc(size, size_of::<usize>(), 0, semantics);

            mem.store(ScmBytevector {
                header: ScmCellHeader {
                    as_header: Header {
                        type_id: TypeId::Bytevector,
                        pad: [0; 4],
                        flags: 0,
                    },
                },
                elems: null_mut(),
                length: n,
                data: [],
            });

            let data = (*mem.to_mut_ptr::<ScmBytevector>()).data.as_mut_ptr();
            (*mem.to_mut_ptr::<ScmBytevector>()).elems = data;
            data.write_bytes(init, n);
            let reference = transmute::<_, ObjectReference>(mem);
            mutator.post_alloc(reference, size, semantics);

            Value::encode_object_value(ScmCellRef(transmute(reference)))
        }
    }

    pub fn make_bytevector_from_slice<const IMMORTAL: bool>(&mut self, slice: &[u8]) -> Value {
        let size = round_up(size_of::<ScmBytevector>() + slice.len(), 8, 0);
        let semantics = if IMMORTAL {
            AllocationSemantics::Immortal
        } else {
            AllocationSemantics::Default
        };
        unsafe {
            let mutator = self.mutator.assume_init_mut();
            let mem = mutator.alloc(size, size_of::<usize>(), 0, semantics);

            mem.store(ScmBytevector {
                header: ScmCellHeader {
                    as_header: Header {
                        type_id: TypeId::Bytevector,
                        pad: [0; 4],
                        flags: 0,
                    },
                },
                elems: null_mut(),
                length: slice.len(),
                data: [],
            });

            let addr = mem.to_mut_ptr::<ScmBytevector>();

            let data = (*mem.to_mut_ptr::<ScmBytevector>()).data.as_mut_ptr();
            (*addr).elems = data;
            data.copy_from_nonoverlapping(slice.as_ptr(), slice.len());
            let reference = transmute::<_, ObjectReference>(mem);
            mutator.post_alloc(reference, size, semantics);

            Value::encode_object_value(ScmCellRef(transmute(reference)))
        }
    }

    pub fn make_bytevector_mapping<const IMMORTAL: bool>(
        &mut self,
        addr: *mut u8,
        len: usize,
    ) -> Value {
        let size = round_up(size_of::<ScmBytevector>(), 8, 0);
        let semantics = if IMMORTAL {
            AllocationSemantics::Immortal
        } else {
            AllocationSemantics::Default
        };
        unsafe {
            let mutator = self.mutator.assume_init_mut();
            let mem = mutator.alloc(size, size_of::<usize>(), 0, semantics);

            mem.store(ScmBytevector {
                header: ScmCellHeader {
                    as_header: Header {
                        type_id: TypeId::Bytevector,
                        pad: [0; 4],
                        flags: 0,
                    },
                },
                elems: addr,
                length: len,
                data: [],
            });

            let addr = mem.to_mut_ptr::<ScmBytevector>();

            let data = (*mem.to_mut_ptr::<ScmBytevector>()).data.as_mut_ptr();
            (*addr).elems = data;

            let reference = transmute::<_, ObjectReference>(mem);
            mutator.post_alloc(reference, size, semantics);

            let val = Value::encode_object_value(ScmCellRef(transmute(reference)));
            scm_bytevector_set_mapping(val);
            val 
        }
    }

    pub fn make_string<const IMMORTAL: bool>(&mut self, str: &str) -> Value {
        let size = round_up(size_of::<ScmString>() + str.len() + 1, 8, 0);
        let semantics = if IMMORTAL {
            AllocationSemantics::Immortal
        } else {
            AllocationSemantics::Default
        };
        unsafe {
            let mutator = self.mutator.assume_init_mut();
            let mem = mutator.alloc(size, size_of::<usize>(), 0, semantics);

            mem.store(ScmString {
                header: ScmCellHeader {
                    as_header: Header {
                        type_id: TypeId::String,
                        pad: [0; 4],
                        flags: 0,
                    },
                },
                length: str.len(),
                name: [],
            });

            let name = (*mem.to_mut_ptr::<ScmString>()).name.as_mut_ptr();
            name.copy_from_nonoverlapping(str.as_ptr(), str.len());
            *name.add(str.len()) = 0;

            let reference = transmute::<_, ObjectReference>(mem);
            mutator.post_alloc(reference, size, semantics);

            Value::encode_object_value(ScmCellRef(transmute(reference)))
        }
    }

    pub fn make_program<const IMMORTAL: bool>(
        &mut self,
        vcode: *const u8,
        num_free_vars: u32,
    ) -> Value {
        let size = round_up(
            size_of::<ScmProgram>() + size_of::<Value>() * num_free_vars as usize,
            8,
            0,
        );
        let semantics = if IMMORTAL {
            AllocationSemantics::Immortal
        } else {
            AllocationSemantics::Default
        };
        unsafe {
            let mutator = self.mutator.assume_init_mut();
            let mem = mutator.alloc(size, size_of::<usize>(), 0, semantics);

            mem.store(ScmProgram {
                header: ScmCellHeader {
                    as_header: Header {
                        type_id: TypeId::Program,
                        pad: [0; 4],
                        flags: 0,
                    },
                },
                constants: Value::encode_undefined_value(),
                vcode: vcode as *mut u8,
                nfree: num_free_vars as _,
                free: [],
            });

            let mut free = (*mem.to_mut_ptr::<ScmProgram>()).free.as_mut_ptr();
            let end = free.add(num_free_vars as usize);

            while free < end {
                free.write(Value::encode_undefined_value());
                free = free.add(1);
            }

            let reference = transmute::<_, ObjectReference>(mem);
            mutator.post_alloc(reference, size, semantics);

            Value::encode_object_value(ScmCellRef(transmute(reference)))
        }
    }

    pub fn make_symbol(&mut self, str: &str, gensym: bool, interned: bool) -> Value {
        let size = round_up(size_of::<ScmSymbol>() + str.len() + 1, 8, 0);

        unsafe {
            let mutator = self.mutator.assume_init_mut();
            let mem = mutator.alloc(size, size_of::<usize>(), 0, AllocationSemantics::Immortal);

            /*mem.store::<ScmCellHeader>(ScmCellHeader {
                as_header: Header {
                    type_id: TypeId::Symbol,
                    pad: [0; 4],
                    flags: 0,
                },
            });

            mem.add(size_of::<ScmCellHeader>())
                .store(Value::encode_int32(str.len() as i32));
            let strstart = mem
                .add(size_of::<ScmCellHeader>() + size_of::<Value>())
                .to_mut_ptr::<u8>();
            strstart.copy_from_nonoverlapping(str.as_bytes().as_ptr(), str.len());
            strstart.add(str.len()).write_bytes(0, 1);
            let reference = ObjectReference::from_raw_address(mem);*/

            mem.store(ScmSymbol {
                header: ScmCellHeader {
                    as_header: Header {
                        type_id: TypeId::Symbol,
                        pad: [0; 4],
                        flags: 0,
                    },
                },
                length: str.len() as u32,
                gensym,
                interned,
                name: [],
            });
            let addr = mem.to_mut_ptr::<ScmSymbol>();
            let name = (*addr).name.as_mut_ptr();
            name.copy_from_nonoverlapping(str.as_bytes().as_ptr(), str.len());
            name.add(str.len()).write(0);
            let reference = transmute::<_, ObjectReference>(mem);
            mutator.post_alloc(reference, size, AllocationSemantics::Default);

            Value::encode_object_value(ScmCellRef(transmute(reference)))
        }
    }

    pub fn make_gloc(&mut self, name: Value, value: Value) -> Value {
        let size = round_up(size_of::<ScmGloc>(), 8, 0);

        unsafe {
            let mutator = self.mutator.assume_init_mut();
            let mem = mutator.alloc(size, size_of::<usize>(), 0, AllocationSemantics::Default);
            mem.store(ScmGloc {
                header: ScmCellHeader {
                    as_header: Header {
                        type_id: TypeId::GLOC,
                        pad: [0; 4],
                        flags: 0,
                    },
                },
                name,
                value,
            });

            let reference = transmute::<_, ObjectReference>(mem);
            mutator.post_alloc(reference, size, AllocationSemantics::Default);

            Value::encode_object_value(ScmCellRef(transmute(reference)))
        }
    }

    pub fn make_box(&mut self) -> Value {
        let size = round_up(size_of::<ScmBox>(), 8, 0);

        unsafe {
            let mutator = self.mutator.assume_init_mut();
            let mem = mutator.alloc(size, size_of::<usize>(), 0, AllocationSemantics::Default);
            mem.store(ScmBox {
                header: ScmCellHeader {
                    as_header: Header {
                        type_id: TypeId::Box,
                        pad: [0; 4],
                        flags: 0,
                    },
                },
                value: Value::encode_null_value(),
            });

            let reference = transmute::<_, ObjectReference>(mem);
            mutator.post_alloc(reference, size, AllocationSemantics::Default);

            Value::encode_object_value(ScmCellRef(transmute(reference)))
        }
    }

    pub fn make_hashtable(&mut self, n: u32, typ: HashTableType) -> Value {
        let n = lookup_hashtable_size(n);
        let size = round_up(size_of::<ScmHashTable>(), 8, 0);
        let datum_size = size_of::<HashTableRec>() + size_of::<Value>() * ((n + n) as usize - 1);

        let (hash, equiv): (HashProc, CompareProc) = match typ {
            HashTableType::Eq => (dummy_hash, dummy_equiv),

            HashTableType::Eqv => (eqv_hash, eqv_hash_equiv),

            HashTableType::Equal => (equal_hash, equal_hash_equiv),

            HashTableType::String => (string_hash, string_hash_equiv),

            _ => (dummy_hash, dummy_equiv),
        };
        unsafe {
            let mutator = self.mutator();
            let semantics = if datum_size >= 16*1024 {
                AllocationSemantics::Los
            } else {
                AllocationSemantics::Default
            };
            let datum_addr = mutator.alloc(
                datum_size,
                size_of::<usize>(),
                0,
                semantics,
            );

            datum_addr.store(HashTableRec {
                capacity: n,
                live: 0,
                used: 0,
                header: ScmCellHeader {
                    as_header: Header {
                        type_id: TypeId::HashTableRec,
                        pad: [0; 4],
                        flags: 0,
                    },
                },
                elts: [Value::encode_empty_value()],
            });

            let datum_ptr = datum_addr.to_mut_ptr::<HashTableRec>();
            let elts = (*datum_ptr).elts.as_mut_ptr();
            for i in 0..(n + n) {
                elts.add(i as usize).write(Value::encode_empty_value());
            }

            mutator.post_alloc(
                transmute(datum_addr),
                datum_size,
                AllocationSemantics::Default,
            );

            let mut datum = Value::encode_object_value(transmute(datum_addr));

            let mem = gc_protect!(self => datum => self.mutator().alloc(size, size_of::<usize>(), 0, AllocationSemantics::Default));

            let hashtable = mem.to_mut_ptr::<ScmHashTable>();
            hashtable.write(ScmHashTable {
                hdr: ScmCellHeader {
                    as_header: Header {
                        type_id: TypeId::HashTable,
                        pad: [0; 4],
                        flags: 0,
                    },
                },
                lock: RawMutex::INIT,
                hash,
                equiv,
                datum: transmute(datum),
                handlers: Value::encode_bool_value(false),
                typ,
                rehash: false,
            });

            let reference = transmute::<_, ObjectReference>(mem);
            self.mutator()
                .post_alloc(reference, size, AllocationSemantics::Default);

            Value::encode_object_value(ScmCellRef(transmute(reference)))
        }
    }

    pub fn make_weak_hashtable(&mut self, n: u32) -> Value {
        let size = round_up(size_of::<WeakHashtable>(), 8, 0);
        let datum_size = round_up(
            size_of::<WeakHashTableRec>() + size_of::<Value>() * n as usize,
            8,
            0,
        );

        unsafe {
            let datum_addr = self.mutator().alloc(
                datum_size,
                size_of::<usize>(),
                0,
                AllocationSemantics::Default,
            );

            datum_addr.store(WeakHashTableRec {
                capacity: n,
                live: 0,
                used: 0,
                header: ScmCellHeader {
                    as_header: Header {
                        type_id: TypeId::WeakHashTableRec,
                        pad: [0; 4],
                        flags: 0,
                    },
                },
                elts: [Value::encode_empty_value()],
            });

            let datum_ptr = datum_addr.to_mut_ptr::<WeakHashTableRec>();
            let elts = (*datum_ptr).elts.as_mut_ptr();
            for i in 0..n {
                elts.add(i as usize).write(Value::encode_empty_value());
            }

            self.mutator().post_alloc(
                transmute(datum_addr),
                datum_size,
                AllocationSemantics::Default,
            );

            let mut datum = Value::encode_object_value(transmute(datum_addr));

            let mem = gc_protect!(self => datum => self.mutator().alloc(size, size_of::<usize>(), 0, AllocationSemantics::Default));

            let hashtable = mem.to_mut_ptr::<WeakHashtable>();
            hashtable.write(WeakHashtable {
                header: ScmCellHeader {
                    as_header: Header {
                        type_id: TypeId::WeakHashTable,
                        pad: [0; 4],
                        flags: 0,
                    },
                },
                lock: RawMutex::INIT,
                datum: transmute(datum),
            });

            let reference = transmute::<_, ObjectReference>(mem);
            self.mutator()
                .post_alloc(reference, size, AllocationSemantics::Default);

            Value::encode_object_value(ScmCellRef(transmute(reference)))
        }
    }

    pub fn make_syntax_expander(&mut self, expander: fn(Value, Value) -> P<IForm>) -> Value {
        let size = size_of::<ScmSyntaxExpander>();

        let mutator = self.mutator();

        let mem = mutator.alloc(size, size_of::<usize>(), 0, AllocationSemantics::Default);

        unsafe {
            mem.store(ScmSyntaxExpander {
                header: ScmCellHeader {
                    as_header: Header {
                        type_id: TypeId::SyntaxExpander,
                        pad: [0; 4],
                        flags: 0,
                    },
                },
                callback: expander,
            });

            let reference = transmute::<_, ObjectReference>(mem);
            mutator.post_alloc(reference, size, AllocationSemantics::Default);

            Value::encode_object_value(ScmCellRef(transmute(reference)))
        }
    }

    pub fn make_environment(&mut self, name: Value) -> Value {
        let size = round_up(size_of::<ScmEnvironment>(), 8, 0);

        let mutator = self.mutator();
        let semantics = AllocationSemantics::Default;

        let mem = mutator.alloc(size, size_of::<usize>(), 0, semantics);

        unsafe {
            mem.store(ScmEnvironment {
                header: ScmCellHeader {
                    as_header: Header {
                        type_id: TypeId::Environment,
                        pad: [0; 4],
                        flags: 0,
                    },
                },
                syntactic_environment: Mutex::new(define_syntax()),
                name,
                mutable: true,
                ht: Value::encode_null_value(),
                synenv: Value::encode_null_value(),
            });

            let reference = transmute::<_, ObjectReference>(mem);
            mutator.post_alloc(reference, size, semantics);
            let mut env = Value::encode_object_value(ScmCellRef(transmute(reference)));
            let ht = gc_protect!(self => env => self.make_hashtable(113, HashTableType::Eq));
            env.cast_as::<ScmEnvironment>().ht.assign(env, ht);
            let synenv = gc_protect!(self => env => self.make_hashtable(113, HashTableType::Eq));
            env.cast_as::<ScmEnvironment>().synenv.assign(env, synenv);
            env
        }
    }

    pub fn make_weakmapping(&mut self) -> Value {
        let size = round_up(size_of::<ScmWeakMapping>(), 8, 0);

        let mem = self
            .mutator()
            .alloc(size, 8, 0, AllocationSemantics::Default);

        unsafe {
            mem.store(ScmWeakMapping {
                key: Value::encode_bool_value(false),
                value: Value::encode_bool_value(false),
                header: ScmCellHeader::new(TypeId::WeakMapping),
            });

            let reference = ObjectReference::from_raw_address(mem);
            self.mutator()
                .post_alloc(reference, size, AllocationSemantics::Default);

            Value::encode_object_value(transmute(reference))
        }
    }
}
