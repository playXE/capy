use std::mem::{size_of, transmute};
use std::sync::atomic::{Ordering, AtomicI32};

use mmtk::{util::ObjectReference, AllocationSemantics, MutatorContext};

use crate::compiler::tree_il::IForm;
use crate::runtime::control::Winder;
use crate::runtime::hashtable::{WeakHashTableRec, WeakHashtable, lookup_hashtable_size};
use crate::runtime::object::ScmWeakMapping;
use crate::runtime::synrules::{PVRef, SyntaxPattern, SyntaxRuleBranch, SyntaxRules};
use crate::{
    compiler::{expand::define_syntax, tree_il::LVar, P},
    gc_protect,
    runtime::{
        environment::ScmEnvironment,
        object::{
            CleanerType, Header, ScmCellHeader, ScmCellRef, ScmIdentifier, ScmLVar,
            ScmSyntaxExpander, TypeId,
        },
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
                length: n,
                data: [],
            });

            let data = (*mem.to_mut_ptr::<ScmBytevector>()).data.as_mut_ptr();
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
                length: slice.len(),
                data: [],
            });

            let data = (*mem.to_mut_ptr::<ScmBytevector>()).data.as_mut_ptr();
            data.copy_from_nonoverlapping(slice.as_ptr(), slice.len());
            let reference = transmute::<_, ObjectReference>(mem);
            mutator.post_alloc(reference, size, semantics);

            Value::encode_object_value(ScmCellRef(transmute(reference)))
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

            let datum_addr = mutator.alloc(
                datum_size,
                size_of::<usize>(),
                0,
                AllocationSemantics::Default,
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

    pub fn make_identifier<const IMMORTAL: bool>(&mut self) -> Value {
        let size = size_of::<ScmIdentifier>();

        let mutator = self.mutator();
        let semantics = if IMMORTAL {
            AllocationSemantics::Immortal
        } else {
            AllocationSemantics::Default
        };
        let mem = mutator.alloc(size, size_of::<usize>(), 0, semantics);

        unsafe {
            mem.store(ScmIdentifier {
                header: ScmCellHeader {
                    as_header: Header {
                        type_id: TypeId::SyntaxExpander,
                        pad: [0; 4],
                        flags: 0,
                    },
                },
                name: Value::encode_undefined_value(),
                frames: Value::encode_null_value(),
                env: Value::encode_null_value(),
            });

            let reference = transmute::<_, ObjectReference>(mem);
            mutator.post_alloc(reference, size, semantics);

            Value::encode_object_value(ScmCellRef(transmute(reference)))
        }
    }

    pub fn make_lvar(&mut self, lvar: P<LVar>) -> Value {
        let size = size_of::<ScmIdentifier>();

        let mutator = self.mutator();
        let semantics = AllocationSemantics::Default;
        let mem = mutator.alloc(size, size_of::<usize>(), 0, semantics);

        unsafe {
            mem.store(ScmLVar {
                header: ScmCellHeader {
                    as_header: Header {
                        type_id: TypeId::LVar,
                        pad: [0; 4],
                        flags: 0,
                    },
                },
                lvar,
            });

            let reference = transmute::<_, ObjectReference>(mem);
            mutator.post_alloc(reference, size, semantics);
            self.register_cleaner(
                reference,
                CleanerType::Drop({
                    fn drop_lvar(reference: *mut ()) {
                        let lvar = unsafe { transmute::<_, *mut ScmLVar>(reference) };
                        unsafe {
                            core::ptr::drop_in_place(lvar);
                        }
                    }

                    drop_lvar
                }),
            );
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

    pub fn make_winder(&mut self) -> Value {
        static ID: AtomicI32 = AtomicI32::new(i32::MIN);
        let size = round_up(size_of::<Winder>(), 8, 0);

        let mem = self
            .mutator()
            .alloc(size, 8, 0, AllocationSemantics::Default);

        unsafe {
            mem.store(Winder {
                id: ID.fetch_add(1, Ordering::AcqRel)+1,
                after: Value::encode_null_value(),
                before: Value::encode_null_value(),
                handlers: None,
                next: None,
                header: ScmCellHeader::new(TypeId::Winder),
            });
            let reference = ObjectReference::from_raw_address(mem);
            self.mutator()
                .post_alloc(reference, size, AllocationSemantics::Default);

            Value::encode_object_value(transmute(reference))
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

    pub fn make_pvref(&mut self, level: i16, count: i16) -> Value {
        let pvref_size = round_up(size_of::<PVRef>(), 8, 0);

        unsafe {
            let mem = self
                .mutator()
                .alloc(pvref_size, 8, 0, AllocationSemantics::Default);
            mem.store(PVRef {
                header: ScmCellHeader::new(TypeId::PVRef),
                level,
                count,
            });

            let reference = ObjectReference::from_raw_address(mem);
            self.mutator()
                .post_alloc(reference, pvref_size, AllocationSemantics::Default);

            Value::encode_object_value(transmute(reference))
        }
    }

    pub fn make_syntax_pattern(
        &mut self,
        mut pattern: Value,
        mut vars: Value,
        level: i16,
        num_following_items: i16,
    ) -> Value {
        let size = round_up(size_of::<SyntaxPattern>(), 8, 0);
        unsafe {
            let mem = gc_protect!(self => pattern, vars => self.mutator().alloc(size, 8, 0, AllocationSemantics::Default));
            mem.store(SyntaxPattern {
                header: ScmCellHeader::new(TypeId::SyntaxPattern),
                vars,
                pattern,
                level,
                num_following_items,
            });

            let reference = ObjectReference::from_raw_address(mem);
            self.mutator()
                .post_alloc(reference, size, AllocationSemantics::Default);

            Value::encode_object_value(transmute(reference))
        }
    }

    pub fn make_syntax_rules(
        &mut self,
        mut name: Value,
        mut env: Value,
        mut synenv: Value,
        num_rules: u32,
    ) -> Value {
        let size = round_up(
            size_of::<SyntaxRules>() + num_rules as usize * size_of::<SyntaxRuleBranch>(),
            8,
            0,
        );

        unsafe {
            let mem = gc_protect!(self => name, env, synenv => self.mutator().alloc(size, 8, 0, AllocationSemantics::Default));
            mem.store(SyntaxRules {
                header: ScmCellHeader::new(TypeId::SyntaxRules),
                name,
                max_num_pvars: 0,
                env,
                syntax_env: synenv,
                num_rules,
                rules: [],
            });

            let rules_addr = (*mem.to_mut_ptr::<SyntaxRules>()).rules.as_mut_ptr();

            for i in 0..num_rules {
                let rule = rules_addr.add(i as usize);
                rule.write(SyntaxRuleBranch {
                    pattern: Value::encode_undefined_value(),
                    template: Value::encode_undefined_value(),
                    num_pvars: 0,
                    max_level: 0,
                })
            }

            let reference = ObjectReference::from_raw_address(mem);
            self.mutator()
                .post_alloc(reference, size, AllocationSemantics::Default);

            Value::encode_object_value(transmute(reference))
        }
    }
}
