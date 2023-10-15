use crate::runtime::hashtable::{lookup_hashtable_size, WeakHashTableRec, WeakHashtable};
use crate::runtime::object::{scm_bytevector_set_mapping, ScmTuple, ScmWeakMapping};
use crate::{
    compiler::expand::define_syntax,
    gc_protect,
    runtime::{
        environment::ScmEnvironment,
        object::{ScmCellRef, TypeId},
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
use std::mem::{size_of, transmute};
use std::ptr::addr_of_mut;

use super::{
    sync::mutex::{Mutex, RawMutex},
    thread::Thread,
};

impl Thread {
    pub fn make_cons<const IMMORTAL: bool>(&mut self, car: Value, cdr: Value) -> Value {
        let objref = if IMMORTAL {
            self.alloc_immortal(size_of::<ScmPair>(), TypeId::Pair)
        } else {
            self.alloc_small(size_of::<ScmPair>(), TypeId::Pair)
        };

        unsafe {
            (*objref.cast::<ScmPair>()).car = car;
            (*objref.cast::<ScmPair>()).cdr = cdr;

            Value::encode_object_value(ScmCellRef(objref as _))
        }
    }

    pub fn make_vector<const IMMORTAL: bool>(&mut self, len: usize, fill: Value) -> Value {
        let size = round_up(size_of::<ScmVector>() + len * size_of::<Value>(), 8, 0);

        unsafe {
            let objref = if IMMORTAL {
                self.alloc_immortal(size, TypeId::Vector)
            } else {
                self.alloc(size, TypeId::Vector)
            };

            (*objref.cast::<ScmVector>()).length = len;
            let mut cursor = (*objref.cast::<ScmVector>()).values.as_mut_ptr();
            let end = cursor.add(len);

            while cursor < end {
                cursor.write(fill);
                cursor = cursor.add(1);
            }

            Value::encode_object_value(ScmCellRef(objref as _))
        }
    }

    pub fn make_values<const IMMORTAL: bool>(&mut self, len: usize, fill: Value) -> Value {
        let size = round_up(size_of::<ScmVector>() + len * size_of::<Value>(), 8, 0);

        unsafe {
            let objref = if IMMORTAL {
                self.alloc_immortal(size, TypeId::Values)
            } else {
                self.alloc(size, TypeId::Values)
            };

            (*objref.cast::<ScmVector>()).length = len;
            let mut cursor = (*objref.cast::<ScmVector>()).values.as_mut_ptr();
            let end = cursor.add(len);

            while cursor < end {
                cursor.write(fill);
                cursor = cursor.add(1);
            }

            Value::encode_object_value(ScmCellRef(objref as _))
        }
    }

    pub fn make_tuple<const IMMORTAL: bool>(&mut self, len: usize, fill: Value) -> Value {
        let size = round_up(size_of::<ScmTuple>() + len * size_of::<Value>(), 8, 0);

        unsafe {
            let objref = if IMMORTAL {
                self.alloc_immortal(size, TypeId::Tuple)
            } else {
                self.alloc(size, TypeId::Tuple)
            };

            (*objref.cast::<ScmTuple>()).length = len;
            let mut cursor = (*objref.cast::<ScmTuple>()).values.as_mut_ptr();
            let end = cursor.add(len);

            while cursor < end {
                cursor.write(fill);
                cursor = cursor.add(1);
            }

            Value::encode_object_value(ScmCellRef(objref as _))
        }
    }

    pub fn make_bytevector<const IMMORTAL: bool>(&mut self, n: usize, init: u8) -> Value {
        let size = round_up(size_of::<ScmBytevector>() + n, 8, 0);
        unsafe {
            let objref = if IMMORTAL {
                self.alloc_immortal(size, TypeId::Bytevector)
            } else {
                self.alloc(size, TypeId::Bytevector)
            };

            (*objref.cast::<ScmBytevector>()).length = n;
            (*objref.cast::<ScmBytevector>()).elems =
                (*objref.cast::<ScmBytevector>()).data.as_mut_ptr();
            let mut cursor = (*objref.cast::<ScmBytevector>()).elems;
            let end = cursor.add(n);

            while cursor < end {
                cursor.write(init);
                cursor = cursor.add(1);
            }

            Value::encode_object_value(ScmCellRef(objref as _))
        }
    }

    pub fn make_bytevector_from_slice<const IMMORTAL: bool>(&mut self, slice: &[u8]) -> Value {
        let size = round_up(size_of::<ScmBytevector>() + slice.len(), 8, 0);
        unsafe {
            let objref = if IMMORTAL {
                self.alloc_immortal(size, TypeId::Bytevector)
            } else {
                self.alloc(size, TypeId::Bytevector)
            };

            (*objref.cast::<ScmBytevector>()).elems =
                (*objref.cast::<ScmBytevector>()).data.as_mut_ptr();
            (*objref.cast::<ScmBytevector>()).length = slice.len();
            let cursor = (*objref.cast::<ScmBytevector>()).elems;
            cursor.copy_from_nonoverlapping(slice.as_ptr(), slice.len());

            Value::encode_object_value(ScmCellRef(objref as _))
        }
    }

    pub fn make_bytevector_mapping<const IMMORTAL: bool>(
        &mut self,
        addr: *mut u8,
        len: usize,
    ) -> Value {
        let size = round_up(size_of::<ScmBytevector>(), 8, 0);

        unsafe {
            let objref = if IMMORTAL {
                self.alloc_immortal(size, TypeId::Bytevector)
            } else {
                self.alloc(size, TypeId::Bytevector)
            };

            (*objref.cast::<ScmBytevector>()).length = len;
            (*objref.cast::<ScmBytevector>()).elems = addr;
            let val = Value::encode_object_value(ScmCellRef(objref as _));
            scm_bytevector_set_mapping(val);
            val
        }
    }

    pub fn make_string<const IMMORTAL: bool>(&mut self, str: &str) -> Value {
        let size = round_up(size_of::<ScmString>() + str.len() + 1, 8, 0);

        unsafe {
            let objref = if IMMORTAL {
                self.alloc_immortal(size, TypeId::String)
            } else {
                self.alloc(size, TypeId::String)
            };

            (*objref.cast::<ScmString>()).length = str.len();
            let strstart = (*objref.cast::<ScmString>()).name.as_mut_ptr();
            strstart.copy_from_nonoverlapping(str.as_bytes().as_ptr(), str.len());
            strstart.add(str.len()).write(0);

            Value::encode_object_value(ScmCellRef(objref as _))
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
        unsafe {
            let objref = if IMMORTAL {
                self.alloc_immortal(size, TypeId::Program)
            } else {
                self.alloc_small(size, TypeId::Program)
            };

            (*objref.cast::<ScmProgram>()).vcode = vcode;
            (*objref.cast::<ScmProgram>()).nfree = num_free_vars as _;
            for i in 0..num_free_vars {
                (*objref.cast::<ScmProgram>())
                    .free
                    .as_mut_ptr()
                    .add(i as usize)
                    .write(Value::encode_undefined_value());
            }

            Value::encode_object_value(ScmCellRef(objref as _))
        }
    }

    pub fn make_symbol(&mut self, str: &str, gensym: bool, interned: bool) -> Value {
        let size = round_up(size_of::<ScmSymbol>() + str.len() + 1, 8, 0);

        unsafe {
            let obj = if interned {
                self.alloc_immortal(size, TypeId::Symbol)
            } else {
                self.alloc(size, TypeId::Symbol)
            };

            (*obj.cast::<ScmSymbol>())
                .name
                .as_mut_ptr()
                .copy_from_nonoverlapping(str.as_bytes().as_ptr(), str.len());
            (*obj.cast::<ScmSymbol>())
                .name
                .as_mut_ptr()
                .add(str.len())
                .write(0);
            (*obj.cast::<ScmSymbol>()).length = str.len() as _;
            (*obj.cast::<ScmSymbol>()).gensym = gensym;
            (*obj.cast::<ScmSymbol>()).interned = interned;

            Value::encode_object_value(ScmCellRef(obj as _))
        }
    }

    pub fn make_gloc<const IMMORTAL: bool>(&mut self, name: Value, value: Value) -> Value {
        let size = round_up(size_of::<ScmGloc>(), 8, 0);

        unsafe {
            let objref = if IMMORTAL {
                self.alloc_immortal(size, TypeId::GLOC)
            } else {
                self.alloc(size, TypeId::GLOC)
            };

            (*objref.cast::<ScmGloc>()).name = name;
            (*objref.cast::<ScmGloc>()).value = value;

            Value::encode_object_value(ScmCellRef(objref as _))
        }
    }

    pub fn make_box(&mut self) -> Value {
        let size = round_up(size_of::<ScmBox>(), 8, 0);

        unsafe {
            let objref = self.alloc(size, TypeId::Box);

            (*objref.cast::<ScmBox>()).value = Value::encode_undefined_value();

            Value::encode_object_value(ScmCellRef(objref as _))
        }
    }

    pub fn make_hashtable_generic(&mut self, n: u32, mut vector: Value) -> Value {
        let hashtable =
            gc_protect!(self => vector => self.make_hashtable(n, HashTableType::Generic));
        hashtable
            .cast_as::<ScmHashTable>()
            .handlers
            .assign(hashtable, vector);
        hashtable
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
            let datum_addr = self
                .alloc(datum_size, TypeId::HashTableRec)
                .cast::<HashTableRec>();

            (*datum_addr).capacity = n;
            (*datum_addr).live = 0;
            (*datum_addr).used = 0;

            let elts = (*datum_addr).elts.as_mut_ptr();

            for i in 0..(n + n) {
                elts.add(i as usize).write(Value::encode_empty_value());
            }

            let mut datum = Value::encode_object_value(ScmCellRef(datum_addr as _));

            let mem = gc_protect!(self => datum => self.alloc(size, TypeId::HashTable));

            let hashtable = mem.cast::<ScmHashTable>();

            (*hashtable).lock = RawMutex::INIT;
            (*hashtable).hash = hash;
            (*hashtable).equiv = equiv;
            (*hashtable).datum = datum_addr;
            (*hashtable).handlers = Value::encode_bool_value(false);
            (*hashtable).typ = typ;
            (*hashtable).rehash = false;
            (*hashtable).immutable = false;

            Value::encode_object_value(ScmCellRef(mem as _))
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
            let datum_addr = self
                .alloc(datum_size, TypeId::WeakHashTableRec)
                .cast::<WeakHashTableRec>();

            (*datum_addr).capacity = n;
            (*datum_addr).live = 0;
            (*datum_addr).used = 0;
            let elts = (*datum_addr).elts.as_mut_ptr();

            for i in 0..n {
                elts.add(i as usize).write(Value::encode_empty_value());
            }

            let mut datum = Value::encode_object_value(ScmCellRef(datum_addr as _));

            let mem = gc_protect!(self => datum => self.alloc(size, TypeId::WeakHashTable));

            let hashtable = mem.cast::<WeakHashtable>();
            (*hashtable).lock = RawMutex::INIT;
            (*hashtable).immutable = false;
            (*hashtable).datum = datum_addr;

            Value::encode_object_value(ScmCellRef(mem as _))
        }
    }

    pub fn make_environment(&mut self, name: Value) -> Value {
        let size = round_up(size_of::<ScmEnvironment>(), 8, 0);

        let mem = self
            .alloc(size, TypeId::Environment)
            .cast::<ScmEnvironment>();
        unsafe {
            addr_of_mut!((*mem).syntactic_environment).write(Mutex::new(define_syntax()));
            (*mem).name = name;
            (*mem).mutable = true;
            (*mem).ht = Value::encode_null_value();
            (*mem).synenv = Value::encode_null_value();

            let mut env = Value::encode_object_value(ScmCellRef(transmute(mem)));
            let ht = gc_protect!(self => env => self.make_hashtable(113, HashTableType::Eq));
            env.cast_as::<ScmEnvironment>().ht.assign(env, ht);
            let synenv = gc_protect!(self => env => self.make_hashtable(113, HashTableType::Eq));
            env.cast_as::<ScmEnvironment>().synenv.assign(env, synenv);
            env
        }
    }

    pub fn make_weakmapping(&mut self) -> Value {
        let size = round_up(size_of::<ScmWeakMapping>(), 8, 0);

        unsafe {
            let mem = self
                .alloc(size, TypeId::WeakMapping)
                .cast::<ScmWeakMapping>();

            (*mem).key = Value::encode_bool_value(false);
            (*mem).value = Value::encode_bool_value(false);

            Value::encode_object_value(ScmCellRef(mem as _))
        }
    }
}
