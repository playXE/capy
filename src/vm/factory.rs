use std::mem::{size_of, transmute};

use mmtk::{util::ObjectReference, AllocationSemantics, MutatorContext};

use crate::{
    runtime::object::{Header, ScmCellHeader, ScmCellRef, TypeId},
    runtime::{
        object::{ScmBytevector, ScmGloc, ScmPair, ScmProgram, ScmString, ScmSymbol, ScmVector},
        value::Value,
    },
    utils::round_up,
};

use super::thread::Thread;

impl Thread {
    pub fn make_cons<const IMMORTAL: bool>(&mut self, car: Value, cdr: Value) -> Value {
        unsafe {
            let semantics = if IMMORTAL {
                AllocationSemantics::Immortal
            } else {
                AllocationSemantics::Default
            };
            let mutator = self.mutator.assume_init_mut();
            let mem = mutator.alloc(
                size_of::<ScmPair>(),
                size_of::<usize>(),
                0,
                semantics,
            );

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
            mutator.post_alloc(
                reference,
                size_of::<ScmPair>(),
                semantics,
            );

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
}
