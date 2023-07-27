use std::mem::size_of;

use mmtk::{util::ObjectReference, AllocationSemantics, MutatorContext};

use crate::{
    runtime::object::{Header, ScmCellHeader, ScmCellRef, TypeId},
    runtime::value::Value,
    utils::round_up,
};

use super::thread::Thread;

impl Thread {
    pub fn make_cons(&mut self, car: Value, cdr: Value) -> ScmCellRef {
        unsafe {
            let mutator = self.mutator.assume_init_mut();
            let mem = mutator.alloc(
                size_of::<ScmCellHeader>() + size_of::<Value>() * 2,
                size_of::<usize>(),
                0,
                AllocationSemantics::Default,
            );

            mem.store::<ScmCellHeader>(ScmCellHeader {
                as_header: Header {
                    type_id: TypeId::Pair,
                    pad: [0; 4],
                    flags: 0,
                },
            });

            mem.add(size_of::<ScmCellHeader>()).store(car);
            mem.add(size_of::<ScmCellHeader>() + size_of::<Value>())
                .store(cdr);
            let reference = ObjectReference::from_raw_address(mem);
            mutator.post_alloc(
                reference,
                size_of::<ScmCellHeader>() + size_of::<Value>() * 2,
                AllocationSemantics::Default,
            );

            ScmCellRef(reference)
        }
    }

    pub fn make_vector(&mut self, len: usize, fill: Value) -> ScmCellRef {
        let size = size_of::<ScmCellHeader>() + size_of::<Value>() * len + size_of::<Value>();

        unsafe {
            let mutator = self.mutator.assume_init_mut();
            let mem = mutator.alloc(size, size_of::<usize>(), 0, AllocationSemantics::Default);

            mem.store::<ScmCellHeader>(ScmCellHeader {
                as_header: Header {
                    type_id: TypeId::Vector,
                    pad: [0; 4],
                    flags: 0,
                },
            });

            mem.add(size_of::<ScmCellHeader>())
                .store(Value::encode_int32(len as i32));

            for i in 0..len {
                mem.add(size_of::<ScmCellHeader>() + size_of::<Value>() * (i + 1))
                    .store(fill);
            }

            let reference = ObjectReference::from_raw_address(mem);
            mutator.post_alloc(reference, size, AllocationSemantics::Default);

            ScmCellRef(reference)
        }
    }

    pub fn make_bytevector<const IMMORTAL: bool>(&mut self, n: usize, init: u8) -> ScmCellRef {
        let size = round_up(
            size_of::<ScmCellHeader>() + size_of::<u8>() * n + size_of::<Value>(),
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

            mem.store::<ScmCellHeader>(ScmCellHeader {
                as_header: Header {
                    type_id: TypeId::Bytevector,
                    pad: [0; 4],
                    flags: 0,
                },
            });

            mem.add(size_of::<ScmCellHeader>())
                .store(Value::encode_int32(n as i32));
            mem.add(size_of::<ScmCellHeader>() + size_of::<Value>())
                .to_mut_ptr::<u8>()
                .write_bytes(init, n);
            let reference = ObjectReference::from_raw_address(mem);
            mutator.post_alloc(reference, size, semantics);

            ScmCellRef(reference)
        }
    }

    pub fn make_bytevector_from_slice<const IMMORTAL: bool>(&mut self, slice: &[u8]) -> ScmCellRef {
        let size = round_up(
            size_of::<ScmCellHeader>() + size_of::<u8>() * slice.len() + size_of::<Value>(),
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

            mem.store::<ScmCellHeader>(ScmCellHeader {
                as_header: Header {
                    type_id: TypeId::Bytevector,
                    pad: [0; 4],
                    flags: 0,
                },
            });

            mem.add(size_of::<ScmCellHeader>())
                .store(Value::encode_int32(slice.len() as i32));
            mem.add(size_of::<ScmCellHeader>() + size_of::<Value>())
                .to_mut_ptr::<u8>()
                .copy_from_nonoverlapping(slice.as_ptr(), slice.len());
            let reference = ObjectReference::from_raw_address(mem);
            mutator.post_alloc(reference, size, semantics);

            ScmCellRef(reference)
        }
    }

    pub fn make_string<const IMMORTAL: bool>(&mut self, str: &str) -> ScmCellRef {
        let size = round_up(
            size_of::<ScmCellHeader>() + size_of::<u8>() * str.len() + size_of::<Value>() + 1,
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

            mem.store::<ScmCellHeader>(ScmCellHeader {
                as_header: Header {
                    type_id: TypeId::String,
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
            let reference = ObjectReference::from_raw_address(mem);
            mutator.post_alloc(reference, size, semantics);

            ScmCellRef(reference)
        }
    }

    pub fn make_program<const IMMORTAL: bool>(
        &mut self,
        code: *const u8,
        num_free_vars: u32,
    ) -> ScmCellRef {
        let size = round_up(
            size_of::<ScmCellHeader>() + size_of::<Value>() * (num_free_vars as usize + 2),
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

            mem.store::<ScmCellHeader>(ScmCellHeader {
                as_header: Header {
                    type_id: TypeId::Program,
                    pad: [0; 4],
                    flags: 0,
                },
            });

            mem.add(size_of::<ScmCellHeader>()).store(code as usize);
            mem.add(size_of::<ScmCellHeader>() + size_of::<Value>())
                .store(Value::encode_int32(num_free_vars as i32));
            let mut cursor = mem
                .add(size_of::<ScmCellHeader>() + size_of::<Value>() * 2)
                .to_mut_ptr::<Value>();
            for _ in 0..num_free_vars {
                cursor.write(Value::encode_undefined_value());
                cursor = cursor.add(1);
            }

            let reference = ObjectReference::from_raw_address(mem);
            mutator.post_alloc(reference, size, semantics);

            ScmCellRef(reference)
        }
    }

    pub fn make_symbol(&mut self, str: &str) -> ScmCellRef {
        let size = round_up(
            size_of::<ScmCellHeader>() + size_of::<u8>() * str.len() + size_of::<Value>() + 1,
            8,
            0,
        );

        unsafe {
            let mutator = self.mutator.assume_init_mut();
            let mem = mutator.alloc(size, size_of::<usize>(), 0, AllocationSemantics::Immortal);

            mem.store::<ScmCellHeader>(ScmCellHeader {
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
            let reference = ObjectReference::from_raw_address(mem);
            mutator.post_alloc(reference, size, AllocationSemantics::Default);

            ScmCellRef(reference)
        }
    }

    pub fn make_gloc(&mut self, name: Value, value: Value) -> Value {
        let size = round_up(size_of::<ScmCellHeader>() + size_of::<Value>() * 2, 8, 0);

        unsafe {
            let mutator = self.mutator.assume_init_mut();
            let mem = mutator.alloc(size, size_of::<usize>(), 0, AllocationSemantics::Default);

            mem.store::<ScmCellHeader>(ScmCellHeader {
                as_header: Header {
                    type_id: TypeId::GLOC,
                    pad: [0; 4],
                    flags: 0,
                },
            });

            mem.add(size_of::<ScmCellHeader>()).store(name);
            mem.add(size_of::<ScmCellHeader>() + size_of::<Value>())
                .store(value);
            let reference = ObjectReference::from_raw_address(mem);
            mutator.post_alloc(reference, size, AllocationSemantics::Default);

            Value::encode_object_value(ScmCellRef(reference))
        }
    }
}
