use std::mem::size_of;

use mmtk::{AllocationSemantics, MutatorContext, util::ObjectReference};

use crate::{gc::object::{ScmCellHeader, ScmCellRef, Header, TypeId}, runtime::value::Value};

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
                    flags: 0
                }
            });

            mem.add(size_of::<ScmCellHeader>()).store(car);
            mem.add(size_of::<ScmCellHeader>() + size_of::<Value>()).store(cdr);
            let reference = ObjectReference::from_raw_address(mem);
            mutator.post_alloc(reference, size_of::<ScmCellHeader>() + size_of::<Value>() * 2, AllocationSemantics::Default);

            ScmCellRef(reference)
        }
    }
}
