use mmtk::util::{ObjectReference, Address};

use crate::{runtime::{thread::Thread, cell::CellTag}, gc::edges::ScmEdge};

pub extern "C-unwind" fn object_reference_write_slow_call(src: ObjectReference, slot: Address, target: ObjectReference) {
    let mutator = Thread::current();
    mutator
        .mmtk()
        .mutator
        .barrier
        .object_reference_write_slow(src, ScmEdge::from(slot), target)
}

pub extern "C-unwind" fn allocate(thread: &mut Thread, size: usize) -> usize {
    thread.mmtk().alloc(size, CellTag::NULL).to_ptr() as usize
}