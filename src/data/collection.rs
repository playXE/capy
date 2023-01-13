use std::ops::{Deref, DerefMut};

use memoffset::offset_of;

use crate::{prelude::*, utilities::arraylist::ArrayList};

#[repr(C)]
pub struct Vector {
    pub elements: ArrayList<Value>,
    pub mutable: bool,
    pub growable: bool
}


impl Object for Vector {
    fn trace(&self, visitor: &mut dyn rsgc::system::traits::Visitor) {
        self.elements.trace(visitor);
    }
}

impl Allocation for Vector {}


#[repr(C)]
pub struct ByteVector {
    count: u32,
    padding: u32,
    elements: [u8; 0]
}

impl ByteVector {
    pub fn new(ctx: &mut Context, n: usize) -> Handle<ByteVector> {
        unsafe {
            let mut bvec = ctx.mutator().allocate_varsize::<Self>(n);
            let this = bvec.as_mut_ptr();
            (*this).count = n as _;
            core::ptr::write_bytes((*this).elements.as_mut_ptr(), 0, n);
            bvec.assume_init()
        }
    }
}

impl Deref for ByteVector {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        unsafe { std::slice::from_raw_parts(self.elements.as_ptr(), self.count as usize) }
    }
}

impl DerefMut for ByteVector {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { std::slice::from_raw_parts_mut(self.elements.as_mut_ptr(), self.count as usize) }
    }
}

impl Object for ByteVector {}

impl Allocation for ByteVector {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = 1;
    const VARSIZE_OFFSETOF_CAPACITY: usize = 0;
    const VARSIZE_OFFSETOF_LENGTH: usize = 0;
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(Self, elements);
    const VARSIZE_NO_HEAP_PTRS: bool = true;
    const NO_HEAP_PTRS: bool = true;
}