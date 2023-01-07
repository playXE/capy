use crate::{prelude::*, utilities::arraylist::ArrayList};

#[repr(C)]
pub struct Vector {
    pub elements: ArrayList<Value>,
    pub mutable: bool,
    pub growable: bool
}

#[repr(C)]
pub struct ByteVector(pub ArrayList<u8>);

impl Object for Vector {
    fn trace(&self, visitor: &mut dyn rsgc::system::traits::Visitor) {
        self.elements.trace(visitor);
    }
}

impl Allocation for Vector {}

impl Object for ByteVector {
    fn trace(&self, visitor: &mut dyn rsgc::system::traits::Visitor) {
        self.0.trace(visitor);
    }
}

impl Allocation for ByteVector {}