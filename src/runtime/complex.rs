use rsgc::prelude::{Object, Allocation, Handle};

use super::{value::Value, object::ObjectHeader};

pub fn scm_complex_is_exact(c: Value) -> bool {
    if c.is_complex() {
        let c = c.complex();

        return !c.r.is_double() && !c.i.is_double()
    }

    false
}

#[repr(C)]
pub struct Complex {
    pub(crate) object: ObjectHeader,
    pub r: Value,
    pub i: Value
}

impl Object for Complex {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.r.trace(visitor);
        self.i.trace(visitor);
    }
}

impl Allocation for Complex {}

impl Value {
    pub fn complex(self) -> Handle<Complex> {
        debug_assert!(self.is_complex());
        unsafe {
            std::mem::transmute(self)
        }
    }
}