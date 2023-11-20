use crate::runtime::{value::*, cell::CellReference};

use super::stackframe::CallFrame;


pub union Register {
    pub value: Tagged<Value>,
    pub compressed: TaggedValue,
    pub callframe: *mut CallFrame,
    pub code_block: Tagged<CellReference>,
    pub flonum: f64,
    pub word: Word,
    pub u32: u32,
    pub u64: u64,
    pub bits: AsBits
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub struct AsBits {
    pub tag: u32,
    pub payload: u32,
}

impl Register {
    pub fn tag(&self) -> u32 {
        unsafe {
            self.bits.tag
        }
    }

    pub fn tag_mut(&mut self) -> &mut u32 {
        unsafe {
            &mut self.bits.tag
        }
    }

    pub fn payload(&self) -> u32 {
        unsafe {
            self.bits.payload
        }
    }

    pub fn payload_mut(&mut self) -> &mut u32 {
        unsafe {
            &mut self.bits.payload
        }
    }

    pub fn pointer(&self) -> *mut () {
        unsafe { self.u64 as _ }
    }

    

}