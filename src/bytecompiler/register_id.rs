use std::ops::{Deref, DerefMut};

use crate::bytecode::virtual_register::VirtualRegister;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RegisterID {
    refcnt: u32,
    register: VirtualRegister,
    is_temporary: bool,
}

impl RegisterID {
    pub fn new(reg: VirtualRegister) -> Self {
        Self {
            refcnt: 0,
            register: reg,
            is_temporary: false,
        }
    }

    pub fn set_temporary(&mut self, is_temporary: bool) {
        self.is_temporary = is_temporary;
    }

    pub fn is_temporary(&self) -> bool {
        self.is_temporary
    }

    pub fn set_index(&mut self, i: VirtualRegister) {
        self.register = i;
    }

    pub fn index(&self) -> i16 {
        self.register.offset()
    }

    pub fn virtual_register(&self) -> VirtualRegister {
        self.register
    }

    pub fn inc(&mut self) {
        self.refcnt += 1;
    }

    pub fn dec(&mut self) {
        self.refcnt -= 1;
    }

    pub fn refcnt(&self) -> u32 {
        self.refcnt
    }
}

pub struct RegisterRef {
    id: *mut RegisterID,
}

impl RegisterRef {
    pub(super) fn new(reg: *mut RegisterID) -> Self {
        Self { id: reg }
    }
}

impl Deref for RegisterRef {
    type Target = RegisterID;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.id }
    }
}

impl DerefMut for RegisterRef {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.id }
    }
}

impl Clone for RegisterRef {
    fn clone(&self) -> Self {
        Self { id: self.id }
    }
}

impl Copy for RegisterRef {}

impl PartialEq for RegisterRef {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for RegisterRef {}
