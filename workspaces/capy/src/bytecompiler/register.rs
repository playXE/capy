use std::{ptr::NonNull, marker::PhantomData};

use crate::bytecode::virtual_register::VirtualRegister;

pub struct RegisterID {
    refcnt: u32,
    virtual_register: VirtualRegister,
    is_temporary: bool 
}

impl RegisterID {
    pub fn new(virtual_register: VirtualRegister) -> Self {
        Self {
            refcnt: 0,
            virtual_register,
            is_temporary: false
        }
    }

    pub fn set_index(&mut self, r: VirtualRegister) {
        self.virtual_register = r;
    }

    pub fn set_temporary(&mut self) {
        self.is_temporary = true;
    }

    pub fn index(&self) -> i32 {
        self.virtual_register.offset()
    }

    pub fn virtual_register(&self) -> VirtualRegister {
        self.virtual_register
    }

    pub fn is_temporary(&self) -> bool {
        self.is_temporary
    }

    pub fn inc(&mut self) {
        self.refcnt += 1;
    }

    pub fn dec(&mut self) {
        self.refcnt -= 1;
    }

    pub fn ref_count(&self) -> u32 {
        self.refcnt
    }
}

pub struct RegisterIDRef<'a> {
    pub(crate) register: NonNull<RegisterID>,
    pub(crate) marker: PhantomData<&'a RegisterID>
}

impl std::ops::Deref for RegisterIDRef<'_> {
    type Target = RegisterID;

    fn deref(&self) -> &Self::Target {
        unsafe { self.register.as_ref() }
    }
}

impl std::ops::DerefMut for RegisterIDRef<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.register.as_mut() }
    }
}
/* 
impl<'a> Drop for RegisterIDRef<'a> {
    fn drop(&mut self) {
        self.dec();
    }
}

impl<'a> Clone for RegisterIDRef<'a> {
    fn clone(&self) -> Self {
        unsafe {
            (*self.register.as_ptr()).inc();
        }
        Self {
            register: self.register,
            marker: PhantomData
        }
    }
}*/