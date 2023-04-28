use std::{cell::UnsafeCell, ptr::null_mut, mem::size_of};

use rsgc::prelude::Thread;

use super::{callframe::CallFrame, value::ScmValue};

pub struct VM {
    pub(crate) next: *mut VM,
    pub(crate) prev: *mut VM,
    pub(crate) top_call_frame: *mut CallFrame,
    pub(crate) top_entry_frame: *mut CallFrame,
    pub mutator: &'static mut Thread,
}



impl VM {
    pub fn visit_roots(&self, _visitor: &mut dyn rsgc::prelude::Visitor) {
        
    }
}

thread_local! {
    pub(crate) static CONTEXT: UnsafeCell<*mut VM> = UnsafeCell::new(std::ptr::null_mut());
}

pub fn get_context<'a>() -> Option<&'a mut VM> {
    CONTEXT.with(|ptr| unsafe {
        let ptr = ptr.get().read();

        if ptr.is_null() {
            None
        } else {
            Some(&mut *ptr)
        }
    })
}

impl VM {
    pub(crate) fn new(thread: &'static mut Thread) -> Self {
        Self {
            next: std::ptr::null_mut(),
            prev: std::ptr::null_mut(),
            top_call_frame: null_mut(),
            top_entry_frame: null_mut(),
            mutator: thread,
        }
    }
}


pub struct VMEntryRecord {
    pub vm: *const VM,
    pub prev_top_call_frame: *mut CallFrame,
    pub prev_top_entry_frame: *mut CallFrame,
    pub callee: ScmValue,
}

impl VMEntryRecord {
    pub const VM_ENTRY_TOTAL_FRAME_SIZE: usize = (size_of::<Self>() + 16 - 1) & !(16 - 1);
}