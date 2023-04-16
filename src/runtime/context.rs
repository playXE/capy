use std::cell::UnsafeCell;

use rsgc::prelude::Thread;

pub struct Context {
    pub(crate) next: *mut Context,
    pub(crate) prev: *mut Context,
    pub mutator: &'static mut Thread,
}



impl Context {
    pub fn visit_roots(&self, _visitor: &mut dyn rsgc::prelude::Visitor) {
        
    }
}

thread_local! {
    pub(crate) static CONTEXT: UnsafeCell<*mut Context> = UnsafeCell::new(std::ptr::null_mut());
}

pub fn get_context<'a>() -> Option<&'a mut Context> {
    CONTEXT.with(|ptr| unsafe {
        let ptr = ptr.get().read();

        if ptr.is_null() {
            None
        } else {
            Some(&mut *ptr)
        }
    })
}

impl Context {
    pub(crate) fn new(thread: &'static mut Thread) -> Self {
        Self {
            next: std::ptr::null_mut(),
            prev: std::ptr::null_mut(),
            mutator: thread,
        }
    }
}
