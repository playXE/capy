#![allow(dead_code)]
use std::mem::MaybeUninit;

use rsgc::heap::heap::heap;
use rsgc::heap::root_processor::SimpleRoot;
use rsgc::prelude::{Handle, Object};
use rsgc::thread::Thread;

pub fn scm_error(err: &str) -> ! {
    todo!("throw error: {}", err)
}

#[macro_export]
macro_rules! scm_error {
    ($fmt: literal) => {
        $crate::vm::scm_error($fmt)
    };

    ($fmt: literal, $($arg: expr),*) => {
        $crate::vm::scm_error(&format!($fmt, $($arg),*))
    };
}
use rsgc::sync::mutex::{Condvar, RawMutex};

use crate::object::Module;
use crate::value::Value;
pub struct VM {
    thread: &'static mut Thread,
    state: i32,
    vmlock: RawMutex,
    cond: Condvar,
    specific: Value,
    thunk: Value,
    result: Value,
    result_exception: Value,
    module: Option<Handle<Module>>,
}

impl Object for VM {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.specific.trace(visitor);
        self.thunk.trace(visitor);
        self.result.trace(visitor);
        self.result_exception.trace(visitor);
        self.module.trace(visitor);
    }
}

#[thread_local]
static mut VM: MaybeUninit<VM> = MaybeUninit::uninit();

pub struct Runtime {
    threads: Vec<*mut VM>,
    rlock: RawMutex,
}

static mut RUNTIME: MaybeUninit<Runtime> = MaybeUninit::uninit();

pub(crate) fn init_runtime() {
    unsafe {
        RUNTIME = MaybeUninit::new(Runtime {
            threads: Vec::new(),
            rlock: RawMutex::INIT,
        });
    }

    heap().add_root(SimpleRoot::new("runtime", "rt", |processor| unsafe {
        let rt = RUNTIME.assume_init_mut();
        // synchronization: no mutex needed because roots are only processed during STW phase.
        for &thread in rt.threads.iter() {
            // parallel process thread roots
            processor.add_task(
                move |processor| {
                    (*thread).trace(processor.visitor());
                },
                false,
            );
        }
    }));
}

pub fn scm_init_vm() {
    unsafe {
        VM = MaybeUninit::new(VM {
            thread: Thread::current(),
            state: 0,
            vmlock: RawMutex::INIT,
            cond: Condvar::new(),
            specific: Value::encode_null_value(),
            thunk: Value::encode_null_value(),
            result: Value::encode_null_value(),
            result_exception: Value::encode_null_value(),
            module: None,
        });
    }

    unsafe {
        let rt = RUNTIME.assume_init_mut();
        rt.rlock.lock(true);
        rt.threads.push(VM.assume_init_mut());
        rt.rlock.unlock();
    }
}

pub fn scm_vm<'a>() -> &'a mut VM {
    unsafe {
        let vm = VM.assume_init_mut();
        vm
    }
}

pub fn scm_current_module() -> Option<Handle<Module>> {
    scm_vm().module
}
