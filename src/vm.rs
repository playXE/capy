#![allow(dead_code)]
use std::mem::MaybeUninit;
use std::ptr::null_mut;
use std::sync::atomic::{AtomicU64, Ordering};

use rsgc::heap::heap::heap;
use rsgc::heap::root_processor::SimpleRoot;
use rsgc::prelude::{Handle, Object};
use rsgc::system::arraylist::ArrayList;
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

use crate::runtime::object::{Module, ScmResult};
use crate::runtime::value::Value;

use self::callframe::CallFrame;
#[repr(C)]
pub struct VM {
    thread: &'static mut Thread,
    state: i32,
    vmlock: RawMutex,
    cond: Condvar,
    specific: Value,
    thunk: Value,
    result: Value,
    result_exception: Value,
    sp: *mut Value,
    stack: Box<[Value]>,
    pub(crate) module: Option<Handle<Module>>,
    pub(crate) entry: u64,
    /// Trampoline from native to Scheme support
    tail_rator: Value,
    tail_rands: ArrayList<Value>,
    pub(crate) top_call_frame: *mut CallFrame,
    pub(crate) top_entry_frame: *mut CallFrame,
    pub(crate) prev_top_call_frame: *mut CallFrame,
    pub(crate) prev_top_entry_frame: *mut CallFrame,
    
}

impl VM {
    pub(crate) fn next_entry(&mut self) -> u64 {
        let entry = self.entry;
        self.entry = ENTRY.fetch_add(1, Ordering::AcqRel);
        entry
    }

    pub(crate) fn tail_call(&mut self, rator: Value, rands: &[Value]) -> ScmResult {
        self.tail_rator = rator;
        self.tail_rands.clear();
        let t = Thread::current();
        for rand in rands {
            self.tail_rands.push(t, *rand);
        }
        self.entry = 0;
        ScmResult {
            tag: ScmResult::TAIL,
            value: Value::encode_undefined_value(),
        }
    }

    pub fn mutator(&mut self) -> &mut Thread {
        self.thread
    }
}

impl Object for VM {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.specific.trace(visitor);
        self.thunk.trace(visitor);
        self.result.trace(visitor);
        self.result_exception.trace(visitor);
        self.module.trace(visitor);
        self.tail_rands.trace(visitor);
        self.tail_rator.trace(visitor);
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
            sp: null_mut(),
            stack: vec![Value::encode_undefined_value(); 4096].into_boxed_slice(),
            entry: 0,
            tail_rands: ArrayList::new(Thread::current()),
            tail_rator: Value::encode_undefined_value(),
            top_call_frame: null_mut(),
            prev_top_entry_frame: null_mut(),
            prev_top_call_frame: null_mut(),
            top_entry_frame: null_mut()
        });
    }

    unsafe {
        scm_vm().sp = scm_vm().stack.last_mut().unwrap() as *mut Value;
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

pub fn scm_set_current_module(module: Option<Handle<Module>>) {
    scm_vm().module = module;
}

pub mod callframe;
pub mod interpreter;
pub mod stacktrace;
pub mod setjmp;
//pub mod llint;

/// A simple counter used to identify different VM entrypoints.
///
/// Used to protect call/cc from being called from a different VM
/// or call/cc going through a Rust frame.
static ENTRY: AtomicU64 = AtomicU64::new(0);
