#![allow(dead_code)]
use std::mem::MaybeUninit;
use std::ops::Index;
use std::ptr::null_mut;
use std::sync::atomic::{AtomicU64, Ordering, AtomicBool, AtomicI32};

use once_cell::sync::Lazy;
use rsgc::heap::heap::heap;
use rsgc::heap::root_processor::SimpleRoot;
use rsgc::prelude::{Handle, Object, Allocation};
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
use crate::runtime::symbol::Intern;
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
    pub(crate) result: Value,
    result_exception: Value,
    pub(crate) sp: *mut Value,
    pub(crate) stack: Box<[Value]>,
    pub(crate) module: Option<Handle<Module>>,
    pub(crate) entry: u64,
    /// Trampoline from native to Scheme support
    pub(crate) tail_rator: Value,
    pub(crate) tail_rands: ArrayList<Value>,
    pub(crate) top_call_frame: *mut CallFrame,
    pub(crate) top_entry_frame: *mut CallFrame,
    pub(crate) prev_top_call_frame: *mut CallFrame,
    pub(crate) prev_top_entry_frame: *mut CallFrame,
    pub(crate) winders: Option<Handle<Winder>>,
    pub(crate) vmid: i32,
    
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
        ScmResult {
            tag: ScmResult::TAIL,
            value: Value::encode_undefined_value(),
        }
    }

    pub fn mutator(&mut self) -> &mut Thread {
        self.thread
    }

    pub unsafe fn wind_up(&mut self, before: Value, after: Value, handlers: Option<Value>) {
        static ID: AtomicI32 = AtomicI32::new(1);
        let winder = Winder {
            id: ID.fetch_add(1, Ordering::AcqRel),
            before,
            after,
            handlers,
            next: self.winders.take(),
        };
        self.winders = Some(self.mutator().allocate(winder));
    }

    pub unsafe fn wind_down(&mut self) -> Option<Handle<Winder>> {
        let winder = self.winders.take();
        if let Some(winder) = winder {
            self.winders = (*winder).next;
        }
        winder
    }

    pub fn current_handlers(&self) -> Option<Value> {
        let mut winders = self.winders;

        while let Some(winder) = winders {
            if let Some(handler) = winder.handlers {
                return Some(handler);
            }

            winders = winder.next;
        } 

        None
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
        self.winders.trace(visitor);
        visitor.visit_conservative(self.stack.as_ptr().cast(), self.stack.len());
    }
}

#[thread_local]
static mut VM: MaybeUninit<VM> = MaybeUninit::uninit();

pub struct Runtime {
    threads: Vec<*mut VM>,
    rlock: RawMutex,
}

static mut RUNTIME: MaybeUninit<Runtime> = MaybeUninit::uninit();

static RT_INITIALIZED: AtomicBool = AtomicBool::new(false);

pub(crate) fn init_runtime() {
    if RT_INITIALIZED.swap(true, std::sync::atomic::Ordering::AcqRel) {
        return;
    }
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
            /*processor.add_task(
                move |processor| {
                    (*thread).trace(processor.visitor());
                },
                false,
            );*/
            (*thread).trace(processor.visitor());
        }
    }));
}

pub fn scm_init_vm() {
    init_runtime();
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
            top_entry_frame: null_mut(),
            winders: None,
            vmid: THREAD_ID.fetch_add(1, Ordering::AcqRel),
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
static THREAD_ID: AtomicI32 = AtomicI32::new(i32::MIN);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
pub enum InherentSymbol {
    Quote = 0,
    Quasiquote,
    Unquote,
    UnquoteSplicing,
    Syntax,
    Quasisyntax,
    Unsyntax,
    UnsyntaxSplicing,
    LParen,
    RParen,
    LBrack,
    RBrack,
    Dot,

    Count
}

pub struct InherentSymbols {
    symbols: [Value; InherentSymbol::Count as usize],
}   

impl Index<InherentSymbol> for InherentSymbols {
    type Output = Value;

    fn index(&self, index: InherentSymbol) -> &Self::Output {
        &self.symbols[index as usize]
    }
}

impl InherentSymbols {
    fn new() -> Self {
        let mut symbols = [Value::encode_undefined_value(); InherentSymbol::Count as usize];
        symbols[InherentSymbol::Quote as usize] = "quote".intern().into();
        symbols[InherentSymbol::Quasiquote as usize] = "quasiquote".intern().into();
        symbols[InherentSymbol::Unquote as usize] = "unquote".intern().into();
        symbols[InherentSymbol::UnquoteSplicing as usize] = "unquote-splicing".intern().into();
        symbols[InherentSymbol::Syntax as usize] = "syntax".intern().into();
        symbols[InherentSymbol::Quasisyntax as usize] = "quasisyntax".intern().into();
        symbols[InherentSymbol::Unsyntax as usize] = "unsyntax".intern().into();
        symbols[InherentSymbol::UnsyntaxSplicing as usize] = "unsyntax-splicing".intern().into();
        symbols[InherentSymbol::LParen as usize] = "(".intern().into();
        symbols[InherentSymbol::RParen as usize] = ")".intern().into();
        symbols[InherentSymbol::LBrack as usize] = "[".intern().into();
        symbols[InherentSymbol::RBrack as usize] = "]".intern().into();
        symbols[InherentSymbol::Dot as usize] = ".".intern().into();
        InherentSymbols {
            symbols
        }
    }
}

pub fn inherent_symbols() -> &'static InherentSymbols {
    static SYMBOLS: Lazy<InherentSymbols> = Lazy::new(InherentSymbols::new);

    &SYMBOLS
}

pub struct Winder {
    pub(crate) id: i32,
    pub(crate) before: Value,
    pub(crate) after: Value,
    pub(crate) handlers: Option<Value>,
    pub(crate) next: Option<Handle<Self>>
}

impl Object for Winder {  
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.before.trace(visitor);
        self.after.trace(visitor);
        self.handlers.trace(visitor);
        self.next.trace(visitor);
    }
}

impl Allocation for Winder {}

impl PartialEq for Winder {
    fn eq(&self, other: &Self) -> bool {
        self as *const _ == other as *const _
    }
}

impl Eq for Winder {}

impl Winder {

    pub fn len(&self) -> usize {
        let mut ws = Some(self);
        let mut n = 0;
        while let Some(w) = ws {
            n += 1;
            ws = w.next.as_ref().map(|w| &**w);
        }

        n
    }

    pub fn common_prefix(this: Handle<Self>, other: Handle<Self>) -> Option<Handle<Self>> {
        let mut that = Some(other);
        let mut this = Some(this);

        let this_len = this.unwrap().len();
        let that_len = that.unwrap().len();

        if this_len > that_len {
            for _ in that_len..this_len {
                this = this.unwrap().next;
            }
        } else if that_len > this_len {
            for _ in this_len..that_len {
                that = that.unwrap().next;
            }
        }

        while let Some((this_winder, that_winder)) = this.zip(that).filter(|(x, y)| x != y) {
            this = this_winder.next;
            that = that_winder.next;
        }

        this
    }
}