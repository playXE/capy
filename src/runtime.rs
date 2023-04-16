pub mod bigint;
pub mod context;
pub mod env;
pub mod factory;
pub mod object;
pub mod pure_nan;
pub mod list;
pub mod value;
pub mod fmt;

use dashmap::DashMap;
use rsgc::{
    prelude::{Handle, Object, Visitor},
    sync::mutex::{Mutex, RawMutex},
    thread::Thread, heap::root_processor::SimpleRoot,
};

use self::{object::ScmSymbol, value::{Type, ScmValue}};

pub struct Runtime {
    contexts: Mutex<*mut context::Context>,
    symbols: Symbols,
}

impl Runtime {
    pub fn context<'a>(&'a self) -> &mut context::Context {
        if let Some(ctx) = context::get_context() {
            ctx
        } else {
            unsafe {
                self.attach_context();
            }
            context::get_context().unwrap()
        }
    }

    unsafe fn attach_context(&self) {
        let thread = Thread::current();
        assert!(
            thread.is_registered(),
            "Current thread is not attached to RSGC runtime"
        );
        let ctx = Box::new(context::Context::new(thread));

        let mut contexts = self.contexts.lock(true);

        let ptr = Box::into_raw(ctx);

        (*ptr).next = *contexts;
        (*ptr).prev = std::ptr::null_mut();

        if !(*contexts).is_null() {
            (**contexts).prev = ptr;
        }

        *contexts = ptr;

        context::CONTEXT.with(|cptr| {
            cptr.get().write(ptr);
        });
    }

    pub fn intern(&self, name: impl AsRef<str>) -> Handle<ScmSymbol> {
        self.symbols.intern(name)
    }

    fn visit_roots(&self, visitor: &mut dyn Visitor) {
        let contexts = self.contexts.lock(false);

        unsafe {
            let mut ptr = *contexts;

            while !ptr.is_null() {
                (*ptr).visit_roots(visitor);
                ptr = (*ptr).next;
            }

            for sym in self.symbols.symbols.iter() {
                sym.value().trace(visitor);
            }
        }
    }

    pub fn get() -> Option<&'static mut Self> {
        unsafe {
            if RUNTIME.is_null() {
                None
            } else {
                Some(&mut *RUNTIME)
            }
        }
    }

    pub fn new() -> &'static mut Self {
        unsafe {
            assert!(RUNTIME.is_null(), "Runtime is already initialized");
            RUNTIME = Box::into_raw(Box::new(Self {
                contexts: Mutex::new(std::ptr::null_mut()),
                symbols: Symbols::new(),
            }));

            rsgc::heap::heap::heap().add_root(SimpleRoot::new("runtime", "RT", |processor| {
                let runtime = RUNTIME.as_mut().unwrap();
                runtime.visit_roots(processor.visitor());
            }));

            &mut *RUNTIME
        }
    }
}



static mut RUNTIME: *mut Runtime = std::ptr::null_mut();

struct Symbols {
    symbols: DashMap<&'static str, Handle<ScmSymbol>>,
}

impl Symbols {
    fn new() -> Self {
        Self {
            symbols: DashMap::new(),
        }
    }

    fn intern(&self, name: impl AsRef<str>) -> Handle<ScmSymbol> {
        let name = name.as_ref();
        if let Some(sym) = self.symbols.get(name) {
            sym.value().clone()
        } else {
            let mut sym = Thread::current().allocate_varsize::<ScmSymbol>(name.len() + 1);

            let sym = unsafe {
                let sym_ref = sym.assume_init_mut();
                sym_ref.header.typ = Type::Symbol;
                sym_ref.header.lock = RawMutex::INIT;
                sym_ref
                    .data
                    .as_mut_ptr()
                    .copy_from_nonoverlapping(name.as_ptr(), name.len());
                sym_ref.data.as_mut_ptr().add(name.len()).write(0);
                sym.assume_init()
            };
            let name = unsafe {
                std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                    sym.data.as_ptr(),
                    name.len(),
                ))
            };
            self.symbols.insert(name, sym.clone());
            sym
        }
    }
}

pub fn intern(name: impl AsRef<str>) -> ScmValue {
    Runtime::get().unwrap().intern(name).into()
}

