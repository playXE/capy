use std::{panic::AssertUnwindSafe, ptr::null_mut};

use dashmap::DashMap;
use rsgc::heap::{
    heap::Heap, region::HeapArguments, root_processor::SimpleRoot, safepoint::SafepointSynchronize,
};

use crate::{prelude::*, utilities::arraylist::ArrayList};

use self::{file_manager::FileManager, source_manager::SourceManager};

pub mod code;
pub mod context;
pub mod eval_error;
pub mod file_manager;
pub mod libraries;
pub mod source_manager;
pub mod stack;
pub mod syntax_rules;

#[allow(dead_code)]
pub struct Runtime {
    heap: &'static mut Heap,
    pub(crate) symtab: SymbolTable,
    pub(crate) eof: Value,
    pub(crate) void: Value,
    pub(crate) documentation: DashMap<usize, String>,
    #[allow(dead_code)]
    pub(crate) empty_arraylist: Handle<ArrayList<Value>>,
    pub(crate) empty_array: Handle<Array<Value>>,
    pub(crate) contexts: Mutex<*mut Context>,
    pub(crate) file_manager: Mutex<FileManager>,
    pub(crate) source_manager: Mutex<SourceManager>,
    pub(crate) loader: Value,
    pub(crate) open_upvalues: Mutex<Option<Handle<Upvalue>>>,
}

static mut RT: *mut Runtime = null_mut();

impl Runtime {
    pub fn heap(&self) -> &Heap {
        &self.heap
    }

    pub fn heap_mut(&mut self) -> &mut Heap {
        &mut self.heap
    }

    fn add_context(&mut self, ctx: *mut context::Context) {
        unsafe {
            let mut contexts = self.contexts.lock(true);
            (*ctx).next = *contexts;
            (*ctx).prev = null_mut();
            if !contexts.is_null() {
                (**contexts).prev = ctx;
            }
            *contexts = ctx;
        }
    }

    fn remove_context(&mut self, ctx: *mut context::Context) {
        unsafe {
            let mut contexts = self.contexts.lock(true);
            if (*ctx).prev.is_null() {
                *contexts = (*ctx).next;
            } else {
                (*(*ctx).prev).next = (*ctx).next;
            }
            if !(*ctx).next.is_null() {
                (*(*ctx).next).prev = (*ctx).prev;
            }
            drop(contexts);
        }
    }

    pub(crate) unsafe fn close_upvalues(&self, after: *const Value) {
        let mut lock = self.open_upvalues.lock(true);
        let mut ls = *lock;
        let mut prev: Option<Handle<Upvalue>> = None;

        while let Some(upval) = ls {
            let next = upval.next();
            let (val, stack_start, stack_end) = upval.stack_location();

            // Check that the upvalue points to correct stack and that it is not closed already.
            if stack_start >= after as usize && val >= after as *mut Value && stack_end > after as usize {
                upval.close();
                if let Some(prev) = prev {
                    prev.set_next(upval.next());
                } else {
                    *lock = upval.next();
                }
            } else {
                prev = Some(upval);
            }

            ls = next;
        }
    }


    #[allow(dead_code)]
    pub(crate) unsafe fn find_upvalue(&self, val: *const Value) -> Option<Handle<Upvalue>> {
        let lock = self.open_upvalues.lock(true);
        let mut ls = *lock;

        while let Some(upval) = ls {
            let (upval_val, stack_start, stack_end) = upval.stack_location();
            if upval_val == val as *mut Value && stack_start <= val as usize && stack_end > val as usize {
                return Some(upval);
            }
            ls = upval.next();
        }

        None
    }

    pub(crate) unsafe fn push_upvalues(&self, upvalues: Handle<Upvalue>) {
        let mut lock = self.open_upvalues.lock(true);
        let mut ls = *lock;

        if ls.is_none() {
            *lock = Some(upvalues);
            return;
        }

        while let Some(upval) = ls {
            let next = upval.next();
            if next.is_none() {
                upval.set_next(Some(upvalues));
                break;
            }
            ls = next;
        }
    }

    pub fn new(heap: &'static mut Heap) -> &'static mut Self {
        let thr = Thread::current();

        let eof = thr.allocate(Eof);
        let void = thr.allocate(Void);
        let empty_array = Array::new(thr, 0, |_, _| Value::UNDEFINED);
        let empty_arraylist = ArrayList::new(thr);
        let empty_arraylist = thr.allocate(empty_arraylist);
        let this = Box::leak(Box::new(Self {
            heap,
            eof: Value::new(eof),
            documentation: DashMap::new(),
            void: Value::new(void),
            contexts: Mutex::new(null_mut()),
            empty_array,
            empty_arraylist,
            symtab: SymbolTable::new(),
            loader: Value::nil(),
            source_manager: Mutex::new(SourceManager::new()),
            file_manager: Mutex::new(FileManager::new()),
            open_upvalues: Mutex::new(None),
        }));

        unsafe {
            RT = this as *mut Runtime;
        }

        this.heap
            .add_root(SimpleRoot::new("Scheme Runtime", "ScmRT", |processor| {
                assert!(SafepointSynchronize::is_at_safepoint());
                let rt = Runtime::get();
                unsafe {
                    // lock without safepoint check because we're already in safepoint.
                    //
                    // TODO: Might deadlock if new Scheme thread gets spawned, GC triggers while `contexts`
                    // are being locked which would result in locked mutex and thread at safepoint. So
                    // this lock would wait for that thread to release it but it won't until safepoint is released.
                    // We should find better workaround for this. Right now unsafe `get_mut()` is used,
                    // it is not ideal, but should mostly be fine if we're already at safepoint?

                    /* let contexts = rt.contexts.lock(false); */
                    let contexts = rt.contexts.get_mut(); // workaround for deadlock

                    let mut ctx = *contexts;

                    while !ctx.is_null() {
                        (*ctx).roots(processor);
                        ctx = (*ctx).next;
                    }

                    drop(contexts);
                }

                rt.eof.trace(processor.visitor());
                rt.void.trace(processor.visitor());

                library_manager().trace(processor.visitor());

                rt.symtab.trace(processor.visitor());
                rt.loader.trace(processor.visitor());

                for node in rt.documentation.iter() {
                    processor.visitor().visit(*node.key() as *const u8);
                }

                let open_upvalues = rt.open_upvalues.lock(false);

                let mut upvalue = *open_upvalues;

                while let Some(val) = upvalue {
                    val.trace(processor.visitor());
                    upvalue = unsafe { val.next() };
                }

                drop(open_upvalues);
            }));

        let _ = library_manager();
        libraries::core::core_library(this);
        libraries::control_flow::control_flow(this);
        this
    }

    pub fn get() -> &'static mut Self {
        unsafe { &mut *RT }
    }

    pub fn symbol_table(&self) -> &SymbolTable {
        &self.symtab
    }
}

pub fn scm_main_thread<R>(f: impl Fn(&mut Context) -> R) -> R {
    let args = HeapArguments::from_env();
    let f = AssertUnwindSafe(f);
    match rsgc::thread::main_thread(args, move |heap| unsafe {
        heap.add_core_root_set();
        let rt = Runtime::new(rsgc::heap::heap::heap());
        let main_ctx = Context::new(rt, 1024, Thread::current());
        let res = f(main_ctx);

        let _ = Box::from_raw(main_ctx);

        Ok(res)
    }) {
        Err(_) => unreachable!(),
        Ok(val) => val,
    }
}
