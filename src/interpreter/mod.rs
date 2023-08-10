use std::{
    mem::size_of,
    panic::AssertUnwindSafe,
    ptr::{null, null_mut},
};

use mmtk::{
    util::Address,
    vm::{edge_shape::SimpleEdge, RootsWorkFactory},
};

use crate::{
    gc::virtual_memory::{PlatformVirtualMemory, VirtualMemory, VirtualMemoryImpl},
    runtime::value::Value,
    vm::{intrinsics::get_callee_vcode, thread::Thread, BOOT_CONTINUATION_CODE},
};

use self::stackframe::{
    frame_dynamic_link, frame_local, frame_previous_sp, set_frame_dynamic_link,
    set_frame_machine_return_address, set_frame_virtual_return_address, StackElement,
};

pub mod llint;
pub mod engine;
pub mod stackframe;

pub const NUM_ENGINES: usize = 1;

#[repr(C)]
pub struct InterpreterState {
    pub ip: *const u8,
    pub sp: *mut StackElement,
    pub fp: *mut StackElement,
    pub entry_fp: *mut StackElement,
    pub prev_fp: *mut StackElement,
    pub prev_entry_fp: *mut StackElement,
    pub stack_limit: *mut StackElement,
    
    /// Disable JIT
    pub disable_mcode: u8,
    pub stack_size: usize,
    pub stack_bottom: *mut StackElement,
    pub stack_top: *mut StackElement,
    pub mra_after_abort: *const u8,
    pub stack_memory: VirtualMemory,
    pub engines: [unsafe extern "C-unwind" fn(&mut Thread) -> Value; 1],
}

#[repr(C)]
pub struct InterpreterContinuation {
    pub vra: *const u32,
    pub mra: *const u8,
    pub fp_offset: isize,
    pub stack_size: isize,
    pub stack_bottom: *mut StackElement,
    pub flags: u32,
}

impl InterpreterState {
    pub fn new() -> Self {
        let mut this = Self {
            ip: null(),
            sp: null_mut(),
            fp: null_mut(),
            prev_entry_fp: null_mut(),
            prev_fp: null_mut(),
            entry_fp: null_mut(),
            stack_limit: null_mut(),
            
            disable_mcode: 0,
            stack_size: 0,
            stack_bottom: null_mut(),
            stack_top: null_mut(),
            mra_after_abort: null(),
            engines: [engine::rust_engine],
            stack_memory: VirtualMemory::null(),
        };
        unsafe {
            this.prepare_stack();
        }
        this
    }

    pub unsafe fn mark_stack_for_roots(&mut self, factory: &mut impl RootsWorkFactory<SimpleEdge>) {
        // walk slots between fp and sp, repeat until stack top

        let mut fp = self.fp;
        let mut sp = self.sp;
        let mut edges = vec![];
        while fp < self.stack_top {
            while sp < fp {
                let value = sp.cast::<Value>();
                if (*value).is_object() {
                    let edge = SimpleEdge::from_address(Address::from_ptr(value));
                    edges.push(edge);
                }
                sp = sp.add(1);
            }
            sp = frame_previous_sp(fp);
            fp = frame_dynamic_link(fp);
        }

        self.return_unused_stack_to_os();
        factory.create_process_edge_roots_work(edges);
    }

    fn return_unused_stack_to_os(&mut self) {
        let mut lo = self.stack_bottom as usize;
        let mut hi = self.sp as usize;
        let pgsz = VirtualMemory::<PlatformVirtualMemory>::page_size();
        lo &= !(pgsz - 1);
        hi &= !(pgsz - 1);

        if lo < hi {
            PlatformVirtualMemory::dontneed(lo as _, hi - lo);
        }
    }

    #[allow(dead_code)]
    unsafe fn expand_stack(
        &mut self,
        old_bottom: *mut StackElement,
        old_size: usize,
        new_size: usize,
    ) {
        if new_size >= usize::MAX / size_of::<Value>() {
            std::process::abort();
        }

        if new_size <= old_size {
            std::process::abort();
        }

        let extension_size = new_size - old_size;

        if (old_bottom as usize) < extension_size * size_of::<Value>() {
            std::process::abort();
        }

        let mut new_bottom = allocate_stack(new_size);

        core::ptr::copy_nonoverlapping(
            old_bottom,
            new_bottom.address().cast::<StackElement>(),
            old_size,
        );

        std::mem::swap(&mut self.stack_memory, &mut new_bottom);
    }

    unsafe fn prepare_stack(&mut self) {
        self.stack_size =
            VirtualMemory::<PlatformVirtualMemory>::page_size() / size_of::<StackElement>();
        self.stack_memory = allocate_stack(self.stack_size);
        self.stack_bottom = self.stack_memory.address().cast::<StackElement>();
        self.stack_top = self.stack_memory.end() as *mut StackElement;
        self.stack_limit = self.stack_bottom;
        self.ip = null();
        self.fp = self.stack_top;
        self.sp = self.stack_top;
    }

    pub unsafe fn push_sp(&mut self, new_sp: *mut StackElement) {
        self.increase_sp(new_sp, false)
    }

    pub unsafe fn restore_sp(&mut self, new_sp: *mut StackElement) {
        self.increase_sp(new_sp, true)
    }

    unsafe fn increase_sp(&mut self, new_sp: *mut StackElement, restore: bool) {
        if !restore && new_sp < self.stack_limit {
            // FIXME: Expand stack
            std::process::abort();
        } else {
            self.sp = new_sp;
        }
    }
}

fn allocate_stack(mut size: usize) -> VirtualMemory {
    size *= size_of::<StackElement>();

    VirtualMemory::<PlatformVirtualMemory>::allocate_aligned(
        size,
        VirtualMemory::<PlatformVirtualMemory>::page_size(),
        false,
        "stack",
    )
    .expect("Failed to allocate stack")
}

pub fn scm_call_n(thread: &mut Thread, proc: Value, args: &[Value]) -> Result<Value, Value> {
    let return_nlocals = 0;
    let call_nlocals = args.len() + 1;
    let frame_size = 3;

    /* Check that we have enough space for the two stack frames: the
    innermost one that makes the call, and its continuation which
    receives the resulting value(s) and returns from the engine
    call.  */
    let stack_reserve_words = call_nlocals + frame_size + return_nlocals + frame_size;
    unsafe {
        let save_prev_fp = thread.interpreter().prev_fp;
        let save_prev_entry_fp = thread.interpreter().prev_entry_fp;

        thread.interpreter().prev_fp = thread.interpreter().fp;
        thread.interpreter().prev_entry_fp = thread.interpreter().entry_fp;
        let new_sp = thread.interpreter().sp.sub(stack_reserve_words);
        thread.interpreter().push_sp(new_sp);
        let call_fp = thread.interpreter().sp.add(call_nlocals);
        let return_fp = call_fp.add(return_nlocals + frame_size);

        set_frame_virtual_return_address(return_fp, thread.interpreter().ip);
        set_frame_machine_return_address(return_fp, null());
        set_frame_dynamic_link(return_fp, thread.interpreter().fp);

        thread.interpreter().ip = BOOT_CONTINUATION_CODE.as_ptr();

        set_frame_virtual_return_address(call_fp, thread.interpreter().ip);
        set_frame_machine_return_address(call_fp, null());
        set_frame_dynamic_link(call_fp, return_fp);
        *frame_local(call_fp, 0) = proc;

        for i in 0..args.len() {
            *frame_local(call_fp, i + 1) = args[i];
        }

        thread.interpreter().fp = call_fp;
        thread.interpreter().entry_fp = return_fp;
        {
            let call = AssertUnwindSafe(|| {
                thread.interpreter().ip = get_callee_vcode(thread);

                (thread.interpreter().engines[0])(thread)
            });

            let result = std::panic::catch_unwind(|| call());
            thread.interpreter().fp = thread.interpreter().prev_fp;
            thread.interpreter().entry_fp = thread.interpreter().prev_entry_fp;
            thread.interpreter().prev_fp = save_prev_fp;
            thread.interpreter().prev_entry_fp = save_prev_entry_fp;
            match result {
                Ok(val) => return Ok(val),
                Err(err) => {
                    if let Some(val) = err.downcast_ref::<Value>() {
                        return Err(*val);
                    } else {
                        std::panic::resume_unwind(err);
                    }
                }
            }
        }
    }
}
