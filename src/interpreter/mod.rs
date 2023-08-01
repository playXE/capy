use std::{
    mem::size_of,
    ptr::{null, null_mut},
};

use mmtk::{
    util::Address,
    vm::{edge_shape::SimpleEdge, RootsWorkFactory},
};

use crate::{
    gc::virtual_memory::{PlatformVirtualMemory, VirtualMemory, VirtualMemoryImpl},
    runtime::value::Value,
};

use self::stackframe::{frame_dynamic_link, frame_previous_sp, StackElement};

pub mod stackframe;

#[repr(C)]
pub struct InterpreterState {
    pub ip: *const u32,
    pub sp: *mut StackElement,
    pub fp: *mut StackElement,
    pub stack_limit: *mut StackElement,

    /// Flags register
    pub compare_result: Option<std::cmp::Ordering>,
    /// Disable JIT
    pub disable_mcode: u8,
    pub stack_size: usize,
    pub stack_bottom: *mut StackElement,
    pub stack_top: *mut StackElement,
    pub mra_after_abort: *const u8,
    pub stack_memory: VirtualMemory,
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
            stack_limit: null_mut(),
            compare_result: None,
            disable_mcode: 0,
            stack_size: 0,
            stack_bottom: null_mut(),
            stack_top: null_mut(),
            mra_after_abort: null(),
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
                    println!("????");
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
        self.compare_result = None;
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
