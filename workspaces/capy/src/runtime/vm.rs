use std::{
    mem::{size_of, transmute},
    ptr::{null, null_mut, addr_of_mut},
};

use mmtk::{util::Address, vm::RootsWorkFactory};

use crate::{
    gc::{edges::ScmEdge, CapyVM},
    interpreter::{
        StackElement,
    },
};

#[repr(C)]
pub struct VirtualMachine {
    /// Instruction pointer
    pub(crate) ip: *const u8,
    /// Stack pointer
    pub(crate) sp: *mut StackElement,
    /// Frame pointer
    pub(crate) fp: *mut StackElement,
    /// Stack limit address
    pub(crate) stack_limit: *mut StackElement,
    pub(crate) stack_size: usize,
    /// lowest address in allocated stack
    pub(crate) stack_bottom: *mut StackElement,
    /// highest address in allocated stack
    pub(crate) stack_top: *mut StackElement,
    pub(crate) registers: *mut rsetjmp::JumpBuf,
}

impl Drop for VirtualMachine {
    fn drop(&mut self) {
        unsafe {
            free_stack(self.stack_bottom, self.stack_size);
        }
    }
}

impl VirtualMachine {
    pub(crate) fn new() -> Self {
        Self {
            ip: null(),
            sp: null_mut(),
            fp: null_mut(),
            stack_limit: null_mut(),
            stack_size: 0,
            stack_bottom: null_mut(),
            stack_top: null_mut(),
            registers: null_mut(),
        }
    }

    pub(crate) unsafe fn prepare_stack(&mut self) {
        self.stack_size = 4096 / size_of::<StackElement>();
        self.stack_bottom = allocate_stack(self.stack_size);
        self.stack_top = self.stack_bottom.add(self.stack_size);
        self.stack_limit = self.stack_bottom;
        self.ip = null();
        self.fp = self.stack_top;
        self.sp = self.stack_top;
    }

    pub(crate) unsafe fn mark_stack(&mut self, _factory: &mut impl RootsWorkFactory<ScmEdge>) {
       /*  let mut edges = vec![];

        let mut fp = self.fp;
        let mut sp = self.sp;

        while fp < self.stack_top {
            while sp < fp {
                // quick check that `sp` contains a pointer to a heap object.
                // Due to the low-level nature of VM bytecode we might have pointer to non-heap objects on the stack
                // and Value::is_cell() would return true for them as well when MMTk would panic on them.
                if mmtk::memory_manager::is_in_mmtk_spaces::<CapyVM>(transmute(sp.read().as_ptr)) {
                    if (*sp).as_value.is_cell() {
                        edges.push(ScmEdge::from(addr_of_mut!((*sp).as_value)));
                    }
                }

                sp = sp.add(1);
            }
            sp = scm_frame_previous_sp(fp);
            fp = scm_frame_dynamic_link(fp);
        }
        
        factory.create_process_edge_roots_work(edges);*/
    }
}

#[allow(unused_assignments)]
unsafe fn allocate_stack(mut size: usize) -> *mut StackElement {
    size *= size_of::<StackElement>();

    let mut ret = std::ptr::null_mut();

    #[cfg(unix)]
    {
        ret = libc::mmap(
            std::ptr::null_mut(),
            size,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
            -1,
            0,
        ) as *mut StackElement;

        if ret == libc::MAP_FAILED as *mut StackElement {
            ret = std::ptr::null_mut();
        }
    }
    #[cfg(not(unix))]
    {
        ret = mmtk::memory_manager::malloc(size).to_mut_ptr::<StackElement>()
    }

    if ret.is_null() {
        panic!("Failed to allocate stack");
    }

    ret
}

unsafe fn free_stack(stack: *mut StackElement, size: usize) {
    #[cfg(unix)]
    {
        libc::munmap(stack as *mut libc::c_void, size);
    }
    #[cfg(not(unix))]
    {
        mmtk::memory_manager::free(stack as *mut u8, size);
    }
}

#[allow(dead_code)]
unsafe fn expand_stack(
    old_bottom: *mut StackElement,
    old_size: usize,
    new_size: usize,
) -> *mut StackElement {
    let new_bottom = allocate_stack(new_size);

    std::ptr::copy_nonoverlapping(old_bottom, new_bottom, old_size);
    free_stack(old_bottom, old_size);
    new_bottom
}
