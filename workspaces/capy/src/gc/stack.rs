use std::{
    mem::size_of,
    ptr::{null, null_mut},
};

use crate::runtime::{thread::Thread, utils::round_up_usize};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct StackBounds {
    pub origin: *mut u8,
    pub bound: *mut u8,
}

impl StackBounds {
    pub const fn none() -> Self {
        Self {
            origin: null_mut(),
            bound: null_mut(),
        }
    }

    pub fn contains(&self, ptr: *const u8) -> bool {
        self.origin >= ptr as *mut u8 && ptr > self.bound
    }
}

#[cfg(any(target_os = "macos", target_os = "ios"))]
impl StackBounds {
    pub unsafe fn new_thread_stack_bounds(thread: libc::pthread_t) -> Self {
        let origin = libc::pthread_get_stackaddr_np(thread);
        let size = libc::pthread_get_stacksize_np(thread);
        let bound = origin.sub(size);
        Self {
            origin: origin.cast(),
            bound: bound.cast(),
        }
    }
    pub fn current_thread_stack_bounds() -> Self {
        unsafe { Self::new_thread_stack_bounds(thread_self() as _) }
    }
}

#[cfg(all(unix, not(any(target_os = "macos", target_os = "ios"))))]
impl StackBounds {
    #[cfg(target_os = "openbsd")]
    unsafe fn new_thread_stack_bounds(thread: libc::pthread_t) -> Self {
        let mut stack: libc::stack_t = core::mem::MaybeUninit::zeroed().assume_init();
        libc::pthread_stackseg_np(thread, &mut stack);
        let origin = stack.ss_sp;
        let bound = stack.origin.sub(stack.ss_size);
        return Self {
            origin: origin.cast(),
            bound: bound.cast(),
        };
    }

    #[cfg(not(target_os = "openbsd"))]
    unsafe fn new_thread_stack_bounds(thread: libc::pthread_t) -> Self {
        let mut bound = core::ptr::null_mut::<libc::c_void>();
        let mut stack_size = 0;
        let mut sattr: libc::pthread_attr_t = core::mem::MaybeUninit::zeroed().assume_init();
        libc::pthread_attr_init(&mut sattr);
        #[cfg(any(target_os = "freebsd", target_os = "netbsd"))]
        {
            libc::pthread_attr_get_np(thread, &mut sattr);
        }
        #[cfg(not(any(target_os = "freebsd", target_os = "netbsd")))]
        {
            libc::pthread_getattr_np(thread, &mut sattr);
        }
        let _rc = libc::pthread_attr_getstack(&sattr, &mut bound, &mut stack_size);
        libc::pthread_attr_destroy(&mut sattr);
        let origin = bound.add(stack_size);
        Self {
            bound: bound.cast(),
            origin: origin.cast(),
        }
    }

    pub fn current_thread_stack_bounds() -> Self {
        unsafe { Self::new_thread_stack_bounds(thread_self() as _) }
    }
}

#[cfg(windows)]
impl StackBounds {
    pub unsafe fn current_thread_stack_bounds_internal() -> Self {
        use winapi::um::memoryapi::*;
        use winapi::um::winnt::*;
        let mut stack_origin: MEMORY_BASIC_INFORMATION =
            core::mem::MaybeUninit::zeroed().assume_init();
        VirtualQuery(
            &mut stack_origin as *mut MEMORY_BASIC_INFORMATION as *mut _,
            &mut stack_origin,
            core::mem::size_of::<MEMORY_BASIC_INFORMATION>(),
        );

        let origin = stack_origin
            .BaseAddress
            .cast::<u8>()
            .add(stack_origin.RegionSize as _);
        // The stack on Windows consists out of three parts (uncommitted memory, a guard page and present
        // committed memory). The 3 regions have different BaseAddresses but all have the same AllocationBase
        // since they are all from the same VirtualAlloc. The 3 regions are laid out in memory (from high to
        // low) as follows:
        //
        //    High |-------------------|  -----
        //         | committedMemory   |    ^
        //         |-------------------|    |
        //         | guardPage         | reserved memory for the stack
        //         |-------------------|    |
        //         | uncommittedMemory |    v
        //    Low  |-------------------|  ----- <--- stackOrigin.AllocationBase
        //
        // See http://msdn.microsoft.com/en-us/library/ms686774%28VS.85%29.aspx for more information.
        let mut uncommitted_memory: MEMORY_BASIC_INFORMATION =
            core::mem::MaybeUninit::zeroed().assume_init();
        VirtualQuery(
            stack_origin.AllocationBase as *mut _,
            &mut uncommitted_memory,
            core::mem::size_of::<MEMORY_BASIC_INFORMATION>(),
        );
        let mut guard_page: MEMORY_BASIC_INFORMATION =
            core::mem::MaybeUninit::zeroed().assume_init();
        VirtualQuery(
            uncommitted_memory
                .BaseAddress
                .cast::<u8>()
                .add(uncommitted_memory.RegionSize as _)
                .cast(),
            &mut guard_page,
            core::mem::size_of::<MEMORY_BASIC_INFORMATION>(),
        );
        let end_of_stack = stack_origin.AllocationBase as *mut u8;
        let bound = end_of_stack.add(guard_page.RegionSize as _);
        Self {
            origin: origin as *mut u8,
            bound,
        }
    }

    pub fn current_thread_stack_bounds() -> Self {
        unsafe { Self::current_thread_stack_bounds_internal() }
    }
}

fn thread_self() -> u64 {
    #[cfg(unix)]
    unsafe {
        libc::pthread_self() as _
    }

    #[cfg(windows)]
    unsafe {
        extern "C" {
            fn GetCurrentThreadId() -> u32;
        }
        GetCurrentThreadId() as u64
    }
}

#[inline(never)]
pub fn approximate_stack_pointer() -> *const *const u8 {
    let mut x: *const *const u8 = null();
    x = &x as *const *const *const u8 as *const *const u8;
    x
}

cfg_if::cfg_if! {
    if #[cfg(all(not(windows), not(target_vendor="apple")))]
    {
        #[derive(Clone, Copy)]
        pub struct PlatformRegisters {
            pub machine_context: libc::mcontext_t
        }

        pub unsafe fn registers_from_ucontext(ucontext: *mut libc::ucontext_t) -> PlatformRegisters {
            #[cfg(target_os="openbsd")]
            {
                ucontext.cast::<PlatformRegisters>().read()
            }

            #[cfg(not(target_os="openbsd"))]
            {
                (&mut (*ucontext).uc_mcontext as *mut _ as *mut PlatformRegisters).read()
            }
        }
    } else {
        #[derive(Clone, Copy)]
        pub struct PlatformRegisters {
            pub stack_pointer: *mut u8
        }


    }
}

pub fn stack_pointer(regs: &PlatformRegisters) -> *const *const u8 {
    cfg_if::cfg_if! {
        if #[cfg(not(target_vendor="apple"))] {

            cfg_if::cfg_if! {
                if #[cfg(target_os="linux")] {
                    #[cfg(target_arch="x86_64")]
                    {
                        regs.machine_context.gregs[libc::REG_RSP as usize] as _
                    }
                    #[cfg(target_arch="arm64")]
                    {
                        regs.machine_context.sp as _
                    }

                    #[cfg(target_arch="riscv64")]
                    {
                        regs.machine_context.__gregs[libc::REG_RSP as usize] as _
                    }
                } else {
                    compile_error!("NYI")
                }
            }
        } else {
            compile_error!("NYI")
        }
    }
}

pub fn frame_pointer(regs: &PlatformRegisters) -> *const *const u8 {
    cfg_if::cfg_if! {
        if #[cfg(not(target_vendor="apple"))] {

            cfg_if::cfg_if! {
                if #[cfg(target_os="linux")] {
                    #[cfg(target_arch="x86_64")]
                    {
                        regs.machine_context.gregs[libc::REG_RBP as usize] as _
                    }
                    #[cfg(target_arch="arm64")]
                    {
                        regs.machine_context.regs[libc::REG_FP as usize] as _
                    }

                    #[cfg(target_arch="riscv64")]
                    {
                        regs.machine_context.__gregs[libc::REG_RBP as usize] as _
                    }
                } else {
                    compile_error!("NYI")
                }
            }
        } else {
            compile_error!("NYI")
        }
    }
}

pub const fn redzone_adjustment() -> isize {
    #[cfg(not(windows))]
    {
        #[cfg(any(target_arch = "x86_64", target_arch = "arm64"))]
        {
            -128
        }
        #[cfg(not(any(target_arch = "x86_64", target_arch = "arm64")))]
        {
            0
        }
    }

    #[cfg(windows)]
    {
        0
    }
}

pub unsafe fn capture_stack(thread: &Thread, stack_top: *const *const u8) -> (*const u8, usize) {
    let mut begin = thread.stack.origin;
    let end = round_up_usize(stack_top as usize, size_of::<usize>(), 0) as *mut u8;

    let mut end_with_redzone = end.offset(redzone_adjustment());

    if end_with_redzone < thread.stack.bound {
        end_with_redzone = thread.stack.bound;
    }

    std::mem::swap(&mut begin, &mut end_with_redzone);
    (begin, end_with_redzone.offset_from(begin) as usize)
}
