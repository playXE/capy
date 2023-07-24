use std::{mem::MaybeUninit, ptr::null_mut};

use libc::*;

use crate::vm::{safepoint, thread::Thread};

unsafe extern "C" fn sigdie_handler(sig: i32, _info: *mut siginfo_t, _context: *mut c_void) {
    let mut sset = MaybeUninit::<libc::sigset_t>::zeroed().assume_init();
    sigfillset(&mut sset);
    sigprocmask(SIG_UNBLOCK, &mut sset, null_mut());
    signal(sig, SIG_DFL);

    if sig != SIGSEGV && sig != SIGBUS && sig != SIGILL {
        raise(sig);
    }

    // fall-through return to re-execute faulting statement (but without the error handler)
}

pub unsafe extern "C" fn handle_sigsegv(
    _sig: i32,
    info: *mut siginfo_t,
    _context: *mut ucontext_t,
) -> bool {
    if safepoint::addr_in_safepoint((*info).si_addr() as _) {
        let thread = Thread::current();

        log::trace!(target: "gc-safepoint", "{:?} reached safepoint", std::thread::current().id());
        // basically spin-loop that waits for safepoint to be disabled
        thread.enter_safepoint();
        log::trace!(target: "gc-safepoint", "{:?} exit safepoint", std::thread::current().id());
        true
    } else {
        false
    }
}

pub unsafe extern "C" fn segv_handler(sig: i32, info: *mut siginfo_t, context: *mut ucontext_t) {
    // polling page was protected and some thread tried to read from it
    // and we got here. Polling page gets protected only when safepoint is requested.
    if safepoint::addr_in_safepoint((*info).si_addr() as usize) {
        let thread = Thread::current();

        log::trace!(target: "gc-safepoint", "{:?} reached safepoint", std::thread::current().id());
        // basically spin-loop that waits for safepoint to be disabled
        thread.enter_safepoint();

        log::trace!(target: "gc-safepoint", "{:?} exit safepoint", std::thread::current().id());
        return;
    }

    println!(
        "FATAL: Unhandled signal {}: {:p}, backtrace: \n{}",
        sig,
        (*info).si_addr(),
        std::backtrace::Backtrace::force_capture()
    );

    sigdie_handler(sig, info, context as _);
}


pub fn install_signal_handlers() {
    unsafe {
        let mut act: sigaction = std::mem::MaybeUninit::<sigaction>::zeroed().assume_init();

        sigemptyset(&mut act.sa_mask);
        act.sa_sigaction = segv_handler as _;
        act.sa_flags = SA_SIGINFO;

        if sigaction(SIGSEGV, &act, null_mut()) < 0 {
            panic!("failed to set SIGSEGV handler for safepoints");
        }

        // on AArch64 SIGBUS is thrown when accessing undefined memory.
        if sigaction(SIGBUS, &act, null_mut()) < 0 {
            panic!("failed to set SIGBUS handler for safepoints");
        }
    }
}
