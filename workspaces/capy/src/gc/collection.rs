use super::{safepoint::SafepointSynchronize, stack::{registers_from_ucontext, approximate_stack_pointer}, CapyVM};
use crate::{
    runtime::{
        thread::{threads, Thread, ThreadKind},
        Runtime,
    },
    sync::mutex::MutexGuard,
};
use mmtk::{
    memory_manager::{start_control_collector, start_worker},
    vm::{Collection, GCThreadContext},
};
use std::{
    mem::{transmute, MaybeUninit},
    sync::{Arc, Barrier},
};
pub struct VMCollection;

impl Collection<CapyVM> for VMCollection {
    fn spawn_gc_thread(_tls: mmtk::util::VMThread, ctx: mmtk::vm::GCThreadContext<CapyVM>) {
        match ctx {
            GCThreadContext::Controller(mut controller) => {
                let b1 = Arc::new(Barrier::new(2));
                let b2 = b1.clone();

                std::thread::spawn(move || unsafe {
                    b2.wait();
                    let tls = Thread::current();
                    tls.register_worker(true);

                    start_control_collector(&Runtime::get().mmtk, transmute(tls), &mut controller);
                    threads().remove_current_thread();
                });

                b1.wait();
            }

            GCThreadContext::Worker(mut worker) => {
                let b1 = Arc::new(Barrier::new(2));
                let b2 = b1.clone();

                std::thread::spawn(move || unsafe {
                    b2.wait();
                    let tls = Thread::current();
                    tls.register_worker(false);
                    start_worker(&Runtime::get().mmtk, transmute(tls), &mut worker);
                    threads().remove_current_thread();
                });

                b1.wait();
            }
        }
    }

    fn stop_all_mutators<F>(_: mmtk::util::VMWorkerThread, mut mutator_visitor: F)
    where
        F: FnMut(&'static mut mmtk::Mutator<CapyVM>),
    {
        unsafe {
            // Sets safepoint page to `PROT_NONE` and waits for all threads to enter safepoint.
            // Some threads might enter through invocation of `safepoint_scope` function.
            // One place where this is done is inside `Mutex::lock`.
            let mutators = SafepointSynchronize::begin();

            for &mutator in mutators
                .iter()
                .filter(|&&x| (*x).kind == ThreadKind::Mutator)
            {
                mutator_visitor(&mut (*mutator).mmtk.assume_init_mut().mutator);
            }

            std::mem::forget(mutators);
        }
    }

    fn resume_mutators(_: mmtk::util::VMWorkerThread) {
        let threads = threads();

        unsafe {
            let guard = MutexGuard {
                mutex: &threads.threads,
                safepoint: false,
            };

            SafepointSynchronize::end(guard);

            Runtime::get().gc_waiters.notify_all();
        }
    }

    fn out_of_memory(_tls: mmtk::util::VMThread, err_kind: mmtk::util::alloc::AllocationError) {
        eprintln!("Out of memory: {:?}", err_kind);
        std::process::exit(1);
    }

    fn block_for_gc(tls: mmtk::util::VMMutatorThread) {
        let rt = Runtime::get();

        unsafe {
            let mut ucontext = MaybeUninit::uninit();
            libc::getcontext(ucontext.as_mut_ptr());

            let thread: &'static mut Thread = transmute(tls);
            thread.platform_registers = Some(registers_from_ucontext(ucontext.as_mut_ptr()));
            thread.approx_sp = approximate_stack_pointer();
            let mut ml = rt.gc_waiters.lock(true);

            ml.wait();

            thread.platform_registers = None;
        }
    }

    fn schedule_finalization(_tls: mmtk::util::VMWorkerThread) {}

    fn vm_live_bytes() -> usize {
        0
    }
}
