use std::mem::transmute;
use std::slice::Iter;
use std::sync::atomic::{AtomicI8, Ordering};
use std::{mem::MaybeUninit, panic::AssertUnwindSafe};

use mmtk::memory_manager::bind_mutator;
use mmtk::Mutator;

use crate::gc::CapyVM;

// gc_state = 1 means the thread is doing GC or is waiting for the GC to
//              finish.
pub const GC_STATE_WAITING: i8 = 1;
// gc_state = 2 means the thread is running unmanaged code that can be
//              execute at the same time with the GC.
pub const GC_STATE_SAFE: i8 = 2;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum ThreadKind {
    None,
    Mutator,
    Controller,
    Worker,
}

pub struct Thread {
    pub mutator: MaybeUninit<Mutator<CapyVM>>,
    pub id: u64,
    pub safepoint: *mut u8,
    pub gc_state: i8,
    pub kind: ThreadKind,

}

impl Thread {
    pub fn mutator(&mut self) -> &mut Mutator<CapyVM> {
        unsafe { &mut *self.mutator.as_mut_ptr() }
    }

    pub fn current() -> &'static mut Thread {
        unsafe { &mut THREAD }
    }

    pub fn atomic_gc_state(&self) -> &AtomicI8 {
        unsafe { std::mem::transmute(&self.gc_state) }
    }

    #[inline]
    pub unsafe fn gc_state_set(&mut self, state: i8, old_state: i8) -> i8 {
        self.atomic_gc_state().store(state, Ordering::Release);
        if old_state != 0 && state == 0 && !self.safepoint.is_null() {
            self.safepoint();
        }

        old_state
    }

    pub unsafe fn state_save_and_set(&mut self, state: i8) -> i8 {
        self.gc_state_set(state, self.gc_state)
    }

    /// Returns pointer to safepoint page. When JITing your code you can directly
    /// inline safepoint poll into your code.
    pub unsafe fn safepoint_page(&self) -> *mut u8 {
        self.safepoint
    }

    /// Returns true if safepoints are conditional in this build of RSGC.
    pub const fn is_conditional_safepoint() -> bool {
        cfg!(feature = "conditional-safepoint")
    }

    /// Reads from polling page. If safepoint is disabled nothing happens
    /// but when safepoint is enabled this triggers page fault (SIGSEGV/SIGBUS on Linux/macOS/BSD)
    /// and goes into signal to suspend thread.
    ///
    /// # Note
    ///
    /// Enable `conditional-safepoint` feature when running in LLDB/GDB, otherwise safepoint events
    /// will be treatened as segfault by debuggers.
    #[inline(always)]
    pub fn safepoint(&mut self) {
        std::sync::atomic::compiler_fence(Ordering::SeqCst);
        let safepoint = self.safepoint;

        // Two paths here if conditional safepoints disabled:
        //
        // 1) Safepoint page is not armed, so read does not cause anythin
        // 2) Safepoint page is armed, read causes SIGBUG/SIGSEGV and we go into signal handler
        //    where we would wait for safepoint to be disabled using condvar + mutex.
        //
        let val = unsafe { safepoint.read_volatile() };
        let _ = val;
        #[cfg(feature = "conditional-safepoint")]
        {
            // In case of conditional safepoint armed safepoint value is just set to non-zero.
            // If it is non-zero we go to condvar + mutex wait loop
            if val != 0 {
                self.enter_conditional();
            }
        }
        std::sync::atomic::compiler_fence(Ordering::SeqCst);
    }

    #[inline(never)]
    #[cold]
    fn enter_conditional(&mut self) {
        self.enter_safepoint();
    }

    /// Sets last stack pointer for a thread, waits for safepoint to be disabled and executes
    /// tasks that are needed to execute at safepoint for this thread.
    pub(crate) fn enter_safepoint(&mut self) {
        self.set_gc_and_wait();
    }

    pub(crate) fn set_gc_and_wait(&mut self) {
        let state = self.gc_state;
        self.atomic_gc_state()
            .store(GC_STATE_WAITING, Ordering::Release);
        unsafe {
            super::safepoint::wait_gc();
        }
        self.atomic_gc_state().store(state, Ordering::Release);
    }

    /// Returns true if thread is registered in a GC.
    pub fn is_registered(&self) -> bool {
        !self.safepoint.is_null() && unsafe { self.safepoint != &mut SINK }
    }

    pub(crate) fn register_mutator(&mut self) {
        self.safepoint = super::safepoint::SAFEPOINT_PAGE.address();
        self.kind = ThreadKind::Mutator;
        let th = threads();
        th.add_thread(Thread::current());

        for _ in 0..3 {
            self.safepoint();
        }

        let mutator = bind_mutator(&scm_virtual_machine().mmtk, unsafe { transmute(self) });

        Thread::current().mutator = MaybeUninit::new(*mutator);
    }

    pub(crate) fn register_worker(&mut self, controller: bool) {
        self.kind = if controller {
            ThreadKind::Controller
        } else {
            ThreadKind::Worker
        };

        let th = threads();
        th.add_thread(Thread::current());
    }
}

use crate::vm::sync::mutex::*;

use super::scm_virtual_machine;

pub struct Threads {
    pub threads: Mutex<Vec<*mut Thread>>,
    pub cv_join: Condvar,
}

impl Threads {
    pub fn new() -> Self {
        Self {
            threads: Mutex::new(vec![]),
            cv_join: Condvar::new(),
        }
    }

    pub fn add_thread(&self, thread: *mut Thread) {
        let mut threads = self.threads.lock(false);
        threads.push(thread);
    }

    pub fn remove_current_thread(&self) {
        unsafe {
            let thread = Thread::current();
            thread.mutator.assume_init_mut().on_destroy();
            thread.mutator.assume_init_drop();
            let raw = thread as *mut Thread;

            safepoint_scope(|| {
                let mut threads = self.threads.lock(true);
                threads.retain(|th| {
                    let th = *th;
                    if th == raw {
                        false
                    } else {
                        true
                    }
                });
            });

            thread.safepoint = &mut SINK;
            self.cv_join.notify_all();
        }
    }

    pub fn join_all(&self) {
        let mut threads = self.threads.lock(true);

        while threads.len() > 0 {
            self.cv_join.wait(&mut threads);
        }
    }

    pub fn get(&self) -> MutexGuard<'_, Vec<*mut Thread>> {
        let threads = self.threads.lock(false);
        threads
    }

    pub unsafe fn num(&self) -> usize {
        let threads = self.threads.unsafe_get();
        threads.len()
    }

    pub unsafe fn iter_unlocked(&self) -> Iter<*mut Thread> {
        let threads = self.threads.unsafe_get();
        threads.iter()
    }
}

unsafe impl Sync for Threads {}
unsafe impl Send for Threads {}

static THREADS: once_cell::sync::Lazy<Threads> = once_cell::sync::Lazy::new(Threads::new);

pub(crate) fn threads() -> &'static Threads {
    &THREADS
}

/// Enters safepoint scope. This means that current thread is in "safe" state and GC can run.
///
/// Note that `cb` MUST not invoke any GC code or access GC objects otherwise UB will happen.
pub fn safepoint_scope_conditional<R>(enter: bool, cb: impl FnOnce() -> R) -> R {
    let thread = Thread::current();

    unsafe {
        let state = thread.state_save_and_set(if enter { GC_STATE_SAFE } else { 0 });

        let cb = AssertUnwindSafe(cb);
        let result = match std::panic::catch_unwind(move || cb()) {
            Ok(result) => result,
            Err(err) => {
                std::panic::resume_unwind(err);
            }
        };

        thread.gc_state_set(state, if enter { GC_STATE_SAFE } else { 0 });

        result
    }
}

/// Enters safepoint scope. This means that current thread is in "safe" state and GC can run.
///
/// Note that `cb` MUST not invoke any GC code or access GC objects otherwise UB will happen.
pub fn safepoint_scope<R>(cb: impl FnOnce() -> R) -> R {
    safepoint_scope_conditional(true, cb)
}

static mut SINK: u8 = 0;

#[thread_local]
static mut THREAD: Thread = Thread {
    id: 0,
    mutator: MaybeUninit::uninit(),
    safepoint: std::ptr::null_mut(),
    gc_state: 2,
    kind: ThreadKind::None,
};
