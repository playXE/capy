use std::intrinsics::unlikely;
use std::mem::transmute;
use std::slice::Iter;
use std::sync::atomic::{AtomicI8, Ordering};
use std::{mem::MaybeUninit, panic::AssertUnwindSafe};

use mmtk::memory_manager::bind_mutator;
use mmtk::util::metadata::side_metadata::GLOBAL_SIDE_METADATA_VM_BASE_ADDRESS;
use mmtk::util::ObjectReference;
use mmtk::{Mutator, MutatorContext};

use crate::gc::{CapyVM, ObjEdge};

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

#[repr(C)]
pub struct Thread {
    pub interpreter: MaybeUninit<InterpreterState>,
    pub los_threshold: usize,
    pub needs_wb: bool,
    pub mutator: MaybeUninit<Mutator<CapyVM>>,
    pub shadow_stack: ShadowStack,

    pub id: u64,
    pub safepoint: *mut u8,
    pub gc_state: i8,
    pub kind: ThreadKind,
    pub handles: MaybeUninit<HandleMemory>,
    pub obj_handles: MaybeUninit<ObjStorage>,
    pub local_finalization_queue: MaybeUninit<Vec<(ObjectReference, CleanerType)>>,
}

impl Thread {
    pub fn to_mmtk(&self) -> mmtk::util::VMMutatorThread {
        unsafe { transmute(self) }
    }

    pub fn mutator(&mut self) -> &mut Mutator<CapyVM> {
        unsafe { &mut *self.mutator.as_mut_ptr() }
    }

    pub fn current() -> &'static mut Thread {
        unsafe { &mut THREAD }
    }

    pub fn interpreter(&mut self) -> &mut InterpreterState {
        unsafe { &mut *self.interpreter.as_mut_ptr() }
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

    pub fn request_gc(&self) {
        mmtk::memory_manager::handle_user_collection_request(
            &scm_virtual_machine().mmtk,
            self.to_mmtk(),
        )
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
    #[allow(dead_code)]
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

    pub fn obj_storage(&self) -> &ObjStorage {
        assert!(self.is_registered());
        unsafe { &*self.obj_handles.as_ptr() }
    }

    pub(crate) fn register_mutator(&mut self) {
        self.safepoint = super::safepoint::SAFEPOINT_PAGE.address();
        self.kind = ThreadKind::Mutator;

        let mutator = bind_mutator(&scm_virtual_machine().mmtk, unsafe {
            transmute(Thread::current())
        });
        self.mutator = MaybeUninit::new(*mutator);

        self.handles = MaybeUninit::new(HandleMemory::new());
        let th = threads();
        th.add_thread(self as *mut Thread);
        self.interpreter = MaybeUninit::new(InterpreterState::new(self.mutator()));
        self.shadow_stack.init();
        self.local_finalization_queue = MaybeUninit::new(Vec::with_capacity(128));
        self.obj_handles = MaybeUninit::new(ObjStorage::new("thread-handles"));
        self.needs_wb = scm_virtual_machine().needs_wb;
        self.los_threshold = self
            .mutator()
            .plan
            .constraints()
            .max_non_los_default_alloc_bytes;
    }

    pub(crate) fn deregister_mutator(&mut self) {
        unsafe {
            self.flush_cleaner_queue();
            self.local_finalization_queue.assume_init_drop();
            self.obj_handles.assume_init_drop();
            self.handles.assume_init_drop();
            let th = threads();
            th.remove_current_thread();
        }
    }

    pub fn register_cleaner(&mut self, object: ObjectReference, cleaner: CleanerType) {
        unsafe {
            let queue = self.local_finalization_queue.assume_init_mut();
            let cap = queue.capacity();
            queue.push((object, cleaner));
            if queue.capacity() != cap {
                self.flush_cleaner_queue();
            }
        }
    }

    pub fn flush_cleaner_queue(&mut self) {
        let queue = unsafe { self.local_finalization_queue.assume_init_mut() };
        // Flush queue to global queue
        let vm = scm_virtual_machine();
        // acquire global queue lock, it might enter safepoint while acquiring lock.
        let mut global = vm.finalization_registry.lock(true);
        global.extend(queue.drain(..));
    }

    /// Same as above except does not enter safepoint while acquiring global lock.
    pub(crate) fn flush_cleaner_queue_in_gc(&mut self) {
        let queue = unsafe { self.local_finalization_queue.assume_init_mut() };
        // Flush queue to global queue
        let vm = scm_virtual_machine();

        let mut global = vm.finalization_registry.lock(false);
        global.extend(queue.drain(..));
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

    /// Performs write barrier for objects.
    ///
    /// Arguments:
    /// - `src`: The modified source object.
    /// - `slot`: The location of the field to be modified.
    /// - `target`: The target for the write operation.
    #[inline]
    pub fn reference_write(
        &mut self,
        src: ObjectReference,
        slot: ObjEdge,
        target: ObjectReference,
    ) {
        unsafe {
            // use transmutes here because MMTk calls are inlined only with PGO enabled builds
            // and we still want somewhat decent performance when developing without PGO.
            let addr = transmute::<_, *mut ObjectReference>(slot);
            addr.write(target);
            if self.needs_wb {
                // load unlogged bit from side-metadata.
                // if it is set then we invoke slow-path from MMTk.
                let addr = transmute::<_, usize>(src);
                let meta_addr =
                    transmute::<_, usize>(GLOBAL_SIDE_METADATA_VM_BASE_ADDRESS) + (addr >> 6);
                let shift = (addr >> 3) & 0b111;

                let byte_val = (meta_addr as *const u8).read();
                if unlikely((byte_val >> shift) & 1 == 1) {
                    self.mutator()
                        .barrier()
                        .object_reference_write_slow(src, slot, target);
                }
            }
        }
    }
}

use crate::gc::objstorage::ObjStorage;
use crate::gc::refstorage::HandleMemory;
use crate::gc::shadow_stack::ShadowStack;
use crate::interpreter::InterpreterState;
use crate::runtime::object::CleanerType;
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
        threads
            .iter()
            .filter(|th| (***th).kind == ThreadKind::Mutator)
            .count()
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
    los_threshold: 0,
    needs_wb: false,
    interpreter: MaybeUninit::uninit(),
    mutator: MaybeUninit::uninit(),
    safepoint: std::ptr::null_mut(),
    gc_state: 2,
    shadow_stack: ShadowStack::new(),
    kind: ThreadKind::None,
    handles: MaybeUninit::uninit(),
    obj_handles: MaybeUninit::uninit(),
    local_finalization_queue: MaybeUninit::uninit(),
};
