use std::{
    intrinsics::{likely, unlikely},
    mem::{align_of, size_of, transmute, MaybeUninit},
    panic::AssertUnwindSafe,
    ptr::{null, null_mut},
    sync::atomic::{AtomicI8, Ordering},
};

use crate::{
    gc::{
        edges::SHIFT,
        ptr_compr::HeapCompressionScheme,
        shadow_stack::ShadowStack,
        stack::{PlatformRegisters, StackBounds},
        CapyVM,
    },
    runtime::{
        cell::{SchemeHeader, OBJECT_REF_OFFSET},
        factory::align_allocation,
    },
    sync::mutex::{Condvar, Mutex, MutexGuard}, interpreter::{entry_frame::EntryFrame, stackframe::CallFrame},
};
use mmtk::{
    memory_manager::bind_mutator,
    util::{
        alloc::{AllocatorSelector, BumpAllocator, ImmixAllocator},
        metadata::side_metadata::GLOBAL_SIDE_METADATA_VM_BASE_ADDRESS,
        Address, ObjectReference,
    },
    Mutator, MutatorContext,
};

use super::{
    cell::{CellReference, CellTag},
    utils::round_up_usize,
    value::{TaggedValue, Value},
    GCPlan, Runtime, GC_PLAN,
};

// gc_state = 1 means the thread is doing GC or is waiting for the GC to
//              finish.
pub const GC_STATE_WAITING: i8 = 1;
// gc_state = 2 means the thread is running unmanaged code that can be
//              execute at the same time with the GC.
pub const GC_STATE_SAFE: i8 = 2;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ThreadKind {
    None,
    Mutator,
    GCWorker,
    GCController,
}

#[repr(C)]
pub struct Thread {
    pub top_entry_frame: *mut EntryFrame,
    pub top_call_frame: *mut CallFrame,
    pub mmtk: MaybeUninit<MMTKLocalState>,
    pub selector: AllocatorSelector,
    pub stack: StackBounds,
    pub platform_registers: Option<PlatformRegisters>,
    pub approx_sp: *const *const u8,
    pub id: u64,
    pub safepoint_addr: *mut u8,
    pub gc_state: i8,
    pub kind: ThreadKind,
}

impl Thread {
    pub fn is_mutator(&self) -> bool {
        self.kind == ThreadKind::Mutator
    }

    pub fn to_mmtk(&self) -> mmtk::util::VMMutatorThread {
        unsafe { std::mem::transmute(self) }
    }

    pub fn mutator(&mut self) -> &mut Box<Mutator<CapyVM>> {
        unsafe { &mut (*self.mmtk.as_mut_ptr()).mutator }
    }

    pub fn atomic_gc_state(&self) -> &AtomicI8 {
        unsafe { std::mem::transmute(&self.gc_state) }
    }

    #[inline]
    pub unsafe fn gc_state_set(&mut self, state: i8, old_state: i8) -> i8 {
        self.atomic_gc_state().store(state, Ordering::Release);
        if old_state != 0 && state == 0 && !self.safepoint_addr.is_null() {
            self.safepoint();
        }

        old_state
    }

    pub fn request_gc(&self) {
        mmtk::memory_manager::handle_user_collection_request(&Runtime::get().mmtk, self.to_mmtk())
    }

    pub unsafe fn initialize_main(&mut self) {
        mmtk::memory_manager::initialize_collection(Runtime::get().mmtk(), transmute(self));
        Runtime::get().init_main();
    }

    pub unsafe fn state_save_and_set(&mut self, state: i8) -> i8 {
        self.gc_state_set(state, self.gc_state)
    }

    /// Returns pointer to safepoint page. When JITing your code you can directly
    /// inline safepoint poll into your code.
    pub unsafe fn safepoint_page(&self) -> *mut u8 {
        self.safepoint_addr
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
        let safepoint = self.safepoint_addr;

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
            crate::gc::safepoint::wait_gc();
        }
        self.atomic_gc_state().store(state, Ordering::Release);
    }

    pub fn current() -> &'static mut Thread {
        unsafe { &mut THREAD }
    }

    pub fn get_registers(&self) -> (&PlatformRegisters, usize) {
        (
            self.platform_registers.as_ref().unwrap(),
            size_of::<PlatformRegisters>(),
        )
    }


    pub(crate) fn register_worker(&mut self, controller: bool) {
        self.kind = if controller {
            ThreadKind::GCController
        } else {
            ThreadKind::GCWorker
        };

        let th = threads();
        th.add_thread(Thread::current());
    }

    pub unsafe fn register_mutator(&mut self) {
        self.safepoint_addr = crate::gc::safepoint::SAFEPOINT_PAGE.address();
        self.kind = ThreadKind::Mutator;

        let mutator = bind_mutator(&Runtime::get().mmtk, unsafe {
            transmute(Thread::current())
        });

        let local_state = MMTKLocalState {
            bump_pointer: 0,
            bump_limit: 0,
            base: if cfg!(feature = "compression-oops") {
                HeapCompressionScheme::base()
            } else {
                0
            },
            shift: SHIFT,
            los_threshold: 0,
            shadow_stack: ShadowStack::new(),
            needs_write_barrier: false,
            inline_alloc: false,
            mutator,
            selector: AllocatorSelector::None,
        };
        let th = threads();
        th.add_thread(self as *mut Thread);

        self.mmtk = MaybeUninit::new(local_state);
        self.stack = StackBounds::current_thread_stack_bounds();
        self.mmtk().init();
    }

    pub unsafe fn deregister_mutator(&mut self) {
        threads().remove_current_thread();
        self.mmtk().store_bump_pointer();
        mmtk::memory_manager::destroy_mutator(&mut self.mmtk().mutator);
        self.kind = ThreadKind::None;
    }

    pub fn mmtk(&mut self) -> &mut MMTKLocalState {
        unsafe { &mut *self.mmtk.as_mut_ptr() }
    }

    /// Writes `value` to `cell[offset]`. Performs write-barrier if necessary.
    pub fn reference_write<T>(
        &mut self,
        mut cell: CellReference<T>,
        offset: usize,
        value: TaggedValue,
    ) {
        // SAFETY: currently all GCs in mmtk-core use post-write barrier.
        // just set the value and only then check if we need to do write barrier.
        unsafe { cell.value_set_unchecked(offset, value) }
        let mmtk = self.mmtk();

        if mmtk.needs_write_barrier && value.is_cell() {
            unsafe {
                let addr = transmute::<_, usize>(cell);
                let meta_addr =
                    transmute::<_, usize>(GLOBAL_SIDE_METADATA_VM_BASE_ADDRESS) + (addr >> 6);
                let shift = (addr >> 3) & 0b111;

                let byte_val = (meta_addr as *const u8).read();

                if unlikely((byte_val >> shift) & 1 == 1) {
                    let src = cell.0;
                    let slot = cell.edge(offset);
                    let target = value
                        .get_cell_reference_fast::<()>(cell.to_ptr() as _)
                        .cell()
                        .0;
                    mmtk.mutator
                        .barrier()
                        .object_reference_write_slow(src, slot, target);
                }
            }
        }
    }

    pub fn shadow_stack(&mut self) -> &mut ShadowStack {
        &mut self.mmtk().shadow_stack
    }
}

/// Encapsulates MMTK mutator and various state needed for allocation.
///
/// In our binding we inline allocations for all Immix GC plans and GenCopy/Semispace GC plans.
///
/// MarkCompact allocation is not inlined due to the need of VO bits which requrie post-allocation
/// operation to be performed.
///
/// MarkSweep is not yet supported to be inlined.
pub struct MMTKLocalState {
    pub bump_pointer: usize,
    pub bump_limit: usize,
    pub base: usize,
    pub shift: usize,
    pub los_threshold: usize,
    pub inline_alloc: bool,
    pub needs_write_barrier: bool,
    pub shadow_stack: ShadowStack,
    pub mutator: Box<Mutator<CapyVM>>,
    pub selector: AllocatorSelector,
}

impl MMTKLocalState {
    fn init(&mut self) {
        self.los_threshold = self
            .mutator
            .plan
            .constraints()
            .max_non_los_default_alloc_bytes;
        self.needs_write_barrier = match GC_PLAN.get().unwrap() {
            GCPlan::StickyImmix => true,
            _ => false,
        };

        self.selector = mmtk::memory_manager::get_allocator_mapping(
            Runtime::get().mmtk(),
            mmtk::AllocationSemantics::Default,
        );

        self.fetch_bump_pointer();

        if let AllocatorSelector::None = self.selector {
            self.inline_alloc = false;
        } else {
            self.inline_alloc = true;
            log::info!("GC inline allocation enabled");
        }
        self.shadow_stack.init();
    }

    fn fetch_bump_pointer(&mut self) {
        let bump_pointer = unsafe {
            let selector = self.selector;
            match selector {
                AllocatorSelector::BumpPointer(_) => {
                    self.mutator
                        .allocator_impl::<BumpAllocator<CapyVM>>(selector)
                        .bump_pointer
                }
                AllocatorSelector::Immix(_) => {
                    self.mutator
                        .allocator_impl::<ImmixAllocator<CapyVM>>(selector)
                        .bump_pointer
                }
                _ => {
                    self.selector = AllocatorSelector::None;

                    return;
                }
            }
        };

        self.bump_pointer = bump_pointer.cursor.as_usize();
        self.bump_limit = bump_pointer.limit.as_usize();
    }

    fn store_bump_pointer(&mut self) {
        unsafe {
            let selector = self.selector;
            match selector {
                AllocatorSelector::BumpPointer(_) => self
                    .mutator
                    .allocator_impl_mut::<BumpAllocator<CapyVM>>(selector)
                    .bump_pointer
                    .reset(
                        Address::from_usize(self.bump_pointer),
                        Address::from_usize(self.bump_limit),
                    ),
                AllocatorSelector::Immix(_) => {
                    self.mutator
                        .allocator_impl_mut::<ImmixAllocator<CapyVM>>(selector)
                        .bump_pointer
                        .reset(
                            Address::from_usize(self.bump_pointer),
                            Address::from_usize(self.bump_limit),
                        );
                }
                _ => {}
            }
        };
    }

    #[inline]
    pub fn alloc_small(&mut self, size: usize, type_id: CellTag) -> CellReference {
        debug_assert!(size <= self.los_threshold);

        let res = align_allocation(self.bump_pointer, align_of::<usize>(), 0);

        if likely(res + size <= self.bump_limit) {
            let objref = res as *mut SchemeHeader;
            unsafe {
                objref.write(SchemeHeader::new(type_id));

                self.bump_pointer = res + size;

                return std::mem::transmute(objref.add(1));
            }
        }

        self.alloc_slow(size, type_id)
    }
    #[inline]
    pub fn alloc(&mut self, size: usize, type_id: CellTag) -> CellReference {
        // do not check for `inline_alloc` here. If it is false
        // then `bump_pointer + size <= bump_limit` will fail.
        if likely(size <= self.los_threshold) {
            let res = align_allocation(self.bump_pointer, 16, 0);

            if likely(res + size <= self.bump_limit) {
                let objref = res as *mut SchemeHeader;
                // SAFETY:
                // - `objref` is not NULL and is aligned to 8 bytes.
                // - `post_alloc` action is not required, all allocations that require it
                // go to `alloc_slow`
                //
                unsafe {
                    objref.write(SchemeHeader::new(type_id));

                    self.bump_pointer = res + size;

                    return std::mem::transmute(objref.add(1));
                }
            }
        }
        self.alloc_slow(size, type_id)
    }
    #[cold]
    #[inline(never)]
    pub fn alloc_slow(&mut self, size: usize, type_id: CellTag) -> CellReference {
        let size = round_up_usize(size, 8, 0);
        if size > self.los_threshold {
            log::debug!(target: "capy", "LOS allocation of {} bytes requested", size);
            self.alloc_los(size, type_id)
        } else if self.inline_alloc {
            unsafe {
                log::debug!(target: "capy", "Inline allocation of {} bytes failed, falling back to slow path", size);
                self.store_bump_pointer();
                let mem = self.mutator.alloc_slow(
                    size,
                    align_of::<usize>(),
                    0,
                    mmtk::AllocationSemantics::Default,
                );

                let objref: ObjectReference = std::mem::transmute(mem + OBJECT_REF_OFFSET);
                let cell = CellReference::<()>(objref, Default::default());
                *cell.header_mut() = SchemeHeader::new(type_id);
                self.mutator
                    .post_alloc(objref, size, mmtk::AllocationSemantics::Default);
                self.fetch_bump_pointer();
                cell
            }
        } else {
            unsafe {
                let mem = self.mutator.alloc(
                    size,
                    align_of::<usize>(),
                    0,
                    mmtk::AllocationSemantics::Default,
                );
                let objref = mem.to_mut_ptr::<SchemeHeader>();
                objref.write(SchemeHeader::new(type_id));
                let objref = std::mem::transmute(objref.add(1));

                self.mutator
                    .post_alloc(objref, size, mmtk::AllocationSemantics::Default);
                CellReference(objref, Default::default())
            }
        }
    }

    pub fn alloc_immortal(&mut self, size: usize, type_id: CellTag) -> CellReference {
        unsafe {
            let size = round_up_usize(size, align_of::<usize>(), 0);
            let mem = self.mutator.alloc(
                size,
                align_of::<usize>(),
                OBJECT_REF_OFFSET as _,
                mmtk::AllocationSemantics::Immortal,
            );
            let objref = mem.to_mut_ptr::<SchemeHeader>();

            objref.write(SchemeHeader::new(type_id));
            let objref = std::mem::transmute(objref.add(1));
            self.mutator
                .post_alloc(objref, size, mmtk::AllocationSemantics::Immortal);
            println!("alloc {:p}", objref.to_raw_address().to_ptr::<u8>());
            CellReference(objref, Default::default())
        }
    }

    fn alloc_los(&mut self, size: usize, type_id: CellTag) -> CellReference {
        unsafe {
            let mem =
                self.mutator
                    .alloc(size, align_of::<usize>(), 0, mmtk::AllocationSemantics::Los);
            let objref = mem.to_mut_ptr::<SchemeHeader>();

            objref.write(SchemeHeader::new(type_id));
            let objref = std::mem::transmute(objref.add(1));
            self.mutator
                .post_alloc(objref, size, mmtk::AllocationSemantics::Los);
            CellReference(objref, Default::default())
        }
    }
}

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
            thread.mmtk.assume_init_mut().mutator.on_destroy();
            thread.mmtk.assume_init_drop();
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

            thread.safepoint_addr = &mut SINK;
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

    pub unsafe fn iter_unlocked(&self) -> std::slice::Iter<*mut Thread> {
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
    top_entry_frame: null_mut(),
    top_call_frame: null_mut(),
    stack: StackBounds {
        bound: null_mut(),
        origin: null_mut(),
    },
    platform_registers: None,
    approx_sp: null(),
    selector: AllocatorSelector::None,
    mmtk: MaybeUninit::uninit(),
    safepoint_addr: null_mut(),
    gc_state: 2,
    kind: ThreadKind::None,
};
