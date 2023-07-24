use once_cell::sync::Lazy;
use parking_lot::{lock_api::RawMutex, Condvar, Mutex};
use std::sync::atomic::{AtomicU32, AtomicU8, Ordering};
#[cfg(target_family = "windows")]
use winapi::um::{memoryapi::*, winnt::*};

use crate::{
    vm::sync::mutex::MutexGuard,
    vm::thread::{threads, ThreadKind},
};

use super::{
    super::gc::virtual_memory::{page_size, Protection, VirtualMemory},
    signals::install_signal_handlers,
    thread::Thread,
};

pub(crate) static SAFEPOINT_PAGE: Lazy<VirtualMemory> = Lazy::new(|| {
    VirtualMemory::allocate_aligned(page_size(), page_size(), false, "safepoint page")
        .expect("could not allocate GC synchronization page")
});
pub(crate) static SAFEPOINT_ENABLE_CNT: AtomicU8 = AtomicU8::new(0);

pub(crate) static SAFEPOINT_LOCK: Mutex<()> = Mutex::const_new(parking_lot::RawMutex::INIT, ());
pub(crate) static SAFEPOINT_COND: Condvar = Condvar::new();
pub(crate) static GC_RUNNING: AtomicU32 = AtomicU32::new(0);

pub fn addr_in_safepoint(addr: usize) -> bool {
    SAFEPOINT_PAGE.contains(addr)
}

pub(crate) fn enable() {
    if SAFEPOINT_ENABLE_CNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed) != 0 {
        return;
    }

    #[cfg(not(feature = "conditional-safepoint"))]
    {
        SAFEPOINT_PAGE.protect(Protection::NoAccess);
    }
    #[cfg(feature = "conditional-safepoint")]
    unsafe {
        let pageaddr = SAFEPOINT_PAGE.address();
        pageaddr.write(1);
    }
}
pub(crate) fn disable() {
    if SAFEPOINT_ENABLE_CNT.fetch_sub(1, std::sync::atomic::Ordering::Relaxed) - 1 != 0 {
        return;
    }

    #[cfg(not(feature = "conditional-safepoint"))]
    {
        SAFEPOINT_PAGE.protect(Protection::ReadWrite);
    }

    #[cfg(feature = "conditional-safepoint")]
    unsafe {
        let pageaddr = SAFEPOINT_PAGE.address();
        pageaddr.write(0);
    }
}

pub(crate) fn init() {
    let addr = SAFEPOINT_PAGE.address();
    log::info!("safepoint page: {:p}", addr);
    install_signal_handlers();
}

pub(crate) fn enter() -> bool {
    let guard = SAFEPOINT_LOCK.lock();

    match GC_RUNNING.compare_exchange(0, 1, Ordering::Relaxed, Ordering::SeqCst) {
        Ok(_) => {
            enable();
            drop(guard);
            true
        }

        Err(_) => unsafe {
            // In case multiple threads enter the GC at the same time, only allow
            // one of them to actually run the collection. We can't just let the
            // master thread do the GC since it might be running unmanaged code
            // and can take arbitrarily long time before hitting a safe point.
            drop(guard);
            wait_gc();
            return false;
        },
    }
}

pub(crate) fn end() {
    let guard = SAFEPOINT_LOCK.lock();

    disable();
    GC_RUNNING.store(0, Ordering::Release);
    drop(guard);
    SAFEPOINT_COND.notify_all();
}

/// Wait for the GC to finish.
///
/// # Safety
///
/// Must be invoked only from GC code, exposed to public only for potential VM implementations to use different STW mechanisms.
pub unsafe fn wait_gc() {
    while GC_RUNNING.load(Ordering::Relaxed) != 0 || GC_RUNNING.load(Ordering::Acquire) != 0 {
        let mut guard = SAFEPOINT_LOCK.lock();
        if GC_RUNNING.load(Ordering::Relaxed) != 0 {
            SAFEPOINT_COND.wait(&mut guard);
        }

        drop(guard);
    }
}

pub const SAFEPOINT_UNSYNCHRONIZED: u8 = 0;
pub const SAFEPOINT_SYNCHRONIZING: u8 = 1;
pub const SAFEPOINT_SYNCHRONIZED: u8 = 2;

static SAFEPOINT_STATE: AtomicU8 = AtomicU8::new(0);

pub(crate) struct SafepointSynchronize {}

impl SafepointSynchronize {
    pub(crate) unsafe fn begin() -> MutexGuard<'static, Vec<*mut Thread>> {
        let threads = threads().get();

        assert!(enter());
        SAFEPOINT_STATE.store(SAFEPOINT_SYNCHRONIZING, Ordering::Release);
        for thread in threads.iter().copied() {
            let th = &*thread;
            while th.kind == ThreadKind::Mutator
                && (th.atomic_gc_state().load(Ordering::Relaxed) == 0
                    || th.atomic_gc_state().load(Ordering::Acquire) == 0)
            {
                std::hint::spin_loop();
            }
        }

        SAFEPOINT_STATE.store(SAFEPOINT_SYNCHRONIZED, Ordering::Release);

        threads
    }

    pub(crate) unsafe fn end(guard: MutexGuard<'static, Vec<*mut Thread>>) {
        drop(guard);
        end();
        SAFEPOINT_STATE.store(SAFEPOINT_UNSYNCHRONIZED, Ordering::Release);
    }   
    #[allow(dead_code)]
    pub fn is_at_safepoint() -> bool {
        SAFEPOINT_STATE.load(Ordering::Acquire) == SAFEPOINT_SYNCHRONIZED
    }
}
