use std::mem::transmute;

use crate::gc::CapyVM;

use self::{
    sync::{monitor::Monitor, mutex::MutexGuard},
    thread::Thread,
};

pub mod safepoint;
pub mod signals;
pub mod factory;
pub mod sync;
pub mod thread;

pub struct VirtualMachine {
    pub mmtk: mmtk::MMTK<CapyVM>,
    pub gc_waiters_lock: Monitor<()>,
    pub safepoint_lock_data: Option<MutexGuard<'static, Vec<*mut Thread>>>,
}

impl VirtualMachine {}

pub fn scm_init(mmtk: mmtk::MMTK<CapyVM>) -> &'static mut VirtualMachine {
    safepoint::init();
    let this = Box::leak(Box::new(VirtualMachine {
        mmtk,
        gc_waiters_lock: Monitor::new(()),
        safepoint_lock_data: None,
    }));

    unsafe {
        VIRTUAL_MACHINE = this as *mut VirtualMachine;

        Thread::current().register_mutator();
       
        mmtk::memory_manager::initialize_collection(
            &scm_virtual_machine().mmtk,
            transmute(Thread::current()),
        );

        this
    }
}

static mut VIRTUAL_MACHINE: *mut VirtualMachine = std::ptr::null_mut();

pub fn scm_virtual_machine() -> &'static mut VirtualMachine {
    unsafe {
        if VIRTUAL_MACHINE.is_null() {
            panic!("Virtual machine is not initialized")
        }
        &mut *VIRTUAL_MACHINE
    }
}

pub fn scm_init_thread() {
    Thread::current().register_mutator();
}
