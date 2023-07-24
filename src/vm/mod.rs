use crate::gc::CapyVM;

use self::{sync::{monitor::Monitor, mutex::MutexGuard}, thread::Thread};



pub mod thread;
pub mod signals;
pub mod safepoint;
pub mod sync;

pub struct VirtualMachine {
    pub mmtk: mmtk::MMTK<CapyVM>,
    pub gc_waiters_lock: Monitor<()>,
    pub safepoint_lock_data: Option<MutexGuard<'static, Vec<*mut Thread>>>,
}

impl VirtualMachine {

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