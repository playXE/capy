use std::{collections::HashMap, mem::transmute};

use crate::{bytecode::image::ImageRegistry, gc::CapyVM, runtime::value::Value};

use self::{
    sync::{
        monitor::Monitor,
        mutex::{MutexGuard, RawMutex},
    },
    thread::Thread,
};

pub mod factory;
pub mod safepoint;
pub mod signals;
pub mod sync;
pub mod thread;
pub mod intrinsics;

pub struct VirtualMachine {
    pub mmtk: mmtk::MMTK<CapyVM>,
    pub gc_waiters_lock: Monitor<()>,
    pub safepoint_lock_data: Option<MutexGuard<'static, Vec<*mut Thread>>>,
    pub(crate) symtable: HashMap<&'static str, Value>,
    pub(crate) symtab_lock: RawMutex,
    pub(crate) images: ImageRegistry,
}

impl VirtualMachine {}

pub fn scm_init(mmtk: mmtk::MMTK<CapyVM>) -> &'static mut VirtualMachine {
    safepoint::init();
    let this = Box::leak(Box::new(VirtualMachine {
        mmtk,
        gc_waiters_lock: Monitor::new(()),
        safepoint_lock_data: None,
        symtab_lock: RawMutex::INIT,
        symtable: HashMap::with_capacity(128),
        images: ImageRegistry::new(),
    }));

    unsafe {
        VIRTUAL_MACHINE = this as *mut VirtualMachine;

        

        mmtk::memory_manager::initialize_collection(
            &scm_virtual_machine().mmtk,
            transmute(Thread::current()),
        );
        Thread::current().register_mutator();
        this
    }
}

impl VirtualMachine {}

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
