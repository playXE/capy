use std::{collections::HashMap, mem::transmute};

use mmtk::util::ObjectReference;

use crate::{
    bytecode::{image::ImageRegistry, opcodes::OP_HALT},
    gc::CapyVM,
    runtime::{environment::Environment, value::Value},
};

use self::{
    sync::{
        monitor::Monitor,
        mutex::{Mutex, MutexGuard, RawMutex},
    },
    thread::Thread,
};

pub mod factory;
pub mod intrinsics;
pub mod safepoint;
pub mod signals;
pub mod sync;
pub mod thread;
pub mod options;

pub static BOOT_CONTINUATION_CODE: &'static [u8] = &[OP_HALT];

pub struct VirtualMachine {
    pub mmtk: mmtk::MMTK<CapyVM>,
    pub gc_waiters_lock: Monitor<()>,
    pub safepoint_lock_data: Option<MutexGuard<'static, Vec<*mut Thread>>>,
    pub(crate) symtable: HashMap<&'static str, Value>,
    pub(crate) symtab_lock: RawMutex,
    pub(crate) images: ImageRegistry,
    pub(crate) finalization_registry: Mutex<Vec<(ObjectReference, Box<dyn FnOnce()>)>>,
    pub(crate) gc_counter: u64,
    pub(crate) toplevel_environment: Mutex<Environment>,
    pub(crate) boot_continuation: Value,
    pub disassemble: bool,
}

impl VirtualMachine {
    pub fn get_cell(&self, key: Value) -> Value {
        self.toplevel_environment
            .lock(true)
            .get_cell(Thread::current(), key)
    }
}

pub fn scm_init(mmtk: mmtk::MMTK<CapyVM>) -> &'static mut VirtualMachine {
    safepoint::init();
    let this = Box::leak(Box::new(VirtualMachine {
        mmtk,
        gc_waiters_lock: Monitor::new(()),
        safepoint_lock_data: None,
        symtab_lock: RawMutex::INIT,
        symtable: HashMap::with_capacity(128),
        images: ImageRegistry::new(),
        finalization_registry: Mutex::new(Vec::with_capacity(128)),
        gc_counter: 0,
        toplevel_environment: Mutex::new(Environment::new()),
        boot_continuation: Value::encode_undefined_value(),
        disassemble: false,
    }));

    unsafe {
        VIRTUAL_MACHINE = this as *mut VirtualMachine;

        mmtk::memory_manager::initialize_collection(
            &scm_virtual_machine().mmtk,
            transmute(Thread::current()),
        );
        Thread::current().register_mutator();

        scm_virtual_machine().boot_continuation =
            Thread::current().make_program::<true>(BOOT_CONTINUATION_CODE.as_ptr(), 0);
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
