use std::{collections::HashMap, mem::transmute};

use mmtk::{util::{ObjectReference, Address}, vm::{RootsWorkFactory, edge_shape::SimpleEdge}};

use crate::{
    bytecode::{image::ImageRegistry, opcodes::OP_HALT},
    gc::{CapyVM, objstorage::{ObjStorage, ParState}},
    runtime::{environment::Environment, value::Value, object::{ScmCellRef, CleanerType}, hashtable::HashTableType},
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
pub mod options;
pub mod safepoint;
pub mod signals;
pub mod sync;
pub mod thread;

pub static BOOT_CONTINUATION_CODE: &'static [u8] = &[OP_HALT];

pub struct VirtualMachine {
    pub mmtk: mmtk::MMTK<CapyVM>,
    pub gc_waiters_lock: Monitor<()>,
    pub safepoint_lock_data: Option<MutexGuard<'static, Vec<*mut Thread>>>,
    pub(crate) symtable: HashMap<&'static str, Value>,
    pub(crate) symtab_lock: RawMutex,
    pub(crate) images: ImageRegistry,
    pub(crate) finalization_registry: Mutex<Vec<(ObjectReference, CleanerType)>>,
    pub(crate) gc_counter: u64,
    pub(crate) toplevel_environment: Mutex<Environment>,
    pub(crate) boot_continuation: Value,
    pub(crate) module_obarray: Value,
    pub disassemble: bool,
    pub(crate) globals: ObjStorage,
    
}

impl VirtualMachine {
    pub fn get_cell(&self, key: Value) -> Value {
        self.toplevel_environment
            .lock(true)
            .get_cell(Thread::current(), key)
    }

    pub(crate) fn scan_roots(&mut self, factory: &mut impl RootsWorkFactory<SimpleEdge>) {
        let mut edges = vec![];
        for (_, value) in self.symtable.iter() {
            let value: &ScmCellRef = unsafe { std::mem::transmute(value) };
            let edge = SimpleEdge::from_address(Address::from_ptr(value));
            edges.push(edge);
        }
        unsafe {
            self.images.visit_roots(factory);

            let env = self.toplevel_environment.get_mut();

            for bucket in env.buckets.iter_mut() {
                if bucket.is_object() {
                    let edge: SimpleEdge = SimpleEdge::from_address(transmute(bucket));
                    edges.push(edge);
                }
            }

            let state = ParState::<false>::new(self.globals.clone(), 1);

            state.iterate(|slot| {
                if (*slot).is_object() {
                    let edge = SimpleEdge::from_address(transmute(slot));
                    if edges.len() > 128 {
                        factory.create_process_edge_roots_work(std::mem::take(&mut edges));
                    }
                    edges.push(edge);
                }
            });

            let edge = SimpleEdge::from_address(Address::from_mut_ptr(&mut self.module_obarray));
            edges.push(edge);
        }

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
        globals: ObjStorage::new("global-roots"),
        module_obarray: Value::encode_undefined_value(),
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

        let modules = Thread::current().make_hashtable(128, HashTableType::Eq);
        scm_virtual_machine().module_obarray = modules;
        intrinsics::init();
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

pub fn scm_global_roots() -> &'static ObjStorage {
    &scm_virtual_machine().globals
}

pub fn scm_init_thread() {
    Thread::current().register_mutator();
}
