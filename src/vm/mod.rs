use std::{collections::HashMap, mem::transmute};

use mmtk::{
    util::{Address, ObjectReference},
    vm::{edge_shape::SimpleEdge, RootsWorkFactory},
};

use crate::{
    bytecode::{image::ImageRegistry, opcodes::OP_HALT},
    gc::{
        objstorage::{ObjStorage, ParState},
        CapyVM,
    },
    runtime::{
        self,
        hashtable::HashTableType,
        object::{CleanerType, ScmCellRef},
        value::Value, control, equality, subr_core, symbol::scm_intern, struct_::StructGlobals,
    },
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
    pub(crate) initialized: bool,
    pub mmtk: mmtk::MMTK<CapyVM>,
    pub gc_waiters_lock: Monitor<()>,
    pub safepoint_lock_data: Option<MutexGuard<'static, Vec<*mut Thread>>>,
    pub(crate) symtable: HashMap<&'static str, Value>,
    pub(crate) symtab_lock: RawMutex,
    pub(crate) images: ImageRegistry,
    pub(crate) finalization_registry: Mutex<Vec<(ObjectReference, CleanerType)>>,
    pub(crate) weakmapping_registry: Mutex<Vec<ObjectReference>>,
    pub(crate) weakhashtable_registry: Mutex<Vec<ObjectReference>>,
    pub(crate) gc_counter: u64,

    pub(crate) boot_continuation: Value,
    pub(crate) module_obarray: Value,
    pub disassemble: bool,
    pub interaction_environment: Value,
    pub(crate) globals: ObjStorage,
    pub struct_globals: StructGlobals,
    pub symbols: [Value; InherentSymbols::Last as usize]
}

impl VirtualMachine {
    pub(crate) fn scan_roots(&mut self, factory: &mut impl RootsWorkFactory<SimpleEdge>) {
        let mut edges = vec![];
        for (_, value) in self.symtable.iter() {
            let value: &ScmCellRef = unsafe { std::mem::transmute(value) };
            let edge = SimpleEdge::from_address(Address::from_ptr(value));
            edges.push(edge);
        }
        unsafe {
            self.images.visit_roots(factory.clone());

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
            if self.interaction_environment.is_object() {
                let edge = SimpleEdge::from_address(Address::from_mut_ptr(
                    &mut self.interaction_environment,
                ));
                edges.push(edge);
            }
            let edge = SimpleEdge::from_address(Address::from_mut_ptr(&mut self.module_obarray));
            edges.push(edge);
        }
        self.struct_globals.scan_roots(factory);
        factory.create_process_edge_roots_work(edges);
    }
}

pub fn scm_init(mmtk: mmtk::MMTK<CapyVM>) -> &'static mut VirtualMachine {
    safepoint::init();
    let this = Box::leak(Box::new(VirtualMachine {
        mmtk,
        gc_waiters_lock: Monitor::new(()),
        weakmapping_registry: Mutex::new(Vec::with_capacity(128)),
        safepoint_lock_data: None,
        symtab_lock: RawMutex::INIT,
        symtable: HashMap::with_capacity(128),
        images: ImageRegistry::new(),
        weakhashtable_registry: Mutex::new(Vec::with_capacity(128)),
        finalization_registry: Mutex::new(Vec::with_capacity(128)),
        gc_counter: 0,
        boot_continuation: Value::encode_undefined_value(),
        disassemble: false,
        globals: ObjStorage::new("global-roots"),
        module_obarray: Value::encode_undefined_value(),
        interaction_environment: Value::encode_null_value(),
        symbols: [Value::encode_undefined_value(); InherentSymbols::Last as usize],
        struct_globals: StructGlobals::default(),
        initialized: false,
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
        this.init_inherent();
        runtime::environment::init_env();
        intrinsics::init();
        control::init();
        equality::init();
        subr_core::init();
        
        this.initialized = true;
        this
    }
}

impl VirtualMachine {
    fn init_inherent(&mut self) {
        self.symbols[InherentSymbols::Ellipsis as usize] = scm_intern("...");
        self.symbols[InherentSymbols::Underbar as usize] = scm_intern("_");
    }

    pub fn inherent_symbol(&self, sym: InherentSymbols) -> Value {
        self.symbols[sym as usize]
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

pub fn scm_global_roots() -> &'static ObjStorage {
    &scm_virtual_machine().globals
}

pub fn scm_init_thread() {
    Thread::current().register_mutator();
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum InherentSymbols {
    Ellipsis,
    Underbar,
    Last,
}