#![allow(unused_imports)]
use std::mem::size_of;

use mmtk::{
    util::{
        constants::LOG_BYTES_IN_GBYTE,
        conversions::{self, chunk_align_down, chunk_align_up},
        heap::vm_layout::VMLayout,
        Address,
    },
    vm::RootsWorkFactory,
};
use once_cell::sync::{Lazy, OnceCell};
use rand::Rng;

use crate::{
    define_flag, define_option_handler,
    gc::{
        edges::{initialize_compressed_oops_base_and_shift, ScmEdge},
        ptr_compr::HeapCompressionScheme,
        CapyVM,
    },
    runtime::{
        cell::{CellReference, CellTag, SchemeHeader},
        utils::{formatted_size, log2i_graceful, round_down_usize, round_up_usize},
        value::Word,
    },
    sync::monitor::Monitor,
};

use self::{
    thread::Thread,
    utils::{get_total_memory, MemorySize},
    value::{Tagged, TaggedValue},
};

pub mod bitfield;
pub mod cell;
pub mod code_block;
pub mod factory;
pub mod flags;
pub mod pure_nan;
pub mod segmented_vec;
pub mod smi;
pub mod tagged;
pub mod thread;
pub mod utils;
pub mod value;
pub mod vm;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum GCPlan {
    #[default]
    Immix,
    StickyImmix,
    MarkSweep,
    NoGC,
}

static GC_PLAN: OnceCell<GCPlan> = OnceCell::new();

fn parse_gc_plan(argument: &str) {
    let lower = argument.to_lowercase();
    let plan = match lower.as_str() {
        "immix" => GCPlan::Immix,
        "stickyimmix" => GCPlan::StickyImmix,
        "marksweep" => GCPlan::MarkSweep,
        "nogc" => GCPlan::NoGC,
        _ => GCPlan::Immix,
    };
    GC_PLAN.get_or_init(|| plan);
}

pub const DEFAULT_MAX_HEAP_SIZE: usize = 96 * 1024 * 1024 + 1; // 96MB
pub const DEFAULT_MIN_HEAP_SIZE: usize = 16 * 1024 * 1024 + 1; // 16MB

define_option_handler!(parse_gc_plan, gc_plan, "GC plan to use");
// MemorySize type allows us to parse strings like `256MB` or `1GB` into bytes.
define_flag!(
    MemorySize,
    min_heap_size,
    MemorySize(DEFAULT_MIN_HEAP_SIZE),
    "Minimum heap size"
);
define_flag!(
    MemorySize,
    max_heap_size,
    MemorySize(DEFAULT_MAX_HEAP_SIZE),
    "Maximum heap size"
);
define_flag!(
    MemorySize,
    initial_heap_size,
    MemorySize(0),
    "Initial heap size"
);
define_flag!(f64, min_ram_percentage, 50.0, "Minimum RAM percentage");
define_flag!(f64, max_ram_percentage, 25.0, "Maximum RAM percentage");
define_flag!(
    f64,
    initial_ram_percentage,
    1.5625,
    "Initial RAM percentage"
);
define_flag!(usize, gc_threads, 4, "Number of GC worker threads");

fn set_heap_size() {
    let phys_mem = get_total_memory();

    if max_heap_size().0 == DEFAULT_MAX_HEAP_SIZE {
        let mut reasonable_max = ((phys_mem as f64 * max_ram_percentage()) / 100.0) as usize;
        let reasonable_min = ((phys_mem as f64 * min_ram_percentage()) / 100.0) as usize;

        if reasonable_min < max_heap_size().0 {
            reasonable_max = reasonable_min;
        } else {
            reasonable_max = reasonable_max.max(max_heap_size().0);
        }

        //log::info!("Maximum heap size: {}", formatted_size(reasonable_max));
        set_max_heap_size(MemorySize(reasonable_max));
    }

    if initial_heap_size().0 == 0 || min_heap_size().0 == 0 {
        let mut reasonable_minimum = 5 * 1024 * 1024; // 5 MB

        reasonable_minimum = reasonable_minimum.min(max_heap_size().0);

        if initial_heap_size().0 == 0 {
            let mut reasonable_initial =
                ((phys_mem as f64 * *initial_ram_percentage()) / 100.0) as usize;
            reasonable_initial = reasonable_initial
                .max(reasonable_minimum)
                .max(min_heap_size().0);
            reasonable_initial = reasonable_initial.min(max_heap_size().0);

            // log::info!("Initial heap size {}", formatted_size(reasonable_initial));
            set_initial_heap_size(MemorySize(reasonable_initial));
        }

        if min_heap_size().0 == 0 {
            //log::info!("Minimum heap size: {}", formatted_size(reasonable_minimum));
            set_min_heap_size(MemorySize(reasonable_minimum));
        }
    }
}

impl GCPlan {
    pub fn to_str(&self) -> &'static str {
        match self {
            GCPlan::Immix => "Immix",
            GCPlan::StickyImmix => "StickyImmix",
            GCPlan::MarkSweep => "MarkSweep",
            GCPlan::NoGC => "NoGC",
        }
    }
}

pub static mut SCM_UNDEFINED: TaggedValue = TaggedValue::new(0);
pub static mut SCM_UNSPECIFIED: TaggedValue = TaggedValue::new(0);
pub static mut SCM_EOF_OBJECT: TaggedValue = TaggedValue::new(0);
pub static mut SCM_TRUE: TaggedValue = TaggedValue::new(0);
pub static mut SCM_FALSE: TaggedValue = TaggedValue::new(0);
pub static mut SCM_NULL: TaggedValue = TaggedValue::new(0);

#[cfg(feature = "compressed-oops")]
pub const PTR_COMPR_BASE_ALIGNMENT: usize = 1 << 32;
#[cfg(feature = "compressed-oops")]
pub const PTR_COMPR_RESERVATION_SIZE: usize = 1 << 32;
#[allow(dead_code)]
fn get_random_mmap_addr() -> usize {
    let mut random = 0;
    let mut rng = rand::thread_rng();

    for _ in 0..4 {
        random |= rng.gen::<usize>();
    }

    #[cfg(target_pointer_width = "64")]
    {
        random <<= 32;
        random |= rand::thread_rng().gen::<usize>();
        random &= 46;
    }
    #[cfg(target_pointer_width = "32")]
    {
        random &= 46;
    }

    unsafe {
        let addr = libc::mmap(
            random as *mut libc::c_void,
            4 * 4096,
            libc::PROT_NONE,
            libc::MAP_PRIVATE | libc::MAP_ANON,
            -1,
            0,
        );
        println!("random: {:x}, addr: {:p}", random, addr);
        libc::munmap(addr, 4 * 4096);

        addr as _
    }
}

fn build_mmtk() -> mmtk::MMTK<CapyVM> {
    let mut builder = mmtk::MMTKBuilder::new();
    builder.set_option("plan", GC_PLAN.get_or_init(|| GCPlan::Immix).to_str());
    log::info!(
        "GC plan: {}",
        GC_PLAN.get_or_init(|| GCPlan::Immix).to_str()
    );
    log::info!(
        "GC Trigger: DynamicHeapSize:{},{}",
        formatted_size(min_heap_size().0),
        formatted_size(max_heap_size().0)
    );
    builder.set_option(
        "gc_trigger",
        &format!(
            "DynamicHeapSize:{},{}",
            min_heap_size().0,
            max_heap_size().0,
        ),
    );
    let threads = if *gc_threads() == 0 {
        std::thread::available_parallelism()
            .map(|x| x.get())
            .unwrap_or(1usize)
            / 2
    } else {
        *gc_threads()
    };
    builder.set_option("threads", threads.to_string().as_str());

    #[cfg(feature = "compressed-oops")]
    {
        let max_heap_size = builder.options.gc_trigger.max_heap_size();
        assert!(
            max_heap_size <= (4usize << LOG_BYTES_IN_GBYTE),
            "Heap size cannot be larger than 4GB when using compressed oops"
        );
        let start = 0x5_0000_0000;
        let end = start + PTR_COMPR_RESERVATION_SIZE; // size + 4GB

        let start = round_down_usize(start, PTR_COMPR_BASE_ALIGNMENT);
        let end = round_up_usize(end, PTR_COMPR_BASE_ALIGNMENT, 0);

        let constants = unsafe {
            VMLayout {
                log_address_space: 32,
                heap_start: Address::from_usize(start),
                heap_end: Address::from_usize(end),
                log_space_extent: 32,
                force_use_contiguous_spaces: false,
            }
        };
        builder.set_vm_layout(constants);
    }

    builder.build()
}

pub struct Runtime {
    pub(crate) mmtk: mmtk::MMTK<CapyVM>,
    pub(crate) gc_waiters: Monitor<()>,
}

impl Runtime {
    pub fn mmtk(&self) -> &mmtk::MMTK<CapyVM> {
        &self.mmtk
    }

    pub fn mmtk_mut(&mut self) -> &mut mmtk::MMTK<CapyVM> {
        &mut self.mmtk
    }

    fn new() -> Self {
        set_heap_size();
        let mmtk = build_mmtk();

        #[cfg(feature = "compressed-oops")]
        {
            initialize_compressed_oops_base_and_shift();
        }
        crate::gc::safepoint::init();

        Self {
            mmtk,
            gc_waiters: Monitor::new(()),
        }
    }

    unsafe fn init_main(&self) {
        let thread = Thread::current();
        debug_assert!(thread.is_mutator());

        let undef = thread.mmtk().alloc_immortal(
            size_of::<SchemeHeader>() + size_of::<Word>(),
            CellTag::UNDEFINED,
        );
        let unspec = thread.mmtk().alloc_immortal(
            size_of::<SchemeHeader>() + size_of::<Word>(),
            CellTag::UNSPECIFIED,
        );
        let eof = thread
            .mmtk()
            .alloc_immortal(size_of::<SchemeHeader>() + size_of::<Word>(), CellTag::EOF);

        let t = thread
            .mmtk()
            .alloc_immortal(size_of::<SchemeHeader>() + size_of::<Word>(), CellTag::TRUE);

        let f = thread.mmtk().alloc_immortal(
            size_of::<SchemeHeader>() + size_of::<Word>(),
            CellTag::FALSE,
        );

        let n = thread
            .mmtk()
            .alloc_immortal(size_of::<SchemeHeader>() + size_of::<Word>(), CellTag::NULL);

        SCM_UNDEFINED = Tagged::<CellReference>::from(undef).to_value();
        SCM_UNSPECIFIED = Tagged::<CellReference>::from(unspec).to_value();
        SCM_EOF_OBJECT = Tagged::<CellReference>::from(eof).to_value();

        SCM_TRUE = Tagged::<CellReference>::from(t).to_value();
        SCM_FALSE = Tagged::<CellReference>::from(f).to_value();
        SCM_NULL = Tagged::<CellReference>::from(n).to_value();
    }

    pub fn get() -> &'static Self {
        &*RUNTIME
    }

    pub(crate) unsafe fn scan_roots(&self, mut factory: impl RootsWorkFactory<ScmEdge>) {
        unsafe {
            let mut edges = vec![];
            let mut edge_from_ref = |val: &mut TaggedValue| {
                if val.is_cell() {
                    edges.push(ScmEdge::from(val));
                }
            };

            // TODO: Potentially does not require scanning since
            // it is in immortal space but we do it anyway just for safety.
            edge_from_ref(&mut SCM_UNDEFINED);
            edge_from_ref(&mut SCM_UNSPECIFIED);
            edge_from_ref(&mut SCM_EOF_OBJECT);
            edge_from_ref(&mut SCM_TRUE);
            edge_from_ref(&mut SCM_FALSE);
            edge_from_ref(&mut SCM_NULL);

            factory.create_process_edge_roots_work(edges);
        }
    }
}

static RUNTIME: Lazy<Runtime> = Lazy::new(|| Runtime::new());

pub fn enter_scheme<F, R>(f: F) -> R
where
    F: FnOnce(&mut Thread) -> R,
{
    let mut thread = Thread::current();
    if thread.is_mutator() {
        return f(&mut thread);
    }
    unsafe {
        thread.register_mutator();
    }

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f(&mut thread)));

    unsafe {
        thread.deregister_mutator();
    }

    match result {
        Ok(result) => result,
        Err(err) => std::panic::resume_unwind(err),
    }
}
