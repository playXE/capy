use once_cell::sync::{Lazy, OnceCell};

use crate::{define_flag, define_option_handler, gc::CapyVM, runtime::utils::formatted_size, sync::monitor::Monitor};

use self::{utils::{get_total_memory, MemorySize}, thread::Thread};

pub mod bitfield;
pub mod cell;
pub mod flags;
pub mod pure_nan;
pub mod thread;
pub mod utils;
pub mod value;
pub mod factory;
pub mod vm;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum GCPlan {
    #[default]
    Immix,
    StickyImmix,
    GenImmix,
    GenCopy,
    MarkSweep,
    SemiSpace,
    NoGC,
}

static GC_PLAN: OnceCell<GCPlan> = OnceCell::new();

fn parse_gc_plan(argument: &str) {
    let lower = argument.to_lowercase();
    let plan = match lower.as_str() {
        "immix" => GCPlan::Immix,
        "stickyimmix" => GCPlan::StickyImmix,
        "genimmix" => GCPlan::GenImmix,
        "gencopy" => GCPlan::GenCopy,
        "marksweep" => GCPlan::MarkSweep,
        "semispace" => GCPlan::SemiSpace,
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
            GCPlan::GenImmix => "GenImmix",
            GCPlan::GenCopy => "GenCopy",
            GCPlan::MarkSweep => "MarkSweep",
            GCPlan::SemiSpace => "SemiSpace",
            GCPlan::NoGC => "NoGC",
        }
    }
}

fn build_mmtk() -> mmtk::MMTK<CapyVM> {
    let mut builder = mmtk::MMTKBuilder::new();
    builder.set_option("plan", GC_PLAN.get_or_init(|| GCPlan::Immix).to_str());
    log::info!("GC plan: {}", GC_PLAN.get_or_init(|| GCPlan::Immix).to_str());
    log::info!("GC Trigger: DynamicHeapSize:{},{}", formatted_size(min_heap_size().0), formatted_size(max_heap_size().0));
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

    builder.build()
}

pub struct Runtime {
    pub(crate) mmtk: mmtk::MMTK<CapyVM>,
    pub(crate) gc_waiters: Monitor<()>
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
        crate::gc::safepoint::init();

       
        Self { mmtk, gc_waiters: Monitor::new(()) }
    }

    pub fn get() -> &'static Self {
        &*RUNTIME
    }
}

static RUNTIME: Lazy<Runtime> = Lazy::new(|| Runtime::new());


pub fn enter_scheme<F, R>(f: F) -> R
where F: FnOnce(&mut Thread) -> R {

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