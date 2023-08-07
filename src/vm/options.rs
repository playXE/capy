use std::{path::PathBuf, str::FromStr};

use crate::utils::env::read_uint_from_str;

pub struct VMOptions {
    pub gc_plan: GCPlan,
    pub gc_min_heap_size: usize,
    pub disassemble: bool,
    pub gc_max_heap_size: usize,
    pub filename: Option<PathBuf>,
}

impl VMOptions {
    pub fn new() -> Self {
        VMOptions {
            gc_plan: GCPlan::Immix,
            gc_min_heap_size: 0,
            disassemble: false,
            gc_max_heap_size: 0,
            filename: None,
        }
    }

    pub fn set_gc_plan(&mut self, plan: GCPlan) {
        self.gc_plan = plan;
    }

    pub fn set_gc_min_heap_size(&mut self, size: usize) {
        self.gc_min_heap_size = size;
    }

    pub fn set_gc_max_heap_size(&mut self, size: usize) {
        self.gc_max_heap_size = size;
    }

    pub fn parse() -> Result<Self, String> {
        parse()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GCPlan {
    Immix,
    StickyImmix,
    MarkSweep,
    GenCopy,
    GenImmix,
    SemiSpace,
    MarkCompact,
}

impl AsRef<str> for GCPlan {
    fn as_ref(&self) -> &str {
        match self {
            GCPlan::Immix => "Immix",
            GCPlan::StickyImmix => "StickyImmix",
            GCPlan::MarkSweep => "MarkSweep",
            GCPlan::GenCopy => "GenCopy",
            GCPlan::GenImmix => "GenImmix",
            GCPlan::SemiSpace => "SemiSpace",
            GCPlan::MarkCompact => "MarkCompact",
        }
    }
}

impl FromStr for GCPlan {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.to_lowercase();
        let s: &str = &s;

        match s {
            "immix" => Ok(GCPlan::Immix),
            "stickyimmix" => Ok(GCPlan::StickyImmix),
            "marksweep" => Ok(GCPlan::MarkSweep),
            "gencopy" => Ok(GCPlan::GenCopy),
            "genimmix" => Ok(GCPlan::GenImmix),
            "semispace" => Ok(GCPlan::SemiSpace),
            "markcompact" => Ok(GCPlan::MarkCompact),
            _ => Err("Invalid GC plan"),
        }
    }
}

pub fn parse() -> Result<VMOptions, String> {
    let mut args = pico_args::Arguments::from_env();
    let mut options = VMOptions::new();

    if args.contains(["-h", "--help"]) {
        println!("Usage: capy-vm [options] <input file>");
        println!("Options:");
        println!("  -h, --help: Print this help message");
        println!("  --gc-plan <plan>: Set the GC plan to use (default: Immix)");
        println!("  --gc-min-heap-size <size>: Set the minimum heap size (default: 64m)");
        println!("  --gc-max-heap-size <size>: Set the maximum heap size (default: 256m)");
        println!("  --disassemble: Dissaemble bytecode images on load");
        std::process::exit(0);
    }

    let gc_plan = match args.opt_value_from_str::<_, GCPlan>("--gc-plan") {
        Ok(Some(plan)) => plan,
        Ok(None) => GCPlan::Immix,
        Err(e) => return Err(e.to_string()),
    };

    let gc_min_heap_size = match args
        .opt_value_from_str::<_, String>("--gc-min-heap-size")
    {
        Ok(Some(size)) => read_uint_from_str(&size).unwrap_or(64 * 1024 * 1024),
        Ok(None) => 64 * 1024 * 1024,
        Err(e) => return Err(e.to_string()),
    };

    let gc_max_heap_size = match args
        .opt_value_from_str::<_, String>("--gc-max-heap-size")
    {
        Ok(Some(size)) => read_uint_from_str(&size).unwrap_or(256 * 1024 * 1024),
        Ok(None) => 256 * 1024 * 1024,
        Err(e) => return Err(e.to_string()),
    };

    let disassemble = args.contains("--disassemble");

    let filename = args.free_from_str::<PathBuf>();

    if let Err(e) = filename {
        return Err(e.to_string());
    }

    options.set_gc_plan(gc_plan);
    options.set_gc_min_heap_size(gc_min_heap_size);
    options.set_gc_max_heap_size(gc_max_heap_size);
    options.disassemble = disassemble;
    options.filename = filename.ok();

    Ok(options)
}   
