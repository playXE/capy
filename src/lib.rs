#![feature(
    arbitrary_self_types,
    offset_of,
    min_specialization,
    thread_local,
    core_intrinsics,
    try_trait_v2
)]
#[macro_use]
pub mod runtime;
pub mod compaux;
pub mod compile;
pub mod interp;
pub mod op;
pub mod repl;
pub mod vm;

pub fn init() {
    runtime::symbol::init_symbols();
    runtime::module::init_modules();
    compile::init_compiler();
    vm::init_runtime();
    runtime::load::init_load();
    runtime::init();
}

pub use rsgc::thread::Thread;
