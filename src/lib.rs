#![feature(arbitrary_self_types, offset_of, min_specialization, thread_local, core_intrinsics)]
#[macro_use]
pub mod vector;
pub mod cmp;
pub mod compaux;
pub mod compile;
pub mod interp;
pub mod list;
pub mod macros;
pub mod module;
pub mod object;
pub mod op;
pub mod fun;
pub mod pure_nan;
pub mod load;
pub mod string;
pub mod support;
pub mod symbol;
pub mod value;
pub mod vm;
pub mod repl;

pub fn init() {
    symbol::init_symbols();
    module::init_modules();
    compile::init_compiler();
    vm::init_runtime();
    load::init_load();
}

pub use rsgc::thread::Thread;
