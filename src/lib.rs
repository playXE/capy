#![feature(arbitrary_self_types, offset_of, min_specialization, thread_local)]
#[macro_use]
pub mod vector;
pub mod cmp;
pub mod comp;
pub mod compaux;
pub mod compile;
pub mod interp;
pub mod list;
pub mod macros;
pub mod module;
pub mod object;
pub mod op;
pub mod pure_nan;
pub mod string;
pub mod support;
pub mod symbol;
pub mod value;
pub mod vm;

pub fn init() {
    symbol::init_symbols();
    module::init_modules();
    compile::init_compiler();
    vm::init_runtime();
}

pub use rsgc::thread::Thread;
