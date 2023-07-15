#![feature(
    arbitrary_self_types,
    offset_of,
    min_specialization,
    thread_local,
    core_intrinsics,
    try_trait_v2,
    absolute_path,
    adt_const_params,
    inherent_associated_types,
    backtrace_frames
)]
#![allow(incomplete_features)]  

macro_rules! scm_symbol {
    ($name: ident, $val: literal) => {
        pub static $name: once_cell::sync::Lazy<$crate::runtime::value::Value> = once_cell::sync::Lazy::new(|| {
            $crate::runtime::symbol::make_symbol($val, true)
        });
    }
}

#[macro_use]
pub mod runtime;
pub mod bytecode;
pub mod compaux;
pub mod compile;
pub mod fasl;
pub mod bytecompiler;
pub mod op;
//pub mod repl;
pub mod support;
pub mod vm;



pub fn init() {
    runtime::symbol::init_symbols();
    runtime::module::init_modules();
    compile::init_compiler();
    runtime::load::init_load();
    runtime::init();
}

pub use rsgc::thread::Thread;
