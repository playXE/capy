#![feature(arbitrary_self_types, offset_of, min_specialization)]
pub mod object;
pub mod pure_nan;
pub mod value;
pub mod op;
pub mod symbol;
pub mod module;
pub mod compaux;
pub mod list;
pub mod compile;
pub mod vm;

pub fn init() {
    symbol::init_symbols();
    module::init_modules();
}