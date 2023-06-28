

#[macro_use]
pub mod list;
#[macro_use]
pub mod vector;
pub mod cmp;
pub mod fun;
pub mod load;
pub mod macros;
pub mod module;
pub mod number;
pub mod object;
pub mod values;
pub mod pure_nan;
pub mod complex;
pub mod string;
pub mod symbol;
pub mod bigint;
pub mod value;
pub mod arith;
pub mod structure;

pub(crate) fn init() {
    number::init_number();
    fun::init();
}