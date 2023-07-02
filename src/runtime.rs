#[macro_use]
pub mod list;
#[macro_use]
pub mod vector;
pub mod arith;
pub mod bigint;
pub mod cmp;
pub mod complex;
pub mod fun;
pub mod load;
pub mod macros;
pub mod module;
pub mod number;
pub mod object;
pub mod pure_nan;
pub mod string;
pub mod structure;
pub mod symbol;
pub mod tuple;
pub mod value;
pub mod values;
pub mod error;
pub mod port;
pub mod portfun;
pub mod print;

pub(crate) fn init() {
    number::init_number();
    fun::init();
    tuple::init_tuple();
    cmp::init_cmp();
    list::init_list();
    error::init_error();
    structure::initialize_struct();
    string::init_string();
    macros::init_macros();
    crate::vm::stacktrace::init_stacktrace();
}
