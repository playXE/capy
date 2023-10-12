use crate::{
    interpreter::scm_call_n,
    vm::{scm_virtual_machine, thread::Thread},
};

use self::{
    environment::environment_get, object::scm_string_str, symbol::scm_intern, value::Value,
};

pub mod arith;
pub mod bytevector;
pub mod control;
pub mod coroutine;
pub mod environment;
pub mod equality;
pub mod error;
pub mod fasl;
pub mod fileio;
pub mod gsubr;
pub mod hashtable;
pub mod env;
pub mod list;
pub mod object;
pub mod pure_nan;
pub mod scheduler;
pub mod subr_arith;
pub mod subr_core;
pub mod subr_fixnum;
pub mod subr_hash;
pub mod symbol;
pub mod value;

pub fn to_string(thread: &mut Thread, value: Value) -> String {
    // expects boot library to be loaded.
    // it should define `->string` in `print.scm`
    if let Ok(to_string) = environment_get(
        scm_virtual_machine().interaction_environment,
        scm_intern("->string"),
    ) {
        if to_string.is_program() {
            match scm_call_n::<true>(thread, to_string, &[value]) {
                Ok(x) if x.is_string() => return scm_string_str(x).to_string(),
                Err(_) | Ok(_) => (),
            }
        }
    }
    // uses our fmt::Display impl which is not as good as the one in Scheme

    value.to_string()
}
