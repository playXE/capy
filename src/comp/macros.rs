use rsgc::thread::Thread;

use crate::{
    list::{scm_cons, scm_has},
    value::Value,
};

pub fn symbols_of(exp: Value) -> Value {
    fn walk(exp: Value, found: Value) -> Value {
        if exp.is_pair() {
            walk(exp.cdr(), walk(exp.car(), found))
        } else if exp.is_symbol() && !scm_has(exp, found) {
            scm_cons(Thread::current(), exp, found)
        } else {
            found
        }
    }

    walk(exp, Value::encode_null_value())
}
