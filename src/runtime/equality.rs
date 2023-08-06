use super::{value::Value, object::scm_symbol_str};

pub fn eqv(a: Value, b: Value) -> bool {
    if a.is_double() && b.is_double() {
        return a.get_double() == b.get_double();
    } else if a.is_symbol() && b.is_symbol() {
        return scm_symbol_str(a) == scm_symbol_str(b);
    } else {
        a == b
    }
}

pub fn eq(a: Value, b: Value) -> bool {
    a == b
}

