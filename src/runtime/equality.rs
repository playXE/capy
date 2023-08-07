use std::cmp::Ordering;

use super::{object::scm_symbol_str, value::Value};

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

pub fn scm_compare(a: Value, b: Value) -> Result<Option<Ordering>, ()> {
   loop {
        if a.type_of() != b.type_of() {
            return Err(());
        }

        if a.is_inline_number() && b.is_inline_number() {
            if a.is_int32() && b.is_int32() {
                return Ok(Some(a.get_int32().cmp(&b.get_int32())));
            } else {
                let a = a.get_number();
                let b = b.get_number();
                return Ok(a.partial_cmp(&b));
            }
        } else {
            return Ok(None)
        }

    }
}
