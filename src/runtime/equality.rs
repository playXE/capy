use std::cmp::Ordering;

use crate::vm::thread::Thread;

use super::{
    gsubr::{scm_define_subr, Subr},
    object::*,
    value::Value,
};

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

pub fn scm_compare(a: Value, b: Value) -> Option<Ordering> {
    loop {
        if a.type_of() != b.type_of() {
            return None;
        }

        if a.is_inline_number() && b.is_inline_number() {
            if a.is_int32() && b.is_int32() {
                return Some(a.get_int32().cmp(&b.get_int32()));
            } else {
                let a = a.get_number();
                let b = b.get_number();
                return a.partial_cmp(&b);
            }
        } else if a.is_string() && b.is_string() {
            return Some(scm_symbol_str(a).cmp(&scm_symbol_str(b)));
        } else {
            return if a == b { Some(Ordering::Equal) } else { None };
        }
    }
}

pub fn equal(mut a: Value, mut b: Value) -> bool {
    'top: loop {
        if a == b {
            return true;
        }

        if a.is_pair() {
            if b.is_pair() {
                if equal(scm_car(a), scm_car(b)) {
                    a = scm_cdr(a);
                    b = scm_cdr(b);
                    continue 'top;
                }
            }

            return false;
        }

        if a.is_vector() {
            if b.is_vector() {
                if scm_vector_length(a) != scm_vector_length(b) {
                    return false;
                }

                for i in 0..scm_vector_length(a) {
                    if !equal(scm_vector_ref(a, i), scm_vector_ref(b, i)) {
                        return false;
                    }
                }

                return true;
            }
        }

        if a.is_bytevector() {
            if b.is_bytevector() {
                if scm_bytevector_length(a) != scm_bytevector_length(b) {
                    return false;
                }

                for i in 0..scm_bytevector_length(a) {
                    if scm_bytevector_ref(a, i) != scm_bytevector_ref(b, i) {
                        return false;
                    }
                }

                return true;
            }

            return false;
        }

        if a.is_string() {
            if b.is_string() {
                return scm_string_str(a) == scm_string_str(b);
            }

            return false;
        }
        break eqv(a, b);
    }
}

extern "C-unwind" fn eq_subr(_thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    Value::encode_bool_value(eq(*a, *b))
}

extern "C-unwind" fn eqv_subr(_thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    Value::encode_bool_value(eqv(*a, *b))
}

extern "C-unwind" fn equal_subr(_thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    Value::encode_bool_value(equal(*a, *b))
}

pub(crate) fn init() {
    scm_define_subr("eq?", 2, 0, 0, Subr::F2(eq_subr));
    scm_define_subr("eqv?", 2, 0, 0, Subr::F2(eqv_subr));
    scm_define_subr("equal?", 2, 0, 0, Subr::F2(equal_subr));
}
