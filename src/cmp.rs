use crate::{compaux::scm_unwrap_identifier, value::Value};

pub fn scm_eq(x: Value, y: Value) -> bool {
    x == y
}

pub fn scm_eqv(x: Value, y: Value) -> bool {
    if x.get_type() != y.get_type() {
        return false;
    }

    if x.is_double() && y.is_double() {
        return x.get_double() == y.get_double();
    }
    scm_eq(x, y)
}

pub fn scm_equal(x: Value, y: Value) -> bool {
    if x.is_double() && y.is_double() {
        return x.get_double() == y.get_double();
    }

    if x == y {
        return true;
    }

    if x.is_pair() && y.is_pair() {
        return scm_equal(x.car(), y.car()) && scm_equal(x.cdr(), y.cdr());
    }

    if x.is_vector() && y.is_vector() {
        if x.vector_len() != y.vector_len() {
            return false;
        }

        for i in 0..x.vector_len() {
            if !scm_equal(x.vector_ref(i), y.vector_ref(i)) {
                return false;
            }
        }

        return true;
    }

    if x.is_string() && y.is_string() {
        return x.string().as_str() == y.string().as_str();
    }

    if x.is_wrapped_identifier() || y.is_wrapped_identifier() {
        let x = if x.is_wrapped_identifier() {
            scm_unwrap_identifier(x.identifier()).into()
        } else {
            x
        };

        let y = if y.is_wrapped_identifier() {
            scm_unwrap_identifier(y.identifier()).into()
        } else {
            y
        };

        return x == y;
    }

    x == y
}
