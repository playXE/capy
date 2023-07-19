use crate::{compaux::scm_unwrap_identifier, runtime::value::Value, vm::callframe::CallFrame};

use super::{
    fun::scm_make_subr,
    module::{scm_define, scm_scheme_module},
    object::ScmResult,
    symbol::Intern,
};

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

    if x.is_tuple() && y.is_tuple() {
        if x.tuple().len() != y.tuple().len() {
            return false;
        }

        for i in 0..x.tuple().len() {
            if !scm_equal(x.tuple_ref(i), y.tuple_ref(i)) {
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

extern "C" fn eq(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(cfr.argument(0) == cfr.argument(1))
}

extern "C" fn eqv(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(scm_eqv(cfr.argument(0), cfr.argument(1)))
}

extern "C" fn equal(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(scm_equal(cfr.argument(0), cfr.argument(1)))
}

extern "C" fn boolean_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(cfr.argument(0).is_boolean())
}

extern "C" fn not(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(Value::encode_bool_value(cfr.argument(0).is_false()))
}

pub(crate) fn init_cmp() {
    let module = scm_scheme_module().module();

    let subr = scm_make_subr("eq?", eq, 2, 2);
    scm_define(module, "eq?".intern(), subr).unwrap();

    let subr = scm_make_subr("eqv?", eqv, 2, 2);
    scm_define(module, "eqv?".intern(), subr).unwrap();

    let subr = scm_make_subr("equal?", equal, 2, 2);
    scm_define(module, "equal?".intern(), subr).unwrap();

    let subr = scm_make_subr("boolean?", boolean_p, 1, 1);
    scm_define(module, "boolean?".intern(), subr).unwrap();

    let subr = scm_make_subr("not", not, 1, 1);
    scm_define(module, "not".intern(), subr).unwrap();
}
