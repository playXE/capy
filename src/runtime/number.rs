use crate::vm::callframe::CallFrame;

use super::{object::ScmResult, value::Value, fun::scm_make_subr, module::{scm_define, scm_scheme_module}, symbol::Intern};

extern "C" fn number_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(cfr.argument(0).is_number())
}

extern "C" fn complex_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(cfr.argument(0).is_complex())
}

extern "C" fn real_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(cfr.argument(0).is_real())
}

extern "C" fn rational_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(cfr.argument(0).is_rational())
}

extern "C" fn f64_is_integer(x: f64) -> bool {
    if x.is_nan() {
        return false;
    }

    if x.is_infinite() {
        return false;
    }

    if x.floor() == x {
        return true;
    }

    false
}

pub fn scm_is_integer(x: Value) -> bool {
    if x.is_int32() || x.is_bignum() {
        return true;
    }

    if x.is_double() {
        return f64_is_integer(x.get_double());
    }

    false
}

extern "C" fn integer_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(scm_is_integer(cfr.argument(0)))
}

extern "C" fn exact_integer_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(cfr.argument(0).is_exact_integer())
}

extern "C" fn exact_nonnegative_integer_p(cfr: &mut CallFrame) -> ScmResult {
    let n = cfr.argument(0);
    if n.is_int32() {
        ScmResult::ok(n.get_int32() >= 0)
    } else if n.is_bignum() {
        ScmResult::ok(!n.bignum().is_negative())
    } else {
        ScmResult::ok(false)
    }
}

extern "C" fn exact_positive_integer_p(cfr: &mut CallFrame) -> ScmResult {
    let n = cfr.argument(0);
    if n.is_int32() {
        ScmResult::ok(n.get_int32() > 0)
    } else if n.is_bignum() {
        ScmResult::ok(!n.bignum().is_negative())
    } else {
        ScmResult::ok(false)
    }
}

extern "C" fn fixnum_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(cfr.argument(0).is_int32())
}

extern "C" fn inexact_real_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(cfr.argument(0).is_double())
}

extern "C" fn flonum_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(cfr.argument(0).is_double())
}

pub fn scm_is_exact(_n: Value) -> bool {
    todo!()
}


pub(crate) fn init_number() {
    

    let module = scm_scheme_module().module();

    let proc = scm_make_subr("number?", number_p, 1, 1);
    scm_define(module, "number?".intern(), proc).unwrap();

    let proc = scm_make_subr("complex?", complex_p, 1, 1);
    scm_define(module, "complex?".intern(), proc).unwrap();

    let proc = scm_make_subr("real?", real_p, 1, 1);
    scm_define(module, "real?".intern(), proc).unwrap();

    let proc = scm_make_subr("rational?", rational_p, 1, 1);
    scm_define(module, "rational?".intern(), proc).unwrap();

    let proc = scm_make_subr("integer?", integer_p, 1, 1);
    scm_define(module, "integer?".intern(), proc).unwrap();

    let proc = scm_make_subr("exact-integer?", exact_integer_p, 1, 1);
    scm_define(module, "exact-integer?".intern(), proc).unwrap();

    let proc = scm_make_subr("exact-nonnegative-integer?", exact_nonnegative_integer_p, 1, 1);
    scm_define(module, "exact-nonnegative-integer?".intern(), proc).unwrap();

    let proc = scm_make_subr("exact-positive-integer?", exact_positive_integer_p, 1, 1);
    scm_define(module, "exact-positive-integer?".intern(), proc).unwrap();

    let proc = scm_make_subr("fixnum?", fixnum_p, 1, 1);
    scm_define(module, "fixnum?".intern(), proc).unwrap();

    let proc = scm_make_subr("inexact-real?", inexact_real_p, 1, 1);
    scm_define(module, "inexact-real?".intern(), proc).unwrap();

    let proc = scm_make_subr("flonum?", flonum_p, 1, 1);
    scm_define(module, "flonum?".intern(), proc).unwrap();

    


}