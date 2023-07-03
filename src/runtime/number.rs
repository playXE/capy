use rsgc::{system::arraylist::ArrayList, thread::Thread};

use crate::{
    compile::{make_iform, Asm, IForm},
    op::Opcode,
    vm::callframe::CallFrame,
};

use super::{
    fun::scm_make_subr_inliner,
    module::{scm_define, scm_scheme_module},
    object::{ScmResult},
    string::make_string,
    symbol::Intern,
    value::Value, arith::{scm_is_zero, scm_is_exact},
};

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
        ScmResult::ok(!n.bignum().is_negative() && !n.bignum().is_zero())
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

extern "C" fn exact_p(cfr: &mut CallFrame) -> ScmResult {
    if let Some(x) = scm_is_exact(cfr.argument(0)) {
        ScmResult::ok(x)
    } else {
        ScmResult::err(make_string(
            Thread::current(),
            &format!("not a number: {:?}", cfr.argument(0)),
        ))
    }
}

pub(crate) fn init_number() {
    let module = scm_scheme_module().module();

    let proc = scm_make_subr_inliner("number?", number_p, 1, 1, |iforms, _| {
        if iforms.len() == 1 {
            Some(make_iform(IForm::Asm(Asm {
                op: Opcode::IsNumber,
                args: ArrayList::from_slice(Thread::current(), iforms),
                exits: false,
                pushes: true,
                operands: None,
                ic: false,
            })))
        } else {
            None
        }
    });
    scm_define(module, "number?".intern(), proc).unwrap();

    let proc = scm_make_subr_inliner("complex?", complex_p, 1, 1, |iforms, _| {
        if iforms.len() == 1 {
            Some(make_iform(IForm::Asm(Asm {
                op: Opcode::IsComplex,
                args: ArrayList::from_slice(Thread::current(), iforms),
                exits: false,
                pushes: true,
                operands: None,
                ic: false,
            })))
        } else {
            None
        }
    });
    scm_define(module, "complex?".intern(), proc).unwrap();

    let proc = scm_make_subr_inliner("real?", real_p, 1, 1, |iforms, _| {
        if iforms.len() == 1 {
            Some(make_iform(IForm::Asm(Asm {
                op: Opcode::IsReal,
                args: ArrayList::from_slice(Thread::current(), iforms),
                exits: false,
                pushes: true,
                operands: None,
                ic: false,
            })))
        } else {
            None
        }
    });
    scm_define(module, "real?".intern(), proc).unwrap();

    let proc = scm_make_subr_inliner("rational?", rational_p, 1, 1, |iforms, _| {
        if iforms.len() == 1 {
            Some(make_iform(IForm::Asm(Asm {
                op: Opcode::IsRational,
                args: ArrayList::from_slice(Thread::current(), iforms),
                exits: false,
                pushes: true,
                operands: None,
                ic: false,
            })))
        } else {
            None
        }
    });
    scm_define(module, "rational?".intern(), proc).unwrap();

    let proc = scm_make_subr_inliner("integer?", integer_p, 1, 1, |iforms, _| {
        if iforms.len() == 1 {
            Some(make_iform(IForm::Asm(Asm {
                op: Opcode::IsInteger,
                args: ArrayList::from_slice(Thread::current(), iforms),
                exits: false,
                pushes: true,
                operands: None,
                ic: false,
            })))
        } else {
            None
        }
    });

    scm_define(module, "integer?".intern(), proc).unwrap();

    let proc = scm_make_subr_inliner("exact-integer?", exact_integer_p, 1, 1, |iforms, _| {
        if iforms.len() == 1 {
            Some(make_iform(IForm::Asm(Asm {
                op: Opcode::IsExactInteger,
                args: ArrayList::from_slice(Thread::current(), iforms),
                exits: false,
                pushes: true,
                operands: None,
                ic: false,
            })))
        } else {
            None
        }
    });
    scm_define(module, "exact-integer?".intern(), proc).unwrap();

    let proc = scm_make_subr_inliner(
        "exact-nonnegative-integer?",
        exact_nonnegative_integer_p,
        1,
        1,
        |iforms, _| {
            if iforms.len() == 1 {
                Some(make_iform(IForm::Asm(Asm {
                    op: Opcode::IsExactNonnegativeInteger,
                    args: ArrayList::from_slice(Thread::current(), iforms),
                    exits: false,
                    pushes: true,
                    operands: None,
                    ic: false,
                })))
            } else {
                None
            }
        },
    );
    scm_define(module, "exact-nonnegative-integer?".intern(), proc).unwrap();

    let proc = scm_make_subr_inliner(
        "exact-positive-integer?",
        exact_positive_integer_p,
        1,
        1,
        |iforms, _| {
            if iforms.len() == 1 {
                Some(make_iform(IForm::Asm(Asm {
                    op: Opcode::IsExactPositiveInteger,
                    args: ArrayList::from_slice(Thread::current(), iforms),
                    exits: false,
                    pushes: true,
                    operands: None,
                    ic: false,
                })))
            } else {
                None
            }
        },
    );
    scm_define(module, "exact-positive-integer?".intern(), proc).unwrap();

    let proc = scm_make_subr_inliner("fixnum?", fixnum_p, 1, 1, |iforms, _| {
        if iforms.len() == 1 {
            Some(make_iform(IForm::Asm(Asm {
                op: Opcode::IsFixnum,
                args: ArrayList::from_slice(Thread::current(), iforms),
                exits: false,
                pushes: true,
                operands: None,
                ic: false,
            })))
        } else {
            None
        }
    });
    scm_define(module, "fixnum?".intern(), proc).unwrap();

    let proc = scm_make_subr_inliner("inexact-real?", inexact_real_p, 1, 1, |iforms, _| {
        if iforms.len() == 1 {
            Some(make_iform(IForm::Asm(Asm {
                op: Opcode::IsInexactReal,
                args: ArrayList::from_slice(Thread::current(), iforms),
                exits: false,
                pushes: true,
                operands: None,
                ic: false,
            })))
        } else {
            None
        }
    });
    scm_define(module, "inexact-real?".intern(), proc).unwrap();

    let proc = scm_make_subr_inliner("flonum?", flonum_p, 1, 1, |iforms, _| {
        if iforms.len() == 1 {
            Some(make_iform(IForm::Asm(Asm {
                op: Opcode::IsFlonum,
                args: ArrayList::from_slice(Thread::current(), iforms),
                exits: false,
                pushes: true,
                operands: None,
                ic: false,
            })))
        } else {
            None
        }
    });
    scm_define(module, "flonum?".intern(), proc).unwrap();

    let proc = scm_make_subr_inliner("exact?", exact_p, 1, 1, |iforms, _| {
        if iforms.len() == 1 {
            Some(make_iform(IForm::Asm(Asm {
                op: Opcode::IsExact,
                args: ArrayList::from_slice(Thread::current(), iforms),

                exits: false,
                pushes: true,
                operands: None,
                ic: false,
            })))
        } else {
            None
        }
    });
    scm_define(module, "exact?".intern(), proc).unwrap();
}


pub fn scm_nonnegative_exact_integer(val: Value) -> bool {
    if val.is_int32() {
        val.get_int32() >= 0
    } else if val.is_bignum() {
        !val.bignum().is_negative()
    } else {
        false
    }
}

pub fn scm_real_valued(val: Value) -> bool {
    if val.is_real() {
        return true;
    }

    if val.is_complex() {
        let cn = val.complex();

        return scm_is_zero(cn.i) == Some(true);
    }

    false
}

pub fn scm_positive(val: Value) -> Option<bool> {
    if !scm_real_valued(val) {
        return None;
    }

    if val.is_int32() {
        return Some(val.get_int32() > 0);
    }

    if val.is_bignum() {
        return Some(!val.bignum().is_negative() && !val.bignum().is_zero());
    }

    if val.is_double() {
        return Some(val.get_double() > 0.0);
    }

    if val.is_rational() {
        return scm_positive(val.rational().num);
    }

    if val.is_complex() {
        let cn = val.complex();

        return scm_positive(cn.r);
    }

    None
}

pub fn scm_negative(val: Value) -> Option<bool> {
    if !scm_real_valued(val) {
        return None;
    }

    if val.is_int32() {
        return Some(val.get_int32() < 0);
    }

    if val.is_bignum() {
        return Some(val.bignum().is_negative() && !val.bignum().is_zero());
    }

    if val.is_double() {
        return Some(val.get_double() < 0.0);
    }

    if val.is_rational() {
        return scm_negative(val.rational().num);
    }

    if val.is_complex() {
        let cn = val.complex();

        return scm_negative(cn.r);
    }

    None
}