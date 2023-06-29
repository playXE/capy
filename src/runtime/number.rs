use rsgc::{system::arraylist::ArrayList, thread::Thread};

use crate::{
    compile::{make_iform, Asm, IForm},
    op::Opcode,
    vm::callframe::CallFrame,
};

use super::{
    complex::scm_complex_is_exact,
    fun::scm_make_subr_inliner,
    module::{scm_define, scm_scheme_module},
    object::{ScmResult, Type},
    string::make_string,
    symbol::Intern,
    value::Value,
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

pub fn scm_is_exact(n: Value) -> Option<bool> {
    if n.is_double() {
        return Some(false);
    }
    if n.is_int32() {
        Some(true)
    } else {
        let typ = n.get_type();

        if typ == Type::BigNum || typ == Type::Rational {
            Some(true)
        } else if typ == Type::Complex {
            Some(scm_complex_is_exact(n))
        } else {
            None
        }
    }
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
