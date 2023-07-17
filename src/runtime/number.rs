use rsgc::{system::arraylist::ArrayList, thread::Thread};

use crate::{
    compile::{make_iform, Asm, IForm},
    op::Opcode,
    vm::callframe::CallFrame,
};

use super::{
    arith::{scm_is_exact, scm_is_zero},
    error::wrong_contract,
    fun::{scm_make_subr_inliner, scm_make_subr},
    module::{scm_define, scm_scheme_module},
    object::ScmResult,
    symbol::Intern,
    value::{scm_int, scm_uint, Value},
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
        wrong_contract::<()>("exact?", "number?", 0, 1, cfr.arguments()).into()
    }
}

extern "C" fn exact_to_inexact(cfr: &mut CallFrame) -> ScmResult {
    let n = cfr.argument(0);
    if n.is_double() {
        return ScmResult::ok(n);
    }
    if !scm_is_exact(n).ok_or_else(|| {
        wrong_contract::<()>("exact->inexact", "exact?", 0, 1, cfr.arguments()).unwrap_err()
    })? {
        return wrong_contract::<()>("exact->inexact", "exact?", 0, 1, cfr.arguments()).into();
    }

    if n.is_int32() {
        ScmResult::ok(n.get_int32() as f64)
    } else if n.is_bignum() {
        ScmResult::ok(n.bignum().f64())
    } else {
        unreachable!()
    }
}

extern "C" fn inexact_to_exact(cfr: &mut CallFrame) -> ScmResult {
    let n = cfr.argument(0);
    if !n.is_number() {
        return wrong_contract::<()>("inexact->exact", "number?", 0, 1, cfr.arguments()).into();
    }
    if n.is_int32() {
        ScmResult::ok(n)
    } else if n.is_bignum() {
        ScmResult::ok(n)
    } else if n.is_double() {
        if n.get_double().is_nan() {
            ScmResult::ok(n)
        } else if n.get_double().is_infinite() {
            ScmResult::ok(n)
        } else if f64_is_integer(n.get_double()) {
            ScmResult::ok(scm_int(n.get_double() as i64))
        } else {
            ScmResult::ok(scm_uint(n.get_double() as u64))
        }
    } else {
        unreachable!()
    }
}

pub(crate) fn init_number() {
    let module = scm_scheme_module().module();

    let subr = scm_make_subr("exact->inexact", exact_to_inexact, 1, 1);
    scm_define(module, "exact->inexact".intern(), subr).unwrap();

    let subr = scm_make_subr("inexact->exact", inexact_to_exact, 1, 1);
    scm_define(module, "inexact->exact".intern(), subr).unwrap();

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

pub fn scm_to_usize(val: Value) -> Option<usize> {
    if val.is_int32() {
        let v = val.get_int32();

        if v >= 0 {
            return Some(v as usize);
        } else {
            return None;
        }
    }

    if val.is_bignum() {
        let v = val.bignum();

        if let Some(v) = v.u64() {
            return Some(v as usize);
        } else {
            return None;
        }
    }

    None
}

pub fn scm_s16(val: Value) -> Option<i16> {
    let val = val.normalized();
    if !val.is_int32() {
        return None;
    }

    let x = val.get_int32();

    if x > i16::MAX as i32 || x < i16::MIN as i32 {
        return None;
    }

    Some(x as i16)
}

pub fn scm_s32(val: Value) -> Option<i32> {
    let val = val.normalized();
    if val.is_double() {
        let x = val.get_double();

        if x > i32::MAX as f64 || x < i32::MIN as f64 {
            return None;
        }

        return Some(x.floor() as i32);
    }
    if !val.is_int32() {
        return None;
    }

    let x = val.get_int32();

    Some(x)
}

pub fn scm_s64(val: Value) -> Option<i64> {
    val.get_int64()
}

pub fn scm_u16(val: Value) -> Option<u16> {
    let val = val.normalized();

    if !val.is_int32() {
        return None;
    }

    let x = val.get_int32();

    if x < u16::MIN as i32 || x > u16::MAX as i32 {
        return None;
    }

    Some(x as _)
}

pub fn scm_u32(val: Value) -> Option<u32> {
    let val = val.normalized();

    if val.is_int32() {
        let x = val.get_int32();

        if x < 0 {
            return None;
        }

        return Some(x as _);
    }

    if val.is_bignum() {
        if let Some(x) = val.bignum().u32() {
            return Some(x);
        }
    }

    None
}

pub fn scm_u64(val: Value) -> Option<u64> {
    let val = val.normalized();

    if val.is_int32() {
        let x = val.get_int32();

        if x < 0 {
            return None;
        }

        return Some(x as _);
    }

    if val.is_bignum() {
        if let Some(x) = val.bignum().u64() {
            return Some(x);
        }
    }

    None
}

pub fn scm_make_s16(mut val: i16, native_endian: bool) -> Value {
    if !native_endian {
        val = val.swap_bytes();
    }

    Value::encode_int32(val as _)
}

pub fn scm_make_s32(mut val: i32, native_endian: bool) -> Value {
    if !native_endian {
        val = val.swap_bytes();
    }

    Value::encode_int32(val)
}

pub fn scm_make_s64(mut val: i64, native_endian: bool) -> Value {
    if !native_endian {
        val = val.swap_bytes();
    }

    scm_int(val)
}

pub fn scm_make_u16(mut val: u16, native_endian: bool) -> Value {
    if !native_endian {
        val = val.swap_bytes();
    }

    Value::encode_int32(val as _)
}

pub fn scm_make_u32(mut val: u32, native_endian: bool) -> Value {
    if !native_endian {
        val = val.swap_bytes();
    }

    scm_uint(val as _)
}

pub fn scm_make_u64(mut val: u64, native_endian: bool) -> Value {
    if !native_endian {
        val = val.swap_bytes();
    }

    scm_uint(val)
}
