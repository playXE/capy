use std::cmp::Ordering;

use rsgc::thread::Thread;

use crate::{
    raise_exn,
    runtime::object::Type,
    vm::{scm_vm, VM},
};

use super::{
    bigint::BigInt,
    error::wrong_contract,
    number::{scm_negative, scm_real_valued},
    object::{make_complex, make_rational},
    value::{scm_int, scm_uint, Value},
};

pub fn scm_is_even(val: Value) -> Option<bool> {
    if val.is_int32() {
        return Some(val.get_int32() % 2 == 0);
    }

    if val.is_bignum() {
        return Some(val.bignum().uwords()[0] & 1 == 0);
    }

    if val.is_double() {
        return Some(val.get_double() * 0.5 == (val.get_double() * 0.5).floor());
    }

    if val.is_rational() {
        return scm_is_even(val.rational().num);
    }

    if val.is_complex() {
        return scm_is_even(val.complex().r);
    }

    None
}

pub fn scm_is_number_equal(lhs: Value, rhs: Value) -> Option<bool> {
    let lhs = lhs.normalized();
    let rhs = rhs.normalized();

    if lhs.is_int32() {
        if rhs.is_int32() {
            return Some(lhs.get_int32() == rhs.get_int32());
        }

        if rhs.is_double() {
            return Some(lhs.get_int32() as f64 == rhs.get_double());
        }

        if rhs.is_bignum() {
            return Some(false);
        }

        if rhs.is_rational() {
            return Some(false);
        }

        if rhs.is_complex() {
            if scm_is_zero(rhs.complex().i)? {
                return scm_is_number_equal(lhs, rhs.complex().r);
            }

            return Some(false);
        }
    }

    if lhs.is_double() {
        if rhs.is_int32() {
            return Some(lhs.get_double() == rhs.get_int32() as f64);
        }

        if rhs.is_double() {
            return Some(lhs.get_double() == rhs.get_double());
        }

        if rhs.is_bignum() {
            if rhs.bignum().f64() == lhs.get_double() {
                return Some(true);
            }
            return Some(false);
        }

        if rhs.is_rational() {
            return Some(false);
        }

        if rhs.is_complex() {
            if scm_is_zero(rhs.complex().i)? {
                return scm_is_number_equal(lhs, rhs.complex().r);
            }

            return Some(false);
        }
    }

    if lhs.is_bignum() {
        if rhs.is_int32() {
            return Some(false);
        }

        if rhs.is_double() {
            if rhs.get_double() == lhs.bignum().f64() {
                return scm_is_number_equal(cnvt_to_exact(lhs)?, rhs);
            }
        }

        if rhs.is_bignum() {
            return Some(lhs.bignum().eq(&rhs.bignum()));
        }

        if rhs.is_rational() {
            return Some(false);
        }

        if rhs.is_complex() {
            if scm_is_zero(rhs.complex().i)? {
                return scm_is_number_equal(lhs, rhs.complex().r);
            }

            return Some(false);
        }
    }

    if lhs.is_complex() {
        if scm_is_zero(lhs.complex().i)? {
            return scm_is_number_equal(lhs.complex().r, rhs);
        }

        if rhs.is_int32() || rhs.is_double() || rhs.is_bignum() || rhs.is_rational() {
            return Some(false);
        }

        if rhs.is_complex() {
            return Some(
                scm_is_number_equal(lhs.complex().r, rhs.complex().r)?
                    && scm_is_number_equal(lhs.complex().i, rhs.complex().i)?,
            );
        }
    }

    None
}

pub fn scm_n_compare(lhs: Value, rhs: Value) -> Option<Ordering> {
    let lhs = lhs.normalized();
    let rhs = rhs.normalized();
    if lhs.is_int32() {
        if rhs.is_int32() {
            return Some(lhs.get_int32().cmp(&rhs.get_int32()));
        }

        if rhs.is_double() {
            let d = lhs.get_int32() as f64 - rhs.get_double();
            if d == 0.0 {
                return Some(Ordering::Equal);
            }

            if d < 0.0 {
                return Some(Ordering::Less);
            } else {
                return Some(Ordering::Greater);
            }
        }

        if rhs.is_bignum() {
            if rhs.bignum().is_negative() {
                return Some(Ordering::Greater);
            } else {
                return Some(Ordering::Less);
            }
        }

        if rhs.is_complex() {
            if scm_is_zero(rhs.complex().i)? {
                return scm_n_compare(lhs, rhs.complex().r);
            }
        }
    }

    if lhs.is_double() {
        if rhs.is_int32() {
            let d = lhs.get_double() - rhs.get_int32() as f64;
            if d == 0.0 {
                return Some(Ordering::Equal);
            }

            if d < 0.0 {
                return Some(Ordering::Less);
            } else {
                return Some(Ordering::Greater);
            }
        }

        if rhs.is_double() {
            let d = lhs.get_double() - rhs.get_double();
            if d == 0.0 {
                return Some(Ordering::Equal);
            }

            if d < 0.0 {
                return Some(Ordering::Less);
            } else {
                return Some(Ordering::Greater);
            }
        }

        if rhs.is_bignum() {
            let d = lhs.get_double() - rhs.bignum().f64();
            if d == 0.0 {
                return Some(Ordering::Equal);
            }

            if d < 0.0 {
                return Some(Ordering::Less);
            } else {
                return Some(Ordering::Greater);
            }
        }

        if rhs.is_complex() {
            if scm_is_zero(rhs.complex().i)? {
                return scm_n_compare(lhs, rhs.complex().r);
            }
        }
    }

    if lhs.is_bignum() {
        if rhs.is_int32() {
            if lhs.bignum().is_negative() {
                return Some(Ordering::Less);
            } else {
                return Some(Ordering::Greater);
            }
        }

        if rhs.is_double() {
            let d = lhs.bignum().f64() - rhs.get_double();
            if d == 0.0 {
                return Some(Ordering::Equal);
            }

            if d < 0.0 {
                return Some(Ordering::Less);
            } else {
                return Some(Ordering::Greater);
            }
        }

        if rhs.is_bignum() {
            return Some(lhs.bignum().cmp(&rhs.bignum()));
        }

        if rhs.is_complex() {
            if scm_is_zero(rhs.complex().i)? {
                return scm_n_compare(lhs, rhs.complex().r);
            }
        }
    }

    if lhs.is_complex() {
        if scm_is_zero(lhs.complex().i)? {
            return scm_n_compare(lhs.complex().r, rhs);
        }
    }
    None
}

pub fn scm_is_number(val: Value) -> bool {
    if val.is_int32() || val.is_double() {
        return true;
    }

    let xtype = val.get_type();

    matches!(xtype, Type::BigNum | Type::Rational | Type::Complex)
}

pub fn scm_is_integer(val: Value) -> bool {
    if val.is_int32() || val.is_bignum() {
        return true;
    }

    if val.is_double() {
        let flonum = val.get_double();

        if flonum.is_nan() || flonum.is_infinite() {
            return false;
        }

        return flonum.round() == flonum;
    } else {
        false
    }
}

pub fn scm_is_real(val: Value) -> bool {
    matches!(
        val.get_type(),
        Type::Int32 | Type::BigNum | Type::Double | Type::Rational
    )
}

pub fn scm_is_finite(val: Value) -> bool {
    if val.is_double() {
        let flonum = val.get_double();

        if flonum.is_nan() || flonum.is_infinite() {
            return false;
        }

        return true;
    } else {
        true
    }
}

pub fn scm_is_exact_integer(val: Value) -> bool {
    matches!(val.get_type(), Type::Int32 | Type::BigNum)
}

pub fn scm_is_exact_non_negative_integer(val: Value) -> bool {
    if val.is_int32() {
        return val.get_int32() >= 0;
    }

    if val.is_bignum() {
        return !val.bignum().is_negative();
    }

    false
}

pub fn scm_is_exact_positive_integer(val: Value) -> bool {
    if val.is_int32() {
        return val.get_int32() > 0;
    }

    if val.is_bignum() {
        return !val.bignum().is_negative() && !val.bignum().is_zero();
    }

    false
}

pub fn scm_is_exact(val: Value) -> Option<bool> {
    if val.is_int32() || val.is_bignum() || val.is_rational() {
        Some(true)
    } else if val.is_double() {
        Some(true)
    } else if val.is_complex() {
        Some(scm_is_exact(val.complex().r)? && scm_is_exact(val.complex().i)?)
    } else {
        None
    }
}

pub fn scm_is_zero(val: Value) -> Option<bool> {
    if val.is_int32() {
        Some(val.get_int32() == 0)
    } else if val.is_bignum() {
        Some(val.bignum().is_zero())
    } else if val.is_double() {
        Some(val.get_double() == 0.0)
    } else if val.is_rational() {
        scm_is_zero(val.rational().num)
    } else if val.is_complex() {
        Some(scm_is_zero(val.complex().r)? && scm_is_zero(val.complex().i)?)
    } else {
        None
    }
}

pub fn scm_is_negative(val: Value) -> Option<bool> {
    if val.is_int32() {
        Some(val.get_int32() < 0)
    } else if val.is_bignum() {
        Some(val.bignum().is_negative())
    } else if val.is_double() {
        Some(val.get_double() < 0.0)
    } else if val.is_rational() {
        scm_is_negative(val.rational().num)
    } else if val.is_complex() {
        Some(scm_is_negative(val.complex().r)?)
    } else {
        None
    }
}

pub fn scm_is_positive(val: Value) -> Option<bool> {
    if val.is_int32() {
        Some(val.get_int32() > 0)
    } else if val.is_bignum() {
        Some(!val.bignum().is_negative() && !val.bignum().is_zero())
    } else if val.is_double() {
        Some(val.get_double() > 0.0)
    } else if val.is_rational() {
        scm_is_positive(val.rational().num)
    } else if val.is_complex() {
        Some(scm_is_positive(val.complex().r)?)
    } else {
        None
    }
}

fn rational_to_double(obj: Value) -> f64 {
    let nume = if obj.rational().num.is_int32() {
        obj.rational().num.get_int32() as f64
    } else {
        obj.rational().num.bignum().f64()
    };

    let deno = if obj.rational().den.is_int32() {
        obj.rational().den.get_int32() as f64
    } else {
        obj.rational().den.bignum().f64()
    };

    nume / deno
}
pub fn real_to_double(obj: Value) -> Result<f64, Value> {
    if obj.is_double() {
        return Ok(obj.get_double());
    }

    if obj.is_int32() {
        return Ok(obj.get_int32() as f64);
    }

    if obj.is_bignum() {
        return Ok(obj.bignum().f64());
    }

    if obj.is_rational() {
        todo!()
    }

    if obj.is_complex() {
        let cn = obj.complex();

        if scm_is_zero(cn.i) == Some(true) {
            return real_to_double(cn.r);
        }
    }

    wrong_contract("real->double", "real?", 0, 1, &[obj])
}

pub fn cnvt_to_inexact(obj: Value) -> Option<Value> {
    if obj.is_int32() {
        return Some(Value::encode_f64_value(obj.get_int32() as _));
    } else if obj.is_double() {
        return Some(obj);
    } else if obj.is_bignum() {
        return Some(Value::encode_f64_value(obj.bignum().f64()));
    } else if obj.is_rational() {
        return Some(rational_to_double(obj).into());
    } else if obj.is_complex() {
        let cn = obj.complex();
        if cn.r.is_double() && cn.i.is_double() {
            return Some(cn.into());
        }

        let r = cnvt_to_inexact(cn.r)?;
        let i = cnvt_to_inexact(cn.i)?;

        return Some(make_complex(r, i));
    }

    None
}

pub fn cnvt_to_exact(obj: Value) -> Option<Value> {
    if obj.is_double() {
        if obj.get_double() == 0.0 {
            return Some(Value::encode_int32(0));
        }

        let val = obj.get_double();

        if val as i32 as f64 == val {
            return Some(Value::encode_int32(val as i32));
        }

        let bigint = BigInt::from_f64(Thread::current(), val);

        return Some(Value::encode_object_value(bigint));
    }

    if obj.is_complex() {
        let cn = obj.complex();
        if cn.r.is_double() || cn.i.is_double() {
            return Some(cn.into());
        }
    }

    if obj.is_int32() || obj.is_bignum() || obj.is_rational() {
        Some(obj)
    } else {
        None
    }
}

pub fn arith_inverse(vm: &mut VM, obj: Value) -> Option<Value> {
    if obj.is_int32() {
        if obj.get_int32() > 0 {
            if obj.get_int32() == 1 {
                return Some(obj);
            }

            return Some(make_rational(Value::encode_int32(1), obj));
        }

        if obj.get_int32() == -1 {
            return Some(obj);
        }

        return Some(make_rational(
            Value::encode_int32(-1),
            arith_negate(vm, obj)?,
        ));
    }

    if obj.is_double() {
        return Some(Value::encode_f64_value(1.0 / obj.get_double()));
    }

    if obj.is_bignum() {
        if !obj.bignum().is_negative() {
            return Some(make_rational(Value::encode_int32(1), obj));
        } else {
            return Some(make_rational(
                Value::encode_int32(-1),
                arith_negate(vm, obj)?,
            ));
        }
    }

    if obj.is_complex() {
        return arith_div(vm, Value::encode_int32(1), obj);
    }

    None
}

fn norm_complex(_vm: &mut VM, real: Value, imag: Value) -> Value {
    if imag.is_int32() && imag.get_int32() == 0 {
        real
    } else if imag.is_bignum() && imag.bignum().is_zero() {
        real
    } else if real.is_double() || imag.is_double() {
        let real = cnvt_to_inexact(real).unwrap();
        let imag = cnvt_to_inexact(imag).unwrap();

        make_complex(real, imag)
    } else {
        make_complex(real, imag)
    }
}

#[allow(dead_code)]
fn reduce_fixnum_fixnum(numerator: Value, denominator: Value) -> Value {
    let mut nume = numerator.get_int32();
    let mut deno = denominator.get_int32();

    if deno == 1 {
        return numerator;
    }

    if deno == -1 {
        return Value::encode_int32(-nume);
    }

    if deno == 0 {
        return Value::encode_int32(0);
    }

    if nume == 1 {
        if deno < 0 {
            return make_rational(Value::encode_int32(-1), Value::encode_int32(-deno));
        }

        return make_rational(numerator, denominator);
    }

    if nume == -1 {
        if deno < 0 {
            return make_rational(Value::encode_int32(1), Value::encode_int32(-deno));
        }

        return make_rational(numerator, denominator);
    }

    let mut ans_sign = -1;

    if nume < 0 {
        ans_sign = -ans_sign;
        nume = -nume;
    }

    if deno < 0 {
        ans_sign = -ans_sign;
        deno = -deno;
    }

    let mut n1 = nume as i64;
    let mut n2 = deno as i64;

    while n2 != 0 {
        let t = n2;
        n2 = n1 % n2;
        n1 = t;
    }

    let gcd = n1;

    if deno == gcd as i32 {
        return Value::encode_int32(nume.wrapping_mul(ans_sign).wrapping_div(gcd as _));
    }

    make_rational(
        Value::encode_int32(nume.wrapping_mul(ans_sign).wrapping_div(gcd as _)),
        Value::encode_int32(deno.wrapping_div(gcd as _)),
    )
}

pub fn arith_negate(vm: &mut VM, val: Value) -> Option<Value> {
    if val.is_int32() {
        let i = val.get_int32();
        if i == i32::MIN {
            return Some(scm_int(-(i as i64)));
        }
        return Some(Value::encode_int32(-val.get_int32()));
    }

    if val.is_double() {
        return Some(Value::encode_f64_value(-val.get_double()));
    }

    if val.is_bignum() {
        let bignum = val.bignum();

        return Some(Value::encode_object_value(bignum.negate(vm.mutator()).into()).normalized());
    }

    if val.is_rational() {
        let rn = val.rational();

        Some(make_rational(arith_negate(vm, rn.num)?, rn.den))
    } else if val.is_complex() {
        let cn = val.complex();
        let r = arith_negate(vm, cn.r)?;
        let i = arith_negate(vm, cn.i)?;
        Some(make_complex(r, i))
    } else {
        None
    }
}

pub fn arith_mul(vm: &mut VM, lhs: Value, rhs: Value) -> Option<Value> {
    let lhs = lhs.normalized();
    let rhs = rhs.normalized();
    if lhs.is_int32() {
        if lhs.get_int32() == 0 {
            return Some(lhs);
        }

        if rhs.is_int32() {
            let n = lhs.get_int32() as i64 * rhs.get_int32() as i64;

            if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
                return Some(Value::encode_int32(n as i32));
            }

            return Some(Value::encode_object_value(BigInt::from_i64(
                vm.mutator(),
                n,
            )));
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(
                lhs.get_int32() as f64 * rhs.get_double(),
            ));
        }

        if rhs.is_bignum() {
            let lhs = BigInt::from_i64(vm.mutator(), lhs.get_int32() as _);

            return Some(Value::encode_object_value(
                lhs.times(vm.mutator(), rhs.bignum()),
            ));
        }

        if rhs.is_rational() {
            todo!()
        }

        if rhs.is_complex() {
            let real = rhs.complex().r;
            let imag = rhs.complex().i;

            return Some(make_complex(
                arith_mul(vm, lhs, real)?,
                arith_mul(vm, lhs, imag)?,
            ));
        }
    }

    if lhs.is_double() {
        if rhs.is_int32() {
            return Some(Value::encode_f64_value(
                lhs.get_double() * rhs.get_int32() as f64,
            ));
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(lhs.get_double() * rhs.get_double()));
        }

        if rhs.is_bignum() {
            return Some(Value::encode_f64_value(
                lhs.get_double() * rhs.bignum().f64(),
            ));
        }

        if rhs.is_complex() {
            let real = rhs.complex().r;
            let imag = rhs.complex().i;

            return Some(make_complex(
                arith_mul(vm, lhs, real)?,
                arith_mul(vm, lhs, imag)?,
            ));
        }
    }

    if lhs.is_bignum() {
        if rhs.is_int32() {
            if rhs == Value::encode_int32(0) {
                return Some(Value::encode_int32(0));
            }

            let rhs = BigInt::from_i64(vm.mutator(), rhs.get_int32() as _);

            return Some(Value::encode_object_value(
                lhs.bignum().times(vm.mutator(), rhs),
            ));
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(
                lhs.bignum().f64() * rhs.get_double(),
            ));
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(
                lhs.bignum().f64() * rhs.get_double(),
            ));
        }

        if rhs.is_complex() {
            let real = rhs.complex().r;
            let imag = rhs.complex().i;

            return Some(make_complex(
                arith_mul(vm, lhs, real)?,
                arith_mul(vm, lhs, imag)?,
            ));
        }
    }

    if lhs.is_complex() {
        let real = lhs.complex().r;
        let imag = lhs.complex().i;

        if rhs.is_int32() {
            if rhs == Value::encode_int32(0) {
                return Some(Value::encode_int32(0));
            }

            return Some(make_complex(
                arith_mul(vm, real, rhs)?,
                arith_mul(vm, imag, rhs)?,
            ));
        }

        if rhs.is_double() {
            return Some(make_complex(
                arith_mul(vm, real, rhs)?,
                arith_mul(vm, imag, rhs)?,
            ));
        }

        if rhs.is_bignum() {
            return Some(make_complex(
                arith_mul(vm, real, rhs)?,
                arith_mul(vm, imag, rhs)?,
            ));
        }

        if rhs.is_complex() {
            let real2 = rhs.complex().r;
            let imag2 = rhs.complex().i;

            let rr = arith_mul(vm, real, real2)?;
            let ii = arith_mul(vm, imag, imag2)?;
            let ri = arith_mul(vm, real, imag2)?;
            let ir = arith_mul(vm, imag, real2)?;
            let real = arith_sub(vm, rr, ii)?;
            let imag = arith_add(vm, ri, ir)?;

            return Some(make_complex(real, imag));
        }
    }

    None
}

pub fn arith_sub(vm: &mut VM, lhs: Value, rhs: Value) -> Option<Value> {
    let lhs = lhs.normalized();
    let rhs = rhs.normalized();

    if lhs.is_int32() {
        if rhs.is_int32() {
            let n = lhs.get_int32() as i64 - rhs.get_int32() as i64;

            if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
                return Some(Value::encode_int32(n as i32));
            }

            return Some(Value::encode_object_value(BigInt::from_i64(
                vm.mutator(),
                n,
            )));
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(
                lhs.get_int32() as f64 - rhs.get_double(),
            ));
        }

        if rhs.is_bignum() {
            let lhs = BigInt::from_i64(vm.mutator(), lhs.get_int32() as _);

            return Some(Value::encode_object_value(
                lhs.minus(vm.mutator(), rhs.bignum()),
            ));
        }

        if rhs.is_rational() {
            todo!()
        }

        if rhs.is_complex() {
            let real = rhs.complex().r;
            let imag = rhs.complex().i;

            return Some(make_complex(
                arith_sub(vm, lhs, real)?,
                arith_sub(vm, lhs, imag)?,
            ));
        }
    }
    if lhs.is_double() {
        if rhs.is_int32() {
            return Some(Value::encode_f64_value(
                lhs.get_double() - rhs.get_int32() as f64,
            ));
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(lhs.get_double() - rhs.get_double()));
        }

        if rhs.is_bignum() {
            return Some(Value::encode_f64_value(
                lhs.get_double() - rhs.bignum().f64(),
            ));
        }

        if rhs.is_complex() {
            let real = rhs.complex().r;
            let imag = rhs.complex().i;

            return Some(make_complex(
                arith_sub(vm, lhs, real)?,
                arith_sub(vm, lhs, imag)?,
            ));
        }
    }

    if lhs.is_bignum() {
        if rhs.is_int32() {
            let rhs = BigInt::from_i64(vm.mutator(), rhs.get_int32() as _);

            return Some(Value::encode_object_value(
                lhs.bignum().minus(vm.mutator(), rhs),
            ));
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(
                lhs.bignum().f64() - rhs.get_double(),
            ));
        }

        if rhs.is_bignum() {
            return Some(Value::encode_object_value(
                lhs.bignum().minus(vm.mutator(), rhs.bignum()),
            ));
        }

        if rhs.is_complex() {
            let real = rhs.complex().r;
            let imag = rhs.complex().i;

            return Some(make_complex(
                arith_sub(vm, lhs, real)?,
                arith_sub(vm, lhs, imag)?,
            ));
        }
    }

    if lhs.is_complex() {
        let real = lhs.complex().r;
        let imag = lhs.complex().i;

        if rhs.is_int32() {
            return Some(make_complex(
                arith_sub(vm, real, rhs)?,
                arith_sub(vm, imag, rhs)?,
            ));
        }

        if rhs.is_double() {
            return Some(make_complex(
                arith_sub(vm, real, rhs)?,
                arith_sub(vm, imag, rhs)?,
            ));
        }

        if rhs.is_bignum() {
            return Some(make_complex(
                arith_sub(vm, real, rhs)?,
                arith_sub(vm, imag, rhs)?,
            ));
        }

        if rhs.is_complex() {
            let real2 = rhs.complex().r;
            let imag2 = rhs.complex().i;

            let real = arith_sub(vm, real, real2)?;
            let imag = arith_sub(vm, imag, imag2)?;

            return Some(make_complex(real, imag));
        }
    }

    None
}

pub fn arith_exp(_vm: &mut VM, val: Value) -> Result<Value, Value> {
    if val.is_int32() {
        if val.get_int32() == 0 {
            return Ok(Value::encode_int32(1));
        }

        return Ok(Value::encode_f64_value((val.get_int32() as f64).exp()));
    }

    if scm_real_valued(val) {
        return Ok(Value::encode_f64_value(real_to_double(val)?.exp()));
    }

    wrong_contract("exp", "real?", 0, 1, &[val])
}
/*
pub fn arith_expt(vm: &mut VM, lhs: Value, rhs: Value) -> Result<Value, Value> {
    if lhs.is_double() && lhs.get_double() == 0.0 {
        if lhs.is_complex() {
            if scm_positive(rhs.complex().r) == Some(true) {
                return Ok(Value::encode_f64_value(0.0));
            }
        } else {
            if scm_positive(rhs) == Some(true) {
                return Ok(Value::encode_f64_value(0.0));
            }
        }
    }

    if scm_is_exact(rhs) == Some(true) {
        if rhs.is_int32() {
            if rhs.is_int32() {
                if rhs.get_int32() == 0 {
                    return Ok(Value::encode_int32(1));
                }

                if lhs.is_double() {
                    return Ok(Value::encode_f64_value(lhs.get_double().powi(rhs.get_int32())));
                }

                return expt(vm, lhs, rhs);
            }
        }

        todo!()
    } else {
        if rhs.is_double() {
            if scm_real_valued(lhs) && scm_negative(lhs) == Some(false) {
                let n = rhs.get_double();

                return Ok(Value::encode_f64_value(lhs.get_double().powf(n)));
            }


            let m = arith_mul()
            return arith_exp(vm, )
        }
    }
}*/

pub fn arith_expt(vm: &mut VM, mut lhs: Value, rhs: Value) -> Option<Value> {
    let mut n = rhs.get_int32() as i64;
    if n == 0 {
        return Some(Value::encode_int32(1));
    }

    if n == 1 {
        return Some(lhs);
    }

    if n < 0 {
        todo!()
    }

    if !lhs.is_complex() && scm_negative(lhs) == Some(true) {
        todo!()
    }

    if lhs == Value::encode_int32(0) {
        return Some(lhs);
    }

    if lhs == Value::encode_int32(1) {
        return Some(lhs);
    }

    if lhs == Value::encode_int32(2) {
        if n + 1 <= 31 {
            return Some(Value::encode_int32(1 << n));
        }

        todo!()
    }

    let mut ans = Value::encode_int32(1);

    loop {
        if n & 1 != 0 {
            if ans == Value::encode_int32(1) {
                ans = lhs;
            } else {
                ans = arith_mul(vm, ans, lhs)?;
            }

            if n == 1 {
                return Some(ans);
            }
        }

        lhs = arith_mul(vm, lhs, lhs)?;
        n >>= 1;
    }
}

pub fn arith_add(vm: &mut VM, lhs: Value, rhs: Value) -> Option<Value> {
    let lhs = lhs.normalized();
    let rhs = rhs.normalized();

    if lhs.is_int32() {
        if rhs.is_int32() {
            let n = lhs.get_int32() as i64 + rhs.get_int32() as i64;

            if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
                return Some(Value::encode_int32(n as i32));
            }

            return Some(Value::encode_object_value(BigInt::from_i64(
                vm.mutator(),
                n,
            )));
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(
                lhs.get_int32() as f64 + rhs.get_double(),
            ));
        }

        if rhs.is_bignum() {
            let lhs = BigInt::from_i64(vm.mutator(), lhs.get_int32() as _);

            return Some(Value::encode_object_value(
                lhs.plus(vm.mutator(), rhs.bignum()),
            ));
        }

        if rhs.is_rational() {
            todo!()
        }

        if rhs.is_complex() {
            let real = rhs.complex().r;
            let imag = rhs.complex().i;

            return Some(make_complex(
                arith_add(vm, lhs, real)?,
                arith_add(vm, lhs, imag)?,
            ));
        }
    }
    if lhs.is_double() {
        if rhs.is_int32() {
            return Some(Value::encode_f64_value(
                lhs.get_double() + rhs.get_int32() as f64,
            ));
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(lhs.get_double() + rhs.get_double()));
        }

        if rhs.is_bignum() {
            return Some(Value::encode_f64_value(
                lhs.get_double() + rhs.bignum().f64(),
            ));
        }

        if rhs.is_complex() {
            let real = rhs.complex().r;
            let imag = rhs.complex().i;

            return Some(make_complex(
                arith_add(vm, lhs, real)?,
                arith_add(vm, lhs, imag)?,
            ));
        }
    }

    if lhs.is_bignum() {
        if rhs.is_int32() {
            let rhs = BigInt::from_i64(vm.mutator(), rhs.get_int32() as _);

            return Some(Value::encode_object_value(
                lhs.bignum().plus(vm.mutator(), rhs),
            ));
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(
                lhs.bignum().f64() + rhs.get_double(),
            ));
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(
                lhs.bignum().f64() + rhs.get_double(),
            ));
        }

        if rhs.is_bignum() {
            return Some(Value::encode_object_value(
                lhs.bignum().plus(vm.mutator(), rhs.bignum()),
            ));
        }

        if rhs.is_complex() {
            let real = rhs.complex().r;
            let imag = rhs.complex().i;

            return Some(make_complex(
                arith_add(vm, lhs, real)?,
                arith_add(vm, lhs, imag)?,
            ));
        }
    }

    if lhs.is_complex() {
        let real = lhs.complex().r;
        let imag = lhs.complex().i;

        if rhs.is_int32() {
            return Some(make_complex(
                arith_add(vm, real, rhs)?,
                arith_add(vm, imag, rhs)?,
            ));
        }

        if rhs.is_double() {
            return Some(make_complex(
                arith_add(vm, real, rhs)?,
                arith_add(vm, imag, rhs)?,
            ));
        }

        if rhs.is_bignum() {
            return Some(make_complex(
                arith_add(vm, real, rhs)?,
                arith_add(vm, imag, rhs)?,
            ));
        }

        if rhs.is_complex() {
            let real2 = rhs.complex().r;
            let imag2 = rhs.complex().i;

            let real = arith_add(vm, real, real2)?;
            let imag = arith_add(vm, imag, imag2)?;

            return Some(make_complex(real, imag));
        }
    }

    None
}

pub fn arith_div(vm: &mut VM, lhs: Value, rhs: Value) -> Option<Value> {
    let lhs = lhs.normalized();
    let rhs = rhs.normalized();

    if lhs.is_int32() {
        if rhs.is_int32() {
            let n = lhs.get_int32() as i64 / rhs.get_int32() as i64;

            if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
                return Some(Value::encode_int32(n as i32));
            }

            return Some(Value::encode_object_value(BigInt::from_i64(
                vm.mutator(),
                n,
            )));
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(
                lhs.get_int32() as f64 / rhs.get_double(),
            ));
        }

        if rhs.is_bignum() {
            let lhs = BigInt::from_i64(vm.mutator(), lhs.get_int32() as _);

            return Some(
                Value::encode_object_value(lhs.divided(vm.mutator(), rhs.bignum()).0).normalized(),
            );
        }

        if rhs.is_rational() {
            todo!()
        }

        if rhs.is_complex() {
            let real = rhs.complex().r;
            let imag = rhs.complex().i;

            return Some(make_complex(
                arith_div(vm, lhs, real)?,
                arith_div(vm, lhs, imag)?,
            ));
        }
    }
    if lhs.is_double() {
        if rhs.is_int32() {
            return Some(Value::encode_f64_value(
                lhs.get_double() / rhs.get_int32() as f64,
            ));
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(lhs.get_double() / rhs.get_double()));
        }

        if rhs.is_bignum() {
            return Some(Value::encode_f64_value(
                lhs.get_double() / rhs.bignum().f64(),
            ));
        }

        if rhs.is_complex() {
            let real = rhs.complex().r;
            let imag = rhs.complex().i;

            return Some(make_complex(
                arith_add(vm, lhs, real)?,
                arith_add(vm, lhs, imag)?,
            ));
        }
    }

    if lhs.is_bignum() {
        if rhs.is_int32() {
            let rhs = BigInt::from_i64(vm.mutator(), rhs.get_int32() as _);

            return Some(
                Value::encode_object_value(lhs.bignum().divided(vm.mutator(), rhs).0).normalized(),
            );
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(
                lhs.bignum().f64() / rhs.get_double(),
            ));
        }

        if rhs.is_bignum() {
            return Some(
                Value::encode_object_value(lhs.bignum().divided(vm.mutator(), rhs.bignum()).0)
                    .normalized(),
            );
        }

        if rhs.is_complex() {
            let real = rhs.complex().r;
            let imag = rhs.complex().i;

            return Some(make_complex(
                arith_add(vm, lhs, real)?,
                arith_add(vm, lhs, imag)?,
            ));
        }
    }

    if lhs.is_complex() {
        let real = lhs.complex().r;
        let imag = lhs.complex().i;

        if rhs.is_int32() {
            return Some(make_complex(
                arith_add(vm, real, rhs)?,
                arith_add(vm, imag, rhs)?,
            ));
        }

        if rhs.is_double() {
            return Some(make_complex(
                arith_add(vm, real, rhs)?,
                arith_add(vm, imag, rhs)?,
            ));
        }

        if rhs.is_bignum() {
            return Some(make_complex(
                arith_add(vm, real, rhs)?,
                arith_add(vm, imag, rhs)?,
            ));
        }

        if rhs.is_complex() {
            let real2 = rhs.complex().r;
            let imag2 = rhs.complex().i;

            let real = arith_sub(vm, real, real2)?;
            let imag = arith_sub(vm, imag, imag2)?;

            return Some(make_complex(real, imag));
        }
    }

    None
}

pub fn arith_quotient(vm: &mut VM, lhs: Value, rhs: Value) -> Option<Value> {
    let lhs = lhs.normalized();
    let rhs = rhs.normalized();

    if lhs.is_int32() {
        if rhs.is_int32() {
            let n = lhs.get_int32() as i64 / rhs.get_int32() as i64;

            if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
                return Some(Value::encode_int32(n as i32));
            }

            return Some(Value::encode_object_value(BigInt::from_i64(
                vm.mutator(),
                n,
            )));
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(
                lhs.get_int32() as f64 / rhs.get_double(),
            ));
        }

        if rhs.is_bignum() {
            let lhs = BigInt::from_i64(vm.mutator(), lhs.get_int32() as _);

            return Some(
                Value::encode_object_value(lhs.divided(vm.mutator(), rhs.bignum()).0).normalized(),
            );
        }

        if rhs.is_rational() {
            todo!()
        }

        if rhs.is_complex() {
            let real = rhs.complex().r;
            let imag = rhs.complex().i;

            return Some(make_complex(
                arith_quotient(vm, lhs, real)?,
                arith_quotient(vm, lhs, imag)?,
            ));
        }
    }
    if lhs.is_double() {
        if rhs.is_int32() {
            return Some(Value::encode_f64_value(
                lhs.get_double() / rhs.get_int32() as f64,
            ));
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(lhs.get_double() % rhs.get_double()));
        }

        if rhs.is_bignum() {
            return Some(Value::encode_f64_value(
                lhs.get_double() / rhs.bignum().f64(),
            ));
        }

        if rhs.is_complex() {
            let real = rhs.complex().r;
            let imag = rhs.complex().i;

            return Some(make_complex(
                arith_quotient(vm, lhs, real)?,
                arith_quotient(vm, lhs, imag)?,
            ));
        }
    }

    if lhs.is_bignum() {
        if rhs.is_int32() {
            let rhs = BigInt::from_i64(vm.mutator(), rhs.get_int32() as _);

            return Some(
                Value::encode_object_value(lhs.bignum().divided(vm.mutator(), rhs).0).normalized(),
            );
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(
                lhs.bignum().f64() / rhs.get_double(),
            ));
        }

        if rhs.is_bignum() {
            return Some(
                Value::encode_object_value(lhs.bignum().divided(vm.mutator(), rhs.bignum()).0)
                    .normalized(),
            );
        }

        if rhs.is_complex() {
            let real = rhs.complex().r;
            let imag = rhs.complex().i;

            return Some(make_complex(
                arith_add(vm, lhs, real)?,
                arith_add(vm, lhs, imag)?,
            ));
        }
    }

    if lhs.is_complex() {
        let real = lhs.complex().r;
        let imag = lhs.complex().i;

        if rhs.is_int32() {
            return Some(make_complex(
                arith_add(vm, real, rhs)?,
                arith_add(vm, imag, rhs)?,
            ));
        }

        if rhs.is_double() {
            return Some(make_complex(
                arith_add(vm, real, rhs)?,
                arith_add(vm, imag, rhs)?,
            ));
        }

        if rhs.is_bignum() {
            return Some(make_complex(
                arith_add(vm, real, rhs)?,
                arith_add(vm, imag, rhs)?,
            ));
        }

        if rhs.is_complex() {
            let real2 = rhs.complex().r;
            let imag2 = rhs.complex().i;

            let real = arith_sub(vm, real, real2)?;
            let imag = arith_sub(vm, imag, imag2)?;

            return Some(make_complex(real, imag));
        }
    }

    None
}

pub fn arith_remainder(vm: &mut VM, lhs: Value, rhs: Value) -> Option<Value> {
    let lhs = lhs.normalized();
    let rhs = rhs.normalized();

    if lhs.is_int32() {
        if rhs.is_int32() {
            let n = lhs.get_int32() as i64 % rhs.get_int32() as i64;

            if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
                return Some(Value::encode_int32(n as i32));
            }

            return Some(Value::encode_object_value(BigInt::from_i64(
                vm.mutator(),
                n,
            )));
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(
                lhs.get_int32() as f64 % rhs.get_double(),
            ));
        }

        if rhs.is_bignum() {
            let lhs = BigInt::from_i64(vm.mutator(), lhs.get_int32() as _);

            return Some(
                Value::encode_object_value(lhs.divided(vm.mutator(), rhs.bignum()).1).normalized(),
            );
        }

        if rhs.is_rational() {
            todo!()
        }

        if rhs.is_complex() {
            let real = rhs.complex().r;
            let imag = rhs.complex().i;

            return Some(make_complex(
                arith_remainder(vm, lhs, real)?,
                arith_remainder(vm, lhs, imag)?,
            ));
        }
    }
    if lhs.is_double() {
        if rhs.is_int32() {
            return Some(Value::encode_f64_value(
                lhs.get_double() % rhs.get_int32() as f64,
            ));
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(lhs.get_double() % rhs.get_double()));
        }

        if rhs.is_bignum() {
            return Some(Value::encode_f64_value(
                lhs.get_double() % rhs.bignum().f64(),
            ));
        }

        if rhs.is_complex() {
            let real = rhs.complex().r;
            let imag = rhs.complex().i;

            return Some(make_complex(
                arith_remainder(vm, lhs, real)?,
                arith_remainder(vm, lhs, imag)?,
            ));
        }
    }

    if lhs.is_bignum() {
        if rhs.is_int32() {
            let rhs = BigInt::from_i64(vm.mutator(), rhs.get_int32() as _);

            return Some(
                Value::encode_object_value(lhs.bignum().divided(vm.mutator(), rhs).1).normalized(),
            );
        }

        if rhs.is_double() {
            return Some(Value::encode_f64_value(
                lhs.bignum().f64() % rhs.get_double(),
            ));
        }

        if rhs.is_bignum() {
            return Some(
                Value::encode_object_value(lhs.bignum().divided(vm.mutator(), rhs.bignum()).1)
                    .normalized(),
            );
        }

        if rhs.is_complex() {
            let real = rhs.complex().r;
            let imag = rhs.complex().i;

            return Some(make_complex(
                arith_remainder(vm, lhs, real)?,
                arith_remainder(vm, lhs, imag)?,
            ));
        }
    }

    None
}

pub fn arith_bitcount(lhs: Value) -> Option<Value> {
    if lhs.is_int32() {
        let n = lhs.get_int32();
        if n == 0 {
            return Some(Value::encode_int32(0));
        } else if n > 0 {
            return Some(Value::encode_int32((n as u32).count_ones() as i32));
        } else {
            return Some(Value::encode_int32((!(n as u32)).count_ones() as i32));
        }
    }

    if lhs.is_bignum() {
        let n = lhs.bignum();

        return Some(scm_int(n.bit_count() as _));
    }

    None
}

pub fn arith_bitlength(value: Value) -> Option<Value> {
    if value.is_int32() {
        let n = value.get_int32();
        if n == 0 {
            return Some(Value::encode_int32(0));
        }

        let n2 = if n < 0 { !n } else { n };

        return Some(Value::encode_int32(32 - n2.leading_zeros() as i32));
    }

    if value.is_bignum() {
        let n = value.bignum();

        if n.is_zero() {
            return Some(Value::encode_int32(0));
        }
        if !n.is_negative() {
            return Some(scm_int(n.bit_size() as _));
        }
        let x = n.not(scm_vm().mutator());
        return Some(scm_int(x.bit_size() as _));
    }

    None
}

pub fn arith_lognot(vm: &mut VM, val: Value) -> Option<Value> {
    if val.is_int32() {
        let n = !(val.get_int32() as i64);
        if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
            return Some(Value::encode_int32(n as i32));
        }

        return Some(scm_int(n));
    }

    if val.is_bignum() {
        let n = val.bignum();

        return Some(Value::encode_object_value(n.not(vm.mutator())).normalized());
    }

    None
}

pub fn arith_logand(vm: &mut VM, lhs: Value, rhs: Value) -> Option<Value> {
    if lhs.is_int32() {
        if rhs.is_int32() {
            return Some(Value::encode_int32(lhs.get_int32() & rhs.get_int32()));
        }

        if rhs.is_bignum() {
            let rhs = rhs.bignum();

            return Some(
                rhs.and32(vm.mutator(), lhs.get_int32() as u32, lhs.get_int32() < 0)
                    .into(),
            );
        }
    }

    if lhs.is_bignum() {
        if rhs.is_int32() {
            let lhs = lhs.bignum();

            return Some(
                lhs.and32(vm.mutator(), rhs.get_int32() as u32, rhs.get_int32() < 0)
                    .into(),
            );
        }

        if rhs.is_bignum() {
            return Some(
                Value::encode_object_value(lhs.bignum().and(vm.mutator(), rhs.bignum()))
                    .normalized(),
            );
        }
    }

    None
}

pub fn arith_logior(vm: &mut VM, lhs: Value, rhs: Value) -> Option<Value> {
    if lhs.is_int32() {
        if rhs.is_int32() {
            return Some(Value::encode_int32(lhs.get_int32() | rhs.get_int32()));
        }

        if rhs.is_bignum() {
            let rhs = rhs.bignum();

            return Some(
                rhs.or32(vm.mutator(), lhs.get_int32() as u32, lhs.get_int32() < 0)
                    .into(),
            );
        }
    }

    if lhs.is_bignum() {
        if rhs.is_int32() {
            let lhs = lhs.bignum();

            return Some(
                lhs.or32(vm.mutator(), rhs.get_int32() as u32, rhs.get_int32() < 0)
                    .into(),
            );
        }

        if rhs.is_bignum() {
            return Some(
                Value::encode_object_value(lhs.bignum().or(vm.mutator(), rhs.bignum()))
                    .normalized(),
            );
        }
    }

    None
}

pub fn arith_logxor(vm: &mut VM, lhs: Value, rhs: Value) -> Option<Value> {
    if lhs.is_int32() {
        if rhs.is_int32() {
            return Some(Value::encode_int32(lhs.get_int32() ^ rhs.get_int32()));
        }

        if rhs.is_bignum() {
            let rhs = rhs.bignum();

            return Some(
                rhs.xor32(vm.mutator(), lhs.get_int32() as u32, lhs.get_int32() < 0)
                    .into(),
            );
        }
    }

    if lhs.is_bignum() {
        if rhs.is_int32() {
            let lhs = lhs.bignum();

            return Some(
                lhs.xor32(vm.mutator(), rhs.get_int32() as u32, rhs.get_int32() < 0)
                    .into(),
            );
        }

        if rhs.is_bignum() {
            return Some(
                Value::encode_object_value(lhs.bignum().xor(vm.mutator(), rhs.bignum()))
                    .normalized(),
            );
        }
    }

    None
}

pub fn arith_logash(vm: &mut VM, lhs: Value, rhs: Value) -> Option<Value> {
    let shift = rhs.get_int32();

    if lhs.is_int32() {
        if shift <= 32 {
            let mut n = lhs.get_int32() as i64;
            if shift > 0 {
                n = n << shift;
            } else {
                n = n >> -shift;
            }

            if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
                return Some(Value::encode_int32(n as i32));
            }

            return Some(scm_int(n));
        } else {
            let bn = BigInt::from_i64(vm.mutator(), lhs.get_int32() as i64);
            return Some(bn.shift(vm.mutator(), shift).into());
        }
    }

    if lhs.is_bignum() {
        let bn = lhs.bignum();

        return Some(bn.shift(vm.mutator(), shift).into());
    }

    None
}

pub fn arith_first_bit_set(val: Value) -> Option<Value> {
    if val.is_int32() {
        let n = val.get_int32();

        if n == 0 {
            return Some(Value::encode_int32(-1));
        }

        let bit = n.trailing_zeros() as i32;

        return Some(Value::encode_int32(bit));
    }

    if val.is_bignum() {
        let bn = val.bignum();

        return Some(
            bn.first_bit_set()
                .map(|x| scm_uint(x as _))
                .unwrap_or(scm_int(-1)),
        );
    }

    None
}

pub fn arith_last_bit_set(val: Value) -> Option<Value> {
    if val.is_int32() {
        let n = val.get_int32();

        if n == 0 {
            return Some(Value::encode_int32(-1));
        }

        let bit = 31 - n.leading_zeros() as i32;

        return Some(Value::encode_int32(bit));
    }

    if val.is_bignum() {
        let bn = val.bignum();

        return Some(
            bn.last_bit_set()
                .map(|x| scm_uint(x as _))
                .unwrap_or(scm_int(-1)),
        );
    }

    None
}



fn parse_negate(vm: &mut VM, val: Value) -> Value {
    if val.is_int32() {
        let n = val.get_int32();

        if n == i32::MIN {
            return scm_int(-(n as i64));
        } else {
            return Value::encode_int32(-n);
        }
    } else if val.is_double() {
        return Value::encode_f64_value(-val.get_double());
    } else if val.is_bignum() {
        return Value::encode_object_value(val.bignum().negate(vm.mutator()));
    } else if val.is_rational() {
        todo!()
    } else if val.is_complex() {
        let real = val.complex().r;
        let imag = val.complex().i;

        return make_complex(parse_negate(vm, real), parse_negate(vm, imag));
    } else {
        unreachable!()
    }
}
#[allow(unused_assignments, unused_variables)]
pub fn parse_number(vm: &mut VM, mut s: &[u8], prefix: u8, mut radix: u8) -> Result<Value, Value> {
    let mut negative = false;
    let mut nosign = true;
    let mut exact = false;
    let mut inexact = false;

    match prefix {
        b'e' | b'E' => {
            exact = true;
        }

        b'i' | b'I' => {
            inexact = true;
        }

        _ => (),
    }

    match radix {
        b'b' | b'B' => {
            radix = 2;
        }

        b'o' | b'O' => {
            radix = 8;
        }

        b'd' | b'D' => {
            radix = 10;
        }
        b'x' | b'X' => {
            radix = 16;
        }

        _ => (),
    }

    while s[0] == b'#' {
        match s[1] {
            b'i' | b'I' => {
                if exact | inexact {
                    return Ok(false.into());
                }

                inexact = true;
            }

            b'e' | b'E' => {
                if exact | inexact {
                    return Ok(false.into());
                }

                exact = true;
            }

            b'b' | b'B' => {
                if radix != 0 {
                    return Ok(false.into());
                }

                radix = 2;
            }

            b'o' | b'O' => {
                if radix != 0 {
                    return Ok(false.into());
                }

                radix = 8;
            }

            b'd' | b'D' => {
                if radix != 0 {
                    return Ok(false.into());
                }

                radix = 10;
            }

            b'x' | b'X' => {
                if radix != 0 {
                    return Ok(false.into());
                }

                radix = 16;
            }

            _ => return Ok(false.into()),
        }

        s = &s[2..];
    }

    if radix == 0 {
        radix = 10;
    }

    if s[0] == b'-' {
        s = &s[1..];

        if s[0] == b'i' || s[0] == b'I' {
            if s[1] == 0 {
                if inexact {
                    return Ok(make_complex(
                        Value::encode_f64_value(0.0),
                        Value::encode_f64_value(-1.0),
                    ));
                } else {
                    return Ok(make_complex(
                        Value::encode_int32(0),
                        Value::encode_int32(-1),
                    ));
                }
            }
        }

        negative = true;
        nosign = true;
    } else if s[0] == b'+' {
        s = &s[1..];

        if s[0] == b'i' || s[0] == b'I' {
            if s[1] == 0 {
                if inexact {
                    return Ok(make_complex(
                        Value::encode_f64_value(0.0),
                        Value::encode_f64_value(1.0),
                    ));
                } else {
                    return Ok(make_complex(Value::encode_int32(0), Value::encode_int32(1)));
                }
            }
        }

        nosign = false;
    }

    let mut real = Value::encode_bool_value(false);
    let imag = real;
    let angle = real;

    s = parse_ureal(vm, s, radix, exact, &mut real)?;

    if !real.is_false() {
        if negative {
            real = parse_negate(vm, real);
        }

        if s[0] == b'/' {
            return Ok(Value::encode_bool_value(false)); // TODO: Rationals
        }

        if exact {
            real = cnvt_to_exact(real).unwrap();
        }

        if inexact {
            real = cnvt_to_inexact(real).unwrap();
        }

        let angle = match s[0] {
            0 => return Ok(real),
            b'@' => {
                if s[1] == b'+' {
                    s = &s[2..];
                    negative = false;
                    true
                } else if s[1] == b'-' {
                    s = &s[2..];
                    negative = true;
                    true
                } else {
                    s = &s[1..];
                    true
                }
            }

            b'i' | b'I' => {
                if s[1] != 0 {
                    return Ok(false.into());
                }

                if nosign {
                    return Ok(false.into());
                }

                if exact {
                    return Ok(norm_complex(vm, 0.into(), cnvt_to_exact(real).unwrap()));
                }

                if inexact {
                    return Ok(norm_complex(vm, 0.into(), cnvt_to_inexact(real).unwrap()));
                }

                return Ok(norm_complex(vm, 0.into(), real));
            }

            b'+' => {
                if s[1] == b'i' || s[1] == b'I' {
                    if s[2] == 0 {
                        if exact {
                            return Ok(norm_complex(vm, cnvt_to_exact(real).unwrap(), 1.into()));
                        } else if inexact {
                            return Ok(norm_complex(vm, cnvt_to_inexact(real).unwrap(), 1.into()));
                        } else {
                            return Ok(norm_complex(vm, real, 1.into()));
                        }
                    }
                }

                s = &s[1..];
                negative = false;
                false
            }

            b'-' => {
                if s[1] == b'i' || s[1] == b'I' {
                    if s[2] == 0 {
                        if exact {
                            return Ok(norm_complex(
                                vm,
                                cnvt_to_exact(real).unwrap(),
                                Value::encode_int32(-1),
                            ));
                        } else if inexact {
                            return Ok(norm_complex(
                                vm,
                                cnvt_to_inexact(real).unwrap(),
                                Value::encode_f64_value(-1.0),
                            ));
                        } else {
                            return Ok(norm_complex(vm, real, Value::encode_int32(-1)));
                        }
                    }
                }

                s = &s[1..];
                negative = true;
                false
            }

            _ => return Ok(false.into()),
        };

        if angle {
            todo!()
        } else {
            todo!()
        }
    } else {
        Ok(false.into())
    }
}

pub fn parse_uinteger<'a>(
    vm: &mut VM,
    s: &'a [u8],
    radix: u8,
    ans: &mut Value,
) -> Result<&'a [u8], Value> {
    let mut p = s;

    let mut value: i128 = 0;

    let mut c;
    let mut prev;
    loop {
        c = p[0];
        prev = p;
        p = &p[1..];

        let digit;

        if c == 0 {
            break;
        }

        if c == b'#' {
            return Ok(prev);
        }

        if c >= b'0' && c <= b'9' {
            digit = c - b'0';
        } else if c >= b'a' {
            digit = c - b'a' + 10;
        } else if c >= b'A' {
            digit = c - b'A' + 10;
        } else {
            break;
        }

        if digit < radix {
            value *= radix as i128;
            value += digit as i128;

            if value <= i32::MAX as i128 {
                continue;
            }

            return parse_ubignum(vm, s, radix, ans);
        }

        break;
    }

    if prev.as_ptr() != s.as_ptr() {
        *ans = Value::encode_int32(value as i32);
    }

    return Ok(prev);
}

fn parse_ubignum<'a>(
    vm: &mut VM,
    s: &'a [u8],
    radix: u8,
    ans: &mut Value,
) -> Result<&'a [u8], Value> {
    let mut p = s;
    let mut prev;
    let mut digits = Vec::new();

    let mut c;

    loop {
        c = p[0];
        prev = p;
        p = &p[1..];

        let digit;

        if c == 0 {
            break;
        }

        if c == b'#' {
            return Ok(p);
        }

        if c >= b'0' && c <= b'9' {
            digit = c - b'0';
        } else if c >= b'a' {
            digit = c - b'a' + 10;
        } else if c >= b'A' {
            digit = c - b'A' + 10;
        } else {
            break;
        }

        if digit < radix {
            digits.push(digit);
            continue;
        }

        break;
    }

    if prev.as_ptr() != s.as_ptr() {
        let val = BigInt::from_digits(
            vm.mutator(),
            &digits,
            false,
            &match radix {
                2 => BigInt::BIN_BASE,
                8 => BigInt::OCT_BASE,
                10 => BigInt::DEC_BASE,
                16 => BigInt::HEX_BASE,
                _ => unreachable!(),
            },
        );

        *ans = Value::encode_object_value(val);
    }
    return Ok(prev);
}

fn nextfloat(z: f64) -> f64 {
    let (m, k, _sign) = decode_double(z);

    if m == IEXPT_2N53 - 1 {
        return ((IEXPT_2N53 - 1) as f64) * 2f64.powi(k + 1);
    }

    ((m + 1) as f64).powi(k)
}

fn prevfloat(z: f64) -> f64 {
    let (m, k, _sign) = decode_double(z);

    if m == IEXPT_2N53 - 1 {
        return ((IEXPT_2N53 - 1) as f64) * 2f64.powi(k - 1);
    }

    ((m - 1) as f64).powi(k)
}

/// Reference:
/// William D. Clinger.
/// How to read floating point numbers accurately
/// Proceedings of the ACM SIGPLAN 1990 conference on Programming language design and implementation, p.92-101, June 1990
pub fn algorithm_r(f: u128, e: i32, z0: f64) -> f64 {
    let mut z = z0;
    let x0;
    let pow10e;

    if e >= 0 {
        x0 = Some(f as i128 * 10i128.pow(e as _));
        pow10e = None;
    } else {
        x0 = Some(f as i128);
        pow10e = Some(10i128.pow((-e) as _));
    }

    loop {
        if z.is_infinite() {
            return z;
        }

        let (m, k, _) = decode_double(z);
        let x;
        let y;
        if e >= 0 {
            if k >= 0 {
                x = x0.unwrap();
                y = m as i128;
            } else {
                x = x0.unwrap().ilog(-(k as i128)) as i128;
                y = m as i128;
            }
        } else {
            if k >= 0 {
                x = f as i128;
                y = m as i128 * pow10e.unwrap();
            } else {
                x = 0;
                y = 0;
            }
        }

        let d = x - y;
        let mut d2 = (m.wrapping_add(m) as i128).wrapping_mul(d);

        let neg_d = d < 0;

        if neg_d {
            d2 = -d2;
        }

        let test = d2.cmp(&y);

        if test == Ordering::Less {
            if neg_d && m == IEXPT_2N52 && ucmp3(d2, d2, y) == Ordering::Greater {
                z = prevfloat(z);
                continue;
            }

            return z;
        }
        if test == Ordering::Equal {
            if (m & 1) == 0 {
                if neg_d && m == IEXPT_2N52 {
                    z = prevfloat(z);
                    continue;
                }
                return z;
            }

            return if neg_d { prevfloat(z) } else { nextfloat(z) };
        }
        z = if neg_d { prevfloat(z) } else { nextfloat(z) };
    }
}

pub fn ucmp3(n1: i128, n2: i128, n3: i128) -> Ordering {
    if n1 < n2 {
        return Ordering::Less;
    }

    if n1 > n2 {
        return Ordering::Greater;
    }

    if n1 < n3 {
        return Ordering::Less;
    }

    if n1 > n3 {
        return Ordering::Greater;
    }

    return Ordering::Equal;
}

pub fn parse_udecimal<'a>(
    vm: &mut VM,
    s: &'a [u8],
    radix: u8,
    ans: &mut Value,
) -> Result<&'a [u8], Value> {
    if (s[0] as char).to_lowercase().next().unwrap() == 'n' && &s[1..5] == b"an.0" {
        *ans = Value::encode_nan_value();
        return Ok(&s[5..]);
    }

    if (s[0] as char).to_lowercase().next().unwrap() == 'i' && &s[1..5] == b"nf.0" {
        *ans = Value::encode_f64_value(f64::INFINITY);
        return Ok(&s[5..]);
    }

    if s[0] == 0 || radix != 10 {
        return Ok(s);
    }

    let mut p = s;
    let mut prev;

    let mut digit_count = 0;
    let mut fraction_count = 0;
    let mut exponent = 0;
    let mut exponent_negative = false;

    let mut c;
    let mut digit;

    let parse_integral = loop {
        c = p[0];
        prev = p;
        p = &p[1..];

        if c == 0 {
            break false;
        }

        if c == b'0' {
            digit_count += 1;
            continue;
        }

        p = prev;

        break true;
    };

    if !parse_integral {
        if prev.as_ptr() != s.as_ptr() {
            *ans = Value::encode_f64_value(0.0);
        }

        return Ok(prev);
    }

    #[derive(PartialEq, Clone, Copy, Eq)]
    enum Parse {
        Done,
        Fraction,
        Exponent,
        Precision,
    }

    let mut value = 0i128;

    let mut parse = loop {
        c = p[0];
        prev = p;
        p = &p[1..];
        if c == 0 {
            break Parse::Done;
        }
        if c == b'#' {
            return Ok(prev);
        }

        if c >= b'0' && c <= b'9' {
            digit_count += 1;
            digit = c - b'0';

            value = value.wrapping_mul(10); //arith_mul(vm, value, Value::encode_int32(10));

            value = value.wrapping_add(digit as i128);
            //arith_add(vm, value, Value::encode_int32(digit as i32));
            continue;
        }

        if c == b'.' {
            break Parse::Fraction;
        }

        if "esfdlESFDL".contains(c as char) {
            break Parse::Exponent;
        }

        p = prev;
        break Parse::Precision;
    };
    'parser: loop {
        match parse {
            Parse::Exponent => {
                if p[0] == b'|' {
                    return Ok(p);
                }

                if p[0] == b'-' {
                    exponent_negative = true;
                    p = &p[1..];
                } else if p[0] == b'+' {
                    p = &p[1..];
                }

                if p[0] != 0 {
                    loop {
                        c = p[0];
                        prev = p;
                        p = &p[1..];

                        if c == 0 {
                            break;
                        }

                        if c == b'#' {
                            return Ok(prev);
                        }

                        if c >= b'0' && c <= b'9' {
                            digit = c - b'0';
                            exponent = exponent * 10 + digit as i32;
                            if exponent < 0 {
                                return raise_exn!(
                                    FailOutOfMemory,
                                    &[],
                                    "flonum exponent overflow"
                                );
                            }
                            continue;
                        }

                        p = prev;
                        parse = Parse::Precision;
                        continue 'parser;
                    }
                } else {
                    return Ok(p);
                }

                p = prev;
                parse = Parse::Precision;
            }

            Parse::Fraction => {
                loop {
                    c = p[0];
                    prev = p;
                    p = &p[1..];
                    if c == 0 {
                        break;
                    }
                    if c == b'#' {
                        return Ok(prev);
                    }

                    if c >= b'0' && c <= b'9' {
                        if digit_count < 308 {
                            digit_count += 1;
                            digit = c - b'0';

                            value = value.wrapping_mul(10);
                            //arith_mul(vm, value, Value::encode_int32(10));
                            value = value.wrapping_add(digit as i128);
                            // arith_add(vm, value, Value::encode_int32(digit as i32));
                            fraction_count += 1;
                        }
                        continue;
                    }

                    if "esfdlESFDL".contains(c as char) {
                        parse = Parse::Exponent;
                        continue 'parser;
                    }
                    p = prev;
                    parse = Parse::Precision;
                    continue 'parser;
                }

                p = prev;
                parse = Parse::Done;
            }

            Parse::Precision => {
                if c == b'|' {
                    let mut precision = Value::encode_int32(0);
                    p = parse_uinteger(vm, p, 10, &mut precision)?;
                }

                parse = Parse::Done;
            }

            Parse::Done => {
                if digit_count == 0 {
                    return Ok(p);
                }

                if value == 0 {
                    *ans = Value::encode_f64_value(0.0);
                    return Ok(p);
                }

                if exponent_negative {
                    exponent = -exponent;
                }

                exponent = exponent - fraction_count as i32;

                let estimation = pow10n(value as f64, exponent);
                //value as f64 * 10.0f64.powi(exponent);

                *ans = Value::encode_f64_value(algorithm_r(value as _, exponent, estimation));
                return Ok(p);
            }
        }
    }
}

pub const IEXPT_2N52: i64 = 0x10000000000000;
pub const IEXPT_2N53: i64 = 0x20000000000000;

pub fn decode_double(n: f64) -> (i64, i32, i32) {
    let bits = n.to_bits();

    let mant_bits = bits & (IEXPT_2N52 - 1) as u64;
    let sign_bits = bits >> 63;
    let exp_bits = (bits >> 52) & 0x7ff;

    if n == 0.0 {
        return (0, 0, if sign_bits != 0 { -1 } else { 1 });
    }

    if n.is_nan() {
        return (0x18000000000000, 972, 1);
    }

    if n.is_infinite() {
        return (0x10000000000000, 972, if sign_bits != 0 { -1 } else { 1 });
    }

    let exp = if exp_bits != 0 {
        exp_bits as i64 - 1023
    } else {
        -1022
    } - 52;

    let sign = if sign_bits != 0 { -1 } else { 1 };

    let mant_bits = if exp_bits != 0 {
        mant_bits as i64 | IEXPT_2N52
    } else {
        mant_bits as i64
    };

    (mant_bits, exp as i32, sign)
}

pub fn parse_ureal<'a>(
    vm: &mut VM,
    s: &'a [u8],
    radix: u8,
    _exact: bool,
    ans: &mut Value,
) -> Result<&'a [u8], Value> {
    if radix == 10 {
        let n = s.iter().position(|&c| ".esfdlESFDL|".contains(c as char));

        if let Some(n) = n {
            if s.iter()
                .position(|&c| c == b'/')
                .map(|p| p > n)
                .unwrap_or(true)
            {
                return parse_udecimal(vm, s, radix, ans);
            }
        }
    }

    parse_uinteger(vm, s, radix, ans)
}

fn pow10n(mut value: f64, mut n: i32) -> f64 {
    const BIGTENS: [f64; 5] = [1.0e+16, 1.0e+32, 1.0e+64, 1.0e+128, 1.0e+256];
    const TENS: [f64; 23] = [
        1.0, 1.0e+1, 1.0e+2, 1.0e+3, 1.0e+4, 1.0e+5, 1.0e+6, 1.0e+7, 1.0e+8, 1.0e+9, 1.0e+10,
        1.0e+11, 1.0e+12, 1.0e+13, 1.0e+14, 1.0e+15, 1.0e+16, 1.0e+17, 1.0e+18, 1.0e+19, 1.0e+20,
        1.0e+21, 1.0e+22,
    ];

    let mut inflate = true;

    if n < 0 {
        inflate = false;
        n = -n;
    }

    if n <= 22 {
        return if inflate {
            value * TENS[n as usize]
        } else {
            value / TENS[n as usize]
        };
    }

    if n > 511 {
        n = 511;
    }

    if (n & 0x0f) != 0 {
        if inflate {
            value *= TENS[(n & 0x0f) as usize];
        } else {
            value /= TENS[(n & 0x0f) as usize];
        }
    }

    n >>= 4;

    for i in 0..=4 {
        if (n & 1) != 0 {
            if inflate {
                value *= BIGTENS[i];
            } else {
                value /= BIGTENS[i];
            }
        }

        n >>= 1;
    }
    if value == 0.0 {
        return 1.0 * (-1074f64).powi(2);
    }
    value
}
