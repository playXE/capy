use std::cmp::Ordering;

use crate::vm::thread::Thread;

use super::{
    arith,
    control::{invalid_argument_violation, wrong_type_argument_violation},
    gsubr::{scm_define_subr, Subr},
    list::scm_length,
    object::{scm_car, scm_cdr, scm_string_str},
    value::Value,
};

extern "C-unwind" fn string_to_number(
    thread: &mut Thread,
    text: &mut Value,
    radix: &mut Value,
) -> Value {
    let rdx: u32 = if radix.is_undefined() {
        10
    } else if radix.is_int32() {
        match radix.get_int32() {
            2 => 2,
            8 => 8,
            10 => 10,
            16 => 16,
            _ => {
                invalid_argument_violation(
                    thread,
                    "string->number",
                    "radix can only be 2, 8, 10, 16",
                    *radix,
                    1,
                    2,
                    &[text, radix],
                );
            }
        }
    } else {
        invalid_argument_violation(
            thread,
            "string->number",
            "exact nonnegative integer",
            *radix,
            1,
            2,
            &[text, radix],
        );
    };

    if !text.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "string->number",
            0,
            "string",
            *text,
            2,
            &[text, radix],
        );
    }

    let txt = scm_string_str(*text);

    let num = arith::parse_number(thread, txt, rdx);

    if let Some(num) = num {
        return num;
    }

    Value::encode_bool_value(false)
}

extern "C-unwind" fn number_to_string(
    thread: &mut Thread,
    num: &mut Value,
    radix: &mut Value,
) -> Value {
    let rdx: u32 = if radix.is_undefined() {
        10
    } else if radix.is_int32() {
        match radix.get_int32() {
            2 => 2,
            8 => 8,
            10 => 10,
            16 => 16,
            _ => {
                invalid_argument_violation(
                    thread,
                    "string->number",
                    "radix can only be 2, 8, 10, 16",
                    *radix,
                    1,
                    2,
                    &[num, radix],
                );
            }
        }
    } else {
        invalid_argument_violation(
            thread,
            "string->number",
            "exact nonnegative integer",
            *radix,
            1,
            2,
            &[num, radix],
        );
    };

    if !num.is_number() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "number->string",
            0,
            "number",
            *num,
            2,
            &[num, radix],
        )
    }

    let s = arith::cnvt_number_to_string(thread, *num, rdx as _);

    thread.make_string::<false>(&s)
}

pub(crate) extern "C-unwind" fn intrinsic_add(
    thread: &mut Thread,
    mut a: Value,
    mut b: Value,
) -> Value {
    if !a.is_number() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "+",
            0,
            "number",
            a,
            2,
            &[&mut a, &mut b],
        );
    }

    if !b.is_number() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "+",
            1,
            "number",
            b,
            2,
            &[&mut a, &mut b],
        );
    }
    arith::arith_add(thread, a, b)
}

pub(crate) extern "C-unwind" fn intrinsic_sub(
    thread: &mut Thread,
    mut a: Value,
    mut b: Value,
) -> Value {
    if !a.is_number() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "-",
            0,
            "number",
            a,
            2,
            &[&mut a, &mut b],
        );
    }

    if !b.is_number() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "-",
            1,
            "number",
            b,
            2,
            &[&mut a, &mut b],
        );
    }
    arith::arith_sub(thread, a, b)
}

pub(crate) extern "C-unwind" fn intrinsic_mul(
    thread: &mut Thread,
    mut a: Value,
    mut b: Value,
) -> Value {
    if !a.is_number() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "*",
            0,
            "number",
            a,
            2,
            &[&mut a, &mut b],
        );
    }

    if !b.is_number() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "*",
            1,
            "number",
            b,
            2,
            &[&mut a, &mut b],
        );
    }
    arith::arith_mul(thread, a, b)
}

pub(crate) extern "C-unwind" fn intrinsic_div(
    thread: &mut Thread,
    mut a: Value,
    mut b: Value,
) -> Value {
    if !a.is_number() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "/",
            0,
            "number",
            a,
            2,
            &[&mut a, &mut b],
        );
    }

    if !b.is_number() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "/",
            1,
            "number",
            b,
            2,
            &[&mut a, &mut b],
        );
    }

    if b == Value::encode_int32(0) {
        invalid_argument_violation(thread, "/", "undefined for 0", b, 1, 2, &[&mut a, &mut b]);
    }

    arith::arith_div(thread, a, b)
}

extern "C-unwind" fn subr_abs(thread: &mut Thread, a: &mut Value) -> Value {
    if arith::real_p(*a) {
        return arith::arith_magnitude(thread, *a);
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(thread, "abs", 0, "real", *a, 1, &[a])
    }
}

// div
extern "C-unwind" fn subr_int_div(thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    if arith::real_p(*a) {
        if arith::n_finite_p(*a) {
            if arith::real_p(*b) {
                if arith::n_finite_p(*b) {
                    return arith::arith_integer_div(thread, *a, *b);
                } else {
                    invalid_argument_violation(
                        thread,
                        "div",
                        "neither infinite nor a NaN",
                        *b,
                        1,
                        2,
                        &[a, b],
                    );
                }
            } else {
                wrong_type_argument_violation::<{ usize::MAX }>(
                    thread,
                    "div",
                    1,
                    "real",
                    *b,
                    2,
                    &[a, b],
                )
            }
        } else {
            invalid_argument_violation(
                thread,
                "div",
                "neither infinite nor a NaN",
                *a,
                0,
                2,
                &[a, b],
            );
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(thread, "div", 0, "real", *a, 2, &[a, b])
    }
}

extern "C-unwind" fn subr_int_div0(thread: &mut Thread, lhs: &mut Value, rhs: &mut Value) -> Value {
    if !arith::real_p(*lhs) {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "div0",
            0,
            "real",
            *lhs,
            2,
            &[lhs, rhs],
        )
    }

    if !arith::n_finite_p(*lhs) {
        invalid_argument_violation(
            thread,
            "div0",
            "neither infinite nor a NaN",
            *lhs,
            0,
            2,
            &[lhs, rhs],
        );
    }

    if !arith::real_p(*rhs) {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "div0",
            1,
            "real",
            *rhs,
            2,
            &[lhs, rhs],
        )
    }

    if !arith::n_finite_p(*rhs) {
        invalid_argument_violation(
            thread,
            "div0",
            "neither infinite nor a NaN",
            *rhs,
            1,
            2,
            &[lhs, rhs],
        );
    }

    if !arith::n_zero_p(*rhs) {
        invalid_argument_violation(thread, "div0", "not zero", *rhs, 1, 2, &[lhs, rhs]);
    }

    return arith::arith_integer_div(thread, *lhs, *rhs);
}

extern "C-unwind" fn subr_numerator(thread: &mut Thread, a: &mut Value) -> Value {
    if arith::real_value_p(*a) {
        if a.is_double() && a.get_double() == 0.0 {
            return *a;
        }

        let inexact = a.is_double();

        let obj = arith::cnvt_to_exact(thread, *a);
        if obj.is_rational() {
            if inexact {
                return arith::cnvt_to_inexact(thread, obj.get_rational().numerator);
            }

            return obj.get_rational().numerator;
        }

        if inexact {
            return arith::cnvt_to_inexact(thread, obj);
        } else {
            return obj;
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(thread, "numerator", 0, "real", *a, 1, &[a])
    }
}

extern "C-unwind" fn subr_denominator(thread: &mut Thread, a: &mut Value) -> Value {
    if arith::real_value_p(*a) {
        if a.is_double() && a.get_double() == 0.0 {
            return *a;
        }

        let inexact = a.is_double();

        let obj = arith::cnvt_to_exact(thread, *a);
        if obj.is_rational() {
            if inexact {
                return arith::cnvt_to_inexact(thread, obj.get_rational().denominator);
            }

            return obj.get_rational().denominator;
        }

        if inexact {
            return Value::encode_f64_value(1.0);
        } else {
            return Value::encode_int32(1);
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "denominator",
            0,
            "real",
            *a,
            1,
            &[a],
        )
    }
}
extern "C-unwind" fn subr_floor(thread: &mut Thread, obj: &mut Value) -> Value {
    if obj.is_complex() {
        if arith::n_zero_p(obj.get_complex().imag) {
            *obj = obj.get_complex().real;
        } else {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "floor",
                0,
                "real",
                *obj,
                1,
                &[obj],
            )
        }
    }

    if obj.is_double() {
        return Value::encode_f64_value(obj.get_double().floor());
    }

    if obj.is_int32() || obj.is_bignum() {
        return *obj;
    }

    if obj.is_rational() {
        return arith::arith_floor(thread, *obj);
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "floor",
            0,
            "number",
            *obj,
            1,
            &[obj],
        )
    }
}

extern "C-unwind" fn subr_ceiling(thread: &mut Thread, obj: &mut Value) -> Value {
    if obj.is_complex() {
        if arith::n_zero_p(obj.get_complex().imag) {
            *obj = obj.get_complex().real;
        } else {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "ceiling",
                0,
                "real",
                *obj,
                1,
                &[obj],
            )
        }
    }

    if obj.is_double() {
        return Value::encode_f64_value(obj.get_double().ceil());
    }

    if obj.is_int32() || obj.is_bignum() {
        return *obj;
    }

    if obj.is_rational() {
        if !arith::n_negative_p(obj.get_rational().numerator) {
            let quotient = arith::arith_quotient(
                thread,
                obj.get_rational().numerator,
                obj.get_rational().denominator,
            );
            return arith::arith_add(thread, quotient, Value::encode_int32(1));
        }

        return arith::arith_quotient(
            thread,
            obj.get_rational().numerator,
            obj.get_rational().denominator,
        );
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "ceiling",
            0,
            "number",
            *obj,
            1,
            &[obj],
        )
    }
}

extern "C-unwind" fn subr_truncate(thread: &mut Thread, obj: &mut Value) -> Value {
    if obj.is_complex() {
        if arith::n_zero_p(obj.get_complex().imag) {
            *obj = obj.get_complex().real;
        } else {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "truncate",
                0,
                "real",
                *obj,
                1,
                &[obj],
            )
        }
    }

    if obj.is_double() {
        return Value::encode_f64_value(obj.get_double().trunc());
    }

    if obj.is_int32() || obj.is_bignum() {
        return *obj;
    }

    if obj.is_rational() {
        return arith::arith_quotient(
            thread,
            obj.get_rational().numerator,
            obj.get_rational().denominator,
        );
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "truncate",
            0,
            "number",
            *obj,
            1,
            &[obj],
        )
    }
}

extern "C-unwind" fn round(thread: &mut Thread, obj: &mut Value) -> Value {
    if obj.is_complex() {
        if arith::n_zero_p(obj.get_complex().imag) {
            *obj = obj.get_complex().real;
        } else {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "round",
                0,
                "real",
                *obj,
                1,
                &[obj],
            )
        }
    }

    if obj.is_double() {
        let value = obj.get_double();
        let ans = (value + 0.5).floor();
        if ans != value + 0.5 {
            return Value::encode_f64_value(ans);
        }

        if ans * 0.5 == (ans * 0.5).floor() {
            return Value::encode_f64_value(ans);
        }

        return Value::encode_f64_value(ans - 1.0);
    }

    if obj.is_int32() || obj.is_bignum() {
        return *obj;
    }

    if obj.is_rational() {
        let negative = arith::n_negative_p(obj.get_rational().numerator);

        let half = thread.make_rational::<false>(
            Value::encode_int32(if negative { -1 } else { 1 }),
            Value::encode_int32(2),
        );
        let n_and_half = arith::arith_add(thread, *obj, half);

        if n_and_half.is_rational() {
            return arith::arith_quotient(
                thread,
                n_and_half.get_rational().numerator,
                n_and_half.get_rational().denominator,
            );
        } else {
            if arith::n_even_p(n_and_half) {
                return n_and_half;
            }

            return arith::arith_add(
                thread,
                n_and_half,
                Value::encode_int32(if negative { -1 } else { 1 }),
            );
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "round",
            0,
            "number",
            *obj,
            1,
            &[obj],
        )
    }
}

extern "C-unwind" fn subr_exp(thread: &mut Thread, obj: &mut Value) -> Value {
    if arith::number_p(*obj) {
        return arith::arith_exp(thread, *obj);
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(thread, "exp", 0, "number", *obj, 1, &[obj])
    }
}

extern "C-unwind" fn subr_expt(thread: &mut Thread, lhs: &mut Value, rhs: &mut Value) -> Value {
    if arith::number_p(*lhs) {
        if arith::number_p(*rhs) {
            return arith::arith_expt(thread, *lhs, *rhs);
        } else {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "expt",
                1,
                "number",
                *rhs,
                2,
                &[lhs, rhs],
            )
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "expt",
            0,
            "number",
            *lhs,
            2,
            &[lhs, rhs],
        )
    }
}

extern "C-unwind" fn subr_log(thread: &mut Thread, lhs: &mut Value, rhs: &mut Value) -> Value {
    if rhs.is_undefined() {
        // (log <val>)

        if arith::number_p(*lhs) {
            if lhs.is_int32() && lhs.get_int32() == 0 {
                invalid_argument_violation(thread, "log", "undefined for 0", *lhs, 0, 1, &[lhs]);
            }
            return arith::arith_log(thread, *lhs);
        } else {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "log",
                0,
                "number",
                *lhs,
                1,
                &[lhs, rhs],
            )
        }
    }

    if !arith::number_p(*lhs) {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "log",
            0,
            "number",
            *lhs,
            2,
            &[lhs, rhs],
        )
    }

    if lhs.is_int32() && lhs.get_int32() == 0 {
        invalid_argument_violation(thread, "log", "undefined for 0", *lhs, 0, 2, &[lhs, rhs]);
    }

    if !arith::number_p(*rhs) {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "log",
            1,
            "number",
            *rhs,
            2,
            &[lhs, rhs],
        )
    }

    *lhs = arith::arith_log(thread, *lhs);
    let log_rhs = arith::arith_log(thread, *rhs);

    arith::arith_div(thread, *lhs, log_rhs)
}

extern "C-unwind" fn subr_infinite_p(thread: &mut Thread, lhs: &mut Value) -> Value {
    if arith::real_p(*lhs) {
        if lhs.is_double() {
            return Value::encode_bool_value(lhs.get_double().is_infinite());
        } else {
            return Value::encode_bool_value(false);
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "infinite?",
            0,
            "real",
            *lhs,
            1,
            &[lhs],
        )
    }
}

extern "C-unwind" fn subr_finite_p(thread: &mut Thread, lhs: &mut Value) -> Value {
    if arith::real_p(*lhs) {
        if lhs.is_double() {
            return Value::encode_bool_value(
                !(lhs.get_double().is_finite() || lhs.get_double().is_nan()),
            );
        } else {
            return Value::encode_bool_value(true);
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "finite?",
            0,
            "real",
            *lhs,
            1,
            &[lhs],
        )
    }
}

extern "C-unwind" fn subr_nan_p(thread: &mut Thread, lhs: &mut Value) -> Value {
    if arith::real_p(*lhs) {
        if lhs.is_double() {
            return Value::encode_bool_value(lhs.get_double().is_nan());
        } else {
            return Value::encode_bool_value(false);
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "finite?",
            0,
            "real",
            *lhs,
            1,
            &[lhs],
        )
    }
}

extern "C-unwind" fn subr_even_p(thread: &mut Thread, lhs: &mut Value) -> Value {
    if arith::integer_value_p(*lhs) {
        return Value::encode_bool_value(arith::n_even_p(*lhs));
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "even?",
            0,
            "integer",
            *lhs,
            1,
            &[lhs],
        )
    }
}

extern "C-unwind" fn subr_odd_p(thread: &mut Thread, lhs: &mut Value) -> Value {
    if arith::integer_value_p(*lhs) {
        return Value::encode_bool_value(!arith::n_even_p(*lhs));
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "odd?",
            0,
            "integer",
            *lhs,
            1,
            &[lhs],
        )
    }
}

extern "C-unwind" fn subr_negative_p(thread: &mut Thread, lhs: &mut Value) -> Value {
    if arith::real_value_p(*lhs) {
        return Value::encode_bool_value(arith::n_negative_p(*lhs));
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "negative?",
            0,
            "real",
            *lhs,
            1,
            &[lhs],
        )
    }
}

extern "C-unwind" fn subr_positive_p(thread: &mut Thread, lhs: &mut Value) -> Value {
    if arith::real_value_p(*lhs) {
        return Value::encode_bool_value(arith::n_positive_p(*lhs));
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "positive?",
            0,
            "real",
            *lhs,
            1,
            &[lhs],
        )
    }
}

extern "C-unwind" fn subr_zero_p(thread: &mut Thread, lhs: &mut Value) -> Value {
    if arith::number_p(*lhs) {
        return Value::encode_bool_value(arith::n_zero_p(*lhs));
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "zero?",
            0,
            "number",
            *lhs,
            1,
            &[lhs],
        )
    }
}

extern "C-unwind" fn inexact(thread: &mut Thread, obj: &mut Value) -> Value {
    if arith::number_p(*obj) {
        return arith::cnvt_to_inexact(thread, *obj);
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "inexact",
            0,
            "number",
            *obj,
            1,
            &[obj],
        )
    }
}

extern "C-unwind" fn exact(thread: &mut Thread, obj: &mut Value) -> Value {
    if arith::number_p(*obj) {
        return arith::cnvt_to_exact(thread, *obj);
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "exact",
            0,
            "number",
            *obj,
            1,
            &[obj],
        )
    }
}

extern "C-unwind" fn subr_inexact_p(thread: &mut Thread, obj: &mut Value) -> Value {
    if arith::number_p(*obj) {
        return Value::encode_bool_value(!arith::n_exact_p(*obj));
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "inexact?",
            0,
            "number",
            *obj,
            1,
            &[obj],
        )
    }
}

extern "C-unwind" fn subr_exact_p(thread: &mut Thread, obj: &mut Value) -> Value {
    if arith::number_p(*obj) {
        return Value::encode_bool_value(arith::n_exact_p(*obj));
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "exact?",
            0,
            "number",
            *obj,
            1,
            &[obj],
        )
    }
}

extern "C-unwind" fn subr_integer_valued_p(_thread: &mut Thread, obj: &mut Value) -> Value {
    return Value::encode_bool_value(arith::integer_value_p(*obj));
}

extern "C-unwind" fn subr_rational_valued_p(_thread: &mut Thread, obj: &mut Value) -> Value {
    return Value::encode_bool_value(arith::rational_value_p(*obj));
}

extern "C-unwind" fn subr_real_valued_p(_thread: &mut Thread, obj: &mut Value) -> Value {
    return Value::encode_bool_value(arith::real_value_p(*obj));
}

extern "C-unwind" fn subr_rational_p(_thread: &mut Thread, obj: &mut Value) -> Value {
    return Value::encode_bool_value(arith::rational_p(*obj));
}

extern "C-unwind" fn subr_integer_p(_thread: &mut Thread, obj: &mut Value) -> Value {
    return Value::encode_bool_value(arith::integer_p(*obj));
}

extern "C-unwind" fn subr_real_p(_thread: &mut Thread, obj: &mut Value) -> Value {
    return Value::encode_bool_value(arith::real_p(*obj));
}

extern "C-unwind" fn subr_complex_p(_thread: &mut Thread, obj: &mut Value) -> Value {
    return Value::encode_bool_value(obj.is_number());
}

pub(crate) fn intrinsic_quotient(thread: &mut Thread, mut a: Value, mut b: Value) -> Value {
    if arith::integer_value_p(a) {
        if arith::integer_value_p(b) {
            return arith::arith_quotient(thread, a, b);
        } else {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "quotient",
                1,
                "integer",
                b,
                2,
                &[&mut a, &mut b],
            )
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "quotient",
            0,
            "integer",
            a,
            2,
            &[&mut a, &mut b],
        )
    }
}

pub(crate) fn intrinsic_remainder(thread: &mut Thread, mut a: Value, mut b: Value) -> Value {
    if arith::integer_value_p(a) {
        if arith::integer_value_p(b) {
            return arith::arith_remainder(thread, a, b);
        } else {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "remainder",
                1,
                "integer",
                b,
                2,
                &[&mut a, &mut b],
            )
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "remainder",
            0,
            "integer",
            a,
            2,
            &[&mut a, &mut b],
        )
    }
}

extern "C-unwind" fn subr_modulo(thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    if arith::integer_value_p(*a) {
        if arith::integer_value_p(*b) {
            return arith::arith_modulo(thread, *a, *b);
        } else {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "modulo",
                1,
                "integer",
                *a,
                2,
                &[a, b],
            )
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "modulo",
            0,
            "integer",
            *a,
            2,
            &[a, b],
        )
    }
}

extern "C-unwind" fn subr_bitwise_bit_count(thread: &mut Thread, a: &mut Value) -> Value {
    if arith::exact_integer_p(*a) {
        return arith::arith_bit_count(thread, *a);
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "bitwise-bit-count",
            0,
            "exact integer",
            *a,
            1,
            &[a],
        );
    }
}

extern "C-unwind" fn bitwise_length(thread: &mut Thread, a: &mut Value) -> Value {
    if arith::exact_integer_p(*a) {
        return arith::arith_bit_length(*a);
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "bitwise-length",
            0,
            "exact integer",
            *a,
            1,
            &[a],
        );
    }
}
extern "C-unwind" fn bitwise_first_bit_set(thread: &mut Thread, a: &mut Value) -> Value {
    if arith::exact_integer_p(*a) {
        return arith::arith_first_bit_set(*a);
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "bitwise-first-bit-set",
            0,
            "exact integer",
            *a,
            1,
            &[a],
        );
    }
}

extern "C-unwind" fn bitwise_not(thread: &mut Thread, a: &mut Value) -> Value {
    if arith::exact_integer_p(*a) {
        return arith::arith_lognot(thread, *a);
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "bitwise-not",
            0,
            "exact integer",
            *a,
            1,
            &[a],
        );
    }
}

extern "C-unwind" fn bitwise_and(thread: &mut Thread, rest: &mut Value) -> Value {
    let length = scm_length(*rest).unwrap(); // guaranteed to be a valid list

    if length == 0 {
        return Value::encode_int32(-1);
    }

    if length == 1 {
        if arith::exact_integer_p(scm_car(*rest)) {
            return scm_car(*rest);
        } else {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "bitwise-and",
                0,
                "exact integer",
                scm_car(*rest),
                1,
                &[rest],
            );
        }
    }

    if length == 2 {
        let mut a = scm_car(*rest);
        let mut b = scm_car(scm_cdr(*rest));

        if arith::exact_integer_p(a) {
            if arith::exact_integer_p(b) {
                return arith::arith_logand(thread, a, b);
            } else {
                wrong_type_argument_violation::<{ usize::MAX }>(
                    thread,
                    "bitwise-and",
                    1,
                    "exact integer",
                    a,
                    2,
                    &[&mut a, &mut b],
                );
            }
        } else {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "bitwise-and",
                0,
                "exact integer",
                a,
                2,
                &[&mut a, &mut b],
            );
        }
    }
    let mut xs = *rest;
    for i in 0..length {
        let arg = scm_car(xs);
        if !arith::exact_integer_p(arg) {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "bitwise-and",
                i,
                "exact integer",
                arg,
                length,
                &[rest],
            );
        }

        xs = scm_cdr(xs);
    }

    let mut acc = scm_car(*rest);
    for _ in 1..length {
        *rest = scm_cdr(*rest);
        acc = arith::arith_logand(thread, acc, scm_car(*rest));
    }

    acc
}

extern "C-unwind" fn bitwise_ior(thread: &mut Thread, rest: &mut Value) -> Value {
    let length = scm_length(*rest).unwrap(); // guaranteed to be a valid list

    if length == 0 {
        return Value::encode_int32(0);
    }

    if length == 1 {
        if arith::exact_integer_p(scm_car(*rest)) {
            return scm_car(*rest);
        } else {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "bitwise-ior",
                0,
                "exact integer",
                scm_car(*rest),
                1,
                &[rest],
            );
        }
    }

    if length == 2 {
        let mut a = scm_car(*rest);
        let mut b = scm_car(scm_cdr(*rest));

        if arith::exact_integer_p(a) {
            if arith::exact_integer_p(b) {
                return arith::arith_logior(thread, a, b);
            } else {
                wrong_type_argument_violation::<{ usize::MAX }>(
                    thread,
                    "bitwise-ior",
                    1,
                    "exact integer",
                    a,
                    2,
                    &[&mut a, &mut b],
                );
            }
        } else {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "bitwise-ior",
                0,
                "exact integer",
                a,
                2,
                &[&mut a, &mut b],
            );
        }
    }
    let mut xs = *rest;
    for i in 0..length {
        let arg = scm_car(xs);
        if !arith::exact_integer_p(arg) {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "bitwise-ior",
                i,
                "exact integer",
                arg,
                length,
                &[rest],
            );
        }

        xs = scm_cdr(xs);
    }

    let mut acc = scm_car(*rest);
    for _ in 1..length {
        *rest = scm_cdr(*rest);
        acc = arith::arith_logior(thread, acc, scm_car(*rest));
    }

    acc
}

extern "C-unwind" fn bitwise_xor(thread: &mut Thread, rest: &mut Value) -> Value {
    let length = scm_length(*rest).unwrap(); // guaranteed to be a valid list

    if length == 0 {
        return Value::encode_int32(0);
    }

    if length == 1 {
        if arith::exact_integer_p(scm_car(*rest)) {
            return scm_car(*rest);
        } else {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "bitwise-xor",
                0,
                "exact integer",
                scm_car(*rest),
                1,
                &[rest],
            );
        }
    }

    if length == 2 {
        let mut a = scm_car(*rest);
        let mut b = scm_car(scm_cdr(*rest));

        if arith::exact_integer_p(a) {
            if arith::exact_integer_p(b) {
                return arith::arith_logxor(thread, a, b);
            } else {
                wrong_type_argument_violation::<{ usize::MAX }>(
                    thread,
                    "bitwise-xor",
                    1,
                    "exact integer",
                    a,
                    2,
                    &[&mut a, &mut b],
                );
            }
        } else {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "bitwise-xor",
                0,
                "exact integer",
                a,
                2,
                &[&mut a, &mut b],
            );
        }
    }
    let mut xs = *rest;
    for i in 0..length {
        let arg = scm_car(xs);
        if !arith::exact_integer_p(arg) {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "bitwise-xor",
                i,
                "exact integer",
                arg,
                length,
                &[rest],
            );
        }

        xs = scm_cdr(xs);
    }

    let mut acc = scm_car(*rest);
    for _ in 1..length {
        *rest = scm_cdr(*rest);
        acc = arith::arith_logxor(thread, acc, scm_car(*rest));
    }

    acc
}

extern "C-unwind" fn bitwise_arithmetic_shift(
    thread: &mut Thread,
    a: &mut Value,
    b: &mut Value,
) -> Value {
    if arith::exact_integer_p(*a) {
        if b.is_int32() {
            return arith::arith_logash(thread, *a, *b);
        } else {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "bitwise-arithmetic-shift",
                1,
                "fixnum",
                *b,
                2,
                &[a, b],
            );
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "bitwise-arithmetic-shift",
            0,
            "exact integer",
            *a,
            2,
            &[a, b],
        );
    }
}

extern "C-unwind" fn subr_mod(thread: &mut Thread, lhs: &mut Value, rhs: &mut Value) -> Value {
    if arith::integer_value_p(*lhs) {
        if arith::integer_value_p(*rhs) {
            return arith::arith_modulo(thread, *lhs, *rhs);
        } else {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "mod",
                1,
                "integer",
                *rhs,
                2,
                &[lhs, rhs],
            )
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "mod",
            0,
            "integer",
            *lhs,
            2,
            &[lhs, rhs],
        )
    }
}

pub(crate) fn init_arith() {
    scm_define_subr("string->number", 1, 1, 0, Subr::F2(string_to_number));
    scm_define_subr("number->string", 1, 1, 0, Subr::F2(number_to_string));
    scm_define_subr("abs", 1, 0, 0, Subr::F1(subr_abs));
    scm_define_subr("div", 2, 0, 0, Subr::F2(subr_int_div));
    scm_define_subr("div0", 2, 0, 0, Subr::F2(subr_int_div0));
    scm_define_subr("numerator", 1, 0, 0, Subr::F1(subr_numerator));
    scm_define_subr("denominator", 1, 0, 0, Subr::F1(subr_denominator));
    scm_define_subr("floor", 1, 0, 0, Subr::F1(subr_floor));
    scm_define_subr("ceiling", 1, 0, 0, Subr::F1(subr_ceiling));
    scm_define_subr("truncate", 1, 0, 0, Subr::F1(subr_truncate));
    scm_define_subr("round", 1, 0, 0, Subr::F1(round));
    scm_define_subr("exp", 1, 0, 0, Subr::F1(subr_exp));
    scm_define_subr("expt", 2, 0, 0, Subr::F2(subr_expt));
    scm_define_subr("log", 1, 1, 0, Subr::F2(subr_log));
    scm_define_subr("infinite?", 1, 0, 0, Subr::F1(subr_infinite_p));
    scm_define_subr("finite?", 1, 0, 0, Subr::F1(subr_finite_p));
    scm_define_subr("nan?", 1, 0, 0, Subr::F1(subr_nan_p));
    scm_define_subr("even?", 1, 0, 0, Subr::F1(subr_even_p));
    scm_define_subr("odd?", 1, 0, 0, Subr::F1(subr_odd_p));
    scm_define_subr("negative?", 1, 0, 0, Subr::F1(subr_negative_p));
    scm_define_subr("positive?", 1, 0, 0, Subr::F1(subr_positive_p));
    scm_define_subr("zero?", 1, 0, 0, Subr::F1(subr_zero_p));
    scm_define_subr("inexact", 1, 0, 0, Subr::F1(inexact));
    scm_define_subr("exact", 1, 0, 0, Subr::F1(exact));
    scm_define_subr("inexact?", 1, 0, 0, Subr::F1(subr_inexact_p));
    scm_define_subr("exact?", 1, 0, 0, Subr::F1(subr_exact_p));
    scm_define_subr("integer-valued?", 1, 0, 0, Subr::F1(subr_integer_valued_p));
    scm_define_subr(
        "rational-valued?",
        1,
        0,
        0,
        Subr::F1(subr_rational_valued_p),
    );
    scm_define_subr("real-valued?", 1, 0, 0, Subr::F1(subr_real_valued_p));
    scm_define_subr("rational?", 1, 0, 0, Subr::F1(subr_rational_p));
    scm_define_subr("integer?", 1, 0, 0, Subr::F1(subr_integer_p));
    scm_define_subr("real?", 1, 0, 0, Subr::F1(subr_real_p));
    scm_define_subr("complex?", 1, 0, 0, Subr::F1(subr_complex_p));
    scm_define_subr("modulo", 2, 0, 0, Subr::F2(subr_modulo));
    scm_define_subr("mod", 2, 0, 0, Subr::F2(subr_mod));
    scm_define_subr(
        "bitwise-bit-count",
        1,
        0,
        0,
        Subr::F1(subr_bitwise_bit_count),
    );
    scm_define_subr("bitwise-length", 1, 0, 0, Subr::F1(bitwise_length));
    scm_define_subr(
        "bitwise-first-bit-set",
        1,
        0,
        0,
        Subr::F1(bitwise_first_bit_set),
    );
    scm_define_subr("bitwise-not", 1, 0, 0, Subr::F1(bitwise_not));
    scm_define_subr("bitwise-and", 0, 0, 1, Subr::F1(bitwise_and));
    scm_define_subr("bitwise-ior", 0, 0, 1, Subr::F1(bitwise_ior));
    scm_define_subr("bitwise-xor", 0, 0, 1, Subr::F1(bitwise_xor));
    scm_define_subr(
        "bitwise-arithmetic-shift",
        2,
        0,
        0,
        Subr::F2(bitwise_arithmetic_shift),
    );
}

pub fn scm_n_compare(thread: &mut Thread, mut a: Value, mut b: Value, who: &str) -> Ordering {
    if !a.is_number() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            who,
            0,
            "number",
            a,
            1,
            &[&mut a, &mut b],
        );
    }

    if !b.is_number() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            who,
            1,
            "number",
            b,
            2,
            &[&mut a, &mut b],
        );
    }

    arith::n_compare(thread, a, b)
}
