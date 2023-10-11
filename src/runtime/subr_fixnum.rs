use crate::vm::thread::Thread;

use super::{
    control::wrong_type_argument_violation,
    gsubr::{scm_define_subr, Subr},
    value::Value,
};

extern "C-unwind" fn fx_eq(thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    if !a.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(thread, "fx=", 0, "fixnum", *a, 2, &[a, b]);
    }

    if !b.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(thread, "fx=", 1, "fixnum", *b, 2, &[a, b]);
    }

    Value::encode_bool_value(a == b)
}

extern "C-unwind" fn fx_lt(thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    if !a.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(thread, "fx<", 0, "fixnum", *a, 2, &[a, b]);
    }

    if !b.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(thread, "fx<", 1, "fixnum", *b, 2, &[a, b]);
    }

    Value::encode_bool_value(a.get_int32() < b.get_int32())
}

extern "C-unwind" fn fx_gt(thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    if !a.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(thread, "fx>", 0, "fixnum", *a, 2, &[a, b]);
    }

    if !b.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(thread, "fx>", 1, "fixnum", *b, 2, &[a, b]);
    }

    Value::encode_bool_value(a.get_int32() > b.get_int32())
}

extern "C-unwind" fn fx_le(thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    if !a.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "fx<=",
            0,
            "fixnum",
            *a,
            2,
            &[a, b],
        );
    }

    if !b.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "fx<=",
            1,
            "fixnum",
            *b,
            2,
            &[a, b],
        );
    }

    Value::encode_bool_value(a.get_int32() <= b.get_int32())
}

extern "C-unwind" fn fx_ge(thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    if !a.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "fx>=",
            0,
            "fixnum",
            *a,
            2,
            &[a, b],
        );
    }

    if !b.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "fx>=",
            1,
            "fixnum",
            *b,
            2,
            &[a, b],
        );
    }

    Value::encode_bool_value(a.get_int32() >= b.get_int32())
}

extern "C-unwind" fn fx_add(thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    if !a.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(thread, "fx+", 0, "fixnum", *a, 2, &[a, b]);
    }

    if !b.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(thread, "fx+", 1, "fixnum", *b, 2, &[a, b]);
    }

    Value::encode_int32(a.get_int32().wrapping_add(b.get_int32()))
}

extern "C-unwind" fn fx_sub(thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    if !a.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(thread, "fx-", 0, "fixnum", *a, 2, &[a, b]);
    }

    if !b.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(thread, "fx-", 1, "fixnum", *b, 2, &[a, b]);
    }

    Value::encode_int32(a.get_int32().wrapping_sub(b.get_int32()))
}

extern "C-unwind" fn fx_mul(thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    if !a.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(thread, "fx*", 0, "fixnum", *a, 2, &[a, b]);
    }

    if !b.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(thread, "fx*", 1, "fixnum", *b, 2, &[a, b]);
    }

    Value::encode_int32(a.get_int32().wrapping_mul(b.get_int32()))
}

extern "C-unwind" fn fx_div(thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    if !a.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(thread, "fx/", 0, "fixnum", *a, 2, &[a, b]);
    }

    if !b.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(thread, "fx/", 1, "fixnum", *b, 2, &[a, b]);
    }

    Value::encode_int32(a.get_int32().wrapping_div(b.get_int32()))
}

extern "C-unwind" fn fx_logand(thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    if !a.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "fxlogand",
            0,
            "fixnum",
            *a,
            2,
            &[a, b],
        );
    }

    if !b.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "fxlogand",
            1,
            "fixnum",
            *b,
            2,
            &[a, b],
        );
    }

    Value::encode_int32(a.get_int32() & b.get_int32())
}

extern "C-unwind" fn fx_logior(thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    if !a.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "fxlogior",
            0,
            "fixnum",
            *a,
            2,
            &[a, b],
        );
    }

    if !b.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "fxlogior",
            1,
            "fixnum",
            *b,
            2,
            &[a, b],
        );
    }

    Value::encode_int32(a.get_int32() | b.get_int32())
}

extern "C-unwind" fn fx_logxor(thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    if !a.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "fxlogxor",
            0,
            "fixnum",
            *a,
            2,
            &[a, b],
        );
    }

    if !b.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "fxlogxor",
            1,
            "fixnum",
            *b,
            2,
            &[a, b],
        );
    }

    Value::encode_int32(a.get_int32() ^ b.get_int32())
}

extern "C-unwind" fn fx_lsh(thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    if !a.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "fxlsh",
            0,
            "fixnum",
            *a,
            2,
            &[a, b],
        );
    }

    if !b.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "fxlsh",
            1,
            "fixnum",
            *b,
            2,
            &[a, b],
        );
    }

    Value::encode_int32(a.get_int32().wrapping_shl(b.get_int32() as _))
}

extern "C-unwind" fn fx_sha(thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    if !a.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "fxash",
            0,
            "fixnum",
            *a,
            2,
            &[a, b],
        );
    }

    if !b.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "fxash",
            1,
            "fixnum",
            *b,
            2,
            &[a, b],
        );
    }

    Value::encode_int32(a.get_int32().wrapping_shr(b.get_int32() as _))
}

extern "C-unwind" fn fixnum_p(_thread: &mut Thread, a: &mut Value) -> Value {
    Value::encode_bool_value(a.is_int32())
}

extern "C-unwind" fn fxrshl(thread: &mut Thread, a: &mut Value, b: &mut Value) -> Value {
    if !a.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "fxrshl",
            0,
            "fixnum",
            *a,
            2,
            &[a, b],
        );
    }

    if !b.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "fxrshl",
            1,
            "fixnum",
            *b,
            2,
            &[a, b],
        );
    }

    Value::encode_int32(a.get_int32().wrapping_shl(b.get_int32() as _))
}

pub(crate) fn init() {
    scm_define_subr("fx=", 2, 0, 0, Subr::F2(fx_eq));
    scm_define_subr("fx<", 2, 0, 0, Subr::F2(fx_lt));
    scm_define_subr("fx>", 2, 0, 0, Subr::F2(fx_gt));
    scm_define_subr("fx<=", 2, 0, 0, Subr::F2(fx_le));
    scm_define_subr("fx>=", 2, 0, 0, Subr::F2(fx_ge));
    scm_define_subr("fx+", 2, 0, 0, Subr::F2(fx_add));
    scm_define_subr("fx-", 2, 0, 0, Subr::F2(fx_sub));
    scm_define_subr("fx*", 2, 0, 0, Subr::F2(fx_mul));
    scm_define_subr("fx/", 2, 0, 0, Subr::F2(fx_div));
    scm_define_subr("fxlogand", 2, 0, 0, Subr::F2(fx_logand));
    scm_define_subr("fxlogior", 2, 0, 0, Subr::F2(fx_logior));
    scm_define_subr("fxlogxor", 2, 0, 0, Subr::F2(fx_logxor));
    scm_define_subr("fxlsh", 2, 0, 0, Subr::F2(fx_lsh));
    scm_define_subr("fxsha", 2, 0, 0, Subr::F2(fx_sha));
    scm_define_subr("fixnum?", 1, 0, 0, Subr::F1(fixnum_p));
    scm_define_subr("fxrshl", 2, 0, 0, Subr::F2(fxrshl));
}
