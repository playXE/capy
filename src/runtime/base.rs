use std::{
    io::Cursor,
    sync::atomic::{AtomicI64, Ordering},
};

use murmur3::murmur3_32;
use rsgc::{system::arraylist::ArrayList, thread::Thread};

use crate::{
    compile::{make_iform, Asm, AsmOperand, IForm},
    op::Opcode,
    raise_exn,
    vm::{callframe::CallFrame, scm_vm},
};

use super::{
    arith::scm_is_exact_non_negative_integer,
    error::{out_of_range, wrong_contract},
    fun::{scm_make_subr, scm_make_subr_inliner},
    list::{scm_cons, scm_is_list, scm_length},
    module::{scm_capy_module, scm_define},
    object::{ScmResult, MAX_ARITY},
    string::make_string,
    symbol::Intern,
    value::{scm_int, EncodedValueDescriptor, Value},
    vector::make_vector,
};

extern "C" fn undefined(_: &mut CallFrame) -> ScmResult {
    ScmResult::ok(Value::encode_undefined_value())
}

extern "C" fn undefined_p(cfr: &mut CallFrame) -> ScmResult {
    let v = cfr.argument(0);
    ScmResult::ok(v.is_undefined())
}

extern "C" fn symbol_p(cfr: &mut CallFrame) -> ScmResult {
    let v = cfr.argument(0);
    ScmResult::ok(v.is_symbol())
}

extern "C" fn symbol_eq(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 2 {
        if !cfr.argument(0).is_symbol() {
            return wrong_contract::<()>(
                "symbol=?",
                "symbol?",
                0,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        if !cfr.argument(1).is_symbol() {
            return wrong_contract::<()>(
                "symbol=?",
                "symbol?",
                1,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        return ScmResult::ok(cfr.argument(0) == cfr.argument(1));
    }

    for i in 0..cfr.argument_count() {
        if !cfr.argument(i).is_symbol() {
            return wrong_contract::<()>(
                "symbol=?",
                "symbol?",
                i as _,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }
    }

    for i in 0..cfr.argument_count() - 1 {
        if cfr.argument(i) != cfr.argument(i + 1) {
            return ScmResult::ok(Value::encode_bool_value(false));
        }
    }

    ScmResult::ok(Value::encode_bool_value(true))
}

extern "C" fn symbol_string(cfr: &mut CallFrame) -> ScmResult {
    let v = cfr.argument(0);
    if !v.is_symbol() {
        return wrong_contract::<()>(
            "symbol->string",
            "symbol?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let s = v.strsym();
    ScmResult::ok(make_string(Thread::current(), s))
}

extern "C" fn string_symbol(cfr: &mut CallFrame) -> ScmResult {
    let v = cfr.argument(0);
    if !v.is_string() {
        return wrong_contract::<()>(
            "string->symbol",
            "string?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let s = v.strsym();
    ScmResult::ok(s.intern())
}

extern "C" fn char_p(cfr: &mut CallFrame) -> ScmResult {
    let v = cfr.argument(0);
    ScmResult::ok(v.is_char())
}

extern "C" fn char_integer(cfr: &mut CallFrame) -> ScmResult {
    let v = cfr.argument(0);
    if !v.is_char() {
        return wrong_contract::<()>(
            "char->integer",
            "char?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    ScmResult::ok(scm_int(v.get_char() as u32 as i64))
}

extern "C" fn integer_char(cfr: &mut CallFrame) -> ScmResult {
    let v = cfr.argument(0);
    if !scm_is_exact_non_negative_integer(v) {
        return wrong_contract::<()>(
            "integer->char",
            "exact-non-negative-integer?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let c = if v.is_int32() {
        v.get_int32() as u32
    } else if v.is_bignum() {
        v.bignum().u32().unwrap()
    } else {
        unreachable!()
    };

    if c > 0x10ffff {
        return raise_exn!(
            (),
            Fail,
            &[],
            "integer->char: code point out of range: {}",
            c
        )
        .into();
    }

    if c >= 0xd800 && c <= 0xdfff {
        return raise_exn!(
            (),
            Fail,
            &[],
            "integer->char: code point in excluded range: {}",
            c
        )
        .into();
    }

    let c = char::from_u32(c).unwrap();
    ScmResult::ok(Value::encode_char(c))
}

extern "C" fn char_eq(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 2 {
        if !cfr.argument(0).is_char() {
            return wrong_contract::<()>(
                "char=?",
                "char?",
                0,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        if !cfr.argument(1).is_char() {
            return wrong_contract::<()>(
                "char=?",
                "char?",
                1,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        return ScmResult::ok(cfr.argument(0) == cfr.argument(1));
    }

    for i in 0..cfr.argument_count() {
        if !cfr.argument(i).is_char() {
            return wrong_contract::<()>(
                "char=?",
                "char?",
                i as _,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }
    }

    for i in 0..cfr.argument_count() - 1 {
        if cfr.argument(i) != cfr.argument(i + 1) {
            return ScmResult::ok(Value::encode_bool_value(false));
        }
    }

    ScmResult::ok(Value::encode_bool_value(true))
}

extern "C" fn char_lt(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 2 {
        if !cfr.argument(0).is_char() {
            return wrong_contract::<()>(
                "char<?",
                "char?",
                0,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        if !cfr.argument(1).is_char() {
            return wrong_contract::<()>(
                "char<?",
                "char?",
                1,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        return ScmResult::ok(cfr.argument(0).get_char() < cfr.argument(1).get_char());
    }

    for i in 0..cfr.argument_count() {
        if !cfr.argument(i).is_char() {
            return wrong_contract::<()>(
                "char<?",
                "char?",
                i as _,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }
    }

    for i in 0..cfr.argument_count() - 1 {
        if cfr.argument(i).get_char() >= cfr.argument(i + 1).get_char() {
            return ScmResult::ok(Value::encode_bool_value(false));
        }
    }

    ScmResult::ok(Value::encode_bool_value(true))
}

extern "C" fn char_le(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 2 {
        if !cfr.argument(0).is_char() {
            return wrong_contract::<()>(
                "char<=?",
                "char?",
                0,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        if !cfr.argument(1).is_char() {
            return wrong_contract::<()>(
                "char<=?",
                "char?",
                1,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        return ScmResult::ok(cfr.argument(0).get_char() <= cfr.argument(1).get_char());
    }

    for i in 0..cfr.argument_count() {
        if !cfr.argument(i).is_char() {
            return wrong_contract::<()>(
                "char<=?",
                "char?",
                i as _,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }
    }

    for i in 0..cfr.argument_count() - 1 {
        if cfr.argument(i).get_char() > cfr.argument(i + 1).get_char() {
            return ScmResult::ok(Value::encode_bool_value(false));
        }
    }

    ScmResult::ok(Value::encode_bool_value(true))
}

extern "C" fn char_gt(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 2 {
        if !cfr.argument(0).is_char() {
            return wrong_contract::<()>(
                "char>?",
                "char?",
                0,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        if !cfr.argument(1).is_char() {
            return wrong_contract::<()>(
                "char>?",
                "char?",
                1,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        return ScmResult::ok(cfr.argument(0).get_char() > cfr.argument(1).get_char());
    }

    for i in 0..cfr.argument_count() {
        if !cfr.argument(i).is_char() {
            return wrong_contract::<()>(
                "char>?",
                "char?",
                i as _,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }
    }

    for i in 0..cfr.argument_count() - 1 {
        if cfr.argument(i).get_char() <= cfr.argument(i + 1).get_char() {
            return ScmResult::ok(Value::encode_bool_value(false));
        }
    }

    ScmResult::ok(Value::encode_bool_value(true))
}

extern "C" fn char_ge(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 2 {
        if !cfr.argument(0).is_char() {
            return wrong_contract::<()>(
                "char>=?",
                "char?",
                0,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        if !cfr.argument(1).is_char() {
            return wrong_contract::<()>(
                "char>=?",
                "char?",
                1,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        return ScmResult::ok(cfr.argument(0).get_char() >= cfr.argument(1).get_char());
    }

    for i in 0..cfr.argument_count() {
        if !cfr.argument(i).is_char() {
            return wrong_contract::<()>(
                "char>=?",
                "char?",
                i as _,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }
    }

    for i in 0..cfr.argument_count() - 1 {
        if cfr.argument(i).get_char() < cfr.argument(i + 1).get_char() {
            return ScmResult::ok(Value::encode_bool_value(false));
        }
    }

    ScmResult::ok(Value::encode_bool_value(true))
}

extern "C" fn string_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(cfr.argument(0).is_string())
}

extern "C" fn subr_make_string(cfr: &mut CallFrame) -> ScmResult {
    let n = if !(scm_is_exact_non_negative_integer(cfr.argument(0)) && cfr.argument(0).is_int32()) {
        return wrong_contract::<()>(
            "make-string",
            "(and/c fixnum? exact-non-negative-integer?)",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    } else {
        cfr.argument(0).get_int32()
    };

    if cfr.argument_count() == 1 {
        let mut x = String::with_capacity(n as usize);
        for _ in 0..n {
            x.push(0x20u8 as char);
        }

        return ScmResult::ok(make_string(Thread::current(), &x));
    } else {
        let ch = if !cfr.argument(1).is_char() {
            return wrong_contract::<()>(
                "make-string",
                "char?",
                1,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        } else {
            cfr.argument(1).get_char()
        };

        let mut x = String::with_capacity(n as usize);
        for _ in 0..n {
            x.push(ch);
        }

        return ScmResult::ok(make_string(Thread::current(), &x));
    }
}

extern "C" fn string(cfr: &mut CallFrame) -> ScmResult {
    let mut x = String::new();
    for i in 0..cfr.argument_count() {
        if !cfr.argument(i).is_char() {
            return wrong_contract::<()>(
                "string",
                "char?",
                i as _,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        x.push(cfr.argument(i).get_char());
    }

    ScmResult::ok(make_string(Thread::current(), &x))
}

extern "C" fn list_to_string(cfr: &mut CallFrame) -> ScmResult {
    if !scm_is_list(cfr.argument(0)) {
        return wrong_contract::<()>(
            "list->string",
            "list?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let mut x = String::new();

    scm_dolist!(p, cfr.argument(0), {
        if !p.is_char() {
            return wrong_contract::<()>(
                "list->string",
                "(listof char?)",
                0,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        x.push(p.get_char());
    });

    ScmResult::ok(make_string(Thread::current(), &x))
}

extern "C" fn string_length(cfr: &mut CallFrame) -> ScmResult {
    if !cfr.argument(0).is_string() {
        return wrong_contract::<()>(
            "string-length",
            "string?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    ScmResult::ok(cfr.argument(0).string().len() as i32)
}

extern "C" fn string_ref(cfr: &mut CallFrame) -> ScmResult {
    if !cfr.argument(0).is_string() {
        return wrong_contract::<()>(
            "string-ref",
            "string?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    if !(scm_is_exact_non_negative_integer(cfr.argument(1)) && cfr.argument(1).is_int32()) {
        return wrong_contract::<()>(
            "string-ref",
            "(and/c fixnum? exact-non-negative-integer?)",
            1,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let s = cfr.argument(0).string();
    let i = cfr.argument(1).get_int32();
    let count = s.chars().count() as i32;
    if i >= count as i32 {
        return wrong_contract::<()>(
            "string-ref",
            format!("(and/c fixnum? (< fixnum? {}))", count).as_str(),
            1,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    ScmResult::ok(Value::encode_char(s.chars().nth(i as usize).unwrap()))
}

extern "C" fn string_set(cfr: &mut CallFrame) -> ScmResult {
    if !cfr.argument(0).is_string() {
        return wrong_contract::<()>(
            "string-set!",
            "string?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    if !(scm_is_exact_non_negative_integer(cfr.argument(1)) && cfr.argument(1).is_int32()) {
        return wrong_contract::<()>(
            "string-set!",
            "(and/c fixnum? exact-non-negative-integer?)",
            1,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    if !cfr.argument(2).is_char() {
        return wrong_contract::<()>(
            "string-set!",
            "char?",
            2,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let s = cfr.argument(0).string();
    let i = cfr.argument(1).get_int32();
    let count = s.chars().count() as i32;

    if i >= count as i32 {
        return wrong_contract::<()>(
            "string-set!",
            format!("(and/c fixnum? (< fixnum? {}))", count).as_str(),
            1,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let mut s = s.to_owned();
    s.remove(i as usize);
    s.insert(i as usize, cfr.argument(2).get_char());

    ScmResult::ok(s)
}

extern "C" fn string_eq(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 2 {
        if !cfr.argument(0).is_string() {
            return wrong_contract::<()>(
                "string=?",
                "string?",
                0,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        if !cfr.argument(1).is_string() {
            return wrong_contract::<()>(
                "string=?",
                "string?",
                1,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        return ScmResult::ok(cfr.argument(0).strsym() == cfr.argument(1).strsym());
    }

    for i in 0..cfr.argument_count() {
        if !cfr.argument(i).is_string() {
            return wrong_contract::<()>(
                "string=?",
                "string?",
                i as _,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }
    }

    for i in 0..cfr.argument_count() - 1 {
        if cfr.argument(i).strsym() != cfr.argument(i + 1).strsym() {
            return ScmResult::ok(false);
        }
    }

    ScmResult::ok(true)
}

extern "C" fn string_lt(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 2 {
        if !cfr.argument(0).is_string() {
            return wrong_contract::<()>(
                "string<?",
                "string?",
                0,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        if !cfr.argument(1).is_string() {
            return wrong_contract::<()>(
                "string<?",
                "string?",
                1,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        return ScmResult::ok(cfr.argument(0).strsym() < cfr.argument(1).strsym());
    }

    for i in 0..cfr.argument_count() {
        if !cfr.argument(i).is_string() {
            return wrong_contract::<()>(
                "string<?",
                "string?",
                i as _,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }
    }

    for i in 0..cfr.argument_count() - 1 {
        if cfr.argument(i).strsym() >= cfr.argument(i + 1).strsym() {
            return ScmResult::ok(false);
        }
    }

    ScmResult::ok(true)
}

extern "C" fn string_le(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 2 {
        if !cfr.argument(0).is_string() {
            return wrong_contract::<()>(
                "string<=?",
                "string?",
                0,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        if !cfr.argument(1).is_string() {
            return wrong_contract::<()>(
                "string<=?",
                "string?",
                1,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        return ScmResult::ok(cfr.argument(0).strsym() <= cfr.argument(1).strsym());
    }

    for i in 0..cfr.argument_count() {
        if !cfr.argument(i).is_string() {
            return wrong_contract::<()>(
                "string<=?",
                "string?",
                i as _,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }
    }

    for i in 0..cfr.argument_count() - 1 {
        if cfr.argument(i).strsym() > cfr.argument(i + 1).strsym() {
            return ScmResult::ok(false);
        }
    }

    ScmResult::ok(true)
}

extern "C" fn string_gt(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 2 {
        if !cfr.argument(0).is_string() {
            return wrong_contract::<()>(
                "string<=?",
                "string?",
                0,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        if !cfr.argument(1).is_string() {
            return wrong_contract::<()>(
                "string<=?",
                "string?",
                1,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        return ScmResult::ok(cfr.argument(0).strsym() > cfr.argument(1).strsym());
    }

    for i in 0..cfr.argument_count() {
        if !cfr.argument(i).is_string() {
            return wrong_contract::<()>(
                "string<=?",
                "string?",
                i as _,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }
    }

    for i in 0..cfr.argument_count() - 1 {
        if cfr.argument(i).strsym() <= cfr.argument(i + 1).strsym() {
            return ScmResult::ok(false);
        }
    }

    ScmResult::ok(true)
}

extern "C" fn string_ge(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 2 {
        if !cfr.argument(0).is_string() {
            return wrong_contract::<()>(
                "string<=?",
                "string?",
                0,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        if !cfr.argument(1).is_string() {
            return wrong_contract::<()>(
                "string<=?",
                "string?",
                1,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        return ScmResult::ok(cfr.argument(0).strsym() >= cfr.argument(1).strsym());
    }

    for i in 0..cfr.argument_count() {
        if !cfr.argument(i).is_string() {
            return wrong_contract::<()>(
                "string<=?",
                "string?",
                i as _,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }
    }

    for i in 0..cfr.argument_count() - 1 {
        if cfr.argument(i).strsym() < cfr.argument(i + 1).strsym() {
            return ScmResult::ok(false);
        }
    }

    ScmResult::ok(true)
}

extern "C" fn substring(cfr: &mut CallFrame) -> ScmResult {
    let string = cfr.argument(0);
    let start = cfr.argument(1);
    let end = cfr.argument(2);

    if !string.is_string() {
        return wrong_contract::<()>(
            "substring",
            "string?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    if !(scm_is_exact_non_negative_integer(start) && start.is_int32()) {
        return wrong_contract::<()>(
            "substring",
            "(and/c exact-non-negative-integer? fixnum?)",
            1,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    if !(scm_is_exact_non_negative_integer(end) && end.is_int32()) {
        return wrong_contract::<()>(
            "substring",
            "(and/c exact-non-negative-integer? fixnum?)",
            2,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let start = start.get_int32() as usize;
    let end = end.get_int32() as usize;

    let count = string.strsym().chars().count();

    if start > count {
        return out_of_range::<()>(
            "substring",
            Some("string"),
            "start",
            Value::encode_int32(start as _),
            Value::encode_int32(count as i32),
            0,
            count as _,
        )
        .into();
    }

    if end > count {
        return out_of_range::<()>(
            "substring",
            Some("string"),
            "end",
            Value::encode_int32(end as _),
            Value::encode_int32(count as i32),
            0,
            count as _,
        )
        .into();
    }

    if start > end {
        return out_of_range::<()>(
            "substring",
            Some("string"),
            "start",
            Value::encode_int32(start as _),
            Value::encode_int32(end as i32),
            0,
            end as _,
        )
        .into();
    }

    let string = string
        .strsym()
        .chars()
        .skip(start)
        .take(end - start)
        .collect::<String>();

    ScmResult::ok(make_string(Thread::current(), &string))
}

extern "C" fn string_copy(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 1 {
        let string = cfr.argument(0);

        if !string.is_string() {
            return wrong_contract::<()>(
                "string-copy",
                "string?",
                0,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        return ScmResult::ok(make_string(Thread::current(), string.strsym()));
    }

    let string = cfr.argument(0);
    if !string.is_string() {
        return wrong_contract::<()>(
            "string-copy",
            "string?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let count = string.strsym().chars().count();

    let start = cfr.argument(1);

    let end = if cfr.argument_count() > 2 {
        cfr.argument(2)
    } else {
        Value::encode_int32(count as _)
    };

    if !(scm_is_exact_non_negative_integer(start) && start.is_int32()) {
        return wrong_contract::<()>(
            "string-copy",
            "(and/c exact-non-negative-integer? fixnum?)",
            1,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    if !(scm_is_exact_non_negative_integer(end) && end.is_int32()) {
        return wrong_contract::<()>(
            "string-copy",
            "(and/c exact-non-negative-integer? fixnum?)",
            2,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let start = start.get_int32() as usize;
    let end = end.get_int32() as usize;

    if start > count {
        return out_of_range::<()>(
            "string-copy",
            Some("string"),
            "start",
            Value::encode_int32(start as _),
            Value::encode_int32(count as i32),
            0,
            count as _,
        )
        .into();
    }

    if end > count {
        return out_of_range::<()>(
            "string-copy",
            Some("string"),
            "end",
            Value::encode_int32(end as _),
            Value::encode_int32(count as i32),
            0,
            count as _,
        )
        .into();
    }

    if start > end {
        return out_of_range::<()>(
            "string-copy",
            Some("string"),
            "start",
            Value::encode_int32(start as _),
            Value::encode_int32(end as i32),
            0,
            end as _,
        )
        .into();
    }

    let string = string
        .strsym()
        .chars()
        .skip(start)
        .take(end - start)
        .collect::<String>();

    ScmResult::ok(make_string(Thread::current(), &string))
}

extern "C" fn string_list(cfr: &mut CallFrame) -> ScmResult {
    let string = cfr.argument(0);

    if !string.is_string() {
        return wrong_contract::<()>(
            "string-list",
            "string?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let mut list = Value::encode_null_value();

    for c in string.strsym().chars().rev() {
        list = scm_cons(Thread::current(), Value::encode_char(c), list);
    }

    ScmResult::ok(list)
}

extern "C" fn string_append(cfr: &mut CallFrame) -> ScmResult {
    let mut string = String::new();

    for i in 0..cfr.argument_count() {
        let arg = cfr.argument(i);

        if !arg.is_string() {
            return wrong_contract::<()>(
                "string-append",
                "string?",
                i as i32,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        string.push_str(arg.strsym());
    }

    ScmResult::ok(make_string(Thread::current(), &string))
}

extern "C" fn string_contains(cfr: &mut CallFrame) -> ScmResult {
    let string = cfr.argument(0);

    if !string.is_string() {
        return wrong_contract::<()>(
            "string-contains?",
            "string?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    for i in 1..cfr.argument_count() {
        let arg = cfr.argument(i);

        if !arg.is_string() {
            return wrong_contract::<()>(
                "string-contains?",
                "string?",
                i as i32,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        if string.strsym().contains(arg.strsym()) {
            return ScmResult::ok(Value::encode_bool_value(true));
        }
    }

    ScmResult::ok(Value::encode_bool_value(false))
}

extern "C" fn string_split(cfr: &mut CallFrame) -> ScmResult {
    let string = cfr.argument(0);

    if !string.is_string() {
        return wrong_contract::<()>(
            "string-split",
            "string?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let sep = if cfr.argument_count() > 1 {
        cfr.argument(1)
    } else {
        Value::encode_char(' ')
    };

    if !sep.is_char() && !sep.is_string() {
        return wrong_contract::<()>(
            "string-split",
            "(or/c string? char?)",
            1,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let mut list = Value::encode_null_value();
    let split = if sep.is_char() {
        sep.get_char().to_string()
    } else {
        sep.strsym().to_string()
    };
    for s in string.strsym().split(&split) {
        list = scm_cons(
            Thread::current(),
            make_string(Thread::current(), s).into(),
            list,
        );
    }

    ScmResult::ok(list)
}

extern "C" fn vector_p(cfr: &mut CallFrame) -> ScmResult {
    let arg = cfr.argument(0);

    ScmResult::ok(Value::encode_bool_value(arg.is_vector()))
}

extern "C" fn make_vector_proc(cfr: &mut CallFrame) -> ScmResult {
    let count = cfr.argument(0);

    if !scm_is_exact_non_negative_integer(count) {
        return wrong_contract::<()>(
            "make-vector",
            "exact-nonnegative-integer?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let count = count.get_int32() as usize;

    let fill = if cfr.argument_count() > 1 {
        cfr.argument(1)
    } else {
        Value::encode_undefined_value()
    };
    let t = Thread::current();
    let mut vector = make_vector(t, count);

    for i in 0..count {
        vector[i] = fill;
    }
    ScmResult::ok(vector)
}

extern "C" fn vector_proc(cfr: &mut CallFrame) -> ScmResult {
    let mut vector = make_vector(Thread::current(), cfr.argument_count() as usize);

    for i in 0..cfr.argument_count() {
        vector[i] = cfr.argument(i);
    }

    ScmResult::ok(vector)
}

extern "C" fn vector_ref(cfr: &mut CallFrame) -> ScmResult {
    let vector = cfr.argument(0);
    let index = cfr.argument(1);

    if !vector.is_vector() {
        return wrong_contract::<()>(
            "vector-ref",
            "vector?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    if !(scm_is_exact_non_negative_integer(index) && index.is_int32()) {
        return wrong_contract::<()>(
            "vector-ref",
            "exact-nonnegative-integer?",
            1,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let index = index.get_int32() as usize;

    let vector = vector.vector();

    if index >= vector.len() {
        return out_of_range::<()>(
            "vector-ref",
            Some("vector"),
            "",
            Value::encode_int32(index as _),
            Value::encode_int32(vector.len() as i32),
            0,
            vector.len() as _,
        )
        .into();
    }

    ScmResult::ok(vector[index])
}

extern "C" fn vector_cas(cfr: &mut CallFrame) -> ScmResult {
    let vector = cfr.argument(0);
    let index = cfr.argument(1);
    let old: Value = cfr.argument(2);
    let new = cfr.argument(3);

    if !vector.is_vector() {
        return wrong_contract::<()>(
            "vector-cas!",
            "vector?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    if !(scm_is_exact_non_negative_integer(index) && index.is_int32()) {
        return wrong_contract::<()>(
            "vector-cas!",
            "exact-nonnegative-integer?",
            1,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let index = index.get_int32() as usize;

    let vector = vector.vector();

    if index >= vector.len() {
        return out_of_range::<()>(
            "vector-cas!",
            Some("vector"),
            "",
            Value::encode_int32(index as _),
            Value::encode_int32(vector.len() as i32),
            0,
            vector.len() as _,
        )
        .into();
    }
    unsafe {
        let ptr = vector.data.as_ptr().add(index).cast::<AtomicI64>();

        match (*ptr).compare_exchange_weak(
            old.get_raw(),
            new.get_raw(),
            Ordering::SeqCst,
            Ordering::Relaxed,
        ) {
            Ok(old) => ScmResult::ok(Value(EncodedValueDescriptor { as_int64: old })),
            Err(old) => ScmResult::ok(Value(EncodedValueDescriptor { as_int64: old })),
        }
    }
}

extern "C" fn vector_set(cfr: &mut CallFrame) -> ScmResult {
    let vector = cfr.argument(0);
    let index = cfr.argument(1);
    let value = cfr.argument(2);

    if !vector.is_vector() {
        return wrong_contract::<()>(
            "vector-set!",
            "vector?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    if !(scm_is_exact_non_negative_integer(index) && index.is_int32()) {
        return wrong_contract::<()>(
            "vector-set!",
            "exact-nonnegative-integer?",
            1,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let index = index.get_int32() as usize;

    let mut vector = vector.vector();

    if index >= vector.len() {
        return out_of_range::<()>(
            "vector-set!",
            Some("vector"),
            "",
            Value::encode_int32(index as _),
            Value::encode_int32(vector.len() as i32),
            0,
            vector.len() as _,
        )
        .into();
    }
    Thread::current().write_barrier(vector);
    vector[index] = value;

    ScmResult::ok(Value::encode_undefined_value())
}

extern "C" fn vector_list(cfr: &mut CallFrame) -> ScmResult {
    let vector = cfr.argument(0);

    if !vector.is_vector() {
        return wrong_contract::<()>(
            "vector->list",
            "vector?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let vector = vector.vector();

    let mut list = Value::encode_null_value();
    let t = Thread::current();
    for i in (0..vector.len()).rev() {
        list = scm_cons(t, vector[i], list);
    }

    ScmResult::ok(list)
}

extern "C" fn list_vector(cfr: &mut CallFrame) -> ScmResult {
    let list = cfr.argument(0);

    if !scm_is_list(list) {
        return wrong_contract::<()>(
            "list->vector",
            "list?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let mut vector = make_vector(Thread::current(), scm_length(list).unwrap() as usize);

    let mut i = 0;
    let mut list = list;
    while !list.is_null() {
        vector[i] = list.car();
        list = list.cdr();
        i += 1;
    }

    ScmResult::ok(vector)
}

extern "C" fn vector_fill(cfr: &mut CallFrame) -> ScmResult {
    let vector = cfr.argument(0);
    let fill = cfr.argument(1);

    if !vector.is_vector() {
        return wrong_contract::<()>(
            "vector-fill!",
            "vector?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let mut vector = vector.vector();
    Thread::current().write_barrier(vector);
    for i in 0..vector.len() {
        vector[i] = fill;
    }

    ScmResult::ok(Value::encode_undefined_value())
}

extern "C" fn vector_length(cfr: &mut CallFrame) -> ScmResult {
    let vector = cfr.argument(0);

    if !vector.is_vector() {
        return wrong_contract::<()>(
            "vector-length",
            "vector?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    ScmResult::ok(Value::encode_int32(vector.vector().len() as i32))
}

fn init_vector() {
    let module = scm_capy_module().module();

    let subr = scm_make_subr_inliner("vector?", vector_p, 1, 1, |iforms, _| {
        if iforms.len() == 1 {
            Some(make_iform(IForm::Asm(Asm {
                op: Opcode::IsVector,
                args: ArrayList::from_slice(Thread::current(), iforms),
                operands: None,
                exits: false,
                pushes: true,
                ic: false,
            })))
        } else {
            None
        }
    });
    scm_define(module, "vector?".intern(), subr.into()).unwrap();

    let subr = scm_make_subr_inliner("make-vector", make_vector_proc, 1, 2, |iforms, _| {
        if iforms.len() == 1 || iforms.len() == 2 {
            let n = iforms[0];
            if let IForm::Const(n) = &*n {
                if n.is_int32() && n.get_int32() >= 0 && n.get_int32() <= u16::MAX as i32 {
                    let fill = if iforms.len() == 2 {
                        iforms[1]
                    } else {
                        make_iform(IForm::Const(Value::encode_undefined_value()))
                    };

                    return Some(make_iform(IForm::Asm(Asm {
                        op: Opcode::MakeVector,
                        args: ArrayList::from_slice(Thread::current(), &[fill]),
                        operands: Some(ArrayList::from_slice(
                            Thread::current(),
                            &[AsmOperand::I16(n.get_int32() as u16 as i16)],
                        )),
                        exits: false,
                        pushes: true,
                        ic: false,
                    })));
                }
            }
        }

        None
    });
    scm_define(module, "make-vector".intern(), subr.into()).unwrap();

    let subr = scm_make_subr_inliner("vector", vector_proc, 0, MAX_ARITY, |iforms, _| {
        let n = iforms.len();
        if n > u16::MAX as usize {
            return None;
        }

        let operands =
            ArrayList::from_slice(Thread::current(), &[AsmOperand::I16(n as u16 as i16)]);

        Some(make_iform(IForm::Asm(Asm {
            op: Opcode::Vector,
            args: ArrayList::from_slice(Thread::current(), iforms),
            operands: Some(operands),
            exits: false,
            pushes: true,
            ic: false,
        })))
    });
    scm_define(module, "vector".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("vector-ref", vector_ref, 2, 2);
    scm_define(module, "vector-ref".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("vector-cas!", vector_cas, 4, 4);
    scm_define(module, "vector-cas!".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("vector-set!", vector_set, 3, 3);
    scm_define(module, "vector-set!".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("vector->list", vector_list, 1, 1);
    scm_define(module, "vector->list".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("list->vector", list_vector, 1, 1);
    scm_define(module, "list->vector".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("vector-fill!", vector_fill, 2, 2);
    scm_define(module, "vector-fill!".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("vector-length", vector_length, 1, 1);
    scm_define(module, "vector-length".intern(), subr.into()).unwrap();
}
pub const HASH_SEED: u32 = 5731;

extern "C" fn symbol_hash(cfr: &mut CallFrame) -> ScmResult {
    let x = cfr.argument(0);

    if !x.is_symbol() {
        return wrong_contract::<()>(
            "symbol-hash",
            "symbol?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let ptr = x.symbol().as_ptr() as usize;
    let bytes = ptr.to_le_bytes();

    let hash = match murmur3::murmur3_32(&mut Cursor::new(bytes), HASH_SEED).map_err(|err| {
        let exn = raise_exn!((), Fail, &[], "symbol-hash failed: {}", err);
        exn.unwrap_err()
    }) {
        Ok(hash) => hash,
        Err(err) => return Err::<(), _>(err).into(),
    };
    ScmResult::ok(Value::encode_int32(hash as i32))
}

extern "C" fn string_hash(cfr: &mut CallFrame) -> ScmResult {
    let x = cfr.argument(0);

    if !x.is_string() {
        return wrong_contract::<()>(
            "string-hash",
            "string?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let s = x.string();

    let mut hash = murmur3_32(&mut Cursor::new(5022200u32.to_le_bytes()), HASH_SEED).unwrap();

    hash = murmur3_32(&mut Cursor::new(s.len().to_le_bytes()), hash).unwrap();

    hash = murmur3_32(&mut Cursor::new(s.as_bytes()), hash).unwrap();

    ScmResult::ok(Value::encode_int32(hash as i32))
}

pub fn object_hash(x: Value) -> Value {
    if x.is_symbol() {
        let ptr = x.symbol().as_ptr() as usize;
        let bytes = ptr.to_le_bytes();

        let hash = murmur3::murmur3_32(&mut Cursor::new(bytes), HASH_SEED).unwrap();
        Value::encode_int32(hash as i32)
    } else if x.is_number() {
        if x.is_int32() || x.is_double() {
            let bits = x.get_raw();

            let hash = murmur3_32(&mut Cursor::new(bits.to_le_bytes()), HASH_SEED).unwrap();

            Value::encode_int32(hash as i32)
        } else if x.is_bignum() {
            let mut hash = HASH_SEED;

            hash = murmur3_32(&mut Cursor::new(7900000u32.to_le_bytes()), hash).unwrap();

            let bignum = x.bignum();

            hash = murmur3_32(&mut Cursor::new(&[bignum.is_negative() as u8]), hash).unwrap();

            for word in bignum.uwords().iter() {
                hash = murmur3_32(&mut Cursor::new(word.to_le_bytes()), hash).unwrap();
            }

            hash = murmur3_32(&mut Cursor::new(bignum.uwords().len().to_le_bytes()), hash).unwrap();

            Value::encode_int32(hash as i32)
        } else {
            todo!()
        }
    } else if x.is_string() {
        let s = x.string();

        let mut hash = murmur3_32(&mut Cursor::new(5022200u32.to_le_bytes()), HASH_SEED).unwrap();

        hash = murmur3_32(&mut Cursor::new(s.len().to_le_bytes()), hash).unwrap();

        hash = murmur3_32(&mut Cursor::new(s.as_bytes()), hash).unwrap();

        Value::encode_int32(hash as i32)
    } else {
        let raw = x.get_raw();

        let hash = murmur3_32(&mut Cursor::new(raw.to_le_bytes()), HASH_SEED).unwrap();

        Value::encode_int32(hash as i32)
    }
}

extern "C" fn object_hash_proc(cfr: &mut CallFrame) -> ScmResult {
    let x = cfr.argument(0);

    ScmResult::ok(object_hash(x))
}

extern "C" fn equal_hash(cfr: &mut CallFrame) -> ScmResult {
    fn do_hash(val: Value, hash: u32, budget: isize) -> u32 {
        if budget > 0 {
            if val.is_string() {
                let s = val.string();

                let mut hash =
                    murmur3_32(&mut Cursor::new(5022200u32.to_le_bytes()), hash).unwrap();

                hash = murmur3_32(&mut Cursor::new(s.len().to_le_bytes()), hash).unwrap();

                hash = murmur3_32(&mut Cursor::new(s.as_bytes()), hash).unwrap();
                hash
            } else if val.is_pair() {
                let budget = budget / 2;

                let mut hash = do_hash(val.car(), hash, budget);
                hash = do_hash(val.cdr(), hash, budget);

                hash
            } else if val.is_vector() {
                let mut hash =
                    murmur3_32(&mut Cursor::new(4003330u32.to_le_bytes()), hash).unwrap();

                let vec = val.vector();

                hash = murmur3_32(&mut Cursor::new(vec.len().to_le_bytes()), hash).unwrap();

                for &val in vec.iter() {
                    hash = do_hash(val, hash, budget);
                }

                hash
            } else if val.is_bytevector() {
                let mut hash =
                    murmur3_32(&mut Cursor::new(4488623u32.to_le_bytes()), hash).unwrap();

                let vec = val.bytevector();

                hash = murmur3_32(&mut Cursor::new(vec.len().to_le_bytes()), hash).unwrap();

                hash = murmur3_32(&mut Cursor::new(&*vec), hash).unwrap();

                hash
            } else {
                object_hash(val).get_int32() as u32
            }
        } else {
            0
        }
    }

    let x = cfr.argument(0);

    ScmResult::ok(Value::encode_int32(do_hash(x, HASH_SEED, 100) as _))
}

extern "C" fn eq_hash(cfr: &mut CallFrame) -> ScmResult {
    let x = cfr.argument(0);

    let hash = murmur3_32(&mut Cursor::new(x.get_raw().to_le_bytes()), HASH_SEED).unwrap();

    ScmResult::ok(Value::encode_int32(hash as i32))
}

extern "C" fn type_of(cfr: &mut CallFrame) -> ScmResult {
    let val = cfr.argument(0);

    ScmResult::ok(val.get_type() as i32)
}

extern "C" fn host_architecture(_cfr: &mut CallFrame) -> ScmResult {
    let arch = target_lexicon::HOST.architecture;

    ScmResult::ok(make_string(scm_vm().mutator(), arch.to_string()))
}

extern "C" fn host_vendor(_cfr: &mut CallFrame) -> ScmResult {
    let vendor = target_lexicon::HOST.vendor;

    ScmResult::ok(make_string(scm_vm().mutator(), vendor.to_string()))
}

extern "C" fn host_os(_cfr: &mut CallFrame) -> ScmResult {
    let os = target_lexicon::HOST.operating_system;

    ScmResult::ok(make_string(scm_vm().mutator(), os.to_string()))
}

extern "C" fn host_env(_cfr: &mut CallFrame) -> ScmResult {
    let env = target_lexicon::HOST.environment;

    ScmResult::ok(make_string(scm_vm().mutator(), env.to_string()))
}

extern "C" fn host_binary_format(_cfr: &mut CallFrame) -> ScmResult {
    let format = target_lexicon::HOST.binary_format;

    ScmResult::ok(make_string(scm_vm().mutator(), format.to_string()))
}

extern "C" fn getenv(cfr: &mut CallFrame) -> ScmResult {
    let name = cfr.argument(0);
    if !name.is_string() {
        return wrong_contract::<()>("getenv", "string?", 0, 1, &[name]).into();
    }
  
    let value = std::env::var(name.strsym());
    match value {
        Ok(x) => ScmResult::ok(make_string(scm_vm().mutator(), x)),
        Err(_) => ScmResult::ok(false),
    }
}

pub(crate) fn init_base() {
    let module = scm_capy_module().module();
    init_vector();

    let subr = scm_make_subr("getenv", getenv, 1, 1);
    scm_define(module, "getenv".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("%host-architecture", host_architecture, 0, 0);
    scm_define(module, "%host-architecture".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("%host-vendor", host_vendor, 0, 0);
    scm_define(module, "%host-vendor".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("%host-os", host_os, 0, 0);
    scm_define(module, "%host-os".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("%host-env", host_env, 0, 0);
    scm_define(module, "%host-env".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("%host-binary-format", host_binary_format, 0, 0);
    scm_define(module, "%host-binary-format".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("eq-hash", eq_hash, 1, 1);
    scm_define(module, "eq-hash".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("object-hash", object_hash_proc, 1, 1);
    scm_define(module, "object-hash".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("equal-hash", equal_hash, 1, 1);
    scm_define(module, "equal-hash".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("symbol-hash", symbol_hash, 1, 1);
    scm_define(module, "symbol-hash".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("string-hash", string_hash, 1, 1);
    scm_define(module, "string-hash".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("undefined", undefined, 0, 0);
    scm_define(module, "undefined".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("undefined?", undefined_p, 1, 1);
    scm_define(module, "undefined?".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("symbol?", symbol_p, 1, 1);
    scm_define(module, "symbol?".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("symbol=?", symbol_eq, 2, MAX_ARITY);
    scm_define(module, "symbol=?".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("symbol->string", symbol_string, 1, 1);
    scm_define(module, "symbol->string".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("string->symbol", string_symbol, 1, 1);
    scm_define(module, "string->symbol".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("char?", char_p, 1, 1);
    scm_define(module, "char?".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("char->integer", char_integer, 1, 1);
    scm_define(module, "char->integer".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("integer->char", integer_char, 1, 1);
    scm_define(module, "integer->char".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("char=?", char_eq, 2, MAX_ARITY);
    scm_define(module, "char=?".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("char<?", char_lt, 2, MAX_ARITY);
    scm_define(module, "char<?".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("char<=?", char_le, 2, MAX_ARITY);
    scm_define(module, "char<=?".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("char>?", char_gt, 2, MAX_ARITY);
    scm_define(module, "char>?".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("char>=?", char_ge, 2, MAX_ARITY);
    scm_define(module, "char>=?".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("string?", string_p, 1, 1);
    scm_define(module, "string?".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("make-string", subr_make_string, 1, 2);
    scm_define(module, "make-string".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("string", string, 0, MAX_ARITY);
    scm_define(module, "string".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("list->string", list_to_string, 1, 1);
    scm_define(module, "list->string".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("string-length", string_length, 1, 1);
    scm_define(module, "string-length".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("string-ref", string_ref, 2, 2);
    scm_define(module, "string-ref".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("string-set!", string_set, 3, 3);
    scm_define(module, "string-set!".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("string=?", string_eq, 2, MAX_ARITY);
    scm_define(module, "string=?".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("string<?", string_lt, 2, MAX_ARITY);
    scm_define(module, "string<?".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("string<=?", string_le, 2, MAX_ARITY);
    scm_define(module, "string<=?".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("string>?", string_gt, 2, MAX_ARITY);
    scm_define(module, "string>?".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("string>=?", string_ge, 2, MAX_ARITY);
    scm_define(module, "string>=?".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("substring", substring, 3, 3);
    scm_define(module, "substring".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("string-copy", string_copy, 1, 3);
    scm_define(module, "string-copy".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("string->list", string_list, 1, 1);
    scm_define(module, "string->list".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("string-append", string_append, 0, MAX_ARITY);
    scm_define(module, "string-append".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("string-contains?", string_contains, 1, MAX_ARITY);
    scm_define(module, "string-contains?".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("string-split", string_split, 2, 2);
    scm_define(module, "string-split".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("type-of", type_of, 1, 1);
    scm_define(module, "type-of".intern(), subr.into()).unwrap();
}
