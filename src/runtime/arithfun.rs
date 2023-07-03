use std::{hint::unreachable_unchecked, intrinsics::unlikely, cmp::Ordering};

use crate::{vm::{callframe::CallFrame, scm_vm}, runtime::{fun::scm_make_subr, module::scm_define, object::MAX_ARITY, symbol::Intern}};

use super::{
    arith::{arith_add, arith_negate, scm_is_number, arith_sub, arith_mul, arith_div, arith_quotient, arith_remainder, scm_is_integer, scm_n_compare, scm_is_real, scm_is_number_equal},
    error::wrong_contract,
    object::ScmResult,
    value::Value, module::scm_capy_module,
};

pub(crate) fn init_arith() {
    let capy = scm_capy_module().module();

    macro_rules! defproc {
        ($($name: ident, $lit: literal, $mina: expr, $maxa: expr)*) => {
            $(

                let subr = scm_make_subr($lit, $name, $mina, $maxa);
                scm_define(capy, $lit.intern().into(), subr).unwrap();
            )*
        };
    }
    defproc! {
        plus, "+", 0, MAX_ARITY
        minus, "-", 1, MAX_ARITY
        mul, "*", 0, MAX_ARITY
        div, "/", 1, MAX_ARITY
        quotient, "quotient", 2, 2
        remainder, "remainder", 2, 2
        less, "<", 2, MAX_ARITY
        less_equal, "<=", 2, MAX_ARITY
        greater, ">", 2, MAX_ARITY
        greater_equal, ">=", 2, MAX_ARITY
        equal, "=", 2, MAX_ARITY
    }
}

extern "C" fn plus(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 2 {
        let a = cfr.argument(0);
        let b = cfr.argument(1);

        if a.is_int32() && b.is_int32() {
            if let Some(x) = a.get_int32().checked_add(b.get_int32()) {
                return ScmResult::ok(Value::encode_int32(x));
            }
        } else if a.is_double() && b.is_double() {
            return ScmResult::ok(Value::encode_untrusted_f64_value(
                a.get_double() + b.get_double(),
            ));
        }
    }

    if cfr.argument_count() == 1 {
        if unlikely(!scm_is_number(cfr.argument(1))) {
            return wrong_contract::<()>("+", "number?", 0, 1, cfr.arguments()).into();
        }
    }

    if cfr.argument_count() == 0 {
        return ScmResult::ok(Value::encode_int32(0));
    }

    for i in 0..cfr.argument_count() {
        if unlikely(!scm_is_number(cfr.argument(i))) {
            return wrong_contract::<()>(
                "+",
                "number?",
                i as _,
                cfr.argument_count() as _,
                cfr.arguments(),
            )
            .into();
        }
    }

    let mut acc = cfr.arguments()[0];
    let vm = scm_vm();
    for i in 1..cfr.argument_count() {
        acc = match arith_add(vm, acc, cfr.arguments()[i]) {
            Some(x) => x,
            None => unsafe { unreachable_unchecked() },
        }
    }

    ScmResult::ok(acc)
}

extern "C" fn minus(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 2 {
        let a = cfr.argument(0);
        let b = cfr.argument(1);

        if a.is_int32() && b.is_int32() {
            if let Some(x) = a.get_int32().checked_sub(b.get_int32()) {
                return ScmResult::ok(Value::encode_int32(x));
            }
        } else if a.is_double() && b.is_double() {
            return ScmResult::ok(Value::encode_untrusted_f64_value(
                a.get_double() - b.get_double(),
            ));
        }
    }

    if cfr.argument_count() == 1 {
        if unlikely(!scm_is_number(cfr.argument(0))) {
            return wrong_contract::<()>("-", "number?", 0, 1, cfr.arguments()).into();
        }

        return ScmResult::ok(unsafe { arith_negate(scm_vm(), cfr.argument(0)).unwrap_unchecked() });
    }

    for i in 0..cfr.argument_count() {
        if unlikely(!scm_is_number(cfr.argument(i))) {
            return wrong_contract::<()>(
                "-",
                "number?",
                i as _,
                cfr.argument_count() as _,
                cfr.arguments(),
            )
            .into();
        }
    }

    let mut acc = cfr.arguments()[0];
    let vm = scm_vm();
    for i in 1..cfr.argument_count() {
        acc = match arith_sub(vm, acc, cfr.arguments()[i]) {
            Some(x) => x,
            None => unsafe { unreachable_unchecked() },
        }
    }

    ScmResult::ok(acc)
}

extern "C" fn mul(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 2 {
        let a = cfr.argument(0);
        let b = cfr.argument(1);

        if a.is_int32() && b.is_int32() {
            if let Some(x) = a.get_int32().checked_mul(b.get_int32()) {
                return ScmResult::ok(Value::encode_int32(x));
            }
        } else if a.is_double() && b.is_double() {
            return ScmResult::ok(Value::encode_untrusted_f64_value(
                a.get_double() * b.get_double(),
            ));
        }
    }

    if cfr.argument_count() == 1 {
        if unlikely(!scm_is_number(cfr.argument(1))) {
            return wrong_contract::<()>("*", "number?", 0, 1, cfr.arguments()).into();
        }
    }

    for i in 0..cfr.argument_count() {
        if unlikely(!scm_is_number(cfr.argument(i))) {
            return wrong_contract::<()>(
                "*",
                "number?",
                i as _,
                cfr.argument_count() as _,
                cfr.arguments(),
            )
            .into();
        }
    }

    let mut acc = cfr.arguments()[0];
    let vm = scm_vm();
    for i in 1..cfr.argument_count() {
        acc = match arith_mul(vm, acc, cfr.arguments()[i]) {
            Some(x) => x,
            None => unsafe { unreachable_unchecked() },
        }
    }

    ScmResult::ok(acc)
}

extern "C" fn div(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 2 {
        let a = cfr.argument(0);
        let b = cfr.argument(1);

        if a.is_int32() && b.is_int32() {
            if let Some(x) = a.get_int32().checked_div(b.get_int32()) {
                return ScmResult::ok(Value::encode_int32(x));
            }
        } else if a.is_double() && b.is_double() {
            return ScmResult::ok(Value::encode_untrusted_f64_value(
                a.get_double() / b.get_double(),
            ));
        }
    }

    if cfr.argument_count() == 1 {
        if unlikely(!scm_is_number(cfr.argument(1))) {
            return wrong_contract::<()>("*", "number?", 0, 1, cfr.arguments()).into();
        }
    }

    for i in 0..cfr.argument_count() {
        if unlikely(!scm_is_number(cfr.argument(i))) {
            return wrong_contract::<()>(
                "*",
                "number?",
                i as _,
                cfr.argument_count() as _,
                cfr.arguments(),
            )
            .into();
        }
    }

    let mut acc = cfr.arguments()[0];
    let vm = scm_vm();
    for i in 1..cfr.argument_count() {
        acc = match arith_div(vm, acc, cfr.arguments()[i]) {
            Some(x) => x,
            None => unsafe { unreachable_unchecked() },
        }
    }

    ScmResult::ok(acc)
}

extern "C" fn remainder(cfr: &mut CallFrame) -> ScmResult {

    let a = cfr.argument(0);
    let b = cfr.argument(1);

    if !scm_is_integer(a) {
        return wrong_contract::<()>("remainder", "integer?", 0, 2, cfr.arguments()).into();
    }

    if !scm_is_integer(b) {
        return wrong_contract::<()>("remainder", "integer?", 1, 2, cfr.arguments()).into();
    }

    if a.is_int32() && b.is_int32() {
        if let Some(x) = a.get_int32().checked_rem(b.get_int32()) {
            return ScmResult::ok(Value::encode_int32(x));
        }
    }

    if a.is_double() && b.is_double() {
        return ScmResult::ok(Value::encode_untrusted_f64_value(
            a.get_double() % b.get_double(),
        ));
    }

    ScmResult::ok(arith_remainder(scm_vm(), a, b).unwrap())
}

extern "C" fn quotient(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() != 2 {
        return wrong_contract::<()>(
            "quotient",
            "(number? number?)",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    let _a = cfr.argument(0);
    let _b = cfr.argument(1);

    let a = cfr.argument(0);
    let b = cfr.argument(1);

    if !scm_is_integer(a) {
        return wrong_contract::<()>("remainder", "integer?", 0, 2, cfr.arguments()).into();
    }

    if !scm_is_integer(b) {
        return wrong_contract::<()>("remainder", "integer?", 1, 2, cfr.arguments()).into();
    }

    if a.is_int32() && b.is_int32() {
        if let Some(x) = a.get_int32().checked_div(b.get_int32()) {
            return ScmResult::ok(Value::encode_int32(x));
        }
    }

    if a.is_double() && b.is_double() {
        return ScmResult::ok(Value::encode_untrusted_f64_value(
            a.get_double() / b.get_double(),
        ));
    }

    ScmResult::ok(arith_quotient(scm_vm(), a, b).unwrap())
}

extern "C" fn less(cfr: &mut CallFrame) -> ScmResult {
    let a = cfr.argument(0);
    let b = cfr.argument(1);

    if !scm_is_real(a) {
        return wrong_contract::<()>("<", "real?", 0, cfr.argument_count() as i32, cfr.arguments()).into();
    }

    if !scm_is_real(b) {
        return wrong_contract::<()>("<", "real?", 1, cfr.argument_count() as i32, cfr.arguments()).into();
    }

    if cfr.argument_count() == 2 {
        if a.is_int32() && b.is_int32() {
            return ScmResult::ok(Value::encode_bool_value(a.get_int32() < b.get_int32()));
        } else if a.is_double() && b.is_double() {
            return ScmResult::ok(Value::encode_bool_value(a.get_double() < b.get_double()));
        } else {
            match scm_n_compare(a, b).unwrap() {
                Ordering::Less => return ScmResult::ok(Value::encode_bool_value(true)),
                Ordering::Equal => return ScmResult::ok(Value::encode_bool_value(false)),
                Ordering::Greater => return ScmResult::ok(Value::encode_bool_value(false)),
            }
        }
    }

    for i in 0..cfr.argument_count() {
        if !scm_is_real(cfr.argument(i)) {
            return wrong_contract::<()>("<", "real?", i as _, cfr.argument_count() as i32, cfr.arguments()).into();
        }
    }

    let mut acc = true;

    for i in 0..cfr.argument_count() - 1 {
        let a = cfr.argument(i);
        let b = cfr.argument(i + 1);

        if a.is_int32() && b.is_int32() {
            acc = acc && a.get_int32() < b.get_int32();
        } else if a.is_double() && b.is_double() {
            acc = acc && a.get_double() < b.get_double();
        } else {
            match scm_n_compare(a, b).unwrap() {
                Ordering::Less => acc = acc && true,
                Ordering::Equal => acc = acc && false,
                Ordering::Greater => acc = acc && false,
            }
        }
    }

    ScmResult::ok(Value::encode_bool_value(acc))
}

extern "C" fn less_equal(cfr: &mut CallFrame) -> ScmResult {
    let a = cfr.argument(0);
    let b = cfr.argument(1);

    if !scm_is_real(a) {
        return wrong_contract::<()>("<=", "real?", 0, cfr.argument_count() as i32, cfr.arguments()).into();
    }

    if !scm_is_real(b) {
        return wrong_contract::<()>("<=", "real?", 1, cfr.argument_count() as i32, cfr.arguments()).into();
    }

    if cfr.argument_count() == 2 {
        if a.is_int32() && b.is_int32() {
            return ScmResult::ok(Value::encode_bool_value(a.get_int32() <= b.get_int32()));
        } else if a.is_double() && b.is_double() {
            return ScmResult::ok(Value::encode_bool_value(a.get_double() <= b.get_double()));
        } else {
            match scm_n_compare(a, b).unwrap() {
                Ordering::Less => return ScmResult::ok(Value::encode_bool_value(true)),
                Ordering::Equal => return ScmResult::ok(Value::encode_bool_value(true)),
                Ordering::Greater => return ScmResult::ok(Value::encode_bool_value(false)),
            }
        }
    }

    for i in 0..cfr.argument_count() {
        if !scm_is_real(cfr.argument(i)) {
            return wrong_contract::<()>("<=", "real?", i as _, cfr.argument_count() as i32, cfr.arguments()).into();
        }
    }

    let mut acc = true;

    for i in 0..cfr.argument_count() - 1 {
        let a = cfr.argument(i);
        let b = cfr.argument(i + 1);

        if a.is_int32() && b.is_int32() {
            acc = acc && a.get_int32() <= b.get_int32();
        } else if a.is_double() && b.is_double() {
            acc = acc && a.get_double() <= b.get_double();
        } else {
            match scm_n_compare(a, b).unwrap() {
                Ordering::Less => acc = acc && true,
                Ordering::Equal => acc = acc && true,
                Ordering::Greater => acc = acc && false,
            }
        }
    }

    ScmResult::ok(Value::encode_bool_value(acc))
}

extern "C" fn greater(cfr: &mut CallFrame) -> ScmResult {
    let a = cfr.argument(0);
    let b = cfr.argument(1);

    if !scm_is_real(a) {
        return wrong_contract::<()>("<", "real?", 0, cfr.argument_count() as i32, cfr.arguments()).into();
    }

    if !scm_is_real(b) {
        return wrong_contract::<()>("<", "real?", 1, cfr.argument_count() as i32, cfr.arguments()).into();
    }

    if cfr.argument_count() == 2 {
        if a.is_int32() && b.is_int32() {
            return ScmResult::ok(Value::encode_bool_value(a.get_int32() > b.get_int32()));
        } else if a.is_double() && b.is_double() {
            return ScmResult::ok(Value::encode_bool_value(a.get_double() > b.get_double()));
        } else {
            match scm_n_compare(a, b).unwrap() {
                Ordering::Less => return ScmResult::ok(Value::encode_bool_value(false)),
                Ordering::Equal => return ScmResult::ok(Value::encode_bool_value(false)),
                Ordering::Greater => return ScmResult::ok(Value::encode_bool_value(true)),
            }
        }
    }

    for i in 0..cfr.argument_count() {
        if !scm_is_real(cfr.argument(i)) {
            return wrong_contract::<()>("<", "real?", i as _, cfr.argument_count() as i32, cfr.arguments()).into();
        }
    }

    let mut acc = true;

    for i in 0..cfr.argument_count() - 1 {
        let a = cfr.argument(i);
        let b = cfr.argument(i + 1);

        if a.is_int32() && b.is_int32() {
            acc = acc && a.get_int32() > b.get_int32();
        } else if a.is_double() && b.is_double() {
            acc = acc && a.get_double() > b.get_double();
        } else {
            match scm_n_compare(a, b).unwrap() {
                Ordering::Less => acc = acc && false,
                Ordering::Equal => acc = acc && false,
                Ordering::Greater => acc = acc && true,
            }
        }
    }

    ScmResult::ok(Value::encode_bool_value(acc))
}

extern "C" fn greater_equal(cfr: &mut CallFrame) -> ScmResult {
    let a = cfr.argument(0);
    let b = cfr.argument(1);

    if !scm_is_real(a) {
        return wrong_contract::<()>("<=", "real?", 0, cfr.argument_count() as i32, cfr.arguments()).into();
    }

    if !scm_is_real(b) {
        return wrong_contract::<()>("<=", "real?", 1, cfr.argument_count() as i32, cfr.arguments()).into();
    }

    if cfr.argument_count() == 2 {
        if a.is_int32() && b.is_int32() {
            return ScmResult::ok(Value::encode_bool_value(a.get_int32() >= b.get_int32()));
        } else if a.is_double() && b.is_double() {
            return ScmResult::ok(Value::encode_bool_value(a.get_double() >= b.get_double()));
        } else {
            match scm_n_compare(a, b).unwrap() {
                Ordering::Less => return ScmResult::ok(Value::encode_bool_value(false)),
                Ordering::Equal => return ScmResult::ok(Value::encode_bool_value(true)),
                Ordering::Greater => return ScmResult::ok(Value::encode_bool_value(true)),
            }
        }
    }

    for i in 0..cfr.argument_count() {
        if !scm_is_real(cfr.argument(i)) {
            return wrong_contract::<()>("<=", "real?", i as _, cfr.argument_count() as i32, cfr.arguments()).into();
        }
    }

    let mut acc = true;

    for i in 0..cfr.argument_count() - 1 {
        let a = cfr.argument(i);
        let b = cfr.argument(i + 1);

        if a.is_int32() && b.is_int32() {
            acc = acc && a.get_int32() >= b.get_int32();
        } else if a.is_double() && b.is_double() {
            acc = acc && a.get_double() >= b.get_double();
        } else {
            match scm_n_compare(a, b).unwrap() {
                Ordering::Less => acc = acc && false,
                Ordering::Equal => acc = acc && true,
                Ordering::Greater => acc = acc && true,
            }
        }
    }
    ScmResult::ok(Value::encode_bool_value(acc))
}

extern "C" fn equal(cfr: &mut CallFrame) -> ScmResult {
    let a = cfr.argument(0);
    let b = cfr.argument(1);

    if !scm_is_number(a) {
        return wrong_contract::<()>("=", "number?", 0, cfr.argument_count() as i32, cfr.arguments()).into();
    }

    if !scm_is_number(b) {
        return wrong_contract::<()>("=", "number?", 1, cfr.argument_count() as i32, cfr.arguments()).into();
    }

    if cfr.argument_count() == 2 {
        return ScmResult::ok(scm_is_number_equal(a, b).unwrap());
    }

    let mut acc = true;

    for i in 0..cfr.argument_count() - 1 {
        acc = acc && scm_is_number_equal(cfr.argument(i), cfr.argument(i + 1)).unwrap();
    }

    ScmResult::ok(Value::encode_bool_value(acc))
}