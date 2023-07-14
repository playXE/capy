use std::{cmp::Ordering, hint::unreachable_unchecked, intrinsics::unlikely};

use rsgc::{prelude::Handle, system::arraylist::ArrayList, thread::Thread};

use crate::{
    compile::{make_iform, Asm, AsmOperand, IForm},
    op::Opcode,
    runtime::{
        fun::{scm_make_subr, scm_make_subr_inliner},
        module::scm_define,
        object::MAX_ARITY,
        symbol::Intern,
    },
    vm::{callframe::CallFrame, scm_vm},
};

use super::{
    arith::{
        arith_add, arith_div, arith_mul, arith_negate, arith_quotient, arith_remainder, arith_sub,
        scm_is_integer, scm_is_number, scm_is_number_equal, scm_is_real, scm_n_compare,
    },
    error::wrong_contract,
    module::scm_capy_module,
    object::ScmResult,
    value::Value,
};

pub(crate) fn init_arith() {
    let capy = scm_capy_module().module();

    macro_rules! defproc {
        ($($name: ident, $lit: literal, $mina: expr, $maxa: expr, $inliner: expr)*) => {
            $(
                if let Some(inliner) = $inliner {
                    let subr = scm_make_subr_inliner($lit, $name, $mina, $maxa, inliner);
                    scm_define(capy, $lit.intern().into(), subr).unwrap();
                } else {
                    let subr = scm_make_subr($lit, $name, $mina, $maxa);
                    scm_define(capy, $lit.intern().into(), subr).unwrap();
                }
            )*
        };
    }

    fn inline_add(iforms: &[Handle<IForm>], _: Value) -> Option<Handle<IForm>> {
        if iforms.len() != 2 {
            return None;
        }

        if let IForm::Const(x) = &*iforms[1] {
            if x.is_int32() {
                let operands =
                    ArrayList::from_slice(Thread::current(), &[AsmOperand::I32(x.get_int32())]);
                return Some(make_iform(IForm::Asm(Asm {
                    op: Opcode::Addi,
                    args: ArrayList::from_slice(Thread::current(), &[iforms[0]]),
                    operands: Some(operands),
                    exits: false,
                    pushes: true,
                    ic: false,
                })));
            }
        }

        Some(make_iform(IForm::Asm(Asm {
            op: Opcode::Add,
            args: ArrayList::from_slice(Thread::current(), iforms),
            operands: None,
            exits: false,
            pushes: true,
            ic: false,
        })))
    }

    fn inline_sub(iforms: &[Handle<IForm>], _: Value) -> Option<Handle<IForm>> {
        if iforms.len() != 2 {
            return None;
        }

        Some(make_iform(IForm::Asm(Asm {
            op: Opcode::Sub,
            args: ArrayList::from_slice(Thread::current(), iforms),
            operands: None,
            exits: false,
            pushes: true,
            ic: false,
        })))
    }

    fn inline_mul(iforms: &[Handle<IForm>], _: Value) -> Option<Handle<IForm>> {
        if iforms.len() != 2 {
            return None;
        }

        Some(make_iform(IForm::Asm(Asm {
            op: Opcode::Mul,
            args: ArrayList::from_slice(Thread::current(), iforms),
            operands: None,
            exits: false,
            pushes: true,
            ic: false,
        })))
    }

    fn inline_quotient(iforms: &[Handle<IForm>], _: Value) -> Option<Handle<IForm>> {
        if iforms.len() != 2 {
            return None;
        }

        Some(make_iform(IForm::Asm(Asm {
            op: Opcode::Quotient,
            args: ArrayList::from_slice(Thread::current(), iforms),
            operands: None,
            exits: false,
            pushes: true,
            ic: false,
        })))
    }

    fn inline_equal(iforms: &[Handle<IForm>], _: Value) -> Option<Handle<IForm>> {
        if iforms.len() != 2 {
            return None;
        }

        Some(make_iform(IForm::Asm(Asm {
            op: Opcode::NumberEqual,
            args: ArrayList::from_slice(Thread::current(), iforms),
            operands: None,
            exits: false,
            pushes: true,
            ic: false,
        })))
    }

    fn inline_less(iforms: &[Handle<IForm>], _: Value) -> Option<Handle<IForm>> {
        if iforms.len() != 2 {
            return None;
        }

        Some(make_iform(IForm::Asm(Asm {
            op: Opcode::Less,
            args: ArrayList::from_slice(Thread::current(), iforms),
            operands: None,
            exits: false,
            pushes: true,
            ic: false,
        })))
    }

    fn inline_less_equal(iforms: &[Handle<IForm>], _: Value) -> Option<Handle<IForm>> {
        if iforms.len() != 2 {
            return None;
        }

        Some(make_iform(IForm::Asm(Asm {
            op: Opcode::LessEqual,
            args: ArrayList::from_slice(Thread::current(), iforms),
            operands: None,
            exits: false,
            pushes: true,
            ic: false,
        })))
    }

    fn inline_greater(iforms: &[Handle<IForm>], _: Value) -> Option<Handle<IForm>> {
        if iforms.len() != 2 {
            return None;
        }

        Some(make_iform(IForm::Asm(Asm {
            op: Opcode::Greater,
            args: ArrayList::from_slice(Thread::current(), iforms),
            operands: None,
            exits: false,
            pushes: true,
            ic: false,
        })))
    }

    fn inline_greater_equal(iforms: &[Handle<IForm>], _: Value) -> Option<Handle<IForm>> {
        if iforms.len() != 2 {
            return None;
        }

        Some(make_iform(IForm::Asm(Asm {
            op: Opcode::GreaterEqual,
            args: ArrayList::from_slice(Thread::current(), iforms),
            operands: None,
            exits: false,
            pushes: true,
            ic: false,
        })))
    }

    defproc! {
        plus, "+", 0, MAX_ARITY, Some(inline_add)
        minus, "-", 1, MAX_ARITY, Some(inline_sub)
        mul, "*", 0, MAX_ARITY, Some(inline_mul)
        div, "/", 1, MAX_ARITY, None
        quotient, "quotient", 2, 2, Some(inline_quotient)
        remainder, "remainder", 2, 2, None
        less, "<", 2, MAX_ARITY, Some(inline_less)
        less_equal, "<=", 2, MAX_ARITY, Some(inline_less_equal)
        greater, ">", 2, MAX_ARITY, Some(inline_greater)
        greater_equal, ">=", 2, MAX_ARITY, Some(inline_greater_equal)
        equal, "=", 2, MAX_ARITY, Some(inline_equal)
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
        if unlikely(!scm_is_number(cfr.argument(0))) {
            return wrong_contract::<()>("+", "number?", 0, 1, cfr.arguments()).into();
        }

        return ScmResult::ok(cfr.argument(0));
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

        return ScmResult::ok(unsafe {
            arith_negate(scm_vm(), cfr.argument(0)).unwrap_unchecked()
        });
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
        return wrong_contract::<()>(
            "<",
            "real?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    if !scm_is_real(b) {
        return wrong_contract::<()>(
            "<",
            "real?",
            1,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
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
            return wrong_contract::<()>(
                "<",
                "real?",
                i as _,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
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
        return wrong_contract::<()>(
            "<=",
            "real?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    if !scm_is_real(b) {
        return wrong_contract::<()>(
            "<=",
            "real?",
            1,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
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
            return wrong_contract::<()>(
                "<=",
                "real?",
                i as _,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
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
        return wrong_contract::<()>(
            "<",
            "real?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    if !scm_is_real(b) {
        return wrong_contract::<()>(
            "<",
            "real?",
            1,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
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
            return wrong_contract::<()>(
                "<",
                "real?",
                i as _,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
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
        return wrong_contract::<()>(
            "<=",
            "real?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    if !scm_is_real(b) {
        return wrong_contract::<()>(
            "<=",
            "real?",
            1,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
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
            return wrong_contract::<()>(
                "<=",
                "real?",
                i as _,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
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
        return wrong_contract::<()>(
            "=",
            "number?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    if !scm_is_number(b) {
        return wrong_contract::<()>(
            "=",
            "number?",
            1,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
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
