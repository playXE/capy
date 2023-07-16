use std::intrinsics::unlikely;

use crate::{
    compile::{make_iform, Asm, AsmOperand, IForm},
    op::Opcode,
    raise_exn,
    runtime::object::{ExtendedPair, ObjectHeader, Pair, Type, Vector},
    runtime::value::Value,
    runtime::vector::make_vector,
    vm::{callframe::CallFrame, scm_vm, VM},
};
use rsgc::{prelude::Handle, system::arraylist::ArrayList, thread::Thread};

use super::{
    error::wrong_contract,
    fun::{scm_make_subr, scm_make_subr_inliner},
    module::{scm_define, scm_scheme_module},
    object::{ScmResult, MAX_ARITY},
    symbol::Intern,
    value::scm_int,
};

pub fn scm_cons(t: &mut Thread, car: Value, cdr: Value) -> Value {
    Value::encode_object_value(t.allocate(Pair {
        object: ObjectHeader::new(Type::Pair),
        car,
        cdr,
    }))
}

pub fn scm_econs(t: &mut Thread, attr: Value, car: Value, cdr: Value) -> Value {
    let pair = t.allocate(ExtendedPair {
        object: ObjectHeader::new(Type::Pair),
        car,
        cdr,
        attr,
    });
    Value::encode_object_value(pair)
}

pub fn scm_memq(obj: Value, list: Value) -> Value {
    let mut list = list;
    while !list.is_null() {
        if obj == list.car() {
            return list;
        }
        list = list.cdr();
    }
    Value::encode_bool_value(false)
}

#[macro_export]
macro_rules! scm_for_each {
    ($p: ident, $list: expr, $b: block) => {{
        let mut $p = $list;
        while $p.is_xtype($crate::runtime::object::Type::Pair) {
            $b
            #[allow(unreachable_code)]
            {
                $p = $p.cdr();
            }
        }};
    };

    (declared $p: ident, $list: expr, $b: block) => {{
        $p = $list;
        while $p.is_xtype($crate::runtime::object::Type::Pair) {
            $b
            #[allow(unreachable_code)]
            {
                $p = $p.cdr();
            }
        }
    }}
}

/// Same as `scm_for_each!` but instead iterates on list elements.
#[macro_export]
macro_rules! scm_dolist {
    ($p: ident, $list: expr, $b: block) => {
        let mut ls = $list;

        while ls.is_xtype($crate::runtime::object::Type::Pair) {
            let $p = ls.car();
            $b
            #[allow(unreachable_code)]
            {
                ls = ls.cdr();
            }
        }
    };
}

pub fn scm_append(thread: &mut Thread, list1: Value, list2: Value) -> Value {
    if list1.is_null() {
        return list2;
    }
    let mut list1 = list1;

    let result = scm_cons(thread, list1.car(), Value::encode_null_value());
    let mut tail = result;

    list1 = list1.cdr();

    while list1.is_xtype(Type::Pair) {
        let pair = scm_cons(thread, list1.car(), Value::encode_null_value());
        tail.pair().cdr = pair;
        tail = pair;
        list1 = list1.cdr();
    }

    tail.pair().cdr = list2;
    result
}

pub fn scm_list(thread: &mut Thread, values: &[Value]) -> Value {
    let mut result = Value::encode_null_value();
    for &value in values.iter().rev() {
        result = scm_cons(thread, value, result);
    }
    result
}

pub fn scm_list_from_iter(
    thread: &mut Thread,
    iter: impl DoubleEndedIterator<Item = Value>,
) -> Value {
    let mut result = Value::encode_null_value();
    for value in iter.rev() {
        result = scm_cons(thread, value, result);
    }
    result
}

pub fn scm_list_star(thread: &mut Thread, values: &[Value]) -> Value {
    let mut result = values[values.len() - 1];
    for &value in values.iter().rev().skip(1) {
        result = scm_cons(thread, value, result);
    }
    result
}

pub fn scm_list_ref(list: Value, index: usize) -> Value {
    let mut list = list;
    for _ in 0..index {
        list = list.cdr();
    }
    list.car()
}

pub fn scm_assq(obj: Value, list: Value) -> Value {
    let mut list = list;
    while !list.is_null() {
        if !list.car().is_xtype(Type::Pair) {
            return Value::encode_bool_value(false);
        }
        if obj == list.car().car() {
            return list.car();
        }
        list = list.cdr();
    }
    Value::encode_bool_value(false)
}

pub fn scm_map(thread: &mut Thread, mut func: impl FnMut(Value) -> Value, list: Value) -> Value {
    let mut result = Value::encode_null_value();
    let mut tail = Value::encode_null_value();
    let mut list = list;
    while !list.is_null() {
        let value = func(list.car());
        let pair = scm_cons(thread, value, Value::encode_null_value());
        if tail.is_null() {
            result = pair;
        } else {
            thread.write_barrier(tail.pair());
            tail.pair().cdr = pair;
        }
        tail = pair;
        list = list.cdr();
    }
    result
}

pub fn scm_map2(
    thread: &mut Thread,
    mut func: impl FnMut(Value, Value) -> Value,
    list1: Value,
    list2: Value,
) -> Value {
    let mut result = Value::encode_null_value();
    let mut tail = Value::encode_null_value();
    let mut list1 = list1;
    let mut list2 = list2;
    while !list1.is_null() && !list2.is_null() {
        let value = func(list1.car(), list2.car());
        let pair = scm_cons(thread, value, Value::encode_null_value());
        if tail.is_null() {
            result = pair;
        } else {
            thread.write_barrier(tail.pair());
            tail.pair().cdr = pair;
        }
        tail = pair;
        list1 = list1.cdr();
        list2 = list2.cdr();
    }
    result
}

pub fn scm_try_map<E>(
    thread: &mut Thread,
    mut func: impl FnMut(Value) -> Result<Value, E>,
    list: Value,
) -> Result<Value, E> {
    let mut result = Value::encode_null_value();
    let mut tail = Value::encode_null_value();
    let mut list = list;
    while !list.is_null() {
        let value = func(list.car())?;
        let pair = scm_cons(thread, value, Value::encode_null_value());
        if tail.is_null() {
            result = pair;
        } else {
            thread.write_barrier(tail.pair());
            tail.pair().cdr = pair;
        }
        tail = pair;
        list = list.cdr();
    }
    Ok(result)
}

pub fn scm_try_map2<E>(
    thread: &mut Thread,
    mut func: impl FnMut(Value, Value) -> Result<Value, E>,
    list1: Value,
    list2: Value,
) -> Result<Value, E> {
    let mut result = Value::encode_null_value();
    let mut tail = Value::encode_null_value();
    let mut list1 = list1;
    let mut list2 = list2;
    while !list1.is_null() && !list2.is_null() {
        let value = func(list1.car(), list2.car())?;
        let pair = scm_cons(thread, value, Value::encode_null_value());
        if tail.is_null() {
            result = pair;
        } else {
            thread.write_barrier(tail.pair());
            tail.pair().cdr = pair;
        }
        tail = pair;
        list1 = list1.cdr();
        list2 = list2.cdr();
    }
    Ok(result)
}

/// Maps a function over a list of values, returning a vector of the results.
pub fn scm_try_map_vector<E>(
    thread: &mut Thread,
    mut func: impl FnMut(Value) -> Result<Value, E>,
    list: Value,
) -> Result<Handle<Vector>, E> {
    let mut result = make_vector(Thread::current(), scm_length(list).unwrap());
    let mut list = list;
    let mut i = 0;
    while !list.is_null() {
        let value = func(list.car())?;
        thread.write_barrier(result);
        result[i] = value;
        i += 1;
        list = list.cdr();
    }
    Ok(result)
}

pub fn scm_fold(mut func: impl FnMut(Value, Value) -> Value, mut acc: Value, list: Value) -> Value {
    let mut list = list;
    while !list.is_null() {
        acc = func(acc, list.car());
        list = list.cdr();
    }
    acc
}

pub fn scm_is_circular_list(list: Value) -> bool {
    /*let mut list = list;
    while !list.is_null() {
        if !list.is_xtype(Type::Pair) {
            return false;
        }
        list = list.cdr();
    }*/
    if list.is_null() {
        return false;
    }

    let mut fast = list;
    let mut slow = fast;

    while fast.is_pair() {
        fast = fast.cdr();
        if !fast.is_pair() {
            return false;
        }

        fast = fast.cdr();
        slow = slow.cdr();

        if fast == slow {
            return false;
        }
    }

    false
}

pub fn scm_is_list(list: Value) -> bool {
    if list.is_null() {
        return true;
    }

    let mut fast = list;
    let mut slow = fast;

    while fast.is_pair() {
        fast = fast.cdr();
        if !fast.is_pair() {
            return fast.is_null();
        }

        fast = fast.cdr();
        slow = slow.cdr();

        if fast == slow {
            return false;
        }
    }

    fast.is_null()
}

pub fn scm_length(list: Value) -> Option<usize> {
    let mut list = list;
    let mut len = 0;
    while !list.is_null() {
        if !list.is_xtype(Type::Pair) {
            return None;
        }
        len += 1;
        list = list.cdr();
    }
    Some(len)
}

pub fn scm_filter(mut pred: impl FnMut(Value) -> bool, list: Value) -> Value {
    if list.is_null() {
        Value::encode_null_value()
    } else if list.is_pair() {
        if pred(list.car()) {
            scm_cons(Thread::current(), list.car(), scm_filter(pred, list.cdr()))
        } else {
            scm_filter(pred, list.cdr())
        }
    } else {
        panic!("scm_filter: expected list")
    }
}

pub fn scm_reverse(thread: &mut Thread, list: Value) -> Value {
    let mut list = list;
    let mut result = Value::encode_null_value();
    while !list.is_null() {
        let pair = scm_cons(thread, list.car(), result);
        result = pair;
        list = list.cdr();
    }
    result
}

pub fn scm_has(val: Value, list: Value) -> bool {
    let mut list = list;
    while !list.is_null() {
        if val == list.car() {
            return true;
        }
        list = list.cdr();
    }
    false
}

pub fn scm_acons(t: &mut Thread, caar: Value, cdar: Value, cdr: Value) -> Value {
    let y = scm_cons(t, Value::encode_null_value(), Value::encode_null_value());
    let z = scm_cons(t, Value::encode_null_value(), Value::encode_null_value());

    y.pair().car = caar;
    y.pair().cdr = cdar;
    z.pair().car = y;
    z.pair().cdr = cdr;

    z
}

pub fn scm_last_pair(mut l: Value) -> Value {
    if !l.is_pair() {
        panic!("scm_last_pair: expected list")
    }

    let mut slow = l;
    loop {
        if !l.cdr().is_pair() {
            return l;
        }

        l = l.cdr();

        if !l.cdr().is_pair() {
            return l;
        }

        l = l.cdr();

        slow = slow.cdr();

        if l == slow {
            panic!("scm_last_pair: circular list")
        }
    }
}

#[macro_export]
macro_rules! scm_append1 {
    ($t: expr, $start: expr, $last: expr, $obj: expr) => {
        if $start.is_null() {
            *$start = $crate::runtime::list::scm_cons(
                $t,
                $obj,
                $crate::runtime::value::Value::encode_null_value(),
            );
            *$last = *$start;
        } else {
            $t.write_barrier($last.pair());
            $last.pair().cdr = $crate::runtime::list::scm_cons(
                $t,
                $obj,
                $crate::runtime::value::Value::encode_null_value(),
            );
            *$last = $last.pair().cdr;
        }
    };
}

pub fn scm_append2(t: &mut Thread, list: Value, obj: Value) -> Value {
    if list.is_null() {
        return obj;
    }

    let mut cp;
    let mut start = Value::encode_null_value();
    let mut last = Value::encode_null_value();

    scm_for_each!(declared cp, list, {
        scm_append1!(t, &mut start, &mut last, cp.car());
    });

    if !cp.is_null() {
        panic!("not a list");
    }
    t.write_barrier(last.pair());
    last.pair().cdr = obj;

    start
}

pub fn vector_to_list(thread: &mut Thread, vector: Handle<Vector>) -> Value {
    let mut result = Value::encode_null_value();
    for i in (0..vector.len()).rev() {
        result = scm_cons(thread, vector[i], result);
    }
    result
}

pub fn list_to_vector(thread: &mut Thread, list: Value) -> Handle<Vector> {
    let mut result = make_vector(thread, scm_length(list).unwrap());
    let mut list = list;
    let mut i = 0;
    while !list.is_null() {
        thread.write_barrier(result);
        result[i] = list.car();
        i += 1;
        list = list.cdr();
    }
    result
}

pub fn scm_reverse2x(t: &mut Thread, list: Value, tail: Value) -> Value {
    if !list.is_pair() {
        return tail;
    }

    let mut result = tail;
    let mut first = list;
    let mut next;

    while first.is_pair() {
        next = first.cdr();
        t.write_barrier(first.pair());
        first.pair().cdr = result;
        result = first;
        first = next;
    }

    result
}

pub fn scm_reversex(t: &mut Thread, list: Value) -> Value {
    scm_reverse2x(t, list, Value::encode_null_value())
}
#[macro_export]
macro_rules! scm_append {
    ($t: expr, $start: expr, $last: expr, $obj: expr) => {{
        let list = $obj;

        if $start.is_null() {
            *$start = list;
            if !list.is_null() {
                *$last = $crate::runtime::list::scm_last_pair(list);
            }
        } else {
            $t.write_barrier($last.pair());
            $last.pair().cdr = list;
            *$last = $crate::runtime::list::scm_last_pair(*$last);
        }
    }};
}

extern "C" fn car(cfr: &mut CallFrame) -> ScmResult {
    if unlikely(!cfr.argument(0).is_pair()) {
        return wrong_contract::<()>("car", "pair?", 0, 1, cfr.arguments()).into();
    }

    ScmResult::ok(cfr.argument(0).car())
}

extern "C" fn cdr(cfr: &mut CallFrame) -> ScmResult {
    if unlikely(!cfr.argument(0).is_pair()) {
        return wrong_contract::<()>("cdr", "pair?", 0, 1, cfr.arguments()).into();
    }

    ScmResult::ok(cfr.argument(0).cdr())
}

extern "C" fn set_car(cfr: &mut CallFrame) -> ScmResult {
    if unlikely(!cfr.argument(0).is_pair()) {
        return wrong_contract::<()>("set-car!", "pair?", 0, 2, cfr.arguments()).into();
    }

    Thread::current().write_barrier(cfr.argument(0).pair());
    cfr.argument(0).pair().car = cfr.argument(1);

    ScmResult::ok(Value::encode_undefined_value())
}

extern "C" fn set_cdr(cfr: &mut CallFrame) -> ScmResult {
    if unlikely(!cfr.argument(0).is_pair()) {
        return wrong_contract::<()>("set-cdr!", "pair?", 0, 2, cfr.arguments()).into();
    }

    Thread::current().write_barrier(cfr.argument(0).pair());
    cfr.argument(0).pair().cdr = cfr.argument(1);

    ScmResult::ok(Value::encode_undefined_value())
}

extern "C" fn list(cfr: &mut CallFrame) -> ScmResult {
    let mut result = Value::encode_null_value();
    for i in (0..cfr.argument_count()).rev() {
        result = scm_cons(&mut Thread::current(), cfr.argument(i), result);
    }
    ScmResult::ok(result)
}

extern "C" fn cons(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(scm_cons(
        &mut Thread::current(),
        cfr.argument(0),
        cfr.argument(1),
    ))
}

extern "C" fn pair_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(Value::encode_bool_value(cfr.argument(0).is_pair()))
}

extern "C" fn list_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(scm_is_list(cfr.argument(0)))
}

extern "C" fn null_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(Value::encode_bool_value(cfr.argument(0).is_null()))
}

fn do_transpose(vm: &mut VM, each_len: usize, argc: usize, args: &mut [Value]) -> Value {
    let mut ans = Value::encode_null_value();
    let mut ans_tail = ans;

    for _ in 0..each_len {
        let elt = scm_cons(vm.mutator(), args[0].car(), Value::encode_null_value());
        let mut elt_tail = elt;
        args[0] = args[0].cdr();

        for n in 1..argc {
            vm.mutator().write_barrier(elt_tail.pair());
            elt_tail.set_cdr(scm_cons(
                vm.mutator(),
                args[n].car(),
                Value::encode_null_value(),
            ));
            elt_tail = elt_tail.cdr();
            args[n] = args[n].cdr();
        }

        if ans.is_null() {
            ans = scm_cons(vm.mutator(), elt, Value::encode_null_value());
            ans_tail = ans;
        } else {
            vm.mutator().write_barrier(ans_tail.pair());
            ans_tail.set_cdr(scm_cons(vm.mutator(), elt, Value::encode_null_value()));
            ans_tail = ans_tail.cdr();
        }
    }

    ans
}

extern "C" fn list_transpose(cfr: &mut CallFrame) -> ScmResult {
    let mut args = cfr.arguments_mut();

    if scm_is_list(args[0]) {
        let each_len = scm_length(args[0]).unwrap();

        for i in 1..args.len() {
            if !scm_is_list(args[i]) {
                return wrong_contract::<()>(
                    "list-transpose",
                    "list?",
                    i as _,
                    cfr.argument_count() as _,
                    cfr.arguments(),
                )
                .into();
            }

            if scm_length(args[i]).unwrap() != each_len {
                return raise_exn!(
                    (),
                    Fail,
                    &[],
                    "list-transpose: all lists must have the same length"
                )
                .into();
            }
        }

        ScmResult::ok(do_transpose(scm_vm(), each_len, args.len(), &mut args))
    } else {
        wrong_contract::<()>(
            "list-transpose",
            "list?",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into()
    }
}

extern "C" fn list_transpose_plus(cfr: &mut CallFrame) -> ScmResult {
    let args = cfr.arguments_mut();
    if scm_is_list(args[0]) {
        let each_len = scm_length(args[0]).unwrap();
        for i in 1..args.len() {
            if !scm_is_list(args[i]) {
                return ScmResult::ok(false);
            }

            if scm_length(args[i]).unwrap() != each_len {
                return ScmResult::ok(false);
            }
        }

        ScmResult::ok(do_transpose(scm_vm(), each_len, args.len(), args))
    } else {
        ScmResult::ok(false)
    }
}

extern "C" fn list_transpose_star(cfr: &mut CallFrame) -> ScmResult {
    let mut finite = false;
    let mut each_len = usize::MAX;

    for i in 0..cfr.argument_count() {
        if scm_is_list(cfr.argument(i)) {
            let n = scm_length(cfr.argument(i)).unwrap();

            if n < each_len {
                each_len = n;
            }

            finite = true;
            continue;
        }

        if scm_is_circular_list(cfr.argument(i)) {
            continue;
        }

        return wrong_contract::<()>(
            "list-transpose*",
            "list?",
            i as _,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    if finite {
        let mut args = cfr.arguments_mut();
        ScmResult::ok(do_transpose(scm_vm(), each_len, args.len(), &mut args))
    } else {
        raise_exn!((), Fail, &[], "list-transpose*: expected at least one finite list as argument\n  arguments...: {:?}" cfr.arguments()).into()
    }
}

extern "C" fn cons_star(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 1 {
        return ScmResult::ok(cfr.argument(0));
    }
    let thr = Thread::current();
    let obj = scm_cons(thr, cfr.argument(0), Value::encode_null_value());
    let mut tail = obj;
    for i in 1..cfr.argument_count() - 1 {
        let e = scm_cons(thr, cfr.argument(i), Value::encode_null_value());
        thr.write_barrier(obj.pair());
        tail.set_cdr(e);
        tail = e;
    }

    thr.write_barrier(obj.pair());
    tail.set_cdr(cfr.argument(cfr.argument_count() - 1));

    ScmResult::ok(obj)
}

extern "C" fn length_proc(cfr: &mut CallFrame) -> ScmResult {
    if let Some(len) = scm_length(cfr.argument(0)) {
        return ScmResult::ok(scm_int(len as _));
    } else {
        return wrong_contract::<()>(
            "length",
            "list?",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }
}

extern "C" fn append_proc(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 0 {
        return ScmResult::ok(Value::encode_null_value());
    }

    for i in 0..cfr.argument_count() {
        if !scm_is_list(cfr.argument(i)) {
            return wrong_contract::<()>(
                "append",
                "list?",
                i as _,
                cfr.argument_count() as _,
                cfr.arguments(),
            )
            .into();
        }
    }

    let mut res = cfr.arguments()[cfr.argument_count() - 1];

    for i in (0..cfr.argument_count() - 1).rev() {
        res = scm_append(scm_vm().mutator(), cfr.argument(i), res);
    }

    ScmResult::ok(res)
}

extern "C" fn reverse(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() != 1 {
        return wrong_contract::<()>(
            "reverse",
            "list?",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    let mut res = Value::encode_null_value();
    let mut lst = cfr.argument(0);

    if !scm_is_list(lst) {
        return wrong_contract::<()>(
            "reverse",
            "list?",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    while !lst.is_null() {
        res = scm_cons(scm_vm().mutator(), lst.car(), res);
        lst = lst.cdr();
    }

    ScmResult::ok(res)
}

pub(crate) fn init_list() {
    let module = scm_scheme_module().module();

    let subr = scm_make_subr("list-transpose", list_transpose, 1, MAX_ARITY);
    scm_define(module, "list-transpose".intern(), subr).unwrap();

    let subr = scm_make_subr("list-transpose*", list_transpose_star, 1, MAX_ARITY);
    scm_define(module, "list-transpose*".intern(), subr).unwrap();

    let subr = scm_make_subr("list-transpose+", list_transpose_plus, 1, MAX_ARITY);
    scm_define(module, "list-transpose+".intern(), subr).unwrap();

    let subr = scm_make_subr("cons*", cons_star, 1, MAX_ARITY);
    scm_define(module, "cons*".intern(), subr).unwrap();

    let subr = scm_make_subr("length", length_proc, 1, 1);
    scm_define(module, "length".intern(), subr).unwrap();

    let subr = scm_make_subr("append", append_proc, 0, MAX_ARITY);
    scm_define(module, "append".intern(), subr).unwrap();

    let subr = scm_make_subr("reverse", reverse, 1, 1);
    scm_define(module, "reverse".intern(), subr).unwrap();

    let subr = scm_make_subr_inliner("car", car, 1, 1, |forms, _| {
        if forms.len() != 1 {
            return None;
        }

        let asm = Asm {
            op: Opcode::Car,
            args: ArrayList::from_slice(Thread::current(), &[forms[0]]),
            operands: None,
            exits: false,
            pushes: true,
            ic: false,
        };

        Some(make_iform(IForm::Asm(asm)))
    });

    scm_define(module, "car".intern(), subr).unwrap();

    let subr = scm_make_subr_inliner("cdr", cdr, 1, 1, |forms, _| {
        if forms.len() != 1 {
            return None;
        }

        let asm = Asm {
            op: Opcode::Cdr,
            args: ArrayList::from_slice(Thread::current(), &[forms[0]]),
            operands: None,
            exits: false,
            pushes: true,
            ic: false,
        };

        Some(make_iform(IForm::Asm(asm)))
    });

    scm_define(module, "cdr".intern(), subr).unwrap();

    let subr = scm_make_subr_inliner("set-car!", set_car, 2, 2, |forms, _| {
        if forms.len() != 2 {
            return None;
        }

        let asm = Asm {
            op: Opcode::SetCar,
            args: ArrayList::from_slice(Thread::current(), &[forms[0], forms[1]]),
            operands: None,
            exits: false,
            pushes: false,
            ic: false,
        };

        Some(make_iform(IForm::Asm(asm)))
    });

    scm_define(module, "set-car!".intern(), subr).unwrap();

    let subr = scm_make_subr_inliner("set-cdr!", set_cdr, 2, 2, |forms, _| {
        if forms.len() != 2 {
            return None;
        }

        let asm = Asm {
            op: Opcode::SetCdr,
            args: ArrayList::from_slice(Thread::current(), &[forms[0], forms[1]]),
            operands: None,
            exits: false,
            pushes: false,
            ic: false,
        };

        Some(make_iform(IForm::Asm(asm)))
    });

    scm_define(module, "set-cdr!".intern(), subr).unwrap();

    let subr = scm_make_subr_inliner("list", list, 0, MAX_ARITY, |forms, _| {
        let args = ArrayList::from_slice(Thread::current(), &forms);

        let asm = Asm {
            op: Opcode::List,
            args,
            operands: Some(ArrayList::from_slice(
                Thread::current(),
                &[AsmOperand::I16(forms.len() as _)],
            )),
            exits: false,
            pushes: true,
            ic: false,
        };

        Some(make_iform(IForm::Asm(asm)))
    });

    scm_define(module, "list".intern(), subr).unwrap();

    let subr = scm_make_subr_inliner("cons", cons, 2, 2, |forms, _| {
        if forms.len() != 2 {
            return None;
        }

        let asm = Asm {
            op: Opcode::Cons,
            args: ArrayList::from_slice(Thread::current(), &[forms[0], forms[1]]),
            operands: None,
            exits: false,
            pushes: true,
            ic: false,
        };

        Some(make_iform(IForm::Asm(asm)))
    });

    scm_define(module, "cons".intern(), subr).unwrap();

    let subr = scm_make_subr_inliner("pair?", pair_p, 1, 1, |forms, _| {
        if forms.len() != 1 {
            return None;
        }

        let asm = Asm {
            op: Opcode::IsPair,
            args: ArrayList::from_slice(Thread::current(), &[forms[0]]),
            operands: None,
            exits: false,
            pushes: true,
            ic: false,
        };

        Some(make_iform(IForm::Asm(asm)))
    });

    scm_define(module, "pair?".intern(), subr).unwrap();

    let subr = scm_make_subr_inliner("list?", list_p, 1, 1, |forms, _| {
        if forms.len() != 1 {
            return None;
        }

        let asm = Asm {
            op: Opcode::IsList,
            args: ArrayList::from_slice(Thread::current(), &[forms[0]]),
            operands: None,
            exits: false,
            pushes: true,
            ic: false,
        };

        Some(make_iform(IForm::Asm(asm)))
    });

    scm_define(module, "list?".intern(), subr).unwrap();

    let subr = scm_make_subr_inliner("null?", null_p, 1, 1, |forms, _| {
        if forms.len() != 1 {
            return None;
        }

        let asm = Asm {
            op: Opcode::IsNull,
            args: ArrayList::from_slice(Thread::current(), &[forms[0]]),
            operands: None,
            exits: false,
            pushes: true,
            ic: false,
        };

        Some(make_iform(IForm::Asm(asm)))
    });

    scm_define(module, "null?".intern(), subr).unwrap();
}

pub fn scm_assoc_ref(
    list: Value,
    val: Value,
    mut eq: impl FnMut(Value, Value) -> bool,
    default: Option<Value>,
) -> Value {
    scm_for_each!(cp, list, {
        let entry = cp.car();

        if !entry.is_pair() {
            cp = cp.cdr();
            continue;
        }

        if eq(val, entry.car()) {
            return entry;
        }
    });

    match default {
        Some(default) => default,
        None => false.into(),
    }
}

pub fn assoc_delete_x(elt: Value, mut alist: Value, cmp: impl Fn(Value, Value) -> bool) -> Value {
    let mut cp = Value::encode_null_value();
    let mut prev = cp;

    scm_for_each!(declared cp, alist, {
        let e = cp.car();

        if e.is_pair() {
            if cmp(e.car(), elt) {
                if prev.is_null() {
                    alist = cp.cdr();
                    cp = cp.cdr();
                    continue;
                } else {
                    prev.set_cdr(cp.cdr());
                }
            }
        }
        prev = cp;
    });
    alist
}

pub fn scm_member(mut list: Value, elt: Value, cmp: impl Fn(Value, Value) -> bool) -> Value {
    while !list.is_null() {
        let e = list.car();

        if cmp(e, elt) {
            return list;
        }

        list = list.cdr();
    }

    false.into()
}
