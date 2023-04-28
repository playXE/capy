use super::vm::VM;
use super::value::ScmValue;

pub fn memq(_cx: &mut VM, k: ScmValue, ls: ScmValue) -> ScmValue {
    let mut ls = ls;
    loop {
        if ls.is_null() {
            return ScmValue::encode_bool_value(false);
        }

        let car = ls.car();
        if car == k {
            return ls;
        }

        ls = ls.cdr();
    }
}

pub fn assq(_cx: &mut VM, k: ScmValue, ls: ScmValue) -> ScmValue {
    let mut ls = ls;
    loop {
        if ls.is_null() {
            return ScmValue::encode_bool_value(false);
        }

        let car = ls.car();
        if car.car() == k {
            return car;
        }

        ls = ls.cdr();
    }
}

pub fn append(cx: &mut VM, ls: ScmValue, values: impl Iterator<Item = ScmValue>) -> ScmValue {
    let mut ls = ls;
    let mut last = ScmValue::encode_null_value();

    while !ls.is_null() {
        last = ls;
        ls = ls.cdr();
    }

    let mut ls = ls;
    for value in values {
        let pair = cx.make_pair(value, ScmValue::encode_null_value());

        if last.is_null() {
            ls = pair;
        } else {
            last.set_cdr(pair);
        }

        last = pair;
    }

    ls
}

pub fn append_one(cx: &mut VM, mut l1: ScmValue, l2: ScmValue) -> ScmValue {
    let mut first;
    let mut last = None::<ScmValue>;
    let mut v;

    first = None;

    while l1.is_pair() {
        v = cx.make_pair(l1.car(), ScmValue::encode_null_value());

        if first.is_none() {
            first = Some(v);
        } else {
            cx.mutator.write_barrier(last.unwrap().get_object());
            last.unwrap().set_cdr(v);
        }

        last = Some(v);
        l1 = l1.cdr();
    }

    if !l1.is_null() {
        panic!("append_one: not a proper list");
    }

    if last.is_none() {
        return l2;
    }

    cx.mutator.write_barrier(last.unwrap().get_object());
    last.unwrap().set_cdr(l2);
    first.unwrap()
}

pub fn proper_list_length(ls: ScmValue) -> Option<usize> {
    let mut ls = ls;
    let mut len = 0;

    while !ls.is_null() {
        len += 1;
        ls = ls.cdr();
    }

    if ls.is_null() {
        Some(len)
    } else {
        None
    }
}

pub fn reverse(cx: &mut VM, ls: ScmValue) -> ScmValue {
    let mut ls = ls;
    let mut newls = ScmValue::encode_null_value();

    while !ls.is_null() {
        let pair = cx.make_pair(ls.car(), newls);
        newls = pair;
        ls = ls.cdr();
    }

    newls
}

pub fn map(cx: &mut VM, ls: ScmValue, mut f: impl FnMut(&mut VM, ScmValue) -> ScmValue) -> ScmValue {
    let mut ls = ls;
    let mut newls = ScmValue::encode_null_value();

    while !ls.is_null() {
        let res = f(cx, ls.car());
        let pair = cx.make_pair(res, newls);
        newls = pair;
        ls = ls.cdr();
    }
    
    let rev = reverse(cx, newls);

    rev
}