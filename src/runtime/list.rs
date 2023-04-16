use super::context::Context;
use super::value::ScmValue;

pub fn memq(_cx: &mut Context, k: ScmValue, ls: ScmValue) -> ScmValue {
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

pub fn append(cx: &mut Context, ls: ScmValue, values: impl Iterator<Item = ScmValue>) -> ScmValue {
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

pub fn append_one(cx: &mut Context, ls: ScmValue, value: ScmValue) -> ScmValue {
    let mut ls = ls;
    let mut last = ScmValue::encode_null_value();

    while !ls.is_null() {
        last = ls;
        ls = ls.cdr();
    }

    let pair = cx.make_pair(value, ScmValue::encode_null_value());

    if last.is_null() {
        ls = pair;
    } else {
        last.set_cdr(pair);
    }

    ls
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

pub fn reverse(cx: &mut Context, ls: ScmValue) -> ScmValue {
    let mut ls = ls;
    let mut newls = ScmValue::encode_null_value();

    while !ls.is_null() {
        let pair = cx.make_pair(ls.car(), newls);
        newls = pair;
        ls = ls.cdr();
    }

    newls
}

pub fn map(cx: &mut Context, ls: ScmValue, mut f: impl FnMut(&mut Context, ScmValue) -> ScmValue) -> ScmValue {
    let mut ls = ls;
    let mut newls = ScmValue::encode_null_value();

    while !ls.is_null() {
        let res = f(cx, ls.car());
        let pair = cx.make_pair(res, newls);
        newls = pair;
        ls = ls.cdr();
    }

    reverse(cx, newls)
}