use crate::{
    gc::shadow_stack::Rooted,
    gc_frame,
    runtime::object::{scm_cdr, scm_set_cdr},
    vm::thread::Thread,
};

use super::{object::scm_car, value::Value};

pub fn scm_assq(key: Value, list: Value) -> Value {
    let mut cell = list;
    if !cell.is_object() || cell.is_null() {
        return Value::encode_bool_value(false);
    }

    while cell.is_pair() {
        let pair = cell;
        if !scm_car(pair).is_object() {
            return Value::encode_bool_value(false);
        }
        let kv = scm_car(pair);

        if scm_car(kv) == key {
            return kv.into();
        }

        cell = scm_cdr(pair);
    }

    Value::encode_bool_value(false)
}

pub fn scm_map(mut proc: impl FnMut(&Value) -> Value, list: Value) -> Value {
    let thread = Thread::current();
    gc_frame!(thread.stackchain() => result = Value::encode_null_value(), tail = Value::encode_null_value(), list = list);

    while !list.is_null() {
        let value = scm_car(*list);
        gc_frame!(thread.stackchain() => value = value);
        *value = proc(&value);
        let pair = thread.make_cons::<false>(*value, Value::encode_null_value());
        if tail.is_null() {
            *result = pair.into();
        } else {
            scm_set_cdr(*tail, thread, pair.into());
        }
        *tail = pair.into();
        *list = scm_cdr(*list);
    }

    *result
}

#[macro_export]
macro_rules! scm_list {
    ($($val: expr),*) => {
        {
            let mut thread = $crate::vm::thread::Thread::current();


            $crate::gc_frame!(thread.stackchain() => result = $crate::runtime::value::Value::encode_null_value(), tail = $crate::runtime::value::Value::encode_null_value());
            $(
                let value = $val;
                $crate::gc_frame!(thread.stackchain() => value = value);
                let pair = thread.make_cons(*value, $crate::runtime::value::Value::encode_null_value());
                if tail.is_null() {
                    *result = pair.into();
                } else {
                    $crate::runtime::object::scm_set_cdr(tail.get_object(), &mut thread, pair.into());
                }
                *tail = pair.into();
            )*
            *result
        }
    };
}

#[macro_export]
macro_rules! scm_cons {
    ($car: expr, $cdr: expr) => {
        {
            let thread = $crate::vm::thread::Thread::current();
            let car = $car;
            let cdr = $cdr;
            $crate::gc_frame!(thread.stackchain() => car = car, cdr = cdr);
            thread.make_cons::<false>(*car, *cdr)
        }
    };
}

pub fn copy_alist(list: &Value) -> Value {
    scm_map(
        |&kv| scm_cons!(scm_car(kv), scm_cdr(kv)),
        *list,
    )
}

pub fn scm_append(ls2: &Rooted, ls1: &Rooted) -> Value {
    if ls1.is_null() {
        return **ls2;
    }
    let thread = Thread::current();
    let res = scm_cons!(scm_car(**ls1), Value::encode_null_value());
    let ls = **ls1;
    gc_frame!(thread.stackchain() => result = res, tail = res, ls1 = ls);

    *ls1 = scm_cdr(*ls1);

    while ls1.is_pair() {
        let pair = thread.make_cons::<false>(scm_car(*ls1), Value::encode_null_value());
        scm_set_cdr(*tail, thread, pair.into());
        *tail = pair.into();
        *ls1 = scm_cdr(*ls1);
    }

    scm_set_cdr(*tail, thread, **ls2);

    *result
}
