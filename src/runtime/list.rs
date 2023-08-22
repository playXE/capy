use crate::{
    runtime::object::{scm_cdr, scm_set_cdr},
    vm::thread::Thread, gc_protect,
};

use super::{object::{scm_car, scm_set_car}, value::Value, gsubr::{scm_define_subr, Subr}};

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



pub fn scm_acons(mut caar: Value, mut cdar: Value, mut cdr: Value) -> Value {
    let thread = Thread::current();
    let mut kv = gc_protect!(thread => caar, cdar, cdr => thread.make_cons::<false>(Value::encode_bool_value(false), Value::encode_bool_value(false)));

    scm_set_car(kv, thread, caar);
    scm_set_cdr(kv, thread, cdar);

    let pair = gc_protect!(thread => kv, cdr => thread.make_cons::<false>(Value::encode_bool_value(false), Value::encode_bool_value(false)));
    scm_set_car(pair, thread, kv);
    scm_set_cdr(pair, thread, cdr);

    pair
}

pub fn scm_length(sx: Value) -> Option<usize> {
    let mut i = 0;
    let mut tortoise = sx;
    let mut hare = sx;

    loop {
        if !hare.is_pair() {
            if hare.is_null() {
                return Some(i);
            } else {
                return None;
            }
        }

        hare = scm_cdr(hare);
        i += 1;

        if !hare.is_pair() {
            if hare.is_null() {
                return Some(i);
            } else {
                return None;
            }
        }
        hare = scm_cdr(hare);
        i += 1;
        tortoise = scm_cdr(tortoise);

        if hare == tortoise {   
            return None;
        }
    }
}


extern "C-unwind" fn list_p(thread: &mut Thread, xs: &mut Value) -> Value {
   
    let mut tortoise = *xs;
    let mut hare = *xs;

    loop {
        if !hare.is_pair() {
            if hare.is_null() {
                return Value::encode_bool_value(true);
            } else {
                return Value::encode_bool_value(false);
            }
        }

        hare = scm_cdr(hare);
        tortoise = scm_cdr(tortoise);

        if !hare.is_pair() {
            if hare.is_null() {
                return Value::encode_bool_value(true);
            } else {
                return Value::encode_bool_value(false);
            }
        }
        hare = scm_cdr(hare);

        if hare == tortoise {   
            return Value::encode_bool_value(false);
        }
    }
}

pub(crate) fn init() {
    scm_define_subr("list?", 1, 0, 0, Subr::F1(list_p));
}