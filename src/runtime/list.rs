use crate::{
    gc_protect,
    runtime::object::{scm_cdr, scm_set_cdr},
    vm::thread::Thread,
};

use super::{
    gsubr::{scm_define_subr, Subr},
    object::{scm_car, scm_set_car},
    value::Value,
};

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

pub fn scm_cons(mut car: Value, mut cdr: Value) -> Value {
    let thread = Thread::current();
    let pair = gc_protect!(thread => car, cdr => thread.make_cons::<false>(Value::encode_bool_value(false), Value::encode_bool_value(false)));
    scm_set_car(pair, thread, car);
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

pub fn scm_last_pair(val: Value) -> Value {
    let mut slow = val;
    let mut l = slow;
    loop {
        if !scm_cdr(l).is_pair() {
            return l;
        }

        l = scm_cdr(l);

        if !scm_cdr(l).is_pair() {
            return l;
        }

        l = scm_cdr(l);
        slow = scm_cdr(slow);

        if l == slow {
            panic!("circular list detected");
        }
    }
}

#[macro_export]
macro_rules! scm_append1 {
    ($start: expr, $last: expr, $obj: expr) => {
        if $start.is_null() {
            *$start = $crate::runtime::list::scm_cons(
                $obj,
                $crate::runtime::value::Value::encode_null_value(),
            );
            *$last = *$start;
        } else {
            $crate::runtime::object::scm_set_cdr(
                *$last,
                $crate::vm::thread::Thread::current(),
                $crate::runtime::list::scm_cons(
                    $obj,
                    $crate::runtime::value::Value::encode_null_value(),
                ),
            );
            *$last = scm_cdr(*$last);
        }
    };
}

#[macro_export]
macro_rules! scm_append {
    ($start: expr, $last: expr, $obj: expr) => {
        let list = $obj;

        if $start.is_null() {
            *$start = list;
            if !list.is_null() {
                *$last = $crate::runtime::list::scm_last_pair(list);
            }
        } else {
            scm_set_cdr(*$last, $crate::vm::thread::Thread::current(), list);
            *$last = $crate::runtime::list::scm_last_pair(*last);
        }
    };
}

pub fn scm_append2(list: Value, obj: Value) -> Value {
    if list.is_null() {
        return obj;
    }
    let t = Thread::current();

    let mut cp;
    let mut start = Value::encode_null_value();
    let mut last = start;

    cp = list;

    while cp.is_pair() {
        gc_protect!(t => cp, start, last => scm_append1!(&mut start, &mut last, scm_car(cp)));
        cp = scm_cdr(cp);
    }

    scm_set_cdr(last, Thread::current(), obj);
    start
}

pub fn scm_reverse2x(list: Value, tail: Value) -> Value {
    if !list.is_pair() {
        return tail;
    }

    let mut result = tail;
    let mut first = list;
    let mut next;

    while first.is_pair() {
        next = scm_cdr(first);
        scm_set_cdr(first, Thread::current(), result);
        result = first;
        first = next;
    }

    result
}

pub fn scm_reversex(list: Value) -> Value {
    scm_reverse2x(list, Value::encode_null_value())
}

extern "C-unwind" fn list_p(_thread: &mut Thread, xs: &mut Value) -> Value {
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
