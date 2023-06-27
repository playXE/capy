use crate::{
    runtime::object::{ExtendedPair, ObjectHeader, Pair, Type, Vector},
    runtime::value::Value,
    runtime::vector::make_vector,
};
use rsgc::{prelude::Handle, thread::Thread};

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

pub fn scm_is_list(list: Value) -> bool {
    let mut list = list;
    while !list.is_null() {
        if !list.is_xtype(Type::Pair) {
            return false;
        }
        list = list.cdr();
    }
    true
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
            *$start = $crate::runtime::list::scm_cons($t, $obj, $crate::runtime::value::Value::encode_null_value());
            *$last = *$start;
        } else {
            $t.write_barrier($last.pair());
            $last.pair().cdr =
                $crate::runtime::list::scm_cons($t, $obj, $crate::runtime::value::Value::encode_null_value());
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
