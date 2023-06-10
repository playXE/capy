use rsgc::thread::Thread;

use crate::{value::Value, object::{Pair, ObjectHeader, Type}};

pub fn scm_cons(t: &mut Thread, car: Value, cdr: Value) -> Value {
    Value::encode_object_value(t.allocate(Pair {
        object: ObjectHeader::new(Type::Pair),
        car,
        cdr 
    }))
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
    ($p: ident, $list: expr, $b: block) => {
        let mut $p = $list;
        while $p.is_xtype($crate::object::Type::Pair) {
            $b
            #[allow(unreachable_code)]
            {
                $p = $p.cdr();
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

pub fn scm_list_ref(list: Value, index: usize) -> Value {
    let mut list = list;
    for _ in 0..index {
        list = list.cdr();
    }
    list.car()
}