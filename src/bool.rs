use once_cell::sync::Lazy;

use crate::{
    value::{Type, Value},
    vm::{Trampoline, Vm},
};

pub fn eq(obj1: Value, obj2: Value) -> bool {
    obj1.raw() == obj2.raw()
}

pub fn eqv(obj1: Value, obj2: Value) -> bool {
    if let Some(cmp) = is_eqv(obj1, obj2) {
        return cmp;
    }

    false
}

pub fn equal(obj1: Value, obj2: Value) -> bool {
    if let Some(cmp) = is_fast_equal(obj1, obj2) {
        return cmp;
    }

    is_slow_equal(obj1, obj2).unwrap_or(false)
}

fn f64_eqv(a: f64, b: f64) -> bool {
    if a.is_nan() {
        if b.is_nan() {
            return true;
        }
        return false;
    }
    if a == 0.0 {
        if b == 0.0 {
            return a.signum() == b.signum();
        }
    }

    return a == b;
}

fn is_eqv(obj1: Value, obj2: Value) -> Option<bool> {
    if obj1.raw() == obj2.raw() {
        return Some(true);
    }

    let t1 = obj1.get_type();
    let t2 = obj2.get_type();

    if t1 != t2 {
        return None;
    } else {
        if t1 == Type::Double {
            return Some(f64_eqv(obj1.double(), obj2.double()));
        } else if t1 == Type::Char {
            return Some(obj1.char() == obj2.char());
        }
    }

    Some(false)
}

fn is_fast_equal(obj1: Value, obj2: Value) -> Option<bool> {
    if let Some(cmp) = is_eqv(obj1, obj2) {
        return Some(cmp);
    }

    let t1 = obj1.get_type();
    let t2 = obj2.get_type();

    if t1 != t2 {
        return None;
    }

    match t1 {
        Type::Str => {
            return Some(obj1.str() == obj2.str());
        }
        Type::Char => {
            return Some(obj1.char() == obj2.char());
        }

        Type::ByteVector => {
            return Some(obj1.byte_vector_as_slice() == obj2.byte_vector_as_slice());
        }
        _ => (),
    }

    Some(false)
}

fn is_slow_equal(obj1: Value, obj2: Value) -> Option<bool> {
    if let Some(cmp) = is_fast_equal(obj1, obj2) {
        return Some(cmp);
    }

    let t1 = obj1.get_type();
    let t2 = obj2.get_type();

    if t1 != t2 {
        return None;
    }

    match t1 {
        Type::Pair => {
            let mut p1 = obj1;
            let mut p2 = obj2;

            loop {
                if p1.nullp() {
                    return Some(p2.nullp());
                }

                if p2.nullp() {
                    return Some(false);
                }

                let car1 = p1.car();
                let car2 = p2.car();

                if let Some(cmp) = is_slow_equal(car1, car2) {
                    if !cmp {
                        return Some(false);
                    }
                } else {
                    return None;
                }

                p1 = p1.cdr();
                p2 = p2.cdr();
            }
        }
        Type::Vector => {
            let len1 = obj1.vector_len();
            let len2 = obj2.vector_len();

            if len1 != len2 {
                return Some(false);
            }

            for i in 0..len1 {
                let v1 = obj1.vector_ref(i);
                let v2 = obj2.vector_ref(i);

                if let Some(cmp) = is_slow_equal(v1, v2) {
                    if !cmp {
                        return Some(false);
                    }
                } else {
                    return None;
                }
            }

            return Some(true);
        }
        _ => (),
    }

    Some(false)
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        if b {
            Value::make_true()
        } else {
            Value::make_false()
        }
    }
}

pub static EQ_PROC: Lazy<Value> = Lazy::new(|| {
    Vm::make_procedure(
        "eq?",
        |_, _, args| {
            let obj1 = args[0];
            let obj2 = args[1];

            Trampoline::Return(eq(obj1, obj2).into())
        },
        2,
        2,
    )
});

pub static EQV_PROC: Lazy<Value> = Lazy::new(|| {
    Vm::make_procedure(
        "eqv?",
        |_, _, args| {
            let obj1 = args[0];
            let obj2 = args[1];

            Trampoline::Return(eqv(obj1, obj2).into())
        },
        2,
        2,
    )
});

pub static EQUAL_PROC: Lazy<Value> = Lazy::new(|| {
    Vm::make_procedure(
        "equal?",
        |_, _, args| {
            let obj1 = args[0];
            let obj2 = args[1];

            Trampoline::Return(equal(obj1, obj2).into())
        },
        2,
        2,
    )
});

define_proc! {
    extern "not", not(_vm, args) 1, 1 => {
        let obj = args[0];

        if obj.falsep() {
            return Trampoline::Return(Value::make_true());
        }

        return Trampoline::Return(Value::make_false());
    }
}