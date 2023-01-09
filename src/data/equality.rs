use crate::prelude::{Context, Type, Value};

use super::exception::SourcePosition;

#[derive(PartialEq, PartialOrd, Debug)]
pub enum NumberPair {
    Fixnum(i32, i32),
    Flonum(f64, f64),
}

impl NumberPair {
    pub fn new(ctx: &mut Context, x: Value, y: Value) -> Self {
        if x.is_int32() && y.is_int32() {
            Self::Fixnum(x.get_int32(), y.get_int32())
        } else if x.is_double() && y.is_double() {
            Self::Flonum(x.get_double(), y.get_double())
        } else if x.is_number() && y.is_number() {
            Self::Flonum(x.get_number(), y.get_number())
        } else {
            x.assert_type(ctx, SourcePosition::unknown(), &[Type::Number]);
            y.assert_type(ctx, SourcePosition::unknown(), &[Type::Number]);
            unreachable!()
        }
    }

    pub fn eq(&self) -> Value {
        match self {
            Self::Fixnum(x, y) => Value::new(x == y),
            Self::Flonum(x, y) => Value::new(x == y),
        }
    }

    pub fn cmp(&self) -> Ordering {
        match self {
            Self::Fixnum(x, y) => x.cmp(y),
            Self::Flonum(x, y) => x.partial_cmp(y).unwrap_or_else(|| Ordering::Greater),
        }
    }
}

use std::{cmp::Ordering, ops::*};

macro_rules! impl_op {
    ($($op: ident),*) => {
        $(
            impl NumberPair {
                pub fn $op(&self) -> Value {
                    match self {
                        Self::Fixnum(x, y) => paste::paste! {
                            Value::new(x.[< wrapping_ $op >](*y))
                        },
                        Self::Flonum(x, y) => Value::new(x.$op(y)),
                    }
                }
            }
        )*
    };
}

impl_op! {
    add, sub, mul, rem, div
}

pub fn eqv(_ctx: &mut Context, lhs: Value, rhs: Value) -> bool {
    if (lhs.is_undefined() && rhs.is_undefined())
        || (lhs.is_null() && rhs.is_null())
        || (lhs.is_eof() && rhs.is_eof())
        || (lhs.is_true() && rhs.is_true())
        || (lhs.is_false() && rhs.is_false())
        || (lhs.is_void() && rhs.is_void())
    {
        return true;
    }

    if lhs.is_int32() && rhs.is_int32() {
        return lhs.get_int32() == rhs.get_int32();
    }

    if lhs.is_double() && rhs.is_double() {
        return lhs.get_double() == rhs.get_double();
    }

    if lhs.is_pair() && rhs.is_pair() {
        return lhs.raw() == rhs.raw();
        //return eqv(ctx, lhs.car(), rhs.car()) && eqv(ctx, lhs.cdr(), rhs.cdr());
    }

    if lhs.is_bytes() && rhs.is_bytes() {
        let b1 = lhs.get_bytes();
        let b2 = rhs.get_bytes();

        return b1.as_ptr() == b2.as_ptr();
    }

    if lhs.is_string() && rhs.is_string() {
        return lhs.get_string() == rhs.get_string();
    }

    lhs.raw() == rhs.raw()
}



pub fn equal(ctx: &mut Context, lhs: Value, rhs: Value) -> bool {
    if (lhs.is_undefined() && rhs.is_undefined())
        || (lhs.is_null() && rhs.is_null())
        || (lhs.is_eof() && rhs.is_eof())
        || (lhs.is_true() && rhs.is_true())
        || (lhs.is_false() && rhs.is_false())
        || (lhs.is_void() && rhs.is_void())
    {
        return true;
    }

    if lhs.is_int32() && rhs.is_int32() {
        return lhs.get_int32() == rhs.get_int32();
    }

    if lhs.is_double() && rhs.is_double() {
        return lhs.get_double() == rhs.get_double();
    }

    if lhs.is_pair() && rhs.is_pair() {
       
        return eqv(ctx, lhs.car(), rhs.car()) && eqv(ctx, lhs.cdr(), rhs.cdr());
    }

    if lhs.is_vector() && rhs.is_vector() {
        let v1 = lhs.get_vector();
        let v2 = rhs.get_vector();

        if v1.elements.len() != v2.elements.len() {
            return false;
        }

        if v1.mutable != v2.mutable {
            return false;
        }

        if v1.growable != v2.growable {
            return false;
        }

        for i in 0..v1.elements.len() {
            if !equal(ctx, v1.elements[i], v2.elements[i]) {
                return false;
            }
        }

        return true;
    }

    if lhs.is_bytes() && rhs.is_bytes() {
        let b1 = lhs.get_bytes();
        let b2 = rhs.get_bytes();

        return &b1.0[0..] == &b2.0[0..];
    }

    if lhs.is_string() && rhs.is_string() {
        return lhs.get_string() == rhs.get_string();
    }

    lhs.raw() == rhs.raw()
}

pub fn eq(_ctx: &mut Context, lhs: Value, rhs: Value) -> bool {
    if (lhs.is_undefined() && rhs.is_undefined())
        || (lhs.is_null() && rhs.is_null())
        || (lhs.is_eof() && rhs.is_eof())
        || (lhs.is_true() && rhs.is_true())
        || (lhs.is_false() && rhs.is_false())
        || (lhs.is_void() && rhs.is_void())
    {
        return true;
    }

    if lhs.is_int32() && rhs.is_int32() {
        return lhs.get_int32() == rhs.get_int32();
    }

    if lhs.is_double() && rhs.is_double() {
        return lhs.get_double() == rhs.get_double();
    }

   
    lhs.raw() == rhs.raw()
}