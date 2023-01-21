use rsgc::prelude::{Object, Allocation};

use crate::prelude::Value;

use super::bigint::BigInt;

pub struct Rational {
    numerator: Value,
    denominator: Value,
}

impl Rational {
    pub fn numerator(&self) -> Value {
        self.numerator
    }
    pub fn denominator(&self) -> Value {
        self.denominator
    }

    pub fn int_value(&self) -> Value {
        if self.denominator().is_int32() && self.denominator().get_int32() == 1 {
            self.numerator()
        } else if self.denominator().is_handle_of::<BigInt>() && self.denominator().get_handle_of::<BigInt>().is_one() {
            self.numerator()
        } else {
            Value::nil()
        }
    }

}

impl Object for Rational {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.numerator.trace(visitor);
        self.denominator.trace(visitor);
    }
}

impl Allocation for Rational {}