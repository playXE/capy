//! Factory methods for creating Scheme values.

use rsgc::sync::mutex::RawMutex;

use super::bigint::BigInt;
use super::context::Context;
use super::object::{ScmByteVector, ScmString, ScmVector};
use super::value::ScmValue;
use super::{object, value};

impl Context {
    pub fn make_int(&mut self, value: i64) -> ScmValue {
        if value as i32 as i64 == value {
            /* fast path: NaN-boxed int32 */
            ScmValue::encode_int32(value as i32)
        } else {
            /* slow path: heap allocated bigint */
            let bigint = BigInt::from_i64(self.mutator, value);

            ScmValue::encode_object_value(bigint)
        }
    }

    pub fn make_bigint(&mut self, negative: bool, words: impl AsRef<[u32]>) -> ScmValue {
        let bigint = BigInt::from_words_u32(self.mutator, words.as_ref(), negative);

        ScmValue::encode_object_value(bigint)
    }

    pub fn make_float(&mut self, value: f64) -> ScmValue {
        ScmValue::encode_f64_value(value)
    }

    pub fn make_pair(&mut self, car: ScmValue, cdr: ScmValue) -> ScmValue {
        let pair = self.mutator.allocate(object::ScmPair {
            header: object::ScmHeader {
                typ: value::Type::Pair,
                lock: RawMutex::INIT,
            },
            car,
            cdr,
        });

        ScmValue::encode_object_value(pair)
    }

    pub fn make_vector(&mut self, len: u32, init: ScmValue) -> ScmValue {
        unsafe {
            let mut vec = self.mutator.allocate_varsize::<ScmVector>(len as _);

            let vec_ref = vec.assume_init_mut();

            vec_ref.header.typ = value::Type::Vector;
            vec_ref.header.lock = RawMutex::INIT;
            vec_ref.len = len;

            for i in 0..len {
                vec_ref.data.as_mut_ptr().add(i as _).write(init);
            }

            ScmValue::encode_object_value(vec.assume_init())
        }
    }

    pub fn make_vector_from(&mut self, data: impl AsRef<[ScmValue]>) -> ScmValue {
        unsafe {
            let data = data.as_ref();
            let mut vec = self.mutator.allocate_varsize::<ScmVector>(data.len() as _);

            let vec_ref = vec.assume_init_mut();

            vec_ref.header.typ = value::Type::Vector;
            vec_ref.header.lock = RawMutex::INIT;
            vec_ref.len = data.len() as _;

            for i in 0..data.len() {
                vec_ref.data.as_mut_ptr().add(i as _).write(data[i]);
            }

            ScmValue::encode_object_value(vec.assume_init())
        }
    }

    pub fn make_bytevector(&mut self, len: u32, init: u8) -> ScmValue {
        unsafe {
            let mut vec = self.mutator.allocate_varsize::<ScmByteVector>(len as _);

            let vec_ref = vec.assume_init_mut();

            vec_ref.header.typ = value::Type::ByteVector;
            vec_ref.header.lock = RawMutex::INIT;
            vec_ref.len = len;

            for i in 0..len {
                vec_ref.data.as_mut_ptr().add(i as _).write(init);
            }

            ScmValue::encode_object_value(vec.assume_init())
        }
    }


    pub fn make_bytevector_from(&mut self, data: impl AsRef<[u8]>) -> ScmValue {
        unsafe {
            let data = data.as_ref();
            let mut vec = self
                .mutator
                .allocate_varsize::<ScmByteVector>(data.len() as _);

            let vec_ref = vec.assume_init_mut();

            vec_ref.header.typ = value::Type::ByteVector;
            vec_ref.header.lock = RawMutex::INIT;
            vec_ref.len = data.len() as _;

            for i in 0..data.len() {
                vec_ref.data.as_mut_ptr().add(i as _).write(data[i]);
            }

            ScmValue::encode_object_value(vec.assume_init())
        }
    }

    pub fn make_string(&mut self, value: impl AsRef<str>) -> ScmValue {
        let value = value.as_ref();
        let mut string = self.mutator.allocate_varsize::<ScmString>(value.len() + 1);
        unsafe {
            let string_ref = string.assume_init_mut();

            string_ref.header.typ = value::Type::Str;
            string_ref.header.lock = RawMutex::INIT;
            string_ref.len = value.len() as u32 + 1;

            string_ref.data.as_mut_ptr().copy_from_nonoverlapping(value.as_ptr(), value.len());
            string_ref.data.as_mut_ptr().add(value.len()).write(0);

            ScmValue::encode_object_value(string.assume_init())
        }
    }

    pub fn make_char(&mut self, value: char) -> ScmValue {
        let char = self.mutator.allocate(object::ScmChar {
            header: object::ScmHeader {
                typ: value::Type::Char,
                lock: RawMutex::INIT,
            },
            value,
        });

        ScmValue::encode_object_value(char)
    }

    pub fn make_list(&mut self, values: impl AsRef<[ScmValue]>) -> ScmValue {
        let values = values.as_ref();
        let mut list = ScmValue::encode_null_value();

        for value in values.iter().rev() {
            list = self.make_pair(*value, list);
        }

        list
    }
}
