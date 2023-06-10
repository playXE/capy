use std::fmt::Debug;

use crate::object::{Bytevector, Module, ObjectHeader, Pair, Str, Symbol, Type, Vector, GLOC, Identifier};

use super::pure_nan::*;
use rsgc::prelude::{Allocation, Handle, Object};

#[derive(Clone, Copy)]
pub struct Value(EncodedValueDescriptor);

#[derive(Clone, Copy)]
pub(crate) union EncodedValueDescriptor {
    as_int64: i64,
    ptr: usize,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self.0.as_int64 == other.0.as_int64 }
    }
}
impl Eq for Value {}

impl Value {
    /*
     * On 64-bit platforms USE(JSVALUE64) should be defined, and we use a NaN-encoded
     * form for immediates.
     *
     * The encoding makes use of unused NaN space in the IEEE754 representation.  Any value
     * with the top 13 bits set represents a QNaN (with the sign bit set).  QNaN values
     * can encode a 51-bit payload.  Hardware produced and C-library payloads typically
     * have a payload of zero.  We assume that non-zero payloads are available to encode
     * pointer and integer values.  Since any 64-bit bit pattern where the top 15 bits are
     * all set represents a NaN with a non-zero payload, we can use this space in the NaN
     * ranges to encode other values (however there are also other ranges of NaN space that
     * could have been selected).
     *
     * This range of NaN space is represented by 64-bit numbers begining with the 15-bit
     * hex patterns 0xFFFC and 0xFFFE - we rely on the fact that no valid double-precision
     * numbers will fall in these ranges.
     *
     * The top 15-bits denote the type of the encoded JSValue:
     *
     *     Pointer {  0000:PPPP:PPPP:PPPP
     *              / 0002:****:****:****
     *     Double  {         ...
     *              \ FFFC:****:****:****
     *     Integer {  FFFE:0000:IIII:IIII
     *
     * The scheme we have implemented encodes double precision values by performing a
     * 64-bit integer addition of the value 2^49 to the number. After this manipulation
     * no encoded double-precision value will begin with the pattern 0x0000 or 0xFFFE.
     * Values must be decoded by reversing this operation before subsequent floating point
     * operations may be peformed.
     *
     * 32-bit signed integers are marked with the 16-bit tag 0xFFFE.
     *
     * The tag 0x0000 denotes a pointer, or another form of tagged immediate. Boolean,
     * null and undefined values are represented by specific, invalid pointer values:
     *
     *     False:     0x06
     *     True:      0x07
     *     Undefined: 0x0a
     *     Null:      0x02
     *
     * These values have the following properties:
     * - Bit 1 (Othectxag) is set for all four values, allowing real pointers to be
     *   quickly distinguished from all immediate values, including these invalid pointers.
     * - With bit 3 masked out (UndefinedTag), Undefined and Null share the
     *   same value, allowing null & undefined to be quickly detected.
     *
     * No valid JSValue will have the bit pattern 0x0, this is used to represent array
     * holes, and as a C++ 'no value' result (e.g. JSValue() has an internal value of 0).
     *
     * When USE(BIGINT32), we have a special representation for BigInts that are small (32-bit at most):
     *      0000:XXXX:XXXX:0012
     * This representation works because of the following things:
     * - It cannot be confused with a Double or Integer thanks to the top bits
     * - It cannot be confused with a pointer to a Cell, thanks to bit 1 which is set to true
     * - It cannot be confused with a pointer to wasm thanks to bit 0 which is set to false
     * - It cannot be confused with true/false because bit 2 is set to false
     * - It cannot be confused for null/undefined because bit 4 is set to true
     */

    pub const DOUBLE_ENCODE_OFFSET_BIT: usize = 49;
    pub const DOUBLE_ENCODE_OFFSET: i64 = 1 << Self::DOUBLE_ENCODE_OFFSET_BIT as i64;
    pub const NUMBER_TAG: i64 = 0xfffe000000000000u64 as i64;
    pub const LOWEST_OF_HIGH_BITS: i64 = 1 << 49;

    pub const OTHER_TAG: i32 = 0x2;
    pub const BOOL_TAG: i32 = 0x4;
    pub const UNDEFINED_TAG: i32 = 0x8;
    pub const NATIVE32_TAG: i32 = 0x12;
    pub const NATIVE32_MASK: i64 = Self::NUMBER_TAG | Self::NATIVE32_TAG as i64;

    pub const VALUE_FALSE: i32 = Self::OTHER_TAG | Self::BOOL_TAG | false as i32;
    pub const VALUE_TRUE: i32 = Self::OTHER_TAG | Self::BOOL_TAG | true as i32;
    pub const VALUE_UNDEFINED: i32 = Self::OTHER_TAG | Self::UNDEFINED_TAG;
    pub const VALUE_NULL: i32 = Self::OTHER_TAG;

    pub const MISC_TAG: i64 =
        Self::OTHER_TAG as i64 | Self::BOOL_TAG as i64 | Self::UNDEFINED_TAG as i64;
    pub const NOT_CELL_MASK: i64 = Self::NUMBER_TAG as i64 | Self::OTHER_TAG as i64;

    pub const VALUE_EMPTY: i64 = 0x0;
    pub const VALUE_DELETED: i64 = 0x4;

    pub const UNDEFINED: Value = Self::encode_undefined_value();

    #[inline]
    pub fn encode_empty_value() -> Self {
        Self(EncodedValueDescriptor {
            as_int64: Self::VALUE_EMPTY,
        })
    }
    #[inline]
    pub fn encode_object_value<T: Object + ?Sized>(gc: Handle<T>) -> Self {
        Self(EncodedValueDescriptor {
            ptr: unsafe { std::mem::transmute(gc) },
        })
    }

    #[inline]
    pub const fn encode_undefined_value() -> Self {
        Self(EncodedValueDescriptor {
            as_int64: Self::VALUE_UNDEFINED as _,
        })
    }

    #[inline]
    pub const fn encode_null_value() -> Self {
        Self(EncodedValueDescriptor {
            as_int64: Self::VALUE_NULL as _,
        })
    }

    #[inline]
    pub fn encode_bool_value(x: bool) -> Self {
        if x {
            Self(EncodedValueDescriptor {
                as_int64: Self::VALUE_TRUE as _,
            })
        } else {
            Self(EncodedValueDescriptor {
                as_int64: Self::VALUE_FALSE as _,
            })
        }
    }
    #[inline]
    pub fn is_empty(self) -> bool {
        unsafe { self.0.as_int64 == Self::VALUE_EMPTY }
    }

    #[inline]
    pub fn is_undefined(self) -> bool {
        self == Self::encode_undefined_value()
    }
    #[inline]
    pub fn is_null(self) -> bool {
        self == Self::encode_null_value()
    }

    #[inline]
    pub fn is_true(self) -> bool {
        self == Self::encode_bool_value(true)
    }

    #[inline]
    pub fn is_false(self) -> bool {
        self == Self::encode_bool_value(false)
    }

    #[inline]
    pub fn is_boolean(self) -> bool {
        unsafe { (self.0.as_int64 & !1) == Self::VALUE_FALSE as i64 }
    }

    #[inline]
    pub fn is_pointer(self) -> bool {
        unsafe { (self.0.as_int64 & Self::NOT_CELL_MASK) == 0 }
    }

    #[inline]
    pub fn is_int32(self) -> bool {
        unsafe { (self.0.as_int64 & Self::NUMBER_TAG) == Self::NUMBER_TAG }
    }

    #[inline]
    pub fn is_number(self) -> bool {
        unsafe { (self.0.as_int64 & Self::NUMBER_TAG) != 0 }
    }

    #[inline]
    pub fn get_object(self) -> Handle<dyn Object> {
        assert!(self.is_object());

        unsafe { std::mem::transmute(self.0.ptr) }
    }

    #[inline]
    pub fn is_object(self) -> bool {
        self.is_pointer() /*&& !self.is_empty()*/
    }
    #[inline]
    pub fn get_int32(self) -> i32 {
        unsafe { self.0.as_int64 as i32 }
    }

    #[inline]
    pub fn get_number(self) -> f64 {
        if self.is_int32() {
            return self.get_int32() as _;
        }
        self.get_double()
    }
    #[inline]
    pub fn get_double(self) -> f64 {
        assert!(self.is_double());
        f64::from_bits((unsafe { self.0.as_int64 - Self::DOUBLE_ENCODE_OFFSET }) as u64)
    }
    #[inline]
    pub fn is_double(self) -> bool {
        self.is_number() && !self.is_int32()
    }

    #[inline]
    pub fn is_bool(self) -> bool {
        unsafe { (self.0.as_int64 & !1) == Self::VALUE_FALSE as i64 }
    }

    #[inline]
    pub fn encode_f64_value(x: f64) -> Self {
        Self(EncodedValueDescriptor {
            as_int64: x.to_bits() as i64 + Self::DOUBLE_ENCODE_OFFSET,
        })
    }

    #[inline]
    pub fn encode_untrusted_f64_value(x: f64) -> Self {
        Self::encode_f64_value(purify_nan(x))
    }

    #[inline]
    pub fn encode_nan_value() -> Self {
        Self::encode_f64_value(pure_nan())
    }

    #[inline]
    pub fn encode_int32(x: i32) -> Self {
        Self(EncodedValueDescriptor {
            as_int64: Self::NUMBER_TAG | x as u32 as u64 as i64,
        })
    }

    #[inline]
    pub fn get_raw(self) -> i64 {
        unsafe { self.0.as_int64 }
    }

    #[inline]
    pub fn get_native_u32(self) -> u32 {
        unsafe { (self.0.as_int64 >> 16) as u32 }
    }

    #[inline]
    pub fn encode_native_u32(x: u32) -> Self {
        Self(EncodedValueDescriptor {
            as_int64: (((x as u64) << 16) | Self::NATIVE32_TAG as u64) as i64,
        })
    }
    #[inline]
    pub fn is_native_value(self) -> bool {
        unsafe { (self.0.as_int64 & Self::NATIVE32_MASK) == Self::NATIVE32_TAG as i64 }
    }

    #[inline]
    pub fn get_bool(self) -> bool {
        assert!(self.is_bool());
        self == Self::encode_bool_value(true)
    }
}

impl Into<Value> for bool {
    fn into(self) -> Value {
        Value::encode_bool_value(self)
    }
}

impl Into<Value> for i32 {
    fn into(self) -> Value {
        Value::encode_int32(self)
    }
}

impl Into<Value> for f64 {
    fn into(self) -> Value {
        Value::encode_f64_value(self)
    }
}

impl<T: Object> Into<Value> for Handle<T> {
    fn into(self) -> Value {
        Value::encode_object_value(self)
    }
}

impl Object for Value {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        if self.is_object() {
            self.get_object().trace(visitor);
        }
    }
}

impl Allocation for Value {}

impl Value {
    /// Check if the value is a heap-allocated type.
    #[inline(always)]
    pub fn is_xtype(self, x: Type) -> bool {
        if !self.is_object() {
            return false;
        }

        let hdr = unsafe {
            let ptr = std::mem::transmute::<_, *const ObjectHeader>(self.get_object());
            let hdr = ptr as *const ObjectHeader;
            (*hdr).typ
        };

        hdr == x
    }
    /// Check if the value is of type `x`.
    #[inline(always)]
    pub fn is_type(self, x: Type) -> bool {
        match x {
            Type::Null => self.is_null(),
            Type::Undefined => self.is_undefined(),
            Type::True => self.is_true(),
            Type::False => self.is_false(),
            Type::Int32 => self.is_int32(),
            Type::Double => self.is_double(),
            _ => self.is_xtype(x),
        }
    }

    pub fn gloc(self) -> Handle<GLOC> {
        assert!(self.is_xtype(Type::GLOC));
        unsafe { std::mem::transmute(self.0.ptr) }
    }

    pub fn module(self) -> Handle<Module> {
        assert!(self.is_xtype(Type::Module));
        unsafe { std::mem::transmute(self.0.ptr) }
    }

    pub fn pair(self) -> Handle<Pair> {
        assert!(self.is_xtype(Type::Pair));
        unsafe { std::mem::transmute(self.0.ptr) }
    }

    pub fn vector(self) -> Handle<Vector> {
        assert!(self.is_xtype(Type::Vector));
        unsafe { std::mem::transmute(self.0.ptr) }
    }

    pub fn bytevector(self) -> Handle<Bytevector> {
        assert!(self.is_xtype(Type::Bytevector));
        unsafe { std::mem::transmute(self.0.ptr) }
    }

    pub fn string(self) -> Handle<Str> {
        assert!(self.is_xtype(Type::Str));
        unsafe { std::mem::transmute(self.0.ptr) }
    }

    pub fn symbol(self) -> Handle<Symbol> {
        assert!(self.is_xtype(Type::Symbol));
        unsafe { std::mem::transmute(self.0.ptr) }
    }

    pub fn identifier(self) -> Handle<Identifier> {
        assert!(self.is_xtype(Type::Identifier));
        unsafe { std::mem::transmute(self.0.ptr) }
    }

    pub fn car(self) -> Value {
        assert!(self.is_xtype(Type::Pair));
        {
            (*self.pair()).car
        }
    }

    pub fn cdr(self) -> Value {
        assert!(self.is_xtype(Type::Pair));
        {
            (*self.pair()).cdr
        }
    }

    pub fn caar(self) -> Value {
        self.car().car()
    }

    pub fn cadr(self) -> Value {
        self.cdr().car()
    }

    pub fn cdar(self) -> Value {
        self.car().cdr()
    }

    pub fn cddr(self) -> Value {
        self.cdr().cdr()
    }

    pub fn caddr(self) -> Value {
        self.cdr().cdr().car()
    }

    pub fn cadddr(self) -> Value {
        self.cdr().cdr().cdr().car()
    }

    pub fn cddddr(self) -> Value {
        self.cdr().cdr().cdr().cdr()
    }

    pub fn cdddddr(self) -> Value {
        self.cdr().cdr().cdr().cdr().cdr()
    }

    pub fn cdadr(self) -> Value {
        self.cdr().car().cdr()
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_bool() {
            if self.is_true() {
                write!(f, "#t")
            } else {
                write!(f, "#f")
            }
        } else if self.is_null() {
            write!(f, "()")
        } else if self.is_undefined() {
            write!(f, "#<undefined>")
        } else if self.is_int32() {
            write!(f, "{}", self.get_int32())
        } else if self.is_double() {
            write!(f, "{}", self.get_double())
        } else if self.is_object() {
            let obj = *self;
            if obj.is_xtype(Type::Str) {
                write!(f, "{}", obj.string())
            } else if obj.is_xtype(Type::Symbol) {
                write!(f, "{}", obj.symbol())
            } else if obj.is_xtype(Type::Module) {
                write!(
                    f,
                    "#<module '{:?}' at 0x{:x}>",
                    obj.module().name,
                    self.get_raw()
                )
            } else {
                let obj = unsafe {
                    let ptr = std::mem::transmute::<_, *const ObjectHeader>(obj.get_object());
                    let hdr = ptr as *const ObjectHeader;
                    (*hdr).typ
                };

                write!(f, "#<{:?} 0x{:x}>", obj, self.get_raw())
            }
        } else {
            write!(f, "#<unknown>")
        }
    }
}

impl PartialEq<str> for Value {
    fn eq(&self, other: &str) -> bool {
        if !self.is_object() {
            return false;
        }

        if self.is_xtype(Type::Str) {
            return &**self.string() == other;
        } else if self.is_xtype(Type::Symbol) {
            return &**self.symbol() == other;
        } else {
            false
        }
    }
}