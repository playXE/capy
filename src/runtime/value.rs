use rsgc::{
    prelude::{Allocation, Handle, Object},
    sync::mutex::RawMutex,
};

use super::{object::*, pure_nan::*};

#[derive(Clone, Copy)]
pub struct ScmValue(EncodedValueDescriptor);

#[derive(Clone, Copy)]
pub(crate) union EncodedValueDescriptor {
    as_int64: i64,
    ptr: usize,
}

impl PartialEq for ScmValue {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self.0.as_int64 == other.0.as_int64 }
    }
}
impl Eq for ScmValue {}

impl ScmValue {
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

    pub const UNDEFINED: ScmValue = Self::encode_undefined_value();

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
        self.is_pointer() && !self.is_empty()
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

impl Into<ScmValue> for bool {
    fn into(self) -> ScmValue {
        ScmValue::encode_bool_value(self)
    }
}

impl Into<ScmValue> for i32 {
    fn into(self) -> ScmValue {
        ScmValue::encode_int32(self)
    }
}

impl Into<ScmValue> for f64 {
    fn into(self) -> ScmValue {
        ScmValue::encode_f64_value(self)
    }
}

impl<T: Object> Into<ScmValue> for Handle<T> {
    fn into(self) -> ScmValue {
        ScmValue::encode_object_value(self)
    }
}

impl ScmValue {
    pub unsafe fn heap_header<'a>(self) -> &'a ScmHeader {
        let ptr = self.0.ptr as *const ScmHeader;
        &*ptr
    }

    pub unsafe fn heap_header_mut<'a>(self) -> &'a mut ScmHeader {
        let ptr = self.0.ptr as *mut ScmHeader;
        &mut *ptr
    }

    pub fn get_type(self) -> Type {
        if self.is_undefined() {
            return Type::Undef;
        } else if self.is_bool() {
            return if self.get_bool() {
                Type::True
            } else {
                Type::False
            };
        } else if self.is_null() {
            return Type::Null;
        } else if self.is_int32() {
            return Type::Integer;
        } else if self.is_double() {
            return Type::Double;
        } else if self.is_object() {
            unsafe {
                return self.heap_header().typ;
            }
        } else {
            unreachable!()
        }
    }

    pub fn is_on_heap(self) -> bool {
        self.get_type() > Type::Eof // eof is statically allocated
    }

    pub fn encode_eof() -> Self {
        Self(EncodedValueDescriptor {
            as_int64: &EOF_VAL as *const _ as i64,
        })
    }

    pub fn is_pair(self) -> bool {
        self.is_object() && self.get_type() == Type::Pair
    }

    pub fn is_symbol(self) -> bool {
        self.is_object() && self.get_type() == Type::Symbol
    }

    pub fn is_symbol_of(self, name: &str) -> bool {
        self.is_symbol() && self.strvalue() == name
    }

    pub fn is_string(self) -> bool {
        self.is_object() && self.get_type() == Type::Str
    }

    pub fn is_vector(self) -> bool {
        self.is_object() && self.get_type() == Type::Vector
    }

    pub fn is_bytevector(self) -> bool {
        self.is_object() && self.get_type() == Type::ByteVector
    }

    pub fn is_port(self) -> bool {
        self.is_object() && self.get_type() == Type::Port
    }

    pub fn is_eof(self) -> bool {
        self.is_object() && self.get_type() == Type::Eof
    }

    pub fn is_procedure(self) -> bool {
        self.is_object()
            && self.get_type() >= Type::PrimitiveProcedure
            && self.get_type() <= Type::NativeProcedure
    }

    pub fn is_values(self) -> bool {
        self.is_object() && self.get_type() == Type::Values
    }

    pub fn is_hashtable(self) -> bool {
        self.is_object() && self.get_type() == Type::HashTable
    }

    pub fn is_env(self) -> bool {
        self.is_object() && self.get_type() == Type::Env
    }

    pub fn is_boxed(self) -> bool {
        self.is_object() && self.get_type() == Type::Boxed
    }

    pub fn get_pair(self) -> Handle<ScmPair> {
        assert!(self.is_pair());
        unsafe { Handle::from_raw(self.0.ptr as *mut u8) }
    }

    pub fn get_symbol(self) -> Handle<ScmSymbol> {
        assert!(self.is_symbol());
        unsafe { Handle::from_raw(self.0.ptr as *mut u8) }
    }

    pub fn get_string(self) -> Handle<ScmString> {
        assert!(self.is_string());
        unsafe { Handle::from_raw(self.0.ptr as *mut u8) }
    }

    pub fn get_vector(self) -> Handle<ScmVector> {
        assert!(self.is_vector());
        unsafe { Handle::from_raw(self.0.ptr as *mut u8) }
    }

    pub fn get_bytevector(self) -> Handle<ScmByteVector> {
        assert!(self.is_bytevector());
        unsafe { Handle::from_raw(self.0.ptr as *mut u8) }
    }

    pub fn strvalue<'a>(self) -> &'a str {
        assert!(self.is_string() || self.is_symbol());

        unsafe {
            if self.is_string() {
                std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                    self.get_string().data.as_ptr(),
                    self.get_string().len as usize - 1,
                ))
            } else {
                std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                    self.get_symbol().data.as_ptr(),
                    self.get_symbol().len as usize - 1,
                ))
            }
        }
    }

    pub fn strlen(self) -> usize {
        assert!(self.is_string() || self.is_symbol());

        if self.is_string() {
            (*self.get_string()).len()
        } else {
            (*self.get_symbol()).len()
        }
    }

    pub fn car(self) -> Self {
        assert!(self.is_pair());
        {
            self.get_pair().car
        }
    }

    pub fn cdr(self) -> Self {
        assert!(self.is_pair());
        {
            self.get_pair().cdr
        }
    }

    pub fn set_car(self, val: Self) {
        assert!(self.is_pair());
        {
            self.get_pair().car = val;
        }
    }

    pub fn set_cdr(self, val: Self) {
        assert!(self.is_pair());
        {
            self.get_pair().cdr = val;
        }
    }

    pub fn caar(&self) -> Self {
        self.car().car()
    }

    pub fn cadr(&self) -> Self {
        self.cdr().car()
    }

    pub fn cdar(&self) -> Self {
        self.car().cdr()
    }

    pub fn cddr(&self) -> Self {
        self.cdr().cdr()
    }

    pub fn caaar(&self) -> Self {
        self.car().car().car()
    }

    pub fn caadr(&self) -> Self {
        self.cdr().car().car()
    }

    pub fn cadar(&self) -> Self {
        self.car().cdr().car()
    }

    pub fn caddr(&self) -> Self {
        self.cdr().cdr().car()
    }

    pub fn cdaar(&self) -> Self {
        self.car().car().cdr()
    }

    pub fn cdadr(&self) -> Self {
        self.cdr().car().cdr()
    }

    pub fn cddar(&self) -> Self {
        self.car().cdr().cdr()
    }

    pub fn cdddr(&self) -> Self {
        self.cdr().cdr().cdr()
    }

    pub fn caaaar(&self) -> Self {
        self.car().car().car().car()
    }

    pub fn caaadr(&self) -> Self {
        self.cdr().car().car().car()
    }

    pub fn caadar(&self) -> Self {
        self.car().cdr().car().car()
    }

    pub fn caaddr(&self) -> Self {
        self.cdr().cdr().car().car()
    }

    pub fn cadaar(&self) -> Self {
        self.car().car().cdr().car()
    }

    pub fn cadadr(&self) -> Self {
        self.cdr().car().cdr().car()
    }

    pub fn caddar(&self) -> Self {
        self.car().cdr().cdr().car()
    }

    pub fn cadddr(&self) -> Self {
        self.cdr().cdr().cdr().car()
    }

    pub fn cdaaar(&self) -> Self {
        self.car().car().car().cdr()
    }

    pub fn cdaadr(&self) -> Self {
        self.cdr().car().car().cdr()
    }

    pub fn cdadar(&self) -> Self {
        self.car().cdr().car().cdr()
    }

    pub fn cdaddr(&self) -> Self {
        self.cdr().cdr().car().cdr()
    }

    pub fn cddaar(&self) -> Self {
        self.car().car().cdr().cdr()
    }

    pub fn cddadr(&self) -> Self {
        self.cdr().car().cdr().cdr()
    }

    pub fn cdddar(&self) -> Self {
        self.car().cdr().cdr().cdr()
    }

    pub fn cddddr(&self) -> Self {
        self.cdr().cdr().cdr().cdr()
    }

    pub fn vector_ref(self, idx: usize) -> Self {
        assert!(self.is_vector());
        self.get_vector()[idx]
    }

    pub fn vector_set(self, idx: usize, val: Self) {
        assert!(self.is_vector());
        self.get_vector()[idx] = val;
    }

    pub fn vector_length(self) -> usize {
        assert!(self.is_vector());
        self.get_vector().len()
    }

    pub fn bytevector_ref(self, idx: usize) -> u8 {
        assert!(self.is_bytevector());
        self.get_bytevector()[idx]
    }

    pub fn bytevector_set(self, idx: usize, val: u8) {
        assert!(self.is_bytevector());
        self.get_bytevector()[idx] = val;
    }

    pub fn bytevector_length(self) -> usize {
        assert!(self.is_bytevector());
        self.get_bytevector().len()
    }

    pub fn bytevector_slice<'a>(self) -> &'a [u8] {
        assert!(self.is_bytevector());
        unsafe {
            let bv = self.get_bytevector();
            std::slice::from_raw_parts(bv.as_ptr(), bv.len())
        }
    }

    pub fn bytevector_slice_mut<'a>(self) -> &'a mut [u8] {
        assert!(self.is_bytevector());
        unsafe {
            let mut bv = self.get_bytevector();
            std::slice::from_raw_parts_mut(bv.as_mut_ptr(), bv.len())
        }
    }

    pub fn to_boolean(self) -> bool {
        !self.is_false()
    }


}

impl Object for ScmValue {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        if self.is_on_heap() {
            self.get_object().trace(visitor);
        }
    }
}

impl Allocation for ScmValue {}

/// A static value that represents `#<eof>` object.
pub static EOF_VAL: ScmHeader = ScmHeader {
    typ: Type::Eof,
    lock: RawMutex::INIT,
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(u8)]
pub enum Type {
    True = 0,
    False = 1,
    Undef = 2,
    Null = 3,
    Integer = 4,
    Double = 5,
    /*
    Heap objects start

    Note that it is VERY important that GCed object types are *after* non-GCed types. All types after `Eof` are GCed.
    `Eof` is the last non-GCed type that is statically allocated.

     */
    Eof,
    Bignum,
    Rational,

    Complex,

    Pair,
    Symbol,
    Str,
    ByteVector,
    Vector,
    Char,
    PrimitiveProcedure,
    ClosedPrimitiveProcedure,
    Parameter,
    ReturnCont,
    ProcStruct,
    NativeProcedure,

    Struct,
    StructType,
    StructProperty,

    Values,
    HashTable,
    Env,
    Boxed,
    Port,
}

pub mod jit {
    use super::*;
    use b3::{BasicBlockBuilder, ValueId};

    pub fn encode_int32(builder: &mut BasicBlockBuilder, value: ValueId) -> ValueId {
        let x = builder.zext32(value);
        let number_tag = builder.const64(ScmValue::NUMBER_TAG as i64);
        builder.binary(b3::Opcode::BitOr, x, number_tag)
    }

    pub fn encode_bool_value(builder: &mut BasicBlockBuilder, value: ValueId) -> ValueId {
        let x = builder.zext32(value);
        let number_tag = builder.const64(ScmValue::OTHER_TAG as i64 | ScmValue::BOOL_TAG as i64);
        builder.binary(b3::Opcode::BitOr, x, number_tag)
    }

    pub fn encode_undefined(builder: &mut BasicBlockBuilder) -> ValueId {
        builder.const64(ScmValue::UNDEFINED_TAG as i64)
    }

    pub fn encode_object(_builder: &mut BasicBlockBuilder, value: ValueId) -> ValueId {
        value
    }

    pub fn encode_f64(builder: &mut BasicBlockBuilder, value: ValueId) -> ValueId {
        let x = builder.bitwise_cast(b3::Type::Int64, value);
        let number_tag = builder.const64(ScmValue::DOUBLE_ENCODE_OFFSET as i64);
        builder.binary(b3::Opcode::Add, x, number_tag)
    }

    pub fn decode_int32(builder: &mut BasicBlockBuilder, value: ValueId) -> ValueId {
        builder.trunc(value)
    }

    pub fn decode_bool_value(builder: &mut BasicBlockBuilder, value: ValueId) -> ValueId {
        let x = ScmValue::encode_bool_value(true);

        let x = builder.const64(unsafe { x.0.as_int64 });

        builder.binary(b3::Opcode::Equal, value, x)
    }

    pub fn decode_f64(builder: &mut BasicBlockBuilder, value: ValueId) -> ValueId {
        let offset = builder.const64(ScmValue::DOUBLE_ENCODE_OFFSET as i64);
        let x = builder.binary(b3::Opcode::Sub, offset, value);
        builder.bitwise_cast(b3::Type::Double, x)
    }
}
