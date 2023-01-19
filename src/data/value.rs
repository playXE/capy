use super::{
    exception::{Exception, SourcePosition},
    pure_nan::{pure_nan, purify_nan}, structure::{StructInstance, StructType, StructProperty},
};
use crate::{
    data::special_form::SpecialForm,
    prelude::{*, eval_error::EvalError},
    runtime::Runtime,
    utilities::{arraylist::ArrayList, string_builder::StringBuilder},
};
use core::fmt;
use std::{collections::HashSet, fmt::Debug, intrinsics::likely, hash::Hash};
#[derive(Copy, Clone, Debug)]
pub struct Value(pub(crate) EncodedValueDescriptor);
#[derive(Clone, Copy)]
pub(crate) union EncodedValueDescriptor {
    as_int64: i64,
    #[cfg(target_pointer_width = "32")]
    as_double: f64,

    ptr: usize,
    #[allow(dead_code)]
    as_bits: AsBits,
}

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        unsafe { self.0.as_int64.hash(state) }
    }
}

impl Debug for EncodedValueDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("EncodedValueDescriptor {}", unsafe {
            self.ptr
        }))
    }
}
impl Value {
    pub fn raw(self) -> u64 {
        unsafe { std::mem::transmute(self) }
    }
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self.0.as_int64 == other.0.as_int64 }
    }
}
impl Eq for Value {}
#[derive(Clone, Copy, PartialEq, Eq)]
#[cfg(target_endian = "little")]
#[repr(C)]
pub struct AsBits {
    pub payload: i32,
    pub tag: i32,
}
#[derive(Clone, Copy, PactxialEq, Eq)]
#[cfg(target_endian = "big")]
#[repr(C)]
pub struct AsBits {
    pub tag: i32,
    pub payload: i32,
}

#[cfg(target_pointer_width = "32")]
impl Value {
    pub const INT32_TAG: u64 = 0xffffffff;
    pub const BOOLEAN_TAG: u64 = 0xfffffffe;
    pub const NULL_TAG: u64 = 0xfffffffd;
    pub const UNDEFINED_TAG: u64 = 0xfffffffc;
    pub const CELL_TAG: u64 = 0xfffffffb;
    pub const EMPTY_VALUE_TAG: u64 = 0xfffffffa;
    pub const DELETED_VALUE_TAG: u64 = 0xfffffff9;
    pub const LOWEST_TAG: u64 = Self::DELETED_VALUE_TAG;
    #[inline]
    pub fn encode_null_value() -> Self {
        Self(EncodedValueDescriptor {
            as_bits: AsBits {
                tag: Self::NULL_TAG as _,
                payload: 0,
            },
        })
    }
    #[inline]
    pub fn encode_undefined_value() -> Self {
        Self(EncodedValueDescriptor {
            as_bits: AsBits {
                tag: Self::UNDEFINED_TAG as _,
                payload: 0,
            },
        })
    }
    #[inline]
    pub fn encode_bool_value(x: bool) -> Self {
        Self(EncodedValueDescriptor {
            as_bits: AsBits {
                tag: Self::BOOLEAN_TAG as _,
                payload: x as _,
            },
        })
    }

    #[inline]
    pub fn encode_handle_value<T: Object + ?Sized>(val: Handle<T>) -> Self {
        Self(EncodedValueDescriptor {
            as_bits: AsBits {
                tag: Self::CELL_TAG as _,
                payload: unsafe { std::mem::transmute(val) },
            },
        })
    }
    #[inline]
    pub fn encode_empty_value() -> Self {
        Self(EncodedValueDescriptor {
            as_bits: AsBits {
                tag: Self::EMPTY_VALUE_TAG as _,
                payload: 0,
            },
        })
    }
    #[inline]
    pub fn is_undefined(self) -> bool {
        self.tag() == Self::UNDEFINED_TAG as _
    }

    #[inline]
    pub fn is_null(self) -> bool {
        self.tag() == Self::NULL_TAG as _
    }
    #[inline]
    pub fn is_pointer(self) -> bool {
        self.tag() == Self::CELL_TAG as _
    }
    #[inline]
    pub fn is_int32(self) -> bool {
        self.tag() == Self::INT32_TAG as _
    }
    #[inline]
    pub fn is_double(self) -> bool {
        (self.tag() as u64) < Self::LOWEST_TAG
    }
    pub fn is_bool(self) -> bool {
        self.tag() == Self::BOOL_TAG as _
    }
    #[inline]
    pub fn get_bool(self) -> bool {
        assectx!(self.is_bool());
        self.payload() == 1
    }
    #[inline]
    pub fn is_empty(self) -> bool {
        self.tag() == Self::EMPTY_TAG as _
    }
    #[inline]
    pub fn tag(&self) -> i32 {
        unsafe { self.0.as_bits.tag }
    }
    #[inline]
    pub fn payload(&self) -> i32 {
        unsafe { self.0.as_bits.payload }
    }

    #[inline]
    pub fn is_handle(&self) -> bool {
        self.is_pointer() && !self.is_empty()
    }

    #[inline]
    pub fn get_handle(self) -> Handle<dyn Object> {
        assert!(self.is_handle());

        unsafe { std::mem::transmute(self.as_bits.payload) }
    }
}
#[cfg(target_pointer_width = "64")]
impl Value {
    /*
     * On 64-bit platforms USE(Value64) should be defined, and we use a NaN-encoded
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
     * The top 15-bits denote the type of the encoded Value:
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
     * No valid Value will have the bit pattern 0x0, this is used to represent array
     * holes, and as a C++ 'no value' result (e.g. Value() has an internal value of 0).
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
    pub fn encode_handle_value<T: Object + ?Sized>(gc: Handle<T>) -> Self {
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
    pub fn get_handle(self) -> Handle<dyn Object> {
        assert!(self.is_handle());

        unsafe { std::mem::transmute(self.0.ptr) }
    }

    #[inline]
    pub fn is_handle(self) -> bool {
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

impl Object for Value {
    fn trace(&self, visitor: &mut dyn rsgc::system::traits::Visitor) {
        if self.is_handle() {
            self.get_handle().trace(visitor);
        }
    }
}

impl Value {
    pub fn typ(&self) -> Type {
        if self.is_null() {
            Type::Null
        } else if self.is_void() {
            Type::Void
        } else if self.is_undefined() {
            Type::Undefined
        } else if self.is_bool() {
            Type::Boolean
        } else if self.is_int32() {
            Type::Fixnum
        } else if self.is_double() {
            Type::Float
        } else if self.is_bytes() {
            Type::ByteVector
        } else if self.is_vector() {
            Type::Vector
        } else if self.is_symbol() {
            Type::Symbol
        } else if self.is_string() {
            Type::Str
        } else if self.is_pair() {
            Type::Pair
        } else if self.is_eof() {
            Type::Eof
        } else if self.is_handle_of::<Procedure>() {
            Type::Procedure
        } else if self.is_char() {
            Type::Char
        } else if self.is_handle_of::<StructInstance>() {
            Type::Struct(self.get_handle_of::<StructInstance>().struct_type())
        } else if self.is_handle_of::<StructType>() {
            Type::StructType(Some(self.get_handle_of()))
        } else {
            todo!()
        }
    }

    #[inline]
    pub unsafe fn fill(start: *mut Self, end: *mut Self, fill: Value) {
        let mut cur = start;
        while cur != end {
            cur.write(fill);
            cur = cur.add(1);
        }
    }

    #[inline]
    pub unsafe fn uninit_copy(
        mut first: *mut Self,
        last: *mut Self,
        mut result: *mut Value,
    ) -> *mut Value {
        while first != last {
            result.write(first.read());
            first = first.add(1);
            result = result.add(1);
        }
        result
    }

    #[inline]
    pub unsafe fn copy_backward(
        first: *mut Self,
        mut last: *mut Self,
        mut result: *mut Value,
    ) -> *mut Value {
        while first != last {
            last = last.sub(1);
            result = result.sub(1);
            result.write(last.read());
        }
        result
    }
    #[inline]
    pub unsafe fn copy(
        mut first: *mut Self,
        last: *mut Self,
        mut result: *mut Value,
    ) -> *mut Value {
        while first != last {
            result.write(first.read());
            first = first.add(1);
            result = result.add(1);
        }
        result
    }
    #[inline]
    pub fn new(x: impl Into<Self>) -> Self {
        x.into()
    }

    #[inline]
    pub fn is_pair(self) -> bool {
        self.is_handle() && self.get_handle().is::<Pair>()
    }

    pub fn pair(self) -> Handle<Pair> {
        assert!(self.is_pair());
        unsafe { self.get_handle().downcast::<Pair>().unwrap_unchecked() }
    }

    pub fn cdr(self) -> Self {
        self.pair().cdr()
    }

    pub fn car(self) -> Self {
        self.pair().car()
    }

    pub fn cadr(self) -> Self {
        self.cdr().car()
    }

    pub fn caddr(self) -> Self {
        self.cdr().cdr().car()
    }

    pub fn cddr(self) -> Self {
        self.cdr().cdr()
    }

    pub fn cdddr(self) -> Self {
        self.cdr().cdr().cdr()
    }

    pub fn cddddr(self) -> Self {
        self.cdr().cdr().cdr().cdr()
    }

    pub fn cdar(self) -> Self {
        self.car().cdr()
    }

    pub fn is_list_recsafe(self) -> bool {
        let mut turtle;
        let mut hare = self;

        if !hare.is_pair() {
            return hare.is_null();
        }

        turtle = hare;

        hare = hare.cdr();

        while hare.is_pair() {
            if hare.raw() == turtle.raw() {
                return false;
            }

            hare = hare.cdr();
            if hare.is_pair() {
                hare = hare.cdr();
            }

            turtle = turtle.cdr();
        }

        hare.is_null()
    }

    pub fn is_list(self) -> bool {
        let mut hare = self;

        if !hare.is_pair() {
            return hare.is_null();
        }

        while hare.is_pair() {
            hare = hare.cdr();
            if hare.is_null() {
                return true;
            }
        }

        false
    }

    pub fn length(self) -> usize {
        let mut len = 0;
        let mut hare = self;

        while hare.is_pair() {
            len += 1;
            hare = hare.cdr();
        }

        len
    }

    pub fn length_recsafe(self) -> (bool, usize) {
        let mut res = 1;
        let mut ls2;
        let mut ls1 = self;

        if !ls1.is_pair() {
            return (true, 0);
        }

        ls2 = ls1.car();
        while ls2.is_pair() && ls2.cdr().is_pair() {
            if ls1.raw() == ls2.raw() {
                return (false, res + 1);
            }

            res += 2;
            ls1 = ls1.cdr();
            ls2 = ls2.cddr();
        }

        (true, res + if ls2.is_pair() { 1 } else { 0 })
    }

    pub fn is_handle_of<T: Object>(self) -> bool {
        self.is_handle() && self.get_handle().is::<T>()
    }

    pub fn get_handle_of<T: Object>(self) -> Handle<T> {
        assert!(self.is_handle_of::<T>());
        unsafe { self.get_handle().downcast::<T>().unwrap_unchecked() }
    }

    pub fn is_environment(self) -> bool {
        self.is_handle_of::<Environment>()
    }

    pub fn get_environment(self) -> Handle<Environment> {
        self.get_handle_of::<Environment>()
    }

    pub fn is_vector(self) -> bool {
        self.is_handle_of::<Vector>()
    }

    pub fn get_vector(self) -> Handle<Vector> {
        self.get_handle_of::<Vector>()
    }

    pub fn is_symbol(self) -> bool {
        self.is_handle_of::<Symbol>()
    }

    pub fn get_symbol(self) -> Handle<Symbol> {
        self.get_handle_of::<Symbol>()
    }

    pub fn is_string(self) -> bool {
        self.is_handle_of::<Str>()
    }

    pub fn get_string(self) -> Handle<Str> {
        self.get_handle_of::<Str>()
    }

    pub fn is_bytes(self) -> bool {
        self.is_handle_of::<ByteVector>()
    }

    pub fn get_bytes(self) -> Handle<ByteVector> {
        self.get_handle_of::<ByteVector>()
    }

    pub fn is_void(self) -> bool {
        self.is_handle_of::<Void>()
    }

    pub fn is_eof(self) -> bool {
        self.is_handle_of::<Eof>()
    }

    pub fn eof() -> Self {
        Runtime::get().eof
    }

    pub fn void() -> Self {
        Runtime::get().void
    }

    pub fn make_list_from_stack(ctx: &mut Context, exprs: &[Value], append: Value) -> Value {
        let mut res = append;
        for expr in exprs {
            res = ctx.make_pair(*expr, res);
        }

        res
    }

    pub fn make_list_arraylist(ctx: &mut Context, ls: &ArrayList<Value>, append: Value) -> Value {
        let reverse = ls.reverse(ctx.mutator());
        Self::make_list_from_stack(ctx, &reverse, append)
    }

    pub fn make_list_slice(ctx: &mut Context, orig_ls: &[Value], append: Value) -> Value {
        let mut ls = ArrayList::with_capacity(ctx.mutator(), orig_ls.len());
        for expr in orig_ls.iter().rev() {
            ls.push(ctx.mutator(), *expr);
        }
        Self::make_list_from_stack(ctx, &ls, append)
    }
}

impl Value {
    pub fn nil() -> Self {
        Self::encode_null_value()
    }
}

impl From<i32> for Value {
    fn from(x: i32) -> Self {
        Self::encode_int32(x)
    }
}

impl From<f64> for Value {
    fn from(x: f64) -> Self {
        Self::encode_untrusted_f64_value(x)
    }
}

impl From<bool> for Value {
    fn from(x: bool) -> Self {
        Self::encode_bool_value(x)
    }
}

impl<T: Object> From<Handle<T>> for Value {
    fn from(x: Handle<T>) -> Self {
        Self::encode_handle_value(x)
    }
}

pub struct Null;

impl From<Null> for Value {
    fn from(_: Null) -> Self {
        Self::nil()
    }
}

pub struct Pair {
    pub car: Value,
    pub cdr: Value,
}

impl Object for Pair {
    fn trace(&self, visitor: &mut dyn Visitor) {
        self.car.trace(visitor);
        self.cdr.trace(visitor);
    }
}

impl Allocation for Pair {}

impl Pair {
    pub fn new(thread: &mut Thread, car: Value, cdr: Value) -> Handle<Self> {
        let pair = thread.allocate(Self { car, cdr });
        pair
    }

    pub fn car(&self) -> Value {
        self.car
    }

    pub fn cdr(&self) -> Value {
        self.cdr
    }

    pub fn set_car(&mut self, car: Value) {
        self.car = car;
    }

    pub fn set_cdr(&mut self, cdr: Value) {
        self.cdr = cdr;
    }
}

impl Allocation for Value {}

pub struct Eof;
pub struct Void;

impl Object for Eof {}
impl Object for Void {}

impl Allocation for Eof {}
impl Allocation for Void {}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Undefined,
    Error,
    Void,
    Eof,
    Null,
    Symbol,
    Boolean,
    Fixnum,
    Integer,
    Float,
    Str,
    Char,
    ByteVector,
    Pair,
    Box,
    Array,
    Vector,
    Procedure,
    Parameter,
    Special,
    Env,
    Port,
    Number,
    List,
    ProperList,
    AssocList,
    InputPort,
    OutputPort,
    TextInputPort,
    TextOutputPort,
    BinaryInputPort,
    BinaryOutputPort,
    Syntax,
    Table,
    Struct(Handle<StructType>),
    StructType(Option<Handle<StructType>>),
    StructProperty(Option<Handle<StructProperty>>),
    Object(Handle<Symbol>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Undefined => write!(f, "undefined"),
            Type::Error => write!(f, "error"),
            Type::Void => write!(f, "void"),
            Type::Eof => write!(f, "eof"),
            Type::Null => write!(f, "null"),
            Type::Symbol => write!(f, "symbol"),
            Type::Boolean => write!(f, "boolean"),
            Type::Fixnum => write!(f, "fixnum"),
            Type::Integer => write!(f, "integer"),
            Type::Char => write!(f, "char"),
            Type::Float => write!(f, "float"),
            Type::Str => write!(f, "string"),
            Type::ByteVector => write!(f, "bytevector"),
            Type::Pair => write!(f, "pair"),
            Type::Box => write!(f, "box"),
            Type::Array => write!(f, "array"),
            Type::Vector => write!(f, "vector"),
            Type::Procedure => write!(f, "procedure"),
            Type::Parameter => write!(f, "parameter"),
            Type::Special => write!(f, "special"),
            Type::Env => write!(f, "environment"),
            Type::Port => write!(f, "port"),
            Type::Number => write!(f, "number"),
            Type::List => write!(f, "list"),
            Type::ProperList => write!(f, "proper-list"),
            Type::AssocList => write!(f, "assoc-list"),
            Type::InputPort => write!(f, "input-port"),
            Type::OutputPort => write!(f, "output-port"),
            Type::TextInputPort => write!(f, "text-input-port"),
            Type::TextOutputPort => write!(f, "text-output-port"),
            Type::BinaryInputPort => write!(f, "binary-input-port"),
            Type::BinaryOutputPort => write!(f, "binary-output-port"),
            Type::Syntax => write!(f, "syntax"),
            Type::Table => write!(f, "table"),
            Type::Struct(name) => write!(f, "#<{}>", name.name()),
            Type::StructType(None) => write!(f, "#<struct-type>"),
            Type::StructType(Some(ty)) => write!(f, "#<struct-type {}>", ty.name()),
            Type::StructProperty(Some(ty)) => write!(f, "#<struct-property {}>", ty.name),
            Type::StructProperty(None) => write!(f, "#<struct-property>"),
            Type::Object(sym) => write!(f, "{}", sym.identifier()),
        }
    }
}

impl Object for Type {}
impl Allocation for Type {}

impl Value {
    pub fn to_string(&self, escape: bool) -> String {
        fn string_repr_of(
            visited: &mut HashSet<*mut u8>,
            obj_id: &mut std::collections::HashMap<*mut u8, usize>,
            val: Value,
            escape: bool,
        ) -> String {
            let obj_id_string = |obj_id: &mut std::collections::HashMap<*mut u8, usize>,
                                 obj: Handle<dyn Object>| {
                if let Some(id) = obj_id.get(&obj.as_ptr()) {
                    Some(format!("#{}", id))
                } else if visited.contains(&obj.as_ptr()) {
                    obj_id.insert(obj.as_ptr(), obj_id.len());
                    Some(format!("#{}", obj_id.len() - 1))
                } else {
                    None
                }
            };

            let fix_string = |obj_id: &std::collections::HashMap<_, _>,
                              _ref: Handle<dyn Object>,
                              str: String| {
                if let Some(id) = obj_id.get(&_ref.as_ptr()) {
                    format!("#{}={}", id, str)
                } else {
                    str
                }
            };

            fn double_string(f: f64) -> String {
                if f.is_infinite() {
                    if f.is_sign_negative() {
                        "-inf.0".to_string()
                    } else {
                        "+inf.0".to_string()
                    }
                } else if f.is_nan() {
                    if f.is_sign_negative() {
                        "-nan.0".to_string()
                    } else {
                        "+nan.0".to_string()
                    }
                } else {
                    f.to_string()
                }
            }

            if val.is_undefined() {
                "#<undef>".to_string()
            } else if val.is_void() {
                "#<void>".to_string()
            } else if val.is_eof() {
                "#<eof>".to_string()
            } else if val.is_null() {
                "()".to_string()
            } else if val.is_bool() {
                if val.get_bool() {
                    "#t".to_string()
                } else {
                    "#f".to_string()
                }
            } else if val.is_handle_of::<Uninit>() {
                let uninit = val.get_handle_of::<Uninit>();
                format!("#<uninit {}>", uninit.sym().identifier())
            } else if val.is_handle_of::<Symbol>() {
                val.get_handle_of::<Symbol>().identifier().to_string()
            } else if val.is_int32() {
                val.get_int32().to_string()
            } else if val.is_double() {
                double_string(val.get_double())
            } else if val.is_string() {
                if escape {
                    format!("{:?}", val.get_string().to_string())
                } else {
                    val.get_string().to_string()
                }
            } else if val.is_pair() {
                let pair = val.get_handle_of::<Pair>();

                if let Some(res) = obj_id_string(obj_id, Handle::as_dyn(&pair)) {
                    return res;
                } else {
                    visited.insert(pair.as_ptr());

                    let mut expr = Value::new(pair);
                    let mut builder = StringBuilder::new("(", "", Some(" "), None);
                    while expr.is_pair() {
                        let car = expr.car();
                        let cdr = expr.cdr();

                        builder.append(string_repr_of(visited, obj_id, car, escape));
                        expr = cdr;
                    }
                    let res = if expr.is_null() {
                        format!("{})", builder)
                    } else {
                        format!(
                            "{} . {})",
                            builder,
                            string_repr_of(visited, obj_id, expr, escape)
                        )
                    };
                    visited.remove(&pair.as_ptr());

                    fix_string(obj_id, Handle::as_dyn(&pair), res)
                }
            } else if val.is_bytes() {
                let bytes = val.get_handle_of::<ByteVector>();

                let mut builder = StringBuilder::new("#u8(", ")", Some(" "), None);

                for byte in bytes.iter() {
                    builder.append(format!("{}", byte));
                }

                builder.to_string()
            } else if val.is_handle_of::<Value>() {
                let boxed = val.get_handle_of::<Value>();
                if let Some(res) = obj_id_string(obj_id, Handle::as_dyn(&boxed)) {
                    res
                } else {
                    visited.insert(boxed.as_ptr());
                    let res = format!("#<box {}>", string_repr_of(visited, obj_id, *boxed, escape));
                    visited.remove(&boxed.as_ptr());
                    fix_string(obj_id, Handle::as_dyn(&boxed), res)
                }
            } else if val.is_handle_of::<Values>() {
                let mut builder = StringBuilder::new("#<values", ">", Some(" "), Some(": "));

                let mut expr = val.get_handle_of::<Values>().0;

                while expr.is_pair() {
                    let car = expr.car();
                    let cdr = expr.cdr();

                    builder.append(string_repr_of(visited, obj_id, car, escape));
                    expr = cdr;
                }

                builder.to_string()
            } else if val.is_handle_of::<Array<Value>>() {
                let array = val.get_handle_of::<Array<Value>>();

                if let Some(res) = obj_id_string(obj_id, Handle::as_dyn(&array)) {
                    return res;
                } else {
                    visited.insert(array.as_ptr());

                    let mut builder = StringBuilder::new("#<array", ">", Some(" "), None);

                    for elem in array.iter() {
                        builder.append(string_repr_of(visited, obj_id, *elem, escape));
                    }

                    let res = builder.to_string();
                    visited.remove(&array.as_ptr());

                    fix_string(obj_id, Handle::as_dyn(&array), res)
                }
            } else if val.is_handle_of::<Vector>() {
                let vector = val.get_handle_of::<Vector>();

                if let Some(res) = obj_id_string(obj_id, Handle::as_dyn(&vector)) {
                    return res;
                } else {
                    visited.insert(vector.as_ptr());
                    let prefix = if vector.growable { "#g(" } else { "#(" };
                    let mut builder = StringBuilder::new(prefix, ">", Some(" "), None);

                    for elem in vector.elements.iter() {
                        builder.append(string_repr_of(visited, obj_id, *elem, escape));
                    }

                    let res = builder.to_string();
                    visited.remove(&vector.as_ptr());

                    fix_string(obj_id, Handle::as_dyn(&vector), res)
                }
            } else if val.is_handle_of::<Procedure>() {
                let proc = val.get_handle_of::<Procedure>();

                match proc.kind {
                    ProcedureKind::Parameter(tuple) => {
                        if let Some(res) = obj_id_string(obj_id, Handle::as_dyn(&proc)) {
                            return res;
                        } else {
                            visited.insert(proc.as_ptr());
                            let res = format!(
                                "#<parameter {}: {}>",
                                proc.name(),
                                string_repr_of(visited, obj_id, tuple.cdr(), escape)
                            );
                            visited.remove(&proc.as_ptr());
                            fix_string(obj_id, Handle::as_dyn(&tuple), res)
                        }
                    }
                    ProcedureKind::RawContinuation(_) => {
                        format!("#<raw-continuation {}>", proc.embedded_name())
                    }
                    ProcedureKind::Closure(ClosureType::Continuation, _, _, _) => {
                        format!("#<continuation {}>", proc.embedded_name())
                    }

                    _ => {
                        format!("#<procedure {}>", proc.embedded_name())
                    }
                }
            } else if val.is_handle_of::<SpecialForm>() {
                format!("#<special {}>", val.get_handle_of::<SpecialForm>().name())
            } else if val.is_handle_of::<Syntax>() {
                string_repr_of(visited, obj_id, val.get_handle_of::<Syntax>().expr, escape)
            } else if val.is_handle_of::<Library>() {
                format!(
                    "#<library {:p}: {}>",
                    val.get_handle(),
                    string_repr_of(visited, obj_id, val.get_handle_of::<Library>().name, escape)
                )
            } else if val.is_handle_of::<Gloc>() {
                format!(
                    "#<gloc {:p}: {}>",
                    val.raw() as *mut u8,
                    val.get_handle_of::<Gloc>().value.to_string(escape)
                )
            } else if val.is_handle_of::<Exception>() {
                format!("#<exception {}>", val.get_handle_of::<Exception>().inline_description())
            } else if val.is_handle_of::<Identifier>() {
                format!("#<identifier {}>", val.get_handle_of::<Identifier>().name.to_string(false))
            } else if val.is_handle_of::<StructInstance>() {
                format!("#<{}>", val.get_handle_of::<StructInstance>().struct_type().name())
            } else if val.is_handle_of::<StructType>() {
                format!("#<struct-type {}>", val.get_handle_of::<StructType>().name())
            } else {
                format!("#<unknown {:p}>", val.raw() as *mut u8)
            }
        }

        let mut visited = HashSet::new();
        let mut obj_id = std::collections::HashMap::new();
        string_repr_of(&mut visited, &mut obj_id, self.clone(), escape)
    }

    #[inline]
    pub fn assert_type(&self, ctx: &mut Context, pos: SourcePosition, types: &[Type]) -> ScmResult<()> {
        for typ in types {
            if let Some(subtypes) = typ.included() {
                for subtype in subtypes {
                    if likely(*subtype == self.typ()) {
                        return Ok(())
                    }
                }
            }

            if likely(self.typ() == *typ) {
                return Ok(());
            }
        }

        let exc = Exception::type_error(ctx, types, *self, pos);
        ctx.error(exc)
    }

    #[inline]
    pub fn is_char(self) -> bool {
        self.is_native_value()
    }

    #[inline]
    pub fn encode_char(c: u32) -> Self {
        Self::encode_native_u32(c as u32)
    }
}

pub struct Values(pub Value);

impl Object for Values {
    fn trace(&self, visitor: &mut dyn Visitor) {
        self.0.trace(visitor);
    }
}

impl Allocation for Values {}

pub struct Syntax {
    pub pos: SourcePosition,
    pub expr: Value,
}

impl Object for Syntax {
    fn trace(&self, visitor: &mut dyn Visitor) {
        self.expr.trace(visitor);
    }
}

impl Allocation for Syntax {}

pub enum GlocFlag {
    BindingMut,
    BindingConst,
    BindingCustom(
        Option<fn(&mut Context, Handle<Gloc>) -> ScmResult>,
        Option<fn(&mut Context, Handle<Gloc>, Value) -> ScmResult<()>>,
    ),
}

pub struct Gloc {
    name: Handle<Symbol>,
    module: Handle<Library>,
    pub(crate) value: Value,
    get: Option<fn(&mut Context, Handle<Gloc>) -> ScmResult>,
    set: Option<fn(&mut Context, Handle<Gloc>, Value) -> ScmResult<()>>,
}

impl Gloc {
    pub fn name(&self) -> Handle<Symbol> {
        self.name
    }
    pub fn module(&self) -> Handle<Library> {
        self.module
    }

    pub(crate) fn new(name: Handle<Symbol>, module: Handle<Library>, value: Value) -> Self {
        Self {
            name,
            module,
            value,
            get: None,
            set: None,
        }
    }

    pub fn mark(&mut self, flag: GlocFlag) {
        match flag {
            GlocFlag::BindingConst => {
                self.get = None;
                self.set = Some({
                    #[allow(unused_variables)]
                    fn set(ctx: &mut Context, gloc: Handle<Gloc>, value: Value) -> ScmResult<()> {
                        let exc = Exception::eval(ctx, EvalError::BindingImmutable, &[Value::new(gloc.name)], SourcePosition::unknown());
                        ctx.error(exc)
                    }

                    set
                })
            }
            GlocFlag::BindingMut => {}

            _ => todo!(),
        }
    }

    pub fn get(ctx: &mut Context, gloc: Handle<Gloc>) -> ScmResult {
        if let Some(get) = gloc.get {
            get(ctx, gloc)
        } else {
            Ok(gloc.value)
        }
    }

    pub fn set(ctx: &mut Context, mut gloc: Handle<Gloc>, value: Value) -> ScmResult<()> {
        if let Some(set) = gloc.set {
            set(ctx, gloc, value)
        } else {
            gloc.value = value;
            Ok(())
        }
    }
}

impl Object for Gloc {
    fn trace(&self, visitor: &mut dyn Visitor) {
        self.value.trace(visitor);
        self.name.trace(visitor);
        self.module.trace(visitor);
       // println!("TRACE GLOC {:p}", self);
    }
}

impl Drop for Gloc {
    fn drop(&mut self) {
        //println!("DROP GLOC {:p}", self);
    }
}

impl Allocation for Gloc {}

/// Upvalue is a reference to a value on the stack or a closed value.
pub struct Upvalue {
    pub(crate) next: Option<Handle<Self>>,
    pub(crate) closed: bool,
    pub(crate) state: UpvalueState,
}

pub(crate) union UpvalueState {
    pub closed: Value,
    pub stack: *mut Value,
}

impl Object for Upvalue {
    fn trace(&self, visitor: &mut dyn Visitor) {
        self.get().trace(visitor);
        if let Some(next) = self.next {
            next.trace(visitor);
        }
    }
}

impl Allocation for Upvalue {}

impl Upvalue {
    pub(crate) fn next(&self) -> Option<Handle<Upvalue>> {
        self.next
    }

    pub(crate) fn stack_location(&self) -> *mut Value {
        debug_assert!(!self.is_closed());
        unsafe { self.state.stack }
    }

    #[inline]
    pub(crate) fn close(&mut self) {
        unsafe {
            let val = self.state.stack.read();
            self.state.closed = val;
            self.closed = true;
        }
    }

    /// Checks if upvalue is closed
    #[inline]
    pub fn is_closed(&self) -> bool {
        self.closed
    }

    #[inline]
    pub fn get(&self) -> Value {
        if self.is_closed() {
            unsafe { self.state.closed }
        } else {
            unsafe { self.state.stack.read() }
        }
    }

    #[inline]
    pub fn set(&mut self, value: Value) {
        if self.is_closed() {
            self.state.closed = value;
        } else {
            unsafe {
                self.state.stack.write(value);
            }
        }
    }
}

impl Type {
    pub fn included(&self) -> Option<&[Type]> {
        Some(match self {
            Type::Procedure => &[Type::Procedure, Type::Parameter],
            Type::Number => &[Type::Integer, Type::Fixnum, Type::Float],
            Type::Integer => &[Type::Integer, Type::Fixnum],
            Type::Float => &[Type::Float],
            Type::ProperList => &[Type::Null, Type::Pair, Type::ProperList],
            Type::AssocList => &[Type::AssocList, Type::Null, Type::Pair],
            Type::List => &[
                Type::List,
                Type::Pair,
                Type::Null,
                Type::ProperList,
                Type::AssocList,
            ],
            _ => return None,
        })
    }
}




impl Default for Value {
    fn default() -> Self {
        Self::nil()
    }
}

impl Value {
    pub fn numberp(self) -> bool {
        self.is_int32() || self.is_double()
    }

    pub fn intp(self) -> bool {
        self.is_int32()
    }
}