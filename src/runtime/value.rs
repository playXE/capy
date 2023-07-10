use std::{
    fmt::{self, Debug},
    hash::Hash,
};

use crate::{
    compile::LVar,
    runtime::macros::SyntaxRules,
    runtime::object::{
        Box, Bytevector, ClosedNativeProcedure, ExtendedPair, Identifier, Macro, Module,
        NativeProcedure, ObjectHeader, Pair, Procedure, ReaderReference, Str, Symbol, Syntax, Type,
        Vector, GLOC,
    },
    vm::scm_vm, 
};

use super::{
    bigint::BigInt,
    object::{Tuple, Rational, },
    port::{port_extract_string, port_open_bytevector, Port, SCM_PORT_DIRECTION_OUT, SCM_PORT_DIRECTION_IN},
    print::Printer,
    pure_nan::*,
    symbol::Intern,
};
use rsgc::{
    prelude::{Allocation, Handle, Object},
    thread::Thread,
};

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Value(EncodedValueDescriptor);

#[derive(Clone, Copy)]
#[repr(C)]
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
     * On 64-bit platforms we use a NaN-encoded form for immediates.
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

    #[inline(always)]
    pub fn encode_empty_value() -> Self {
        Self(EncodedValueDescriptor {
            as_int64: Self::VALUE_EMPTY,
        })
    }
    #[inline(always)]
    pub fn encode_object_value<T: Object + ?Sized>(gc: Handle<T>) -> Self {
        Self(EncodedValueDescriptor {
            ptr: unsafe { std::mem::transmute(gc) },
        })
    }

    #[inline(always)]
    pub const fn encode_undefined_value() -> Self {
        Self(EncodedValueDescriptor {
            as_int64: Self::VALUE_UNDEFINED as _,
        })
    }

    #[inline(always)]
    pub const fn encode_null_value() -> Self {
        Self(EncodedValueDescriptor {
            as_int64: Self::VALUE_NULL as _,
        })
    }

    #[inline(always)]
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
    #[inline(always)]
    pub fn is_empty(self) -> bool {
        unsafe { self.0.as_int64 == Self::VALUE_EMPTY }
    }

    #[inline(always)]
    pub fn is_undefined(self) -> bool {
        self == Self::encode_undefined_value()
    }
    #[inline(always)]
    pub fn is_null(self) -> bool {
        self == Self::encode_null_value()
    }
    #[inline(always)]
    pub fn is_true(self) -> bool {
        self == Self::encode_bool_value(true)
    }

    #[inline(always)]
    pub fn is_false(self) -> bool {
        self == Self::encode_bool_value(false)
    }

    #[inline(always)]
    pub fn is_boolean(self) -> bool {
        unsafe { (self.0.as_int64 & !1) == Self::VALUE_FALSE as i64 }
    }

    #[inline(always)]
    pub fn is_pointer(self) -> bool {
        unsafe { (self.0.as_int64 & Self::NOT_CELL_MASK) == 0 }
    }

    #[inline(always)]
    pub fn is_int32(self) -> bool {
        unsafe { (self.0.as_int64 & Self::NUMBER_TAG) == Self::NUMBER_TAG }
    }

    #[inline(always)]
    pub fn is_inline_number(self) -> bool {
        unsafe { (self.0.as_int64 & Self::NUMBER_TAG) != 0 }
    }

    #[inline(always)]
    pub fn get_object(self) -> Handle<dyn Object> {
        debug_assert!(self.is_object());

        unsafe { std::mem::transmute(self.0.ptr) }
    }

    #[inline(always)]
    pub fn is_object(self) -> bool {
        self.is_pointer() && !self.is_empty()
    }
    #[inline(always)]
    pub fn get_int32(self) -> i32 {
        unsafe { self.0.as_int64 as i32 }
    }

    #[inline(always)]
    pub fn get_number(self) -> f64 {
        if self.is_int32() {
            return self.get_int32() as _;
        }
        self.get_double()
    }
    #[inline(always)]
    pub fn get_double(self) -> f64 {
        assert!(self.is_double());
        f64::from_bits((unsafe { self.0.as_int64 - Self::DOUBLE_ENCODE_OFFSET }) as u64)
    }
    #[inline(always)]
    pub fn is_double(self) -> bool {
        self.is_inline_number() && !self.is_int32()
    }

    #[inline(always)]
    pub fn is_bool(self) -> bool {
        unsafe { (self.0.as_int64 & !1) == Self::VALUE_FALSE as i64 }
    }

    #[inline(always)]
    pub fn encode_f64_value(x: f64) -> Self {
        Self(EncodedValueDescriptor {
            as_int64: x.to_bits() as i64 + Self::DOUBLE_ENCODE_OFFSET,
        })
    }

    #[inline(always)]
    pub fn encode_untrusted_f64_value(x: f64) -> Self {
        Self::encode_f64_value(purify_nan(x))
    }

    #[inline(always)]
    pub fn encode_nan_value() -> Self {
        Self::encode_f64_value(pure_nan())
    }

    #[inline(always)]
    pub fn encode_int32(x: i32) -> Self {
        Self(EncodedValueDescriptor {
            as_int64: Self::NUMBER_TAG | x as u32 as u64 as i64,
        })
    }

    #[inline(always)]
    pub fn get_raw(self) -> i64 {
        unsafe { self.0.as_int64 }
    }

    #[inline(always)]
    pub fn get_char(self) -> char {
        unsafe { char::from_u32_unchecked((self.0.as_int64 >> 16) as u32) }
    }

    #[inline(always)]
    pub fn encode_char(x: char) -> Self {
        Self(EncodedValueDescriptor {
            as_int64: (((x as u32 as u64) << 16) | Self::NATIVE32_TAG as u64) as i64,
        })
    }
    #[inline(always)]
    pub fn is_char(self) -> bool {
        unsafe { (self.0.as_int64 & Self::NATIVE32_MASK) == Self::NATIVE32_TAG as i64 }
    }

    #[inline(always)]
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
        if self.is_object() && !self.is_empty() {
            
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

    pub fn get_type(self) -> Type {
        if self.is_object() && !self.is_empty() {
            let hdr = unsafe {
                let ptr = std::mem::transmute::<_, *const ObjectHeader>(self.get_object());
                let hdr = ptr as *const ObjectHeader;
                (*hdr).typ
            };

            return hdr;
        }

        if self.is_null() {
            return Type::Null;
        }

        if self.is_undefined() {
            return Type::Undefined;
        }

        if self.is_true() {
            return Type::True;
        }

        if self.is_false() {
            return Type::False;
        }

        if self.is_int32() {
            return Type::Int32;
        }

        if self.is_double() {
            return Type::Double;
        }

        if self.is_char() {
            return Type::Char;
        }

        unreachable!("{}", self)
    }
    #[inline(always)]
    pub fn gloc(self) -> Handle<GLOC> {
        debug_assert!(self.is_xtype(Type::GLOC));
        unsafe { std::mem::transmute(self.0.ptr) }
    }
    #[inline(always)]
    pub fn module(self) -> Handle<Module> {
        debug_assert!(self.is_xtype(Type::Module));
        unsafe { std::mem::transmute(self.0.ptr) }
    }
    #[inline(always)]
    pub fn pair(self) -> Handle<Pair> {
        debug_assert!(self.is_xtype(Type::Pair));
        unsafe { std::mem::transmute(self.0.ptr) }
    }
    #[inline(always)]
    pub fn vector(self) -> Handle<Vector> {
        debug_assert!(self.is_xtype(Type::Vector) || self.is_xtype(Type::Values));
        unsafe { std::mem::transmute(self.0.ptr) }
    }
    #[inline(always)]
    pub fn bytevector(self) -> Handle<Bytevector> {
        debug_assert!(self.is_xtype(Type::Bytevector));
        unsafe { std::mem::transmute(self.0.ptr) }
    }   
    #[inline(always)]
    pub fn string(self) -> Handle<Str> {
        debug_assert!(self.is_xtype(Type::Str));
        unsafe { std::mem::transmute(self.0.ptr) }
    }
    #[inline(always)]
    pub fn symbol(self) -> Handle<Symbol> {
        debug_assert!(self.is_xtype(Type::Symbol));
        unsafe { std::mem::transmute(self.0.ptr) }
    }
    #[inline(always)]
    pub fn identifier(self) -> Handle<Identifier> {
        debug_assert!(self.is_xtype(Type::Identifier));
        unsafe { std::mem::transmute(self.0.ptr) }
    }
    #[inline(always)]
    pub fn car(self) -> Value {
        debug_assert!(self.is_xtype(Type::Pair));
        {
            (*self.pair()).car
        }
    }
    #[inline(always)]
    pub fn cdr(self) -> Value {
        debug_assert!(self.is_xtype(Type::Pair));
        {
            (*self.pair()).cdr
        }
    }   
    #[inline(always)]
    pub fn extended_pair_attr(self) -> Value {
        debug_assert!(self.is_extended_pair());
        unsafe {
            let pair: Handle<ExtendedPair> = std::mem::transmute(self.0.ptr);
            pair.attr
        }
    }
    #[inline(always)]
    pub fn set_extended_pair_attr(self, attr: Value) {
        debug_assert!(self.is_extended_pair());
        unsafe {
            let mut pair: Handle<ExtendedPair> = std::mem::transmute(self.0.ptr);
            pair.attr = attr;
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

    pub fn cdddr(self) -> Value {
        self.cdr().cdr().cdr()
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
    #[inline(always)]
    pub fn set_cdr(self, val: Value) {
        debug_assert!(self.is_xtype(Type::Pair));
        {
            (*self.pair()).cdr = val;
        }
    }
    #[inline(always)]
    pub fn set_car(self, val: Value) {
        debug_assert!(self.is_xtype(Type::Pair));
        {
            (*self.pair()).car = val;
        }
    }
    #[inline(always)]
    pub fn vector_ref(self, idx: usize) -> Value {
        debug_assert!(self.is_xtype(Type::Vector));
        {
            self.vector()[idx]
        }
    }
    #[inline(always)]
    pub fn vector_set(self, idx: usize, val: Value) {
        debug_assert!(self.is_xtype(Type::Vector));
        {
            self.vector()[idx] = val;
        }
    }
    #[inline(always)]
    pub fn values_ref(self, idx: usize) -> Value {
        debug_assert!(self.is_xtype(Type::Values));
        {
            self.vector()[idx]
        }
    }
    #[inline(always)]
    pub fn values(self) -> Handle<Vector> {
        debug_assert!(self.is_xtype(Type::Values));
        unsafe { std::mem::transmute(self.0.ptr) }
    }
    #[inline(always)]
    pub fn is_values(self) -> bool {
        self.is_xtype(Type::Values)
    }
    #[inline(always)]
    pub fn is_eof_object(self) -> bool {
        self.is_xtype(Type::EofObject)
    }
    #[inline(always)]
    pub fn eof_object() -> Self {
        *super::object::EOF_OBJECT
    }
    #[inline(always)]
    pub fn is_bytevector(self) -> bool {
        self.is_xtype(Type::Bytevector)
    }
    #[inline(always)]
    pub fn bytevector_ref(self, idx: usize) -> u8 {
        debug_assert!(self.is_xtype(Type::Bytevector));
        {
            self.bytevector()[idx]
        }
    }
    #[inline(always)]
    pub fn bytevector_set(self, idx: usize, val: u8) {
        debug_assert!(self.is_xtype(Type::Bytevector));
        {
            self.bytevector()[idx] = val;
        }
    }
    #[inline(always)]
    pub fn vector_len(self) -> usize {
        debug_assert!(self.is_xtype(Type::Vector) || self.is_xtype(Type::Values));
        {
            self.vector().len()
        }
    }
    #[inline(always)]
    pub fn bytevector_len(self) -> usize {
        debug_assert!(self.is_xtype(Type::Bytevector));
        {
            self.bytevector().len()
        }
    }
    #[inline(always)]
    pub fn is_identifier(self) -> bool {
        self.is_xtype(Type::Identifier) || self.is_xtype(Type::Symbol)
    }
    #[inline(always)]
    pub fn is_wrapped_identifier(self) -> bool {
        self.is_xtype(Type::Identifier)
    }
    #[inline(always)]
    pub fn is_pair(self) -> bool {
        self.is_xtype(Type::Pair)
    }
    #[inline(always)]
    pub fn is_extended_pair(self) -> bool {
        self.is_xtype(Type::Pair) && self.object_header().is_extended_pair()
    }
    #[inline(always)]
    pub fn is_vector(self) -> bool {
        self.is_xtype(Type::Vector)
    }
    #[inline(always)]
    pub fn is_syntax(self) -> bool {
        self.is_xtype(Type::Syntax)
    }
    #[inline(always)]
    pub fn is_synrules(self) -> bool {
        self.is_xtype(Type::Synrules)
    }
    #[inline(always)]
    pub fn syntax_rules(self) -> Handle<SyntaxRules> {
        assert!(self.is_xtype(Type::Synrules));
        unsafe { std::mem::transmute(self.0.ptr) }
    }
    #[inline(always)]
    pub fn is_module(self) -> bool {
        self.is_xtype(Type::Module)
    }
    #[inline(always)]
    pub fn is_macro(self) -> bool {
        self.is_xtype(Type::Macro)
    }
    #[inline(always)]
    pub fn is_string(self) -> bool {
        self.is_xtype(Type::Str)
    }
    #[inline(always)]
    pub fn is_symbol(self) -> bool {
        self.is_xtype(Type::Symbol)
    }
    #[inline(always)]
    pub fn is_lvar(self) -> bool {
        self.is_xtype(Type::LVar)
    }
    #[inline(always)]
    pub fn object_header(self) -> ObjectHeader {
        debug_assert!(self.is_object());
        unsafe {
            let ptr = self.get_raw() as usize;
            (ptr as *const ObjectHeader).read()
        }
    }
    #[inline(always)]
    pub fn lvar(self) -> Handle<LVar> {
        debug_assert!(self.is_xtype(Type::LVar));
        unsafe { std::mem::transmute(self.0.ptr) }
    }
    #[inline(always)]
    pub fn syntax(self) -> Handle<Syntax> {
        debug_assert!(self.is_xtype(Type::Syntax));
        unsafe { std::mem::transmute(self.0.ptr) }
    }
    #[inline(always)]
    pub fn strsym<'a>(&self) -> &'a str {
        debug_assert!(self.is_xtype(Type::Str) || self.is_xtype(Type::Symbol));
        if self.is_string() {
            // SAFETY: `Value` is transparent wrapper around pointer types
            let s: &Handle<Str> = unsafe { std::mem::transmute(self) };

            &**s
        } else {
            let s: &Handle<Symbol> = unsafe { std::mem::transmute(self) };

            &**s
        }
    }

    pub fn first(self) -> Value {
        self.car()
    }

    pub fn second(self) -> Value {
        self.cdr().car()
    }

    pub fn third(self) -> Value {
        self.cdr().cdr().car()
    }

    pub fn fourth(self) -> Value {
        self.cdr().cdr().cdr().car()
    }

    pub fn fifth(self) -> Value {
        self.cdr().cdr().cdr().cdr().car()
    }

    pub fn sixth(self) -> Value {
        self.cdr().cdr().cdr().cdr().cdr().car()
    }

    pub fn seventh(self) -> Value {
        self.cdr().cdr().cdr().cdr().cdr().cdr().car()
    }

    pub fn eighth(self) -> Value {
        self.cdr().cdr().cdr().cdr().cdr().cdr().cdr().car()
    }

    pub fn ninth(self) -> Value {
        self.cdr().cdr().cdr().cdr().cdr().cdr().cdr().cdr().car()
    }

    pub fn tenth(self) -> Value {
        self.cdr()
            .cdr()
            .cdr()
            .cdr()
            .cdr()
            .cdr()
            .cdr()
            .cdr()
            .cdr()
            .car()
    }
    #[inline(always)]
    pub fn is_reader_reference(self) -> bool {
        self.is_xtype(Type::ReaderReference)
    }
    #[inline(always)]
    pub fn is_reader_reference_realized(self) -> bool {
        self.is_xtype(Type::ReaderReference) && !self.reader_reference().value.is_undefined()
    }
    #[inline(always)]
    pub fn reader_reference(self) -> Handle<ReaderReference> {
        debug_assert!(self.is_xtype(Type::ReaderReference));
        unsafe { std::mem::transmute(self) }
    }
    #[inline(always)]
    pub fn to_bool(self) -> bool {
        !self.is_false() && !self.is_empty()
    }
    #[inline(always)]
    pub fn is_procedure(self) -> bool {
        self.is_xtype(Type::Procedure)
            || self.is_xtype(Type::NativeProcedure)
            || self.is_xtype(Type::ClosedNativeProcedure)
    }
    #[inline(always)]
    pub fn is_native_procedure(self) -> bool {
        self.is_xtype(Type::NativeProcedure) || self.is_xtype(Type::ClosedNativeProcedure)
    }
    #[inline(always)]
    pub fn is_closed_native_procedure(self) -> bool {
        self.is_xtype(Type::ClosedNativeProcedure)
    }
    #[inline(always)]
    pub fn is_vm_procedure(self) -> bool {
        self.is_xtype(Type::Procedure)
    }
    #[inline(always)]
    pub fn procedure(self) -> Handle<Procedure> {
        debug_assert!(self.is_procedure());
        unsafe { std::mem::transmute(self) }
    }
    #[inline(always)]
    pub fn closed_native_procedure(self) -> Handle<ClosedNativeProcedure> {
        debug_assert!(self.is_closed_native_procedure());
        unsafe { std::mem::transmute(self) }
    }
    #[inline(always)]
    pub fn native_procedure(self) -> Handle<NativeProcedure> {
        debug_assert!(self.is_native_procedure());
        unsafe { std::mem::transmute(self) }
    }
    #[inline(always)]
    pub fn is_box(self) -> bool {
        self.is_xtype(Type::Box)
    }
    #[inline(always)]
    pub fn r#box(self) -> Handle<Box> {
        debug_assert!(self.is_box());
        unsafe { std::mem::transmute(self) }
    }
    #[inline(always)]
    pub fn box_ref(self) -> Value {
        debug_assert!(self.is_box());
        unsafe {
            let b: Handle<Box> = std::mem::transmute(self);
            b.value
        }
    }
    #[inline(always)]
    pub fn box_set(self, value: Value) {
        debug_assert!(self.is_box());
        unsafe {
            let mut b: Handle<Box> = std::mem::transmute(self);
            b.value = value;
        }
    }
    #[inline(always)]
    pub fn r#macro(self) -> Handle<Macro> {
        debug_assert!(self.is_macro());
        unsafe { std::mem::transmute(self) }
    }
    #[inline(always)]
    pub fn is_number(self) -> bool {
        self.is_int32()
            || self.is_double()
            || (self.get_type() >= Type::BigNum && self.get_type() <= Type::Complex)
    }
    #[inline(always)]
    pub fn is_bignum(self) -> bool {
        self.is_xtype(Type::BigNum)
    }
    #[inline(always)]
    pub fn is_rational(self) -> bool {
        self.is_xtype(Type::Rational)
    }

    pub fn is_complex(self) -> bool {
        self.is_xtype(Type::Complex)
    }

    pub fn is_exact_integer(self) -> bool {
        self.is_int32() || self.is_bignum()
    }

    pub fn is_exact_real(self) -> bool {
        self.is_exact_integer() || self.is_rational()
    }
    #[inline(always)]
    pub fn is_real(self) -> bool {
        self.is_exact_real() || self.is_double()
    }

    pub fn bignum(self) -> Handle<BigInt> {
        debug_assert!(self.is_bignum());
        unsafe { std::mem::transmute(self) }
    }

    pub fn rational(self) -> Handle<Rational> {
        debug_assert!(self.is_rational());
        unsafe { std::mem::transmute(self) }
    }


    pub fn is_tuple(self) -> bool {
        self.is_xtype(Type::Tuple)
    }

    pub fn tuple(self) -> Handle<Tuple> {
        debug_assert!(self.is_tuple());
        unsafe { std::mem::transmute(self) }
    }

    pub fn tuple_ref(self, index: usize) -> Value {
        debug_assert!(self.is_tuple());
        unsafe {
            let t: Handle<Tuple> = std::mem::transmute(self);
            t[index]
        }
    }

    pub fn tuple_set(self, index: usize, value: Value) {
        debug_assert!(self.is_tuple());
        unsafe {
            let mut t: Handle<Tuple> = std::mem::transmute(self);
            t[index] = value;
        }
    }

    pub fn is_port(self) -> bool {
        self.is_xtype(Type::Port)
    }

    pub fn port(self) -> Handle<Port> {
        debug_assert!(self.is_port());
        unsafe { std::mem::transmute(self) }
    }

    pub fn is_input_port(self) -> bool {
        self.is_port() && (self.port().direction & SCM_PORT_DIRECTION_IN) != 0
    }

    pub fn is_output_port(self) -> bool {
        self.is_port() && (self.port().direction & SCM_PORT_DIRECTION_OUT) != 0
    }

    pub fn get_int64(self) -> Option<i64> {
        if !self.is_exact_integer() {
            return None;
        }

        if self.is_int32() {
            return Some(self.get_int32() as i64);
        } 

        let bignum = self.bignum();
        bignum.i64()
    }

    pub fn normalized(self) -> Value {
        if self.is_bignum() {
            if let Some(x) = self.bignum().i32() {
                Self::encode_int32(x)
            } else {
                self 
            }
        } else if self.is_rational() {
            let d = self.rational().den;

            if d.is_int32() {
                if d.get_int32() == 1 {
                    self.rational().num.normalized()
                } else {
                    self 
                }
            } else if d.is_bignum() {
                if d.bignum().is_one() {
                    self.rational().num.normalized()
                } else {
                    self 
                }
            } else {
                self 
            }
        } else {
            self
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        /*if self.is_bool() {
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
            if self.is_pair() {
                write!(f, "(")?;
                let mut obj = *self;
                while obj.is_pair() {
                    write!(f, "{:?}", obj.car())?;
                    obj = obj.cdr();
                    if !obj.is_pair() {
                        break;
                    } else {
                        write!(f, " ")?;
                    }
                }
                if !obj.is_null() {
                    write!(f, " . {:?})", obj)?;
                } else {
                    write!(f, ")")?;
                }

                return Ok(());
            }

            if self.is_xtype(Type::Synpattern) {
                return crate::runtime::macros::pattern_print(f, *self);
            } else if self.is_xtype(Type::Pvref) {
                return crate::runtime::macros::pvref_print(f, *self);
            }

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
            } else if obj.is_xtype(Type::Identifier) {
                write!(
                    f,
                    "#<identifier {:?}@{:?} {:p}>",
                    self.identifier().module.module().name,
                    self.identifier().name,
                    self.identifier().as_ptr()
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
        }*/

        {
            let port = Port::new(Thread::current());
            port_open_bytevector(
                port,
                "Debug".intern().into(),
                SCM_PORT_DIRECTION_OUT,
                false.into(),
                false.into(),
            );
            let mut printer = Printer::new(scm_vm(), port);
            match printer.write(*self) {
                Ok(_) => match printer.flush() {
                    Ok(_) => write!(f, "{}", port_extract_string(port).unwrap().string()),
                    Err(_) => write!(f, "Error flushing port"),
                },
                Err(_) => write!(f, "Error writing to port"),
            }
        }
    }
}

impl PartialEq<str> for Value {
    fn eq(&self, other: &str) -> bool {
        if !self.is_object() {
            return false;
        }

        if self.is_xtype(Type::Str) {
            return &***self.string() == other;
        } else if self.is_xtype(Type::Symbol) {
            return &**self.symbol() == other;
        } else {
            false
        }
    }
}

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.get_raw().hash(state);
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}


pub fn scm_box(val: Value) -> Value {
    Thread::current().allocate(Box {
        header: ObjectHeader::new(Type::Box),
        value: val,
    }).into()
}

pub fn scm_int(x: i64) -> Value {
    if x as i32 as i64 == x {
        Value::encode_int32(x as _)
    } else {
        BigInt::from_i64(Thread::current(), x).into()
    }
}