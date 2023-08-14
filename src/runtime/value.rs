use std::{hash::Hash, mem::transmute};

use mmtk::{
    memory_manager::object_reference_write,
    util::Address,
    vm::{edge_shape::SimpleEdge, EdgeVisitor},
};

use crate::{runtime::object::*, vm::thread::Thread, compiler::{tree_il::LVar, P}};

use super::{
    object::TypeId,
    pure_nan::{pure_nan, purify_nan},
};

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Value(pub EncodedValueDescriptor);

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        unsafe { self.0.as_int64.hash(state) }
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
pub union EncodedValueDescriptor {
    pub as_int64: i64,
    pub ptr: usize,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self.0.as_int64 == other.0.as_int64 }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Value({:x})", unsafe { self.0.as_int64 })
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
    pub const fn encode_empty_value() -> Self {
        Self(EncodedValueDescriptor {
            as_int64: Self::VALUE_EMPTY,
        })
    }
    #[inline(always)]
    pub fn encode_object_value(reference: ScmCellRef) -> Self {
        Self(EncodedValueDescriptor {
            ptr: unsafe { std::mem::transmute(reference) },
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
    pub fn get_object(self) -> ScmCellRef {
        debug_assert!(self.is_object(), "{:?}", self);

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
        } else if self.is_double() {
            self.get_double()
        } else {
            pure_nan()
        }
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

impl Value {
    pub fn visit_edge<EV: EdgeVisitor<SimpleEdge>>(&mut self, visitor: &mut EV) {
        if self.is_object() {
            // Pointers in Value are transparent, we can directly pass them as edge
            visitor.visit_edge(SimpleEdge::from_address(Address::from_mut_ptr(self)));
        }
    }

    pub fn cast_as<'a, T>(self) -> &'a mut T {
        unsafe {
            debug_assert!(self.is_object());
            &mut *(self.0.ptr as *mut T)
        }
    }

    pub fn type_of(self) -> TypeId {
        if self.is_object() {
            self.get_object().header().type_id()
        } else {
            if self.is_true() {
                TypeId::True
            } else if self.is_false() {
                TypeId::False
            } else if self.is_int32() {
                TypeId::Int32
            } else if self.is_double() {
                TypeId::Double
            } else if self.is_char() {
                TypeId::Char
            } else if self.is_undefined() {
                TypeId::Undefined
            } else if self.is_null() {
                TypeId::Null
            } else {
                unreachable!()
            }
        }
    }

    pub fn assign(&mut self, src: Value, other: Value) {
        let thr = Thread::current();
        unsafe {
            debug_assert!(src.is_object());
            if other.is_object() {
                object_reference_write(
                    thr.mutator(),
                    transmute(src),
                    transmute::<_, SimpleEdge>(self),
                    transmute(other),
                );
            } else {
                *self = other;
            }
        }
    }

    pub fn is_syntax_expander(self) -> bool {
        self.type_of() == TypeId::SyntaxExpander
    }

    pub fn syntax_expander<'a>(self) -> &'a mut ScmSyntaxExpander {
        self.cast_as()
    }

    pub fn is_identifier(self) -> bool {
        self.type_of() == TypeId::Identifier
    }

    pub fn identifier<'a>(self) -> &'a mut ScmIdentifier {
        self.cast_as()
    }

    pub fn is_lvar(self) -> bool {
        self.type_of() == TypeId::LVar
    }

    pub fn lvar(self) -> P<LVar> {
        self.cast_as::<ScmLVar>().lvar.clone()
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_null() {
            write!(f, "()")
        } else if self.is_undefined() {
            write!(f, "#<undefined>")
        } else if self.is_true() {
            write!(f, "#t")
        } else if self.is_false() {
            write!(f, "#f")
        } else if self.is_int32() {
            write!(f, "{}", self.get_int32())
        } else if self.is_double() {
            write!(f, "{}", self.get_double())
        } else if self.is_pair() {
            let mut lst = *self;

            write!(f, "(")?;

            loop {
                write!(f, "{}", scm_car(lst))?;

                lst = scm_cdr(lst);

                if lst.is_null() {
                    break;
                }

                if lst.is_pair() {
                    write!(f, " ")?;
                } else {
                    write!(f, " . {}", lst)?;
                    break;
                }
            }

            write!(f, ")")
        } else if self.is_vector() {
            let mut vec = *self;

            write!(f, "#(")?;

            loop {
                write!(f, "{}", scm_vector_ref(vec, 0))?;

                vec = scm_vector_ref(vec, 1);

                if vec.is_null() {
                    break;
                }

                write!(f, " ")?;
            }

            write!(f, ")")
        } else if self.is_string() {
            write!(f, "\"{}\"", scm_string_str(*self))
        } else if self.is_symbol() {
            write!(f, "{}", scm_symbol_str(*self))
        } else if self.is_char() {
            write!(f, "#\\{}", self.get_char())
        } else if self.is_box() {
            write!(f, "#<box {}>", self.cast_as::<ScmBox>().value)
        } else if self.is_program() {
            write!(f, "#<program at {:p}>", self.cast_as::<ScmProgram>().vcode)
        } else {
            write!(f, "#<unknown {:x}>", self.get_raw())
        }
    }
}
