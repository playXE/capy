//! Heap cell representation.
//!
//!
//! CapyScheme allocates one word for header + N words for payload. Word size is `size_of::<Value>()` which is always 64-bit.
//!
//!
//! # Header
//!
//! ```text
//! +==========================+ <- Header/object start
//! |    Cell tag (16 bit)     |
//! +--------------------------+
//! |    Cell hash (24 bit)    |
//! +--------------------------+
//! |    Other bits (24 bit)   | <- used for MMTk metadata, might store some bits for specific types, like length for tuples.
//! +==========================+
//! |    Word 0 (64 bit)       |
//! +--------------------------+
//! |    Word 1 (64 bit)       |
//! +--------------------------+
//! |    ...                   |
//! +--------------------------+ <- Object end
//!
//! ```

use std::{marker::PhantomData, mem::{size_of, transmute}, hash::Hasher};

use ahash::AHasher;
use mmtk::{
    util::{Address, ObjectReference},
    vm::{
        edge_shape::SimpleEdge, VMGlobalLogBitSpec, VMLocalForwardingBitsSpec,
        VMLocalForwardingPointerSpec, VMLocalLOSMarkNurserySpec, VMLocalMarkBitSpec,
    },
};

use super::{bitfield::BitField, thread::Thread, value::Value};
use crate::{define_vm_metadata_spec, gc::CapyVM};

/* Tells us if cell has any GC pointers inside it */
define_vm_metadata_spec!(PointerlessBitSpec, false, 0, 8);

pub const POINTERLESS_BIT_METADATA_SPEC: PointerlessBitSpec = PointerlessBitSpec::in_header(60);
pub const FORWARDING_BITS_METADATA_SPEC: VMLocalForwardingBitsSpec =
    VMLocalForwardingBitsSpec::in_header(61);
pub const LOGGING_SIDE_METADATA_SPEC: VMGlobalLogBitSpec = VMGlobalLogBitSpec::side_first();
pub const FORWARDING_POINTER_METADATA_SPEC: VMLocalForwardingPointerSpec =
    VMLocalForwardingPointerSpec::in_header(0);
pub const MARKING_METADATA_SPEC: VMLocalMarkBitSpec = VMLocalMarkBitSpec::side_first();
pub const LOS_METADATA_SPEC: VMLocalLOSMarkNurserySpec = VMLocalLOSMarkNurserySpec::in_header(61);

pub type CellTagBitfield = BitField<16, 0, false>;
pub type CellHashBitfield = BitField<2, 16, false>;

pub const HASH_STATE_HASHED: usize = 0b00;
pub const HASH_STATE_HASHED_AND_MOVED: usize = 0b01;
pub const HASH_STATE_UNHASHED: usize = 0b10;

pub const HASHCODE_SIZE: usize = size_of::<u64>();
pub const HASHCODE_OFFSET: isize = -(HASHCODE_SIZE as isize);

#[derive(PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub struct CellReference<T = ()>(pub ObjectReference, pub PhantomData<*mut T>);

impl<T> Copy for CellReference<T> {}
impl<T> Clone for CellReference<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> CellReference<T> {
    #[inline(always)]
    pub fn header<'a>(&self) -> &'a SchemeHeader {
        unsafe { &*self.0.to_header::<CapyVM>().to_ptr::<SchemeHeader>() }
    }
    #[inline(always)]
    pub fn header_mut<'a>(&self) -> &'a mut SchemeHeader {
        unsafe { &mut *self.0.to_header::<CapyVM>().to_mut_ptr::<SchemeHeader>() }
    }
    #[inline(always)]
    pub fn pointerless(&self) -> bool {
        unsafe { POINTERLESS_BIT_METADATA_SPEC.load::<CapyVM, u8>(self.0, None) != 0 }
    }

    pub fn to_ptr(self) -> *mut Value {
        unsafe { std::mem::transmute(self) }
    }

    pub fn from_ptr(ptr: *mut Value) -> Self {
        unsafe { std::mem::transmute(ptr) }
    }

    pub fn word_ref(&self, index: usize) -> Value {
        unsafe { self.to_ptr().add(index as usize).read() }
    }

    pub fn word_ref_mut(&mut self, index: usize) -> &'static mut Value {
        unsafe { &mut *self.to_ptr().add(index as usize) }
    }

    /// Set word at `index` to `value`.
    ///
    /// # Safety
    ///
    /// Unsafe because does not insert GC write barrier.
    pub unsafe fn word_set_unchecked(&mut self, index: usize, value: Value) {
        self.to_ptr().add(index as usize).write(value)
    }

    pub fn word_set(&mut self, thread: &mut Thread, index: usize, value: Value) {
        thread.reference_write(self.upcast(), index, value)
    }

    pub fn u64_ref(&self, index: usize) -> u64 {
        unsafe { self.to_ptr().add(index as usize).read().get_raw() as _ }
    }

    pub fn u64_set(&mut self, index: usize, value: u64) {
        unsafe { self.to_ptr().add(index as usize).cast::<u64>().write(value) }
    }

    pub fn u8_ref(self, offset: usize) -> u8 {
        unsafe { self.to_ptr().cast::<u8>().add(offset).read() }
    }

    pub fn u8_set(self, offset: usize, value: u8) {
        unsafe { self.to_ptr().cast::<u8>().add(offset).write(value) }
    }

    pub fn u16_ref(self, offset: usize) -> u16 {
        unsafe { self.to_ptr().cast::<u16>().add(offset).read() }
    }

    pub fn u16_set(self, offset: usize, value: u16) {
        unsafe { self.to_ptr().cast::<u16>().add(offset).write(value) }
    }

    pub fn u32_ref(self, offset: usize) -> u32 {
        unsafe { self.to_ptr().cast::<u32>().add(offset).read() }
    }

    pub fn u32_set(self, offset: usize, value: u32) {
        unsafe { self.to_ptr().cast::<u32>().add(offset).write(value) }
    }

    #[inline(always)]
    pub fn edge(self, index: usize) -> SimpleEdge {
        unsafe {
            let reference = self.to_ptr().add(index);
            let edge = SimpleEdge::from_address(Address::from_mut_ptr(reference));

            edge
        }
    }

    pub unsafe fn downcast_unchecked<U>(self) -> CellReference<U> {
        CellReference(self.0, PhantomData)
    }

    pub fn downcast<U: Tagged>(self) -> CellReference<U> {
        debug_assert!(self.header().cell_tag() == U::TAG);
        unsafe { self.downcast_unchecked() }
    }

    pub fn try_downcast<U: Tagged>(self) -> Option<CellReference<U>> {
        if self.header().cell_tag() == U::TAG {
            Some(unsafe { self.downcast_unchecked() })
        } else {
            None
        }
    }

    pub fn cell_tag(self) -> CellTag {
        self.header().cell_tag()
    }

    pub fn upcast(self) -> CellReference {
        CellReference(self.0, PhantomData)
    }

    pub fn vector(self) -> CellReference<Vector> {
        self.downcast()
    }

    pub fn pair(self) -> CellReference<Pair> {
        self.downcast()
    }

    pub fn rational(self) -> CellReference<Rational> {
        self.downcast()
    }

    pub fn complex(self) -> CellReference<Complex> {
        self.downcast()
    }

    pub fn bytevector(self) -> CellReference<Bytevector> {
        self.downcast()
    }

    pub fn string(self) -> CellReference<String> {
        self.downcast()
    }

    pub fn symbol(self) -> CellReference<Symbol> {
        self.downcast()
    }

    pub fn program(self) -> CellReference<Program> {
        self.downcast()
    }

    pub fn is<U: Tagged>(self) -> bool {
        self.header().cell_tag() == U::TAG
    }

    /// Initializes cell header after allocation in MMTk.
    pub unsafe fn post_alloc(at: *mut Self, tag: CellTag) {
        let header = (&mut *at).header_mut();
        header.set_cell_tag(tag);
        header.set_cell_hash(0);
    }

    pub fn hashcode(self) -> u64 {
        let header = self.header();
        let hash_state = header.cell_hash() as usize;

        match hash_state {
            HASH_STATE_HASHED_AND_MOVED => {
                unsafe {
                    let mut addr = transmute::<_, *mut Value>(self);
                    addr = addr.sub(1);
                    addr.cast::<u64>().read()
                }
            }

            /* HASH_STATE_HASHED | HASH_STATE_UNHASHED */
            _ => {
                let mut hasher = AHasher::default();
                hasher.write_usize(unsafe { transmute(self) });

                let hash = hasher.finish();
                // transitions from HASH_STATE_UNHASHED to HASH_STATE_HASHED
                self.header_mut().set_cell_hash(HASH_STATE_HASHED as _);

                hash
            }
        }
    }

    pub(crate) fn hashcode_after_copy(self, hashcode: u64) {
        unsafe {
            let mut addr = transmute::<_, *mut Value>(self);
            addr = addr.sub(1);
            addr.cast::<u64>().write(hashcode);
        }
    }
}


pub extern "C" fn scm_pointerless_p(cell: CellReference) -> bool {
    cell.pointerless()
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct SchemeHeader(pub u64);

impl SchemeHeader {
    pub fn new(tag: CellTag) -> Self {
        let mut this = Self(0);
        this.set_cell_tag(tag);
        this.set_cell_hash(HASH_STATE_UNHASHED as _);
        this
    }

    #[inline(always)]
    pub const fn cell_tag(self) -> CellTag {
        CellTag(CellTagBitfield::decode(self.0) as _)
    }
    #[inline(always)]
    pub fn set_cell_tag(&mut self, tag: CellTag) {
        self.0 = CellTagBitfield::update(tag.0 as _, self.0);
    }
    #[inline(always)]
    pub const fn cell_hash(self) -> u32 {
        CellHashBitfield::decode(self.0) as _
    }
    #[inline(always)]
    pub fn set_cell_hash(&mut self, hash: u32) {
        self.0 = CellHashBitfield::update(hash as _, self.0);
    }
    #[inline(always)]
    pub const fn other_bits(self) -> u32 {
        ((self.0 >> 40) & 0xFFFFFF) as u32
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct CellTag(pub u16);

pub type CellTagType = BitField<14, 0, false>;
pub type CellTagFeature = BitField<2, { CellTagType::NEXT_BIT }, false>;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[rustfmt::skip]
pub enum CellFeature {
    /// Cell is defined by runtime and has known layout.
    /// 
    /// Might be variadic as well.
    None            = 0b00,
    VectorLike      = 0b01,
    BytevectorLike  = 0b10,
}

impl CellTag {
    pub const fn new(tag: u16, feature: CellFeature) -> Self {
        let tag = CellTagType::encode(tag as _);
        let feature_and_tag = CellTagFeature::update(feature as _, tag);

        Self(feature_and_tag as _)
    }

    pub const fn tag(self) -> u16 {
        CellTagType::decode(self.0 as _) as _
    }

    pub const fn feature(self) -> CellFeature {
        match CellTagFeature::decode(self.0 as _) as _ {
            0b00 => CellFeature::None,
            0b01 => CellFeature::VectorLike,
            0b10 => CellFeature::BytevectorLike,
            _ => unreachable!(),
        }
    }

    pub const PAIR: Self = Self::new(0, CellFeature::None);
    pub const PROGRAM: Self = Self::new(1, CellFeature::None);
    pub const RATIONAL: Self = Self::new(2, CellFeature::None);
    pub const COMPLEX: Self = Self::new(3, CellFeature::None);
    // vector-like start
    pub const VECTOR: Self = Self::new(0, CellFeature::VectorLike);
    pub const STRUCTURE: Self = Self::new(1, CellFeature::VectorLike);

    // bytevector-like start
    pub const BYTEVECTOR: Self = Self::new(0, CellFeature::BytevectorLike);
    pub const STRING: Self = Self::new(1, CellFeature::BytevectorLike);
    pub const SYMBOL: Self = Self::new(2, CellFeature::BytevectorLike);
}

pub trait Tagged {
    const TAG: CellTag;
}

pub struct Pair;

impl Tagged for Pair {
    const TAG: CellTag = CellTag::PAIR;
}

impl CellReference<Pair> {
    pub const CAR_OFFSET: usize = 1;
    pub const CDR_OFFSET: usize = 2;
    pub const WORD_COUNT: usize = 3;

    pub fn car(self) -> Value {
        self.word_ref(Self::CAR_OFFSET)
    }

    pub fn cdr(self) -> Value {
        self.word_ref(Self::CDR_OFFSET)
    }

    pub fn set_car(mut self, thread: &mut Thread, value: Value) {
        self.word_set(thread, Self::CAR_OFFSET, value)
    }

    pub fn set_cdr(mut self, thread: &mut Thread, value: Value) {
        self.word_set(thread, Self::CDR_OFFSET, value)
    }
}

pub struct Vector;

impl Tagged for Vector {
    const TAG: CellTag = CellTag::VECTOR;
}

impl CellReference<Vector> {
    pub const LENGTH_OFFSET: usize = 1;
    pub const DATA_START_OFFSET: usize = 2;
    pub const BASE_WORD_COUNT: usize = 2;
    pub fn length(self) -> usize {
        self.word_ref(Self::LENGTH_OFFSET).get_raw() as u64 as usize
    }

    pub fn get(self, index: usize) -> Value {
        debug_assert!(index < self.length());
        self.word_ref(Self::DATA_START_OFFSET + index)
    }

    pub fn set(mut self, thread: &mut Thread, index: usize, value: Value) {
        debug_assert!(index < self.length());
        self.word_set(thread, Self::DATA_START_OFFSET + index, value)
    }

    pub unsafe fn set_unchecked(mut self, index: usize, value: Value) {
        debug_assert!(index < self.length());
        self.word_set_unchecked(Self::DATA_START_OFFSET + index, value)
    }

    pub unsafe fn set_length(mut self, length: usize) {
        self.u64_set(Self::LENGTH_OFFSET, length as _)
    }
}

impl std::ops::Deref for CellReference<Vector> {
    type Target = [Value];

    fn deref(&self) -> &Self::Target {
        unsafe {
            std::slice::from_raw_parts(self.to_ptr().add(Self::DATA_START_OFFSET), self.length())
        }
    }
}

impl std::ops::DerefMut for CellReference<Vector> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            std::slice::from_raw_parts_mut(
                self.to_ptr().add(Self::DATA_START_OFFSET),
                self.length(),
            )
        }
    }
}

pub struct Bytevector;

impl Tagged for Bytevector {
    const TAG: CellTag = CellTag::BYTEVECTOR;
}

impl CellReference<Bytevector> {
    pub const LENGTH_OFFSET: usize = 1;
    pub const DATA_START_OFFSET: usize = 2;
    pub const BASE_WORD_COUNT: usize = 2;
    pub fn length(self) -> usize {
        self.word_ref(Self::LENGTH_OFFSET).get_raw() as u64 as usize
    }

    pub fn get(self, index: usize) -> u8 {
        debug_assert!(index < self.length());
        self.u8_ref(Self::DATA_START_OFFSET * size_of::<Value>() + index)
    }

    pub fn set(self, index: usize, value: u8) {
        debug_assert!(index < self.length());
        self.u8_set(Self::DATA_START_OFFSET * size_of::<Value>() + index, value)
    }

    pub unsafe fn set_length(mut self, length: usize) {
        self.u64_set(Self::LENGTH_OFFSET, length as _)
    }
}

impl std::ops::Deref for CellReference<Bytevector> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        unsafe {
            std::slice::from_raw_parts(
                self.to_ptr().add(Self::DATA_START_OFFSET).cast(),
                self.length(),
            )
        }
    }
}

impl std::ops::DerefMut for CellReference<Bytevector> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            std::slice::from_raw_parts_mut(
                self.to_ptr().add(Self::DATA_START_OFFSET).cast(),
                self.length(),
            )
        }
    }
}

impl Tagged for String {
    const TAG: CellTag = CellTag::STRING;
}

impl CellReference<String> {
    pub const LENGTH_OFFSET: usize = 1;

    pub fn length(self) -> usize {
        self.word_ref(Self::LENGTH_OFFSET).get_raw() as u64 as usize
    }
}

impl std::ops::Deref for CellReference<String> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                self.to_ptr().add(Self::LENGTH_OFFSET + 1).cast(),
                self.length(),
            ))
        }
    }
}

impl std::ops::DerefMut for CellReference<String> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            std::str::from_utf8_unchecked_mut(std::slice::from_raw_parts_mut(
                self.to_ptr().add(Self::LENGTH_OFFSET + 1).cast(),
                self.length(),
            ))
        }
    }
}

pub struct Symbol;

impl Tagged for Symbol {
    const TAG: CellTag = CellTag::SYMBOL;
}

impl CellReference<Symbol> {
    pub const LENGTH_OFFSET: usize = 1;

    pub fn length(self) -> usize {
        self.word_ref(Self::LENGTH_OFFSET).get_raw() as u64 as usize
    }
}

impl std::ops::Deref for CellReference<Symbol> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                self.to_ptr().add(Self::LENGTH_OFFSET + 1).cast(),
                self.length(),
            ))
        }
    }
}

pub struct Program;

impl Tagged for Program {
    const TAG: CellTag = CellTag::PROGRAM;
}

impl CellReference<Program> {
    pub const VCODE_OFFSET: usize = 1;
    pub const NUM_FREE_VARS_OFFSET: usize = 2;
    pub const FREE_VARS_START_OFFSET: usize = 3;

    pub fn vcode(self) -> *const u8 {
        self.u64_ref(Self::VCODE_OFFSET) as *const u8
    }

    pub fn num_free_vars(self) -> usize {
        self.u64_ref(Self::NUM_FREE_VARS_OFFSET) as usize
    }

    pub fn free_vars<'a>(self) -> &'a [Value] {
        unsafe {
            std::slice::from_raw_parts(
                self.to_ptr().add(Self::FREE_VARS_START_OFFSET),
                self.num_free_vars(),
            )
        }
    }

    pub fn free_var(self, index: usize) -> Value {
        debug_assert!(index < self.num_free_vars());
        self.word_ref(Self::FREE_VARS_START_OFFSET + index)
    }

    pub fn set_free_var(mut self, thread: &mut Thread, index: usize, value: Value) {
        debug_assert!(index < self.num_free_vars());
        self.word_set(thread, Self::FREE_VARS_START_OFFSET + index, value)
    }
}

impl std::ops::Deref for CellReference<Program> {
    type Target = [Value];

    fn deref(&self) -> &Self::Target {
        unsafe {
            std::slice::from_raw_parts(
                self.to_ptr().add(Self::FREE_VARS_START_OFFSET),
                self.num_free_vars(),
            )
        }
    }
}

impl std::ops::DerefMut for CellReference<Program> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            std::slice::from_raw_parts_mut(
                self.to_ptr().add(Self::FREE_VARS_START_OFFSET),
                self.num_free_vars(),
            )
        }
    }
}

pub struct Rational;

impl Tagged for Rational {
    const TAG: CellTag = CellTag::RATIONAL;
}

impl CellReference<Rational> {
    pub const NUMERATOR_OFFSET: usize = 1;
    pub const DENOMINATOR_OFFSET: usize = 2;

    pub fn numerator(self) -> Value {
        self.word_ref(Self::NUMERATOR_OFFSET)
    }

    pub fn denominator(self) -> Value {
        self.word_ref(Self::DENOMINATOR_OFFSET)
    }

    pub fn set_numerator(mut self, value: Value) {
        unsafe { self.word_set_unchecked(Self::NUMERATOR_OFFSET, value) } 
    }

    pub fn set_denominator(mut self, value: Value) {
        unsafe { self.word_set_unchecked(Self::DENOMINATOR_OFFSET, value) } 
    }
}

pub struct Complex;

impl Tagged for Complex {
    const TAG: CellTag = CellTag::COMPLEX;
}

impl CellReference<Complex> {
    pub const REAL_OFFSET: usize = 1;
    pub const IMAG_OFFSET: usize = 2;

    pub fn real(self) -> Value {
        self.word_ref(Self::REAL_OFFSET)
    }

    pub fn imag(self) -> Value {
        self.word_ref(Self::IMAG_OFFSET)
    }

    pub fn set_real(mut self, value: Value) {
        unsafe { self.word_set_unchecked(Self::REAL_OFFSET, value) }
    }

    pub fn set_imag(mut self, value: Value) {
        unsafe { self.word_set_unchecked(Self::IMAG_OFFSET, value) } 
    }
}
