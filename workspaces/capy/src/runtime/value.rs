use std::{marker::PhantomData, mem::size_of};

use crate::gc::ptr_compr::HeapCompressionScheme;

use super::{
    cell::{CellReference, Pair},
    smi::Smi,
    tagged::TaggedImpl,
    thread::Thread,
    *,
};

#[cfg(any(feature = "compressed-oops", target_pointer_width = "32"))]
pub type Word = u32;
#[cfg(not(any(feature = "compressed-oops", target_pointer_width = "32")))]
pub type Word = u64;

#[cfg(any(feature = "compressed-oops", target_pointer_width = "32"))]
pub const SMI_BITS: isize = 31;

#[cfg(not(any(feature = "compressed-oops", target_pointer_width = "32")))]
pub const SMI_BITS: isize = 32;

pub const SMI_TAG: usize = 0;
pub const SMI_TAG_SIZE: usize = 1;
pub const SMI_TAG_MASK: usize = (1 << SMI_TAG_SIZE) - 1;

pub struct SmiTagging<const N: usize>;

pub const ISIZE_ALL_BITS_SET: isize = -1;
pub const USIZE_ALL_BITS_SET: usize = usize::MAX;

// Smi constants for systems where tagged pointer is a 32-bit value
impl SmiTagging<4> {
    pub const SMI_SHIFT_SIZE: usize = 0;
    pub const SMI_VALUE_SIZE: usize = 31;

    pub const SMI_MIN_VALUE: isize = (USIZE_ALL_BITS_SET << (Self::SMI_VALUE_SIZE - 1)) as isize;
    pub const SMI_MAX_VALUE: isize = -(Self::SMI_MIN_VALUE + 1);

    #[inline(always)]
    pub const fn smi_to_int(value: usize) -> i32 {
        let shift_bits = SMI_TAG_SIZE + Self::SMI_SHIFT_SIZE;
        (value as u32 as i32) >> shift_bits
    }

    #[inline(always)]
    pub const fn is_valid_smi(value: isize) -> bool {
        (value as usize - Self::SMI_MIN_VALUE as usize)
            <= (Self::SMI_MAX_VALUE as usize).wrapping_sub(Self::SMI_MIN_VALUE as usize)
    }
}

impl SmiTagging<8> {
    pub const SMI_SHIFT_SIZE: usize = 31;
    pub const SMI_VALUE_SIZE: usize = 32;

    pub const SMI_MIN_VALUE: isize = (USIZE_ALL_BITS_SET << (Self::SMI_VALUE_SIZE - 1)) as isize;
    pub const SMI_MAX_VALUE: isize = -(Self::SMI_MIN_VALUE + 1);

    pub const fn smi_to_int(value: usize) -> i32 {
        let shift_bits = SMI_TAG_SIZE + Self::SMI_SHIFT_SIZE;
        (value as isize >> shift_bits) as i32
    }

    pub const fn is_valid_smi(value: isize) -> bool {
        value as i32 as isize == value
    }
}
pub const API_TAGGED_SIZE: usize = if cfg!(feature = "compressed-oops") {
    4
} else {
    size_of::<usize>()
};

pub const fn pointer_compression_is_enabled() -> bool {
    size_of::<usize>() != API_TAGGED_SIZE
}

pub type PlatformSmiTagging = SmiTagging<API_TAGGED_SIZE>;

pub const SMI_SHIFT_SIZE: usize = PlatformSmiTagging::SMI_SHIFT_SIZE;
pub const SMI_VALUE_SIZE: usize = PlatformSmiTagging::SMI_VALUE_SIZE;
pub const SMI_MIN_VALUE: isize = PlatformSmiTagging::SMI_MIN_VALUE;
pub const SMI_MAX_VALUE: isize = PlatformSmiTagging::SMI_MAX_VALUE;

pub const fn int_to_smi(value: i32) -> usize {
    ((value as usize) << (SMI_TAG_SIZE + SMI_SHIFT_SIZE)) | SMI_TAG
}

pub const HEAP_OBJECT_TAG: usize = 1;
pub const HEAP_OBJECT_TAG_SIZE: usize = 1;
pub const HEAP_OBJECT_TAG_MASK: usize = (1 << HEAP_OBJECT_TAG_SIZE) - 1;

pub const fn has_smi_tag(value: usize) -> bool {
    (value as Word & SMI_TAG_MASK as Word) == SMI_TAG as Word
}

pub const fn has_heap_object_tag(value: usize) -> bool {
    (value as Word & HEAP_OBJECT_TAG_MASK as Word) == HEAP_OBJECT_TAG as Word
}

pub type TaggedValue = TaggedImpl<Word>;
pub type TaggedBase = TaggedImpl<usize>;

/// `Tagged<T>`` represents an uncompressed V8 tagged pointer.
///
/// The tagged pointer is a pointer-sized value with a tag in the LSB. The value
/// is either:
///
///   * A pointer to an object on the MMTk heap, with the tag set to 1
///   * A small integer (Smi), shifted right, with the tag set to 0
///
/// The exact encoding differs depending on 32- vs 64-bit architectures, and in
/// the latter case, whether or not pointer compression is enabled.
///
/// On 32-bit architectures, this is:
///             |----- 32 bits -----|
/// Pointer:    |______address____01|
///    Smi:     |____int31_value___0|
///
/// On 64-bit architectures with pointer compression:
///             |----- 32 bits -----|----- 32 bits -----|
/// Pointer:    |________base_______|______offset_____01|
///    Smi:     |......garbage......|____int31_value___0|
///
/// On 64-bit architectures without pointer compression:
///             |----- 32 bits -----|----- 32 bits -----|
/// Pointer:    |________________address______________01|
///    Smi:     |____int32_value____|00...............00|
///

#[repr(transparent)]
pub struct Tagged<T>(pub TaggedBase, pub PhantomData<T>);

impl<T> Clone for Tagged<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Tagged<T> {}

impl<T> Tagged<T> {
    pub const fn to_value(self) -> TaggedValue {
        TaggedValue::new(self.ptr() as _)
    }

    pub const fn new(address: usize) -> Self {
        Self(TaggedBase::new(address), PhantomData)
    }

    pub const IS_FULL: bool = true;

    pub const fn ptr(self) -> usize {
        self.0.ptr
    }

    pub fn to_smi(self) -> Tagged<Smi> {
        self.0.to_smi()
    }

    pub fn get_cell_reference<U>(self) -> Tagged<CellReference<U>> {
        self.0.get_cell_reference()
    }

    pub fn get_cell_reference_fast<U>(self, on_heap_addr: usize) -> Tagged<CellReference<U>> {
        self.0.get_cell_reference_fast(on_heap_addr)
    }

    #[inline(always)]
    pub fn cast<U>(other: Tagged<U>) -> Self {
        Self(other.0, PhantomData)
    }
}

trait TaggedTrait {
    fn is_smi(self) -> bool;
    fn is_cell_reference(self) -> bool;
    fn cast<U>(other: Tagged<U>) -> Self;
}

// Specialization for `Value` where it's unknown whether this is a Smi or CellReference
impl Tagged<Value> {}
impl TaggedTrait for Tagged<Value> {
    #[inline(always)]
    fn is_smi(self) -> bool {
        has_smi_tag(self.ptr())
    }

    #[inline(always)]
    fn is_cell_reference(self) -> bool {
        has_heap_object_tag(self.ptr())
    }

    #[inline(always)]
    fn cast<U>(other: Tagged<U>) -> Self {
        Self(other.0, PhantomData)
    }
}

impl<T> PartialEq for Tagged<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr() == other.ptr()
    }
}

impl<T> Eq for Tagged<T> {}

impl Tagged<Smi> {
    #[inline(always)]
    pub const fn value(self) -> i32 {
        PlatformSmiTagging::smi_to_int(self.ptr())
    }
}

impl TaggedTrait for Tagged<Smi> {
    #[inline(always)]
    fn is_cell_reference(self) -> bool {
        false
    }
    #[inline(always)]
    fn is_smi(self) -> bool {
        true
    }
    #[inline(always)]
    fn cast<U>(other: Tagged<U>) -> Self {
        debug_assert!(has_smi_tag(other.ptr()));
        Tagged(other.0, PhantomData)
    }
}

impl<T> Tagged<CellReference<T>> {
    #[inline(always)]
    pub fn cell(self) -> CellReference<T> {
        CellReference::from_ptr(self.ptr().wrapping_sub(HEAP_OBJECT_TAG) as _)
    }
}

impl<T> std::ops::Deref for Tagged<CellReference<T>> {
    type Target = CellReference<T>;

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self.ptr().wrapping_sub(HEAP_OBJECT_TAG) as *const CellReference<T>) }
    }
}

impl<T> std::ops::DerefMut for Tagged<CellReference<T>> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self.ptr().wrapping_sub(HEAP_OBJECT_TAG) as *mut CellReference<T>) }
    }
}

impl<T> From<CellReference<T>> for Tagged<CellReference<T>> {
    fn from(value: CellReference<T>) -> Self {
        Self::new(value.to_ptr() as usize + HEAP_OBJECT_TAG)
    }
}

impl<T> TaggedTrait for Tagged<CellReference<T>> {
    #[inline(always)]
    fn cast<U>(other: Tagged<U>) -> Self {
        debug_assert!(has_heap_object_tag(other.ptr()));
        Self(other.0, PhantomData)
    }
    #[inline(always)]
    fn is_cell_reference(self) -> bool {
        true
    }
    #[inline(always)]
    fn is_smi(self) -> bool {
        false
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
#[repr(transparent)]
pub struct Value(pub Word);

pub fn null() -> TaggedValue {
    unsafe { SCM_NULL }
}

pub fn undefined() -> TaggedValue {
    unsafe { SCM_UNDEFINED }
}

pub fn true_value() -> TaggedValue {
    unsafe { SCM_TRUE }
}

pub fn false_value() -> TaggedValue {
    unsafe { SCM_FALSE }
}

pub fn unspecified() -> TaggedValue {
    unsafe { SCM_UNSPECIFIED }
}

pub fn eof_object() -> TaggedValue {
    unsafe { SCM_EOF_OBJECT }
}

