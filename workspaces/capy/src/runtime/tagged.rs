use std::marker::PhantomData;
use std::mem::size_of;

use num::ToPrimitive;
use num::Unsigned;

use crate::gc::ptr_compr::HeapCompressionScheme;

use super::cell::CellReference;
use super::smi::Smi;
use super::value::TaggedValue;
use super::value::has_heap_object_tag;
use super::value::has_smi_tag;
use super::value::Tagged;
use super::value::Value;

#[repr(transparent)]
pub struct TaggedImpl<StorageType: Unsigned + Copy + ToPrimitive> {
    pub ptr: StorageType,
}

impl<T: Unsigned + Copy + ToPrimitive> Copy for TaggedImpl<T> {}
impl<T: Unsigned + Copy + ToPrimitive> Clone for TaggedImpl<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<StorageType: Unsigned + Copy + ToPrimitive> TaggedImpl<StorageType> {
    pub const IS_FULL: bool = size_of::<StorageType>() == size_of::<usize>();

    pub fn to_usize(self) -> usize {
        unsafe { self.ptr.to_usize().unwrap_unchecked() }
    }

    #[inline(always)]
    pub const fn ptr(self) -> StorageType {
        self.ptr
    }
    #[inline(always)]
    pub const fn new(t: StorageType) -> Self {
        Self { ptr: t }
    }
    #[inline]
    pub fn is_cell(self) -> bool {
        has_heap_object_tag(self.to_usize())
    }

    #[inline]
    pub fn is_smi(self) -> bool {
        has_smi_tag(self.to_usize())
    }

    #[inline]
    pub fn to_smi(self) -> Tagged<Smi> {
        if Self::IS_FULL {
            return Tagged::<Smi>::new(self.to_usize());
        }

        Tagged::<Smi>::new(HeapCompressionScheme::decompress_tagged_signed(
            self.to_usize() as _,
        ))
    }

    #[inline]
    pub fn get_cell_reference<U>(self) -> Tagged<CellReference<U>> {
        if Self::IS_FULL {
            return Tagged::<CellReference<U>>::new(self.to_usize());
        }

        Tagged::<CellReference<U>>::new(HeapCompressionScheme::decompress_tagged(
            self.to_usize() as _
        ))
    }

    #[inline]
    pub fn get_cell_reference_fast<U>(self, on_heap_addr: usize) -> Tagged<CellReference<U>> {
        if Self::IS_FULL {
            return Tagged::<CellReference<U>>::new(self.to_usize());
        }

        Tagged::<CellReference<U>>::new(HeapCompressionScheme::decompress_tagged_fast(
            on_heap_addr,
            self.to_usize() as _,
        ))
    }

    #[inline]
    pub fn to_value(self, on_heap_addr: usize) -> Tagged<Value> {
        if Self::IS_FULL {
            return Tagged::<Value>::new(self.to_usize());
        }

        if self.is_smi() {
            return Tagged::<Value>::new(HeapCompressionScheme::decompress_tagged_signed(
                self.to_usize() as _,
            ));
        }

        Tagged::<Value>::new(HeapCompressionScheme::decompress_tagged_fast(
            on_heap_addr,
            self.to_usize() as _,
        ))
    }

    

    pub fn null() -> TaggedValue {
        super::value::null()
    }

    pub fn undefined() -> TaggedValue {
        super::value::undefined()
    }

    pub fn unspecified() -> TaggedValue {
        super::value::unspecified()
    }

    pub fn r#true() -> TaggedValue {
        super::value::true_value()
    }

    pub fn r#false() -> TaggedValue {
        super::value::false_value()
    }

    pub fn eof_object() -> TaggedValue {
        super::value::eof_object()
    }


}
