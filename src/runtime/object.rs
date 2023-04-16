use std::ops::{Deref, DerefMut};

use super::value::{ScmValue, Type};
use rsgc::{
    prelude::{Allocation, Object},
    sync::mutex::RawMutex,
};

#[repr(C)]
pub struct ScmHeader {
    pub(crate) typ: Type,
    pub(crate) lock: RawMutex,
}

#[repr(C)]
pub struct ScmPair {
    pub(crate) header: ScmHeader,
    pub(crate) car: ScmValue,
    pub(crate) cdr: ScmValue,
}

impl Object for ScmPair {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.car.trace(visitor);
        self.cdr.trace(visitor);
    }
}

impl Allocation for ScmPair {}

#[repr(C)]
pub struct ScmVector {
    pub(crate) header: ScmHeader,
    pub(crate) len: u32,
    pub(crate) _pad: u32,
    pub(crate) data: [ScmValue; 0],
}

impl ScmVector {
    pub fn len(&self) -> usize {
        self.len as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
}

impl Object for ScmVector {
    fn trace(&self, _visitor: &mut dyn rsgc::prelude::Visitor) {}

    fn trace_range(&self, from: usize, to: usize, visitor: &mut dyn rsgc::prelude::Visitor) {
        for i in from..to {
            self[i].trace(visitor);
        }
    }
}

impl Allocation for ScmVector {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = std::mem::size_of::<ScmValue>();
    const VARSIZE_OFFSETOF_CAPACITY: usize = memoffset::offset_of!(Self, len) as usize;
    const VARSIZE_OFFSETOF_LENGTH: usize = memoffset::offset_of!(Self, len) as usize;
    const VARSIZE_OFFSETOF_VARPART: usize = memoffset::offset_of!(Self, data) as usize;
}

impl Deref for ScmVector {
    type Target = [ScmValue];

    fn deref(&self) -> &Self::Target {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr(), self.len as usize) }
    }
}

impl DerefMut for ScmVector {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { std::slice::from_raw_parts_mut(self.data.as_mut_ptr(), self.len as usize) }
    }
}

impl std::ops::Index<usize> for ScmVector {
    type Output = ScmValue;

    fn index(&self, index: usize) -> &Self::Output {
        &self.data[index]
    }
}

impl std::ops::IndexMut<usize> for ScmVector {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.data[index]
    }
}

#[repr(C)]
pub struct ScmString {
    pub(crate) header: ScmHeader,
    pub(crate) len: u32,
    pub(crate) _pad: u32,
    pub(crate) data: [u8; 0],
}

impl ScmString {
    pub fn len(&self) -> usize {
        self.len as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn as_str(&self) -> &str {
        unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                self.data.as_ptr(),
                self.len as usize - 1,
            ))
        }
    }
}

impl Object for ScmString {
    fn trace(&self, _visitor: &mut dyn rsgc::prelude::Visitor) {}
}

impl Allocation for ScmString {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = std::mem::size_of::<u8>();
    const NO_HEAP_PTRS: bool = true;
    const VARSIZE_NO_HEAP_PTRS: bool = true;
    const VARSIZE_OFFSETOF_CAPACITY: usize = memoffset::offset_of!(Self, len) as usize;
    const VARSIZE_OFFSETOF_LENGTH: usize = memoffset::offset_of!(Self, len) as usize;
    const VARSIZE_OFFSETOF_VARPART: usize = memoffset::offset_of!(Self, data) as usize;
}

impl Deref for ScmString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                self.data.as_ptr(),
                self.len as usize - 1,
            ))
        }
    }
}

impl AsRef<[u8]> for ScmString {
    fn as_ref(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl AsRef<str> for ScmString {
    fn as_ref(&self) -> &str {
        &**self
    }
}

impl std::ops::Index<usize> for ScmString {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.data[index]
    }
}

#[repr(C)]
pub struct ScmSymbol {
    pub(crate) header: ScmHeader,
    pub(crate) len: u32,
    pub(crate) generated: bool,
    pub(crate) uninterned: bool,
    pub(crate) _pad: [u8; 2],
    pub(crate) data: [u8; 0],
}

impl ScmSymbol {
    pub fn len(&self) -> usize {
        self.len as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn as_str(&self) -> &str {
        unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                self.data.as_ptr(),
                self.len as usize - 1,
            ))
        }
    }
}

impl Object for ScmSymbol {
    fn trace(&self, _visitor: &mut dyn rsgc::prelude::Visitor) {}
}

impl Allocation for ScmSymbol {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = std::mem::size_of::<u8>();
    const NO_HEAP_PTRS: bool = true;
    const VARSIZE_NO_HEAP_PTRS: bool = true;
    const VARSIZE_OFFSETOF_CAPACITY: usize = memoffset::offset_of!(Self, len) as usize;
    const VARSIZE_OFFSETOF_LENGTH: usize = memoffset::offset_of!(Self, len) as usize;
    const VARSIZE_OFFSETOF_VARPART: usize = memoffset::offset_of!(Self, data) as usize;
}

impl Deref for ScmSymbol {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                self.data.as_ptr(),
                self.len as usize - 1,
            ))
        }
    }
}

impl AsRef<[u8]> for ScmSymbol {
    fn as_ref(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr(), self.len as usize - 1) }
    }
}

impl AsRef<str> for ScmSymbol {
    fn as_ref(&self) -> &str {
        &**self
    }
}

#[repr(C)]
pub struct ScmByteVector {
    pub(crate) header: ScmHeader,
    pub(crate) len: u32,
    pub(crate) _pad: u32,
    pub(crate) data: [u8; 0],
}

impl ScmByteVector {
    pub fn len(&self) -> usize {
        self.len as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
}

impl Object for ScmByteVector {
    fn trace(&self, _visitor: &mut dyn rsgc::prelude::Visitor) {}
}

impl Allocation for ScmByteVector {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = std::mem::size_of::<u8>();
    const NO_HEAP_PTRS: bool = true;
    const VARSIZE_NO_HEAP_PTRS: bool = true;
    const VARSIZE_OFFSETOF_CAPACITY: usize = memoffset::offset_of!(Self, len) as usize;
    const VARSIZE_OFFSETOF_LENGTH: usize = memoffset::offset_of!(Self, len) as usize;
    const VARSIZE_OFFSETOF_VARPART: usize = memoffset::offset_of!(Self, data) as usize;
}

impl Deref for ScmByteVector {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr(), self.len as usize) }
    }
}

impl DerefMut for ScmByteVector {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { std::slice::from_raw_parts_mut(self.data.as_mut_ptr(), self.len as usize) }
    }
}

impl AsRef<[u8]> for ScmByteVector {
    fn as_ref(&self) -> &[u8] {
        self
    }
}

impl AsMut<[u8]> for ScmByteVector {
    fn as_mut(&mut self) -> &mut [u8] {
        self
    }
}

impl std::ops::Index<usize> for ScmByteVector {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.data[index]
    }
}

impl std::ops::IndexMut<usize> for ScmByteVector {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.data[index]
    }
}

#[repr(C)]
pub struct ScmChar {
    pub(crate) header: ScmHeader,
    pub(crate) value: char,
}

impl ScmChar {
    pub fn value(&self) -> char {
        self.value
    }
}

impl Object for ScmChar {
    fn trace(&self, _visitor: &mut dyn rsgc::prelude::Visitor) {}
}

impl Allocation for ScmChar {
    const VARSIZE: bool = false;
    const VARSIZE_ITEM_SIZE: usize = 0;
    const NO_HEAP_PTRS: bool = true;
    const VARSIZE_NO_HEAP_PTRS: bool = true;
    const VARSIZE_OFFSETOF_CAPACITY: usize = 0;
    const VARSIZE_OFFSETOF_LENGTH: usize = 0;
    const VARSIZE_OFFSETOF_VARPART: usize = 0;
}

impl Deref for ScmChar {
    type Target = char;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl AsRef<char> for ScmChar {
    fn as_ref(&self) -> &char {
        &self.value
    }
}
