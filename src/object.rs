use std::{mem::{size_of, offset_of}, hash::Hash, ops::{Deref, DerefMut}, fmt::{Display, Debug, Formatter}};

use rsgc::{prelude::{Handle, Object, Allocation}, system::collections::hashmap::HashMap};

use crate::value::Value;

#[repr(C)]
pub struct ObjectHeader {
    pub(crate) typ: Type,

}

impl ObjectHeader {
    #[inline(always)]
    pub(crate) const fn new(typ: Type) -> Self {
        Self { typ }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Type {
    Null,
    Undefined,
    True,
    False,
    Int32,
    Double,

    BigNum,
    Complex,
    Rational,
    Str,
    Vector,
    Pair,
    Bytevector,
    Identifier,
    Symbol,
    Module,
    GLOC,


}

#[repr(C)]
pub struct Pair {
    pub(crate) object: ObjectHeader,
    pub(crate) car: Value,
    pub(crate) cdr: Value,
}

impl Object for Pair {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.car.trace(visitor);
        self.cdr.trace(visitor);
    }
}

impl Allocation for Pair {}

#[repr(C)]
pub struct Vector {
    pub(crate) object: ObjectHeader,
    pub(crate) length: u32,
    pub(crate) _pad: u32,
    pub(crate) data: [Value; 0],
}

impl Object for Vector {
    fn trace_range(&self, from: usize, to: usize, visitor: &mut dyn rsgc::prelude::Visitor) {
        for i in from..to {
            unsafe {
                self.data.get_unchecked(i).trace(visitor);
            }
        }
    }
}

impl Allocation for Vector {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = size_of::<Value>();
    const VARSIZE_NO_HEAP_PTRS: bool = false;
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(Vector, length);
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(Vector, length);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(Vector, data);
}

#[repr(C)]
pub struct Bytevector {
    pub(crate) object: ObjectHeader,
    pub(crate) length: u32,
    pub(crate) _pad: u32,
    pub(crate) data: [u8; 0],
}

impl Object for Bytevector {}

impl Allocation for Bytevector {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = size_of::<u8>();
    const VARSIZE_NO_HEAP_PTRS: bool = true;
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(Bytevector, length);
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(Bytevector, length);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(Bytevector, data);
}

#[repr(C)]
pub struct Identifier {
    pub(crate) object: ObjectHeader,
    pub(crate) name: Value,
    pub(crate) env: Value,
    pub(crate) module: Value,
}

impl Object for Identifier {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.name.trace(visitor);
        self.env.trace(visitor);
        self.module.trace(visitor);
    }
}

impl Allocation for Identifier {}

#[repr(C)]
pub struct Symbol {
    pub(crate) object: ObjectHeader,
    pub(crate) length: u32,
    pub(crate) generated: bool,
    pub(crate) interned: bool,
    pub(crate) _pad: [u8; 2],
    pub(crate) data: [u8; 0],
}

impl Object for Symbol {}

impl Allocation for Symbol {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = size_of::<u8>();
    const VARSIZE_NO_HEAP_PTRS: bool = true;
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(Symbol, length);
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(Symbol, length);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(Symbol, data);
}

#[repr(C)]
pub struct BigNum {
    pub(crate) object: ObjectHeader,
    pub(crate) length: u32,
    pub(crate) _pad: u32,
    pub(crate) data: [u32; 0],
}

impl Object for BigNum {}

impl Allocation for BigNum {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = size_of::<u32>();
    const VARSIZE_NO_HEAP_PTRS: bool = true;
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(BigNum, length);
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(BigNum, length);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(BigNum, data);
}

#[repr(C)]
pub struct Complex {
    pub(crate) object: ObjectHeader,
    pub(crate) real: Value,
    pub(crate) imag: Value,
}

impl Object for Complex {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.real.trace(visitor);
        self.imag.trace(visitor);
    }
}

impl Allocation for Complex {}

#[repr(C)]
pub struct Rational {
    pub(crate) object: ObjectHeader,
    pub(crate) num: Value,
    pub(crate) den: Value,
}

impl Object for Rational {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.num.trace(visitor);
        self.den.trace(visitor);
    }
}

impl Allocation for Rational {}

#[repr(C)]
pub struct Str {
    pub(crate) object: ObjectHeader,
    pub(crate) length: u32,
    pub(crate) _pad: u32,
    pub(crate) data: [u8; 0],
}



impl Object for Str {}
impl Allocation for Str {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = size_of::<u8>();
    const VARSIZE_NO_HEAP_PTRS: bool = true;
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(Str, length);
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(Str, length);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(Str, data);
}

impl Deref for Str {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        unsafe {
            let ptr = self.data.as_ptr();
            let len = self.length as usize;
            let slice = std::slice::from_raw_parts(ptr, len);
            std::str::from_utf8_unchecked(slice)
        }
    }
}

impl AsRef<str> for Str {
    fn as_ref(&self) -> &str {
        self.deref()
    }
}

impl AsRef<[u8]> for Str {
    fn as_ref(&self) -> &[u8] {
        unsafe {
            let ptr = self.data.as_ptr();
            let len = self.length as usize;
            std::slice::from_raw_parts(ptr, len)
        }
    }
}

impl Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        unsafe {
            let ptr = self.data.as_ptr();
            let len = self.length as usize;
            let slice = std::slice::from_raw_parts(ptr, len);
            std::str::from_utf8_unchecked(slice)
        }
    }
}

impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        self.deref()
    }
}

impl AsRef<[u8]> for Symbol {
    fn as_ref(&self) -> &[u8] {
        unsafe {
            let ptr = self.data.as_ptr();
            let len = self.length as usize;
            std::slice::from_raw_parts(ptr, len)
        }
    }
}

impl Deref for Bytevector {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        unsafe {
            let ptr = self.data.as_ptr();
            let len = self.length as usize;
            std::slice::from_raw_parts(ptr, len)
        }
    }
}

impl AsRef<[u8]> for Bytevector {
    fn as_ref(&self) -> &[u8] {
        self.deref()
    }
}

impl DerefMut for Bytevector {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            let ptr = self.data.as_mut_ptr();
            let len = self.length as usize;
            std::slice::from_raw_parts_mut(ptr, len)
        }
    }
}

impl AsMut<[u8]> for Bytevector {
    fn as_mut(&mut self) -> &mut [u8] {
        self.deref_mut()
    }
}

impl Deref for Vector {
    type Target = [Value];

    fn deref(&self) -> &Self::Target {
        unsafe {
            let ptr = self.data.as_ptr();
            let len = self.length as usize;
            std::slice::from_raw_parts(ptr, len)
        }
    }
}

impl AsRef<[Value]> for Vector {
    fn as_ref(&self) -> &[Value] {
        self.deref()
    }
}

impl DerefMut for Vector {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            let ptr = self.data.as_mut_ptr();
            let len = self.length as usize;
            std::slice::from_raw_parts_mut(ptr, len)
        }
    }
}

impl AsMut<[Value]> for Vector {
    fn as_mut(&mut self) -> &mut [Value] {
        self.deref_mut()
    }
}

impl Hash for Str {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let str: &str = self.deref();
        str.hash(state)
    }
}

impl Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const Symbol).hash(state)
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        (self as *const Symbol) == (other as *const Symbol)
    }
}

impl PartialEq<str> for Symbol {
    fn eq(&self, other: &str) -> bool {
        let str1: &str = self.deref();
        str1 == other
    }
}

impl Eq for Symbol {}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str: &str = self.deref();
        write!(f, "{}", str)
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str: &str = self.deref();
        write!(f, "{}", str)
    }
}

impl Display for Str {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str: &str = self.deref();
        write!(f, "{}", str)
    }
}

impl Debug for Str {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str: &str = self.deref();
        write!(f, "{}", str)
    }
}

impl PartialEq for Str {
    fn eq(&self, other: &Self) -> bool {
        let str1: &str = self.deref();
        let str2: &str = other.deref();

        str1 == str2
    }
}


/// A module keeps "toplevel environment", which maps names of free
/// variables (symbols) to a location (GLOCs).
#[repr(C)]
pub struct Module {
    pub(crate) object: ObjectHeader,
    pub(crate) name: Value,
    /// List of imported modules.
    pub(crate) imported: Value,
    pub(crate) export_all: bool,
    pub(crate) parents: Value,
    pub(crate) mpl: Value,
    pub(crate) depended: Value,
    pub(crate) internal: Handle<HashMap<Handle<Symbol>, Value>>,
    pub(crate) external: Handle<HashMap<Handle<Symbol>, Value>>,
    pub(crate) origin: Value,
    pub(crate) prefix: Value,
    pub(crate) info: Value,
    pub(crate) sealed: bool,
}

impl Object for Module {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.name.trace(visitor);
        self.imported.trace(visitor);
        self.parents.trace(visitor);
        self.mpl.trace(visitor);
        self.depended.trace(visitor);
        self.internal.trace(visitor);
        self.external.trace(visitor);
        self.origin.trace(visitor);
        self.prefix.trace(visitor);
        self.info.trace(visitor);
    }
}

impl Allocation for Module {}

#[repr(C)]
pub struct GLOC {
    pub(crate) object: ObjectHeader,
    pub(crate) name: Value,
    pub(crate) module: Value,
    pub(crate) value: Value,
    pub(crate) hidden: bool,
    pub(crate) getter: Option<fn(Handle<GLOC>) -> Value>,
    pub(crate) setter: Option<fn(Handle<GLOC>, Value)>,
}

impl Object for GLOC {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.name.trace(visitor);
        self.module.trace(visitor);
        self.value.trace(visitor);
    }
}

impl Allocation for GLOC {}


/* 
#[repr(C)]
pub struct Class {
    pub(crate) object: ObjectHeader,
    pub(crate) parent: Option<Handle<Class>>,
    pub(crate) name: Handle<Str>,
    /// Offset of the first field in the instance. It is calculated 
    /// as the sum of the sizes of all fields in the parent classes: 
    /// ```text
    /// class.field_start = parent.field_start + parent.fields.len()
    /// ```
    pub(crate) field_start: u16,
    /// Array of strings. String at index N is the name of the field at index `N + field_start` in the instance.
    pub(crate) fields: HashMap<Handle<Str>, u16>,
    /// Map of method names to method bodies.
    pub(crate) methods: HashMap<Handle<Str>, u32>,
    pub(crate) method_bodies: Handle<Array>,
    pub(crate) static_methods: HashMap<Handle<Str>, u32>,
    pub(crate) static_method_bodies: Handle<Array>,
}

impl Object for Class {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        if let Some(parent) = &self.parent {
            parent.trace(visitor);
        }

        self.name.trace(visitor);
        self.fields.trace(visitor);
        self.methods.trace(visitor);
        self.method_bodies.trace(visitor);
        self.static_methods.trace(visitor);
        self.static_method_bodies.trace(visitor);
    }
}

impl Allocation for Class {}

#[repr(C)]
pub struct Str {
    pub(crate) object: ObjectHeader,
    pub(crate) length: u32,
    pub(crate) _pad: u32,
    pub(crate) data: [u8; 0],
}

impl Object for Str {}
impl Allocation for Str {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = size_of::<u8>();
    const VARSIZE_NO_HEAP_PTRS: bool = true;
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(Str, length);
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(Str, length);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(Str, data);
}

impl Hash for Str {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let str: &str = self;
        str.hash(state);
    }
}

impl AsRef<str> for Str {
    fn as_ref(&self) -> &str {
        self
    }
}

impl Deref for Str {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(self.data.as_ptr(), self.length as usize))
        }
    }
}

impl PartialEq for Str {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl Eq for Str {}

#[repr(C)]
pub struct Array {
    pub(crate) object: ObjectHeader,
    pub(crate) length: u32,
    pub(crate) _pad: u32,
    pub(crate) data: [Value; 0],
}

impl Object for Array {
    fn trace_range(&self, from: usize, to: usize, visitor: &mut dyn rsgc::prelude::Visitor) {
        let ptr = self.data.as_ptr();
        for i in from..to {
            unsafe {
                (*ptr.add(i)).trace(visitor);
            }
        }
    }
}


impl Allocation for Array {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = size_of::<Value>();
    const VARSIZE_NO_HEAP_PTRS: bool = false;
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(Array, length);
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(Array, length);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(Array, data);
}

#[repr(C)]
pub struct Instance {
    pub(crate) object: ObjectHeader,
    /// Number of fields in the instance. Exist here only for RSGC
    /// to be able to trace the fields.
    pub(crate) length: u32,
    pub(crate) fields: [Value; 0],
}

impl Object for Instance {
    fn trace_range(&self, from: usize, to: usize, visitor: &mut dyn rsgc::prelude::Visitor) {
        let ptr = self.fields.as_ptr();
        for i in from..to {
            unsafe {
                (*ptr.add(i)).trace(visitor);
            }
        }
    }
}

impl Allocation for Instance {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = size_of::<Value>();
    const VARSIZE_NO_HEAP_PTRS: bool = false;
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(Instance, length);
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(Instance, length);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(Instance, fields);
}*/