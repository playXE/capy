use std::mem::size_of;

use memoffset::offset_of;
use rsgc::{sync::mutex::RawMutex, prelude::{Handle, Allocation, Object}, system::array::Array};

use crate::{prelude::{Context, Value}, ScmResult};

pub type HashProc = fn(&mut Context, Value, bound: u32) -> ScmResult<u32>;
pub type EquivProc = fn(&mut Context, Value, Value) -> ScmResult<bool>;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum HashTableType {
    Eq,
    Eqv,
    Equal,
    String,
    Generic,
}

pub struct HashTable {
    lock: RawMutex,
    pub(crate) equiv: EquivProc,
    pub(crate) hash: HashProc,
    pub(crate) datum: Handle<HashEntry>,
    pub(crate) handlers: Value,
    pub(crate) typ: HashTableType,
}

impl Object for HashTable {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.datum.trace(visitor);
        self.handlers.trace(visitor);
    }
}
impl Allocation for HashTable {}

#[repr(C)]
pub struct HashEntry {
    pub capacity: u32,
    pub used: u32,
    pub live: u32, 
    pub elts: [Value; 0]
}

impl HashEntry {

}

impl Allocation for HashEntry {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = size_of::<Value>();
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(Self, capacity);
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(Self, used);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(Self, elts);
}
impl Object for HashEntry {
    fn trace_range(&self, from: usize, to: usize, visitor: &mut dyn rsgc::prelude::Visitor) {
        for i in from..to {
            unsafe {
                self.elts.as_ptr().add(i).as_ref().unwrap().trace(visitor);
            }
        }
    }
}