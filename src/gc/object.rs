use std::mem::size_of;

use mmtk::{
    memory_manager::object_reference_write,
    util::{Address, ObjectReference},
    vm::edge_shape::SimpleEdge,
};

use crate::{runtime::value::Value, vm::thread::Thread};

use super::CapyVM;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(u16)]
pub enum TypeId {
    True,
    False,
    Null,
    Undefined,
    Int32,
    Double,
    Char,

    // heap obj start
    Pair,
    Vector,
    String,
    Symbol,
    Program,
    Box,
    Tuple,
    Bytevector,
    Port,
    Intrinsic,
    ClosedIntrinsic,
    Rational,
    Complex,
    Bignum,
}

#[cfg(target_pointer_width = "64")]
pub type Pad = [u8; 4];
#[cfg(target_pointer_width = "32")]
pub type Pad = [u8; 0];

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Header {
    type_id: TypeId,
    pad: Pad,
    flags: u16,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub union ScmCellHeader {
    pub as_header: Header,
    pub as_word: u64,
}
impl ScmCellHeader {
    pub fn type_id(self) -> TypeId {
        unsafe { self.as_header.type_id }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd)]
pub struct ScmCellRef(pub ObjectReference);

impl ScmCellRef {
    pub fn from_address(addr: Address) -> Self {
        Self(ObjectReference::from_address::<CapyVM>(addr))
    }

    pub fn to_address(self) -> Address {
        self.0.to_address::<CapyVM>()
    }

    #[inline]
    pub fn slot_ref(self, offset: usize) -> Value {
        unsafe {
            let address = self.0.to_raw_address().add(offset * size_of::<Value>());
            address.load::<Value>()
        }
    }
    #[inline]
    pub fn slot_set(self, thread: &mut Thread, offset: usize, value: Value) {
        unsafe {
            let slot = self.0.to_raw_address().add(offset * size_of::<Value>());
            let edge = SimpleEdge::from_address(slot);

            if value.is_object() {
                object_reference_write(&mut thread.mutator(), self.0, edge, value.get_object().0);
            } else {
                slot.store(value);
            }
        }
    }

    #[inline]
    pub fn raw_load<T: Copy>(self, offset: usize) -> T {
        unsafe {
            let address = self.0.to_raw_address().add(offset);
            address.load::<T>()
        }
    }

    #[inline]
    pub fn raw_store<T>(self, offset: usize, value: T) {
        unsafe {
            let address = self.0.to_raw_address().add(offset);
            address.store(value);
        }
    }

    #[inline]
    pub fn edge(self, offset: usize) -> SimpleEdge {
        SimpleEdge::from_address(self.0.to_raw_address().add(offset))
    }

    #[inline]
    pub fn header(self) -> ScmCellHeader {
        unsafe { self.0.to_raw_address().load::<ScmCellHeader>() }
    }
}

pub fn scm_car(cell: ScmCellRef) -> Value {
    cell.slot_ref(1)
}

pub fn scm_cdr(cell: ScmCellRef) -> Value {
    cell.slot_ref(2)
}

pub fn scm_set_car(cell: ScmCellRef, thread: &mut Thread, value: Value) {
    cell.slot_set(thread, 1, value)
}

pub fn scm_set_cdr(cell: ScmCellRef, thread: &mut Thread, value: Value) {
    cell.slot_set(thread, 2, value)
}


pub fn scm_program_code(program: ScmCellRef) -> *const u32 {
    unsafe {
        std::mem::transmute(program.slot_ref(1))
    }
}

pub fn scm_program_num_free_vars(program: ScmCellRef) -> u32 {
    program.slot_ref(2).get_int32() as _ 
}

pub fn scm_program_free_variable(program: ScmCellRef, index: u32) -> Value {
    program.slot_ref(3 + index as usize)
}

pub fn scm_program_set_free_variable(program: ScmCellRef, thread: &mut Thread, index: u32, value: Value) {
    program.slot_set(thread, 3 + index as usize, value)
}

impl Value {
    pub fn is_pair(self) -> bool {
        self.is_object() && self.get_object().header().type_id() == TypeId::Pair
    }

    pub fn is_vector(self) -> bool {
        self.is_object() && self.get_object().header().type_id() == TypeId::Vector
    }

    pub fn is_string(self) -> bool {
        self.is_object() && self.get_object().header().type_id() == TypeId::String
    }

    pub fn is_symbol(self) -> bool {
        self.is_object() && self.get_object().header().type_id() == TypeId::Symbol
    }

    pub fn is_program(self) -> bool {
        self.is_object() && self.get_object().header().type_id() == TypeId::Program
    }

    pub fn is_box(self) -> bool {
        self.is_object() && self.get_object().header().type_id() == TypeId::Box
    }

    pub fn is_tuple(self) -> bool {
        self.is_object() && self.get_object().header().type_id() == TypeId::Tuple
    }

    pub fn is_bytevector(self) -> bool {
        self.is_object() && self.get_object().header().type_id() == TypeId::Bytevector
    }

    pub fn is_port(self) -> bool {
        self.is_object() && self.get_object().header().type_id() == TypeId::Port
    }

    pub fn is_intrinsic(self) -> bool {
        self.is_object() && self.get_object().header().type_id() == TypeId::Intrinsic
    }

    pub fn is_closed_intrinsic(self) -> bool {
        self.is_object() && self.get_object().header().type_id() == TypeId::ClosedIntrinsic
    }
    
}

impl Into<Value> for ScmCellRef {
    fn into(self) -> Value {
        Value::encode_object_value(self)
    }
}