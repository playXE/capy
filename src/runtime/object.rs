use std::mem::size_of;

use mmtk::{
    memory_manager::object_reference_write,
    util::{Address, ObjectReference},
    vm::edge_shape::SimpleEdge,
};

use crate::{runtime::value::Value, vm::thread::Thread};

use crate::gc::CapyVM;

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
    GLOC,
    Tuple,
    Bytevector,
    Port,
    Subroutine,
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
    pub type_id: TypeId,
    pub pad: Pad,
    pub flags: u16,
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
            debug_assert_ne!(self.0, ObjectReference::NULL);
            if value.is_object() {
                debug_assert_ne!(value.get_object().0, ObjectReference::NULL);
                object_reference_write(&mut thread.mutator(), self.0, edge, value.get_object().0);
            } else {
                debug_assert!(!value.is_empty());
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

    #[inline]
    pub fn cast_as<T>(&mut self) -> &mut T {
        unsafe { &mut *self.0.to_raw_address().to_mut_ptr() }
    }
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

    pub fn is_subroutine(self) -> bool {
        self.is_object() && self.get_object().header().type_id() == TypeId::Subroutine
    }

    pub fn is_gloc(self) -> bool {
        self.is_object() && self.get_object().header().type_id() == TypeId::GLOC
    }
}

impl Into<Value> for ScmCellRef {
    fn into(self) -> Value {
        Value::encode_object_value(self)
    }
}

pub fn scm_car(cell: Value) -> Value {
    cell.get_object().cast_as::<ScmPair>().car
}

pub fn scm_cdr(cell: Value) -> Value {
    cell.get_object().cast_as::<ScmPair>().cdr
}

#[inline]
pub fn scm_set_car(cell: Value, thread: &mut Thread, value: Value) {
    let mut pair = cell.get_object();
    let pair = pair.cast_as::<ScmPair>();
    if value.is_object() {
        object_reference_write(
            thread.mutator(),
            cell.get_object().0,
            SimpleEdge::from_address(Address::from_ptr(&mut pair.car)),
            value.get_object().0,
        );
    } else {
        pair.car = value;
    }
}

#[inline]
pub fn scm_set_cdr(cell: Value, thread: &mut Thread, value: Value) {
    let mut pair = cell.get_object();
    let pair = pair.cast_as::<ScmPair>();
    if value.is_object() {
        object_reference_write(
            thread.mutator(),
            cell.get_object().0,
            SimpleEdge::from_address(Address::from_ptr(&mut pair.cdr)),
            value.get_object().0,
        );
    } else {
        pair.cdr = value;
    }
}

pub fn scm_vector_ref(vector: Value, index: u32) -> Value {
    debug_assert!(vector.is_vector());
    unsafe {
        debug_assert!(index < vector.get_object().cast_as::<ScmVector>().length as u32);
        vector
            .get_object()
            .cast_as::<ScmVector>()
            .values
            .as_ptr()
            .add(index as usize)
            .read()
    }
}

pub fn scm_vector_ref_mut<'a>(vector: Value, index: u32) -> &'a mut Value {
    debug_assert!(vector.is_vector());
    unsafe {
        debug_assert!(index < vector.get_object().cast_as::<ScmVector>().length as u32);
        &mut *vector
            .get_object()
            .cast_as::<ScmVector>()
            .values
            .as_mut_ptr()
            .add(index as usize)
    }
}

pub fn scm_vector_set(vector: Value, thread: &mut Thread, index: u32, value: Value) {
    debug_assert!(vector.is_vector());
    debug_assert!(index < vector.get_object().cast_as::<ScmVector>().length as u32);
    let mut v = vector.get_object();
    let vec = v.cast_as::<ScmVector>();

    if value.is_object() {
        object_reference_write(
            thread.mutator(),
            vector.get_object().0,
            SimpleEdge::from_address(Address::from_mut_ptr(unsafe {
                vec.values.as_mut_ptr().add(index as usize)
            })),
            value.get_object().0,
        );
    } else {
        unsafe {
            vec.values.as_mut_ptr().add(index as usize).write(value);
        }
    }
}

pub fn scm_bytevector_ref(bytevector: Value, index: u32) -> u8 {
    debug_assert!(bytevector.is_bytevector());
    unsafe {
        debug_assert!(index < bytevector.get_object().cast_as::<ScmBytevector>().length as u32);
        bytevector
            .get_object()
            .cast_as::<ScmBytevector>()
            .data
            .as_ptr()
            .add(index as usize)
            .read()
    }
}

pub fn scm_bytevector_set(bytevector: Value, index: u32, value: u8) {
    unsafe {
        debug_assert!(bytevector.is_bytevector());
        debug_assert!(index < bytevector.get_object().cast_as::<ScmBytevector>().length as u32);
        let mut bvec = bytevector.get_object();
        let bvec = bvec.cast_as::<ScmBytevector>();
        bvec.data.as_mut_ptr().add(index as usize).write(value);
    }
}

pub fn scm_bytevector_as_slice<'a>(bytevector: Value) -> &'a [u8] {
    unsafe {
        debug_assert!(bytevector.is_bytevector());
        let mut bvec = bytevector.get_object();
        let bvec = bvec.cast_as::<ScmBytevector>();
        std::slice::from_raw_parts(bvec.data.as_ptr(), bvec.length)
    }
}

pub fn scm_bytevector_as_slice_mut<'a>(bytevector: Value) -> &'a mut [u8] {
    unsafe {
        debug_assert!(bytevector.is_bytevector());
        let mut bvec = bytevector.get_object();
        let bvec = bvec.cast_as::<ScmBytevector>();
        std::slice::from_raw_parts_mut(bvec.data.as_mut_ptr(), bvec.length)
    }
}

pub fn scm_bytevector_length(bytevector: Value) -> u32 {
    debug_assert!(bytevector.is_bytevector());
    bytevector.get_object().cast_as::<ScmBytevector>().length as _
}

pub fn scm_vector_length(vector: Value) -> u32 {
    debug_assert!(vector.is_vector());
    vector.get_object().cast_as::<ScmVector>().length as _
}

pub fn scm_program_code(program: Value) -> *const u8 {
    debug_assert!(program.is_program());
    program.get_object().cast_as::<ScmProgram>().vcode
}

pub fn scm_program_num_free_vars(program: Value) -> u32 {
    debug_assert!(program.is_program());
    program.get_object().cast_as::<ScmProgram>().nfree as _
}

pub fn scm_program_free_variable(program: Value, index: u32) -> Value {
    debug_assert!(program.is_program());
    unsafe {
        debug_assert!(index < program.get_object().cast_as::<ScmProgram>().nfree as u32);
        program
            .get_object()
            .cast_as::<ScmProgram>()
            .free
            .as_ptr()
            .add(index as usize)
            .read()
    }
}

pub fn scm_program_free_var_mut<'a>(program: Value, index: u32) -> &'a mut Value {
    debug_assert!(program.is_program());
    unsafe {
        debug_assert!(index < program.get_object().cast_as::<ScmProgram>().nfree as u32);
        &mut *program
            .get_object()
            .cast_as::<ScmProgram>()
            .free
            .as_mut_ptr()
            .add(index as usize)
    }
}

pub fn scm_program_set_free_variable(
    program: Value,
    thread: &mut Thread,
    index: u32,
    value: Value,
) {
    debug_assert!(program.is_program());
    let mut prog = program.get_object();
    let prog = prog.cast_as::<ScmProgram>();

    if value.is_object() {
        object_reference_write(
            thread.mutator(),
            program.get_object().0,
            SimpleEdge::from_address(Address::from_mut_ptr(unsafe {
                prog.free.as_mut_ptr().add(index as usize)
            })),
            value.get_object().0,
        );
    } else {
        unsafe {
            prog.free.as_mut_ptr().add(index as usize).write(value);
        }
    }
}

pub fn scm_string_str<'a>(str: Value) -> &'a str {
    unsafe {
        debug_assert!(str.is_string());
        let mut str = str.get_object();
        let str = str.cast_as::<ScmString>();
        std::str::from_utf8_unchecked(std::slice::from_raw_parts(str.name.as_ptr(), str.length))
    }
}

pub fn scm_string_cstr<'a>(str: Value) -> &'a str {
    unsafe {
        debug_assert!(str.is_string());
        let mut str = str.get_object();
        let str = str.cast_as::<ScmString>();
        std::str::from_utf8_unchecked(std::slice::from_raw_parts(
            str.name.as_ptr(),
            str.length + 1,
        ))
    }
}

pub fn scm_symbol_str<'a>(symbol: Value) -> &'a str {
    unsafe {
        debug_assert!(symbol.is_symbol());
        let mut str = symbol.get_object();
        let str = str.cast_as::<ScmSymbol>();
        std::str::from_utf8_unchecked(std::slice::from_raw_parts(
            str.name.as_ptr(),
            str.length as _,
        ))
    }
}

pub fn scm_symbol_cstr<'a>(symbol: Value) -> &'a str {
    unsafe {
        debug_assert!(symbol.is_symbol());

        let mut str = symbol.get_object();
        let str = str.cast_as::<ScmSymbol>();
        std::str::from_utf8_unchecked(std::slice::from_raw_parts(
            str.name.as_ptr(),
            str.length as usize + 1,
        ))
    }
}

pub fn scm_gloc_value(value: Value) -> Value {
    debug_assert!(value.is_gloc());
    value.get_object().cast_as::<ScmGloc>().value
}

pub fn scm_gloc_set(value: Value, thread: &mut Thread, new_value: Value) {
    debug_assert!(value.is_gloc());
    let mut gloc = value.get_object();
    let gloc = gloc.cast_as::<ScmGloc>();
    if new_value.is_object() {
        object_reference_write(
            thread.mutator(),
            value.get_object().0,
            SimpleEdge::from_address(Address::from_mut_ptr(&mut gloc.value)),
            new_value.get_object().0,
        );
    } else {
        gloc.value = new_value;
    }
}

pub fn scm_gloc_name(value: ScmCellRef) -> Value {
    value.slot_ref(2)
}

/// Very simple way to print SCM object
pub fn dump_object(val: Value) -> Result<String, std::fmt::Error> {
    use std::fmt::Write;
    let mut f = String::new();

    if val.is_false() {
        write!(f, "#f")?;
    } else if val.is_true() {
        write!(f, "#t")?;
    } else if val.is_null() {
        write!(f, "'()")?;
    } else if val.is_undefined() {
        write!(f, "#<undefined>")?;
    } else if val.is_int32() {
        write!(f, "{}", val.get_int32())?;
    } else if val.is_double() {
        write!(f, "{}", val.get_double())?;
    } else if val.is_pair() {
        write!(f, "(")?;

        let mut cell = val;
        loop {
            write!(f, "{}", dump_object(scm_car(cell))?)?;
            if scm_cdr(cell).is_pair() {
                write!(f, " ")?;
                cell = scm_cdr(cell);
            } else if scm_cdr(cell).is_null() {
                break;
            } else {
                write!(f, " . {}", dump_object(scm_cdr(cell))?)?;
                break;
            }
        }

        write!(f, ")")?;
    } else if val.is_vector() {
        let cell = val;
        write!(f, "#(")?;

        for i in 0..scm_vector_length(cell) {
            write!(f, "{}", dump_object(scm_vector_ref(cell, i))?)?;
            if i != scm_vector_length(cell) - 1 {
                write!(f, " ")?;
            }
        }

        write!(f, ")")?;
    } else if val.is_symbol() {
        write!(f, "{}", scm_symbol_str(val))?;
    } else if val.is_string() {
        write!(f, "{:?}", scm_string_str(val))?;
    } else {
        write!(f, "#<unknown {:?}>", val)?;
    }

    Ok(f)
}

#[repr(C)]
pub struct ScmPair {
    pub header: ScmCellHeader,
    pub car: Value,
    pub cdr: Value,
}

#[repr(C)]
pub struct ScmVector {
    pub header: ScmCellHeader,
    pub length: usize,
    pub values: [Value; 0],
}

#[repr(C)]
pub struct ScmSymbol {
    pub header: ScmCellHeader,
    pub length: u32,
    pub gensym: bool,
    pub interned: bool,
    pub name: [u8; 0],
}

#[repr(C)]
pub struct ScmString {
    pub header: ScmCellHeader,
    pub length: usize,
    pub name: [u8; 0],
}

#[repr(C)]
pub struct ScmBytevector {
    pub header: ScmCellHeader,
    pub length: usize,
    pub data: [u8; 0],
}

#[repr(C)]
pub struct ScmGloc {
    pub header: ScmCellHeader,
    pub value: Value,
    pub name: Value,
}

#[repr(C)]
pub struct ScmProgram {
    pub header: ScmCellHeader,
    pub vcode: *const u8,
    pub nfree: usize,
    pub free: [Value; 0],
}

#[repr(C)]
pub struct ScmBox {
    pub header: ScmCellHeader,
    pub value: Value,
}

#[repr(C)]
pub struct ScmSubroutine {
    pub header: ScmCellHeader,
    pub name: Value,
    pub callback: extern "C" fn(thread: &mut Thread),
    pub mina: u32,
    pub maxa: u32,
    pub nenv: u32,
    pub env: [Value; 0],
}
