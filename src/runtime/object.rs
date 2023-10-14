use std::mem::{size_of, transmute};

use mmtk::{
    memory_manager::object_reference_write,
    util::{
        metadata::{header_metadata::HeaderMetadataSpec, MetadataSpec},
        Address, ObjectReference,
    },
    vm::edge_shape::SimpleEdge,
};

use crate::{
    compiler::{tree_il::IForm, P},
    gc::{CapyVM, ObjEdge},
    runtime::value::Value,
    utils::bitfield::BitField,
    vm::thread::Thread,
};

pub type HashStateSpec = BitField<2, 59, false>;
pub type ImmutSpec = BitField<1, 55, false>;

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
    Tuple,
    String,
    Symbol,
    Program,
    Box,
    GLOC,
    Bytevector,
    Port,
    Subroutine,
    Rational,
    Complex,
    Bignum,
    Environment,
    HashTable,
    HashTableRec,
    WeakHashTable,
    WeakHashTableRec,
    WeakMapping,
    SyntaxExpander,
    VMCont,
    Continuation,
    EofObject,
    Frame,
}

pub const HASH_STATE_UNHASHED: u8 = 0;
pub const HASH_STATE_HASHED: u8 = 1;
pub const HASH_STATE_HASHED_AND_MOVED: u8 = 2;
pub const HASHCODE_OFFSET: isize = -(size_of::<usize>() as isize);

#[cfg(target_pointer_width = "64")]
pub type Pad = [u8; 4];
#[cfg(target_pointer_width = "32")]
pub type Pad = [u8; 0];

#[derive(Clone, Copy)]
#[repr(C, packed)]
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
    pub const fn new(type_id: TypeId) -> Self {
        Self {
            as_header: Header {
                type_id,
                pad: [0; 4],
                flags: 0,
            },
        }
    }

    pub fn type_id(self) -> TypeId {
        unsafe { self.as_header.type_id }
    }

    pub fn hash_state(self) -> u8 {
        unsafe { HashStateSpec::decode(self.as_word) as u8 }
    }

    pub fn set_hash_state(&mut self, state: u8) {
        unsafe {
            self.as_word = HashStateSpec::update(state as _, self.as_word);
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd)]
pub struct ScmCellRef(pub usize);

impl ScmCellRef {
    pub fn from_address(addr: Address) -> Self {
        Self(unsafe { transmute(addr) })
    }

    pub fn to_address(self) -> Address {
        unsafe { transmute(self) }
    }
    pub fn object_reference(self) -> ObjectReference {
        ObjectReference::from_address::<CapyVM>(self.to_address())
    }
    #[inline]
    pub fn slot_ref(self, offset: usize) -> Value {
        unsafe {
            let address = self.0 + offset * size_of::<Value>();
            (address as *const Value).read()
        }
    }
    #[inline]
    pub fn slot_set(self, thread: &mut Thread, offset: usize, value: Value) {
        unsafe {
            let slot = self.0 + (offset * size_of::<Value>());
            let edge = ObjEdge::from_address(Address::from_usize(slot)); //ObjEdge::from_address(slot);
            debug_assert_ne!(self.0, 0);
            if value.is_object() {
                debug_assert_ne!(value.get_object().0, 0);
                object_reference_write(
                    &mut thread.mutator(),
                    transmute(self.0),
                    edge,
                    transmute(value),
                );
            } else {
                debug_assert!(!value.is_empty());
                (slot as *mut Value).write(value);
            }
        }
    }

    #[inline]
    pub fn raw_load<T: Copy>(self, offset: usize) -> T {
        unsafe {
            let address = self.0 + offset;
            (address as *const T).read()
        }
    }

    #[inline]
    pub fn raw_store<T>(self, offset: usize, value: T) {
        unsafe {
            let address = self.0 + offset;
            (address as *mut T).write(value);
        }
    }

    #[inline]
    pub fn edge(self, offset: usize) -> SimpleEdge {
        //ObjEdge::from_address(self.0.to_raw_address().add(offset))
        unsafe { transmute(self.0 + offset) }
    }

    #[inline]
    pub fn header(self) -> ScmCellHeader {
        unsafe { (self.0 as *const ScmCellHeader).read() }
    }

    #[inline]
    pub fn cast_as<T>(&mut self) -> &mut T {
        unsafe { &mut *(self.0 as *mut T) }
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

pub fn scm_caar(cell: Value) -> Value {
    scm_car(scm_car(cell))
}

pub fn scm_cdar(cell: Value) -> Value {
    scm_car(scm_cdr(cell))
}

pub fn scm_cadr(cell: Value) -> Value {
    scm_car(scm_cdr(cell))
}

pub fn scm_caddr(cell: Value) -> Value {
    scm_car(scm_cdr(scm_cdr(cell)))
}

pub fn scm_cadddr(cell: Value) -> Value {
    scm_car(scm_cdr(scm_cdr(scm_cdr(cell))))
}

pub fn scm_cadar(cell: Value) -> Value {
    scm_car(scm_cdr(scm_car(cell)))
}

pub fn scm_cdadr(cell: Value) -> Value {
    scm_cdr(scm_car(scm_cdr(cell)))
}

pub fn scm_cdddr(cell: Value) -> Value {
    scm_cdr(scm_cdr(scm_cdr(cell)))
}

#[inline]
pub fn scm_set_car(cell: Value, thread: &mut Thread, value: Value) {
    let mut pair = cell.get_object();
    let pair = pair.cast_as::<ScmPair>();
    if value.is_object() {
        unsafe {
            object_reference_write(
                thread.mutator(),
                transmute(cell.get_object().0),
                transmute(&mut pair.car),
                transmute(value),
            );
        }
    } else {
        pair.car = value;
    }
}

#[inline]
pub fn scm_set_cdr(cell: Value, thread: &mut Thread, value: Value) {
    let mut pair = cell.get_object();
    let pair = pair.cast_as::<ScmPair>();
    if value.is_object() {
        unsafe {
            object_reference_write(
                thread.mutator(),
                transmute(cell.get_object().0),
                transmute(&mut pair.cdr),
                transmute(value),
            );
        }
    } else {
        pair.cdr = value;
    }
}

pub fn scm_vector_ref(vector: Value, index: u32) -> Value {
    debug_assert!(vector.is_vector());
    unsafe {
        debug_assert!(
            index < vector.get_object().cast_as::<ScmVector>().length as u32,
            "index out of bounds: {} >= {}",
            index,
            vector.get_object().cast_as::<ScmVector>().length as u32
        );
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
        unsafe {
            thread.reference_write(
                transmute(vector),
                transmute(vec.values.as_mut_ptr().add(index as usize)),
                transmute(value),
            );
        }
    } else {
        unsafe {
            vec.values.as_mut_ptr().add(index as usize).write(value);
        }
    }
}

pub fn scm_tuple_ref(tuple: Value, index: u32) -> Value {
    debug_assert!(tuple.is_tuple());
    unsafe {
        debug_assert!(
            index < tuple.get_object().cast_as::<ScmTuple>().length as u32,
            "index out of bounds: {} >= {}",
            index,
            tuple.get_object().cast_as::<ScmTuple>().length as u32
        );
        tuple
            .get_object()
            .cast_as::<ScmTuple>()
            .values
            .as_ptr()
            .add(index as usize)
            .read()
    }
}

pub fn scm_tuple_ref_mut<'a>(tuple: Value, index: u32) -> &'a mut Value {
    debug_assert!(tuple.is_tuple());
    unsafe {
        debug_assert!(index < tuple.get_object().cast_as::<ScmTuple>().length as u32);
        &mut *tuple
            .get_object()
            .cast_as::<ScmTuple>()
            .values
            .as_mut_ptr()
            .add(index as usize)
    }
}

pub fn scm_tuple_set(tuple: Value, thread: &mut Thread, index: u32, value: Value) {
    debug_assert!(tuple.is_tuple());
    debug_assert!(index < tuple.get_object().cast_as::<ScmTuple>().length as u32);
    let mut tup = tuple.get_object();
    let tup = tup.cast_as::<ScmTuple>();

    if value.is_object() {
        unsafe {
            thread.reference_write(
                transmute(tuple),
                transmute(tup.values.as_mut_ptr().add(index as usize)),
                transmute(value),
            );
        }
    } else {
        unsafe {
            tup.values.as_mut_ptr().add(index as usize).write(value);
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
            .elems
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
        bvec.elems.add(index as usize).write(value);
    }
}

pub fn scm_bytevector_as_slice<'a>(bytevector: Value) -> &'a [u8] {
    unsafe {
        debug_assert!(bytevector.is_bytevector());
        let mut bvec = bytevector.get_object();
        let bvec = bvec.cast_as::<ScmBytevector>();
        std::slice::from_raw_parts(bvec.elems, bvec.length)
    }
}

pub fn scm_bytevector_as_slice_mut<'a>(bytevector: Value) -> &'a mut [u8] {
    unsafe {
        debug_assert!(bytevector.is_bytevector());
        let mut bvec = bytevector.get_object();
        let bvec = bvec.cast_as::<ScmBytevector>();
        std::slice::from_raw_parts_mut(bvec.elems, bvec.length)
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

pub fn scm_vector_as_slice<'a>(vector: Value) -> &'a [Value] {
    unsafe {
        debug_assert!(vector.is_vector());
        let mut vec = vector.get_object();
        let vec = vec.cast_as::<ScmVector>();
        std::slice::from_raw_parts(vec.values.as_ptr(), vec.length)
    }
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
        debug_assert!(
            index < program.get_object().cast_as::<ScmProgram>().nfree as u32,
            "index out of bounds: {} >= {}",
            index,
            program.get_object().cast_as::<ScmProgram>().nfree as u32
        );
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
        unsafe {
            thread.reference_write(
                transmute(program),
                transmute(prog.free.as_mut_ptr().add(index as usize)),
                transmute(value),
            );
        }
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

pub fn scm_string_mut_str<'a>(str: Value) -> &'a mut str {
    unsafe {
        debug_assert!(str.is_string());
        let mut str = str.get_object();
        let str = str.cast_as::<ScmString>();
        std::str::from_utf8_unchecked_mut(std::slice::from_raw_parts_mut(
            str.name.as_mut_ptr(),
            str.length,
        ))
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
        unsafe {
            object_reference_write(
                thread.mutator(),
                transmute(value),
                transmute(&mut gloc.value),
                //ObjEdge::from_address(Address::from_mut_ptr(&mut gloc.value)),
                transmute(new_value),
            );
        }
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
    } else if val.is_tuple() {
        let t = val.cast_as::<ScmTuple>();
        write!(f, "#<tuple")?;
        for i in 0..t.length {
            let val = scm_tuple_ref(val, i as _);
            write!(f, " {}", dump_object(val)?)?;
        }
        write!(f, ">")?
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
pub struct ScmTuple {
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
    /// Pointer to the first element of the bytevector.
    ///
    /// Can point to `data` field or to some other address, basically to create mapping.
    pub elems: *mut u8,
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
    pub constants: Value,
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

/// Syntax expander expands Scheme into Tree IL.
#[repr(C)]
pub struct ScmSyntaxExpander {
    pub header: ScmCellHeader,
    pub callback: fn(Value, Value) -> P<IForm>,
}

pub enum CleanerType {
    Drop(fn(*mut ())),
    Callback(Box<dyn FnOnce()>),
}

#[repr(C)]
pub struct ScmWeakMapping {
    pub header: ScmCellHeader,
    pub key: Value,
    pub value: Value,
}

pub struct BytevectorMappingBitSpec(MetadataSpec);

impl BytevectorMappingBitSpec {
    pub const LOG_NUM_BITS: usize = 0;
    pub const IS_GLOBAL: bool = false;

    pub const fn in_header(bit_offset: isize) -> Self {
        Self(MetadataSpec::InHeader(HeaderMetadataSpec {
            bit_offset,
            num_of_bits: 1,
        }))
    }

    pub const fn as_spec(&self) -> &MetadataSpec {
        &self.0
    }
}

pub const BYTEVECTOR_MAPPING_BIT_SPEC: BytevectorMappingBitSpec =
    BytevectorMappingBitSpec::in_header(59);

#[inline(never)]
pub fn scm_bytevector_set_mapping(bvec: Value) {
    let obj = bvec.get_object().object_reference();
    let spec = BYTEVECTOR_MAPPING_BIT_SPEC.as_spec();
    unsafe {
        spec.store::<CapyVM, u8>(obj, 1, None);
    }
}
#[inline(never)]
pub fn scm_bytevector_is_mapping(bvec: Value) -> bool {
    let obj = bvec.get_object().object_reference();
    let spec = BYTEVECTOR_MAPPING_BIT_SPEC.as_spec();
    unsafe { spec.load::<CapyVM, u8>(obj, None) != 0 }
}

#[repr(C)]
pub struct ScmRational {
    pub header: ScmCellHeader,
    pub numerator: Value,
    pub denominator: Value,
}

#[repr(C)]
pub struct ScmComplex {
    pub header: ScmCellHeader,
    pub real: Value,
    pub imag: Value,
}

impl Value {
    pub fn is_rational(self) -> bool {
        self.is_object() && self.get_object().header().type_id() == TypeId::Rational
    }

    pub fn is_complex(self) -> bool {
        self.is_object() && self.get_object().header().type_id() == TypeId::Complex
    }

    pub fn get_rational<'a>(self) -> &'a mut ScmRational {
        debug_assert!(self.is_rational());
        self.cast_as::<ScmRational>()
    }

    pub fn get_complex<'a>(self) -> &'a mut ScmComplex {
        debug_assert!(self.is_complex());
        self.cast_as::<ScmComplex>()
    }
}

pub fn scm_rational_numerator(rational: Value) -> Value {
    debug_assert!(rational.is_rational());
    rational.get_object().cast_as::<ScmRational>().numerator
}

pub fn scm_rational_denominator(rational: Value) -> Value {
    debug_assert!(rational.is_rational());
    rational.get_object().cast_as::<ScmRational>().denominator
}

pub fn scm_complex_real(complex: Value) -> Value {
    debug_assert!(complex.is_complex());
    complex.get_object().cast_as::<ScmComplex>().real
}

pub fn scm_complex_imag(complex: Value) -> Value {
    debug_assert!(complex.is_complex());
    complex.get_object().cast_as::<ScmComplex>().imag
}
