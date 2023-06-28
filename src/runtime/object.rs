use std::{
    fmt::{Debug, Display, Formatter},
    hash::Hash,
    mem::{offset_of, size_of},
    ops::{Deref, DerefMut, Index, IndexMut, Try, FromResidual},
};

use rsgc::utils::bitfield::BitField;
use rsgc::{
    prelude::{Allocation, Handle, Object},
    system::{array::Array, collections::hashmap::HashMap},
    thread::Thread,
};

use crate::{
    compile::IForm, runtime::string::make_string, runtime::value::Value, vm::{callframe::CallFrame, scm_vm},
};

pub const EXTENDED_PAIR_BIT: usize = 0;
pub const EXTENDED_PAIR_BIT_SIZE: usize = 1;
pub type ExtendedPairBitfield = BitField<EXTENDED_PAIR_BIT_SIZE, EXTENDED_PAIR_BIT, false>;

#[repr(C)]
pub struct ObjectHeader {
    pub(crate) typ: Type,
    pub(crate) flags: u32,
}

impl ObjectHeader {
    #[inline(always)]
    pub(crate) const fn new(typ: Type) -> Self {
        Self { typ, flags: 0 }
    }

    #[inline(always)]
    pub const fn is_extended_pair(&self) -> bool {
        ExtendedPairBitfield::decode(self.flags as u64) != 0
    }

    pub fn set_extended_pair(&mut self, value: bool) {
        self.flags = ExtendedPairBitfield::update(value as _, self.flags as u64) as u32;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum Type {
    Null = 0,
    Undefined,
    True,
    False,
    Int32,
    Double,
    BigNum,
    Rational,
    Complex,
    Str,
    Vector,
    Values,
    Pair,
    Bytevector,
    Identifier,
    Symbol,
    Module,
    GLOC,
    Syntax,
    Macro,
    SyntaxRules,
    ReaderReference,
    LVar,
    Synrules,
    Synpattern,
    Pvref,
    Box,
    CodeBlock,
    Procedure,
    NativeProcedure,
    ClosedNativeProcedure,
    Struct,
    StructType,
    StructProperty,
    Parameter,
}

#[repr(C)]
pub struct ExtendedPair {
    pub(crate) object: ObjectHeader,
    pub(crate) car: Value,
    pub(crate) cdr: Value,
    pub(crate) attr: Value,
}

impl Object for ExtendedPair {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.car.trace(visitor);
        self.cdr.trace(visitor);
        self.attr.trace(visitor);
    }
}

impl Allocation for ExtendedPair {}

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

impl Str {
    pub fn as_str(&self) -> &Str {
        &*self
    }
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
    pub(crate) setter: Option<fn(Handle<GLOC>, Value) -> Result<(), Value>>,
}

impl Object for GLOC {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.name.trace(visitor);
        self.module.trace(visitor);
        self.value.trace(visitor);
    }
}

impl Allocation for GLOC {}

/// Syntax is a built-in procedure to compile given form.
#[repr(C)]
pub struct Syntax {
    pub(crate) header: ObjectHeader,
    pub(crate) callback: fn(Value, Value) -> Result<Handle<IForm>, Value>,
}

impl Object for Syntax {
    fn trace(&self, _visitor: &mut dyn rsgc::prelude::Visitor) {}
}
impl Allocation for Syntax {}

/// An object to keep unrealized circular reference (e.g. #N=) during
/// 'read'.  It is replaced by the reference value before exiting 'read',
/// and it shouldn't leak out to the normal Scheme program, except the
/// code that handles it explicitly (like read-time constructor).
/// This object is also used in `scm_unwrap_syntax` to track circular structures.
#[repr(C)]
pub struct ReaderReference {
    pub(crate) header: ObjectHeader,
    pub(crate) value: Value,
}

impl Object for ReaderReference {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.value.trace(visitor);
    }
}

impl Allocation for ReaderReference {}

#[repr(C)]
pub struct CodeBlock {
    pub(crate) header: ObjectHeader,
    pub name: Value,
    /// vector of constants
    pub literals: Value,
    /// vector of code blocks
    pub fragments: Handle<Array<Handle<CodeBlock>>>,
    pub(crate) mina: u32,
    pub(crate) maxa: u32,
    pub(crate) code_len: u32,
    pub(crate) code: [u8; 0],
}

impl CodeBlock {
    pub fn start_ip(&self) -> *const u8 {
        self.code.as_ptr()
    }

    pub fn code(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.code.as_ptr(), self.code_len as usize) }
    }
}

impl Index<usize> for CodeBlock {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        unsafe { self.code.get_unchecked(index) }
    }
}

impl Object for CodeBlock {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.name.trace(visitor);
        self.literals.trace(visitor);
        self.fragments.trace(visitor);
    }
}

impl Allocation for CodeBlock {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = 1;
    const VARSIZE_NO_HEAP_PTRS: bool = true;
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(CodeBlock, code_len);
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(CodeBlock, code_len);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(CodeBlock, code);
}

#[repr(C)]
pub struct Procedure {
    pub(crate) header: ObjectHeader,
    pub(crate) name: Value,
    pub code: Handle<CodeBlock>,
    pub(crate) env_size: u32,
    pub(crate) captures: [Value; 0],
}

impl Object for Procedure {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.name.trace(visitor);
        self.code.trace(visitor);
    }

    fn trace_range(&self, from: usize, to: usize, visitor: &mut dyn rsgc::prelude::Visitor) {
        for i in from..to {
            unsafe {
                self.captures.get_unchecked(i).trace(visitor);
            }
        }
    }
}

impl Allocation for Procedure {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = size_of::<Value>();
    const VARSIZE_NO_HEAP_PTRS: bool = false;
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(Procedure, env_size);
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(Procedure, env_size);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(Procedure, captures);
}

pub type ProcedureInliner = fn(&[Handle<IForm>], Value) -> Option<Handle<IForm>>;

#[repr(C)]
pub struct NativeProcedure {
    pub(crate) header: ObjectHeader,
    pub(crate) name: Value,
    pub(crate) mina: u32,
    pub(crate) maxa: u32,
    pub(crate) inliner: Option<ProcedureInliner>,
    pub(crate) callback: extern "C" fn(&mut CallFrame) -> ScmResult,
}

impl Object for NativeProcedure {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.name.trace(visitor);
    }
}

impl Allocation for NativeProcedure {}

#[repr(C)]
pub struct ClosedNativeProcedure {
    pub(crate) header: ObjectHeader,
    pub(crate) name: Value,
    pub(crate) mina: u32,
    pub(crate) maxa: u32,
    pub(crate) inliner: Option<ProcedureInliner>,
    pub(crate) callback: extern "C" fn(&mut CallFrame) -> ScmResult,
    pub(crate) env_size: u32,
    pub(crate) captures: [Value; 0],
}

impl Index<usize> for ClosedNativeProcedure {
    type Output = Value;
    fn index(&self, index: usize) -> &Self::Output {
        assert!(index < self.env_size as usize);
        unsafe { self.captures.get_unchecked(index) }
    }
}

impl IndexMut<usize> for ClosedNativeProcedure {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        assert!(index < self.env_size as usize);
        unsafe { self.captures.get_unchecked_mut(index) }
    }
}

impl Object for ClosedNativeProcedure {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.name.trace(visitor);
    }

    fn trace_range(&self, from: usize, to: usize, visitor: &mut dyn rsgc::prelude::Visitor) {
        for i in from..to {
            unsafe {
                self.captures.get_unchecked(i).trace(visitor);
            }
        }
    }
}

impl Allocation for ClosedNativeProcedure {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = size_of::<Value>();
    const VARSIZE_NO_HEAP_PTRS: bool = false;
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(ClosedNativeProcedure, env_size);
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(ClosedNativeProcedure, env_size);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(ClosedNativeProcedure, captures);
}

#[repr(C)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct ScmResult {
    pub(crate) tag: u8,
    pub(crate) value: Value,
}

impl ScmResult {
    pub const OK: u8 = 0;
    pub const TAIL: u8 = 1;
    pub const ERR: u8 = 2;

    pub fn ok(value: impl Into<Value>) -> Self {
        Self {
            tag: Self::OK,
            value: value.into(),
        }
    }

    pub fn tail(rator: impl Into<Value>, rands: &[Value]) -> Self {
        let vm = scm_vm();
        vm.tail_call(rator.into(), rands)
    }

    pub fn err(value: impl Into<Value>) -> Self {
        Self {
            tag: Self::ERR,
            value: value.into(),
        }
    }

    pub fn is_ok(&self) -> bool {
        self.tag == Self::OK
    }

    pub fn is_tail(&self) -> bool {
        self.tag == Self::TAIL
    }

    pub fn is_err(&self) -> bool {
        self.tag == Self::ERR
    }

    pub fn value(&self) -> Value {
        self.value
    }
}

pub const MAX_ARITY: u32 = 0x7FFFFFFF;

pub fn check_arity(mina: u32, maxa: u32, argc: usize) -> bool {
    if argc < mina as usize {
        false
    } else if argc > maxa as usize {
        false
    } else {
        true
    }
}

pub fn wrong_arity(name: Value, argc: u32, mina: u32, maxa: u32) -> Value {
    let t = Thread::current();

    (if mina == maxa {
        make_string(
            t,
            &format!("{:?}: expected {} arguments, got {}", name, mina, argc),
        )
    } else if maxa == MAX_ARITY {
        make_string(
            t,
            &format!(
                "{:?}: expected at least {} arguments, got {}",
                name, mina, argc
            ),
        )
    } else {
        make_string(
            t,
            &format!(
                "{:?}: expected between {} and {} arguments, got {}",
                name, mina, maxa, argc
            ),
        )
    })
    .into()
}

#[repr(C)]
pub struct Box {
    pub(crate) header: ObjectHeader,
    pub(crate) value: Value,
}

impl Object for Box {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.value.trace(visitor);
    }
}

impl Allocation for Box {}

pub fn make_box(t: &mut Thread, value: Value) -> Value {
    let box_ = t.allocate(Box {
        header: ObjectHeader::new(Type::Box),
        value,
    });
    box_.into()
}

#[repr(C)]
pub struct Macro {
    pub(crate) header: ObjectHeader,
    pub(crate) transformer: Value,
}

impl Object for Macro {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.transformer.trace(visitor);
    }
}

impl Allocation for Macro {}

// implement Try trait for ScmResult

impl Try for ScmResult {
    type Output = Value;
    type Residual = Value;
    
    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self.tag {
            ScmResult::OK => std::ops::ControlFlow::Continue(self.value),
            ScmResult::TAIL => std::ops::ControlFlow::Break(self.value),
            ScmResult::ERR => std::ops::ControlFlow::Break(self.value),
            _ => unreachable!(),
        }
    }

    fn from_output(output: Self::Output) -> Self {
        Self {
            tag: ScmResult::OK,
            value: output,
        }
    }
}

impl FromResidual<Value> for ScmResult {
    fn from_residual(residual: Value) -> Self {
        Self {
            tag: ScmResult::ERR,
            value: residual,
        }
    }
}

impl FromResidual<Result<Value, Value>> for ScmResult {
    fn from_residual(residual: Result<Value, Value>) -> Self {
        match residual {
            Ok(val) => ScmResult::ok(val),
            Err(value) => Self {
                tag: ScmResult::ERR,
                value,
            },
        }
    }
}