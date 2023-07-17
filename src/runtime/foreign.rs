
use rsgc::prelude::{Allocation, Handle, Object};
use rsgc::system::array::Array;
use rsgc::utils::round_up;
use std::mem::{align_of, size_of, transmute};
use std::ptr::null;
use libffi::low::{ffi_arg, ffi_type};

use crate::runtime::list::{scm_is_list, scm_length};
use crate::runtime::string::make_string;
use crate::runtime::value::scm_uint;
use crate::{
    raise_exn,
    runtime::error::wrong_contract,
    vm::{callframe::CallFrame, scm_vm},
};

use super::number::scm_s32;
use super::{
    arith::scm_is_exact_positive_integer,
    fun::scm_make_subr,
    module::{scm_define, scm_foreign_module},
    number::scm_to_usize,
    object::{Bytevector, ObjectHeader, ScmResult, Type},
    symbol::Intern,
    value::{scm_int, Value},
    vector::make_bytevector_from_raw_parts,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum ForeignType {
    Void = 0,
    Float,
    Double,
    Uint8,
    Int8,
    Uint16,
    Int16,
    Uint32,
    Int32,
    Uint64,
    Int64,
}

#[repr(C)]
pub struct Pointer {
    pub(crate) header: ObjectHeader,
    pub(crate) pointer: *mut (),
}

#[repr(C)]
pub struct PointerWFinalizer {
    pub(crate) header: ObjectHeader,
    pub(crate) pointer: *mut (),
    pub(crate) finalizer: extern "C" fn(*mut ()),
}


impl Drop for PointerWFinalizer {
    fn drop(&mut self) {
        (self.finalizer)(self.pointer);
    }
}

unsafe impl Object for Pointer {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        unsafe {
            visitor.visit_conservative(&self.pointer as *const _ as *const *const u8, 1);
        }
    }
}
unsafe impl Allocation for Pointer {
    const FINALIZE: bool = false;
    const DESTRUCTIBLE: bool = true;
}

impl Drop for Pointer {
    fn drop(&mut self) {

    }
}

unsafe impl Object for PointerWFinalizer {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        unsafe {
            visitor.visit_conservative(&self.pointer as *const _ as *const *const u8, 1);
        }
    }
}
unsafe impl Allocation for PointerWFinalizer {
    const FINALIZE: bool = true;
    const DESTRUCTIBLE: bool = true;
}

impl Value {
    pub fn is_foreign_pointer(self) -> bool {
        self.is_xtype(Type::Pointer)
    }

    pub fn foreign_pointer(self) -> Handle<Pointer> {
        debug_assert!(self.is_foreign_pointer());
        unsafe { std::mem::transmute(self) }
    }

    pub fn foreign_pointer_value(self) -> *mut () {
        self.foreign_pointer().pointer
    }

    pub fn is_cif(self) -> bool {
        self.is_xtype(Type::CIF)
    }

    pub fn cif(self) -> Handle<CIF> {
        debug_assert!(self.is_cif());
        unsafe { std::mem::transmute(self) }
    }
}

scm_symbol!(SYM_VOID, "void");
scm_symbol!(SYM_FLOAT, "float");
scm_symbol!(SYM_DOUBLE, "double");
scm_symbol!(SYM_UINT8, "uint8");
scm_symbol!(SYM_INT8, "int8");
scm_symbol!(SYM_UINT16, "uint16");
scm_symbol!(SYM_INT16, "int16");
scm_symbol!(SYM_UINT32, "uint32");
scm_symbol!(SYM_INT32, "int32");
scm_symbol!(SYM_UINT64, "uint64");
scm_symbol!(SYM_INT64, "int64");
scm_symbol!(SYM_SHORT, "short");
scm_symbol!(SYM_INT, "int");
scm_symbol!(SYM_LONG, "long");
scm_symbol!(SYM_UNSIGNED_SHORT, "unsigned-short");
scm_symbol!(SYM_UNSIGNED_INT, "unsigned-int");
scm_symbol!(SYM_UNSIGNED_LONG, "unsigned-long");
scm_symbol!(SYM_SIZE_T, "size_t");
scm_symbol!(SYM_SSIZE_T, "ssize_t");
scm_symbol!(SYM_PTRDIFF_T, "ptrdiff_t");
scm_symbol!(SYM_INTPTR_T, "intptr_t");
scm_symbol!(SYM_UINTPTR_T, "uintptr_t");
scm_symbol!(SYM_ASTERISK, "*");
scm_symbol!(SYM_NULL, "%null-pointer");
scm_symbol!(NULL_POINTER_ERROR, "null-pointer-error");

extern "C" fn pointer_p(cfr: &mut CallFrame) -> ScmResult {
    let arg = cfr.argument(0);
    ScmResult::ok(Value::encode_bool_value(arg.is_foreign_pointer()))
}

pub fn scm_make_pointer(
    address: Value,
    finalizer: Option<extern "C" fn(*mut ())>,
) -> Result<Value, Value> {
    let c_address;

    if cfg!(target_pointer_width = "32") {
        if address.is_int32() {
            c_address = address.get_int32() as *mut ();
        } else {
            return wrong_contract("make-pointer", "int32?", 0, 1, &[address]);
        }
    } else {
        if address.is_bignum() {
            let bignum = address.bignum().i64().ok_or_else(|| {
                wrong_contract::<()>("make-pointer", "int64?", 0, 1, &[address]).unwrap_err()
            })?;
            c_address = bignum as *mut ();
        } else {
            return wrong_contract("make-pointer", "int64?", 0, 1, &[address]);
        }
    }

    let pointer: Value = if let Some(finalizer) = finalizer {
        scm_vm()
            .mutator()
            .allocate(PointerWFinalizer {
                header: ObjectHeader::new(Type::Pointer),
                pointer: c_address,
                finalizer,
            })
            .into()
    } else {
        scm_vm()
            .mutator()
            .allocate(Pointer {
                header: ObjectHeader::new(Type::Pointer),
                pointer: c_address,
            })
            .into()
    };

    Ok(pointer)
}

extern "C" fn make_pointer_proc(cfr: &mut CallFrame) -> ScmResult {
    let addr = cfr.argument(0);

    scm_make_pointer(addr, None).into()
}

extern "C" fn pointer_address(cfr: &mut CallFrame) -> ScmResult {
    let pointer = cfr.argument(0);
    if !pointer.is_foreign_pointer() {
        return wrong_contract::<()>("pointer-address", "pointer?", 0, 1, &[pointer]).into();
    }

    let address = pointer.foreign_pointer_value() as i64;

    ScmResult::ok(scm_int(address))
}

extern "C" fn scheme_to_pointer(cfr: &mut CallFrame) -> ScmResult {
    let addr = scm_int(cfr.argument(0).get_raw());
    scm_make_pointer(addr, None).into()
}

extern "C" fn scheme_from_pointer(cfr: &mut CallFrame) -> ScmResult {
    let pointer = cfr.argument(0);

    if !pointer.is_foreign_pointer() {
        return wrong_contract::<()>("scheme-from-pointer", "pointer?", 0, 1, &[pointer]).into();
    }

    let address = pointer.foreign_pointer_value() as i64;

    ScmResult::ok(Value(super::value::EncodedValueDescriptor {
        as_int64: address,
    }))
}

pub fn scm_from_pointer(ptr: *mut ()) -> Value {
    scm_vm()
        .mutator()
        .allocate(Pointer {
            header: ObjectHeader::new(Type::Pointer),
            pointer: ptr,
        })
        .into()
}

/// Return btyevector aliasing `size` bytes pointed to by `pointer`.
pub unsafe fn scm_pointer_to_bytevector(
    pointer: Handle<Pointer>,
    size: usize,
) -> Result<Handle<Bytevector>, Value> {
    let ret = make_bytevector_from_raw_parts(scm_vm().mutator(), pointer.pointer.cast(), size);

    Ok(ret)
}

extern "C" fn pointer_to_bytevector_proc(cfr: &mut CallFrame) -> ScmResult {
    let pointer = cfr.argument(0);
    if !pointer.is_pointer() {
        return wrong_contract::<()>("pointer->bytevector", "pointer?", 0, 1, &[pointer]).into();
    }

    let len = cfr.argument(1);
    if !scm_is_exact_positive_integer(len) {
        return wrong_contract::<()>(
            "pointer->bytevector",
            "exact-positive-integer?",
            1,
            2,
            &[pointer, len],
        )
        .into();
    }
    let len = if len.is_bignum() {
        len.bignum().u64().ok_or_else(|| {
            raise_exn!(
                (),
                FailOutOfMemory,
                &[],
                "pointer->bytevector: length too large: {}",
                len
            )
            .unwrap_err()
        })? as usize
    } else {
        len.get_int32() as usize
    };

    let bytevector = unsafe { scm_pointer_to_bytevector(pointer.foreign_pointer(), len) };

    bytevector.into()
}

extern "C" fn bytevector_to_pointer(cfr: &mut CallFrame) -> ScmResult {
    let bv = cfr.argument(0);

    if !bv.is_bytevector() {
        return wrong_contract::<()>(
            "bytevector->pointer",
            "bytevector?",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    let offset = if cfr.argument_count() > 1 {
        scm_to_usize(cfr.argument(1)).ok_or_else(|| {
            wrong_contract::<()>(
                "bytevector->pointer",
                "exact-positive-integer?",
                1,
                2,
                cfr.arguments(),
            )
            .unwrap_err()
        })?
    } else {
        0
    };

    let pointer = unsafe {
        scm_vm().mutator().allocate(Pointer {
            header: ObjectHeader::new(Type::Pointer),
            pointer: bv.bytevector().contents.add(offset) as *mut (),
        })
    };

    ScmResult::ok(pointer)
}

/// Assuming pointer points to a memory region that
/// hols a pointer, return this pointer
extern "C" fn dereference_pointer(cfr: &mut CallFrame) -> ScmResult {
    let pointer = cfr.argument(0);

    if !pointer.is_foreign_pointer() {
        return wrong_contract::<()>("pointer->pointer", "pointer?", 0, 1, &[pointer]).into();
    }

    let pointer = pointer.foreign_pointer_value() as *mut Value;

    let pointer = unsafe { *pointer };

    ScmResult::ok(pointer)
}

extern "C" fn string_to_pointer(cfr: &mut CallFrame) -> ScmResult {
    let string = cfr.argument(0);

    if !string.is_string() {
        return wrong_contract::<()>("string->pointer", "string?", 0, 1, &[string]).into();
    }

    let string = string.string();
    let bytes = string.as_bytes();

    // will be traced conservatively inside `Pointer` object.
    let mut arr = Array::<u8>::zeroed(scm_vm().mutator(), bytes.len() + 1);
    arr[0..bytes.len()].copy_from_slice(bytes);

    let pointer = {
        scm_vm().mutator().allocate(Pointer {
            header: ObjectHeader::new(Type::Pointer),
            pointer: &mut arr[0] as *mut u8 as *mut (),
        })
    };

    ScmResult::ok(pointer)
}

extern "C" fn pointer_to_string(cfr: &mut CallFrame) -> ScmResult {
    let pointer = cfr.argument(0);

    if !pointer.is_foreign_pointer() {
        return wrong_contract::<()>("pointer->string", "pointer?", 0, 1, &[pointer]).into();
    }

    let pointer = pointer.foreign_pointer_value() as *mut u8;

    let mut len = 0;
    while unsafe { *pointer.add(len) } != 0 {
        len += 1;
    }

    let string = unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(pointer, len)) };

    ScmResult::ok(make_string(scm_vm().mutator(), string))
}

pub fn scm_align_of(x: Value) -> Result<usize, Value> {
    if x.is_int32() {
        let x = x.get_int32();

        if x < ForeignType::Float as i32 || x > ForeignType::Int64 as i32 {
            return raise_exn!(Fail, &[], "align-of: invalid type: {}", x);
        }

        let typ: ForeignType = unsafe { std::mem::transmute(x as u8) };

        Ok(match typ {
            ForeignType::Float => align_of::<f32>(),
            ForeignType::Double => align_of::<f64>(),
            ForeignType::Int8 => align_of::<i8>(),
            ForeignType::Int16 => align_of::<i16>(),
            ForeignType::Int32 => align_of::<i32>(),
            ForeignType::Int64 => align_of::<i64>(),
            ForeignType::Uint8 => align_of::<u8>(),
            ForeignType::Uint16 => align_of::<u16>(),
            ForeignType::Uint32 => align_of::<u32>(),
            ForeignType::Uint64 => align_of::<u64>(),
            _ => unreachable!(),
        })
    } else if x == *SYM_ASTERISK {
        Ok(align_of::<usize>())
    } else if x.is_pair() {
        let mut max = 0;

        let mut typ = x;

        while typ.is_pair() {
            let align = scm_align_of(typ.car())?;

            if align > max {
                max = align;
            }

            typ = typ.cdr();
        }

        Ok(max)
    } else {
        raise_exn!(Fail, &[], "align-of: invalid type: {}", x)
    }
}

pub fn scm_size_of(x: Value) -> Result<usize, Value> {
    if x.is_int32() {
        let x = x.get_int32();

        if x < ForeignType::Float as i32 || x > ForeignType::Int64 as i32 {
            return raise_exn!(Fail, &[], "size-of: invalid type: {}", x);
        }

        let typ: ForeignType = unsafe { std::mem::transmute(x as u8) };

        Ok(match typ {
            ForeignType::Float => size_of::<f32>(),
            ForeignType::Double => size_of::<f64>(),
            ForeignType::Int8 => size_of::<i8>(),
            ForeignType::Int16 => size_of::<i16>(),
            ForeignType::Int32 => size_of::<i32>(),
            ForeignType::Int64 => size_of::<i64>(),
            ForeignType::Uint8 => size_of::<u8>(),
            ForeignType::Uint16 => size_of::<u16>(),
            ForeignType::Uint32 => size_of::<u32>(),
            ForeignType::Uint64 => size_of::<u64>(),
            _ => unreachable!(),
        })
    } else if x == *SYM_ASTERISK {
        Ok(size_of::<*mut ()>())
    } else if x.is_pair() {
        // a struct
        let mut off = 0;
        let mut typ = x;
        let align = scm_align_of(typ)?;

        while typ.is_pair() {
            off = round_up(off, scm_align_of(typ.car())?, 0);
            off += scm_size_of(typ.car())?;
            typ = typ.cdr();
        }

        Ok(round_up(off, align, 0))
    } else {
        raise_exn!(Fail, &[], "size-of: invalid type: {}", x)
    }
}

extern "C" fn size_of_proc(cfr: &mut CallFrame) -> ScmResult {
    let x = cfr.argument(0);

    ScmResult::ok(scm_uint(scm_size_of(x)? as u64))
}

extern "C" fn align_of_proc(cfr: &mut CallFrame) -> ScmResult {
    let x = cfr.argument(0);

    ScmResult::ok(scm_uint(scm_align_of(x)? as u64))
}

fn parse_ffi_type(
    mut typ: Value,
    return_p: bool,
    n_structs: &mut usize,
    n_struct_elts: &mut usize,
) -> bool {
    if typ.is_int32() {
        let x = typ.get_int32();
        if x < 0 || x > ForeignType::Int64 as i32 {
            return false;
        } else if x == ForeignType::Void as i32 && !return_p {
            return false;
        }

        return true;
    }

    if typ == *SYM_ASTERISK {
        return true;
    }

    let Some(mut len) = scm_length(typ) else {
        return false;
    };

    while len != 0 {
        len -= 1;

        if !parse_ffi_type(typ.car(), false, n_structs, n_struct_elts) {
            return false;
        }

        *n_struct_elts += 1;
        typ = typ.cdr();
    }

    *n_structs += 1;
    true
}
use crate::op::Opcode;
use crate::runtime::fun::make_closed_procedure;
use crate::runtime::object::CodeBlock;
use libffi::middle::Type as FFIType;


fn make_ffi_type(typ: Value) -> Result<libffi::middle::Type, Value> {
    if typ.is_int32() {
        let x = typ.get_int32();

        if x < 0 || x > ForeignType::Int64 as i32 {
            return raise_exn!(Fail, &[], "size-of: invalid type: {}", x);
        }

        let typ: ForeignType = unsafe { std::mem::transmute(x as u8) };

        Ok(match typ {
            ForeignType::Float => FFIType::f32(),
            ForeignType::Double => FFIType::f64(),
            ForeignType::Int8 => FFIType::i8(),
            ForeignType::Int16 => FFIType::i16(),
            ForeignType::Int32 => FFIType::i32(),
            ForeignType::Int64 => FFIType::i64(),
            ForeignType::Uint8 => FFIType::u8(),
            ForeignType::Uint16 => FFIType::u16(),
            ForeignType::Uint32 => FFIType::u32(),
            ForeignType::Uint64 => FFIType::u64(),
            ForeignType::Void => FFIType::void(),
        })
    } else if typ == *SYM_ASTERISK {
        Ok(FFIType::pointer())
    } else if typ.is_pair() {
        let mut elts = vec![];

        let mut typ = typ;

        while typ.is_pair() {
            elts.push(make_ffi_type(typ.car())?);
            typ = typ.cdr();
        }

        Ok(FFIType::structure(elts))
    } else {
        raise_exn!(Fail, &[], "make-ffi-type: invalid type: {}", typ)
    }
}

#[repr(C)]
pub struct CIF {
    header: ObjectHeader,
    cif: libffi::middle::Cif,
    nargs: u32,
}

fn make_cif(return_type: Value, arg_types: Value, caller: &str) -> Result<Value, Value> {
    let nargs = scm_length(arg_types).ok_or_else(|| {
        raise_exn!(
            (),
            Fail,
            &[],
            "{}: list of arguments expected, got: {}",
            caller,
            arg_types
        )
        .unwrap_err()
    })?;
    if !parse_ffi_type(return_type, true, &mut 0, &mut 0) {
        return raise_exn!(
            Fail,
            &[],
            "{}: invalid return type: {}",
            caller,
            return_type
        );
    }
    let rtype = make_ffi_type(return_type)?;

    let mut walk = arg_types;
    let mut args = Vec::with_capacity(nargs);
    while walk.is_pair() {
        if !parse_ffi_type(walk.car(), false, &mut 0, &mut 0) {
            return raise_exn!(
                Fail,
                &[],
                "{}: invalid argument type {}",
                caller,
                walk.car()
            );
        }
        args.push(make_ffi_type(walk.car())?);
        walk = walk.cdr();
    }

    let cif = scm_vm().mutator().allocate(CIF {
        header: ObjectHeader::new(Type::CIF),
        cif: libffi::middle::Cif::new(args, rtype),
        nargs: nargs as u32,
    });

    Ok(cif.into())
}

unsafe impl Object for CIF {}
unsafe impl Allocation for CIF {
    const FINALIZE: bool = true;
    const DESTRUCTIBLE: bool = true;
}

impl Drop for CIF {
    fn drop(&mut self) {
        
    }
}

fn foreign_call_stub(nargs: u32) -> Handle<CodeBlock> {
    let mut code = vec![];
    code.push(Opcode::EnterBlacklisted as u8); 
    code.push(Opcode::AssertArgCount as u8);
    code.extend_from_slice(&(nargs as u16).to_le_bytes());
    code.push(Opcode::ForeignCall as u8);
    code.extend_from_slice(&(0u16).to_le_bytes());
    code.extend_from_slice(&(1u16).to_le_bytes());
    code.push(Opcode::Return as u8);
    
    let mut cb = scm_vm().mutator().allocate_varsize::<CodeBlock>(code.len());

    unsafe {
        let cb_ptr = cb.as_mut_ptr();
        cb_ptr.write(CodeBlock {
            header: ObjectHeader::new(Type::CodeBlock),
            ranges: None,
            code_len: code.len() as u32,
            fragments: Array::new(scm_vm().mutator(), 0, |_, _| unreachable!()),
            literals: Value::encode_null_value(),
            maxa: nargs,
            mina: nargs,
            mcode: null(),
            name: "<foreign-call-stub>".intern().into(),
            num_vars: 0,
            stack_size: 0,
            code: [],
        });
        (*cb_ptr).code.as_mut_ptr().copy_from_nonoverlapping(code.as_ptr(), code.len());
        cb.assume_init()
    }
}

fn cif_to_procedure(cif: Handle<CIF>, func_ptr: Value) -> Value {
    let cb = foreign_call_stub(cif.nargs);

    let mut proc = make_closed_procedure(scm_vm().mutator(), cb, 2);
    unsafe {
        proc.captures.as_mut_ptr().add(0).write(cif.into());
        proc.captures.as_mut_ptr().add(1).write(func_ptr);
    }

    proc.into()
}



extern "C" fn pointer_to_procedure(cfr: &mut CallFrame) -> ScmResult {
    let return_type = cfr.argument(0);
    let pointer = cfr.argument(1);
    let arg_types = cfr.argument(2);

    if !pointer.is_foreign_pointer() {
        return raise_exn!(
            (),
            Fail,
            &[],
            "pointer->procedure: expected pointer, got {}",
            pointer
        )
        .into();
    }

    if !scm_is_list(arg_types) {
        return raise_exn!(
            (),
            Fail,
            &[],
            "pointer->procedure: expected list of argument types, got {}",
            arg_types
        )
        .into();
    }

    let cif = make_cif(return_type, arg_types, "pointer->procedure")?;

    let proc = cif_to_procedure(cif.cif(), pointer);

    ScmResult::ok(proc)
}
use libffi::low::types;
use crate::runtime::number::{scm_s64, scm_u32, scm_u64};

unsafe fn pack(typ: *mut ffi_type, loc: *mut (), return_value: bool) -> Value {
    let t = (*typ).type_;
    if t == types::void.type_ {
        return Value::encode_undefined_value();
    }

    /* For integer return values smaller than `int', libffi stores the
       result in an `ffi_arg'-long buffer, of which only the
       significant bits must be kept---hence the pair of casts below.
       See <http://thread.gmane.org/gmane.comp.lib.ffi.general/406>
       for details.  */

    if t == types::uint8.type_ {
        if return_value {
            return Value::encode_int32(loc.cast::<ffi_arg>().read() as u8 as i32);
        } else {
            return scm_uint(loc.cast::<u8>().read() as u64);
        }
    }

    if t == types::sint8.type_ {
        if return_value {
            return Value::encode_int32(loc.cast::<ffi_arg>().read() as i8 as i32);
        } else {
            return scm_int(loc.cast::<i8>().read() as i64);
        }
    }

    if t == types::uint16.type_ {
        if return_value {
            return Value::encode_int32(loc.cast::<ffi_arg>().read() as u16 as i32);
        } else {
            return scm_uint(loc.cast::<u16>().read() as u64);
        }
    }

    if t == types::sint16.type_ {
        if return_value {
            return Value::encode_int32(loc.cast::<ffi_arg>().read() as i16 as i32);
        } else {
            return scm_int(loc.cast::<i16>().read() as i64);
        }
    }

    if t == types::uint32.type_ {
        if return_value {
            return Value::encode_int32(loc.cast::<ffi_arg>().read() as u32 as i32);
        } else {
            return scm_uint(loc.cast::<u32>().read() as u64);
        }
    }

    if t == types::sint32.type_ {
        if return_value {
            return Value::encode_int32(loc.cast::<ffi_arg>().read() as i32);
        } else {
            return scm_int(loc.cast::<i32>().read() as i64);
        }
    }

    if t == types::uint64.type_ {
        return scm_uint(loc.cast::<u64>().read());
    }

    if t == types::sint64.type_ {
        return scm_int(loc.cast::<i64>().read());
    }

    if t == types::double.type_ {
        return Value::encode_untrusted_f64_value(loc.cast::<f64>().read());
    }

    if t == types::float.type_ {
        return Value::encode_untrusted_f64_value(loc.cast::<f32>().read() as f64);
    }

    if t == types::pointer.type_ {
        return scm_from_pointer(loc);
    }

    if t == libffi::low::type_tag::STRUCT {
        let mut arr = Array::<u8>::zeroed(scm_vm().mutator(), (*typ).size as usize);
        std::ptr::copy_nonoverlapping(loc.cast::<u8>(), arr.as_mut_ptr(), (*typ).size as usize);
        return scm_from_pointer(&mut arr[0] as *mut u8 as *mut ())
    }

    unreachable!()
}

unsafe fn unpack(typ: *mut ffi_type, val: Value, loc: *mut (), return_value: bool) -> Result<(), Value> {
    let t = (*typ).type_;

    if t == types::void.type_ {
        return Ok(());
    }

    if t == types::float.type_ {
        if !val.is_number() {
            return raise_exn!(Fail, &[], "wrong type of argument for CIF float: {}", val);
        }

        let val = val.get_number();
        loc.cast::<f32>().write(val as f32);
    }

    if t == types::double.type_ {
        if !val.is_number() {
            return raise_exn!(Fail, &[], "wrong type of argument for CIF double: {}", val);
        }   
        let val = val.get_number();
        loc.cast::<f64>().write(val);
    }
    /* For integer return values smaller than `int', libffi expects the
       result in an `ffi_arg'-long buffer.  */
    if t == types::uint8.type_ {
        if !val.is_int32() {
            return raise_exn!(Fail, &[], "wrong type of argument for CIF uint8: {}", val);
        }
        if return_value {
            loc.cast::<ffi_arg>().write(val.get_int32() as u8 as ffi_arg);
        } else {
            loc.cast::<u8>().write(val.get_int32() as u8);
        }
    }

    if t == types::sint8.type_ {
        if !val.is_int32() {
            return raise_exn!(Fail, &[], "wrong type of argument for CIF sint8: {}", val);
        }
        if return_value {
            loc.cast::<ffi_arg>().write(val.get_int32() as i8 as ffi_arg);
        } else {
            loc.cast::<i8>().write(val.get_int32() as i8);
        }
    }

    if t == types::uint16.type_ {
        if !val.is_int32() {
            return raise_exn!(Fail, &[], "wrong type of argument for CIF uint16: {}", val);
        }
        if return_value {
            loc.cast::<ffi_arg>().write(val.get_int32() as u16 as ffi_arg);
        } else {
            loc.cast::<u16>().write(val.get_int32() as u16);
        }
    }

    if t == types::sint16.type_ {
        if !val.is_int32() {
            return raise_exn!(Fail, &[], "wrong type of argument for CIF sint16: {}", val);
        }
        if return_value {
            loc.cast::<ffi_arg>().write(val.get_int32() as i16 as ffi_arg);
        } else {
            loc.cast::<i16>().write(val.get_int32() as i16);
        }
    }

    if t == types::uint32.type_ {
        let val = scm_u32(val).ok_or_else(|| raise_exn!((), Fail, &[], "wrong type of argument for CIF uint32: {}", val).unwrap_err())?;
        if return_value {
            loc.cast::<ffi_arg>().write(val as ffi_arg);
        } else {
            loc.cast::<u32>().write(val as u32);
        }
    }

    if t == types::sint32.type_ {
        let val = scm_s32(val).ok_or_else(|| raise_exn!((), Fail, &[], "wrong type of argument for CIF sint32: {}", val).unwrap_err())?;
        if return_value {
            loc.cast::<ffi_arg>().write(val as ffi_arg);
        } else {
            loc.cast::<i32>().write(val);
        }
    }

    if t == types::uint64.type_ {
        let val = scm_u64(val).ok_or_else(|| raise_exn!((), Fail, &[], "wrong type of argument for CIF uint64: {}", val).unwrap_err())?;
        loc.cast::<u64>().write(val);
    }

    if t == types::sint64.type_ {
        let val = scm_s64(val).ok_or_else(|| raise_exn!((), Fail, &[], "wrong type of argument for CIF sint64: {}", val).unwrap_err())?;
        loc.cast::<i64>().write(val);
    }

    if t == types::pointer.type_ {
        if !val.is_foreign_pointer() {
            return raise_exn!(Fail, &[], "wrong type of argument for CIF pointer: {}", val);
        }

        loc.cast::<*mut ()>().write(val.foreign_pointer_value());
    }

    if t == libffi::low::type_tag::STRUCT {
        if !val.is_foreign_pointer() {
            return raise_exn!(Fail, &[], "wrong type of argument for CIF struct: {}", val);
        }

        let ptr = val.foreign_pointer_value();
        let size = (*typ).size;

        std::ptr::copy_nonoverlapping(ptr.cast::<u8>(), loc.cast::<u8>(), size);
    }

    Ok(())
}
use tinyvec::TinyVec;
pub(crate) fn scm_foreign_call(cif: Value, pointer: Value, cfr: &mut CallFrame) -> Result<Value, Value> {
    let cif = cif.cif();
    let pointer = pointer.foreign_pointer_value();

    let mut args = TinyVec::<[usize; 16]>::with_capacity(cif.nargs as usize);
    let mut arg_size = 0;
    
    unsafe {
        let raw = cif.cif.as_raw_ptr();

        for i in 0..(*raw).nargs {
            let i = i as usize;
            arg_size += (*(*raw).arg_types.add(i).read()).size + (*(*raw).arg_types.add(i).read()).alignment as usize - 1;
        }

        arg_size += (*(*raw).rtype).size + size_of::<usize>().max((*(*raw).rtype).alignment as usize);
        let mut data = TinyVec::<[u8; 128]>::with_capacity(arg_size);
        let data_ptr = data.as_mut_ptr();
        let mut off = 0;
        for i in 0..cif.nargs as usize {
            args.push(round_up(data_ptr.add(off) as usize, (*(*raw).arg_types.add(i).read()).alignment as usize, 0));
            unpack((*raw).arg_types.add(i).read(), cfr.argument(i), args[i] as *mut (), false)?;
            off = args[i] - data_ptr as usize + (*(*raw).arg_types.add(i).read()).size;
        }
        /* Prepare space for the return value.  On some platforms, such as
        `armv5tel-*-linux-gnueabi', the return value has to be at least
        word-aligned, even if its type doesn't have any alignment requirement as is
        the case with `char'.  */
        let rvalue = round_up(data_ptr.add(off) as usize, size_of::<usize>().max((*(*raw).rtype).alignment as usize), 0) as *mut ();

        libffi::raw::ffi_call(raw, Some(transmute(pointer)), rvalue.cast(), args.as_mut_ptr().cast());

        Ok(pack((*raw).rtype, rvalue, true))
    }
}

pub fn init_foreign() {
    let mut module = scm_foreign_module().module();
    module.export_all = true;
    let subr = scm_make_subr("pointer?", pointer_p, 1, 1);
    scm_define(module, "pointer?".intern(), subr).unwrap();

    let subr = scm_make_subr("make-pointer", make_pointer_proc, 1, 1);
    scm_define(module, "make-pointer".intern(), subr).unwrap();

    let subr = scm_make_subr("pointer-address", pointer_address, 1, 1);
    scm_define(module, "pointer-address".intern(), subr).unwrap();

    let subr = scm_make_subr("scheme->pointer", scheme_to_pointer, 1, 1);
    scm_define(module, "scheme->pointer".intern(), subr).unwrap();

    let subr = scm_make_subr("pointer->scheme", scheme_from_pointer, 1, 1);
    scm_define(module, "pointer->scheme".intern(), subr).unwrap();

    let subr = scm_make_subr("pointer->bytevector", pointer_to_bytevector_proc, 2, 2);
    scm_define(module, "pointer->bytevector".intern(), subr).unwrap();

    let subr = scm_make_subr("bytevector->pointer", bytevector_to_pointer, 1, 2);
    scm_define(module, "bytevector->pointer".intern(), subr).unwrap();

    let subr = scm_make_subr("dereference-pointer", dereference_pointer, 1, 1);
    scm_define(module, "dereference-pointer".intern(), subr).unwrap();

    let subr = scm_make_subr("string->pointer", string_to_pointer, 1, 1);
    scm_define(module, "string->pointer".intern(), subr).unwrap();

    let subr = scm_make_subr("pointer->string", pointer_to_string, 1, 1);
    scm_define(module, "pointer->string".intern(), subr).unwrap();

    let subr = scm_make_subr("size-of", size_of_proc, 1, 1);
    scm_define(module, "size-of".intern(), subr).unwrap();

    let subr = scm_make_subr("align-of", align_of_proc, 1, 1);
    scm_define(module, "align-of".intern(), subr).unwrap();

    let subr = scm_make_subr("pointer->procedure", pointer_to_procedure, 3, 3);
    scm_define(module, "pointer->procedure".intern(), subr).unwrap();

    macro_rules! def_syms {
        ($($name: literal => $ty: path),*) => {
            $(
                let val = Value::encode_int32($ty as i32);
                scm_define(module, $name.intern(), val).unwrap();
            )*
        }
    }

    def_syms!(
        "void" => ForeignType::Void,
        "float" => ForeignType::Float,
        "double" => ForeignType::Double,
        "uint8" => ForeignType::Uint8,
        "sint8" => ForeignType::Int8,
        "uint16" => ForeignType::Uint16,
        "sint16" => ForeignType::Int16,
        "uint32" => ForeignType::Uint32,
        "sint32" => ForeignType::Int32,
        "uint64" => ForeignType::Uint64,
        "sint64" => ForeignType::Int64
    );
}
