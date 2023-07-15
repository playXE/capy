use rsgc::prelude::{Allocation, Handle, Object};

use crate::{
    raise_exn,
    runtime::error::wrong_contract,
    vm::{callframe::CallFrame, scm_vm},
};

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
pub enum ForeignType {
    Void,
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
unsafe impl Allocation for Pointer {}

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
        return wrong_contract::<()>("bytevector->pointer", "bytevector?", 0, cfr.argument_count() as _, cfr.arguments()).into();
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

pub fn init_foreign() {
    let module = scm_foreign_module().module();

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
}
