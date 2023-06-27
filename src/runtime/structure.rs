use std::mem::offset_of;
use std::mem::size_of;

use rsgc::prelude::Allocation;
use rsgc::prelude::Handle;
use rsgc::prelude::Object;
use rsgc::system::array::Array;
use rsgc::thread::Thread;

use crate::vm::callframe::CallFrame;

use super::object::*;
use super::string::make_string;
use super::value::*;

#[repr(C)]
pub struct StructProperty {
    pub(crate) header: ObjectHeader,
    pub(crate) can_impersonate: bool,
    pub(crate) name: Value,
    pub(crate) guard: Value,
    pub(crate) supers: Value,
    pub(crate) contract_name: Value,
    pub(crate) module: Value,
}

impl Object for StructProperty {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.name.trace(visitor);
        self.guard.trace(visitor);
        self.supers.trace(visitor);
        self.contract_name.trace(visitor);
        self.module.trace(visitor);
    }
}

impl Allocation for StructProperty {}

#[repr(C)]
pub struct StructType {
    pub(crate) header: ObjectHeader,
    pub(crate) num_slots: i16,
    pub(crate) num_islots: i16,
    pub(crate) name_pos: u16,
    pub(crate) more_flags: i32,
    pub(crate) name: Value,
    pub(crate) accessor: Value,
    pub(crate) mutator: Value,
    pub(crate) uninit_val: Value,
    pub(crate) props: Handle<Array<(Value, Value)>>,
    pub(crate) proc_attr: Value,
    pub(crate) guard: Value,
}

impl Object for StructType {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.name.trace(visitor);
        self.accessor.trace(visitor);
        self.mutator.trace(visitor);
        self.uninit_val.trace(visitor);
        self.props.trace(visitor);
        self.proc_attr.trace(visitor);
        self.guard.trace(visitor);
    }
}

impl Allocation for StructType {}

#[repr(C)]
pub struct Structure {
    pub(crate) header: ObjectHeader,
    pub(crate) type_: Handle<StructType>,
    pub(crate) num_slots: u32,
    pub(crate) slots: [Value; 0],
}

impl Object for Structure {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.type_.trace(visitor);
    }

    fn trace_range(&self, from: usize, to: usize, visitor: &mut dyn rsgc::prelude::Visitor) {
        unsafe {
            for i in from..to {
                let slot = self.slots.get_unchecked(i);
                slot.trace(visitor);
            }
        }
    }
}

impl Allocation for Structure {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = size_of::<Value>();
    const VARSIZE_NO_HEAP_PTRS: bool = false;
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(Structure, num_slots);
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(Structure, num_slots);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(Structure, slots);
}

pub const MAX_STRUCT_FIELD_COUNT: usize = 32768;

pub const STRUCT_TYPE_FLAG_NONFAIL_CONSTRUCTOR: i32 = 0x1;
pub const STRUCT_TYPE_FLAG_SYSTEM_OPAQUE: i32 = 0x2;
pub const STRUCT_TYPE_FLAG_AUTHENTIC: i32 = 0x4;
pub const STRUCT_TYPE_FLAG_SEALED: i32 = 0x8;

impl Value {
    pub fn is_struct_type(self) -> bool {
        self.is_xtype(Type::StructType)
    }

    pub fn is_struct_property(self) -> bool {
        self.is_xtype(Type::StructProperty)
    }

    pub fn is_structure(self) -> bool {
        self.is_xtype(Type::Struct)
    }

    pub fn structure_type(self) -> Handle<StructType> {
        debug_assert!(self.is_structure());
        unsafe { std::mem::transmute(self) }
    }

    pub fn structure(self) -> Handle<Structure> {
        debug_assert!(self.is_structure());
        unsafe { std::mem::transmute(self) }
    }

    pub fn structure_property(self) -> Handle<StructProperty> {
        debug_assert!(self.is_struct_property());
        unsafe { std::mem::transmute(self) }
    }
}

use super::symbol::*;

fn make_name(base: &str, intern: bool) -> Value {
    if intern {
        make_symbol(base, true)
    } else {
        make_string(Thread::current(), base).into()
    }
}

fn type_name(base: &str, intern: bool) -> Value {
    make_name(&format!("struct:{}", base), intern)
}

fn cstr_name(base: &str, intern: bool) -> Value {
    make_name(base, intern)
}

fn cstr_make_name(base: &str, intern: bool) -> Value {
    make_name(&format!("make-{}", base), intern)
}

fn pred_name(base: &str, intern: bool) -> Value {
    make_name(&format!("{}?", base), intern)
}

fn get_name(base: &str, field: &str, intern: bool) -> Value {
    make_name(&format!("{}-{}", base, field), intern)
}

fn set_name(base: &str, field: &str, intern: bool) -> Value {
    make_name(&format!("set-{}-{}!", base, field), intern)
}

fn genget_name(base: &str, intern: bool) -> Value {
    make_name(&format!("{}-ref", base), intern)
}

fn genset_name(base: &str, intern: bool) -> Value {
    make_name(&format!("{}-set!", base), intern)
}

extern "C" fn prop_pred(cfr: &mut CallFrame) -> ScmResult {
    let prop = cfr.callee().closed_native_procedure()[0];

    let v = cfr.argument(0);

    let stype = if v.is_structure() {
        v.structure().type_
    } else if v.is_struct_type() {
        v.structure_type()
    } else {
        return ScmResult::ok(false);
    };

    for i in 0..stype.props.len() {
        if prop == stype.props[i].0 {
            return ScmResult::ok(true);
        }
    }

    ScmResult::ok(false)
}

fn do_prop_accessor(prop: Value, arg: Value) -> Option<Value> {
    let stype = if arg.is_structure() {
        Some(arg.structure().type_)
    } else if arg.is_struct_type() {
        Some(arg.structure_type())
    } else {
        None
    };


    stype.and_then(|stype| {
        for i in (0..stype.props.len()).rev() {
            if stype.props[i].0 == prop {
                return Some(stype.props[i].1);
            }
        }

        None
    })
}