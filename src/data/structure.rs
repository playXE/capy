use std::{mem::size_of, hash::Hash};
use rsgc::prelude::{Handle, Object, Allocation};

use crate::{prelude::{Symbol, Value, Context, Procedure}, utilities::arraylist::ArrayList};


pub struct StructProperty {
    pub(crate) name: Handle<Symbol>,
    pub(crate) guard: Value,
    /// implied properties: listof (cons <prop> <proc>)
    pub(crate) supers: Value, 
    pub(crate) contract_name: Value
}

impl PartialEq for StructProperty {
    fn eq(&self, other: &Self) -> bool {
        self as *const Self == other as *const Self 
    }
}

impl PartialEq<Option<Handle<Self>>> for StructProperty {
    fn eq(&self, other: &Option<Handle<Self>>) -> bool {
        match other {
            Some(other) => self as *const Self == other.as_ptr() as *const Self,
            None => true
        }
    }
}

impl Eq for StructProperty {}

impl Hash for StructProperty {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const Self).hash(state);
    }
}

#[repr(C)]
pub struct StructType {
    name: Handle<Symbol>,
    pub(crate) init_field_cnt: u32,
    pub(crate) accessor_proc: Value,
    pub(crate) mutator_proc: Value,
    pub(crate) predicate_proc: Value,
    pub(crate) constructor_proc: Value,
    pub(crate) super_type: Option<Handle<StructType>>,
    pub(crate) guard: Option<Handle<Procedure>>,
    /// an array of pair of (property, value) pairs
    /// 
    /// todo: convert to a hashmap after certain size
    pub(crate) props: Handle<ArrayList<Value>>,
}


#[repr(C)]
pub struct StructInstance {
    struct_type: Handle<StructType>,
    field_cnt: u32,
    padding: u32,
    values: [Value; 0]
}

impl Allocation for StructInstance {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = size_of::<Value>();
    const VARSIZE_OFFSETOF_CAPACITY: usize = memoffset::offset_of!(Self, field_cnt);
    const VARSIZE_OFFSETOF_LENGTH: usize = memoffset::offset_of!(Self, field_cnt);
    const VARSIZE_OFFSETOF_VARPART: usize = memoffset::offset_of!(Self, values);
}


impl PartialEq for StructType {
    fn eq(&self, other: &Self) -> bool {
        self as *const Self == other as *const Self 
    }
}

impl PartialEq<Option<Handle<Self>>> for StructType {
    fn eq(&self, other: &Option<Handle<Self>>) -> bool {
        match other {
            Some(other) => self as *const Self == other.as_ptr() as *const Self,
            None => true
        }
    }
}

impl Eq for StructType {}

impl Hash for StructType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const Self).hash(state);
    }
}

impl StructType {
    pub fn super_type(&self) -> Option<Handle<StructType>> {
        self.super_type
    }

    pub fn accessor_proc(&self) -> Value {
        self.accessor_proc
    }

    pub fn mutator_proc(&self) -> Value {
        self.mutator_proc
    }

    pub fn init_field_cnt(&self) -> u32 {
        self.init_field_cnt
    }

    pub fn name(&self) -> Handle<Symbol> {
        self.name
    }

    pub fn new(name: Handle<Symbol>, init_field_cnt: u32, constructor_proc: Value, predicate_proc: Value, accessor_proc: Value, mutator_proc: Value, super_type: Option<Handle<StructType>>, props: Handle<ArrayList<Value>>, guard: Option<Handle<Procedure>>) -> Self {
        Self {
            name,
            init_field_cnt,
            constructor_proc,
            predicate_proc,
            accessor_proc,
            mutator_proc,
            super_type,
            props,
            guard
        }
    }

    pub fn field_cnt_for_instance(&self) -> u32 {
        let mut acc = self.init_field_cnt;
        let mut parent = self.super_type;

        while let Some(structure) = parent {
            acc += structure.init_field_cnt();
            parent = structure.super_type();
        }

        acc
    }

    pub fn has_property(&self, prop: Handle<StructProperty>) -> bool {
        self.props.iter().any(|p| p.car().get_handle_of::<StructProperty>() == prop)
    }
}

impl Object for StructType {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.name.trace(visitor);
        self.super_type.trace(visitor);
        self.props.trace(visitor);
        self.guard.trace(visitor);
        self.mutator_proc.trace(visitor);
        self.accessor_proc.trace(visitor);
        self.predicate_proc.trace(visitor);
    }
}

impl Allocation for StructType {}


impl Object for StructProperty {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.name.trace(visitor);
        self.guard.trace(visitor);
        self.supers.trace(visitor);
        self.contract_name.trace(visitor);
    }
}

impl Allocation for StructProperty {}

impl Object for StructInstance {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.struct_type.trace(visitor);
    }

    fn trace_range(&self, from: usize, to: usize, visitor: &mut dyn rsgc::prelude::Visitor) {
        unsafe {
            let values = self.values.as_ptr().add(from);
            let values = std::slice::from_raw_parts(values, to - from);
            for value in values {
                value.trace(visitor);
            }
        }
    }
}


impl StructInstance {
    pub fn new(ctx: &mut Context, struct_type: Handle<StructType>) -> Handle<Self> {
        let field_cnt = struct_type.field_cnt_for_instance();
        unsafe {
            let mut this = ctx.mutator().allocate_varsize::<Self>(field_cnt as _);

            let ptr = this.as_mut_ptr();
            for i in 0..field_cnt {
                (*ptr).values.as_mut_ptr().add(i as _).write(Value::UNDEFINED);
            }
            (*ptr).struct_type = struct_type;
            (*ptr).field_cnt = field_cnt;
            this.assume_init()
        }
    }

    pub fn is_instance_of(&self, struct_type: Handle<StructType>) -> bool {
        let mut parent = Some(self.struct_type);
        while let Some(structure) = parent {
            if structure == struct_type {
                return true;
            }
            parent = structure.super_type();
        }
        false
    }

    pub fn struct_type(&self) -> Handle<StructType> {
        self.struct_type
    }

    pub fn field_ref(&self, index: u32) -> Value {
        unsafe {
            self.values.as_ptr().add(index as _).read()
        }
    }

    pub fn field_set(&mut self, index: u32, value: Value) {
        unsafe {
            self.values.as_mut_ptr().add(index as _).write(value);
        }
    }

    pub fn field_count(&self) -> u32 {
        self.field_cnt
    }
}