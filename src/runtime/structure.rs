#![allow(dead_code)]
use std::{collections::hash_map::RandomState, mem::transmute};

use super::{
    error::*,
    fun::{
        check_proc_arity, get_proc_name, scm_make_closed_native_procedure, scm_make_subr,
        scm_make_subr_closed_inliner, SCM_PRIM_STRUCT_TYPE_CONSTR,
        SCM_PRIM_STRUCT_TYPE_INDEXED_GETTER, SCM_PRIM_STRUCT_TYPE_INDEXED_SETTER,
        SCM_PRIM_STRUCT_TYPE_INDEXLESS_GETTER, SCM_PRIM_STRUCT_TYPE_INDEXLESS_SETTER,
        SCM_PRIM_STRUCT_TYPE_PRED, SCM_PRIM_STRUCT_TYPE_SIMPLE_CONSTR,
    },
    list::{scm_cons, scm_length, scm_list},
    module::{scm_capy_module, scm_define},
    object::ScmResult,
    string::make_string,
    symbol::{make_symbol, Intern},
    vector::{make_values, make_values_n},
};
use crate::{
    compile::{make_iform, Asm, AsmOperand, IForm},
    op::Opcode,
    raise_exn,
    runtime::fun::check_arity,
    vm::{callframe::CallFrame, interpreter::apply, scm_vm},
};
use rsgc::{
    prelude::{Allocation, Handle, Object},
    system::{array::Array, arraylist::ArrayList, collections::hashmap::HashMap},
    thread::Thread,
};

use super::{
    object::{ObjectHeader, Type},
    value::Value,
};

#[repr(C)]
pub struct StructType {
    pub(crate) header: ObjectHeader,
    pub(crate) num_slots: i32,
    pub(crate) num_islots: i32,
    pub(crate) name_pos: i32,
    pub(crate) name: Value,
    pub(crate) accessor: Value,
    pub(crate) mutator: Value,
    pub(crate) uninit_val: Value,
    pub(crate) props: Handle<Array<Value>>,
    pub(crate) guard: Value,
    pub(crate) parent_types: ArrayList<Value>,
}

unsafe impl Object for StructType {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.name.trace(visitor);
        self.accessor.trace(visitor);
        self.mutator.trace(visitor);
        self.uninit_val.trace(visitor);
        self.props.trace(visitor);
        self.guard.trace(visitor);
        self.parent_types.trace(visitor);
    }
}

unsafe impl Allocation for StructType {}

#[repr(C)]
pub struct StructProperty {
    pub(crate) header: ObjectHeader,
    pub(crate) name: Value,
    pub(crate) guard: Value,
    pub(crate) supers: Value,
    pub(crate) contract_name: Value,
}

unsafe impl Object for StructProperty {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.name.trace(visitor);
        self.guard.trace(visitor);
        self.supers.trace(visitor);
        self.contract_name.trace(visitor);
    }
}

unsafe impl Allocation for StructProperty {}

#[repr(C)]
pub struct Structure {
    pub(crate) header: ObjectHeader,
    pub(crate) type_: Handle<StructType>,
    pub(crate) slots: Handle<Array<Value>>,
}

unsafe impl Object for Structure {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.type_.trace(visitor);
        self.slots.trace(visitor);
    }
}

unsafe impl Allocation for Structure {}

pub const STRUCT_NO_TYPE: i32 = 0x01;
pub const STRUCT_NO_CONSTRUCTOR: i32 = 0x02;
pub const STRUCT_NO_PRED: i32 = 0x04;
pub const STRUCT_NO_GET: i32 = 0x08;
pub const STRUCT_NO_SET: i32 = 0x10;
pub const STRUCT_GEN_GET: i32 = 0x20;
pub const STRUCT_GEN_SET: i32 = 0x40;
pub const STRUCT_EXPTIME: i32 = 0x80;
pub const STRUCT_NO_MAKE_PREFIX: i32 = 0x100;
pub const STRUCT_NAMES_AS_STRINGS: i32 = 0x200;

impl Value {
    pub fn is_struct_type(self) -> bool {
        self.is_xtype(Type::StructType)
    }

    pub fn is_struct_property(self) -> bool {
        self.is_xtype(Type::StructProperty)
    }

    pub fn is_struct(self) -> bool {
        self.is_xtype(Type::Struct)
    }

    pub fn struct_type(self) -> Handle<StructType> {
        unsafe {
            debug_assert!(self.is_struct_type());
            transmute(self)
        }
    }

    pub fn struct_property(self) -> Handle<StructProperty> {
        unsafe {
            debug_assert!(self.is_struct_property());
            transmute(self)
        }
    }

    pub fn structure(self) -> Handle<Structure> {
        unsafe {
            debug_assert!(self.is_struct());
            transmute(self)
        }
    }
}

fn apply_guards(stype: Handle<StructType>, args: &[Value]) -> Result<ArrayList<Value>, Value> {
    let mut prev_guards = Value::encode_bool_value(false);
    let mut p = stype.name_pos;
    let vm = scm_vm();
    let argc = args.len();
    let mut args = ArrayList::from_slice_with_capacity(scm_vm().mutator(), args, args.len() + 1);
    args.push(vm.mutator(), Value::encode_null_value());
    let mut guard;
    while p >= 0 {
        if (stype.parent_types[p as usize].struct_type().guard.is_pair()
            || stype.parent_types[p as usize]
                .struct_type()
                .guard
                .is_procedure())
            || !prev_guards.is_false()
        {
            let mut got;

            if prev_guards.is_false() {
                prev_guards = Value::encode_null_value();
            }

            while !prev_guards.is_false() {
                if prev_guards.is_pair() {
                    guard = prev_guards.car();
                } else {
                    guard = stype.parent_types[p as usize].struct_type().guard;
                    if !guard.is_false() {
                        if guard.is_pair() {
                            guard = guard.car();
                        }
                    } else {
                        guard = Value::encode_bool_value(false);
                    }
                }

                if !guard.is_false() {
                    let gcount = stype.parent_types[p as usize].struct_type().num_islots;

                    args[argc] = args[gcount as usize];
                    args[gcount as usize] = stype.name;
                    let v = apply(guard, &args[..gcount as usize + 1])?;

                    got = if v.is_values() { v.values().len() } else { 1 };

                    if got != gcount as usize {
                        return raise_exn!(
                            FailContractArity,
                            &[],
                            "expected {} return value(s) but got {} while calling guard {}",
                            gcount,
                            got,
                            guard
                        );
                    }

                    if v.is_values() {
                        if gcount != 0 {
                            args[0..gcount as usize].copy_from_slice(&v.values());
                        }
                    } else {
                        args[0] = v;
                    }

                    args[gcount as usize] = args[args.len()];
                }

                if prev_guards.is_null() {
                    prev_guards = Value::encode_bool_value(false);
                } else {
                    prev_guards = prev_guards.cdr();
                }
            }
        }

        if stype.parent_types[p as usize].struct_type().guard.is_pair() {
            prev_guards = stype.parent_types[p as usize].struct_type().guard.cdr();
        }

        p -= 1;
    }

    Ok(args)
}

pub fn make_struct_instance_(stype: Value, args: &[Value]) -> Result<Value, Value> {
    let vm = scm_vm();
    let slots = Array::new(
        vm.mutator(),
        stype.struct_type().num_slots as usize,
        |_, _| Value::encode_undefined_value(),
    );
    let mut inst = vm.mutator().allocate(Structure {
        header: ObjectHeader::new(Type::Struct),
        type_: stype.struct_type(),
        slots,
    });

    //let args = apply_guards(stype.struct_type(), args)?;

    let c = stype.struct_type().num_slots as usize;

    let mut j = c;
    let mut i = args.len();

    let mut p = stype.struct_type().name_pos;
    let stype = stype.struct_type();
    while p >= 0 {
        let (mut ns, mut nis): (i32, i32) = if p != 0 {
            (
                stype.parent_types[p as usize].struct_type().num_slots
                    - stype.parent_types[(p as usize) - 1].struct_type().num_slots,
                stype.parent_types[p as usize].struct_type().num_islots
                    - stype.parent_types[(p as usize) - 1]
                        .struct_type()
                        .num_islots,
            )
        } else {
            let parent: Handle<StructType> = stype.parent_types[0usize].struct_type();
            let ns: i32 = parent.num_slots;
            (ns, parent.num_islots)
        };

        ns -= nis;

        // fill in automatic fields
        while ns != 0 {
            j -= 1;
            vm.mutator().write_barrier(inst.slots);
            inst.slots[j] = stype.parent_types[p as usize].struct_type().uninit_val;
            ns -= 1;
        }

        // fill in supplied fields
        while nis > 0 {
            j -= 1;
            i -= 1;
            vm.mutator().write_barrier(inst.slots);
            inst.slots[j] = args[i];
            nis -= 1;
        }
        p -= 1;
    }

    Ok(inst.into())
}

fn make_name(pre: &str, tn: &str, post1: &str, fun: &str, post2: &str, sym: bool) -> Value {
    if sym {
        make_symbol(
            format!("{}{}{}{}{}", pre, tn, post1, fun, post2).as_str(),
            true,
        )
    } else {
        make_string(
            Thread::current(),
            format!("{}{}{}{}{}", pre, tn, post1, fun, post2).as_str(),
        )
        .into()
    }
}

macro_rules! type_name {
    ($base:expr, $sym:expr) => {
        make_name("struct:", $base, "", "", "", $sym)
    };
}

macro_rules! cstr_name {
    ($base:expr, $sym:expr) => {
        make_name("", $base, "", "", "", $sym)
    };
}

macro_rules! cstr_make_name {
    ($base:expr, $sym:expr) => {
        make_name("make-", $base, "", "", "", $sym)
    };
}

macro_rules! pred_name {
    ($base:expr, $sym:expr) => {
        make_name("", $base, "?", "", "", $sym)
    };
}

macro_rules! get_name {
    ($base:expr, $field:expr, $sym:expr) => {
        make_name("", $base, "-", $field, "", $sym)
    };
}

macro_rules! set_name {
    ($base:expr, $field:expr, $sym:expr) => {
        make_name("set-", $base, "-", $field, "!", $sym)
    };
}

macro_rules! genget_name {
    ($base:expr, $sym:expr) => {
        make_name("", $base, "-ref", "", "", $sym)
    };
}

macro_rules! genset_name {
    ($base:expr, $sym:expr) => {
        make_name("set-", $base, "!", "", "", $sym)
    };
}

macro_rules! exptime_name {
    ($base:expr, $sym:expr) => {
        make_name("", $base, "", "", "", $sym)
    };
}
#[allow(unused_macros)]
macro_rules! type_name_str {
    ($base:expr) => {
        make_name("struct:", $base, "", "", "", false)
    };
}

fn do_prop_accessor(prop: Value, arg: Value) -> Option<Value> {
    let stype = if arg.is_struct() {
        arg.structure().type_
    } else if arg.is_struct_type() {
        arg.struct_type()
    } else {
        return None;
    };

    for sprop in stype.props.iter().copied().rev() {
        if sprop.car() == prop {
            return Some(sprop.cdr());
        }
    }

    None
}

extern "C" fn prop_accessor(cfr: &mut CallFrame) -> ScmResult {
    let v = cfr.argument(0);
    let prop = cfr.callee().closed_native_procedure()[0];
    let v = do_prop_accessor(prop, v);

    if let Some(v) = v {
        ScmResult::ok(v)
    } else if cfr.argument_count() == 1 {
        let prop = prop;

        let ctc = if prop.struct_property().contract_name.is_string()
            || prop.struct_property().contract_name.is_symbol()
        {
            prop.struct_property().contract_name.strsym()
        } else {
            prop.struct_property().name.strsym()
        };

        return ScmResult::err(
            wrong_contract::<()>(
                get_proc_name(cfr.callee()).unwrap_or(""),
                ctc,
                0,
                1,
                cfr.arguments(),
            )
            .unwrap_err(),
        );
    } else {
        let v = cfr.argument(1);
        if v.is_procedure() {
            ScmResult::tail(v, &[])
            //vm.tail_apply(v, &[])
        } else {
            ScmResult::ok(v)
        }
    }
}

extern "C" fn prop_pred(cfr: &mut CallFrame) -> ScmResult {
    let v = cfr.argument(0);
    let prop = cfr.callee().closed_native_procedure()[0];

    let stype = if v.is_struct() {
        v.structure().type_
    } else if v.is_struct_type() {
        v.struct_type()
    } else {
        return ScmResult::ok(Value::encode_bool_value(false));
    };

    for sprop in stype.props.iter().copied().rev() {
        if sprop.car() == prop {
            return ScmResult::ok(Value::encode_bool_value(true));
        }
    }

    ScmResult::ok(false)
}

pub(crate) fn make_struct_type_property_raw(
    args: &[Value],
    predout: &mut Value,
    accessout: &mut Value,
) -> Result<Value, Value> {
    let mut supers = Value::encode_null_value();
    let who = "make-struct-type-property";

    if !args[0].is_symbol() {
        return wrong_contract(who, "symbol?", 0, args.len() as _, args);
    }
    let mut a: [Value; 1] = [Value::encode_null_value()];
    let mut accessor_name = None;
    let mut contract_name = None;
    if args.len() > 1 {
        if args[1].to_bool() && !check_proc_arity("", 2, 1, args.len() as _, args)? {
            return wrong_contract(
                who,
                "(or/c (any/c any/c . -> . any) #f)",
                1,
                args.len() as _,
                args,
            );
        }

        if args.len() > 2 {
            supers = args[2];

            if let Some(_) = scm_length(supers) {
                let mut pr = supers;

                while pr.is_pair() {
                    let v = pr.car();
                    if !v.is_pair() {
                        supers = Value::encode_bool_value(false);
                    } else {
                        if v.car().get_type() != Type::StructProperty {
                            supers = Value::encode_bool_value(false);
                        }

                        a[0] = v.cdr();

                        if !check_proc_arity("", 1, 0, 1, &a)? {
                            supers = Value::encode_bool_value(false);
                        }
                    }
                    pr = pr.cdr();
                }
            } else {
                supers = Value::encode_bool_value(false);
            }
            if supers.is_false() {
                return wrong_contract(
                    who,
                    "(listof (cons struct-type-property? (any/c . -> any)))",
                    2,
                    args.len() as _,
                    args,
                );
            }

            if args.len() > 3 {
                if args[3].to_bool() {
                    accessor_name = Some(args[3]);
                    if !accessor_name.unwrap().is_symbol() {
                        return wrong_contract(who, "(or/c symbol? #f)", 3, args.len() as _, args);
                    }
                }

                if args.len() > 4 {
                    if args[4].to_bool() {
                        contract_name = Some(args[4]);
                        if !contract_name.unwrap().is_symbol() {
                            return wrong_contract(
                                who,
                                "(or/c symbol? #f)",
                                4,
                                args.len() as _,
                                args,
                            );
                        }
                    }
                }
            }
        }
    }

    let p = Thread::current().allocate(StructProperty {
        header: ObjectHeader::new(Type::StructProperty),
        name: args[0],
        guard: if args.len() > 1 && args[1].to_bool() {
            args[1]
        } else {
            Value::encode_bool_value(false)
        },
        supers,
        contract_name: contract_name.unwrap_or(Value::encode_bool_value(false)),
    });

    /*StructProperty::new(
        Thread::current(),
        args[0],
        if args.len() > 1 && args[1].is_true() {
            args[1]
        } else {
            Value::make_false()
        },
        supers,
        contract_name.unwrap_or(Value::make_false()),
    );*/

    a[0] = p.into();

    let prop_pred = scm_make_subr_closed_inliner(
        &format!("{}?", args[0]),
        prop_pred,
        1,
        1,
        &[p.into()],
        |iforms, p| {
            if iforms.len() != 1 {
                return None;
            }

            Some(make_iform(IForm::Asm(Asm {
                op: Opcode::StructPropPred,
                args: ArrayList::from_slice(Thread::current(), iforms),
                operands: Some(ArrayList::from_slice(
                    Thread::current(),
                    &[AsmOperand::Constant(p.closed_native_procedure()[0])],
                )),
                exits: false,
                pushes: true,
                ic: false,
            })))
        },
    );

    //Vm::make_closed_procedure(&format!("{}?", name), prop_pred, 1, 1, &[p]);

    let name = if let Some(accessor_name) = accessor_name {
        accessor_name.strsym().to_string()
    } else {
        format!("{}-accessor", args[0])
    };

    *predout = prop_pred;
    *accessout = scm_make_closed_native_procedure(
        Thread::current(),
        make_string(Thread::current(), name).into(),
        prop_accessor,
        1,
        2,
        &[p.into()],
    )
    .into();

    Ok(a[0])
}

extern "C" fn make_struct_type_property(cfr: &mut CallFrame) -> ScmResult {
    let mut a = [Value::encode_null_value(); 3];
    let mut pred = a[0];
    let mut acc = a[0];

    let p = match make_struct_type_property_raw(cfr.arguments(), &mut pred, &mut acc) {
        Ok(v) => v,
        Err(e) => return ScmResult::err(e),
    };

    a[0] = p;
    a[1] = pred;
    a[2] = acc;

    ScmResult::ok(make_values(Thread::current(), &a))
}

pub fn make_struct_type_property_w_guard(name: Value, guard: Value) -> Result<Value, Value> {
    let mut a = [Value::encode_null_value(); 2];
    let mut pred = Value::encode_null_value();
    let mut accessor = Value::encode_null_value();

    a[0] = name;
    a[1] = guard;

    make_struct_type_property_raw(&a, &mut pred, &mut accessor)
}

pub fn make_struct_type_property_(name: Value) -> Result<Value, Value> {
    make_struct_type_property_w_guard(name, false.into())
}

extern "C" fn struct_type_property_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(cfr.argument(0).is_struct_property())
}

pub fn is_struct_type(st: Handle<StructType>, v: Handle<Structure>) -> bool {
    (st.name_pos <= v.type_.name_pos)
        && (st.as_ptr()
            == v.type_.parent_types[st.name_pos as usize]
                .struct_type()
                .as_ptr())
}

pub fn is_struct_instance(typ: Value, v: Value) -> bool {
    if !typ.is_struct_type() {
        return false;
    }

    if !v.is_struct() {
        return false;
    }

    is_struct_type(typ.struct_type(), v.structure())
}

pub fn struct_ref(sv: Value, pos: usize) -> Value {
    sv.structure().slots[pos]
}

pub fn struct_set(sv: Value, pos: usize, v: Value) {
    Thread::current().write_barrier(sv.structure());
    sv.structure().slots[pos] = v;
}

pub fn is_simple_struct_type(stype: Handle<StructType>) -> bool {
    for p in (0..=stype.name_pos).rev() {
        if stype.parent_types[p as usize]
            .struct_type()
            .guard
            .is_procedure()
        {
            return false;
        }

        if stype.parent_types[p as usize].struct_type().num_slots
            != stype.parent_types[p as usize].struct_type().num_islots
        {
            return false;
        }
    }

    true
}

pub fn make_simple_struct_instance_from_array(args: &[Value], typ: Value) -> Value {
    let stype = typ.struct_type();

    let c = stype.num_slots;
    let t = Thread::current();
    let slots = Array::new(t, c as _, |_, i| args[i]);

    /*let s = Structure::new(vm.mutator(), stype, slots);*/

    t.allocate(Structure {
        header: ObjectHeader::new(Type::Struct),
        type_: stype,
        slots,
    })
    .into()
}

pub(crate) extern "C" fn make_simple_struct_instance(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(make_simple_struct_instance_from_array(
        cfr.arguments(),
        cfr.callee().closed_native_procedure()[0],
    ))
}

pub(crate) fn parse_pos(
    who: &str,
    st: Handle<StructType>,
    args: &[Value],
    name: &str,
) -> Result<i32, Value> {
    let mut pos = if !args[1].is_int32() || args[1].get_int32() < 0 {
        return wrong_contract(
            if who.len() == 0 { name } else { who },
            "exact-nonnegative-integer?",
            2,
            args.len() as _,
            args,
        );
    } else {
        args[1].get_int32()
    };

    if (pos < st.num_slots) && (st.name_pos != 0) {
        pos += st.parent_types[st.name_pos as usize - 1]
            .struct_type()
            .num_slots;
    }

    if pos >= st.num_slots {
        let who = if who.len() == 0 { name } else { who };

        let sc = if st.name_pos != 0 {
            st.num_slots
                - st.parent_types[st.name_pos as usize - 1]
                    .struct_type()
                    .num_slots
        } else {
            st.num_slots
        };

        return contract_error(
            who,
            "index too large",
            &[
                &"index",
                &args[1],
                &"maximum allowed index",
                &Value::encode_int32(sc - 1),
                &"structure",
                &args[0],
            ],
        );
    }

    Ok(pos)
}

fn extract_field_proc_name(st_name: Value, vars: &[Value]) -> String {
    let name_info = vars[2];
    let pred_name;
    if name_info.is_symbol() {
        pred_name = name_info.strsym().to_string();
    } else {
        pred_name = format!("{}", st_name.strsym());
    }

    pred_name
}

fn extract_accessor_offset(acc: Value) -> i32 {
    let st = acc.struct_type();

    if st.name_pos != 0 {
        st.parent_types[st.name_pos as usize - 1]
            .struct_type()
            .num_slots
    } else {
        0
    }
}

fn wrong_struct_type(
    vars: &[Value],
    name: &str,
    expected: Value,
    received: Value,
    which: i32,
    argc: i32,
    args: &[Value],
) -> Value {
    let pred_name = extract_field_proc_name(expected, vars);

    if expected == received {
        return contract_error::<()>("contract violation;\n given value instantiates a different structure type with the same name", name, &[
            &"expected", &pred_name,
            &"given", &args[which as usize]
        ]).unwrap_err();
    } else {
        return wrong_contract::<()>(name, &pred_name, which, argc, args).unwrap_err();
    }
}

pub extern "C" fn struct_getter(cfr: &mut CallFrame) -> ScmResult {
    let st = cfr.callee().closed_native_procedure()[0].struct_type();
    let inst = cfr.argument(0);
    let name = cfr.callee().native_procedure().name.strsym();
    if !inst.is_struct() {
        let pred_name = extract_field_proc_name(st.name, &cfr.callee().closed_native_procedure());
        return match wrong_contract::<()>(
            name,
            &pred_name,
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        ) {
            Ok(_) => unreachable!(),
            Err(v) => ScmResult::err(v),
        };
    } else if !is_struct_instance(st.into(), inst) {
        return ScmResult::err(wrong_struct_type(
            &cfr.callee().closed_native_procedure(),
            name,
            st.name,
            inst.structure().type_.name,
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        ));
    }

    let pos = if cfr.arguments().len() == 2 {
        match parse_pos(
            "",
            st,
            cfr.arguments(),
            cfr.callee().native_procedure().name.strsym(),
        ) {
            Ok(p) => p,
            Err(e) => return ScmResult::err(e),
        }
    } else {
        cfr.callee().closed_native_procedure()[1].get_int32()
    };

    ScmResult::ok(struct_ref(inst, pos as _))
}

pub extern "C" fn struct_setter(cfr: &mut CallFrame) -> ScmResult {
    let st = cfr.callee().closed_native_procedure()[0].struct_type();
    let inst = cfr.argument(0);
    let name = cfr.callee().native_procedure().name.strsym();
    if !inst.is_struct() {
        let pred_name = extract_field_proc_name(st.name, &cfr.callee().closed_native_procedure());
        return match wrong_contract::<()>(
            name,
            &pred_name,
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        ) {
            Ok(_) => unreachable!(),
            Err(v) => ScmResult::err(v),
        };
    } else if !is_struct_instance(st.into(), inst) {
        return ScmResult::err(wrong_struct_type(
            &cfr.callee().closed_native_procedure(),
            name,
            st.name,
            inst.structure().type_.name,
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        ));
    }
    let v;
    let pos = if cfr.arguments().len() == 3 {
        v = cfr.argument(2);
        match parse_pos(
            "",
            st,
            cfr.arguments(),
            cfr.callee().native_procedure().name.strsym(),
        ) {
            Ok(p) => p,
            Err(e) => return ScmResult::err(e),
        }
    } else {
        v = cfr.argument(1);
        cfr.callee().closed_native_procedure()[1].get_int32()
    };
    struct_set(inst, pos as _, v);
    ScmResult::ok(Value::encode_undefined_value())
}

extern "C" fn struct_pred(cfr: &mut CallFrame) -> ScmResult {
    let stype = cfr.callee().closed_native_procedure()[0].struct_type();

    ScmResult::ok(is_struct_instance(stype.into(), cfr.argument(0)))
}

extern "C" fn struct_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(cfr.argument(0).is_struct())
}

extern "C" fn struct_type_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(cfr.argument(0).is_struct_type())
}

fn check_struct(who: &str, args: &[Value]) -> Result<(), Value> {
    if !args[0].is_struct_type() {
        return wrong_contract(who, "struct-type?", 0, args.len() as _, args);
    }

    Ok(())
}

extern "C" fn struct_type_pred(cfr: &mut CallFrame) -> ScmResult {
    match check_struct("struct-type-make-predicate", cfr.arguments()) {
        Ok(()) => (),
        Err(e) => return ScmResult::err(e),
    }

    let stype = cfr.argument(0).struct_type();

    let name = format!("{}?", stype.name.strsym());
    let val = make_struct_proc(stype, &name, Value::encode_null_value(), ProcType::Pred, 0);

    ScmResult::ok(val)
}

extern "C" fn struct_type_constr(cfr: &mut CallFrame) -> ScmResult {
    match check_struct("struct-type-make-constructor", cfr.arguments()) {
        Ok(()) => (),
        Err(e) => return ScmResult::err(e),
    }

    let stype = cfr.argument(0).struct_type();
    let args = cfr.arguments();
    let v = if args.len() < 2 || args[1].is_false() {
        format!("make-{}", stype.name.strsym())
    } else if args[1].is_symbol() {
        args[1].strsym().to_string()
    } else {
        return ScmResult::err(
            wrong_contract::<()>(
                "struct-type-make-constructor",
                "symbol?",
                1,
                args.len() as _,
                args,
            )
            .unwrap_err(),
        );
    };
    let val = make_struct_proc(stype, &v, Value::encode_null_value(), ProcType::Constr, 0);

    ScmResult::ok(val)
}

extern "C" fn make_struct_instance(cfr: &mut CallFrame) -> ScmResult {
    match make_struct_instance_(cfr.callee().closed_native_procedure()[0], cfr.arguments()) {
        Ok(v) => ScmResult::ok(v),
        Err(e) => ScmResult::err(e),
    }
}

pub fn make_struct_values(typ: Value, names: &mut [Value], flags: i32) -> Value {
    let stype = typ.struct_type();

    let mut count = names.len();

    if (flags & STRUCT_EXPTIME) != 0 {
        count -= 1;
    }

    let mut values = make_values_n(Thread::current(), count);

    let mut pos = 0;

    if (flags & STRUCT_NO_TYPE) == 0 {
        values[pos] = typ;
        pos += 1;
    }

    if (flags & STRUCT_NO_CONSTRUCTOR) == 0 {
        let nm = names[pos].strsym();

        let vi = make_struct_proc(
            stype,
            nm,
            Value::encode_null_value(),
            ProcType::Constr,
            stype.num_slots,
        );

        values[pos] = vi;
        pos += 1;
    }
    if (flags & STRUCT_NO_PRED) == 0 {
        let nm = names[pos].strsym();

        let vi = make_struct_proc(stype, nm, Value::encode_null_value(), ProcType::Pred, 0);

        values[pos] = vi;
        pos += 1;
    }

    if (flags & STRUCT_GEN_GET) != 0 {
        count -= 1;
    }

    if (flags & STRUCT_GEN_SET) != 0 {
        count -= 1;
    }

    let mut slot_num = if stype.name_pos != 0 {
        stype.parent_types[stype.name_pos as usize - 1]
            .struct_type()
            .num_slots
    } else {
        0
    };
    while pos < count {
        if (flags & STRUCT_NO_GET) == 0 {
            let nm = names[pos].strsym();

            let vi = make_struct_proc(
                stype,
                nm,
                Value::encode_null_value(),
                ProcType::Getter,
                slot_num,
            );
            values[pos] = vi;
            pos += 1;
        }

        if (flags & STRUCT_NO_SET) == 0 {
            let nm = names[pos].strsym();

            let vi = make_struct_proc(
                stype,
                nm,
                Value::encode_null_value(),
                ProcType::Setter,
                slot_num,
            );
            values[pos] = vi;
            pos += 1;
        }

        slot_num += 1;
    }

    if (flags & STRUCT_GEN_GET) != 0 {
        let nm = names[pos].strsym();

        let vi = make_struct_proc(
            stype,
            nm,
            Value::encode_null_value(),
            ProcType::GenGetter,
            slot_num,
        );
        values[pos] = vi;
        pos += 1;
    }

    if (flags & STRUCT_GEN_SET) != 0 {
        let nm = names[pos].strsym();

        let vi = make_struct_proc(
            stype,
            nm,
            Value::encode_null_value(),
            ProcType::GenSetter,
            slot_num,
        );
        values[pos] = vi;
    }

    values.into()
}

fn _make_struct_names(
    base: &str,
    fcount: usize,
    mut field_symbols: Option<Value>,
    field_strings: &[&str],
    flags: i32,
) -> ArrayList<Value> {
    let mut count = 0;

    if (flags & STRUCT_NO_TYPE) == 0 {
        count += 1;
    }

    if (flags & STRUCT_NO_CONSTRUCTOR) == 0 {
        count += 1;
    }

    if (flags & STRUCT_NO_PRED) == 0 {
        count += 1;
    }

    if (flags & STRUCT_GEN_GET) != 0 {
        count += fcount;
    }

    if (flags & STRUCT_GEN_SET) != 0 {
        count += fcount;
    }

    if (flags & STRUCT_NO_GET) == 0 {
        count += 1;
    }

    if (flags & STRUCT_NO_SET) == 0 {
        count += 1;
    }

    let mut names = ArrayList::with_capacity(Thread::current(), count);

    let as_sym = (flags & STRUCT_NAMES_AS_STRINGS) != 0;

    if (flags & STRUCT_NO_TYPE) == 0 {
        let nm = type_name!(base, as_sym);
        names.push(Thread::current(), nm);
    }

    if (flags & STRUCT_NO_CONSTRUCTOR) == 0 {
        let nm = if (flags & STRUCT_NO_MAKE_PREFIX) != 0 {
            cstr_name!(base, as_sym)
        } else {
            cstr_make_name!(base, as_sym)
        };
        names.push(Thread::current(), nm);
    }

    if (flags & STRUCT_NO_PRED) == 0 {
        let nm = pred_name!(base, as_sym);
        names.push(Thread::current(), nm);
    }

    if fcount != 0 {
        for slot_num in 0..fcount {
            let fname = if let Some(fs) = field_symbols {
                let fun = fs.car();
                field_symbols = Some(fs.cdr());

                fun.strsym()
            } else {
                field_strings[slot_num]
            };

            if (flags & STRUCT_NO_GET) == 0 {
                let nm = get_name!(base, fname, as_sym);
                names.push(Thread::current(), nm);
            }

            if (flags & STRUCT_NO_SET) == 0 {
                let nm = set_name!(base, fname, as_sym);
                names.push(Thread::current(), nm);
            }
        }
    }

    if (flags & STRUCT_GEN_GET) != 0 {
        let nm = genget_name!(base, as_sym);
        names.push(Thread::current(), nm);
    }

    if (flags & STRUCT_GEN_SET) != 0 {
        let nm = genset_name!(base, as_sym);
        names.push(Thread::current(), nm);
    }

    if (flags & STRUCT_EXPTIME) != 0 {
        let nm = exptime_name!(base, as_sym);
        names.push(Thread::current(), nm);
    }

    names
}

pub fn make_struct_names(
    base: Value,
    fcount: usize,
    field_symbols: Option<Value>,
    field_strings: &[&str],
    flags: i32,
) -> ArrayList<Value> {
    _make_struct_names(base.strsym(), fcount, field_symbols, field_strings, flags)
}

pub fn make_struct_names_from_array(
    base: &str,
    fcount: usize,
    fields: &[&str],
    flags: i32,
) -> ArrayList<Value> {
    _make_struct_names(base, fcount, None, fields, flags)
}

fn append_super_props(p: Handle<StructProperty>, arg: Value, orig: Value) -> Result<Value, Value> {
    let mut first = None::<Value>;
    let mut last = None::<Value>;

    if p.supers.is_pair() {
        let mut props = p.supers;

        while props.is_pair() {
            let v = props.car();

            let c = apply(v.cdr(), &[arg])?;
            let v = scm_cons(Thread::current(), v.car(), c);

            let pr = scm_cons(Thread::current(), v, Value::encode_null_value());

            if let Some(last) = last {
                last.set_cdr(pr);
            } else {
                first = Some(pr);
            }

            last = Some(pr);

            props = props.cdr();
        }
    }

    Ok(if let Some(last) = last {
        last.set_cdr(orig);
        first.unwrap()
    } else {
        orig
    })
}

pub fn force_struct_type_info(mut stype: Handle<StructType>) {
    if !stype.accessor.is_procedure() {
        let fun = genget_name!(stype.name.strsym(), false);
        let p = make_struct_proc(
            stype,
            fun.strsym(),
            Value::encode_null_value(),
            ProcType::GenGetter,
            0,
        );

        stype.accessor = p;

        let fun = genset_name!(stype.name.strsym(), false);
        let p = make_struct_proc(
            stype,
            fun.strsym(),
            Value::encode_null_value(),
            ProcType::GenSetter,
            0,
        );

        stype.mutator = p;
    }
}

fn get_struct_type_info(args: &[Value], a: &mut [Value]) {
    let stype = args[0].struct_type();

    force_struct_type_info(stype);

    let parent = if stype.name_pos != 0 {
        Some(stype.parent_types[stype.name_pos as usize - 1])
    } else {
        None
    };

    a[0] = stype.name;
    let cnt = stype.num_islots - parent.map(|x| x.struct_type().num_islots).unwrap_or(0);
    a[1] = Value::encode_int32(cnt);
    a[2] = Value::encode_int32(
        stype.num_slots - parent.map(|x| x.struct_type().num_slots).unwrap_or(0) - cnt,
    );
    a[3] = stype.accessor;
    a[4] = stype.mutator;

    a[5] = Value::encode_null_value();
    let p = stype.name_pos - 1;

    a[6] = if p >= 0 {
        stype.parent_types[p as usize].into()
    } else {
        Value::encode_bool_value(false)
    };

    a[7] = (p == stype.name_pos - 1).into();
}

fn guard_property(prop: Value, v: Value, t: Handle<StructType>) -> Result<Value, Value> {
    let p = prop.struct_property();

    if p.guard.is_procedure() {
        let mut info: [Value; 8] = [Value::encode_null_value(); 8];
        get_struct_type_info(&[t.into()], &mut info);

        let l = scm_list(Thread::current(), &info);

        return apply(p.guard, &[v, l]);
    } else {
        Ok(v)
    }
}

fn count_props(mut props: Value) -> usize {
    let mut c = 0;
    while props.is_pair() {
        let v = props.car();
        c += 1;

        if v.car().struct_property().supers.is_pair() {
            c += count_props(v.struct_property().supers);
        }

        props = props.cdr();
    }

    c
}

fn _make_struct_type(
    name: Value,
    parent: Value,
    num_fields: usize,
    num_uninit_fields: usize,
    uninit_val: Option<Value>,
    props: Value,
    guard: Value,
) -> Result<Value, Value> {
    /*let parent_type = if parent.is_false() {
        None
    } else {
        Some(parent.struct_type())
    };

    let depth = parent_type.map(|x| 1 + x.name_pos).unwrap_or(0);

    let num_slots = num_fields as i32
        + num_uninit_fields as i32
        + parent_type.map(|x| x.num_slots).unwrap_or(0);
    let num_islots = num_fields as i32 + parent_type.map(|x| x.num_islots).unwrap_or(0);
    //let sprops = parent_type.map(|x| x.props);
    let num_props = parent_type.map(|x| x.props.len()).unwrap_or(0);

    let uninit_val = uninit_val.unwrap_or(false.into());
    let thr = Thread::current();
    if props.is_pair() {
        let mut snum_props = count_props(props);
        let mut pa = ArrayList::with_capacity(thr, num_props);
        let mut i = 0;
        if parent_type.is_some() {
            for i in 0..num_props {
                pa.push(thr, parent_type.unwrap().props[i as usize]);
            }

            i = parent_type.unwrap().props.len();
        }

        snum_props = i;

        let mut l = props;

        while l.is_pair() {
            let a = l.car();
            let prop = a.car();
            let propv = guard_property(prop, a.cdr(), parent_type.unwrap())?;
        }
    }

    let vm = scm_vm();
    vm.mutator().safepoint();
    let props = ArrayList::with_capacity(Thread::current(), 1);



    let mut parent_types = ArrayList::with_capacity(vm.mutator(), depth as usize + 1);
    unsafe {
        parent_types.set_len(depth as usize + 1);
    }
    for j in (0..depth).rev() {
        parent_types.write_barrier(vm.mutator());
        parent_types[j as usize] = parent_type.unwrap().parent_types[j as usize];
        //parent_types[j as usize] = parent_type.unwrap().parent_types[j as usize];
    }

    /*let this = StructType::new(
        vm.mutator(),
        num_slots,
        num_islots,
        depth,
        name,
        Value::make_false(),
        Value::make_false(),
        uninit_val,
        props,
        guard,
        parent_types,
    );*/

    let mut this = vm.mutator().allocate(StructType {
        header: ObjectHeader::new(Type::StructType),
        num_islots,
        num_slots,
        name_pos: depth,
        name,
        accessor: false.into(),
        mutator: false.into(),
        uninit_val,
        props,
        guard,
        parent_types,
    });

    this.parent_types.write_barrier(vm.mutator());
    this.parent_types[depth as usize] = this.into();

    Ok(this.into())*/

    let parent_type = if parent.is_struct_type() {
        Some(parent.struct_type())
    } else {
        None
    };

    let depth = parent_type.map(|x| 1 + x.name_pos).unwrap_or(0);

    let vm = scm_vm();

    let mut struct_type = vm.mutator().allocate(StructType {
        header: ObjectHeader::new(Type::StructType),
        num_islots: 0,
        num_slots: 0,
        name,
        name_pos: 0,
        accessor: false.into(),
        mutator: false.into(),
        uninit_val: false.into(),
        props: Array::new(Thread::current(), 0, |_, _| Value::encode_null_value()),
        guard: false.into(),
        parent_types: ArrayList::with_capacity(Thread::current(), depth as usize + 1),
    });

    struct_type.name_pos = depth;
    unsafe {
        struct_type.parent_types.set_len(depth as usize + 1);
    }
    vm.mutator().write_barrier(struct_type);
    struct_type.parent_types[depth as usize] = struct_type.into();
    let mut j = depth;

    while j != 0 {
        j -= 1;
        vm.mutator().write_barrier(struct_type);
        struct_type.parent_types[j as usize] = parent_type.unwrap().parent_types[j as usize];
    }

    struct_type.name = name;
    struct_type.num_slots = num_fields as i32
        + num_uninit_fields as i32
        + parent_type.map(|x| x.num_slots).unwrap_or(0);
    struct_type.num_islots = num_fields as i32 + parent_type.map(|x| x.num_islots).unwrap_or(0);

    if num_fields >= u16::MAX as usize
        || num_uninit_fields >= u16::MAX as usize
        || num_uninit_fields + num_fields >= u16::MAX as usize
    {
        return raise_exn!(
            Fail,
            &[],
            "too many fields for a struct-type\n maximum total field count: {}",
            u16::MAX
        );
    }

    if let Some(parent_type) = parent_type {
        struct_type.props = parent_type.props;
    }

    let uninit_val = uninit_val.unwrap_or(false.into());

    struct_type.uninit_val = uninit_val;

    if props.is_pair() {
        let mut num_props = count_props(props);
        let mut can_override = HashMap::with_hasher_and_capacity(RandomState::new(), 4);
        let mut ji = 0;
        for i in 0..struct_type.props.len() {
            let prop = struct_type.props[i];
            can_override.put(Thread::current(), prop, true);
            ji = i;
        }
        let mut skip_supers = false;
        let mut pa = Array::new(Thread::current(), ji + num_props, |_, _| {
            Value::encode_null_value()
        });

        if ji != 0 {
            for i in 0..ji {
                pa[i] = struct_type.props[i];
            }
        }
        num_props = ji;
        let mut l = props;

        while l.is_pair() {
            let mut a = l.car();

            let prop = a.car();

            let propv = guard_property(prop, a.cdr(), struct_type)?;

            let mut j = 0;

            while j < num_props {
                if pa[j] == prop {
                    break;
                }
                j += 1;
            }

            if j < num_props {
                if can_override.get(&prop) == Some(&false) {
                    if propv != pa[j] {
                        break;
                    }
                    skip_supers = true;
                }
                println!("overriden");
                can_override.put(Thread::current(), prop, false);
            } else {
                num_props += 1;
            }

            l = l.cdr();

            if !skip_supers {
                l = append_super_props(prop.struct_property(), propv, l)?;
            }

            a = scm_cons(Thread::current(), prop, propv);
            Thread::current().write_barrier(pa);
            pa[j] = a;
        }

        if num_props != 0 {
            struct_type.props = pa;
        }

        if !l.is_null() {
            let a = l.car();

            return raise_exn!(FailContract, &[], "duplicate property binding: {}", a);
        }
    }

    if !guard.is_false() {
        if !guard.is_procedure() || !check_arity(guard, struct_type.num_islots + 1, false) {
            return raise_exn!(
                Fail,
                &[],
                "guard procedure does not accept correct number of arguments;\n
                should accept one more than the number of constructor arguments\n
                guard procedure: {}, expected arity: {}",
                guard,
                struct_type.num_islots + 1
            );
        }

        struct_type.guard = guard;
    } else {
        struct_type.guard = Value::encode_undefined_value();
    }

    Ok(struct_type.into())
}

pub fn make_struct_type(
    name: Value,
    parent: Value,
    num_fields: usize,
    num_uninit_fields: usize,
    uninit_val: Option<Value>,
    props: Value,
    guard: Value,
) -> Result<Value, Value> {
    _make_struct_type(
        name,
        parent,
        num_fields,
        num_uninit_fields,
        uninit_val,
        props,
        guard,
    )
}

pub fn make_struct_type_from_string(
    name: &str,
    parent: Value,
    num_fields: usize,
    props: Value,
    guard: Value,
) -> Result<Value, Value> {
    let name = name.intern().into();
    _make_struct_type(name, parent, num_fields, 0, None, props, guard)
}

extern "C" fn make_struct_type_proc(cfr: &mut CallFrame) -> ScmResult {
    let args = cfr.arguments();

    if !args[0].is_symbol() {
        return ScmResult::err(
            wrong_contract::<()>("make-struct-type", "symbol?", 0, args.len() as _, args)
                .unwrap_err(),
        );
    }

    if !args[1].is_false() && !args[1].is_struct_type() {
        return ScmResult::err(
            wrong_contract::<()>(
                "make-struct-type",
                "(or/c struct-type? #f)",
                1,
                args.len() as _,
                args,
            )
            .unwrap_err(),
        );
    }

    let initc = if !args[2].is_int32() || args[2].get_int32() < 0 {
        return ScmResult::err(
            wrong_contract::<()>(
                "make-struct-type",
                "exact-nonnegative-integer?",
                2,
                args.len() as _,
                args,
            )
            .unwrap_err(),
        );
    } else {
        args[2].get_int32() as usize
    };

    let uninitc = if !args[3].is_int32() || args[3].get_int32() < 0 {
        return ScmResult::err(
            wrong_contract::<()>(
                "make-struct-type",
                "exact-nonnegative-integer?",
                3,
                args.len() as _,
                args,
            )
            .unwrap_err(),
        );
    } else {
        args[3].get_int32() as usize
    };

    let mut uninit_val: Value = false.into();
    let mut props = Value::encode_null_value();
    let mut guard = Value::encode_bool_value(false);
    let mut cstr_name = Value::encode_null_value();

    if args.len() > 4 {
        uninit_val = args[4];

        if args.len() > 5 {
            props = args[5];

            if args.len() > 6 {
                guard = args[6];

                if !guard.is_procedure() && !guard.is_false() {
                    return ScmResult::err(
                        wrong_contract::<()>(
                            "make-struct-type",
                            "procedure?",
                            6,
                            args.len() as _,
                            args,
                        )
                        .unwrap_err(),
                    );
                }

                if args.len() > 7 {
                    cstr_name = args[7];

                    if !cstr_name.is_symbol() {
                        return ScmResult::err(
                            wrong_contract::<()>(
                                "make-struct-type",
                                "symbol?",
                                7,
                                args.len() as _,
                                args,
                            )
                            .unwrap_err(),
                        );
                    }
                }
            }
        }
    }

    let typ = match _make_struct_type(
        args[0],
        args[1],
        initc,
        uninitc,
        Some(uninit_val),
        props,
        guard,
    ) {
        Ok(v) => v,
        Err(e) => return ScmResult::err(e),
    };

    {
        let mut names = make_struct_names(
            args[0],
            0,
            None,
            &[],
            STRUCT_GEN_GET | STRUCT_GEN_SET | STRUCT_NAMES_AS_STRINGS,
        );

        if cstr_name.is_symbol() {
            names[1] = make_string(Thread::current(), cstr_name.strsym()).into();
        }

        let r = make_struct_values(
            typ,
            &mut names,
            STRUCT_GEN_GET | STRUCT_GEN_SET | STRUCT_NAMES_AS_STRINGS,
        );

        ScmResult::ok(r)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[allow(dead_code)]
enum ProcType {
    Constr,
    Pred,
    GenGetter,
    GenSetter,
    Getter,
    Setter,
}

fn make_struct_proc(
    stype: Handle<StructType>,
    func_name: &str,
    contract: Value,
    proc_type: ProcType,
    field_num: i32,
) -> Value {
    let mut flags = 0;
    let mut p = if proc_type == ProcType::Constr {
        let simple = is_simple_struct_type(stype);
        flags |= if simple {
            SCM_PRIM_STRUCT_TYPE_SIMPLE_CONSTR
        } else {
            SCM_PRIM_STRUCT_TYPE_CONSTR
        };
        scm_make_closed_native_procedure(
            Thread::current(),
            func_name.intern().into(),
            if simple {
                make_simple_struct_instance
            } else {
                make_struct_instance
            },
            stype.num_islots as _,
            stype.num_islots as _,
            &[stype.into()],
        )
    } else if proc_type == ProcType::Pred {
        flags |= SCM_PRIM_STRUCT_TYPE_PRED;
        scm_make_closed_native_procedure(
            Thread::current(),
            func_name.intern().into(),
            struct_pred,
            1,
            1,
            &[stype.into()],
        )
    } else {
        let need_pos = proc_type == ProcType::GenSetter || proc_type == ProcType::GenGetter;

        let mut a = [stype.into(), Value::encode_int32(field_num), contract];

        if proc_type == ProcType::GenGetter || proc_type == ProcType::Getter {
            if need_pos {
                flags |= SCM_PRIM_STRUCT_TYPE_INDEXED_GETTER;
            } else {
                flags |= SCM_PRIM_STRUCT_TYPE_INDEXLESS_GETTER;
            }
            scm_make_closed_native_procedure(
                Thread::current(),
                func_name.intern().into(),
                struct_getter,
                if need_pos { 2 } else { 1 },
                if need_pos { 2 } else { 1 },
                &mut a,
            )
        } else {
            if need_pos {
                flags |= SCM_PRIM_STRUCT_TYPE_INDEXED_SETTER;
            } else {
                flags |= SCM_PRIM_STRUCT_TYPE_INDEXLESS_SETTER;
            }
            scm_make_closed_native_procedure(
                Thread::current(),
                func_name.intern().into(),
                struct_setter,
                if need_pos { 3 } else { 2 },
                if need_pos { 3 } else { 2 },
                &mut a,
            )
        }
    };
    p.header.flags |= flags as u32;
    p.into()
}

fn make_struct_field_xxor(who: &str, getter: bool, args: &[Value]) -> ScmResult {
    if !args[0].is_native_procedure()
        || (args[0].object_header().flags
            & if getter {
                SCM_PRIM_STRUCT_TYPE_INDEXED_GETTER as u32
            } else {
                SCM_PRIM_STRUCT_TYPE_INDEXED_SETTER as u32
            }
            == 0)
    {
        return ScmResult::err(wrong_contract::<()>(who, if getter {
            "(and/c struct-accessor-procedure? (lambda (p) (procedure-arity-includes? p 2)))"
        } else {
            "(and/c struct-mutator-procedure? (lambda (p) (procedure-arity-includes? p 3)))"
        }, 0, args.len() as _, args).unwrap_err());
    }

    let pos = match parse_pos(
        who,
        args[0].closed_native_procedure()[0].struct_type(),
        args,
        "",
    ) {
        Ok(x) => x,
        Err(e) => return ScmResult::err(e),
    };

    let fieldstr;
    let mut name = None;
    let mut contract = Value::encode_null_value();
    let mut _module = Value::encode_null_value();
    if args.len() > 2 {
        if args[2].is_false() {
            fieldstr = None;
        } else {
            if !args[2].is_symbol() {
                return ScmResult::err(
                    wrong_contract::<()>(who, "(or/c symbol? #f)", 2, args.len() as _, args)
                        .unwrap_err(),
                );
            }

            fieldstr = Some(args[2].strsym());
        }

        if args.len() > 3 {
            if args[3].is_false() {
                name = None;
            } else {
                name = fieldstr;

                if !args[3].is_symbol() && !args[3].is_string() {
                    return ScmResult::err(
                        wrong_contract::<()>(
                            who,
                            "(or/c symbol? string? #f)",
                            3,
                            args.len() as _,
                            args,
                        )
                        .unwrap_err(),
                    );
                }

                contract = args[3];

                if args.len() > 4 {
                    if !args[4].is_module() {
                        return ScmResult::err(
                            wrong_contract::<()>(
                                who,
                                "(or/c symbol? module?)",
                                4,
                                args.len() as _,
                                args,
                            )
                            .unwrap_err(),
                        );
                    }

                    _module = args[4];
                }
            }
        }
    } else {
        fieldstr = None;
        name = None;
    }

    let fieldstr = if name.is_none() && fieldstr.is_none() {
        Some(format!("field{:x}", args[1].get_int32()))
    } else {
        fieldstr.map(|x| x.to_string())
    };
    let st = args[0].closed_native_procedure()[0].struct_type();

    if name.is_none() {
        if fieldstr.is_none() {
            if getter {
                name = Some("accessor");
            } else {
                name = Some("mutator");
            }
        } else if getter {
            name = Some(get_name!(st.name.strsym(), &fieldstr.unwrap(), false).strsym());
        } else {
            name = Some(set_name!(st.name.strsym(), &fieldstr.unwrap(), false).strsym());
        }
    }

    ScmResult::ok(make_struct_proc(
        st,
        name.unwrap(),
        contract,
        if getter {
            ProcType::Getter
        } else {
            ProcType::Setter
        },
        pos,
    ))
}

extern "C" fn make_struct_field_accessor(cfr: &mut CallFrame) -> ScmResult {
    make_struct_field_xxor("make-struct-field-accessor", true, cfr.arguments())
}

extern "C" fn make_struct_field_mutator(cfr: &mut CallFrame) -> ScmResult {
    make_struct_field_xxor("make-struct-field-mutator", false, cfr.arguments())
}

pub(crate) fn initialize_struct() {
    let module = scm_capy_module().module();

    let subr = scm_make_subr("make-struct-type", make_struct_type_proc, 4, 8);
    scm_define(module, "make-struct-type".intern(), subr).unwrap();

    let subr = scm_make_subr("make-struct-type-property", make_struct_type_property, 1, 5);
    scm_define(module, "make-struct-type-property".intern(), subr).unwrap();

    let subr = scm_make_subr("struct-type-property?", struct_type_property_p, 1, 1);
    scm_define(module, "struct-type-property?".intern(), subr).unwrap();

    let subr = scm_make_subr("struct?", struct_p, 1, 1);
    scm_define(module, "struct?".intern(), subr).unwrap();

    let subr = scm_make_subr("struct-type?", struct_type_p, 1, 1);
    scm_define(module, "struct-type?".intern(), subr).unwrap();

    let subr = scm_make_subr("struct-type-make-predicate", struct_type_pred, 1, 1);
    scm_define(module, "struct-type-make-predicate".intern(), subr).unwrap();

    let subr = scm_make_subr("struct-type-make-constructor", struct_type_constr, 1, 2);

    scm_define(module, "struct-type-make-constructor".intern(), subr).unwrap();

    let subr = scm_make_subr(
        "make-struct-field-accessor",
        make_struct_field_accessor,
        2,
        5,
    );
    scm_define(module, "make-struct-field-accessor".intern(), subr).unwrap();

    let subr = scm_make_subr("make-struct-field-mutator", make_struct_field_mutator, 2, 5);
    scm_define(module, "make-struct-field-mutator".intern(), subr).unwrap();
}
