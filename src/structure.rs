#![allow(dead_code, unused_variables, unused_macros)]

use rsgc::{
    prelude::Handle,
    system::{array::Array, arraylist::ArrayList},
    thread::Thread,
};

use crate::{
    compiler::env::environment_set,
    error::{contract_error, wrong_contract},
    fun::check_proc_arity,
    value::{Str, StructProperty, StructType, Structure, Type, Value},
    vm::{intern, Trampoline, Vm},
};

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

pub fn make_struct_instance_(vm: &mut Vm, stype: Value, args: &[Value]) -> Result<Value, Value> {
    let slots = Array::new(
        vm.mutator(),
        *stype.structuretype_num_slots() as usize,
        |_, _| Value::make_void(),
    );
    let inst = Structure::new(vm.mutator(), stype.downcast_structuretype(), slots);

    // todo: args = apply_guards(vm, stype, args)?;

    let c = *stype.structuretype_num_slots() as usize;

    let mut j = c;
    let mut i = args.len();

    let mut p = *stype.structuretype_name_pos();

    while p >= 0 {
        let (mut ns, mut nis) = if p != 0 {
            (
                *stype.structuretype_parent_types()[p as usize].structuretype_num_slots()
                    - *stype.structuretype_parent_types()[(p as usize) - 1]
                        .structuretype_num_slots(),
                *stype.structuretype_parent_types()[p as usize].structuretype_num_islots()
                    - *stype.structuretype_parent_types()[(p as usize) - 1]
                        .structuretype_num_islots(),
            )
        } else {
            (
                *stype.structuretype_parent_types()[0].structuretype_num_slots(),
                *stype.structuretype_parent_types()[0].structuretype_num_islots(),
            )
        };

        ns -= nis;

        // fill in automatic fields
        while ns != 0 {
            j -= 1;
            vm.mutator().write_barrier(*inst.struct_slots());
            inst.struct_slots_mut()[j] =
                *stype.structuretype_parent_types()[p as usize].structuretype_uninit_val();
            ns -= 1;
        }
        // fill in supplied fields
        while nis > 0 {
            j -= 1;
            i -= 1;
            vm.mutator().write_barrier(*inst.struct_slots());
            inst.struct_slots_mut()[j] = args[i];
            nis -= 1;
        }
        p -= 1;
    }

    Ok(inst).into()
}

pub(crate) fn make_struct_instance(
    vm: &mut Vm,
    _: Value,
    args: &[Value],
    vars: Handle<Array<Value>>,
    _: &str,
) -> Trampoline {
    make_struct_instance_(vm, vars[0], args).into()
}

fn prop_pred(
    vm: &mut Vm,
    _: Value,
    args: &[Value],
    vars: Handle<Array<Value>>,
    _: &str,
) -> Trampoline {
    let v = args[0];
    let prop = vars[0];

    let stype = if v.structp() {
        *v.struct_stype()
    } else if v.structuretypep() {
        v.downcast_structuretype()
    } else {
        return Trampoline::Return(Value::make_false());
    };

    for sprop in stype.props.iter().copied().rev() {
        if sprop.car() == prop {
            return Trampoline::Return(Value::make_true());
        }
    }

    Trampoline::Return(Value::make_false())
}

fn make_name(pre: &str, tn: &str, post1: &str, fun: &str, post2: &str, sym: bool) -> Value {
    if sym {
        intern(format!("{}{}{}{}{}", pre, tn, post1, fun, post2).as_str())
    } else {
        Str::new(
            Thread::current(),
            format!("{}{}{}{}{}", pre, tn, post1, fun, post2).as_str(),
        )
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

macro_rules! type_name_str {
    ($base:expr) => {
        make_name("struct:", $base, "", "", "", false)
    };
}

fn do_prop_accessor(prop: Value, arg: Value) -> Option<Value> {
    let stype = if arg.structp() {
        *arg.struct_stype()
    } else if arg.structuretypep() {
        arg.downcast_structuretype()
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

fn prop_accessor(
    vm: &mut Vm,
    _: Value,
    args: &[Value],
    vars: Handle<Array<Value>>,
    name: &str,
) -> Trampoline {
    let v = args[0];

    let v = do_prop_accessor(vars[0], v);

    if let Some(v) = v {
        Trampoline::Return(v)
    } else if args.len() == 1 {
        let prop = vars[0];

        let ctc = if prop.structureproperty_contract_name().strp()
            || prop.structureproperty_contract_name().symbolp()
        {
            prop.structureproperty_contract_name().strsym()
        } else {
            prop.structureproperty_name().str()
        };

        return wrong_contract::<()>(name, ctc, 0, 1, args).into();
    } else {
        let v = args[1];
        if v.procedurep() {
            vm.tail_apply(v, &[])
        } else {
            Trampoline::Return(v)
        }
    }
}

pub(crate) fn make_struct_type_property_raw(
    args: &[Value],
    predout: &mut Value,
    accessout: &mut Value,
) -> Result<Value, Value> {
    let mut supers = Value::make_null();
    let who = "make-struct-type-property";

    if !args[0].symbolp() {
        return wrong_contract(who, "symbol?", 0, args.len() as _, args);
    }
    let mut a: [Value; 1] = [Value::null()];
    let mut accessor_name = None;
    let mut contract_name = None;
    if args.len() > 1 {
        if args[1].truep() && !check_proc_arity("", 2, 1, args.len() as _, args)? {
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

            if let Some(_) = supers.proper_list_length() {
                let mut pr = supers;

                while pr.pairp() {
                    let v = pr.car();
                    if !v.pairp() {
                        supers = Value::make_false();
                    } else {
                        if v.car().get_type() != Type::StructureProperty {
                            supers = Value::make_false();
                        }

                        a[0] = v.cdr();

                        if !check_proc_arity("", 1, 0, 1, &a)? {
                            supers = Value::make_false();
                        }
                    }
                    pr = pr.cdr();
                }
            } else {
                supers = Value::make_false();
            }
            if supers.falsep() {
                return wrong_contract(
                    who,
                    "(listof (cons struct-type-property? (any/c . -> any)))",
                    2,
                    args.len() as _,
                    args,
                );
            }

            if args.len() > 3 {
                if args[3].is_true() {
                    accessor_name = Some(args[3]);
                    if !accessor_name.unwrap().symbolp() {
                        return wrong_contract(who, "(or/c symbol? #f)", 3, args.len() as _, args);
                    }
                }

                if args.len() > 4 {
                    if args[4].is_true() {
                        contract_name = Some(args[4]);
                        if !contract_name.unwrap().symbolp() {
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

    let p = StructProperty::new(
        Thread::current(),
        args[0],
        if args.len() > 1 && args[1].is_true() {
            args[1]
        } else {
            Value::make_false()
        },
        supers,
        contract_name.unwrap_or(Value::make_false()),
    );

    a[0] = p;

    let name = Str::new(Thread::current(), format!("{}?", args[0]));
    let prop_pred = Vm::make_closed_procedure(&format!("{}?", name), prop_pred, 1, 1, &[p]);

    let name = if let Some(accessor_name) = accessor_name {
        accessor_name.strsym().to_string()
    } else {
        format!("{}-accessor", args[0])
    };

    let prop_accessor = Vm::make_closed_procedure(&name, prop_accessor, 1, 2, &[p]);

    *predout = prop_pred;
    *accessout = prop_accessor;

    Ok(a[0])
}

define_proc! {
    extern "make-struct-type-property", make_struct_type_property (vm, args) 1, 5 => {
        let mut a = [Value::null(); 3];
        let mut pred = Value::null();
        let mut acc = Value::null();
        let p = match make_struct_type_property_raw(args, &mut pred, &mut acc) {
            Ok(v) => v,
            Err(e) => return Trampoline::Throw(e)
        };

        a[0] = p;
        a[1] = pred;
        a[2] = acc;
        Trampoline::Return(vm.values(&a))
    }
}

pub fn make_struct_type_property_w_guard(name: Value, guard: Value) -> Result<Value, Value> {
    let mut a = [Value::null(); 2];
    let mut pred = Value::null();
    let mut accessor = Value::null();

    a[0] = name;
    a[1] = guard;

    make_struct_type_property_raw(&a, &mut pred, &mut accessor)
}

pub fn make_struct_type_property_(name: Value) -> Result<Value, Value> {
    make_struct_type_property_w_guard(name, Value::make_false())
}

define_proc! {
    extern "struct-type-property?", is_struct_type_property (_vm, args) 1, 1 => {
        let v = args[0];
        if v.get_type() == Type::StructureProperty {
            Trampoline::Return(Value::make_true())
        } else {
            Trampoline::Return(Value::make_false())
        }
    }
}

pub fn is_struct_type(st: Handle<StructType>, v: Handle<Structure>) -> bool {
    (st.name_pos <= v.stype.name_pos)
        && (st.as_ptr()
            == v.stype.parent_types[st.name_pos as usize]
                .downcast_structuretype()
                .as_ptr())
}

pub fn is_struct_instance(typ: Value, v: Value) -> bool {
    if !typ.structuretypep() {
        return false;
    }

    if !v.structp() {
        return false;
    }

    is_struct_type(typ.downcast_structuretype(), v.downcast_struct())
}

pub fn struct_ref(sv: Value, pos: usize) -> Value {
    sv.downcast_struct().slots[pos]
}

pub fn struct_set(sv: Value, pos: usize, v: Value) {
    Thread::current().write_barrier(sv.handle());
    sv.downcast_struct().slots[pos] = v;
}

pub fn is_simple_struct_type(stype: Handle<StructType>) -> bool {
    for p in (0..=stype.name_pos).rev() {
        if stype.parent_types[p as usize]
            .structuretype_guard()
            .is_true()
        {
            return false;
        }

        if stype.parent_types[p as usize].structuretype_num_slots()
            != stype.parent_types[p as usize].structuretype_num_islots()
        {
            return false;
        }
    }

    true
}

pub fn make_simple_struct_instance_from_array(
    vm: &mut Vm,
    args: &[Value],
    typ: Value
) -> Value {
    let stype = typ.downcast_structuretype();

    let c = stype.num_slots;

    let slots = Array::new(vm.mutator(), c as _, |_, i| args[i]);

    let s = Structure::new(vm.mutator(), stype, slots);

   s 
}


pub(crate) fn make_simple_struct_instance(
    vm: &mut Vm,
    _: Value,
    args: &[Value],
    vars: Handle<Array<Value>>,
    _: &str,
) -> Trampoline {
    let stype = vars[0].downcast_structuretype();

    let c = stype.num_slots;

    let slots = Array::new(vm.mutator(), c as _, |_, i| args[i]);

    let s = Structure::new(vm.mutator(), stype, slots);

    Ok(s).into()
}

pub(crate) fn struct_pred(
    _vm: &mut Vm,
    _: Value,
    args: &[Value],
    vars: Handle<Array<Value>>,
    _: &str,
) -> Trampoline {
    let stype = vars[0];

    let val = if is_struct_instance(stype, args[0]) {
        Value::make_true()
    } else {
        Value::make_false()
    };

    Trampoline::Return(val)
}

pub(crate) fn parse_pos(
    _vm: &mut Vm,
    who: &str,
    vars: Handle<Array<Value>>,
    args: &[Value],
    name: &str,
) -> Result<i32, Value> {
    let st = vars[0].downcast_structuretype();

    let mut pos = if !args[1].intp() || args[1].int() < 0 {
        return wrong_contract(
            if who.len() == 0 { name } else { who },
            "exact-nonnegative-integer?",
            2,
            args.len() as _,
            args,
        );
    } else {
        args[1].int()
    };

    if (pos < st.num_slots) && (st.name_pos != 0) {
        pos += st.parent_types[st.name_pos as usize - 1].structuretype_num_slots();
    }

    if pos >= st.num_slots {
        let who = if who.len() == 0 { name } else { who };

        let sc = if st.name_pos != 0 {
            st.num_slots - st.parent_types[st.name_pos as usize - 1].structuretype_num_slots()
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
                &Value::make_int(sc - 1),
                &"structure",
                &args[0],
            ],
        );
    }

    Ok(pos)
}

fn extract_field_proc_name(st_name: Value, vars: Handle<Array<Value>>) -> String {
    let name_info = vars[2];
    let pred_name;
    if name_info.symbolp() {
        pred_name = name_info.strsym().to_string();
    } else {
        pred_name = format!("{}", st_name.strsym());
    }

    pred_name
}

fn extract_accessor_offset(acc: Value) -> i32 {
    let st = acc.downcast_structuretype();

    if st.name_pos != 0 {
        *st.parent_types[st.name_pos as usize - 1].structuretype_num_slots()
    } else {
        0
    }
}

fn wrong_struct_type(
    vars: Handle<Array<Value>>,
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

pub fn struct_getter(
    vm: &mut Vm,
    _: Value,
    args: &[Value],
    vars: Handle<Array<Value>>,
    name: &str,
) -> Trampoline {
    let st = vars[0].downcast_structuretype();

    let inst = args[0];

    if !inst.structp() {
        let pred_name = extract_field_proc_name(st.name, vars);
        return wrong_contract::<()>(name, &pred_name, 0, args.len() as _, args).into();
    } else if !is_struct_instance(vars[0], inst) {
        return Trampoline::Throw(wrong_struct_type(
            vars,
            name,
            st.name,
            inst.struct_stype().name,
            0,
            args.len() as _,
            args,
        ));
    }

    let pos = if args.len() == 2 {
        match parse_pos(vm, "", vars, args, name) {
            Ok(p) => p,
            Err(e) => return Trampoline::Throw(e),
        }
    } else {
        vars[1].int()
    };

    return Trampoline::Return(struct_ref(inst, pos as usize));
}

pub(crate) fn struct_setter(
    vm: &mut Vm,
    _: Value,
    args: &[Value],
    vars: Handle<Array<Value>>,
    name: &str,
) -> Trampoline {
    let st = vars[0].downcast_structuretype();

    let inst = args[0];

    if !inst.structp() {
        let pred_name = extract_field_proc_name(st.name, vars);
        return wrong_contract::<()>(name, &pred_name, 0, args.len() as _, args).into();
    } else if !is_struct_instance(vars[0], inst) {
        return Trampoline::Throw(wrong_struct_type(
            vars,
            name,
            st.name,
            inst.struct_stype().name,
            0,
            args.len() as _,
            args,
        ));
    }

    let v;
    let pos = if args.len() == 3 {
        v = args[2];
        match parse_pos(vm, "", vars, args, name) {
            Ok(p) => p,
            Err(e) => return Trampoline::Throw(e),
        }
    } else {
        v = args[1];
        vars[1].int()
    };

    struct_set(inst, pos as usize, v);

    return Trampoline::Return(Value::make_void());
}

define_proc! {
    extern "struct?", is_struct (_vm, args) 1, 1 => {
        let v = args[0];

        Trampoline::Return(if v.structp() {
            Value::make_true()
        } else {
            Value::make_false()
        })
    }
}

define_proc! {
    extern "struct-type?", is_structure_type (_vm, args) 1, 1 => {
        let v = args[0];

        Trampoline::Return(if v.structuretypep() {
            Value::make_true()
        } else {
            Value::make_false()
        })
    }
}

fn check_struct(who: &str, args: &[Value]) -> Result<(), Value> {
    if !args[0].structuretypep() {
        return wrong_contract(who, "struct-type?", 0, args.len() as _, args);
    }

    Ok(())
}

define_proc! {
    extern "struct-type-make-predicate", struct_type_pred (vm, args) 1, 1 => {
        match check_struct("struct-type-make-predicate", args) {
            Ok(()) => (),
            Err(e) => return Trampoline::Throw(e)
        }
        let stype = args[0];

        let name = format!("{}?", stype.downcast_structuretype().name.strsym());
        let val = make_struct_proc(stype.downcast_structuretype(), &name, Value::make_null(), ProcType::Pred, 0);

        Trampoline::Return(val)
    }
}

define_proc! {
    extern "struct-type-make-constructor", struct_type_constr (_vm, args) 1, 2 => {
        match check_struct("struct-type-make-constructor", args) {
            Ok(()) => (),
            Err(e) => return Trampoline::Throw(e)
        }

        let v = if args.len() < 2 || args[1].falsep() {
            format!("make-{}", args[0].downcast_structuretype().name.strsym())
        } else if args[1].symbolp() {
            args[1].strsym().to_string()
        } else {
            return wrong_contract::<()>("struct-type-make-constructor", "symbol?", 1, args.len() as _, args).into();
        };

        let stype = args[0].downcast_structuretype();

        Trampoline::Return(make_struct_proc(stype, &v, Value::make_null(), ProcType::Constr, 0))
    }
}

pub fn make_struct_values(typ: Value, names: &mut [Value], flags: i32) -> Value {
    let stype = typ.downcast_structuretype();

    let mut count = names.len();

    if (flags & STRUCT_EXPTIME) != 0 {
        count -= 1;
    }

    let values = Value::make_values_n(Thread::current(), count, Value::null());

    let mut pos = 0;

    if (flags & STRUCT_NO_TYPE) == 0 {
        values.values_set(pos, typ);
        pos += 1;
    }

    if (flags & STRUCT_NO_CONSTRUCTOR) == 0 {
        let nm = names[pos].strsym();

        let vi = make_struct_proc(stype, nm, Value::null(), ProcType::Constr, stype.num_slots);

        values.values_set(pos, vi);
        pos += 1;
    }
    if (flags & STRUCT_NO_PRED) == 0 {
        let nm = names[pos].strsym();

        let vi = make_struct_proc(stype, nm, Value::null(), ProcType::Pred, 0);

        values.values_set(pos, vi);
        pos += 1;
    }

    if (flags & STRUCT_GEN_GET) != 0 {
        count -= 1;
    }

    if (flags & STRUCT_GEN_SET) != 0 {
        count -= 1;
    }

    let mut slot_num = if stype.name_pos != 0 {
        *stype.parent_types[stype.name_pos as usize - 1].structuretype_num_slots()
    } else {
        0
    };
    while pos < count {
        if (flags & STRUCT_NO_GET) == 0 {
            let nm = names[pos].strsym();

            let vi = make_struct_proc(stype, nm, Value::null(), ProcType::Getter, slot_num);
            values.values_set(pos, vi);
            pos += 1;
        }

        if (flags & STRUCT_NO_SET) == 0 {
            let nm = names[pos].strsym();

            let vi = make_struct_proc(stype, nm, Value::null(), ProcType::Setter, slot_num);
            values.values_set(pos, vi);
            pos += 1;
        }

        slot_num += 1;
    }

    if (flags & STRUCT_GEN_GET) != 0 {
        let nm = names[pos].strsym();

        let vi = make_struct_proc(stype, nm, Value::null(), ProcType::GenGetter, slot_num);
        values.values_set(pos, vi);
        pos += 1;
    }

    if (flags & STRUCT_GEN_SET) != 0 {
        let nm = names[pos].strsym();

        let vi = make_struct_proc(stype, nm, Value::null(), ProcType::GenSetter, slot_num);
        values.values_set(pos, vi);
    }

    values
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

fn append_super_props(
    vm: &mut Vm,
    p: Handle<StructProperty>,
    arg: Value,
    orig: Value,
) -> Result<Value, Value> {
    let mut first = None::<Value>;
    let mut last = None::<Value>;

    if p.supers.pairp() {
        let mut props = p.supers;

        while props.pairp() {
            let v = props.car();

            let c = vm.apply(v.cdr(), &[arg])?;
            let v = Value::make_cons(v.car(), c);

            let pr = Value::make_cons(v, Value::null());

            if let Some(last) = last {
                last.set_pair_cdr(pr);
            } else {
                first = Some(pr);
            }

            last = Some(pr);

            props = props.cdr();
        }
    }

    Ok(if let Some(last) = last {
        last.set_pair_cdr(orig);
        first.unwrap()
    } else {
        orig
    })
}

pub fn force_struct_type_info(mut stype: Handle<StructType>) {
    if !stype.accessor.procedurep() {
        let fun = genget_name!(stype.name.strsym(), false);
        let p = make_struct_proc(stype, fun.strsym(), Value::null(), ProcType::GenGetter, 0);

        stype.accessor = p;

        let fun = genset_name!(stype.name.strsym(), false);
        let p = make_struct_proc(stype, fun.strsym(), Value::null(), ProcType::GenSetter, 0);

        stype.mutator = p;
    }
}

fn get_struct_type_info(args: &[Value], a: &mut [Value]) {
    let stype = args[0].downcast_structuretype();

    force_struct_type_info(stype);

    let parent = if stype.name_pos != 0 {
        Some(stype.parent_types[stype.name_pos as usize - 1])
    } else {
        None
    };

    a[0] = stype.name;
    let cnt = stype.num_islots - parent.map(|x| *x.structuretype_num_islots()).unwrap_or(0);
    a[1] = Value::make_int(cnt);
    a[2] = Value::make_int(
        stype.num_slots - parent.map(|x| *x.structuretype_num_slots()).unwrap_or(0) - cnt,
    );
    a[3] = stype.accessor;
    a[4] = stype.mutator;

    a[5] = Value::null();
    let p = stype.name_pos - 1;

    a[6] = if p >= 0 {
        stype.parent_types[p as usize]
    } else {
        Value::make_false()
    };

    a[7] = if p == stype.name_pos - 1 {
        Value::make_true()
    } else {
        Value::make_false()
    };
}

fn guard_property(
    vm: &mut Vm,
    prop: Value,
    v: Value,
    t: Handle<StructType>,
) -> Result<Value, Value> {
    let p = prop.downcast_structureproperty();

    if p.guard.is_true() && p.guard.procedurep() {
        let mut info: [Value; 7] = [Value::null(); 7];
        get_struct_type_info(&[t.into()], &mut info);

        let l = Value::make_list(vm.mutator(), &info);

        return vm.apply(p.guard, &[v, l]);
    } else {
        Ok(v)
    }
}

fn _make_struct_type(
    vm: &mut Vm,
    name: Value,
    parent: Value,
    num_fields: usize,
    num_uninit_fields: usize,
    uninit_val: Option<Value>,
    _props: Value,
    guard: Value,
) -> Result<Value, Value> {
    let parent_type = if parent.falsep() {
        None
    } else {
        Some(parent.downcast_structuretype())
    };

    let depth = parent_type.map(|x| 1 + x.name_pos).unwrap_or(0);

    let num_slots = num_fields as i32
        + num_uninit_fields as i32
        + parent_type.map(|x| x.num_slots).unwrap_or(0);
    let num_islots = num_fields as i32 + parent_type.map(|x| x.num_islots).unwrap_or(0);

    //let sprops = parent_type.map(|x| x.props);
    //let mut num_props = parent_type.map(|x| x.props.len()).unwrap_or(0);
    let uninit_val = uninit_val.unwrap_or(Value::make_false());
    /*
    if props.pairp() {
        let snum_props = props.list_length();
        let mut i = 0;
        let mut can_override = HashMap::with_hasher_and_capacity(RandomState::new(), snum_props as u32 + num_props as u32);
        while i < num_props {
            let prop = sprops.unwrap()[i].car();
            can_override.put(vm.mutator(), prop, true);
            i += 1;
        }

        let mut pa = Array::new(vm.mutator(), i as usize + snum_props as usize, |_, _| Value::null());

        if i != 0 {
            pa.copy_from_slice(&sprops.unwrap()[..i]);
        }

        num_props = i;

        let mut l = props;

        while l.pairp() {
            let mut skip_supers = false;

            let a = l.car();

            let prop = a.car();

            //let propv = guard_property(vm, prop, a.cdr(), )

            let propv = a.cdr();
            let mut j = 0;
            while j < num_props {
                if pa[j].car() == prop {
                    break;
                }
            }

            if j < num_props {
                if !can_override.get(&prop).copied().unwrap() {
                    if propv != pa[j].cdr() {
                        break;
                    }
                    skip_supers = true;
                }

                can_override.put(vm.mutator(), prop, false);
            } else {
                snum_props += 1;
            }



            l = l.cdr();

            if !skip_supers {
                l = append_super_props(vm, prop.downcast_structureproperty(), propv, l)?;
            }

            a = Value::make_cons(prop, propv);
            pa[j] = a;
        }

        if snum_props != 0 {

        }

    }   */
    let props = Array::new(vm.mutator(), 0, |_, _| Value::null());
    let mut parent_types = Array::new(vm.mutator(), depth as usize + 1, |_, _| Value::null());
    for j in (0..depth).rev() {
        parent_types[j as usize] = parent_type.unwrap().parent_types[j as usize];
    }

    let this = StructType::new(
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
    );

    parent_types[depth as usize] = this.into();

    Ok(this.into())
}

pub fn make_struct_type(
    vm: &mut Vm,
    name: Value,
    parent: Value,
    num_fields: usize,
    num_uninit_fields: usize,
    uninit_val: Option<Value>,
    props: Value,
    guard: Value,
) -> Result<Value, Value> {
    _make_struct_type(
        vm,
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
    vm: &mut Vm,
    name: &str,
    parent: Value,
    num_fields: usize,
    props: Value,
    guard: Value,
) -> Result<Value, Value> {
    let name = intern(name);
    _make_struct_type(vm, name, parent, num_fields, 0, None, props, guard)
}

define_proc! {
    extern "make-struct-type", make_struct_type_proc (vm, args) 4, 8 => {
        if !args[0].symbolp() {
            return wrong_contract::<()>("make-struct-type", "symbol?", 0, args.len() as _, args).into();
        }

        if !args[1].falsep() && !args[1].structuretypep() {
            return wrong_contract::<()>("make-struct-type", "(or/c struct-type? #f)", 1, args.len() as _, args).into();
        }

        let initc = if !args[2].intp() || args[2].int() < 0 {
            return wrong_contract::<()>("make-struct-type", "exact-nonnegative-integer?", 2, args.len() as _, args).into();
        } else {
            args[2].int() as usize
        };

        let uninitc = if !args[3].intp() || args[3].int() < 0 {
            return wrong_contract::<()>("make-struct-type", "exact-nonnegative-integer?", 3, args.len() as _, args).into();
        } else {
            args[3].int() as usize
        };
        let mut uninit_val = Value::make_false();
        let mut props = Value::make_null();
        let mut guard = Value::make_false();
        let mut cstr_name = Value::make_null();
        if args.len() > 4 {
            uninit_val = args[4];

            if args.len() > 5 {
                props = args[5];

                if args.len() > 6 {
                    guard = args[6];

                    if !guard.procedurep() {
                        return wrong_contract::<()>("make-struct-type", "procedure?", 6, args.len() as _, args).into();
                    }

                    if args.len() > 7 {
                        cstr_name = args[7];

                        if !cstr_name.symbolp() {
                            return wrong_contract::<()>("make-struct-type", "symbol?", 7, args.len() as _, args).into();
                        }
                    }
                }
            }
        }


        let typ = match _make_struct_type(vm, args[0], args[1], initc, uninitc, Some(uninit_val), props, guard) {
            Ok(s) => s,
            Err(e) => return Trampoline::Throw(e)
        };

        {
            let mut names = make_struct_names(args[0], 0, None, &[], STRUCT_GEN_GET | STRUCT_GEN_SET | STRUCT_NAMES_AS_STRINGS);

            if cstr_name.symbolp() {
                names[1] = Str::new(vm.mutator(), cstr_name.strsym());
            }

            let r = make_struct_values(typ, &mut names, STRUCT_GEN_GET | STRUCT_GEN_SET | STRUCT_NAMES_AS_STRINGS);
            Trampoline::Return(r)
        }
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
    let p = if proc_type == ProcType::Constr {
        let simple = is_simple_struct_type(stype);

        Vm::make_closed_procedure(
            func_name,
            if simple {
                make_simple_struct_instance
            } else {
                make_struct_instance
            },
            stype.num_islots,
            stype.num_islots,
            &[unsafe { Value::encode_ptr(stype.as_ptr()) }],
        )
    } else if proc_type == ProcType::Pred {
        Vm::make_closed_procedure(func_name, struct_pred, 1, 1, &[stype.into()])
    } else {
        let need_pos = proc_type == ProcType::GenSetter || proc_type == ProcType::GenGetter;

        let mut a = [stype.into(), Value::make_int(field_num), contract];

        if proc_type == ProcType::GenGetter || proc_type == ProcType::Getter {
            Vm::make_closed_procedure(
                func_name,
                struct_getter,
                if need_pos { 3 } else { 2 },
                if need_pos { 3 } else { 2 },
                &mut a,
            )
        } else {
            Vm::make_closed_procedure(
                func_name,
                struct_setter,
                if need_pos { 3 } else { 2 },
                if need_pos { 3 } else { 2 },
                &mut a,
            )
        }
    };

    p
}

pub fn initialize_struct(env: Value) {
    environment_set(
        env,
        *MAKE_STRUCT_TYPE_PROC_NAME,
        *MAKE_STRUCT_TYPE_PROC_PROC,
    );
    environment_set(
        env,
        *MAKE_STRUCT_TYPE_PROPERTY_NAME,
        *MAKE_STRUCT_TYPE_PROPERTY_PROC,
    );
    environment_set(env, *IS_STRUCTURE_TYPE_NAME, *IS_STRUCTURE_TYPE_PROC);
    environment_set(env, *IS_STRUCT_NAME, *IS_STRUCT_PROC);
    environment_set(env, *STRUCT_TYPE_PRED_NAME, *STRUCT_TYPE_PRED_PROC);
    environment_set(env, *STRUCT_TYPE_CONSTR_NAME, *STRUCT_TYPE_CONSTR_PROC);
}
