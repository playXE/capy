use std::collections::hash_map::RandomState;

use once_cell::sync::OnceCell;
use rsgc::heap::root_processor::SimpleRoot;

use crate::{
    compiler::Compiler,
    data::{
        exception::{Exception, SourcePosition},
        structure::{StructProperty, StructType},
    },
    prelude::{code::Ins, eval_error::EvalError, *},
    utilities::arraylist::ArrayList,
};

const ARITY_FIELDS: &'static [&str] = &["value"];
const DATE_FIELDS: &'static [&str] = &[
    "second",
    "minute",
    "hour",
    "day",
    "month",
    "year",
    "week-day",
    "year-day",
    "dst?",
    "time-zone-offset",
];

const DATE_STAR_FIELDS: &'static [&str] = &["nanosecond", "time-zone-name"];

const LOCATION_FIELDS: &'static [&str] = &["source", "line", "column", "position", "span"];

#[derive(Default)]
pub struct StructGlobals {
    arity_at_least: Value,
    make_arity_at_least: Value,
    location_struct: Value,
    poller_struct: Value,
    write_property: Value,
    print_attribute_property: Value,
    evt_property: Value,
    proc_property: Value,
    method_property: Value,
    checked_proc_property: Value,
    struct_info_proc: Value,
    date: Value,
    date_start: Value,
}

impl Object for StructGlobals {
    fn trace(&self, visitor: &mut dyn Visitor) {
        self.arity_at_least.trace(visitor);
        self.make_arity_at_least.trace(visitor);
        self.location_struct.trace(visitor);
        self.poller_struct.trace(visitor);
        self.write_property.trace(visitor);
        self.print_attribute_property.trace(visitor);
        self.evt_property.trace(visitor);
        self.proc_property.trace(visitor);
        self.method_property.trace(visitor);
        self.checked_proc_property.trace(visitor);
        self.struct_info_proc.trace(visitor);
        self.date.trace(visitor);
        self.date_start.trace(visitor);
    }
}

static STRUCT_GLOBALS: OnceCell<StructGlobals> = OnceCell::new();

pub const BUILTIN_STRUCT_FLAGS: i32 = SCHEME_STRUCT_NO_SET
    | SCHEME_STRUCT_EXPTIME
    | SCHEME_STRUCT_NO_MAKE_PREFIX
    | SCHEME_STRUCT_BUILTIN;

pub(crate) fn structure_library(ctx: &mut Context) {
    let manager = library_manager();
    let thr = Thread::current();

    let base = manager.scheme_module.get_handle_of::<Library>();

    manager.add_definition(
        thr,
        base,
        (
            "make-struct-type",
            Implementation::Native3R(make_struct_type),
        ),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        (
            "make-struct-type-property",
            Implementation::Native1R(make_struct_type_property),
        ),
        true,
        true,
    );

    ctx.runtime()
        .heap_mut()
        .add_root(SimpleRoot::new("struct-globals", "sg", |processor| {
            if let Some(sg) = STRUCT_GLOBALS.get() {
                sg.trace(processor.visitor());
            }
        }));

    let mut as_count = 0;
    let arity_at_least =
        make_struct_type_from_string(ctx, "arity-at-least", None, 1, Value::nil(), Value::nil())
            .unwrap();
    let as_names = make_struct_names_from_array(
        ctx,
        "arity-at-least",
        1,
        ARITY_FIELDS,
        BUILTIN_STRUCT_FLAGS,
        Some(&mut as_count),
    );

    let as_values = make_struct_values(
        ctx,
        arity_at_least,
        &as_names,
        as_count as _,
        BUILTIN_STRUCT_FLAGS,
    )
    .unwrap();

    let make_arity_at_least = as_values[1];

    for i in 0..as_count - 1 {
        let as_val = as_values[i as usize];
        let as_name = as_names[i as usize];

        library_manager().define_const(base, as_name.get_symbol(), as_val, true);
    }

    let date = make_struct_type_from_string(ctx, "date", None, 10, Value::nil(), Value::nil()).unwrap();
    let mut ts_count = 0;
    let ts_names = make_struct_names_from_array(
        ctx,
        "date",
        10,
        DATE_FIELDS,
        BUILTIN_STRUCT_FLAGS,
        Some(&mut ts_count),
    );

    let ts_values = make_struct_values(
        ctx,
        date,
        &ts_names,
        ts_count as _,
        BUILTIN_STRUCT_FLAGS,
    ).unwrap();

    for i in 0..ts_count - 1 {
        let ts_val = ts_values[i as usize];
        let ts_name = ts_names[i as usize];

        library_manager().define_const(base, ts_name.get_symbol(), ts_val, true);
    }

    let date = make_struct_type_from_string(ctx, "date*", None, 2, Value::nil(), Value::nil()).unwrap();

    let mut ts_count = 0;

    let ts_names = make_struct_names_from_array(
        ctx,
        "date*",
        2,
        DATE_STAR_FIELDS,
        BUILTIN_STRUCT_FLAGS,
        Some(&mut ts_count),
    );

    let ts_values = make_struct_values(
        ctx,
        date,
        &ts_names,
        ts_names.len(),
        BUILTIN_STRUCT_FLAGS,
    ).unwrap();

    for i in 0..ts_count - 1 {
        let ts_val = ts_values[i as usize];
        let ts_name = ts_names[i as usize];

        library_manager().define_const(base, ts_name.get_symbol(), ts_val, true);
    }

    let location_struct = make_struct_type_from_string(
        ctx,
        "location",
        None,
        5,
        Value::nil(),
        Value::nil(),
    ).unwrap();

    let mut loc_count = 0;

    let loc_names = make_struct_names_from_array(
        ctx,
        "srcloc",
        5,
        LOCATION_FIELDS,
        BUILTIN_STRUCT_FLAGS,
        Some(&mut loc_count),
    );

    let loc_values = make_struct_values(
        ctx,
        location_struct,
        &loc_names,
        loc_names.len(),
        BUILTIN_STRUCT_FLAGS,
    ).unwrap();

    for i in 0..loc_count - 1 {
        let loc_val = loc_values[i as usize];
        let loc_name = loc_names[i as usize];

        library_manager().define_const(base, loc_name.get_symbol(), loc_val, true);
    }

    STRUCT_GLOBALS.get_or_init(|| {
        let mut globals = StructGlobals::default();

        globals.location_struct = location_struct.into();
        globals.date = date.into();
        globals.arity_at_least = arity_at_least.into();
        globals.make_arity_at_least = make_arity_at_least;

        globals
    });
    
    /*
    manager.add_definition(
        thr,
        base,
        ("struct", Form::Primitive(compile_struct)),
        true,
        true,
    );*/
}

pub fn make_struct_values(
    ctx: &mut Context,
    mut struct_type: Handle<StructType>,
    _names: &[Value],
    mut count: usize,
    flags: i32,
) -> ScmResult<ArrayList<Value>> {
    let module = ctx.registers.module.get_handle_of::<Library>();

    if (flags & SCHEME_STRUCT_EXPTIME) != 0 {
        count -= 1;
    }

    let mut pos = 0;

    let mut values = ArrayList::with(ctx.mutator(), count, count, |_, _| Value::UNDEFINED);

    if (flags & SCHEME_STRUCT_NO_TYPE) == 0 {
        values[pos] = Value::new(struct_type);
        pos += 1;
    }

    if (flags & SCHEME_STRUCT_NO_CONSTR) == 0 {
        let constructor_code = Compiler::compile_custom(
            ctx,
            library_manager().scheme_module.get_handle_of(),
            |ctx, cc| {
                let arg_cnt = struct_type.field_cnt_for_instance();
                cc.emit(ctx, Ins::AssertArgCount(arg_cnt as _));
                let ix = cc.add_constant(ctx, struct_type);
                cc.emit(ctx, Ins::MakeStruct(ix as _));
                cc.emit(ctx, Ins::Return);

                Ok(())
            },
        )?;

        let constructor_proc = Procedure {
            id: Procedure::new_id(),
            kind: ProcedureKind::Closure(
                ClosureType::Constructor(struct_type),
                Value::nil(),
                None,
                constructor_code,
            ),
            module: ctx.registers.module.get_handle_of(),
        };

        let constructor_proc = ctx.mutator().allocate(constructor_proc);

        struct_type.constructor_proc = constructor_proc.into();

        values[pos] = Value::new(constructor_proc);
        pos += 1;
    }

    if (flags & SCHEME_STRUCT_NO_PRED) == 0 {
        let predicate_code = Compiler::compile_custom(ctx, module, |ctx, cc| {
            cc.emit(ctx, Ins::AssertArgCount(1));
            let ix = cc.add_constant(ctx, struct_type);
            cc.emit(ctx, Ins::CheckStruct(ix as _));
            cc.emit(ctx, Ins::Return);

            Ok(())
        })?;

        let predicate_proc = Procedure {
            id: Procedure::new_id(),
            kind: ProcedureKind::Closure(
                ClosureType::Predicate(struct_type),
                Value::nil(),
                None,
                predicate_code,
            ),
            module: ctx.registers.module.get_handle_of(),
        };

        let predicate_proc = ctx.mutator().allocate(predicate_proc);

        struct_type.predicate_proc = predicate_proc.into();

        values[pos] = Value::new(predicate_proc);
        pos += 1;
    }

    if (flags & SCHEME_STRUCT_GEN_GET) != 0 {
        count -= 1;
    }

    if (flags & SCHEME_STRUCT_GEN_SET) != 0 {
        count -= 1;
    }

    let mut slot_num = if let Some(super_type) = struct_type.super_type() {
        super_type.field_cnt_for_instance()
    } else {
        0
    };

    while pos < count {
        let library = ctx.registers.module.get_handle_of::<Library>();
        if (flags & SCHEME_STRUCT_NO_GET) == 0 {
            let accessor_code = Compiler::compile_custom(ctx, library, |ctx, cc| {
                cc.emit(ctx, Ins::AssertArgCount(1));
                let ix = cc.add_constant(ctx, struct_type);
                cc.emit(ctx, Ins::AssertStruct(0, ix as _));
                cc.emit(ctx, Ins::StructRefI(ix as _, slot_num as _));
                cc.emit(ctx, Ins::Return);
                Ok(())
            })?;

            let accessor_proc = Procedure {
                id: Procedure::new_id(),
                kind: ProcedureKind::Closure(
                    ClosureType::Accessor(struct_type),
                    Value::nil(),
                    None,
                    accessor_code,
                ),
                module: ctx.registers.module.get_handle_of(),
            };

            values[pos] = ctx.mutator().allocate(accessor_proc).into();
            pos += 1;
        }

        if (flags & SCHEME_STRUCT_NO_SET) == 0 {
            let mutator_code = Compiler::compile_custom(ctx, library, |ctx, cc| {
                cc.emit(ctx, Ins::AssertArgCount(2));
                let ix = cc.add_constant(ctx, struct_type);
                cc.emit(ctx, Ins::AssertStruct(0, ix as _));
                cc.emit(ctx, Ins::StructSetI(ix as _, slot_num as _));
                cc.emit(ctx, Ins::Return);
                Ok(())
            })?;

            let mutator_proc = Procedure {
                id: Procedure::new_id(),
                kind: ProcedureKind::Closure(
                    ClosureType::Mutator(struct_type),
                    Value::nil(),
                    None,
                    mutator_code,
                ),
                module: ctx.registers.module.get_handle_of(),
            };

            values[pos] = ctx.mutator().allocate(mutator_proc).into();
            pos += 1;
        }

        slot_num += 1;
    }

    if (flags & SCHEME_STRUCT_GEN_GET) != 0 {
        let accessor_code = Compiler::compile_custom(ctx, module, |ctx, cc| {
            cc.emit(ctx, Ins::AssertArgCount(2));
            let ix = cc.add_constant(ctx, struct_type);
            cc.emit(ctx, Ins::AssertStruct(0, ix as _));
            cc.emit(ctx, Ins::StructRef(ix as _));
            cc.emit(ctx, Ins::Return);

            Ok(())
        })?;

        let accessor_proc = Procedure {
            id: Procedure::new_id(),
            kind: ProcedureKind::Closure(
                ClosureType::Accessor(struct_type),
                Value::nil(),
                None,
                accessor_code,
            ),
            module: ctx.registers.module.get_handle_of(),
        };

        let accessor_proc = ctx.mutator().allocate(accessor_proc);

        struct_type.accessor_proc = accessor_proc.into();

        values[pos] = Value::new(accessor_proc);
        pos += 1;
    }
    if (flags & SCHEME_STRUCT_GEN_SET) != 0 {
        let mutator_code = Compiler::compile_custom(ctx, module, |ctx, cc| {
            cc.emit(ctx, Ins::AssertArgCount(3));
            let ix = cc.add_constant(ctx, struct_type);
            cc.emit(ctx, Ins::AssertStruct(0, ix as _));
            cc.emit(ctx, Ins::StructSet(ix as _));
            cc.emit(ctx, Ins::PushTrue);
            cc.emit(ctx, Ins::Return);

            Ok(())
        })?;

        let mutator_proc = Procedure {
            id: Procedure::new_id(),
            kind: ProcedureKind::Closure(
                ClosureType::Mutator(struct_type),
                Value::nil(),
                None,
                mutator_code,
            ),
            module: ctx.registers.module.get_handle_of(),
        };

        let mutator_proc = ctx.mutator().allocate(mutator_proc);
        struct_type.mutator_proc = mutator_proc.into();
        values[pos] = mutator_proc.into();
        pos += 1;
    }

    /*let values = Value::make_list_slice(ctx, &values[..pos], Value::nil());

    Ok(ctx.mutator().allocate(Values(values)).into())*/

    let mut vals = ArrayList::with_capacity(ctx.mutator(), pos);

    for v in &values[..pos] {
        vals.push(ctx.mutator(), *v);
    }

    Ok(vals)
}

// (make-struct-type <id> <parent-id> | #f <init-field-cnt> <props>? <guard>? )
pub fn make_struct_type(
    ctx: &mut Context,
    name: Value,
    struct_type: Value,
    initc: Value,
    rest: &Arguments,
) -> ScmResult {
    if !name.is_symbol() {
        let exc = Exception::type_error(ctx, &[Type::Symbol], name, SourcePosition::unknown());
        return Err(exc.into());
    }

    if !struct_type.is_false() && !struct_type.is_handle_of::<StructType>() {
        let exc = Exception::type_error(
            ctx,
            &[Type::StructType(None)],
            struct_type,
            SourcePosition::unknown(),
        );
        return Err(exc.into());
    }

    if !initc.is_int32() {
        let exc = Exception::type_error(ctx, &[Type::Integer], initc, SourcePosition::unknown());
        return Err(exc.into());
    }

    let props;
    let guard;

    if rest.len() > 0 {
        props = rest[0];

        let mut l = props;

        while l.is_pair() {
            let a = l.car();

            if !a.is_pair() || !a.car().is_handle_of::<StructProperty>() {
                break;
            }

            l = l.cdr();
        }

        if !l.is_null() {
            let exc = Exception::type_error(ctx, &[Type::ProperList], l, SourcePosition::unknown());
            return Err(exc.into());
        }

        if rest.len() > 1 {
            guard = rest[1];
            if !guard.is_handle_of::<Procedure>() {
                let exc = Exception::type_error(
                    ctx,
                    &[Type::Procedure],
                    guard,
                    SourcePosition::unknown(),
                );
                return Err(exc.into());
            }
        } else {
            guard = Value::nil();
        }
    } else {
        props = Value::nil();
        guard = Value::nil();
    }

    let stype = make_struct_type_internal(
        ctx,
        name.get_symbol(),
        if struct_type.is_false() {
            None
        } else {
            Some(struct_type.get_handle_of::<StructType>())
        },
        initc.get_int32() as usize,
        props,
        guard,
    )?;

    let mut i = 0;
    let names = make_struct_names(
        ctx.mutator(),
        stype.name().into(),
        Value::nil(),
        SCHEME_STRUCT_GEN_GET | SCHEME_STRUCT_GEN_SET,
        Some(&mut i),
    );

    let arr = make_struct_values(
        ctx,
        stype,
        &names,
        i as _,
        SCHEME_STRUCT_GEN_GET | SCHEME_STRUCT_GEN_SET,
    )?;

    let ls = Value::make_list_arraylist(ctx, &arr, Value::nil());

    Ok(ctx.mutator().allocate(Values(ls)).into())
}

pub fn make_struct_names(
    thr: &mut Thread,
    base: Value,
    field_symbols: Value,
    flags: i32,
    count_out: Option<&mut u32>,
) -> ArrayList<Value> {
    let len;

    len = if field_symbols.is_pair() {
        field_symbols.length()
    } else {
        0
    };

    _make_struct_names(
        thr,
        &**base.get_symbol().identifier(),
        len as _,
        field_symbols,
        &[],
        flags,
        count_out,
    )
}

/*

// (struct <id> <super-type> (<field> <field> ...))
pub fn compile_struct(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    _: bool,
) -> ScmResult<bool> {
    if form.is_pair() {
        if form.cdr().is_pair() {
            let name = form.cdr().car();
            if !name.is_symbol() {
                let exc =
                    Exception::type_error(ctx, &[Type::Symbol], name, SourcePosition::unknown());
                return Err(exc.into());
            }

            let rest = form.cdr().cdr();

            if rest.is_pair() {
                let (super_type, fields) = if rest.cdr().is_pair() {
                    let super_type = rest.car();
                    let fields = rest.cdr().car();
                    (Some(super_type), fields)
                } else {
                    (None, rest.car())
                };

                let super_type = if let Some(super_type) = super_type {
                    if !super_type.is_symbol() {
                        let exc = Exception::type_error(
                            ctx,
                            &[Type::Symbol],
                            super_type,
                            SourcePosition::unknown(),
                        );
                        return Err(exc.into());
                    }

                    let val = library_manager().find_binding(
                        cc.library,
                        super_type.get_symbol(),
                        false,
                        false,
                    );

                    if let Some(val) = val.map(|x| Gloc::get(ctx, x)).transpose()? {
                        if val.is_handle_of::<StructType>() {
                            Some(val.get_handle_of::<StructType>())
                        } else if val.is_handle_of::<Procedure>() {
                            if let ProcedureKind::Closure(
                                ClosureType::Constructor(ref st),
                                _,
                                _,
                                _,
                            ) = val.get_handle_of::<Procedure>().kind
                            {
                                Some(*st)
                            } else {
                                let exc = Exception::custom(
                                    ctx,
                                    "struct error",
                                    "parent struct type not defined in: $0",
                                    &[super_type],
                                    SourcePosition::unknown(),
                                );
                                return Err(exc.into());
                            }
                        } else {
                            let exc = Exception::custom(
                                ctx,
                                "struct error",
                                "parent struct type not defined in: $0",
                                &[super_type],
                                SourcePosition::unknown(),
                            );
                            return Err(exc.into());
                        }
                    } else {
                        let exc = Exception::custom(
                            ctx,
                            "struct error",
                            "parent struct type not defined in: $0",
                            &[super_type],
                            SourcePosition::unknown(),
                        );
                        return Err(exc.into());
                    }
                } else {
                    None
                };

                let mut syms = ArrayList::with_capacity(ctx.mutator(), 4);

                let mut f = fields;
                while f.is_pair() {
                    let name = f.car();
                    let rest = f.cdr();

                    syms.push(ctx.mutator(), name);

                    f = rest;
                }

                if !f.is_null() {
                    let exc = Exception::eval(
                        ctx,
                        EvalError::MalformedBindings,
                        &[fields],
                        SourcePosition::unknown(),
                    );
                    return Err(exc.into());
                }

                let struct_type = make_struct_type(
                    ctx,
                    name,
                    super_type
                        .map(|x| Value::new(x))
                        .unwrap_or(Value::new(false)),
                    Some(Value::new(syms.len() as i32)),
                    None,
                )?;

                let values = struct_type.get_handle_of::<Values>().0;

                let struct_type = values.car();
                let constructor = values.cdr().car();
                let predicate = values.cdr().cdr().car();
                let _accessor = values.cdr().cdr().cdr().car();
                let _mutator_proc = values.cdr().cdr().cdr().cdr().car();

                let mut off = super_type.map(|x| x.field_cnt_for_instance()).unwrap_or(0);
                if let Some(sup) = super_type {
                    println!("SUPER FIELD COUNT: {}", sup.field_cnt_for_instance());
                }
                let struct_name = ctx
                    .runtime()
                    .symbol_table()
                    .intern(format!("struct:{}", name.get_symbol()));

                if let None = library_manager().insert_binding(
                    ctx,
                    cc.library,
                    struct_name,
                    struct_type,
                    false,
                )? {
                    let exc = Exception::custom(
                        ctx,
                        "library",
                        "duplicate binding: $0",
                        &[name],
                        SourcePosition::unknown(),
                    );
                    return Err(exc.into());
                }

                if let None = library_manager().insert_binding(
                    ctx,
                    cc.library,
                    name.get_symbol(),
                    constructor,
                    false,
                )? {
                    let exc = Exception::custom(
                        ctx,
                        "library",
                        "duplicate binding: $0",
                        &[name],
                        SourcePosition::unknown(),
                    );
                    return Err(exc.into());
                }

                let pred_name = ctx
                    .runtime()
                    .symbol_table()
                    .intern(format!("{}?", name.get_symbol()));

                if let None = library_manager()
                    .insert_binding(ctx, cc.library, pred_name, predicate, false)?
                {
                    let exc = Exception::custom(
                        ctx,
                        "library",
                        "duplicate binding: $0",
                        &[name],
                        SourcePosition::unknown(),
                    );
                    return Err(exc.into());
                }

                for fname in syms.iter() {
                    if !fname.is_symbol() {
                        let exc = Exception::eval(
                            ctx,
                            EvalError::MalformedBinding,
                            &[*fname, fields],
                            SourcePosition::unknown(),
                        );
                        return Err(exc.into());
                    }

                    let fname = fname.get_symbol();

                    let mutator_name = ctx.runtime().symbol_table().intern(format!(
                        "set-{}-{}!",
                        name.to_string(false),
                        fname
                    ));
                    let accessor_name = ctx.runtime().symbol_table().intern(format!(
                        "{}-{}",
                        name.to_string(false),
                        fname
                    ));



                    let mutator_code = Compiler::compile_custom(ctx, cc.library, |ctx, cc| {
                        cc.emit(ctx, Ins::AssertArgCount(2));
                        let ix = cc.add_constant(ctx, struct_type);
                        cc.emit(ctx, Ins::AssertStruct(0, ix as _));

                        cc.emit(ctx, Ins::StructSetI(ix as _, off as _));
                        cc.emit(ctx, Ins::PushTrue);
                        cc.emit(ctx, Ins::Return);
                        Ok(())
                    })?;

                    let accessor_proc = ctx.mutator().allocate(Procedure {
                        id: Procedure::new_id(),
                        kind: ProcedureKind::Closure(
                            ClosureType::Accessor(struct_type.get_handle_of()),
                            Value::nil(),
                            None,
                            accessor_code,
                        ),
                        module: cc.library,
                    });

                    let mutator_proc = ctx.mutator().allocate(Procedure {
                        id: Procedure::new_id(),
                        kind: ProcedureKind::Closure(
                            ClosureType::Mutator(struct_type.get_handle_of()),
                            Value::nil(),
                            None,
                            mutator_code,
                        ),
                        module: cc.library,
                    });

                    if let None = library_manager().insert_binding(
                        ctx,
                        cc.library,
                        accessor_name,
                        Value::new(accessor_proc),
                        false,
                    )? {
                        let exc = Exception::custom(
                            ctx,
                            "library",
                            "duplicate binding: $0",
                            &[Value::new(accessor_name)],
                            SourcePosition::unknown(),
                        );
                        return Err(exc.into());
                    }

                    if let None = library_manager().insert_binding(
                        ctx,
                        cc.library,
                        mutator_name,
                        Value::new(mutator_proc),
                        false,
                    )? {
                        let exc = Exception::custom(
                            ctx,
                            "library",
                            "duplicate binding: $0",
                            &[Value::new(mutator_name)],
                            SourcePosition::unknown(),
                        );
                        return Err(exc.into());
                    }

                    off += 1;
                }

                cc.emit(ctx, Ins::PushVoid);

                return Ok(false);
            }
        }
    }

    let exc = Exception::argument_count(ctx, Some("struct"), 2, 3, form, SourcePosition::unknown());

    Err(exc.into())
}
*/
pub fn make_struct_type_property(ctx: &mut Context, name: Value, rest: &Arguments) -> ScmResult {
    let guard;
    let mut supers;
    if !name.is_symbol() {
        let exc = Exception::type_error(ctx, &[Type::Symbol], name, SourcePosition::unknown());
        return Err(exc.into());
    }
    if rest.is_empty() {
        guard = Value::new(false);
        supers = Value::nil();
    } else {
        guard = rest[0];

        if rest.len() == 2 {
            supers = rest[1];
        } else {
            supers = Value::nil();
        }

        if rest.len() > 2 {
            let list = Value::make_list_slice(ctx, &rest, Value::nil());
            let exc = Exception::argument_count(
                ctx,
                Some("make-struct-type-property"),
                1,
                3,
                list,
                SourcePosition::unknown(),
            );
            return Err(exc.into());
        }
    }

    if !(guard.is_handle_of::<Procedure>() || guard.is_false()) {
        let exc = Exception::type_error(
            ctx,
            &[Type::Procedure, Type::Boolean],
            guard,
            SourcePosition::unknown(),
        );
        return Err(exc.into());
    }

    // create super properties
    let mut new_supers = Value::nil();
    let orig = supers;
    while supers.is_pair() {
        let prop_proc = supers.car();

        if !prop_proc.is_pair() {
            let exc =
                Exception::type_error(ctx, &[Type::Pair], prop_proc, SourcePosition::unknown());
            return Err(exc.into());
        }

        let prop = prop_proc.car();
        let proc = prop_proc.cdr();

        if !prop.is_handle_of::<StructProperty>() {
            let exc = Exception::type_error(ctx, &[Type::Symbol], prop, SourcePosition::unknown());
            return Err(exc.into());
        }

        if !proc.is_handle_of::<Procedure>() {
            let exc =
                Exception::type_error(ctx, &[Type::Procedure], proc, SourcePosition::unknown());
            return Err(exc.into());
        }

        let pair = ctx.make_pair(prop, proc);
        new_supers = ctx.make_pair(pair, new_supers);

        supers = supers.cdr();
    }

    if !supers.is_null() {
        let exc = Exception::type_error(ctx, &[Type::ProperList], orig, SourcePosition::unknown());
        return Err(exc.into());
    }

    let property = StructProperty {
        name: name.get_symbol(),
        supers: new_supers,
        guard,
        contract_name: Value::nil(),
    };

    let handle = ctx.mutator().allocate(property);

    let accessor_code =
        Compiler::compile_custom(ctx, ctx.registers.module.get_handle_of(), |ctx, cc| {
            let ix = cc.add_constant(ctx, handle);
            let br_ip = cc.emit(ctx, Ins::NoOp);
            cc.emit(ctx, Ins::AssertMinArgCount(1));
            cc.emit(
                ctx,
                Ins::StructPropertyAccessor(ix as _, false /* do not handle failure-result */),
            );
            cc.emit(ctx, Ins::Return);
            let off = cc.offset_to_next(br_ip as _);
            cc.code[br_ip] = Ins::BranchIfArgMismatch(off as _, 1);
            cc.emit(ctx, Ins::AssertArgCount(2));
            cc.emit(ctx, Ins::StructPropertyAccessor(ix as _, true));
            cc.emit(ctx, Ins::Return);

            Ok(())
        })?;

    let predicate_code =
        Compiler::compile_custom(ctx, ctx.registers.module.get_handle_of(), |ctx, cc| {
            let ix = cc.add_constant(ctx, handle);
            cc.emit(ctx, Ins::CheckStructProperty(ix as _));
            cc.emit(ctx, Ins::Return);
            Ok(())
        })?;

    let module = ctx.registers.module.get_handle_of();
    let accessor_proc = ctx.mutator().allocate(Procedure {
        kind: ProcedureKind::Closure(
            ClosureType::PropertyAccessor(handle),
            Value::nil(),
            None,
            accessor_code,
        ),
        id: Procedure::new_id(),
        module,
    });

    let predicate_proc = ctx.mutator().allocate(Procedure {
        kind: ProcedureKind::Closure(
            ClosureType::PropertyPredicate(handle),
            Value::nil(),
            None,
            predicate_code,
        ),
        id: Procedure::new_id(),
        module,
    });

    let values = Value::make_list_slice(
        ctx,
        &[
            Value::new(handle),
            Value::new(predicate_proc),
            Value::new(accessor_proc),
        ],
        Value::nil(),
    );

    Ok(ctx.mutator().allocate(Values(values)).into())
}

pub fn make_struct_type_from_string(
    ctx: &mut Context,
    name: &str,
    parent: Option<Handle<StructType>>,
    num_fields: usize,
    props: Value,
    guard: Value,
) -> ScmResult<Handle<StructType>> {
    let name = ctx.runtime().symbol_table().intern(name);

    let r = make_struct_type_internal(ctx, name, parent, num_fields, props, guard)?;

    Ok(r)
}

pub fn make_struct_names_from_array(
    ctx: &mut Context,
    base: &str,
    fcount: u32,
    fields: &[&str],
    flags: i32,
    count_out: Option<&mut u32>,
) -> ArrayList<Value> {
    _make_struct_names(
        ctx.mutator(),
        base,
        fcount,
        Value::nil(),
        &fields,
        flags,
        count_out,
    )
}

pub fn apply_guards(
    ctx: &mut Context,
    stype: Handle<StructType>,
    args: &Arguments,
) -> ScmResult<ArrayList<Value>> {
    /*let mut p = Some(stype);

    let mut prev_guards = None;
    let mut guard;

    while let Some(stype) = p {
        if stype.guard.is_some() || prev_guards.is_some() {

            let mut got;

            if prev_guards.is_none() {
                prev_guards = Some(Value::nil());
            }

            while let Some(prev_guards) = prev_guards {
                if prev_guards.is_pair() {
                    guard = prev_guards.car();
                } else {
                    guard = stype.guard.map(|x| Value::new(x)).unwrap_or(Value::nil());
                }

                if guard.is_handle_of::<Procedure>() {
                    let gcount = stype.init_field_cnt()
                }
            }
        }
    }*/
    let _ = ctx;
    let _ = stype;
    let _ = args;
    todo!()
}

pub fn make_struct_type_internal(
    ctx: &mut Context,
    name: Handle<Symbol>,
    parent: Option<Handle<StructType>>,
    num_fields: usize,
    props: Value,
    guard: Value,
) -> ScmResult<Handle<StructType>> {
    let sprops = ArrayList::new(ctx.mutator());
    let sprops = ctx.mutator().allocate(sprops);
    let mut struct_type = ctx.mutator().allocate(StructType::new(
        name,
        num_fields as _,
        Value::UNDEFINED,
        Value::UNDEFINED,
        Value::UNDEFINED,
        Value::UNDEFINED,
        parent,
        sprops,
        None,
    ));

    if let Some(parent_type) = parent {
        struct_type.props = parent_type.props;
    }

    if !props.is_null() {
        let mut num_props = count_non_proc_props(props);

        let mut can_override = HashMap::with_hasher_and_capacity(RandomState::new(), 4);
        let mut i = 0;
        while i < struct_type.props.len() {
            let prop = struct_type.props[i];
            can_override.put(ctx.mutator(), prop.car(), Value::new(true));
            i += 1;
        }

        let mut pa =
            ArrayList::<Value>::with_capacity(ctx.mutator(), struct_type.props.len() + num_props);

        pa.resize(
            ctx.mutator(),
            struct_type.props.len() + num_props,
            Value::UNDEFINED,
        );
        num_props = i;
        if struct_type.props.len() != 0 {
            for i in 0..struct_type.props.len() {
                let prop = struct_type.props[i];
                pa[i] = prop;
            }
        }

        let mut l = props;

        while l.is_pair() {
            let mut skip_supers = 0;

            let mut a = l.car();
            let prop = a.car().get_handle_of::<StructProperty>();

            let propv = guard_property(ctx, prop, a.cdr(), struct_type)?;

            let mut j = 0;
            while j < num_props {
                if pa[j as usize].car().raw() == prop.as_ptr() as u64 {
                    break;
                }
                j += 1;
            }

            if j < num_props {
                if can_override.get(&Value::new(prop)).is_none() {
                    if propv.raw() != pa[j].cdr().raw() {
                        break;
                    }
                    skip_supers = 1;
                }
            } else {
                num_props += 1;
            }

            l = l.cdr();

            if skip_supers == 0 {
                l = append_super_props(ctx, prop, propv, l)?;
            }

            a = ctx.make_pair(prop.into(), propv);
            pa[j] = a;
        }

        if num_props != 0 {
            struct_type.props = ctx.mutator().allocate(pa);
        }

        if !l.is_null() {
            let a = l.car();
            let exc = Exception::eval(
                ctx,
                EvalError::DuplicateBinding,
                &[a, l],
                SourcePosition::unknown(),
            );

            return Err(exc.into());
        }
    }

    if guard.is_handle_of::<Procedure>() {
        let proc = guard.get_handle_of::<Procedure>();

        if !ctx.check_arity(proc, struct_type.field_cnt_for_instance() as usize + 1) {
            todo!("arity mismatch");
        }

        struct_type.guard = Some(proc);
    }

    Ok(struct_type)
}

pub fn count_non_proc_props(mut props: Value) -> usize {
    let mut p;
    let mut v;
    let mut count = 0;

    while props.is_pair() {
        v = props.car();
        p = v.car().get_handle_of::<StructProperty>();

        count += 1;
        if !p.supers.is_null() {
            count += count_non_proc_props(p.supers);
        }

        props = props.cdr();
    }

    count
}

pub fn guard_property(
    ctx: &mut Context,
    prop: Handle<StructProperty>,
    v: Value,
    t: Handle<StructType>,
) -> ScmResult {
    if prop.guard.is_handle_of::<Procedure>() {
        let a = get_struct_type_info(ctx, t)?;
        let l = Value::make_list_slice(ctx, &a, Value::nil());

        let args = Value::make_list_slice(ctx, &[v, l], Value::nil());

        return ctx.apply(prop.guard, args);
    } else {
        Ok(v)
    }
}

pub fn get_struct_type_info(
    _ctx: &mut Context,
    stype: Handle<StructType>,
) -> ScmResult<[Value; 5]> {
    Ok([
        stype.name().into(),
        Value::new(stype.init_field_cnt as i32),
        stype.accessor_proc(),
        stype.mutator_proc(),
        stype
            .super_type()
            .map(|x| Value::new(x))
            .unwrap_or(Value::nil()),
    ])
}

pub fn append_super_props(
    ctx: &mut Context,
    p: Handle<StructProperty>,
    arg: Value,
    orig: Value,
) -> ScmResult {
    let mut first = Value::nil();
    let mut last = Value::nil();

    if p.supers.is_pair() {
        let mut props = p.supers;

        while props.is_list() {
            let v = props.car();
            let args = Value::make_list_slice(ctx, &[arg], Value::nil());
            let res = ctx.apply(v.cdr(), args)?;
            let v = ctx.make_pair(v, res);
            let pr = ctx.make_pair(v, Value::nil());

            if last.is_pair() {
                last.pair().cdr = pr;
            } else {
                first = pr;
            }
            last = pr;

            props = props.cdr();
        }
    }

    if last.is_pair() {
        last.pair().cdr = orig;
        return Ok(first);
    } else {
        Ok(orig)
    }
}

pub const SCHEME_STRUCT_NO_TYPE: i32 = 0x01;
pub const SCHEME_STRUCT_NO_CONSTR: i32 = 0x02;
pub const SCHEME_STRUCT_NO_PRED: i32 = 0x04;
pub const SCHEME_STRUCT_NO_GET: i32 = 0x08;
pub const SCHEME_STRUCT_NO_SET: i32 = 0x10;
pub const SCHEME_STRUCT_GEN_GET: i32 = 0x20;
pub const SCHEME_STRUCT_GEN_SET: i32 = 0x40;
pub const SCHEME_STRUCT_EXPTIME: i32 = 0x80;
pub const SCHEME_STRUCT_NO_MAKE_PREFIX: i32 = 0x100;
pub const SCHEME_STRUCT_NAMES_ARE_STRINGS: i32 = 0x200;
pub const SCHEME_STRUCT_BUILTIN: i32 = 0x400;

fn _make_struct_names(
    thr: &mut Thread,
    base: &str,
    fcount: u32,
    mut field_symbols: Value,
    field_strings: &[&str],
    flags: i32,
    mut count_out: Option<&mut u32>,
) -> ArrayList<Value> {
    let mut count = 0;
    if (flags & SCHEME_STRUCT_NO_TYPE) == 0 {
        count += 1;
    }

    if (flags & SCHEME_STRUCT_NO_CONSTR) == 0 {
        count += 1;
    }

    if (flags & SCHEME_STRUCT_NO_PRED) == 0 {
        count += 1;
    }

    if (flags & SCHEME_STRUCT_NO_GET) == 0 {
        count += fcount;
    }

    if (flags & SCHEME_STRUCT_NO_SET) == 0 {
        count += fcount;
    }

    if (flags & SCHEME_STRUCT_GEN_GET) != 0 {
        count += 1;
    }

    if (flags & SCHEME_STRUCT_GEN_SET) != 0 {
        count += 1;
    }

    if (flags & SCHEME_STRUCT_EXPTIME) != 0 {
        count += 1;
    }

    if let Some(count_out) = count_out.take() {
        *count_out = count;
    }

    let mut pos = 0;
    let mut names = ArrayList::with(thr, count as _, count as _, |_, _| Value::UNDEFINED);
    let as_sym = (flags & SCHEME_STRUCT_NAMES_ARE_STRINGS) == 0;
    if (flags & SCHEME_STRUCT_NO_TYPE) == 0 {
        let nm = make_name(thr, "struct:", base, "", "", as_sym);
        names[pos] = nm;
        pos += 1;
    }

    if (flags & SCHEME_STRUCT_NO_CONSTR) == 0 {
        if (flags & SCHEME_STRUCT_NO_MAKE_PREFIX) != 0 {
            let nm = make_name(thr, "", base, "", "", as_sym);
            names[pos] = nm;
            pos += 1;
        } else {
            let nm = make_name(thr, "make", "-", base, "", as_sym);
            names[pos] = nm;
            pos += 1;
        }
    }

    if (flags & SCHEME_STRUCT_NO_PRED) == 0 {
        let nm = make_name(thr, "", base, "?", "", as_sym);
        names[pos] = nm;
        pos += 1;
    }

    if fcount != 0 {
        for slot_num in 0..fcount {
            let field_name = if field_symbols.is_pair() {
                let fname = field_symbols.car();
                field_symbols = field_symbols.cdr();

                fname.get_symbol().identifier().to_string()
            } else {
                field_strings[slot_num as usize].to_string()
            };

            if (flags & SCHEME_STRUCT_NO_GET) == 0 {
                let nm = make_name(thr, base, "-", &field_name, "", as_sym);
                names[pos] = nm;
                pos += 1;
            }

            if (flags & SCHEME_STRUCT_NO_SET) == 0 {
                let nm = make_name(
                    thr,
                    "set-",
                    &format!("{}-{}", base, field_name),
                    "!",
                    "",
                    as_sym,
                );
                names[pos] = nm;
                pos += 1;
            }
        }
    }

    if (flags & SCHEME_STRUCT_GEN_GET) != 0 {
        let nm = make_name(thr, base, "-", "ref", "", as_sym);
        names[pos] = nm;
        pos += 1;
    }

    if (flags & SCHEME_STRUCT_GEN_SET) != 0 {
        let nm = make_name(thr, base, "-", "set", "!", as_sym);
        names[pos] = nm;
        pos += 1;
    }

    if (flags & SCHEME_STRUCT_EXPTIME) != 0 {
        let nm = make_name(thr, base, "", "", "", as_sym);
        names[pos] = nm;
    }

    names
}

pub fn make_name(
    thr: &mut Thread,
    base: &str,
    field: &str,
    postifx: &str,
    postfix2: &str,
    as_sym: bool,
) -> Value {
    let name = format!("{}{}{}{}", base, field, postifx, postfix2);
    if as_sym {
        Runtime::get().symbol_table().intern(name).into()
    } else {
        Str::new(thr, name).into()
    }
}
