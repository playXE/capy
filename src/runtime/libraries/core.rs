use r7rs_parser::{expr::NoIntern, parser::ParseError};

use crate::{
    compiler::{r7rs_to_value, Compiler},
    data::{
        exception::{Exception, SourcePosition},
        special_form::Form,
    },
    prelude::{code::Ins, eval_error::EvalError, *},
    utilities::arraylist::ArrayList,
};

pub(crate) fn core_library(rt: &mut Runtime) {
    let manager = library_manager();
    let thr = Thread::current();

    let base = manager.scheme_module.get_handle_of::<Library>();
    let thread = Thread::current();
    let name = Str::new(thread, "<loader>");
    let loader = Procedure {
        kind: ProcedureKind::Primitive(name, Implementation::Apply(compile_and_eval_first), None),
        id: Procedure::new_id(),
        module: base,
    };

    let loader = thread.allocate(loader);
    rt.loader = Value::new(loader);

    manager.add_definition(thr, base, ("load", Implementation::Apply(load)), true, true);
    let id = manager.add_definition(
        thr,
        base,
        ("identity", Implementation::Native1(identity)),
        true,
        true,
    );

    rt.identity = id;
    manager.add_definition(
        thr,
        base,
        Definition::NativeSpecial("null?", Implementation::Native1(is_null), compile_is_null),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        Definition::NativeSpecial("pair?", Implementation::Native1(is_pair), compile_is_pair),
        true,
        true,
    );

    
    manager.add_definition(
        thr,
        base,
        ("symbol?", Implementation::Native1(is_symbol)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        ("string?", Implementation::Native1(is_string)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        ("fixnum?", Implementation::Native1(is_fixnum)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        ("flonum?", Implementation::Native1(is_flonum)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        ("list?", Implementation::Native1(is_list)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        ("vector?", Implementation::Native1(is_vector)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        ("length", Implementation::Native1(length)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        ("list", Implementation::Native0R(list)),
        true,
        true 
    );

    manager.add_definition(thr, base, ("make-parameter", Implementation::Native1O(make_parameter)), true, true);

    manager.add_definition(thr, base, Definition::NativeSpecial("car", Implementation::Native1(car), compile_car), true, true);

    manager.add_definition(thr, base, Definition::NativeSpecial("cdr", Implementation::Native1(cdr), compile_cdr), true, true);

    manager.add_definition(
        thr,
        base,
        Definition::NativeSpecial("cons", Implementation::Native2(cons), compile_cons),
        true,
        true,
    );

    manager.add_definition(thr, base, ("eq?", Implementation::Native2(eq)), true, true);

    manager.add_definition(
        thr,
        base,
        ("eqv?", Implementation::Native2(eqv)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        ("equal?", Implementation::Native2(equal)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        ("_dbg", Implementation::Native0R(dbg)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        ("set-car!", Implementation::Native2(set_car)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        ("set-cdr!", Implementation::Native2(set_cdr)),
        true,
        true,
    );

    let keywords = manager.keyword_module.get_handle_of::<Library>();
    manager.add_definition(
        thr,
        keywords,
        ("lambda", Form::Primitive(compile_lambda)),
        true,
        true,
    );
    manager.add_definition(
        thr,
        keywords,
        ("define", Form::Primitive(compile_define)),
        true,
        true,
    );
    manager.add_definition(
        thr,
        keywords,
        ("define-const", Form::Primitive(compile_define_const)),
        true,
        true,
    );
    manager.add_definition(
        thr,
        keywords,
        ("quote", Form::Primitive(compile_quote)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        keywords,
        ("define-library", Form::Primitive(compile_define_library)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        keywords,
        ("values", Form::Primitive(compile_values)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        keywords,
        ("define-values", Form::Primitive(compile_define_values)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        keywords,
        ("define-syntax", Form::Primitive(compile_define_syntax)),
        true,
        true,
    );
    manager.add_definition(
        thr,
        keywords,
        ("set!", Form::Primitive(compile_set)),
        true,
        true,
    );
}

pub fn compile_and_eval_first(
    ctx: &mut Context,
    args: &Arguments,
) -> ScmResult<(Handle<Procedure>, ArrayList<Value>)> {
    if args.len() != 3 {
        let ls = Value::make_list_slice(ctx, args, Value::nil());
        let exc = Exception::argument_count(ctx, None, 3, 3, ls, SourcePosition::unknown());
        return ctx.error(exc);
    }

    let form = args[0];
    let source_dir = args[1];
    let library = args[2];

    let source_dir = if source_dir.is_string() {
        Some(source_dir.get_string())
    } else {
        None
    };

    let library = if library.is_handle_of::<Library>() {
        library.get_handle_of::<Library>()
    } else {
        library_manager().user_module.get_handle_of::<Library>()
    };

    let code = Compiler::build(ctx, form, library, source_dir)?;

    let named = ClosureType::Named(Str::new(ctx.mutator(), "<loader>"));

    let proc = Procedure {
        kind: ProcedureKind::Closure(named, Value::nil(), None, code),
        id: Procedure::new_id(),
        module: library,
    };
    let args = ArrayList::new(ctx.mutator());
    Ok((ctx.mutator().allocate(proc), args))
}

pub fn load(
    ctx: &mut Context,
    args: &Arguments,
) -> ScmResult<(Handle<Procedure>, ArrayList<Value>)> {
    if args.len() != 1 || args.len() != 2 {
        let ls = Value::make_list_slice(ctx, args, Value::nil());
        let exc = Exception::argument_count(ctx, None, 1, 2, ls, SourcePosition::unknown());
        return ctx.error(exc);
    }

    let path = args.first().unwrap().to_string(false);

    let file_manager = ctx.runtime().file_manager.lock(true);
    let current = std::env::current_dir();
    let cur = current.as_ref().unwrap().to_str();
    let filename = file_manager
        .file_path(&path, cur)
        .or_else(|| file_manager.library_file_path(&path, cur))
        .unwrap_or(path);

    let library = if args.len() == 2 {
        if args[1].is_handle_of::<Library>() {
            args[1].get_handle_of::<Library>()
        } else {
            library_manager().user_module.get_handle_of::<Library>()
        }
    } else {
        library_manager().user_module.get_handle_of::<Library>()
    };

    drop(file_manager);

    let source = match std::fs::read_to_string(&filename) {
        Ok(source) => source,
        Err(_) => {
            let exc = Exception::eval(
                ctx,
                EvalError::LibraryNotFound,
                &[args[0]],
                SourcePosition::unknown(),
            );
            return ctx.error(exc);
        }
    };
    let mut i = NoIntern;
    let mut parser = r7rs_parser::parser::Parser::new(&mut i, &source, false);
    let mut exprs = Value::nil();

    while !parser.finished() {
        match parser.parse(true) {
            Ok(expr) => {
                let val = r7rs_to_value(ctx, u32::MAX, &expr);
                exprs = ctx.make_pair(val, exprs);
            }
            Err(err) => match err {
                ParseError::Lexical(pos, lexical) => {
                    let exc = Exception::lexical(
                        ctx,
                        lexical,
                        &[],
                        SourcePosition {
                            source_id: u32::MAX,
                            position: pos,
                        },
                    );

                    return ctx.error(exc);
                }

                ParseError::Syntax(pos, syntax) => {
                    let exc = Exception::syntax(
                        ctx,
                        syntax,
                        &[],
                        SourcePosition {
                            source_id: u32::MAX,
                            position: pos,
                        },
                    );

                    return ctx.error(exc);
                }
            },
        }
    }

    let loader = ctx.runtime().loader.get_handle_of::<Procedure>();
    let mut args = ArrayList::new(ctx.mutator());
    let dir = std::path::Path::new(&filename)
        .parent()
        .unwrap()
        .to_str()
        .unwrap();
    let dir = Str::new(ctx.mutator(), dir);
    args.push(ctx.mutator(), exprs);
    args.push(ctx.mutator(), Value::new(dir));
    args.push(ctx.mutator(), Value::new(library));

    Ok((loader, args))
}

pub fn compile_lambda(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    _: bool,
) -> ScmResult<bool> {
    let (form, _) = Compiler::desyntax(form);
    if form.is_pair() {
        let rest = form.cdr();
        if rest.is_pair() {
            cc.compile_lambda(
                ctx,
                None,
                rest.car(),
                rest.cdr(),
                false,
                false,
                false,
                false,
            )?;

            return Ok(false);
        }
    }
    let exc = Exception::argument_count(ctx, Some("lambda"), 1, 0, form, SourcePosition::unknown());
    ctx.error(exc)
}

pub fn compile_define(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    _: bool,
) -> ScmResult<bool> {
    let (form, pos) = Compiler::desyntax(form);
    cc.check_toplevel(ctx, form, pos)?;

    if form.is_pair() {
        let (rest, _) = Compiler::desyntax(form.cdr());
        if rest.is_pair() {
            let (sig, _) = Compiler::desyntax(rest.car());
            let (def, _) = Compiler::desyntax(rest.cdr());

            if sig.is_symbol() {
                if def.is_pair() && def.cdr().is_null() {
                    cc.compile(ctx, def.car(), false)?;
                } else {
                    let ls = ctx.make_pair(def, Value::nil());
                    let exc = Exception::eval(ctx, EvalError::MalformedDefinition, &[ls], pos);
                    return ctx.error(exc);
                }

                library_manager().insert_binding(
                    ctx,
                    cc.library,
                    sig.get_symbol(),
                    Value::UNDEFINED,
                    false,
                )?;

                let identifier = Value::new(cc.make_identifier(ctx, sig.get_symbol()));

                let ix = cc.add_constant(ctx, identifier);

                cc.emit(ctx, Ins::DefineGlobal(ix as _, false));
                return Ok(false);
            } else if sig.is_pair() {
                let (sym, _) = Compiler::desyntax(sig.car());
                let (arglist, _) = Compiler::desyntax(sig.cdr());

                let ix = cc.add_constant(ctx, sym);
                cc.compile_lambda(ctx, Some(ix), arglist, def, false, false, false, false)?;
                library_manager().insert_binding(
                    ctx,
                    cc.library,
                    sym.get_symbol(),
                    Value::UNDEFINED,
                    false,
                )?;

                let identifier = cc.make_identifier(ctx, sym.get_symbol());

                let ix = cc.add_constant(ctx, Value::new(identifier));

                cc.emit(ctx, Ins::DefineGlobal(ix as _, false));
                return Ok(false);
            }
        }
    }
    let exc = Exception::argument_count(ctx, Some("define"), 2, 2, form, pos);
    ctx.error(exc)
}

pub fn compile_define_const(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    _: bool,
) -> ScmResult<bool> {
    let (form, pos) = Compiler::desyntax(form);
    cc.check_toplevel(ctx, form, pos)?;

    if form.is_pair() {
        let (rest, _) = Compiler::desyntax(form.cdr());
        if rest.is_pair() {
            let (sig, _) = Compiler::desyntax(rest.car());
            let (def, _) = Compiler::desyntax(rest.cdr());

            if sig.is_symbol() {
                if def.is_pair() && def.cdr().is_null() {
                    cc.compile(ctx, def.car(), false)?;
                } else {
                    let ls = ctx.make_pair(def, Value::nil());
                    let exc = Exception::eval(ctx, EvalError::MalformedDefinition, &[ls], pos);
                    return ctx.error(exc);
                }

                library_manager().insert_binding(
                    ctx,
                    cc.library,
                    sig.get_symbol(),
                    Value::UNDEFINED,
                    true,
                )?;

                let identifier = cc.make_identifier(ctx, sig.get_symbol());

                let ix = cc.add_constant(ctx, identifier);
                cc.emit(ctx, Ins::DefineGlobal(ix as _, true));
                return Ok(false);
            } else if sig.is_pair() {
                let (sym, _) = Compiler::desyntax(sig.car());
                let (arglist, _) = Compiler::desyntax(sig.cdr());

                let ix = cc.add_constant(ctx, sym);
                cc.compile_lambda(ctx, Some(ix), arglist, def, false, false, false, false)?;
                library_manager().insert_binding(
                    ctx,
                    cc.library,
                    sym.get_symbol(),
                    Value::UNDEFINED,
                    true,
                )?;

                let identifier = cc.make_identifier(ctx, sym.get_symbol());

                let ix = cc.add_constant(ctx, identifier);

                cc.emit(ctx, Ins::DefineGlobal(ix as _, true));
                return Ok(false);
            }
        }
    }
    let exc = Exception::argument_count(ctx, Some("define"), 2, 2, form, pos);
    ctx.error(exc)
}

pub fn compile_quote(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    _tail: bool,
) -> ScmResult<bool> {
    let form = Compiler::desyntax_rec(ctx, form);
    if form.is_pair() {
        let rest = form.cdr();
        if rest.is_pair() && rest.cdr().is_null() {
            let quote = rest.car();
            let ix = cc.add_constant(ctx, quote);
            cc.emit(ctx, Ins::PushConstant(ix as _));
            return Ok(false);
        }
    }
    let exc = Exception::argument_count(ctx, Some("quote"), 1, 1, form, SourcePosition::unknown());
    ctx.error(exc)
}

pub fn compile_define_library(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    _: bool,
) -> ScmResult<bool> {
    cc.check_toplevel(ctx, form, SourcePosition::unknown())?;
    if form.is_pair() {
        let rest = form.cdr();
        if rest.is_pair() {
            let name = rest.car();
            let mut lsdecls = rest.cdr();
            let name = library_manager().module_name(ctx, name);
            let name = ctx.runtime().symbol_table().intern(name);
            let library = library_manager()
                .find_module(ctx, name, true, true)?
                .unwrap();

            let mut decls = ArrayList::new(ctx.mutator());

            while lsdecls.is_pair() {
                let decl = lsdecls.car();
                decls.push(ctx.mutator(), decl);
                lsdecls = lsdecls.cdr();
            }

            if !lsdecls.is_null() {
                let exc = Exception::eval(
                    ctx,
                    EvalError::MalformedLibraryDefinition,
                    &[lsdecls],
                    SourcePosition::unknown(),
                );
                return ctx.error(exc);
            }

            for decl in decls.iter() {
                if decl.is_pair() {
                    let sig = decl.car();
                    let spec = decl.cdr();

                    if !sig.is_symbol() {
                        let exc = Exception::eval(
                            ctx,
                            EvalError::MalformedLibraryDefinition,
                            &[sig],
                            SourcePosition::unknown(),
                        );
                        return ctx.error(exc);
                    }

                    let sig = sig.get_symbol();

                    if &*sig.identifier() == "export" {
                        library_manager().export_symbols(ctx, library, spec)?;
                    } else if &*sig.identifier() == "import" {
                        let mut imports = spec;
                        while imports.is_pair() {
                            let import = imports.car();
                            if let Some(imported_library) =
                                library_manager().lookup_import(ctx, import)?
                            {
                                library_manager().import_module(
                                    ctx,
                                    library,
                                    Value::new(imported_library),
                                    Value::nil(),
                                )?;
                            } else {
                                let exc = Exception::eval(
                                    ctx,
                                    EvalError::LibraryNotFound,
                                    &[import],
                                    SourcePosition::unknown(),
                                );
                                return ctx.error(exc);
                            }
                            imports = imports.cdr();
                        }
                    } else if &*sig.identifier() == "begin" {
                        let body = spec;
                        cc.with_library(library, ctx, |ctx, cc| {
                            cc.compile_body(ctx, body, Value::nil(), true)
                        })?;

                        /*match ctx.compile_and_eval(
                            body,
                            library
                        ) {
                            Ok(_) => (),
                            Err(err) => ctx.error(err),
                        }*/
                    }
                } else {
                    let exc = Exception::eval(
                        ctx,
                        EvalError::MalformedLibraryDefinition,
                        &[*decl],
                        SourcePosition::unknown(),
                    );
                    return ctx.error(exc);
                }
            }

            return Ok(false);
        }
    }

    let exc = Exception::argument_count(
        ctx,
        Some("define-library"),
        1,
        1,
        form,
        SourcePosition::unknown(),
    );
    ctx.error(exc)
}

pub fn compile_values(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    _tail: bool,
) -> ScmResult<bool> {
    if form.is_pair() {
        let cdr = form.cdr();

        match cc.compile_exprs(ctx, cdr)? {
            0 => {
                cc.emit(ctx, Ins::PushVoid);
            }
            1 => (),
            n => {
                cc.emit(ctx, Ins::Pack(n as _));
            }
        }

        return Ok(false);
    }

    let exc = Exception::argument_count(ctx, Some("values"), 1, 1, form, SourcePosition::unknown());

    ctx.error(exc)
}

pub fn values(ctx: &mut Context, args: &Arguments) -> Value {
    ArgumentsExt::values(args, ctx)
}

pub fn compile_define_values(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    _tail: bool,
) -> ScmResult<bool> {
    cc.check_toplevel(ctx, form, SourcePosition::unknown())?;

    if form.is_pair()
        && form.cdr().is_pair()
        && form.cdr().cdr().is_pair()
        && form.cdr().cdr().cdr().is_null()
    {
        let rest = form.cdr();
        let sig = rest.car();
        let value = rest.cdr().car();

        if sig.is_null() {
            cc.compile(ctx, value, false)?;
            cc.emit(ctx, Ins::Unpack(0, false));
            cc.emit(ctx, Ins::PushVoid);
            return Ok(false);
        } else if sig.is_symbol() {
            let index = cc.add_constant(ctx, sig);
            cc.compile(ctx, value, false)?;
            cc.emit(ctx, Ins::Unpack(0, true));

            let identifier = cc.make_identifier(ctx, sig.get_symbol());

            let ix = cc.add_constant(ctx, identifier);

            cc.emit(ctx, Ins::DefineGlobal(ix as _, false));
            cc.emit(ctx, Ins::PushConstant(index as _));
            return Ok(false);
        } else if sig.is_pair() {
            cc.compile(ctx, value, false)?;
            let mut syms = ArrayList::new(ctx.mutator());

            let mut vars = sig;

            while vars.is_pair() && vars.car().is_symbol() {
                let rest = vars.cdr();
                let sym = vars.car().get_symbol();
                if syms.contains(&sym) {
                    let exc = Exception::eval(
                        ctx,
                        EvalError::DuplicateBinding,
                        &[Value::new(sym), sig],
                        SourcePosition::unknown(),
                    );
                    return ctx.error(exc);
                }

                syms.push(ctx.mutator(), sym);

                vars = rest;
            }

            if vars.is_null() {
                cc.emit(ctx, Ins::Unpack(syms.len() as _, false));
            } else if vars.is_symbol() {
                cc.emit(ctx, Ins::Unpack(syms.len() as _, true));
                syms.push(ctx.mutator(), vars.get_symbol());
            } else {
                let ls = ctx.make_pair(value, Value::nil());
                let ls = ctx.make_pair(sig, ls);
                let exc = Exception::eval(
                    ctx,
                    EvalError::MalformedDefinition,
                    &[ls],
                    SourcePosition::unknown(),
                );
                return ctx.error(exc);
            }

            let mut res = Value::nil();

            for (_, sym) in syms.iter().enumerate().rev() {
                library_manager().make_binding(
                    cc.library,
                    *sym,
                    Value::UNDEFINED,
                    GlocFlag::BindingMut,
                );
                //let index = cc.add_constant(ctx, Value::new(*sym));
                let identifier = cc.make_identifier(ctx, *sym);

                let ix = cc.add_constant(ctx, identifier);
                cc.emit(ctx, Ins::DefineGlobal(ix as _, false));
                cc.emit(ctx, Ins::Pop);
                res = ctx.make_pair(Value::new(*sym), res);
            }

            if syms.len() == 0 {
                res = Value::void();
            } else if syms.len() == 1 {
                res = Value::new(syms[0]);
            } else {
                res = Value::new(ctx.mutator().allocate(Values(res)));
            }

            let index = cc.add_constant(ctx, res);
            cc.emit(ctx, Ins::PushConstant(index as _));
        }
        Ok(false)
    } else {
        let exc = Exception::argument_count(
            ctx,
            Some("define-values"),
            2,
            2,
            form,
            SourcePosition::unknown(),
        );
        ctx.error(exc)
    }
}

pub fn is_null(_: &mut Context, val: Value) -> ScmResult {
    Ok(Value::new(val.is_null()))
}

pub fn is_pair(_: &mut Context, val: Value) -> ScmResult {
    Ok(Value::new(val.is_pair()))
}

pub fn is_symbol(_: &mut Context, val: Value) -> ScmResult {
    Ok(Value::new(val.is_symbol()))
}

pub fn is_string(_: &mut Context, val: Value) -> ScmResult {
    Ok(Value::new(val.is_string()))
}

pub fn is_fixnum(_: &mut Context, val: Value) -> ScmResult {
    Ok(Value::new(val.is_int32()))
}

pub fn is_flonum(_: &mut Context, val: Value) -> ScmResult {
    Ok(Value::new(val.is_double()))
}

pub fn is_list(_: &mut Context, val: Value) -> ScmResult {
    Ok(Value::new(val.is_list_recsafe()))
}

pub fn is_vector(_: &mut Context, val: Value) -> ScmResult {
    Ok(Value::new(val.is_vector()))
}

pub fn length(ctx: &mut Context, val: Value) -> ScmResult {
    val.assert_type(ctx, SourcePosition::unknown(), &[Type::List])?;
    Ok(Value::new(val.length_recsafe().1 as i32))
}

pub fn car(ctx: &mut Context, val: Value) -> ScmResult {
    val.assert_type(ctx, SourcePosition::unknown(), &[Type::Pair])?;

    Ok(val.car())
}

pub fn cdr(ctx: &mut Context, val: Value) -> ScmResult {
    val.assert_type(ctx, SourcePosition::unknown(), &[Type::Pair])?;

    Ok(val.cdr())
}

pub fn cons(ctx: &mut Context, car: Value, cdr: Value) -> ScmResult {
    Ok(ctx.make_pair(car, cdr))
}

pub fn eq(ctx: &mut Context, a: Value, b: Value) -> ScmResult {
    Ok(crate::data::equality::eq(ctx, a, b).into())
}

pub fn eqv(ctx: &mut Context, a: Value, b: Value) -> ScmResult {
    Ok(crate::data::equality::eqv(ctx, a, b).into())
}

pub fn equal(ctx: &mut Context, a: Value, b: Value) -> ScmResult {
    Ok(crate::data::equality::equal(ctx, a, b).into())
}

pub fn identity(_: &mut Context, val: Value) -> ScmResult {
    Ok(val)
}

pub fn dbg(_: &mut Context, args: &Arguments) -> ScmResult {
    for arg in args.iter() {
        print!("{} ", arg.to_string(false));
    }
    println!();

    Ok(Value::void())
}

#[allow(unused_variables, unused_assignments)]
pub fn compile_define_syntax(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    _tail: bool,
) -> ScmResult<bool> {
    cc.check_toplevel(ctx, form, SourcePosition::unknown())?;
    let kword;
    let transformer;
    let doc;
    if form.is_pair() {
        if form.cdr().is_pair() && form.cdr().cdr().is_pair() && form.cdr().cdr().cdr().is_null() {
            kword = form.cdr().car();
            transformer = form.cdr().cdr().car();
            doc = None;
        } else if form.cdr().is_pair()
            && form.cdr().cdr().is_pair()
            && form.cdr().cdr().car().is_string()
            && form.cdr().cdr().cdr().is_pair()
            && form.cdr().cdr().cdr().cdr().is_null()
        {
            kword = form.cdr().car();
            transformer = form.cdr().cdr().cdr().car();
            doc = Some(form.cdr().cdr().car().get_string());
        } else {
            let exc = Exception::argument_count(
                ctx,
                Some("define-syntax"),
                2,
                3,
                form,
                SourcePosition::unknown(),
            );
            return ctx.error(exc);
        }

        if kword.is_symbol() {
            let sym = kword.get_symbol();

            let old_syntax_sym = cc.syntax_sym;
            cc.syntax_sym = kword;
            cc.compile(ctx, transformer, false)?;
            cc.syntax_sym = old_syntax_sym;

            let env = cc.library;

            library_manager().insert_binding(ctx, env, sym, Value::UNDEFINED, false)?;

            let identifier = cc.make_identifier(ctx, kword.get_symbol());

            let index = cc.add_constant(ctx, identifier);

            cc.emit(ctx, Ins::MakeSyntax(index as _));
            cc.emit(ctx, Ins::DefineGlobal(index as _, false));
            cc.emit(ctx, Ins::PushConstant(index as _));

            return Ok(false);
        }
    }

    let exc = Exception::argument_count(
        ctx,
        Some("define-syntax"),
        2,
        3,
        form,
        SourcePosition::unknown(),
    );
    ctx.error(exc)
}

pub fn compile_set(cc: &mut Compiler, ctx: &mut Context, form: Value, _: bool) -> ScmResult<bool> {
    if form.is_pair()
        && form.cdr().is_pair()
        && form.cdr().car().is_symbol()
        && form.cdr().cdr().is_pair()
        && form.cdr().cdr().cdr().is_null()
    {
        let sym = form.cdr().car().get_symbol();
        let val = form.cdr().cdr().car();

        cc.compile(ctx, val, false)?;
        cc.set_value_of(ctx, sym)?;
        cc.emit(ctx, Ins::PushVoid);
        return Ok(false);
    }

    let exc = Exception::argument_count(ctx, Some("set!"), 2, 2, form, SourcePosition::unknown());
    ctx.error(exc)
}

pub fn compile_is_null(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    _: bool,
) -> ScmResult<bool> {
    if form.is_pair() && form.cdr().cdr().is_null() {
        cc.compile(ctx, form.cdr().car(), false)?;
        cc.emit(ctx, Ins::IsNull);
        Ok(false)
    } else {
        let exc =
            Exception::argument_count(ctx, Some("null?"), 1, 1, form, SourcePosition::unknown());

        ctx.error(exc)
    }
}

pub fn compile_is_pair(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    _: bool,
) -> ScmResult<bool> {
    if form.is_pair() && form.cdr().cdr().is_null() {
        cc.compile(ctx, form.cdr().car(), false)?;
        cc.emit(ctx, Ins::IsPair);
        Ok(false)
    } else {
        let exc =
            Exception::argument_count(ctx, Some("pair?"), 1, 1, form, SourcePosition::unknown());

        ctx.error(exc)
    }
}

pub fn compile_car(cc: &mut Compiler, ctx: &mut Context, form: Value, _: bool) -> ScmResult<bool> {
    if form.is_pair() && form.cdr().cdr().is_null() {
        cc.compile(ctx, form.cdr().car(), false)?;
        cc.emit(ctx, Ins::Car);
        Ok(false)
    } else {
        let exc =
            Exception::argument_count(ctx, Some("car"), 1, 1, form, SourcePosition::unknown());

        ctx.error(exc)
    }
}

pub fn compile_cdr(cc: &mut Compiler, ctx: &mut Context, form: Value, _: bool) -> ScmResult<bool> {
    if form.is_pair() && form.cdr().cdr().is_null() {
        cc.compile(ctx, form.cdr().car(), false)?;
        cc.emit(ctx, Ins::Cdr);
        Ok(false)
    } else {
        let exc =
            Exception::argument_count(ctx, Some("cdr"), 1, 1, form, SourcePosition::unknown());

        ctx.error(exc)
    }
}

pub fn compile_cons(cc: &mut Compiler, ctx: &mut Context, form: Value, _: bool) -> ScmResult<bool> {
    // Pair(_, Pair(car, Pair(cons, nil)))
    if form.is_pair()
        && form.cdr().is_pair()
        && form.cdr().cdr().is_pair()
        && form.cdr().cdr().cdr().is_null()
    {
        let car = form.cdr().car();
        let cdr = form.cdr().cdr().car();
        cc.compile(ctx, car, false)?;
        cc.compile(ctx, cdr, false)?;
        cc.emit(ctx, Ins::Cons);
        Ok(false)
    } else {
        let exc =
            Exception::argument_count(ctx, Some("cons"), 1, 1, form, SourcePosition::unknown());

        ctx.error(exc)
    }
}

pub fn set_car(ctx: &mut Context, cons: Value, car: Value) -> ScmResult {
    cons.assert_type(ctx, SourcePosition::unknown(), &[Type::Pair])?;

    let mut pair = cons.get_handle_of::<Pair>();

    ctx.mutator().write_barrier(pair);
    pair.car = car;
    Ok(Value::void())
}

pub fn set_cdr(ctx: &mut Context, cons: Value, cdr: Value) -> ScmResult {
    cons.assert_type(ctx, SourcePosition::unknown(), &[Type::Pair])?;

    let mut pair = cons.get_handle_of::<Pair>();

    ctx.mutator().write_barrier(pair);
    pair.cdr = cdr;
    Ok(Value::void())
}


pub fn list(ctx: &mut Context, args: &[Value]) -> ScmResult {
    let mut list = Value::nil();

    for arg in args.iter().rev() {
        list = ctx.make_pair(*arg, list);
    }

    Ok(list)
}

pub fn make_parameter(ctx: &mut Context, init: Value, guard: Option<Value>) -> ScmResult {
    let guard = if let Some(guard) = guard {
        if guard.is_false() || guard.is_null() {
            Value::nil()
        } else if guard.is_handle_of::<Procedure>() { 
            guard 
        } else {
            todo!()
        }
    } else {
        Value::nil()
    };

    let parameter = ctx.make_pair(guard, init);
    let proc = Procedure {
        id: Procedure::new_id(),
        kind: ProcedureKind::Parameter(parameter.pair()),
        module: library_manager().scheme_module.get_handle_of()
    };

    Ok(ctx.mutator().allocate(proc).into())
}