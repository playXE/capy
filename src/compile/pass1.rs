use rsgc::{prelude::Handle, system::arraylist::ArrayList, thread::Thread};

use super::{cenv_copy, cenv_lookup, make_iform, IForm, LVar, Let, LetScope, List, Seq};
use crate::{
    compaux::{
        identifier_to_symbol, scm_identifier_to_symbol, scm_make_identifier, scm_unwrap_syntax,
    },
    compile::{
        cenv_extend, cenv_frames, cenv_make_bottom, cenv_module, global_call_type, Call, CallFlag,
        Define, GRef, GSet, GlobalCall, If, LFlag, LRef, LSet, Lambda,
    },
    runtime::list::{
        scm_append, scm_assq, scm_cons, scm_is_list, scm_length, scm_list, scm_list_from_iter,
        scm_list_star, scm_map, scm_map2, scm_reverse,
    },
    runtime::macros::scm_compile_syntax_rules,
    runtime::module::{
        is_global_identifier_eq, scm_export_symbols, scm_find_module, scm_import_module,
        scm_insert_binding, scm_make_module,
    },
    runtime::{object::{Identifier, Module, ObjectHeader, Type}, violation::raise_error},
    runtime::string::make_string,
    runtime::symbol::{gensym, make_symbol},
    runtime::value::Value,
    scm_dolist,
    vm::interpreter::apply,
};

macro_rules! global_id {
    ($name: ident, $sym: literal) => {
        pub static $name: Lazy<Handle<Identifier>> = Lazy::new(|| {
            scm_make_identifier(
                make_symbol($sym, true),
                scm_find_module(make_symbol("capy", true).symbol(), false, true).unwrap(),
                Value::encode_null_value(),
            )
        });
    };

    (r5rs $name: ident, $sym: literal) => {
        pub static $name: Lazy<Handle<Identifier>> = Lazy::new(|| {
            scm_make_identifier(
                make_symbol($sym, true),
                scm_find_module(make_symbol("null", true).symbol(), false, true).unwrap(),
                Value::encode_null_value(),
            )
        });
    };
}

use once_cell::sync::Lazy;

global_id!(DEFINE, "define");
global_id!(DEFINE_SYNTAX, "define-syntax");
global_id!(LAMBDA, "lambda");
global_id!(APPLY, "apply");
global_id!(r5rs R5RS_DEFINE, "define");
global_id!(r5rs R5RS_LAMBDA, "lambda");
global_id!(VALUES, "values");
global_id!(BEGIN, "begin");
global_id!(LET, "let");
global_id!(RECEIVE, "receive");
global_id!(INCLUDE, "include");
global_id!(INCLUDE_CI, "include-ci");
global_id!(ELSE, "else");
global_id!(ARROW, "=>");
global_id!(CURRENT_MODULE, "current-module");
global_id!(WITH_MODULE, "with-module");
global_id!(QUASIQUOTE, "quasiquote");
global_id!(UNQUOTE, "unquote");
global_id!(UNQUOTE_SPLICING, "unquote-splicing");

/// Common entry to handle procedur call.
pub fn pass1_call(
    program: Value,
    proc: Handle<IForm>,
    args: Value,
    cenv: Value,
) -> Result<Handle<IForm>, Value> {
    if let IForm::Lambda(_) = &*proc {
        let mut alist = ArrayList::with_capacity(Thread::current(), scm_length(args).unwrap());

        scm_dolist!(arg, args, {
            alist.push(Thread::current(), pass1(arg, cenv)?);
        });

        expand_inlined_procedure(program, proc, &alist)
    } else if args.is_null() {
        Ok(make_iform(IForm::Call(Call {
            origin: program,
            proc,
            args: ArrayList::new(Thread::current()),
            flag: CallFlag::None,
        })))
    } else {
        let mut alist = ArrayList::with_capacity(Thread::current(), scm_length(args).unwrap());

        scm_dolist!(arg, args, {
            alist.push(Thread::current(), pass1(arg, cenv)?);
        });

        Ok(make_iform(IForm::Call(Call {
            origin: program,
            proc,
            args: alist,
            flag: CallFlag::None,
        })))
    }
}

pub fn pass1_lookup_head(head: Value, cenv: Value) -> Value {
    if head.is_identifier() {
        cenv_lookup(cenv, head)
    } else {
        Value::encode_bool_value(false)
    }
}

pub fn pass1(program: Value, cenv: Value) -> Result<Handle<IForm>, Value> {
    //heap().request_gc();
    fn global_call(program: Value, id: Value, cenv: Value) -> Result<Handle<IForm>, Value> {
        match global_call_type(id, cenv) {
            GlobalCall::Syntax(syntax) => (syntax.callback)(program, cenv),
            GlobalCall::Normal => {
                let gref = make_iform(IForm::GRef(GRef { id }));
                pass1_call(program, gref, program.cdr(), cenv)
            }

            GlobalCall::Macro(m) => {
                let v = pass1(apply(m.r#macro().transformer, &[program, cenv])?, cenv)?;

                Ok(v)
            }

            GlobalCall::Inliner(proc) => expand_inliner(proc, program, id, cenv),
        }
    }

    if program.is_pair() {
        if !scm_is_list(program) {
            return Err(make_string(
                Thread::current(),
                &format!(
                    "proper list is required for application or macro use: '{:?}'",
                    program
                ),
            )
            .into());
        }

        let h = pass1_lookup_head(program.car(), cenv);

        match h {
            h if h.is_wrapped_identifier() => global_call(program, h, cenv),

            h if h.is_lvar() => {
                let lref = make_iform(IForm::LRef(LRef { lvar: h.lvar() }));

                pass1_call(program, lref, program.cdr(), cenv)
            }

            h if h.is_syntax() => (h.syntax().callback)(program, cenv),

            h if h.is_macro() => {
                let transformer = h.r#macro().transformer;

                pass1(apply(transformer, &[program, cenv])?, cenv)
            }

            h if h.is_false() => {
                let rator = pass1(program.car(), cenv)?;
                /*let mut alist =
                    ArrayList::with_capacity(Thread::current(), scm_length(program.cdr()).unwrap());

                scm_dolist!(arg, program.cdr(), {
                    alist.push(Thread::current(), pass1(arg, cenv)?);
                });

                Ok(make_iform(IForm::Call(Call {
                    origin: program,
                    proc: rator,
                    args: alist,
                    flag: CallFlag::None,
                })))*/
                pass1_call(program, rator, program.cdr(), cenv)
            }

            _ => {
                return Err(make_string(
                    Thread::current(),
                    &format!("unknown identifier in call: '{:?}'", program.car()),
                )
                .into())
            }
        }
    } else if program.is_identifier() {
        let r = cenv_lookup(cenv, program);
        if r.is_lvar() {
            Ok(make_iform(IForm::LRef(LRef { lvar: r.lvar() })))
        } else if r.is_wrapped_identifier() {
            Ok(make_iform(IForm::GRef(GRef { id: r })))
        } else {
            return Err(make_string(
                Thread::current(),
                &format!("unknown identifier: '{:?}'", program),
            )
            .into());
        }
    } else {
        Ok(make_iform(IForm::Const(program)))
    }
}

pub fn expand_inlined_procedure(
    src: Value,
    iform: Handle<IForm>,
    iargs: &[Handle<IForm>],
) -> Result<Handle<IForm>, Value> {
    if let IForm::Lambda(ref lambda) = &*iform {
        let args = adjust_arglist(lambda.reqargs, lambda.optarg, iargs, lambda.name)?;
        let lvars = &lambda.lvars;

        for i in 0..lvars.len() {
            let mut lvar = lvars[i];
            Thread::current().write_barrier(lvar);
            lvar.initval = Some(args[i]);
        }

        Ok(make_iform(IForm::Let(Let {
            origin: src,
            scope: LetScope::Let,
            lvars: ArrayList::from_slice(Thread::current(), &lvars), // clone lvars from lambda node
            body: lambda.body,
            inits: args,
        })))
    } else {
        unreachable!("expand_inlined_procedure: not a lambda node")
    }
}

pub fn argcount_is_ok(argc: usize, reqargs: usize, optarg: bool) -> bool {
    (!optarg && argc == reqargs) || (optarg && argc >= reqargs)
}

pub fn adjust_arglist(
    reqargs: usize,
    optarg: bool,
    iargs: &[Handle<IForm>],
    name: Value,
) -> Result<ArrayList<Handle<IForm>>, Value> {
    if !argcount_is_ok(iargs.len(), reqargs, optarg) {
        Err(make_string(
            Thread::current(),
            &format!(
                "wrong number of arguments: {:?} requires {}, but got {}",
                name,
                reqargs,
                iargs.len()
            ),
        )
        .into())
    } else {
        if !optarg {
            let mut args = ArrayList::with_capacity(Thread::current(), reqargs);
            for arg in iargs {
                args.push(Thread::current(), *arg);
            }

            Ok(args)
        } else {
            let (reqs, opts) = iargs.split_at(reqargs);
            let mut reqs_list = ArrayList::with_capacity(Thread::current(), reqs.len());

            for arg in reqs {
                reqs_list.push(Thread::current(), *arg);
            }

            let opt_list = if opts.is_empty() {
                let mut opt_list = ArrayList::with_capacity(Thread::current(), opts.len());
                for opt in opts {
                    opt_list.push(Thread::current(), *opt);
                }
                make_iform(IForm::List(List {
                    origin: Value::encode_null_value(),
                    elems: opt_list,
                }))
            } else {
                make_iform(IForm::Const(Value::encode_null_value()))
            };

            reqs_list.push(Thread::current(), opt_list);

            Ok(reqs_list)
        }
    }
}

pub fn define_syntax() {
    macro_rules! define_syntax {
        ($name: literal, $module: expr, $form: ident, $cenv: ident, $b: block) => {{
            let module = match $module {
                Some(module) => $crate::runtime::symbol::make_symbol(module, true).symbol(),
                None => $crate::runtime::symbol::make_symbol("null", true).symbol(),
            };

            fn stx(
                $form: $crate::runtime::value::Value,
                $cenv: $crate::runtime::value::Value,
            ) -> Result<Handle<$crate::compile::IForm>, Value> {
                $b
            }
            let m = $crate::runtime::module::scm_find_module(module, false, true)
                .expect("error")
                .expect("not found");
            $crate::runtime::module::scm_insert_syntax_binding(
                m,
                $crate::runtime::symbol::make_symbol($name, true).symbol(),
                stx,
            )
            .unwrap();
        }};
    }

    define_syntax!("define", None, form, cenv, {
        pass1_define(form, form, false, false, cenv_module(cenv), cenv)
    });

    define_syntax!("define", Some("capy"), form, cenv, {
        pass1_define(form, form, false, true, cenv_module(cenv), cenv)
    });

    define_syntax!("begin", None, form, cenv, {
        let mut seq = ArrayList::new(Thread::current());

        let t = Thread::current();

        scm_dolist!(expr, form.cdr(), {
            seq.push(t, pass1(expr, cenv)?);
        });

        if seq.len() == 1 {
            Ok(seq[0])
        } else {
            Ok(make_iform(IForm::Seq(Seq {
                origin: form,
                body: seq,
            })))
        }
    });

    define_syntax!("set!", None, form, cenv, {
        if scm_length(form) == Some(3) {
            let var = form.cadr();
            let val = form.caddr();

            let var = cenv_lookup(cenv, var);
            let val = pass1(val, cenv)?;

            if var.is_lvar() {
                Ok(make_iform(IForm::LSet(LSet {
                    lvar: var.lvar(),
                    value: val,
                })))
            } else {
                Ok(make_iform(IForm::GSet(GSet {
                    id: ensure_identifier(var, cenv),
                    value: val,
                })))
            }
        } else {
            Err(make_string(
                Thread::current(),
                "wrong number of arguments: set! requires 2, but got 0",
            )
            .into())
        }
    });

    define_syntax!("lambda", None, form, cenv, {
        if scm_length(form).filter(|&x| x >= 3).is_some() {
            let formals = form.cadr();
            let body = form.cddr();

            fn parse_formals(xs: Value, ys: Value) -> Result<(Value, Value), Value> {
                if xs.is_null() {
                    Ok((
                        scm_reverse(Thread::current(), ys),
                        Value::encode_bool_value(false),
                    ))
                } else if xs.is_identifier() {
                    Ok((scm_reverse(Thread::current(), ys), xs))
                } else if xs.is_pair() {
                    if !xs.car().is_identifier() {
                        return Err(make_string(
                            Thread::current(),
                            &format!("Invalid formal parameter: '{:?}'", xs),
                        )
                        .into());
                    }

                    parse_formals(xs.cdr(), scm_cons(Thread::current(), xs.car(), ys))
                } else {
                    Err(make_string(
                        Thread::current(),
                        &format!("Invalid formal parameter: '{:?}'", xs),
                    )
                    .into())
                }
            }

            let (reqs, rest) = parse_formals(formals, Value::encode_null_value())?;
           
            pass1_vanilla_lambda(
                form,
                if !rest.is_false() {
                    scm_append(
                        Thread::current(),
                        reqs,
                        scm_list(Thread::current(), &[rest]),
                    )
                } else {
                    reqs
                },
                scm_length(reqs).unwrap(),
                if !rest.is_false() { 1 } else { 0 },
                body,
                cenv,
            )
        } else {
            Err(make_string(
                Thread::current(),
                "wrong number of arguments: lambda requires at least 3, but got 0",
            )
            .into())
        }
    });

    define_syntax!("if", None, form, cenv, {
        // (if test then else)
        if scm_length(form) == Some(4) {
            Ok(make_iform(IForm::If(If {
                origin: form,
                cond: pass1(form.cadr(), cenv)?,
                cons: pass1(form.caddr(), cenv)?,
                alt: pass1(form.cadddr(), cenv)?,
            })))
        } else if scm_length(form) == Some(3) {
            // (if test then)
            Ok(make_iform(IForm::If(If {
                origin: form,
                cond: pass1(form.cadr(), cenv)?,
                cons: pass1(form.caddr(), cenv)?,
                alt: make_iform(IForm::Const(Value::encode_undefined_value())),
            })))
        } else {
            Err(make_string(
                Thread::current(),
                "wrong number of arguments: if requires 2 or 3, but got 0",
            )
            .into())
        }
    });

    define_syntax!("let", None, form, cenv, {
        let mut bindings = form.cadr();
        let mut body = form.cddr();

        let lambda = if bindings.is_identifier() {
            let name = bindings;
            bindings = body.car();
            body = body.cdr();
            name
        } else {
            Value::encode_undefined_value()
        };

        if !lambda.is_identifier() {
            if bindings.is_null() {
                pass1_body(body, cenv)
            } else if !scm_is_list(bindings) {
                return Err(make_string(
                    Thread::current(),
                    &format!("Invalid bindings: '{:?}'", bindings),
                )
                .into());
            } else {
                let mut vars = ArrayList::<Handle<LVar>>::new(Thread::current());

                let t = Thread::current();

                scm_dolist!(kv, bindings, {
                    let var = kv.car();
                    if !kv.cdr().is_pair() || !kv.cdr().cdr().is_null() {
                        return Err(make_string(t, &format!("Invalid binding: '{:?}'", kv)).into());
                    }

                    if !var.is_identifier() {
                        return Err(make_string(t, &format!("Invalid binding: '{:?}'", kv)).into());
                    }

                    let lvar = t.allocate(LVar {
                        header: ObjectHeader::new(Type::LVar),
                        name: var,
                        initval: None,
                        ref_count: 0,
                        set_count: 0,
                    });

                    vars.push(t, lvar);
                });

                let mut inits = ArrayList::with_capacity(t, vars.len());
                let mut i = 0;
                scm_dolist!(kv, bindings, {
                    let val = kv.cdr().car();
                    let mut var: Handle<LVar> = vars[i];
                    i += 1;
                    let iexpr = pass1(val, cenv)?;
                    t.write_barrier(var);
                    var.initval = Some(iexpr);
                    inits.push(t, iexpr);
                });

                let newenv = cenv_extend(
                    cenv,
                    scm_list_from_iter(
                        Thread::current(),
                        vars.iter().map(|&lvar| scm_cons(t, lvar.name, lvar.into())),
                    ),
                    make_symbol("LEXICAL", true),
                );

                let body = pass1_body(body, newenv)?;

                Ok(make_iform(IForm::Let(Let {
                    origin: form,
                    scope: LetScope::Rec,
                    lvars: vars,
                    inits,
                    body,
                })))
            }
        } else {
            // named let
            // Named let.  (let name ((var exp) ...) body ...)
            //
            //  We don't use the textbook expansion here
            //    ((letrec ((name (lambda (var ...) body ...))) name) exp ...)
            //
            //  Instead, we use the following expansion, except that we cheat
            //  environment during expanding {exp ...} so that the binding of
            //  name doesn't interfere with exp ....
            //
            //    (letrec ((name (lambda (var ...) body ...))) (name {exp ...}))
            //
            //  The reason is that this form can be more easily spotted by
            //  our simple-minded closure optimizer in Pass 2.
            let t = Thread::current();

            if !scm_is_list(bindings) {
                return Err(make_string(t, &format!("Invalid bindings: '{:?}'", bindings)).into());
            }

            let mut lvar = t.allocate(LVar {
                header: ObjectHeader::new(Type::LVar),
                name: lambda,
                initval: None,
                ref_count: 0,
                set_count: 0,
            });

            let mut args = ArrayList::with_capacity(t, scm_length(bindings).unwrap());

            scm_dolist!(kv, bindings, {
                let var = kv.car();
                if !kv.cdr().is_pair() || !kv.cdr().cdr().is_null() {
                    return Err(make_string(t, &format!("Invalid binding: '{:?}'", kv)).into());
                }

                if !var.is_identifier() {
                    return Err(make_string(t, &format!("Invalid binding: '{:?}'", kv)).into());
                }

                let lvar = t.allocate(LVar {
                    header: ObjectHeader::new(Type::LVar),
                    name: var,
                    initval: None,
                    ref_count: 0,
                    set_count: 0,
                });

                args.push(t, lvar);
            });

            let argenv = cenv;
            let env1 = cenv_extend(
                cenv,
                scm_list(t, &[scm_cons(Thread::current(), lambda, lvar.into())]),
                make_symbol("LEXICAL", true),
            );

            let env2 = cenv_extend(
                env1,
                scm_list_from_iter(
                    Thread::current(),
                    args.iter()
                        .map(|&lvar| scm_cons(Thread::current(), lvar.name, lvar.into())),
                ),
                make_symbol("LEXICAL", true),
            );

            let body = pass1_body(body, env2)?;

            let lam = make_iform(IForm::Lambda(Lambda {
                origin: form,
                name: lambda,
                reqargs: args.len(),
                optarg: false,
                lvars: args,
                body,
                flag: LFlag::Used,
                calls: ArrayList::new(t),
                free_lvars: ArrayList::new(t),
                lifted_var: None,
            }));

            t.write_barrier(lvar);
            lvar.initval = Some(lam);

            let mut cargs = ArrayList::with_capacity(t, scm_length(bindings).unwrap());

            scm_dolist!(kv, bindings, {
                let val = kv.cdr().car();
                let iexpr = pass1(val, argenv)?;
                cargs.push(t, iexpr);
            });

            let call = make_iform(IForm::Call(Call {
                origin: form,
                proc: make_iform(IForm::LRef(LRef { lvar })),
                args: cargs,
                flag: CallFlag::None,
            }));

            let letrec = make_iform(IForm::Let(Let {
                origin: form,
                scope: LetScope::Rec,
                lvars: ArrayList::from_slice(t, &[lvar]),
                inits: ArrayList::from_slice(t, &[lam]),
                body: call,
            }));

            Ok(letrec)
        }
    });

    define_syntax!("letrec", None, form, cenv, {
        pass1_letrec(
            form,
            cenv,
            make_string(Thread::current(), "letrec").into(),
            LetScope::Rec,
        )
    });
    define_syntax!("letrec*", None, form, cenv, {
        pass1_letrec(
            form,
            cenv,
            make_string(Thread::current(), "letrec").into(),
            LetScope::Rec,
        )
    });

    define_syntax!("quote", None, form, _cenv, {
        let val = form.cadr();

        Ok(make_iform(IForm::Const(val)))
    });

    define_syntax!("let-syntax", None, form, cenv, {
        if scm_length(form).filter(|&x| x >= 3).is_some() {
            let bindings = form.cadr();
            let body = form.cddr();

            let mut trans = Value::encode_null_value();

            scm_dolist!(kv, bindings, {
                if scm_length(kv) != Some(2) {
                    return Err(make_string(Thread::current(), "Invalid syntax: let-syntax").into());
                }

                let name = kv.car();
                let trans_spec = kv.cdr().car();

                if !name.is_identifier() {
                    return Err(make_string(Thread::current(), "Invalid syntax: let-syntax").into());
                }

                let transformer = eval_macro_rhs("let-syntax", trans_spec, cenv)?;

                trans = scm_cons(
                    Thread::current(),
                    scm_cons(Thread::current(), name, transformer),
                    trans,
                );
            });

            let newenv = cenv_extend(cenv, trans, make_symbol("SYNTAX", true));

            pass1_body(body, newenv)
        } else {
            Err(make_string(Thread::current(), "Invalid syntax: let-syntax").into())
        }
    });

    define_syntax!("define-module", None, form, _cenv, {
        if scm_length(form).filter(|&x| x >= 3).is_some() && scm_is_list(form) {
            let name = form.cadr();
            let body = form.cddr();

            let m = ensure_module(name, make_symbol("define-module", true), true)?;
            let newenv = cenv_make_bottom(Some(m));

            let mut seq = ArrayList::with_capacity(Thread::current(), scm_length(body).unwrap());

            scm_dolist!(form, body, {
                let iexpr = pass1(form, newenv)?;
                seq.push(Thread::current(), iexpr);
            });

            Ok(make_iform(IForm::Seq(Seq {
                origin: form,
                body: seq,
            })))
        } else {
            Err(make_string(
                Thread::current(),
                &format!("(define-module <name> <body> ...) expected"),
            )
            .into())
        }
    });

    define_syntax!("export", None, form, cenv, {
        scm_export_symbols(cenv_module(cenv), form.cdr())?;

        Ok(make_iform(IForm::Const(Value::encode_undefined_value())))
    });

    define_syntax!("import", None, form, cenv, {
        fn ensure(m: Value) -> Result<Handle<Module>, Value> {
            let o = m;
            let m = scm_find_module(m.symbol(), false, true).unwrap();
            m.ok_or_else(|| {
                make_string(Thread::current(), &format!("Module {:?} not found", o)).into()
            })
        }

        scm_dolist!(f, form.cdr(), {
            if f.is_identifier() {
                let import = scm_identifier_to_symbol(f);
                let m = ensure(import.into())?;
                scm_import_module(cenv_module(cenv), m.into(), Value::encode_null_value())?;
            } else {
                return Err(
                    make_string(Thread::current(), &format!("Invalid import: {:?}", f)).into(),
                );
            }
        });

        Ok(make_iform(IForm::Const(Value::encode_undefined_value())))
    });

    define_syntax!("syntax-rules", None, form, cenv, {
        let literals = form.cadr();
        let rules = form.cddr();

        let sr = scm_compile_syntax_rules(
            Value::encode_null_value(),
            form,
            Value::encode_bool_value(true),
            literals,
            rules,
            cenv_module(cenv),
            cenv,
        )?;

        Ok(make_iform(IForm::Const(sr)))
    });

    define_syntax!("define-syntax", None, form, cenv, {
        if scm_length(form) == Some(3) {
            let name = form.cadr();
            let expr = form.caddr();

            let transformer = eval_macro_rhs("define-syntax", expr, cenv)?; // apply(super::compile(expr, cenv)?, &[])?;

            let id = if name.is_wrapped_identifier() {
                rename_toplevel_identifier(name.identifier())
            } else {
                scm_make_identifier(name, Some(cenv_module(cenv)), Value::encode_null_value())
            };

            /* if !transformer.is_macro() {
                return Err(make_string(
                    Thread::current(),
                    &format!("define-syntax expects syntax transformer"),
                )
                .into());
            }*/

            scm_insert_binding(
                id.module.module(),
                scm_unwrap_syntax(name, false).symbol(),
                transformer,
                0,
                false,
            )?;

            Ok(make_iform(IForm::Const(Value::encode_undefined_value())))
        } else {
            Err(make_string(
                Thread::current(),
                &format!("(define-syntax name expr) expected"),
            )
            .into())
        }
    });

    /*define_syntax!("define-syntax-rules", None, form, cenv, {
        let name = form.cadr();
        let literals = form.caddr();
        let rules = form.cdddr();
        let sr = scm_compile_syntax_rules(
            name,
            form,
            Value::encode_bool_value(true),
            literals,
            rules,
            cenv_module(cenv),
            cenv,
        )?;
        let id = if name.is_wrapped_identifier() {
            rename_toplevel_identifier(name.identifier())
        } else {
            scm_make_identifier(name, Some(cenv_module(cenv)), Value::encode_null_value())
        };
        scm_insert_syntax_rule_binding(
            id.module.module(),
            scm_unwrap_syntax(id.into(), false).symbol(),
            sr.syntax_rules(),
        )?;

        Ok(make_iform(IForm::Const(Value::encode_undefined_value())))
    });*/
}

pub fn ensure_module(thing: Value, name: Value, _create: bool) -> Result<Handle<Module>, Value> {
    let m = if thing.is_identifier() {
        scm_find_module(scm_identifier_to_symbol(thing), false, true)?
    } else if thing.is_module() {
        return Ok(thing.module());
    } else {
        return Err(make_string(
            Thread::current(),
            &format!(
                "{:?} requires a module name or a module, but got: {:?}",
                name, thing
            ),
        )
        .into());
    };

    if let Some(m) = m {
        Ok(m)
    } else {
        scm_make_module(Some(scm_identifier_to_symbol(thing)), true).map(|x| x.unwrap())
    }
}

pub fn ensure_identifier(sym_or_id: Value, cenv: Value) -> Value {
    if sym_or_id.is_symbol() {
        scm_make_identifier(sym_or_id, Some(cenv_module(cenv)), cenv_frames(cenv)).into()
    } else {
        sym_or_id
    }
}

pub fn pass1_define(
    form: Value,
    oform: Value,
    constant: bool,
    extended: bool,
    module: Handle<Module>,
    cenv: Value,
) -> Result<Handle<IForm>, Value> {
    let def = form.car();
    debug_assert!(def.is_xtype(Type::Symbol) && &*def.symbol() == "define");
    let name = form.cadr();

    // (_ (name . args) body ...)
    if name.is_pair() {
        let orig = name;
        let name = name.car();
        let args = orig.cdr();
        let body = form.cddr();

        let lambda = scm_list_star(
            Thread::current(),
            &[make_symbol("lambda", true), args, body],
        );

        let define = scm_list(
            Thread::current(),
            &[make_symbol("define", true), name, lambda],
        );

        pass1_define(define, oform, constant, extended, module, cenv)
    // (_ name)
    } else if form.cddr().is_null() {
        // allow R6RS style (define <name>)
        if !name.is_identifier() {
            return Err(raise_error("define", "<name> should be an identifier", 0));
        }
        pass1_define(
            scm_list(
                Thread::current(),
                &[
                    make_symbol("define", true),
                    name,
                    Value::encode_undefined_value(),
                ],
            ),
            oform,
            constant,
            extended,
            module,
            cenv,
        )
    } else if form.cddr().cdr().is_null() {
        let value = form.caddr();
        if !name.is_identifier() {
            return Err(raise_error("define", "<name> should be an identifier", 0));
        }
        let id = if name.is_wrapped_identifier() {
            rename_toplevel_identifier(name.identifier())
        } else {
            scm_make_identifier(name, module.into(), Value::encode_null_value())
        };

        //let uname = scm_unwrap_syntax(name, false);

        //scm_insert_binding(module, uname.symbol(), Value::encode_empty_value(), 0, true)?;

        Ok(make_iform(IForm::Define(Define {
            origin: oform,
            name: id.into(),
            value: pass1(value, cenv)?,
        })))
    } else {
        Err(make_string(Thread::current(), "define: invalid syntax").into())
    }
}

pub fn is_identifier(name: Value) -> bool {
    name.is_xtype(Type::Symbol) || name.is_xtype(Type::Identifier)
}

pub fn is_wrapped_identifier(name: Value) -> bool {
    name.is_xtype(Type::Identifier)
}

pub fn rename_toplevel_identifier(mut id: Handle<Identifier>) -> Handle<Identifier> {
    let gensym = gensym(&*identifier_to_symbol(id));
    id.name = gensym;
    id
}

pub fn wrap_as_named_expression(_src: Value, iform: Handle<IForm>, _id: Value) -> Handle<IForm> {
    iform
}

/// Compiling body with internal definitions.
pub fn pass1_body(exprs: Value, cenv: Value) -> Result<Handle<IForm>, Value> {
    pass1_body_rec(
        exprs,
        Value::encode_bool_value(false),
        Value::encode_bool_value(false),
        cenv,
    )
}

fn pass1_body_rec(
    exprs: Value,
    mframe: Value,
    vframe: Value,
    cenv: Value,
) -> Result<Handle<IForm>, Value> {
    fn dupe_check(var: Value, mframe: Value, vframe: Value) -> Result<(), Value> {
        if (mframe.is_pair() && !scm_assq(var, mframe).is_false())
            || (vframe.is_pair() && !scm_assq(var, vframe).is_false())
        {
            Err(make_string(Thread::current(), "duplicate variable binding").into())
        } else {
            Ok(())
        }
    }

    match exprs {
        // match ((op . args) . rest)
        exprs if exprs.is_pair() && exprs.car().is_pair() => {
            let rest = exprs.cdr();
            let op = exprs.caar();
            let args = exprs.cdar();

            if (vframe.is_false() || scm_assq(op, vframe).is_false()) && op.is_identifier() {
                let head = cenv_lookup(cenv, op);

                if !scm_is_list(args) {
                    return Err(make_string(
                        Thread::current(),
                        &format!(
                            "proper list required for function application or macro use: '{:?}'",
                            args
                        ),
                    )
                    .into());
                }

                match head {
                    head if head.is_lvar() => {
                        return pass1_body_finish(exprs, mframe, vframe, cenv)
                    }
                    head if head.is_syntax() => {
                        return pass1_body_finish(exprs, mframe, vframe, cenv)
                    }

                    head if head.is_macro() => {
                        /*let form = synrule_expand(
                            Thread::current(),
                            exprs.car(),
                            cenv_module(cenv),
                            cenv_frames(cenv),
                            head.syntax_rules(),
                        )?;*/

                        let expanded = apply(head.r#macro().transformer, &[exprs.car(), cenv])?;

                        return pass1_body_rec(
                            scm_list_star(Thread::current(), &[expanded, rest]),
                            mframe,
                            vframe,
                            cenv,
                        );
                    }

                    head if !head.is_wrapped_identifier() => {
                        return Err(make_string(
                            Thread::current(),
                            &format!("[internal] pass1/body: '{:?}'", head),
                        )
                        .into())
                    }

                    head if head.is_pair() && head.car() == make_symbol(":rec", true) => {
                        return pass1_body_finish(exprs, mframe, vframe, cenv)
                    }

                    head if is_global_identifier_eq(head, (*DEFINE).into())
                        || is_global_identifier_eq(head, (*R5RS_DEFINE).into()) =>
                    {
                        let def = match args {
                            // match ((name . formals) . body)
                            args if args.is_pair() && args.car().is_pair() => {
                                let name = args.caar();
                                let formals = args.cdar();
                                let body = args.cdr();

                                let lam = scm_list_star(
                                    Thread::current(),
                                    &[make_symbol("lambda", true), formals, body],
                                );
                                let rec = scm_list(
                                    Thread::current(),
                                    &[name, make_symbol(":rec", true), lam],
                                );

                                rec
                            }
                            // (var init)
                            args if args.is_pair()
                                && args.cdr().is_pair()
                                && args.cddr().is_null() =>
                            {
                                let var = args.car();
                                let init = args.cadr();

                                let rec = scm_list(
                                    Thread::current(),
                                    &[var, make_symbol(":rec", true), init],
                                );

                                rec
                            }

                            // (var)
                            args if args.is_pair() && args.cdr().is_null() => {
                                let var = args.car();

                                let rec = scm_list(
                                    Thread::current(),
                                    &[
                                        var,
                                        make_symbol(":rec", true),
                                        Value::encode_undefined_value(),
                                    ],
                                );

                                rec
                            }

                            _ => {
                                return Err(make_string(
                                    Thread::current(),
                                    &format!("malformed internal define: '{:?}'", exprs.car()),
                                )
                                .into())
                            }
                        };

                        dupe_check(def.car(), mframe, vframe)?;
                        if mframe.is_null() || mframe.is_false() {
                            let cenv = cenv_extend(
                                cenv,
                                Value::encode_null_value(),
                                make_symbol("SYNTAX", true),
                            );
                            let mframe = cenv_frames(cenv).car();
                            let cenv = cenv_extend(
                                cenv,
                                scm_list(Thread::current(), &[def]),
                                make_symbol("LEXICAL", true),
                            );
                            let vframe = cenv_frames(cenv).car();

                            return pass1_body_rec(rest, mframe, vframe, cenv);
                        } else {
                            vframe.set_cdr(scm_cons(Thread::current(), def, vframe.cdr()));
                            return pass1_body_rec(rest, mframe, vframe, cenv);
                        }
                    }
                    head if is_global_identifier_eq(head, (*BEGIN).into()) => {
                        pass1_body_rec(args, mframe, vframe, cenv)
                    }

                    _ => pass1_body_finish(exprs, mframe, vframe, cenv),
                }
            } else {
                pass1_body_finish(exprs, mframe, vframe, cenv)
            }
        }

        _ => pass1_body_finish(exprs, mframe, vframe, cenv),
    }
}

fn pass1_body_finish(
    exprs: Value,
    mframe: Value,
    vframe: Value,
    cenv: Value,
) -> Result<Handle<IForm>, Value> {
    if mframe.is_false() || mframe.is_null() {
        pass1_body_rest(exprs, cenv)
    } else {
        let t = Thread::current();
        let intdefs = scm_reverse(t, vframe.cdr());
        let vars = scm_map(t, |v| v.car(), intdefs);
        let mut lvars = ArrayList::with_capacity(t, scm_length(vars).unwrap());

        scm_dolist!(var, vars, {
            let lvar = t.allocate(LVar {
                header: ObjectHeader::new(Type::LVar),
                name: var,
                initval: None,
                ref_count: 0,
                set_count: 0,
            });
            lvars.push(t, lvar);
        });

        let lvars_list = scm_list_from_iter(
            t,
            lvars.iter().map(|lvar| Value::encode_object_value(*lvar)),
        );

        vframe.pair().cdr = scm_map2(
            t,
            |e1, e2| scm_cons(Thread::current(), e1, e2),
            vars,
            lvars_list,
        );

        let mut inits = ArrayList::with_capacity(t, lvars.len());

        let inits_expr = scm_map(t, |x| x.cddr(), intdefs);

        let mut i = 0;

        scm_dolist!(init, inits_expr, {
            let lvar = lvars[i];
            i += 1;
            let iexpr = pass1_body_init(lvar, init.car(), cenv)?;
            inits.push(t, iexpr);
        });

        let body = pass1_body_rest(exprs, cenv)?;

        Ok(make_iform(IForm::Let(Let {
            origin: exprs,
            scope: LetScope::Rec,
            lvars,
            inits,
            body,
        })))
    }
}

fn pass1_body_init(
    mut lvar: Handle<LVar>,
    init: Value,
    newenv: Value,
) -> Result<Handle<IForm>, Value> {
    let e = cenv_copy(newenv);

    let iexpr = pass1(init, e)?;
    Thread::current().write_barrier(lvar);
    lvar.initval = Some(iexpr);
    Ok(iexpr)
}

fn pass1_body_rest(exprs: Value, cenv: Value) -> Result<Handle<IForm>, Value> {
    if exprs.is_null() {
        Ok(make_iform(IForm::Const(Value::encode_null_value())))
    } else if exprs.is_pair() && exprs.cdr().is_null() {
        Ok(pass1(exprs.car(), cenv)?)
    } else {
        let t = Thread::current();
        let mut seq = ArrayList::with_capacity(t, scm_length(exprs).unwrap());

        let mut ls = exprs;

        while !ls.is_null() {
            seq.push(t, pass1(ls.car(), cenv)?);
            ls = ls.cdr();
        }

        Ok(make_iform(IForm::Seq(Seq {
            origin: exprs,
            body: seq,
        })))
    }
}

/// R7RS lambda
pub fn pass1_vanilla_lambda(
    form: Value,
    formals: Value,
    nreqs: usize,
    nopts: usize,
    body: Value,
    cenv: Value,
) -> Result<Handle<IForm>, Value> {
    let mut lvars = ArrayList::with_capacity(Thread::current(), 4);
    let t = Thread::current();
    scm_dolist!(formal, formals, {
        let lvar = t.allocate(LVar {
            header: ObjectHeader::new(Type::LVar),
            name: formal,
            initval: None,
            ref_count: 0,
            set_count: 0,
        });
        lvars.push(t, lvar);
    });

    let lvars_list = scm_list_from_iter(t, lvars.iter().map(|x| Value::encode_object_value(*x)));
    let cenv = cenv_extend(
        cenv,
        scm_map2(
            t,
            |x, y| scm_cons(Thread::current(), x, y),
            formals,
            lvars_list,
        ),
        make_symbol("LEXICAL", true),
    );

    let body = pass1_body(body, cenv)?;

    Ok(make_iform(IForm::Lambda(Lambda {
        origin: form,
        lvars,
        reqargs: nreqs,
        optarg: nopts != 0,
        body,
        calls: ArrayList::new(t),
        flag: LFlag::Used,
        name: Value::encode_null_value(),
        free_lvars: ArrayList::new(t),
        lifted_var: None,
    })))
}

pub fn pass1_letrec(
    form: Value,
    cenv: Value,
    _name: Value,
    typ: LetScope,
) -> Result<Handle<IForm>, Value> {
    let bindings = form.cadr();
    let body = form.cddr();

    if bindings.is_null() {
        pass1_body(body, cenv)
    } else {
        if !scm_is_list(bindings) {
            return Err(make_string(
                Thread::current(),
                &format!("Invalid bindings: '{:?}'", bindings),
            )
            .into());
        }

        let t = Thread::current();
        let mut vars = ArrayList::<Handle<LVar>>::new(Thread::current());

        scm_dolist!(kv, bindings, {
            let var = kv.car();
            if !kv.cdr().is_pair() || !kv.cdr().cdr().is_null() {
                return Err(make_string(t, &format!("Invalid binding: '{:?}'", kv)).into());
            }

            if !var.is_identifier() {
                return Err(make_string(t, &format!("Invalid binding: '{:?}'", kv)).into());
            }

            let lvar = t.allocate(LVar {
                header: ObjectHeader::new(Type::LVar),
                name: var,
                initval: None,
                ref_count: 0,
                set_count: 0,
            });

            vars.push(t, lvar);
        });

        let newenv = cenv_extend(
            cenv,
            scm_list_from_iter(
                Thread::current(),
                vars.iter().map(|&lvar| scm_cons(t, lvar.name, lvar.into())),
            ),
            make_symbol("LEXICAL", true),
        );

        let mut inits = ArrayList::with_capacity(t, vars.len());
        let mut i = 0;
        scm_dolist!(kv, bindings, {
            let val = kv.cdr().car();
            let mut var: Handle<LVar> = vars[i];
            i += 1;
            let iexpr = pass1(val, newenv)?;
            t.write_barrier(var);
            var.initval = Some(iexpr);
            inits.push(t, iexpr);
        });

        let body = pass1_body(body, newenv)?;

        Ok(make_iform(IForm::Let(Let {
            origin: form,
            scope: typ,
            lvars: vars,
            inits,
            body,
        })))
    }
}

fn expand_inliner(
    inliner: Value,
    program: Value,
    id: Value,
    cenv: Value,
) -> Result<Handle<IForm>, Value> {
    let Some(proc) = inliner.native_procedure().inliner else {
        let gref = make_iform(IForm::GRef(GRef { id }));
        return pass1_call(program, gref, program.cdr(), cenv)
    };

    let mut args = ArrayList::with_capacity(Thread::current(), scm_length(program.cdr()).unwrap());
    scm_dolist!(arg, program.cdr(), {
        args.push(Thread::current(), pass1(arg, cenv)?);
    });

    if let Some(iform) = proc(&args, inliner) {
        Ok(iform)
    } else {
        let gref = make_iform(IForm::GRef(GRef { id }));
        Ok(make_iform(IForm::Call(Call {
            origin: program,
            proc: gref,
            args,
            flag: CallFlag::None,
        })))
    }
}

fn eval_macro_rhs(name: &str, expr: Value, cenv: Value) -> Result<Value, Value> {
    let transformer = apply(super::compile(expr, cenv)?, &[])?;

    if transformer.is_macro() {
        Ok(transformer)
    } else {
        Err(make_string(
            Thread::current(),
            &format!(
                "{} expects syntax transformer but got: {:?}",
                name, transformer
            ),
        )
        .into())
    }
}
