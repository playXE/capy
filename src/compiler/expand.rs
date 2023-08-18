//! Expand pass
//!
//! Convert S-expressions to Tree IL, expand macros and core forms.

use std::collections::HashMap;

use once_cell::sync::Lazy;

use crate::{
    compiler::{
        identifier_to_symbol,
        sexpr::{sexp_cons, sexp_eq},
        synrules::{compile_syntax_rules, synrule_expand},
        unwrap_identifier, Denotation,
    },
    runtime::{object::scm_symbol_str, symbol::scm_intern, value::Value},
    vm::sync::mutex::Mutex,
};

use super::{make_identifier, sexpr::Sexpr, tree_il::*, unmangled, Cenv, SyntaxEnv, P};

fn is_call_declared(cenv: &Cenv) -> bool {
    let name = scm_intern(".call");
    let id = cenv.lookup(Sexpr::Symbol(name));

    match id {
        Sexpr::Identifier(_) => cenv
            .syntax_env
            .env
            .get(&name)
            .filter(|x| matches!(&***x, Denotation::Macro(_)))
            .is_some(),
        _ => false,
    }
}

pub fn expand_call(
    _program: Sexpr,
    proc: P<IForm>,
    mut args: Sexpr,
    cenv: &Cenv,
) -> Result<P<IForm>, String> {
    if let IForm::Lambda(_) = &*proc.clone() {
        let alist = args
            .list_to_vec()
            .iter()
            .map(|x| pass1(x, cenv))
            .collect::<Result<Vec<_>, _>>()?;

        return expand_inline_procedure(proc, &alist);
    }

    if args.is_null() {
        Ok(P(IForm::Call(Call {
            proc,
            args: Vec::new(),
            flag: CallFlag::None,
        })))
    } else {
        let mut iargs = Vec::new();
        while !args.is_null() {
            iargs.push(pass1(&args.car(), cenv)?);
            args = args.cdr();
        }

        Ok(P(IForm::Call(Call {
            proc,
            args: iargs,
            flag: CallFlag::None,
        })))
    }
}

pub fn expand_inline_procedure(iform: P<IForm>, iargs: &[P<IForm>]) -> Result<P<IForm>, String> {
    if let IForm::Lambda(ref lambda) = &*iform.clone() {
        let args = adjust_arglist(
            lambda.reqargs as _,
            lambda.optarg,
            iargs,
            lambda.name.as_ref(),
        )?;
        let lvars = &lambda.lvars;

        for i in 0..lvars.len() {
            let mut lvar = lvars[i].clone();
            lvar.initval = Some(args[i].clone());
        }

        Ok(P(IForm::Let(Let {
            typ: LetType::Let,
            lvars: lvars.clone(),
            inits: args,
            body: lambda.body.clone(),
        })))
    } else {
        unreachable!("expand_inline_procedure: not a lambda")
    }
}

pub fn argcount_is_ok(argc: usize, reqargs: usize, optarg: bool) -> bool {
    (!optarg && argc == reqargs) || (optarg && argc >= reqargs)
}

pub fn adjust_arglist(
    reqargs: usize,
    optarg: bool,
    iargs: &[P<IForm>],
    name: Option<&String>,
) -> Result<Vec<P<IForm>>, String> {
    if !argcount_is_ok(iargs.len(), reqargs, optarg) {
        if let Some(name) = name {
            return Err(format!(
                "wrong number of arguments to {}: {} given, {} required",
                name,
                iargs.len(),
                reqargs
            ));
        } else {
            return Err(format!(
                "wrong number of arguments: {} given, {} required",
                iargs.len(),
                reqargs
            ));
        }
    } else {
        if !optarg {
            let mut args = Vec::with_capacity(reqargs);
            for arg in iargs {
                args.push(arg.clone());
            }

            Ok(args)
        } else {
            let (reqs, opts) = iargs.split_at(reqargs);
            let mut reqs_list = reqs.to_vec();

            let opt_list = if !opts.is_empty() {
                let opts = opts.to_vec();

                P(IForm::Call(Call {
                    proc: P(IForm::GRef(GRef {
                        name: Sexpr::Symbol(scm_intern("list")),
                    })),
                    args: opts,
                    flag: CallFlag::None,
                }))
            } else {
                P(IForm::Const(Sexpr::Null))
            };

            reqs_list.push(opt_list);

            Ok(reqs_list)
        }
    }
}

pub fn pass1_lookup_head(head: &Sexpr, cenv: &Cenv) -> Sexpr {
    if matches!(head, Sexpr::Identifier(_) | Sexpr::Symbol(_)) {
        cenv.lookup(head.clone())
    } else {
        Sexpr::Boolean(false)
    }
}

pub fn pass1(program: &Sexpr, cenv: &Cenv) -> Result<P<IForm>, String> {
    fn global_call(program: Sexpr, id: Sexpr, cenv: &Cenv) -> Result<P<IForm>, String> {
        let name = identifier_to_symbol(id.clone());
        match cenv.syntax_env.env.get(&name) {
            Some(denotation) => match &**denotation {
                Denotation::Special(special) => special(program, cenv),
                Denotation::Macro(sr) => {
                    let expanded =
                        synrule_expand(program, cenv.syntax_env.clone(), cenv.frames.clone(), &sr)?;
                    pass1(&expanded, cenv)
                }
            },

            None => {
                if is_call_declared(cenv)
                    && matches!(program.car(), Sexpr::Identifier(_) | Sexpr::Symbol(_))
                {
                    let call = Sexpr::list(&[
                        Sexpr::Symbol(scm_intern(".call")),
                        program.car(),
                        program.clone(),
                    ]);

                    let Denotation::Macro(rules) = &*cenv
                        .syntax_env
                        .env
                        .get(&scm_intern(".call"))
                        .cloned()
                        .unwrap()
                    else {
                        unreachable!()
                    };
                    let expr =
                        synrule_expand(call, cenv.syntax_env.clone(), cenv.frames.clone(), &rules)?;

                    if sexp_eq(&program, &expr) {
                        let gref = P(IForm::GRef(GRef { name: id }));

                        return expand_call(program.clone(), gref, program.cdr(), cenv);
                    }

                    return pass1(&expr, cenv);
                }
                let gref = P(IForm::GRef(GRef { name: id }));

                expand_call(program.clone(), gref, program.cdr(), cenv)
            }
        }
    }

    if program.is_pair() {
        if !program.is_list() {
            return Err(format!(
                "proper list is required for application or macro use but got: {}",
                program
            ));
        }

        let h = pass1_lookup_head(&program.car(), cenv);

        match h {
            Sexpr::Identifier(x) => global_call(program.clone(), Sexpr::Identifier(x), cenv),
            Sexpr::Special(special) => special(program.clone(), cenv),
            Sexpr::LVar(lvar) => expand_call(
                program.clone(),
                P(IForm::LRef(LRef { lvar })),
                program.cdr(),
                cenv,
            ),
            Sexpr::SyntaxRules(sr) => {
                let expanded = synrule_expand(
                    program.clone(),
                    cenv.syntax_env.clone(),
                    cenv.frames.clone(),
                    &sr,
                )?;

                pass1(&expanded, cenv)
            }

            Sexpr::Boolean(false) => {
                if false && is_call_declared(cenv) {
                    let call = Sexpr::list(&[
                        Sexpr::Symbol(scm_intern(".call")),
                        program.car(),
                        program.clone(),
                    ]);

                    return pass1(&call, cenv);
                }

                let rator = pass1(&program.car(), cenv)?;

                expand_call(program.clone(), rator, program.cdr(), cenv)
            }

            x => Err(format!("illegal function call: {} {}", program, x)),
        }
    } else if matches!(program, Sexpr::Identifier(_) | Sexpr::Symbol(_)) {
        let r = cenv.lookup(program.clone());

        if let Sexpr::LVar(lvar) = r {
            Ok(P(IForm::LRef(LRef { lvar })))
        } else if let Sexpr::Identifier(id) = r {
            let name = unwrap_identifier(id.clone());

            Ok(P(IForm::GRef(GRef {
                name: Sexpr::Symbol(name),
            })))
        } else {
            Err(format!("illegal variable reference: {}", program))
        }
    } else {
        Ok(P(IForm::Const(program.clone())))
    }
}

pub fn define_syntax() -> P<SyntaxEnv> {
    let mut env = P(SyntaxEnv {
        env: HashMap::with_capacity(32),
        denotation_of_define: None,
        denotation_of_begin: None,
    });

    macro_rules! define_syntax {
        ($name: literal, $form: ident, $cenv: ident, $b: block) => {{
            fn stx($form: Sexpr, $cenv: &Cenv) -> Result<P<IForm>, String> {
                $b
            }

            let denotation = Denotation::Special(stx);

            env.env.insert(scm_intern($name), P(denotation));
        }};
    }

    define_syntax!("define", form, cenv, {
        pass1_define(form.clone(), form, cenv)
    });

    define_syntax!("if", form, cenv, {
        if form.list_length() == Some(4) {
            let cond = pass1(&form.cadr(), cenv)?;
            let consequent = pass1(&form.caddr(), cenv)?;
            let alternative = pass1(&form.cadddr(), cenv)?;

            Ok(P(IForm::If(If {
                cond,
                consequent,
                alternative,
            })))
        } else if form.list_length() == Some(3) {
            let cond = pass1(&form.cadr(), cenv)?;
            let consequent = pass1(&form.caddr(), cenv)?;

            Ok(P(IForm::If(If {
                cond,
                consequent,
                alternative: P(IForm::Const(Sexpr::Undefined)),
            })))
        } else {
            Err(format!("illegal if: {}", form))
        }
    });

    define_syntax!("lambda", form, cenv, {
        if form.list_length().filter(|x| *x >= 3).is_some() {
            let formals = form.cadr();
            let body = form.cddr();

            fn parse_formals(xs: Sexpr, ys: Sexpr) -> Result<(Sexpr, Sexpr), String> {
                if xs.is_null() {
                    Ok((ys.list_reverse(), Sexpr::Boolean(false)))
                } else if matches!(xs, Sexpr::Identifier(_) | Sexpr::Symbol(_)) {
                    Ok((ys.list_reverse(), xs))
                } else if xs.is_pair() {
                    if !matches!(xs.car(), Sexpr::Identifier(_) | Sexpr::Symbol(_)) {
                        return Err(format!("invalid formal parameter: {}", xs));
                    }

                    parse_formals(xs.cdr(), sexp_cons(xs.car(), ys))
                } else {
                    Err(format!("invalid formal parameter: {}", xs))
                }
            }

            let (reqs, rest) = parse_formals(formals, Sexpr::Null)?;

            pass1_vanilla_lambda(
                form,
                if !matches!(rest, Sexpr::Boolean(false)) {
                    Sexpr::append(Sexpr::list(&[rest.clone()]), reqs.clone())
                } else {
                    reqs.clone()
                },
                reqs.list_length().unwrap(),
                if !matches!(rest, Sexpr::Boolean(false)) {
                    1
                } else {
                    0
                },
                body,
                cenv,
            )
        } else {
            Err(format!("illegal lambda: {}", form))
        }
    });

    define_syntax!("set!", form, cenv, {
        if form.list_length() == Some(3) {
            let var = form.cadr();
            let val = form.caddr();

            if !matches!(var, Sexpr::Symbol(_) | Sexpr::Identifier(_)) {
                return Err(format!("illegal set!: {}", form));
            }

            let var = cenv.lookup(var.clone());
            let value = pass1(&val, cenv)?;

            if let Sexpr::LVar(lvar) = var {
                Ok(P(IForm::LSet(LSet { lvar, value })))
            } else {
                Ok(P(IForm::GSet(GSet { name: var, value })))
            }
        } else {
            Err(format!("illegal set!: {}", form))
        }
    });

    define_syntax!("let", form, cenv, {
        let mut bindings = form.cadr();
        let mut body = form.cddr();

        let lambda = if matches!(bindings, Sexpr::Identifier(_) | Sexpr::Symbol(_)) {
            let name = bindings.clone();
            bindings = body.car();
            body = body.cdr();
            name
        } else {
            Sexpr::Undefined
        };

        if matches!(lambda, Sexpr::Undefined) {
            if bindings.is_null() {
                pass1_body(&body, cenv)
            } else if !bindings.is_list() {
                return Err(format!("illegal let: {}", form));
            } else {
                let mut vars = Vec::new();

                let mut ls = bindings.clone();

                while ls.is_pair() {
                    let kv = ls.car();

                    if !kv.cdr().is_pair() || !kv.cdr().cdr().is_null() {
                        return Err(format!("illegal let: {}", form));
                    }

                    let var = kv.car();
                    if !matches!(var, Sexpr::Identifier(_) | Sexpr::Symbol(_)) {
                        return Err(format!("illegal let: {}", form));
                    }

                    let lvar = P(LVar {
                        name: var.clone(),
                        initval: None,
                        arg: false,
                        ref_count: 0,
                        set_count: 0,
                        boxed: false,
                    });

                    vars.push(lvar);

                    ls = ls.cdr();
                }

                let mut inits = Vec::new();

                let mut ls = bindings.clone();
                let mut i = 0;
                while ls.is_pair() {
                    let kv = ls.car();

                    let init = pass1(&kv.cadr(), cenv)?;

                    inits.push(init);
                    vars[i].initval = Some(inits[i].clone());
                    i += 1;
                    ls = ls.cdr();
                }

                let newenv = cenv.extend(
                    Sexpr::list_from_iter(
                        vars.iter()
                            .map(|lvar| sexp_cons(lvar.name.clone(), Sexpr::LVar(lvar.clone()))),
                    ),
                    Sexpr::Symbol(scm_intern("LEXICAL")),
                );

                let body = pass1_body(&body, &newenv)?;

                Ok(P(IForm::Let(Let {
                    typ: LetType::Let,
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

            if !bindings.is_list() {
                return Err(format!("illegal let: {}", form));
            }

            let mut lvar = P(LVar {
                name: lambda.clone(),
                initval: None,
                arg: false,
                ref_count: 0,
                boxed: false,
                set_count: 0,
            });

            let mut args = Vec::new();

            let mut ls = bindings.clone();

            while ls.is_pair() {
                let kv = ls.car();
                if !kv.cdr().is_pair() || !kv.cdr().cdr().is_null() {
                    return Err(format!("illegal let: {}", form));
                }

                let var = kv.car();
                if !matches!(var, Sexpr::Identifier(_) | Sexpr::Symbol(_)) {
                    return Err(format!("illegal let: {}", form));
                }

                let lvar = P(LVar {
                    name: var.clone(),
                    initval: None,
                    arg: false,
                    boxed: false,
                    ref_count: 0,
                    set_count: 0,
                });

                args.push(lvar);

                ls = ls.cdr();
            }

            let argenv = cenv.clone();
            let env1 = cenv.extend(
                Sexpr::list(&[sexp_cons(lambda.clone(), Sexpr::LVar(lvar.clone()))]),
                Sexpr::Symbol(scm_intern("LEXICAL")),
            );

            let env2 = env1.extend(
                Sexpr::list_from_iter(
                    args.iter()
                        .map(|lvar| sexp_cons(lvar.name.clone(), Sexpr::LVar(lvar.clone()))),
                ),
                Sexpr::Symbol(scm_intern("LEXICAL")),
            );
            let body = pass1_body(&body, &env2)?;

            let lam = P(IForm::Lambda(P(Lambda {
                name: None,
                reqargs: args.len() as _,
                optarg: false,
                lvars: args,
                body,
                flag: LambdaFlag::Used,
                calls: vec![],
                lifted_var: LiftedVar::NotLifted,
                free_lvars: Default::default(),
                bound_lvars: Default::default(),
                defs: Default::default(),
            })));

            lvar.initval = Some(lam.clone());

            let mut cargs = Vec::with_capacity(bindings.list_length().unwrap());

            let mut ls = bindings.clone();

            while ls.is_pair() {
                let kv = ls.car();
                cargs.push(pass1(&kv.cadr(), &argenv)?);
                ls = ls.cdr();
            }

            let letrec = P(IForm::Let(Let {
                typ: LetType::Rec,
                lvars: vec![lvar.clone()],
                inits: vec![lam],
                body: P(IForm::Call(Call {
                    proc: P(IForm::LRef(LRef { lvar })),
                    args: cargs,
                    flag: CallFlag::None,
                })),
            }));

            Ok(letrec)
        }
    });

    define_syntax!("letrec", form, cenv, {
        expand_letrec(form, cenv, LetType::Rec)
    });

    define_syntax!("begin", form, cenv, {
        let mut seq = Vec::new();

        let mut ls = form.cdr();

        while ls.is_pair() {
            seq.push(pass1(&ls.car(), cenv)?);
            ls = ls.cdr();
        }

        if seq.len() == 1 {
            Ok(seq[0].clone())
        } else {
            Ok(P(IForm::Seq(Seq { forms: seq })))
        }
    });

    // define-syntax in core compiler supports only
    // `(define-syntax name (syntax-rules ...))` form
    define_syntax!("define-syntax", form, cenv, {
        let name = form.cadr();
        let synrules = form.caddr();
        let mut ncenv = cenv.clone();

        let transformer = {
            let synrule_id = synrules.car();

            if !sexp_eq(&synrule_id, &Sexpr::Symbol(scm_intern("syntax-rules"))) {
                return Err(format!("illegal define-syntax: {}: {}", form, synrule_id));
            }

            let mut literals = synrules.cadr();
            let ellipsis = if matches!(literals, Sexpr::Symbol(_) | Sexpr::Identifier(_)) {
                literals = synrules.caddr();
                Some(synrules.cadr())
            } else {
                None
            };

            let rules = if ellipsis.is_some() {
                synrules.cdddr()
            } else {
                synrules.cddr()
            };

            let Sexpr::SyntaxRules(sr) = compile_syntax_rules(
                name.clone(),
                ellipsis.unwrap_or(Sexpr::Boolean(true)),
                literals,
                rules,
                cenv.syntax_env.clone(),
                cenv.frames.clone(),
            )?
            else {
                unreachable!()
            };

            sr
        };

        let id = if let Sexpr::Identifier(mut id) = name {
            let sym = Sexpr::Gensym(P(scm_symbol_str(unwrap_identifier(id.clone())).to_string()));
            id.name = sym;
            id
        } else {
            make_identifier(name, cenv.syntax_env.clone(), Sexpr::Null)
        };

        let denotation = Denotation::Macro(transformer);

        ncenv.syntax_env.env.insert(
            Value::encode_object_value(unwrap_identifier(id.clone()).get_object()),
            P(denotation),
        );

        Ok(P(IForm::Const(Sexpr::Undefined)))
    });

    define_syntax!("let-syntax", form, cenv, {
        let mut bindings = form.cadr();
        let body = form.cddr();
        let mut transformers = vec![];
        while bindings.is_pair() {
            let spec = bindings.car();
            let name = spec.car();
            let synrules = spec.cadr();
            let transformer = {
                let synrule_id = synrules.car();

                if !sexp_eq(&synrule_id, &Sexpr::Symbol(scm_intern("syntax-rules"))) {
                    return Err(format!("illegal define-syntax: {}: {}", form, synrule_id));
                }

                let mut literals = synrules.cadr();
                let ellipsis = if matches!(literals, Sexpr::Symbol(_) | Sexpr::Identifier(_)) {
                    literals = synrules.caddr();
                    Some(synrules.cadr())
                } else {
                    None
                };

                let rules = if ellipsis.is_some() {
                    synrules.cdddr()
                } else {
                    synrules.cddr()
                };

                let Sexpr::SyntaxRules(sr) = compile_syntax_rules(
                    name.clone(),
                    ellipsis.unwrap_or(Sexpr::Boolean(true)),
                    literals,
                    rules,
                    cenv.syntax_env.clone(),
                    cenv.frames.clone(),
                )?
                else {
                    unreachable!()
                };

                sr
            };

            let id = name;

            let denotation = transformer;
            transformers.push((id, denotation));
            bindings = bindings.cdr();
        }

        let mut ncenv = cenv.extend(
            Sexpr::list_from_iter(
                transformers
                    .drain(..)
                    .map(|(id, denot)| sexp_cons(id, Sexpr::SyntaxRules(denot))),
            ),
            Sexpr::Symbol(scm_intern("SYNTAX")),
        );
        pass1_body(&body, &mut ncenv)
    });

    define_syntax!("quote", form, _cenv, {
        let datum = form.cadr();
        Ok(P(IForm::Const(datum)))
    });

    let denotation_of_define = env.env.get(&scm_intern("define")).unwrap().clone();
    let denotation_of_begin = env.env.get(&scm_intern("begin")).unwrap().clone();

    env.denotation_of_define = Some(denotation_of_define);
    env.denotation_of_begin = Some(denotation_of_begin);

    env
}

fn expand_letrec(form: Sexpr, cenv: &Cenv, typ: LetType) -> Result<P<IForm>, String> {
    let bindings = form.cadr();
    let body = form.cddr();

    if bindings.is_null() {
        return pass1_body(&body, cenv);
    } else {
        if !bindings.is_list() {
            return Err(format!("illegal letrec: {}", form));
        }

        let mut vars = vec![];

        let mut ls = bindings.clone();

        while ls.is_pair() {
            let kv = ls.car();

            let var = kv.car();
            if !kv.cdr().is_pair() || !kv.cddr().is_null() {
                return Err(format!("illegal letrec: {}", form));
            }

            if !matches!(var, Sexpr::Symbol(_) | Sexpr::Identifier(_)) {
                return Err(format!("illegal letrec: {}", form));
            }

            let lvar = P(LVar {
                name: var,
                initval: None,
                boxed: false,
                arg: false,
                ref_count: 0,
                set_count: 0,
            });

            vars.push(lvar.clone());

            ls = ls.cdr();
        }

        let newnev = cenv.extend(
            Sexpr::list_from_iter(
                vars.iter()
                    .map(|lvar| sexp_cons(lvar.name.clone(), Sexpr::LVar(lvar.clone()))),
            ),
            Sexpr::Symbol(scm_intern("LEXICAL")),
        );

        let mut inits = Vec::with_capacity(vars.len());

        let mut ls = bindings.clone();
        let mut i = 0;
        while ls.is_pair() {
            let kv = ls.car();

            let init = kv.cadr();
            let mut var = vars[i].clone();
            i += 1;

            let iexpr = pass1(&init, &newnev)?;

            var.initval = Some(iexpr.clone());
            inits.push(iexpr);

            ls = ls.cdr();
        }

        let body = pass1_body(&body, &newnev)?;

        Ok(P(IForm::Let(Let {
            typ,
            lvars: vars,
            inits,
            body,
        })))
    }
}

fn pass1_define(form: Sexpr, oform: Sexpr, cenv: &Cenv) -> Result<P<IForm>, String> {
    let name = form.cadr();

    // (_ (name . args) body ...)
    if name.is_pair() {
        let orig = name.clone();
        let name = name.car();
        let args = orig.cdr();
        let body = form.cddr();

        let lambda = Sexpr::list_star(&[Sexpr::Symbol(scm_intern("lambda")), args, body]);

        let define = Sexpr::list(&[Sexpr::Symbol(scm_intern("define")), name, lambda]);

        pass1_define(define, orig, cenv)
    } else if form.cddr().is_null() {
        // R6RS style (define <name>)
        todo!()
    } else if form.cddr().cdr().is_null() {
        let value = form.caddr();

        if !matches!(name, Sexpr::Identifier(_) | Sexpr::Symbol(_)) {
            return Err(format!("illegal define: {}", oform));
        }

        let id = if let Sexpr::Identifier(mut id) = name {
            let sym = Sexpr::Gensym(P(scm_symbol_str(unwrap_identifier(id.clone())).to_string()));
            id.name = sym;
            id
        } else {
            make_identifier(name, cenv.syntax_env.clone(), Sexpr::Null)
        };

        let mut value = pass1(&value, cenv)?;

        if let IForm::Lambda(ref mut lam) = &mut *value {
            lam.name = Some(unmangled(scm_symbol_str(unwrap_identifier(id.clone()))).to_string());
        }

        Ok(P(IForm::Define(Define {
            name: Sexpr::Identifier(id),
            value,
        })))
    } else {
        return Err(format!("illegal define: {}", oform));
    }
}

fn pass1_vanilla_lambda(
    _form: Sexpr,
    formals: Sexpr,
    nreqs: usize,
    nopts: usize,
    body: Sexpr,
    cenv: &Cenv,
) -> Result<P<IForm>, String> {
    let mut lvars = Vec::with_capacity(4);

    let mut ls = formals.clone();

    while ls.is_pair() {
        let lvar = P(LVar {
            name: ls.car(),
            initval: None,
            arg: false,
            ref_count: 0,
            set_count: 0,
            boxed: false,
        });

        lvars.push(lvar);

        ls = ls.cdr();
    }

    let lvars_list = Sexpr::list_from_iter(lvars.iter().map(|x| Sexpr::LVar(x.clone())));

    let cenv = cenv.extend(
        Sexpr::list_map2(|x, y| sexp_cons(x.clone(), y.clone()), formals, lvars_list),
        Sexpr::Symbol(scm_intern("LEXICAL")),
    );

    let body = pass1_body(&body, &cenv)?;

    Ok(P(IForm::Lambda(P(Lambda {
        name: None,
        reqargs: nreqs as _,
        optarg: nopts != 0,
        lvars,
        body,
        flag: LambdaFlag::None,
        calls: vec![],
        free_lvars: Default::default(),
        bound_lvars: Default::default(),
        defs: Default::default(),
        lifted_var: LiftedVar::NotLifted,
    }))))
}

fn pass1_body(expr: &Sexpr, cenv: &Cenv) -> Result<P<IForm>, String> {
    pass1_body_rec(
        expr.clone(),
        Sexpr::Boolean(false),
        Sexpr::Boolean(false),
        cenv,
    )
}

fn pass1_body_rec(
    exprs: Sexpr,
    mframe: Sexpr,
    mut vframe: Sexpr,
    cenv: &Cenv,
) -> Result<P<IForm>, String> {
    fn dupe_check(var: Sexpr, mframe: Sexpr, vframe: Sexpr) -> Result<(), String> {
        if (mframe.is_pair() && !matches!(mframe.assq(&var), Sexpr::Boolean(false)))
            || (vframe.is_pair() && !matches!(vframe.assq(&var), Sexpr::Boolean(false)))
        {
            Err(format!("duplicate variable: {}", var))
        } else {
            Ok(())
        }
    }

    match exprs {
        exprs if exprs.is_pair() && exprs.car().is_pair() => {
            let rest = exprs.cdr();
            let op = exprs.caar();
            let args = exprs.cdar();

            if matches!(vframe, Sexpr::Boolean(false))
                || matches!(vframe.assq(&op), Sexpr::Boolean(false))
                    && matches!(op, Sexpr::Identifier(_) | Sexpr::Symbol(_))
            {
                let head = cenv.lookup(op.clone());

                if !args.is_list() {
                    return Err(format!(
                        "proper list is required for application or macro use but got: {}",
                        args
                    ));
                }

                match head {
                    Sexpr::LVar(_) => return pass1_body_finish(exprs, mframe, vframe, cenv),

                    Sexpr::Special(_) => return pass1_body_finish(exprs, mframe, vframe, cenv),

                    Sexpr::SyntaxRules(sr) => {
                        let expanded = synrule_expand(
                            exprs.car(),
                            cenv.syntax_env.clone(),
                            cenv.frames.clone(),
                            &sr,
                        )?;

                        return pass1_body_rec(
                            Sexpr::list_star(&[expanded, rest]),
                            mframe,
                            vframe,
                            cenv,
                        );
                    }

                    head if head.is_pair()
                        && sexp_eq(&head.car(), &Sexpr::Symbol(scm_intern(":rec"))) =>
                    {
                        return pass1_body_finish(exprs, mframe, vframe, cenv);
                    }

                    head if !matches!(head, Sexpr::Identifier(_)) => {
                        panic!("[internal] pass1/body: {}", head)
                    }

                    _ => {
                        if !matches!(head, Sexpr::Identifier(_)) {
                            panic!("pass1/body: {}", head)
                        }
                        let Sexpr::Identifier(head) = head else {
                            unreachable!()
                        };
                        let denotation = cenv.syntax_env.env.get(&Value::encode_object_value(
                            unwrap_identifier(head.clone()).get_object(),
                        ));
                        match denotation {
                            Some(denotation) => {
                                match &**denotation {
                                    Denotation::Macro(sr) => {
                                        let expanded = synrule_expand(
                                            exprs.car(),
                                            cenv.syntax_env.clone(),
                                            cenv.frames.clone(),
                                            &sr,
                                        )?;

                                        return pass1_body_rec(
                                            Sexpr::list_star(&[expanded, rest]),
                                            mframe,
                                            vframe,
                                            cenv,
                                        );
                                    }

                                    Denotation::Special(_) => {
                                        if denotation.as_ptr()
                                            == cenv
                                                .syntax_env
                                                .denotation_of_begin
                                                .clone()
                                                .unwrap()
                                                .as_ptr()
                                        {
                                            return pass1_body_rec(
                                                Sexpr::append(args, rest),
                                                mframe,
                                                vframe,
                                                cenv,
                                            );
                                        } else if denotation.as_ptr()
                                            == cenv
                                                .syntax_env
                                                .denotation_of_define
                                                .clone()
                                                .unwrap()
                                                .as_ptr()
                                        {
                                            let def = match args {
                                                // match ((name . formals) . body)
                                                args if args.is_pair() && args.car().is_pair() => {
                                                    let name = args.caar();
                                                    let formals = args.cdar();
                                                    let body = args.cdr();

                                                    let lam = Sexpr::list_star(&[
                                                        Sexpr::Symbol(scm_intern("lambda")),
                                                        formals,
                                                        body,
                                                    ]);
                                                    let rec = Sexpr::list(&[
                                                        name,
                                                        Sexpr::Symbol(scm_intern(":rec")),
                                                        lam,
                                                    ]);

                                                    rec
                                                }
                                                // (var init)
                                                args if args.is_pair()
                                                    && args.cdr().is_pair()
                                                    && args.cddr().is_null() =>
                                                {
                                                    let var = args.car();
                                                    let init = args.cadr();

                                                    let rec = Sexpr::list(&[
                                                        var,
                                                        Sexpr::Symbol(scm_intern(":rec")),
                                                        init,
                                                    ]);

                                                    rec
                                                }
                                                _ => {
                                                    return Err(format!("illegal define: {}", args))
                                                }
                                            };

                                            dupe_check(def.car(), mframe.clone(), vframe.clone())?;

                                            if matches!(mframe, Sexpr::Null)
                                                || matches!(mframe, Sexpr::Boolean(false))
                                            {
                                                let cenv = cenv.extend(
                                                    Sexpr::Null,
                                                    Sexpr::Symbol(scm_intern("SYNTAX")),
                                                );
                                                let mframe = cenv.frames.car();
                                                let cenv = cenv.extend(
                                                    Sexpr::list(&[def]),
                                                    Sexpr::Symbol(scm_intern("LEXICAL")),
                                                );
                                                let vframe = cenv.frames.car();

                                                return pass1_body_rec(rest, mframe, vframe, &cenv);
                                            } else {
                                                let r = vframe.cdr();
                                                vframe.set_cdr(sexp_cons(def, r));

                                                return pass1_body_rec(rest, mframe, vframe, cenv);
                                            }
                                        }

                                        return pass1_body_finish(exprs, mframe, vframe, cenv);
                                    }
                                }
                            }
                            None => return pass1_body_finish(exprs, mframe, vframe, cenv),
                        }
                    }
                }
            } else {
                pass1_body_finish(exprs, mframe, vframe, cenv)
            }
        }

        _ => pass1_body_finish(exprs, mframe, vframe, cenv),
    }
}

fn pass1_body_finish(
    exprs: Sexpr,
    mframe: Sexpr,
    mut vframe: Sexpr,
    cenv: &Cenv,
) -> Result<P<IForm>, String> {
    if matches!(mframe, Sexpr::Boolean(false) | Sexpr::Null) {
        pass1_body_rest(exprs, cenv)
    } else {
        let intdefs = vframe.cdr().list_reverse();
        let vars = intdefs.list_map(|x| x.car());
        let mut lvars = Vec::new();

        let mut vs = vars.clone();

        while vs.is_pair() {
            let var = vs.car();
            let lvar = P(LVar {
                name: var.clone(),
                initval: None,
                arg: false,
                boxed: false,
                ref_count: 0,
                set_count: 0,
            });

            lvars.push(lvar);
            vs = vs.cdr();
        }

        let lvars_list = Sexpr::list_from_iter(lvars.iter().map(|lvar| Sexpr::LVar(lvar.clone())));
        vframe.set_cdr(Sexpr::list_map2(
            |e1, e2| sexp_cons(e1.clone(), e2.clone()),
            vars.clone(),
            lvars_list.clone(),
        ));

        let mut inits = vec![];

        let inits_expr = intdefs.list_map(|x| x.cddr());

        let mut i = 0;

        let mut ls = inits_expr;

        while ls.is_pair() {
            let init = ls.car();
            let lvar = lvars[i].clone();
            i += 1;
            let iexpr = pass1_body_init(lvar, init.car(), cenv)?;
            inits.push(iexpr);
            ls = ls.cdr();
        }

        let body = pass1_body_rest(exprs, cenv)?;

        Ok(P(IForm::Let(Let {
            typ: LetType::Rec,
            lvars,
            inits,
            body,
        })))
    }
}

fn pass1_body_init(mut lvar: P<LVar>, init: Sexpr, newenv: &Cenv) -> Result<P<IForm>, String> {
    let init = pass1(&init, newenv)?;

    lvar.initval = Some(init.clone());

    Ok(init)
}

fn pass1_body_rest(exprs: Sexpr, cenv: &Cenv) -> Result<P<IForm>, String> {
    if exprs.is_null() {
        Ok(P(IForm::Const(Sexpr::Undefined)))
    } else if exprs.is_pair() && exprs.cdr().is_null() {
        pass1(&exprs.car(), cenv)
    } else {
        let mut seq = Vec::new();

        let mut ls = exprs.clone();

        while ls.is_pair() {
            seq.push(pass1(&ls.car(), cenv)?);
            ls = ls.cdr();
        }

        Ok(P(IForm::Seq(Seq { forms: seq })))
    }
}

struct Expander {
    env: P<SyntaxEnv>,
}

unsafe impl Send for Expander {}

static EXPANDER: Lazy<Mutex<Expander>> = Lazy::new(|| {
    Mutex::new(Expander {
        env: define_syntax(),
    })
});

pub fn expand_default(expr: &Sexpr) -> Result<P<IForm>, String> {
    let expander = EXPANDER.lock(true);

    let res = pass1(
        expr,
        &Cenv {
            syntax_env: expander.env.clone(),
            frames: Sexpr::Null,
        },
    );

    drop(expander);

    res
}
