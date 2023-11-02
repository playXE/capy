//! Expander for R5RS Scheme code.
//!
//! Converts Scheme forms into TreeIL forms. Internal definitions are converted into `rec*` forms.

use super::env::*;
use super::sexpr::*;
use super::tree_il::*;
use crate::reader::SourceLocation;
use crate::syntaxrules::SyntaxRules;
use crate::{
    env::{Environment, Global},
    rc::Rc,
    reader::{SourceProvider, SymbolInterner},
    sexpr::Symbol,
};
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Clone)]
pub struct Cenv {
    pub expr_name: Option<Rc<Symbol>>,
    pub sources: SourceProvider,
    pub env: Rc<Environment>,
    pub global_env: Rc<Environment>,
    pub interner: Rc<SymbolInterner>,
    pub denotation_of_define: Option<Rc<Special>>,
    pub denotation_of_begin: Option<Rc<Special>>,
}

impl Cenv {
    fn source(&self, expr: &Sexpr) -> Option<SourceLocation> {
        self.sources.get(expr)
    }

    fn mark_source(&self, expr: &Sexpr, loc: SourceLocation) {
        self.sources.mark(expr, loc);
    }
}

/// Returns default global environment.
///
/// It defines `define`, `lambda` and other special forms.
pub fn global_env(interner: &Rc<SymbolInterner>) -> Rc<Environment> {
    let mut env = Global {
        bindings: HashMap::new(),
    };

    macro_rules! define_special {
        ($name: literal, $rname: ident, $form: ident, $cenv: ident => $b: block) => {
            fn $rname($form: &Sexpr, $cenv: &Cenv) -> Result<Rc<TreeNode>, ScmError> {
                $b
            }

            env.bindings
                .insert(interner.intern($name), Definition::Special(Rc::new($rname)));
        };
    }

    define_special!("begin", begin, form, cenv => {
        let mut seq = vec![];

        let mut args = form.cdr();

        while let Some((arg, rest)) = args.pair() {
            seq.push(expand(arg, cenv)?);
            args = rest;
        }

        if !args.is_null() {
            return Err(ScmError::UnexpectedType(
                form.clone(),
                "begin requires a proper list of expressions",
            ));
        }

        Ok(make_seq(seq))
    });

    define_special!("define", define, form, cenv => {
        expand_define(form.clone(), form.clone(), cenv)
    });

    define_special!("lambda", lambda, form, cenv => {
        if form.list_length().filter(|x| *x >= 3).is_some() {
            let formals = form.cadr().clone();
            let body = form.cddr();


            fn parse_formals(xs: Sexpr, ys: Sexpr) -> Result<(Sexpr, Sexpr), ScmError> {
                if xs.is_null() {
                    Ok((ys.reverse(), Sexpr::Boolean(false)))
                } else if matches!(xs, Sexpr::Symbol(_)) {
                    Ok((ys.reverse(), xs))
                } else if xs.is_pair() {
                    if !matches!(xs.car(), Sexpr::Symbol(_)) {
                        return Err(ScmError::UnexpectedType(xs.car().clone(), "symbol expected"));
                    }

                    parse_formals(xs.cdr().clone(), cons(xs.car().clone(), ys))
                } else {
                    Err(ScmError::UnexpectedType(xs.clone(), "invalid formal parameter"))
                }
            }

            let (reqs, rest) = parse_formals(formals, Sexpr::Null)?;

            expand_vanilla_lambda(if !rest.is_boolean() {
                reqs.append(&rest)
            } else{
                reqs
            }, 0, rest.is_symbol() as usize, body.clone(), cenv)

        } else {
            return Err(ScmError::UnexpectedType(
                form.clone(),
                "lambda requires a list of parameters and a body",
            ));
        }
    });

    define_special!("if", test, form, cenv => {
        let len = form.list_length().ok_or_else(|| ScmError::UnexpectedType(
            form.clone(),
            "if requires a proper list",
        ))?;

        if len == 3 {
            let test = form.cadr();
            let consequent = form.caddr();

            let test = expand(&test, cenv)?;
            let consequent = expand(&consequent, cenv)?;

            return Ok(make_test(test, consequent, make_constant(Sexpr::Unspecified)));
        } else if len == 4 {
            let test = form.cadr();
            let consequent = form.caddr();
            let alternative = form.cdddr().car();

            let test = expand(&test, cenv)?;
            let consequent = expand(&consequent, cenv)?;
            let alternative = expand(&alternative, cenv)?;

            return Ok(make_test(test, consequent, alternative));
        } else {
            return Err(ScmError::UnexpectedType(
                form.clone(),
                "if requires a test expression and two consequent expressions",
            ));
        }
    });

    define_special!("let", bind, form, cenv => {
        if form.list_length().filter(|x| *x >= 3).is_some() {
            let bindings = form.cadr();

            // (let name ((<var> <expr>) ...) <body>
            if bindings.is_symbol() {
                let name = bindings.symbol().unwrap();
                let bindings = form.caddr();
                let body = form.cdddr();

                let mut bindings_vec = Vec::with_capacity(4);
                let mut ls = bindings;

                while let Some((var_expr, rest)) = ls.pair() {
                    if var_expr.list_length() != Some(2) {
                        return Err(ScmError::UnexpectedType(
                            var_expr.clone(),
                            "(<var> <expr>) expected in `let`",
                        ));
                    }

                    let Some(name) = var_expr.car().symbol() else {
                        return Err(ScmError::UnexpectedType(
                            var_expr.clone(),
                            "symbol expected in `let` for variable name",
                        ));
                    };

                    let var = make_variable(name.clone());
                    let binding = expand(var_expr.cadr(), cenv)?;

                    bindings_vec.push((var, binding));

                    ls = rest;
                }

                if !ls.is_null() {
                    return Err(ScmError::UnexpectedType(
                        form.clone(),
                        "proper list of bindings expected in `let`",
                    ));
                }

                let lam_var = make_variable(name.clone());
                let env_rec = Lexical {
                    bindings: HashMap::from_iter(std::iter::once((name.clone(), Definition::Variable(lam_var.clone())))),
                    parent: cenv.env.clone(),
                };

                let env_rec = Rc::new(Environment::Lexical(env_rec));

                let env_lambda = Lexical {
                    bindings: HashMap::from_iter(bindings_vec.iter().map(|(x, _)| (x.name.clone(), Definition::Variable(x.clone())))),
                    parent: env_rec,
                };

                let env_lambda = Rc::new(Environment::Lexical(env_lambda));
                let src = cenv.source(body);
                let body = expand_body(&body, &Cenv {
                    env: env_lambda,
                    ..cenv.clone()
                })?;

                let proc = make_proc(
                    Some(name.clone()),
                    vec![make_proc_case(
                        CaseInfo {
                            formals: bindings_vec.iter().map(|(x, _)| x.clone()).collect(),
                            rest: false,
                            proper: true,
                        },
                        body,
                    )],
                );

                let funcall = make_fun_call(
                    make_ref(lam_var.clone()),
                    bindings_vec
                        .iter()
                        .map(|(_, init)| init.clone())
                        .collect::<Vec<_>>()
                        .as_slice(),
                    src);

                let bind = make_rec(
                    vec![(lam_var, Rc::new(TreeNode::Proc(proc)))],
                    funcall
                );

                Ok(bind)
            } else if bindings.is_pair() {
                let body = form.cddr();

                let mut vars = Vec::with_capacity(4);
                let mut bindings_vec = Vec::with_capacity(4);
                let mut ls = bindings;

                while let Some((var_expr, rest)) = ls.pair() {
                    if var_expr.list_length() != Some(2) {
                        return Err(ScmError::UnexpectedType(
                            var_expr.clone(),
                            "(<var> <expr>) expected in `let`",
                        ));
                    }

                    let Some(name) = var_expr.car().symbol() else {
                        return Err(ScmError::UnexpectedType(
                            var_expr.clone(),
                            "symbol expected in `let` for variable name",
                        ));
                    };

                    let var = make_variable(name.clone());
                    let binding = expand(var_expr.cadr(), cenv)?;

                    vars.push(var);
                    bindings_vec.push(binding);

                    ls = rest;
                }

                if !ls.is_null() {
                    return Err(ScmError::UnexpectedType(
                        form.clone(),
                        "proper list of bindings expected in `let`",
                    ));
                }

                let mut cenv = cenv.clone();
                let env = Lexical {
                    bindings: HashMap::from_iter(vars.iter().map(|x| (x.name.clone(), Definition::Variable(x.clone())))),
                    parent: cenv.env.clone(),
                };

                cenv.env = Rc::new(Environment::Lexical(env));

                let body = expand_body(&body, &cenv)?;

                Ok(make_bind(vars, bindings_vec, body))
            } else {
                todo!()
            }
        } else {
            return Err(ScmError::UnexpectedType(
                form.clone(),
                "(let ((<var> <expr>) ...) <body>) or (let name ((<var> <expr>) ...) <body>) expected",
            ));
        }
    });

    define_special!("letrec", rec, form, cenv => {
        if form.list_length().filter(|x| *x >= 3).is_some() {
            let bindings = form.cadr();
            let body = form.cddr();

            let mut vars = Vec::with_capacity(4);
            let mut bindings_vec = Vec::with_capacity(4);

            let mut ls = bindings;

            while let Some((var_expr, rest)) = ls.pair() {
                if var_expr.list_length() != Some(2) {
                    return Err(ScmError::UnexpectedType(
                        var_expr.clone(),
                        "(<var> <expr>) expected in `letrec`",
                    ));
                }

                let Some(name) = var_expr.car().symbol() else {
                    return Err(ScmError::UnexpectedType(
                        var_expr.clone(),
                        "symbol expected in `letrec` for variable name",
                    ));
                };

                let var = make_variable(name.clone());

                vars.push(var);
                bindings_vec.push(var_expr.cadr().clone());

                ls = rest;
            }


            let cenv = Cenv {
                env: Rc::new(Environment::Lexical(Lexical {
                    bindings: HashMap::from_iter(vars.iter().map(|x| (x.name.clone(), Definition::Variable(x.clone())))),
                    parent: cenv.env.clone(),
                })),
                ..cenv.clone()
            };

            let bindings = bindings_vec
                .iter()
                .map(|x| expand(x, &cenv))
                .collect::<Result<Vec<_>, _>>()?;

            let body = expand_body(&body, &cenv)?;

            Ok(make_rec(vars.iter().zip(bindings.iter()).map(|(var, expr)| (var.clone(), expr.clone())).collect(), body))
        } else {
            return Err(ScmError::UnexpectedType(
                form.clone(),
                "(letrec ((<var> <expr>) ...) <body>) expected",
            ));
        }
    });

    define_special!("letrec*", recstar, form, cenv => {
        if form.list_length().filter(|x| *x >= 3).is_some() {
            let bindings = form.cadr();
            let body = form.cddr();

            let mut vars = Vec::with_capacity(4);
            let mut bindings_vec = Vec::with_capacity(4);

            let mut ls = bindings;

            while let Some((var_expr, rest)) = ls.pair() {
                if var_expr.list_length() != Some(2) {
                    return Err(ScmError::UnexpectedType(
                        var_expr.clone(),
                        "(<var> <expr>) expected in `letrec*`",
                    ));
                }

                let Some(name) = var_expr.car().symbol() else {
                    return Err(ScmError::UnexpectedType(
                        var_expr.clone(),
                        "symbol expected in `letrec` for variable name",
                    ));
                };

                let var = make_variable(name.clone());

                vars.push(var);
                bindings_vec.push(var_expr.cadr().clone());

                ls = rest;
            }


            let cenv = Cenv {
                env: Rc::new(Environment::Lexical(Lexical {
                    bindings: HashMap::from_iter(vars.iter().map(|x| (x.name.clone(), Definition::Variable(x.clone())))),
                    parent: cenv.env.clone(),
                })),
                ..cenv.clone()
            };

            let bindings = bindings_vec
                .iter()
                .map(|x| expand(x, &cenv))
                .collect::<Result<Vec<_>, _>>()?;

            let body = expand_body(&body, &cenv)?;

            Ok(make_rec(vars.iter().zip(bindings.iter()).map(|(var, expr)| (var.clone(), expr.clone())).collect(), body))
        } else {
            return Err(ScmError::UnexpectedType(
                form.clone(),
                "(letrec* ((<var> <expr>) ...) <body>) expected",
            ));
        }
    });

    // (define-syntax-rules <name> (<literal-id> ...)
    //  [(id . pattern) template] ...)
    define_special!("define-syntax-rules", define_syntax_rules, form, cenv => {
        if form.list_length().filter(|x| *x >= 3).is_some() {
            let name = form.cadr().symbol().clone().ok_or_else(|| ScmError::UnexpectedType(
                form.clone(),
                "symbol expected for syntax-rules name",
            ))?;
            
            let mut ellipsis = None::<Rc<Symbol>>;
            let lit;
            let mut trans_rules;

            let form = form.cddr();

            match form {
                f if f.car().is_symbol() && f.cdr().is_pair() => {
                    ellipsis = f.car().symbol().map(|x| Symbol::root(x).clone());
                    lit = f.cdr().car();
                    trans_rules = f.cdr().cdr();
                }

                f if f.is_pair() => {
                    lit = f.car();
                    trans_rules = f.cdr();
                }

                _ => {
                    return Err(ScmError::UnexpectedType(
                        form.clone(),
                        "(define-syntax-rules <name> (<literal-id> ...)
                        [(id . pattern) template] ...) expected",
                    ));
                }
            }

            let mut patterns = Vec::new();
            let mut templates = Vec::new();

            while let Some((rule, rest)) = trans_rules.pair() {
                let Some((car, Some((cadr, Sexpr::Null)))) = rule.pair().map(|(car, cdr)| (car, cdr.pair())) else {
                    return Err(ScmError::UnexpectedType(
                        rule.clone(),
                        "[(id . pattern) template] expected",
                    ));
                };

                let Some((_, pat)) = car.pair() else {
                    return Err(ScmError::UnexpectedType(
                        car.clone(),
                        "[(id . pattern) template] expected",
                    ));
                };
                
                patterns.push(pat.clone());
                templates.push(cadr.clone());

                trans_rules = rest;
            }

            let mut literal_set = HashSet::new();

            let mut expr = lit;

            while let Some((Some(sym), cdr)) = expr.pair().map(|(car, cdr)| (car.symbol(), cdr)) {
                literal_set.insert(Symbol::root(sym).clone());

                expr = cdr;
            }

            if !expr.is_null() {
                return Err(ScmError::UnexpectedType(
                    lit.clone(),
                    "literal identifier list expected",
                ));
            }

            let rules = SyntaxRules::new(cenv.interner.clone(), Some(name.clone()), literal_set, ellipsis, patterns, templates, cenv.env.clone());
            let mut env = cenv.env.clone();
            match &mut *env {
                Environment::Global(global) => {
                    global.bindings.insert(name.clone(), Definition::Macro(Rc::new(rules)));

                    Ok(make_constant(Sexpr::Unspecified))
                }

                _ => return Err(ScmError::Custom(form.clone(), format!("define-syntax-rules in non-global environment"))),
            }
        } else {
            return Err(ScmError::UnexpectedType(
                form.clone(),
                "(define-syntax-rules <name> (<literal-id> ...)
                [(id . pattern) template] ...) expected",
            ));
        }
    });

    define_special!("quote", quote, form, _cenv => {
        Ok(make_constant(form.cadr().clone()))
    });

    Rc::new(Environment::Global(env))
}

fn expand_vanilla_lambda(
    formals: Sexpr,
    _nreqs: usize,
    nopts: usize,
    body: Sexpr,
    cenv: &Cenv,
) -> Result<Rc<TreeNode>, ScmError> {
    let mut vars = Vec::with_capacity(4);
    let mut ls = formals.clone();

    let mut env = Lexical {
        bindings: HashMap::new(),
        parent: cenv.env.clone(),
    };

    while let Some((formal, rest)) = ls.pair() {
        let var = make_variable(formal.symbol().unwrap().clone());
        env.bindings.insert(
            formal.symbol().unwrap().clone(),
            Definition::Variable(var.clone()),
        );
        vars.push(var);
        ls = rest.clone();
    }

    if let Sexpr::Symbol(ref sym) = ls {
        let var = make_variable(sym.clone());
        env.bindings
            .insert(sym.clone(), Definition::Variable(var.clone()));
        vars.push(var);
    }

    let cenv = Cenv {
        env: Rc::new(Environment::Lexical(env)),
        ..cenv.clone()
    };

    let body = expand_body(&body, &cenv)?;

    Ok(Rc::new(TreeNode::Proc(make_proc(
        cenv.expr_name.clone(),
        vec![make_proc_case(
            CaseInfo {
                formals: vars,
                rest: nopts != 0,
                proper: true,
            },
            body,
        )],
    ))))
}

fn expand_define(form: Sexpr, oform: Sexpr, cenv: &Cenv) -> Result<Rc<TreeNode>, ScmError> {
    let name = form.cadr();

    // (_ (name . args) body ...)
    if name.is_pair() {
        let orig = name.clone();

        let name = name.car();
        let args = orig.cdr();
        let body = form.cddr();

        let lambda = Sexpr::make_list_star(
            &[Sexpr::Symbol(cenv.interner.intern("lambda")), args.clone()],
            body.clone(),
        );

        if let Some(src) = cenv.source(&oform) {
            cenv.mark_source(&lambda, src);
        }

        let define = Sexpr::make_list(&[
            Sexpr::Symbol(cenv.interner.intern("define")),
            name.clone(),
            lambda,
        ]);

        return expand_define(define, orig.clone(), cenv);
    } else if form.cddr().is_null() {
        // R6RS style (define <name>)
        let name = form.cadr();
        if !name.is_symbol() {
            return Err(ScmError::UnexpectedType(
                form.clone(),
                "define requires a variable name",
            ));
        }
        return Ok(make_global_set(
            &cenv.interner,
            name.symbol().unwrap().clone(),
            make_constant(Sexpr::Unspecified),
            cenv.source(&oform),
        ));
    } else if form.cddr().cdr().is_null() {
        let value = form.caddr();

        if !name.is_symbol() {
            return Err(ScmError::UnexpectedType(
                form.clone(),
                "define requires a variable name",
            ));
        }

        let mut cenv = cenv.clone();
        cenv.expr_name = Some(name.symbol().unwrap().clone());
        let mut value = expand(&value, &cenv)?;

        if let TreeNode::Proc(proc) = &mut *value {
            proc.name = Some(name.symbol().unwrap().clone());
        }

        Ok(make_global_set(
            &cenv.interner,
            name.symbol().unwrap().clone(),
            value,
            cenv.source(&oform),
        ))
    } else {
        return Err(ScmError::UnexpectedType(
            form.clone(),
            "define requires a variable name and expression",
        ));
    }
}

fn expand_call(
    program: &Sexpr,
    proc: Rc<TreeNode>,
    mut args: &Sexpr,
    cenv: &Cenv,
) -> Result<Rc<TreeNode>, ScmError> {
    let mut iargs = Vec::new();

    while let Some((arg, rest)) = args.pair() {
        iargs.push(expand(arg, cenv)?);
        args = rest;
    }

    Ok(make_fun_call(proc, &iargs, cenv.source(program)))
}

fn lookup_head(cenv: &Cenv, head: &Sexpr) -> Option<Definition> {
    if matches!(head, Sexpr::Symbol(_)) {
        Some(cenv.env.lookup::<true>(head.symbol().unwrap()))
    } else {
        None
    }
}

pub fn expand(program: &Sexpr, cenv: &Cenv) -> Result<Rc<TreeNode>, ScmError> {
    fn global_call(program: &Sexpr, id: Rc<Symbol>, cenv: &Cenv) -> Result<Rc<TreeNode>, ScmError> {
        match cenv.global_env.lookup::<true>(&id) {
            Definition::Special(special) => special(program, cenv),
            Definition::Macro(rules) => {
                let expanded = rules.expand(program.cdr())?;
                expand(&expanded, cenv)
            }

            Definition::Primitive(primref) => {
                let mut iargs = Vec::new();
                let mut args = program.cdr();

                while let Some((arg, rest)) = args.pair() {
                    iargs.push(expand(arg, cenv)?);
                    args = rest;
                }

                Ok(make_fun_call(
                    make_primref(primref.clone()),
                    &iargs,
                    cenv.source(program),
                ))
            }

            _ => {
                let gref = make_global_ref(&cenv.interner, id.clone(), cenv.source(program));

                expand_call(program, gref, program.cdr(), cenv)
            }
        }
    }
    if program.is_pair() {
        if !program.is_proper_list() {
            return Err(ScmError::UnexpectedType(
                program.clone(),
                "proper list is required for application or macro use",
            ));
        }

        let h = lookup_head(cenv, program.car());

        match h {
            Some(Definition::Macro(rules)) => {
                let expanded = rules.expand(program.cdr())?;
                expand(&expanded, cenv)
            }

            Some(Definition::Special(special)) => special(program, cenv),

            Some(Definition::Primitive(prim)) => {
                let mut iargs = Vec::new();
                let mut args = program.cdr();

                while let Some((arg, rest)) = args.pair() {
                    iargs.push(expand(arg, cenv)?);
                    args = rest;
                }

                Ok(make_fun_call(
                    make_primref(prim.clone()),
                    &iargs,
                    cenv.source(program),
                ))
            }

            Some(Definition::Undefined) if program.car().is_symbol() => {
                global_call(program, program.car().symbol().cloned().unwrap(), cenv)
            }

            _ => {
                if !program.car().is_symbol() {
                    return Err(ScmError::UnexpectedType(
                        program.clone(),
                        "symbol is required for application or macro use",
                    ));
                }
                let rator = expand(program.car(), cenv)?;
                expand_call(program, rator, program.cdr(), cenv)
            }
        }
    } else if program.is_symbol() {
        let r = cenv.env.lookup::<true>(program.symbol().unwrap());

        match r {
            Definition::Variable(var) => Ok(make_ref(var)),
            Definition::Undefined => Ok(make_global_ref(
                &cenv.interner,
                program.symbol().cloned().unwrap(),
                cenv.source(program),
            )),
            _ => Err(ScmError::UnexpectedType(
                program.clone(),
                "illegal variable reference",
            )),
        }
    } else {
        Ok(make_constant(program.clone()))
    }
}

fn expand_body(exprs: &Sexpr, cenv: &Cenv) -> Result<Rc<TreeNode>, ScmError> {
    fn rec(
        exprs: &Sexpr,
        cenv: &Cenv,
        defs: &mut Vec<(Sexpr, Sexpr)>,
    ) -> Result<Rc<TreeNode>, ScmError> {
        if exprs.is_null() {
            return rec_finish(exprs, cenv, defs);
        } else {
            let (expr, rest) = exprs.pair().unwrap();

            if expr.is_pair() {
                let op = expr.car();
                let args = expr.cdr();

                let head = lookup_head(cenv, &op);

                match head {
                    None => {
                        return rec_finish(exprs, cenv, defs);
                    }

                    Some(Definition::Macro(rules)) => {
                        let expanded = rules.expand(expr.cdr())?;
                        
                        return rec(&cons(expanded, rest.clone()), cenv, defs);
                    }

                    Some(Definition::Special(special)) => {
                        if let Some(def) = cenv.denotation_of_define.as_ref() {
                            if Rc::ptr_eq(def, &special) {
                                if !args.is_pair() {
                                    return Err(ScmError::UnexpectedType(
                                        expr.clone(),
                                        "define requires a variable name",
                                    ));
                                }

                                let name_or_lambda = args.car();

                                if name_or_lambda.is_symbol() {
                                    let expr = args.cdr().car();
                                    if !args.cdr().cdr().is_null() {
                                        return Err(ScmError::UnexpectedType(
                                            expr.clone(),
                                            "(define <var> <expr>) expected",
                                        ));
                                    }

                                    defs.push((name_or_lambda.clone(), expr.clone()));
                                } else if name_or_lambda.is_pair() {
                                    let name = name_or_lambda.car();
                                    let params = name_or_lambda.cdr();
                                    let body = args.cdr();

                                    let lam = Sexpr::make_list_star(
                                        &[
                                            Sexpr::Symbol(cenv.interner.intern("lambda")),
                                            params.clone(),
                                        ],
                                        body.clone(),
                                    );

                                    defs.push((name.clone(), lam));
                                } else {
                                    return Err(ScmError::UnexpectedType(
                                        expr.clone(),
                                        "define requires a variable name and expression",
                                    ));
                                }

                                return rec(rest, cenv, defs);
                            }
                        }

                        if let Some(_) = cenv
                            .denotation_of_begin
                            .as_ref()
                            .filter(|b| Rc::ptr_eq(b, &special))
                        {
                            let exprs = args.append(rest);
                            return rec(&exprs, cenv, defs);
                        }

                        return rec_finish(exprs, cenv, defs);
                    }

                    Some(_) => return rec_finish(exprs, cenv, defs),
                }
            } else {
                return rec_finish(exprs, cenv, defs);
            }
        }
    }

    fn rec_finish(
        mut exprs: &Sexpr,
        cenv: &Cenv,
        defs: &mut Vec<(Sexpr, Sexpr)>,
    ) -> Result<Rc<TreeNode>, ScmError> {
        if defs.is_empty() {
            let mut nodes = Vec::new();

            while let Some((expr, rest)) = exprs.pair() {
                nodes.push(expand(expr, cenv)?);
                exprs = rest;
            }

            return Ok(make_seq(nodes));
        } else {
            let mut env = Lexical {
                bindings: HashMap::new(),
                parent: cenv.env.clone(),
            };
            let mut vars = Vec::with_capacity(defs.len());
            for (name, _) in defs.iter() {
                let var = make_variable(name.symbol().unwrap().clone());

                env.bindings.insert(
                    name.symbol().unwrap().clone(),
                    Definition::Variable(var.clone()),
                );
                vars.push(var);
            }

            let mut cenv = Cenv {
                env: Rc::new(Environment::Lexical(env)),
                ..cenv.clone()
            };

            let mut nodes = Vec::new();

            for (name, expr) in defs.iter() {
                cenv.expr_name = Some(name.symbol().unwrap().clone());
                nodes.push(expand(expr, &cenv)?);
                cenv.expr_name = None;
            }

            let bindings = vars
                .iter()
                .zip(nodes.iter())
                .map(|(x, y)| (x.clone(), y.clone()))
                .collect();

            let mut body = Vec::new();

            while let Some((expr, rest)) = exprs.pair() {
                body.push(expand(expr, &cenv)?);
                exprs = rest;
            }

            return Ok(make_rec_star(bindings, make_seq(body)));
        }
    }

    rec(exprs, cenv, &mut Vec::new())
}
