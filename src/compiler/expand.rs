use crate::{
    compiler::syntaxenv::{
        DENOTATION_OF_BEGIN, DENOTATION_OF_DEFINE, DENOTATION_OF_LAMBDA, LAMBDA, SET,
    },
    value::Value,
    vm::{intern, parameterize},
};
use std::cell::Cell;

use super::syntaxenv::{
    global_syntactic_environment, identifier_name, identifier_r_entry, is_identifier_denotation,
    m_strip, rename_formals, rename_vars, syntactic_lookup, syntactic_rename, DENOTATION_OF_APP,
    DENOTATION_OF_DATUM, DENOTATION_OF_IF, DENOTATION_OF_QUOTE, DENOTATION_OF_SET,
    DENOTATION_OF_TOP, GLOBAL_SYNTACTIC_ENVIRONMENT,
};
use super::*;

pub struct Pass1Env {
    pub block_compiling: bool,
    pub block_assignments: Value,
}

pub fn define_syntax_scope(args: &[Value]) -> Value {
    thread_local! {
        static FLAG: Cell<Value> = Cell::new(intern("letrec"));
    }

    fn set_flag(flag: Value) {
        FLAG.with(|f| f.set(flag));
    }

    fn get_flag() -> Value {
        FLAG.with(|f| f.get())
    }

    if args.is_empty() {
        return get_flag();
    } else if args.len() > 1 {
        eprintln!(
            "too many arguments passed to define-syntax-scope: {:?}",
            args
        );
    }

    let l = args[0];
    if l == intern("letrec") || l == intern("letrec*") || l == intern("let*") {
        set_flag(l)
    } else {
        eprintln!("invalid argument passed to define-syntax-scope: {:?}", l);
    }

    get_flag()
}

pub fn make_toplevel_definition(id: Value, exp: Value) -> Result<Value, Value> {
    Ok(make_begin(&[
        make_assignment(id, exp),
        make_constant(id), // TODO: unmangle id
    ]))
}

pub fn expand(def_or_exp: Value, syntaxenv: Value, p1: &mut Pass1Env) -> Result<Value, Value> {
    parameterize(&[(*GLOBAL_SYNTACTIC_ENVIRONMENT, syntaxenv)], || {
        let expanded = desugar_definitions(
            def_or_exp,
            global_syntactic_environment(),
            p1,
            &mut Box::new(make_toplevel_definition),
        )?;
        Ok(make_call(
            make_lambda(
                Value::make_null(),
                Value::make_null(),
                Value::make_null(),
                Value::make_null(),
                Value::make_null(),
                Value::make_null(),
                Value::make_null(),
                expanded,
            ),
            Value::make_null(),
        ))
    })
}

pub fn m_expand(exp: Value, env: Value, p1: &mut Pass1Env) -> Result<Value, Value> {
    if !exp.pairp() {
        m_atom(exp, env)
    } else if !exp.car().symbolp() {
        m_application(exp, env, p1)
    } else {
        let keyword = syntactic_lookup(env, exp.car());

        let cls = keyword.car(); // denotation class

        if cls == intern("special") {
            if keyword == *DENOTATION_OF_APP {
                return m_application(exp.cdr(), env, p1);
            } else if keyword == *DENOTATION_OF_TOP {
                return m_atom(exp.cdr(), global_syntactic_environment());
            } else if keyword == *DENOTATION_OF_DATUM {
                return Ok(m_datum(exp));
            } else if keyword == *DENOTATION_OF_QUOTE {
                return m_quote(exp);
            } else if keyword == *DENOTATION_OF_IF {
                return m_if(exp, env, p1);
            } else if keyword == *DENOTATION_OF_SET {
                return m_set(exp, env, p1);
            } else if keyword == *DENOTATION_OF_BEGIN {
                return m_begin(exp, env, p1);
            } else if keyword == *DENOTATION_OF_LAMBDA {
                return m_lambda(exp, env, p1);
            } else if keyword == *DENOTATION_OF_DEFINE {
                return error("define not allowed in this context");
            } else {
                return error(&format!("[internal] unknown special form: {}", keyword));
            }
        } else {
            return m_application(exp, env, p1);
        }
    }
}

pub fn m_lambda(exp: Value, env: Value, p1: &mut Pass1Env) -> Result<Value, Value> {
    if let Some(_) = exp.proper_list_length().filter(|x| *x > 2) {
        let formals = exp.cadr();
        let alist = rename_vars(formals)?;
        let env = syntactic_rename(env, alist);
        let body = exp.cddr();

        let mut alist2 = alist;

        while alist2.is_true() {
            if alist2.nullp() {
                break;
            }

            if Value::assq(alist2.caar(), alist.cdr()).is_true() {
                return error(&format!("malformed formal parameter list: {}", formals));
            }
            alist2 = alist2.cdr();
        }
        
        let body = m_body(body, env, p1)?;
        println!("body: {}", body);
        let formals = rename_formals(formals, alist);
        println!("formals: {}", formals);
        Ok(make_lambda(
            formals,
            Value::make_null(), // no defs yet
            Value::list_map(Thread::current(), alist, |entry| {
                let x = syntactic_lookup(env, entry.cdr()).cdr();
                println!("x: {}", x);
                x 
            }),
            Value::make_null(),
            Value::make_null(),
            Value::make_null(),
            Value::make_null(),
            body,
        ))
    } else {
        error(&format!("malformed lambda expression: {}", exp))
    }
}

pub fn m_body(exp: Value, env: Value, p1: &mut Pass1Env) -> Result<Value, Value> {
    fn rec(body: Value, env: Value, defs: Value, p1: &mut Pass1Env) -> Result<Value, Value> {
        if body.nullp() {
            return error("empty body");
        }

        let exp = body.car();

        if exp.pairp() && exp.car().symbolp() {
            let denotation = syntactic_lookup(env, exp.car());
            let cls = denotation.car();

            if cls == intern("special") {
                if denotation == *DENOTATION_OF_BEGIN {
                    return rec(
                        Value::list_append(Thread::current(), exp.cdr(), body.cdr()),
                        env,
                        defs,
                        p1,
                    );
                } else if denotation == *DENOTATION_OF_DEFINE {
                    return rec(body.cdr(), env, Value::make_cons(exp, defs), p1);
                } else {
                    return finalize_body(body, env, defs, p1);
                }
            } else {
                return finalize_body(body, env, defs, p1);
            }
        } else {
            return finalize_body(body, env, defs, p1);
        }
    }

    rec(exp, env, Value::make_null(), p1)
}

pub fn finalize_body(
    body: Value,
    env: Value,
    defs: Value,
    p1: &mut Pass1Env,
) -> Result<Value, Value> {
    if defs.nullp() {
        let body = Value::try_list_map(Thread::current(), body, |exp| m_expand(exp, env, p1))?;
        if body.cdr().nullp() {
            Ok(body.car())
        } else {
            Ok(make_begin2(body))
        }
    } else {
        // implements letrec* semantics for internal definitions
        // FIXME: does not enforce letrec* restriction
        // FIXME: could do better if we knew which variables are never assigned

        fn sort_defs(defs: Value) -> Value {
            fn rec(defs: Value, procs: Value, trivs: Value, others: Value) -> Value {
                if defs.nullp() {
                    return Value::list_append_var(Thread::current(), &[procs, trivs, others]);
                } else {
                    let def = defs.car();
                    let defs = defs.cdr();

                    let rhs = def.cadr();

                    if !rhs.pairp() {
                        if rhs.symbolp() {
                            return rec(defs, procs, trivs, Value::make_cons(def, others));
                        } else {
                            return rec(defs, procs, Value::make_cons(def, trivs), others);
                        }
                    } else {
                        let denotation =
                            syntactic_lookup(global_syntactic_environment(), rhs.car());

                        if denotation == *DENOTATION_OF_LAMBDA {
                            return rec(defs, Value::make_cons(def, procs), trivs, others);
                        } else if denotation == *DENOTATION_OF_QUOTE {
                            return rec(defs, procs, Value::make_cons(def, trivs), others);
                        } else {
                            return rec(defs, procs, trivs, Value::make_cons(def, others));
                        }
                    }
                }
            }

            rec(
                defs,
                Value::make_null(),
                Value::make_null(),
                Value::make_null(),
            )
        }

        fn desugar_definition(def: Value) -> Result<Value, Value> {
            if let Some(l) = def.proper_list_length().filter(|x| *x > 2) {
                if def.cadr().pairp() {
                    desugar_definition(Value::make_list(
                        Thread::current(),
                        &[def.car(), def.cadr().car(), {
                            let ls =
                                Value::make_list(Thread::current(), &[*LAMBDA, def.cadr().cdr()]);
                            // append body:
                            let ls = Value::list_append(Thread::current(), ls, def.cddr());
                            ls
                        }],
                    ))
                } else {
                    if l == 3 && def.cadr().symbolp() {
                        Ok(def.cdr())
                    } else {
                        error(&format!("invalid definition: {}", def))
                    }
                }
            } else {
                error(&format!("invalid definition: {}", def))
            }
        }

        fn expand_letrec_star(
            bindings: Value,
            body: Value,
            env: Value,
            p1: &mut Pass1Env,
        ) -> Result<Value, Value> {
            /*
            build the following lambda:
            `(,lambda0 ,(map car bindings)
                       ,@(map (lambda (binding)
                                `(,set!0 ,(car binding)
                                         ,(cadr binding)))
                              bindings)
                         ,@body)
                         */
            let lambda = Value::make_list(
                Thread::current(),
                &[
                    *LAMBDA,
                    Value::list_map(Thread::current(), bindings, |binding| binding.car()),
                ],
            );
            let mut lambda = Value::list_append(
                Thread::current(),
                lambda,
                Value::list_map(Thread::current(), bindings, |binding| {
                    Value::make_list(Thread::current(), &[*SET, binding.car(), binding.cadr()])
                }),
            );
            lambda = Value::list_append(Thread::current(), lambda, body);
            println!("lambda: {}", m_expand(lambda, env, p1)?);
            Ok(make_call(
                m_expand(lambda, env, p1)?,
                Value::list_map(Thread::current(), bindings, |_| {
                    make_constant(Value::make_void())
                }),
            ))
        }

        expand_letrec_star(
            sort_defs(Value::try_list_map(
                Thread::current(),
                Value::list_reverse(Thread::current(), defs),
                desugar_definition,
            )?),
            body,
            env,
            p1,
        )
    }
}

pub fn m_if(exp: Value, env: Value, p1: &mut Pass1Env) -> Result<Value, Value> {
    if let Some(_) = exp.proper_list_length().filter(|x| *x == 3 || *x == 4) {
        let test = m_expand(exp.cadr(), env, p1)?;
        let conseq = m_expand(exp.caddr(), env, p1)?;
        let alt = if exp.cdddr().pairp() {
            m_expand(exp.cadddr(), env, p1)?
        } else {
            make_constant(Value::make_void())
        };

        Ok(make_conditional(test, conseq, alt))
    } else {
        error(&format!("invalid if: {}", exp))
    }
}

pub fn m_set(exp: Value, env: Value, p1: &mut Pass1Env) -> Result<Value, Value> {
    if let Some(_) = exp.proper_list_length().filter(|x| *x == 3) {
        let lhs = m_expand(exp.cadr(), env, p1)?;
        let rhs = m_expand(exp.caddr(), env, p1)?;

        if is_variable(lhs) {
            let x = variable_name(lhs);
            let assignment = make_assignment(x, rhs);
            let denotation = syntactic_lookup(env, x);

            if is_identifier_denotation(denotation) {
                let r_entry = identifier_r_entry(denotation);
                r_entry_refs_set(r_entry, remq(lhs, r_entry_refs(r_entry)));
                r_entry_assigns_set(
                    r_entry,
                    Value::make_cons(assignment, r_entry_assigns(r_entry)),
                );
            }

            if p1.block_compiling {
                p1.block_assignments = Value::make_cons(x, p1.block_assignments);
            }
            Ok(assignment)
        } else {
            error(&format!("invalid set!: {}", exp))
        }
    } else {
        error(&format!("invalid set!: {}", exp))
    }
}

pub fn m_begin(exp: Value, env: Value, p1: &mut Pass1Env) -> Result<Value, Value> {
    if let Some(x) = exp.proper_list_length().filter(|x| *x >= 1) {
        Ok(if x > 1 {
            make_begin2(Value::try_list_map(Thread::current(), exp.cdr(), |exp| {
                m_expand(exp, env, p1)
            })?)
        } else {
            make_constant(Value::make_void())
        })
    } else {
        error(&format!("invalid begin: {}", exp))
    }
}

pub fn m_application(exp: Value, env: Value, p1: &mut Pass1Env) -> Result<Value, Value> {
    if let Some(l) = exp.proper_list_length() {
        if l > 0 {
            let proc = m_expand(exp.car(), env, p1)?;
            let mut args = exp.cdr();
            let mut newargs = Value::make_null();

            while args.pairp() {
                newargs = Value::make_cons(m_expand(args.car(), env, p1)?, newargs);
                args = args.cdr();
            }

            let call = make_call(proc, Value::list_reverse(Thread::current(), newargs));
            if is_variable(proc) {
                let procname = variable_name(proc);
                let denotation = syntactic_lookup(env, procname);
                if is_identifier_denotation(denotation) {
                    let r_entry = identifier_r_entry(denotation);
                    r_entry_calls_set(r_entry, Value::make_cons(call, r_entry_calls(r_entry)));
                }
            }
            return Ok(call);
        }
    }

    error(&format!("invalid application: {}", exp))
}

pub fn m_atom(exp: Value, env: Value) -> Result<Value, Value> {
    if !exp.symbolp() {
        Ok(make_constant(exp))
    } else {
        let denotation = syntactic_lookup(env, exp);

        let denotation_class = denotation.car();

        if denotation_class == intern("special") || denotation_class == intern("macro") {
            eprintln!("Syntactic keyword used as a variable: {}", exp);
            Ok(make_constant(Value::make_true()))
        } else if denotation_class == intern("identifier") {
            let var = make_variable(identifier_name(denotation));
            let r_entry = identifier_r_entry(denotation);
            r_entry_refs_set(r_entry, Value::make_cons(var, r_entry_refs(r_entry)));
            Ok(var)
        } else {
            panic!("unknown denotation class: {}", denotation_class)
        }
    }
}

pub fn m_datum(exp: Value) -> Value {
    make_constant(m_strip(exp.cdr()))
}

pub fn m_quote(exp: Value) -> Result<Value, Value> {
    if exp.cdr().pairp() && exp.cddr().nullp() {
        Ok(make_constant(m_strip(exp.cadr())))
    } else {
        error(&format!("invalid quote: {}", exp))
    }
}
