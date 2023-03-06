//! Scheme compiler
//!
//!
//! Compiles Scheme code into Cranelift IR. Code is compiled in several phases:
//!
//! # Pass 1: Desugaring
//!
//! - top-level `begin`s are flattened
//! - `define`s are converted to `set!`s
//! - internal definitions are converted to `letrec`.
//!
//! # Pass 2: Assignment conversion
//!
//! This pass converts assigned variables into boxes, set! into set-box! and references
//! to the assigned variables into unbox.
//!
//! # Pass 3: Closure conversion
//!
//! This pass collects bound and free-variables in a lambda. Then it updates
//! the lambda to include environment information. Finally, it converts the lambda
//! into a closure.
//!
//! # Pass 4: Lambda lifting
//!
//! All lambdas are lifted to the top-level. Each lambda is given a unique name.
//!
//! # Pass 5:
//!
//! Code is compiled to Cranelift IR.

use crate::value::Value;
use crate::vm::intern;
use crate::{raise_exn, value::SCHEME_MAX_ARGS};
use once_cell::sync::Lazy;
use r7rs_parser::expr::Interner;
use rsgc::thread::Thread;
use std::sync::atomic::{AtomicUsize, Ordering};
pub mod env;

static BEGIN: Lazy<Value> = Lazy::new(|| intern("begin"));
//static DEFINE: Lazy<Value> = Lazy::new(|| intern("define"));

pub fn error<T>(msg: &str) -> Result<T, Value> {
    raise_exn!(Fail, &[], "compiler error: {}", msg)
}
use r7rs_parser::expr::Expr;

pub fn expr_to_value<I: Interner>(i: &I, e: &r7rs_parser::expr::Expr<I>) -> Value {
    match e {
        Expr::Fixnum(n) => Value::make_int(*n),
        Expr::Pair(car, cdr) => {
            let car = expr_to_value(i, car);
            let cdr = expr_to_value(i, cdr);

            Value::make_cons(car, cdr)
        }

        Expr::Symbol(s) => intern(i.description(s)),
        Expr::Null => Value::make_null(),
        Expr::Bool(b) => {
            if *b {
                Value::make_true()
            } else {
                Value::make_false()
            }
        }

        Expr::ByteVector(b) => {
            let bv = Value::make_byte_vector(Thread::current(), b.len() as _, 0);

            for i in 0..b.len() {
                bv.byte_vector_set(i, b[i]);
            }

            bv
        }

        Expr::GrowableVector(v) | Expr::ImmutableVector(v) => {
            let vec = Value::make_vector(Thread::current(), v.len() as _, Value::make_void());

            for j in 0..v.len() {
                vec.vector_set(j, expr_to_value(i, &v[j]));
            }

            vec
        }
        Expr::Float(x) => Value::make_double(Thread::current(), *x),
        Expr::Syntax(_, e) => expr_to_value(i, e),
        Expr::Str(x) => Value::make_str(Thread::current(), x),
        _ => todo!(),
    }
}

pub fn is_constant(exp: Value) -> bool {
    exp.car() == intern("quote")
}

pub fn is_variable(exp: Value) -> bool {
    exp.car() == intern("begin") && exp.cddr().nullp()
}

pub fn is_lambda(exp: Value) -> bool {
    exp.car() == intern("lambda")
}

pub fn is_call(exp: Value) -> bool {
    exp.car().pairp()
}

pub fn is_assignment(exp: Value) -> bool {
    exp.car() == intern("set!")
}

pub fn is_conditional(exp: Value) -> bool {
    exp.car() == intern("if")
}

pub fn is_begin(exp: Value) -> bool {
    exp.car() == intern("begin") && !exp.cddr().nullp()
}

pub fn make_constant(exp: Value) -> Value {
    Value::make_cons(intern("quote"), Value::make_cons(exp, Value::make_null()))
    //Value::make_list(Thread::current(), &[intern("quote"), exp])
}

pub fn make_variable(exp: Value) -> Value {
    Value::make_list(Thread::current(), &[intern("begin"), exp])
}

pub fn make_lambda(
    formals: Value,
    defs: Value,
    r: Value,
    f: Value,
    g: Value,
    decls: Value,
    doc: Value,
    body: Value,
) -> Value {
    Value::make_list(
        Thread::current(),
        &[
            intern("lambda"),
            formals,
            Value::make_cons(intern("begin"), defs),
            Value::make_list(
                Thread::current(),
                &[
                    intern("quote"),
                    Value::make_list(Thread::current(), &[r, f, g, decls, doc]),
                ],
            ),
            body,
        ],
    )
}

pub fn make_call(exp: Value, args: Value) -> Value {
    Value::make_cons(intern("#%app"), Value::make_cons(exp, args))
}

pub fn make_conditional(test: Value, conseq: Value, alt: Value) -> Value {
    Value::make_list(Thread::current(), &[intern("if"), test, conseq, alt])
}

pub fn make_definition(name: Value, value: Value) -> Value {
    Value::make_list(Thread::current(), &[intern("define"), name, value])
}

pub fn constant_value(exp: Value) -> Value {
    exp.cadr()
}

pub fn variable_name(exp: Value) -> Value {
    exp.cadr()
}

pub fn lambda_args(exp: Value) -> Value {
    exp.cadr()
}

pub fn lambda_body(exp: Value) -> Value {
    exp.cddr()
}
pub fn call_proc(exp: Value) -> Value {
    exp.car()
}

pub fn call_args(exp: Value) -> Value {
    exp.cdr()
}

pub fn assignment_lhs(exp: Value) -> Value {
    exp.cadr()
}

pub fn assignment_rhs(exp: Value) -> Value {
    exp.caddr()
}

pub fn assignment_set(exp: Value, newexp: Value) {
    exp.set_pair_car(newexp.car());
    exp.set_pair_cdr(newexp.cdr());
}

pub fn if_test(exp: Value) -> Value {
    exp.cadr()
}

pub fn if_then(exp: Value) -> Value {
    exp.caddr()
}

pub fn if_else(exp: Value) -> Value {
    exp.cadddr()
}

pub fn begin_exprs(exp: Value) -> Value {
    exp.cdr()
}

pub fn def_lhs(exp: Value) -> Value {
    exp.cadr()
}

pub fn def_rhs(exp: Value) -> Value {
    exp.caddr()
}

pub fn variable_set(exp: Value, newexp: Value) {
    exp.set_pair_car(newexp.car());
    exp.set_pair_cdr(Value::list_append(
        Thread::current(),
        newexp.cdr(),
        Value::make_null(),
    ));
}

pub fn lambda_args_set(exp: Value, args: Value) {
    exp.cdr().set_pair_car(args);
}

pub fn lambda_defs_set(exp: Value, defs: Value) {
    exp.caddr().set_pair_cdr(defs);
}

pub fn lambda_r_set(exp: Value, r: Value) {
    exp.cadddr().cadr().set_pair_car(r);
}

pub fn lambda_f_set(exp: Value, f: Value) {
    exp.cadddr().cadr().cdr().set_pair_car(f);
}

pub fn lambda_g_set(exp: Value, g: Value) {
    exp.cadddr().cadr().caddr().set_pair_car(g);
}

pub fn lambda_decls_set(exp: Value, decls: Value) {
    exp.cadddr().cadr().cdddr().set_pair_car(decls);
}

pub fn lambda_body_set(exp: Value, body: Value) {
    exp.cdddr().cdr().set_pair_car(body);
}

pub fn call_proc_set(exp: Value, proc: Value) {
    exp.set_pair_car(proc);
}

pub fn call_args_set(exp: Value, args: Value) {
    exp.set_pair_cdr(args);
}

pub fn assignment_rhs_set(exp: Value, rhs: Value) {
    exp.cddr().set_pair_car(rhs);
}

pub fn if_test_set(exp: Value, test: Value) {
    exp.cdr().set_pair_car(test);
}

pub fn if_then_set(exp: Value, then: Value) {
    exp.cddr().set_pair_car(then);
}

pub fn if_else_set(exp: Value, else_: Value) {
    exp.cdddr().set_pair_car(else_);
}

pub fn make_begin(exprs: &[Value]) -> Value {
    if exprs.len() == 1 {
        exprs[0]
    } else {
        let list = Value::make_list(Thread::current(), exprs);
        Value::make_cons(*BEGIN, list)
    }
}

pub fn make_begin2(exprs: Value) -> Value {
    if exprs.nullp() {
        Value::make_null()
    } else if exprs.cdr().nullp() {
        exprs.car()
    } else {
        Value::make_cons(*BEGIN, exprs)
    }
}

pub fn make_assignment(name: Value, value: Value) -> Value {
    Value::make_cons(
        intern("set!"),
        Value::make_cons(name, Value::make_cons(value, Value::make_null())),
    )
}

pub fn remq(x: Value, y: Value) -> Value {
    if y.nullp() {
        Value::make_null()
    } else if x == y.car() {
        remq(x, y.cdr())
    } else {
        Value::make_cons(y.car(), remq(x, y.cdr()))
    }
}

/* Procedures for fetching referencing information from R-tables. */

pub fn make_r_entry(name: Value, refs: Value, assign: Value, calls: Value) -> Value {
    Value::make_list(Thread::current(), &[name, refs, assign, calls])
}

pub fn r_entry_name(entry: Value) -> Value {
    entry.car()
}

pub fn r_entry_refs(entry: Value) -> Value {
    entry.cadr()
}

pub fn r_entry_assigns(entry: Value) -> Value {
    entry.caddr()
}

pub fn r_entry_calls(entry: Value) -> Value {
    entry.cadddr()
}

pub fn r_entry_refs_set(entry: Value, refs: Value) {
    entry.cdr().set_pair_car(refs);
}

pub fn r_entry_assigns_set(entry: Value, assigns: Value) {
    entry.cddr().set_pair_car(assigns);
}

pub fn r_entry_calls_set(entry: Value, calls: Value) {
    entry.cdddr().set_pair_car(calls);
}

mod pass1 {
    use std::collections::HashSet;

    use super::*;
    pub fn desugar_definitions(exp: Value, defs: &mut Value) -> Result<Value, Value> {
        fn redefinition(id: Value, defs: Value) -> Result<(), Value> {
            if id.symbolp() {
                if Value::memq(id, defs).is_true() {
                    eprintln!("redefinition of {}", id);
                }

                Ok(())
            } else {
                error(&format!("malformed definition: {}", id))
            }
        }

        fn desugar_define(exp: Value, defs: &mut Value) -> Result<Value, Value> {
            if exp.cdr().nullp() {
                return error(&format!("malformed definition: {}", exp));
            } else if exp.cddr().nullp() {
                redefinition(exp.cadr(), *defs)?;
                *defs = Value::make_cons(exp.cadr(), *defs);
                Ok(make_begin(&[
                    make_assignment(exp.cadr(), make_constant(Value::make_void())),
                    make_constant(exp.cadr()),
                ]))
            } else if exp.cadr().pairp() {
                let def = exp.car();
                let pattern = exp.cadr();
                let f = pattern.car();
                let args = pattern.cdr();
                let body = exp.cddr();
                let lam = Value::make_cons(intern("lambda"), Value::make_cons(args, body));
                let new = Value::make_cons(
                    def,
                    Value::make_cons(f, Value::make_cons(lam, Value::make_null())),
                );

                desugar_define(new, defs)
            } else {
                redefinition(exp.cadr(), *defs)?;
                *defs = Value::make_cons(exp.cadr(), *defs);
                Ok(make_begin(&[
                    make_assignment(exp.cadr(), pass1(exp.caddr())?),
                    make_constant(exp.cadr()),
                ]))
            }
        }

        fn define_loop(
            exp: Value,
            rest: Value,
            first: Value,
            defs: &mut Value,
        ) -> Result<Value, Value> {
            if exp.pairp()
                && exp.car().symbolp()
                && exp.car() == intern("begin")
                && exp.cdr().pairp()
            {
                return define_loop(
                    exp.cadr(),
                    Value::list_append(Thread::current(), exp.cddr(), rest),
                    first,
                    defs,
                );
            } else if exp.pairp() && exp.car().symbolp() && exp.car() == intern("define") {
                let exp = desugar_define(exp, defs)?;

                if first.nullp() && rest.nullp() {
                    return Ok(exp);
                } else if rest.nullp() {
                    return Ok(make_begin2(Value::list_reverse(
                        Thread::current(),
                        Value::make_cons(exp, first),
                    )));
                } else {
                    return define_loop(rest.car(), rest.cdr(), Value::make_cons(exp, first), defs);
                }
            } else if rest.nullp() && first.nullp() {
                return pass1(exp);
            } else if rest.nullp() {
                return Ok(make_begin2(Value::list_reverse(
                    Thread::current(),
                    Value::make_cons(pass1(exp)?, first),
                )));
            } else {
                return define_loop(
                    rest.car(),
                    rest.cdr(),
                    Value::make_cons(pass1(exp)?, first),
                    defs,
                );
            }
        }

        define_loop(exp, Value::make_null(), Value::make_null(), defs)
    }

    /// Pass 1:
    ///
    /// 1. Lift defines to top level
    /// 2. Get rid of all internal definitions
    /// 3. Flatten begins
    pub fn pass1(exp: Value) -> Result<Value, Value> {
        if !exp.pairp() {
            if !exp.symbolp() {
                return Ok(make_constant(exp));
            } else {
                return Ok(exp);
            }
        } else if !exp.car().symbolp() {
            return pass1_application(exp);
        } else {
            let kw = exp.car().strsym();

            if kw == "#%app" {
                return pass1_application(exp.cdr());
            } else if kw == "quote" {
                return Ok(make_constant(exp.cadr()));
            } else if kw == "if" {
                return pass1_if(exp);
            } else if kw == "set!" {
                return pass1_set(exp);
            } else if kw == "begin" {
                return pass1_begin(exp);
            } else if kw == "lambda" {
                return pass1_lambda(exp);
            } else if kw == "let" {
                return pass1_let(exp);
            } else if kw == "define" {
                return error(&format!("define not allowed in this context: {}", exp));
            } else {
                return pass1_application(exp);
            }
        }
    }

    fn pass1_lambda(exp: Value) -> Result<Value, Value> {
        if let Some(_) = exp.proper_list_length().filter(|x| *x > 2) {
            let formals = exp.cadr();
            let body = exp.cddr();

            let mut redefine = HashSet::with_capacity(formals.list_length());

            fn rec(orig: Value, alist: Value, redefine: &mut HashSet<Value>) -> Result<(), Value> {
                if alist.nullp() {
                    return Ok(());
                } else if alist.pairp() {
                    rec(orig, alist.car(), redefine)?;
                    rec(orig, alist.cdr(), redefine)
                } else if alist.symbolp() {
                    if redefine.contains(&alist) {
                        return error(&format!("redefinition of formal: {}", orig));
                    } else {
                        redefine.insert(alist);
                        Ok(())
                    }
                } else {
                    error(&format!("malformed lambda: {}", orig))
                }
            }

            rec(formals, formals, &mut redefine)?;

            let body = pass1_body(body)?;

            Ok(Value::make_cons(
                intern("lambda"),
                Value::make_cons(formals, Value::make_cons(body, Value::make_null())),
            ))
        } else {
            error(&format!("malformed lambda: {}", exp))
        }
    }

    fn pass1_if(exp: Value) -> Result<Value, Value> {
        if let Some(l) = exp.proper_list_length() {
            if l == 3 {
                let test = pass1(exp.cadr())?;
                let conseq = pass1(exp.caddr())?;
                let alt = make_constant(Value::make_void());

                Ok(make_conditional(test, conseq, alt))
            } else if l == 4 {
                let test = pass1(exp.cadr())?;
                let conseq = pass1(exp.caddr())?;
                let alt = pass1(exp.cadddr())?;

                Ok(make_conditional(test, conseq, alt))
            } else {
                error(&format!("malformed if: {}", exp))
            }
        } else {
            error(&format!("malformed if: {}", exp))
        }
    }

    fn pass1_set(exp: Value) -> Result<Value, Value> {
        if let Some(l) = exp.proper_list_length() {
            if l == 3 {
                let id = exp.cadr();
                let val = pass1(exp.caddr())?;

                Ok(make_assignment(id, val))
            } else {
                error(&format!("malformed set!: {}", exp))
            }
        } else {
            error(&format!("malformed set!: {}", exp))
        }
    }

    fn pass1_begin(exp: Value) -> Result<Value, Value> {
        if let Some(x) = exp.proper_list_length().filter(|x| *x >= 1) {
            Ok(if x > 1 {
                make_begin2(Value::try_list_map(Thread::current(), exp, |x| pass1(x))?)
            } else {
                make_constant(Value::make_void())
            })
        } else {
            error(&format!("malformed begin: {}", exp))
        }
    }

    fn pass1_application(exp: Value) -> Result<Value, Value> {
        if let Some(_) = exp.proper_list_length() {
            let proc = pass1(exp.car())?;
            let mut args = exp.cdr();
            let mut newargs = Value::make_null();

            while args.pairp() {
                newargs = Value::make_cons(pass1(args.car())?, newargs);
                args = args.cdr();
            }

            let call = make_call(proc, Value::list_reverse(Thread::current(), newargs));

            Ok(call)
        } else {
            error(&format!("malformed application: {}", exp))
        }
    }

    fn get_form(name: &str, exp: Value) -> Result<(Value, Value), Value> {
        if exp.pairp() {
            let bindings = exp.car();
            let body = exp.cdr();

            if !bindings.is_list() {
                error(&format!("malformed bindings in {}: {}", name, exp))
            } else {
                let mut ls = bindings;

                while ls.pairp() {
                    if !ls.car().pairp() {
                        return error(&format!("malformed binding in {}: {}", name, exp));
                    }

                    let name = ls.car().car();
                    if !name.symbolp() {
                        return error(&format!("malformed binding in {}: {}", name, exp));
                    }

                    if !ls.car().cdr().pairp() {
                        return error(&format!("malformed binding in {}: {}", name, exp));
                    }

                    ls = ls.cdr();
                }

                Ok((bindings, body))
            }
        } else {
            error(&format!("malformed {}: {}", name, exp))
        }
    }

    fn pass1_let(exp: Value) -> Result<Value, Value> {
        let (bindings, body) = get_form("let", exp.cdr())?;

        /* check for duplicates */
        let mut alist = bindings;
        while alist.is_true() {
            if alist.nullp() {
                break;
            }

            if Value::assq(alist.caar(), alist.cdr()).is_true() {
                return error(&format!("duplicate binding: {}", alist.caar()));
            }

            alist = alist.cdr()
        }

        /* convert to lambda: */
        let formals = Value::list_map(Thread::current(), bindings, |x| x.car());

        let lambda = Value::make_cons(intern("lambda"), Value::make_cons(formals, body));

        Ok(make_call(
            pass1(lambda)?,
            Value::try_list_map(Thread::current(), bindings, |x| pass1(x.cadr()))?,
        ))
    }

    pub fn pass1_body(form: Value) -> Result<Value, Value> {
        fn rec(body: Value, defs: Value) -> Result<Value, Value> {
            if body.nullp() {
                return error("empty body");
            }

            let exp = body.car();

            if exp.pairp() && exp.car() == intern("begin") {
                return rec(
                    Value::list_append(Thread::current(), exp.cdr(), body.cdr()),
                    defs,
                );
            } else if exp.pairp() && exp.car() == intern("define") {
                return rec(body.cdr(), Value::make_cons(exp, defs));
            } else {
                return pass1_finalize_body(body, defs);
            }
        }

        rec(form, Value::make_null())
    }

    pub fn pass1_finalize_body(body: Value, defs: Value) -> Result<Value, Value> {
        if defs.nullp() {
            let body = Value::try_list_map(Thread::current(), body, |x| pass1(x))?;

            if body.cdr().nullp() {
                Ok(body.car())
            } else {
                Ok(make_begin2(body))
            }
        } else {
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
                            let denotation = rhs.car();
                            if denotation == intern("lambda") {
                                return rec(defs, Value::make_cons(def, procs), trivs, others);
                            } else if denotation == intern("quote") {
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
                                let ls = Value::make_list(
                                    Thread::current(),
                                    &[intern("lambda"), def.cadr().cdr()],
                                );
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

            fn expand_letrec_star(bindings: Value, body: Value) -> Result<Value, Value> {
                let mut lambda = Value::make_list(
                    Thread::current(),
                    &[
                        intern("lambda"),
                        Value::list_map(Thread::current(), bindings, |x| x.car()),
                    ],
                );

                lambda = Value::list_append(
                    Thread::current(),
                    lambda,
                    Value::list_map(Thread::current(), bindings, |binding| {
                        Value::make_list(
                            Thread::current(),
                            &[intern("set!"), binding.car(), binding.cadr()],
                        )
                    }),
                );

                lambda = Value::list_append(Thread::current(), lambda, body);

                Ok(make_call(
                    pass1(lambda)?,
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
            )
        }
    }
}

pub fn make_bound(sym: Value) -> Value {
    Value::make_cons(intern("bound"), sym)
}

pub fn make_free(sym: Value) -> Value {
    Value::make_cons(intern("free"), sym)
}

pub fn set_cons(x: Value, set: Value) -> Value {
    if set.nullp() {
        Value::make_list(Thread::current(), &[x])
    } else if x == set.car() {
        set
    } else {
        Value::make_cons(set.car(), set_cons(x, set.cdr()))
    }
}

pub fn is_set(ls: Value) -> bool {
    if ls.nullp() {
        true
    } else {
        Value::memq(ls.car(), ls.cdr()).falsep() && is_set(ls.cdr())
    }
}

static GEN: AtomicUsize = AtomicUsize::new(0);

pub fn unique_name() -> Value {
    intern(&format!("|.var{}|", GEN.fetch_add(1, Ordering::SeqCst)))
}

pub fn kunique_name() -> Value {
    static GEN1: AtomicUsize = AtomicUsize::new(0);
    intern(&format!("|%k{}", GEN1.fetch_add(1, Ordering::SeqCst)))
}

pub fn runique_name() -> Value {
    static GEN2: AtomicUsize = AtomicUsize::new(0);
    intern(&format!("|%rv{}", GEN2.fetch_add(1, Ordering::SeqCst)))
}

pub mod cps {
    use super::*;

    fn m(aexp: Value) -> Value {
        if aexp.pairp() {
            match aexp.car().strsym() {
                "quote" => aexp,
                "lambda" => {
                    let formals = aexp.cadr();
                    let body = aexp.cddr();

                    let k = kunique_name();

                    let formals = Value::make_cons(k, formals);

                    let body = t_k_star(body, &|aexp| make_call(k, aexp));

                    Value::make_list(Thread::current(), &[intern("lambda"), formals, body])
                }

                _ => unreachable!(),
            }
        } else {

            if aexp == intern("%call/cc") {
                let k = kunique_name();
                let r = runique_name();
                let r2 = kunique_name();
                let k2 = kunique_name();
                let formals = Value::make_list(Thread::current(), &[k, r]);


                let resume = make_call(k, Value::make_cons(r2, Value::make_null()));
                let resume_lambda = Value::make_list(Thread::current(), &[intern("lambda"), Value::make_list(Thread::current(), &[k2, r2]), resume]);

                let body = make_call(r, Value::make_list(Thread::current(), &[k, resume_lambda]));

                return Value::make_list(Thread::current(), &[intern("lambda"), formals, body]);
            }
            aexp
        }
    }

    pub fn t_k(exp: Value, k: &dyn Fn(Value) -> Value) -> Value {
        if exp.pairp() {
            match exp.car().strsym() {
                "lambda" => k(m(exp)),
                "quote" => k(m(exp)),
                "begin" => {
                    let expr = exp.cadr();
                    let exprs = exp.cddr();

                    if exprs.nullp() {
                        t_k(expr, k)
                    } else {
                        t_k(expr, &|_| t_k(Value::make_cons(intern("begin"), exprs), k))
                    }
                }

                "if" => {
                    let test = exp.cadr();
                    let then = exp.caddr();
                    let else_ = exp.cadddr();

                    let rv = runique_name();

                    let cont = Value::make_list(
                        Thread::current(),
                        &[
                            intern("lambda"),
                            Value::make_cons(rv, Value::make_null()),
                            k(rv),
                        ],
                    );

                    t_k(test, &|aexp| {
                        make_conditional(aexp, t_c(then, cont), t_c(else_, cont))
                    })
                }

                "set!" => {
                    let var = exp.cadr();
                    let val = exp.caddr();

                    t_k(val, &|aexp| {
                        /*Value::make_list(
                            Thread::current(),
                            &[
                                intern("set-then!"),
                                var,
                                aexp,
                                k(make_constant(Value::make_void())),
                            ],
                        )*/
                        let assignment = make_assignment(var, aexp);
                        let cont = k(make_constant(Value::make_void()));

                        make_begin(&[assignment, cont])
                    })
                }

                "#%app" => {
                    let rv = runique_name();
                    let cont = Value::make_list(
                        Thread::current(),
                        &[
                            intern("lambda"),
                            Value::make_cons(rv, Value::make_null()),
                            k(rv),
                        ],
                    );

                    t_c(exp, cont)
                }

                _ => unreachable!("invalid expression: {}", exp),
            }
        } else {
            k(m(exp))
        }
    }

    pub fn t_c(exp: Value, c: Value) -> Value {
        if exp.pairp() {
            match exp.car().strsym() {
                "quote" => make_call(c, Value::make_list(Thread::current(), &[m(exp)])),
                "begin" => {
                    let expr = exp.cadr();
                    let exprs = exp.cddr();

                    if exprs.nullp() {
                        t_c(expr, c)
                    } else {
                        t_k(expr, &|_| t_c(Value::make_cons(intern("begin"), exprs), c))
                    }
                }

                "if" => {
                    let test = exp.cadr();
                    let then = exp.caddr();
                    let else_ = exp.cadddr();

                    let k = kunique_name();

                    let lam = Value::make_list(
                        Thread::current(),
                        &[
                            intern("lambda"),
                            Value::make_cons(k, Value::make_null()),
                            t_k(test, &|aexp| {
                                make_conditional(aexp, t_c(then, k), t_c(else_, k))
                            }),
                        ],
                    );

                    make_call(lam, Value::make_cons(c, Value::make_null()))
                }

                "set!" => {
                    let var = exp.cadr();
                    let expr = exp.caddr();

                    t_k(expr, &|aexp| {
                        /*Value::make_list(
                            Thread::current(),
                            &[
                                intern("set-then!"),
                                var,
                                aexp,
                                make_call(c, Value::make_cons(make_constant(Value::make_void()), Value::make_null())),
                            ],
                        )*/

                        let call = make_call(
                            c,
                            Value::make_cons(make_constant(Value::make_void()), Value::make_null()),
                        );
                        let assignment = make_assignment(var, aexp);

                        make_begin(&[assignment, call])
                    })
                }

                "#%app" => {
                    let fun = exp.cadr();
                    let args = exp.cddr();

                    t_k(fun, &|f| {
                        t_k_star(args, &|aexps| make_call(f, Value::make_cons(c, aexps)))
                    })
                }

                "lambda" => m(exp),

                _ => unreachable!("unknwon expression: {}", exp),
            }
        } else {
            make_call(c, Value::make_cons(m(exp), Value::make_null()))
        }
    }

    fn t_k_star(exps: Value, k: &dyn Fn(Value) -> Value) -> Value {
        if exps.nullp() {
            k(Value::make_null())
        } else {
            t_k(exps.car(), &|hd| {
                t_k_star(exps.cdr(), &|tl| k(Value::make_cons(hd, tl)))
            })
        }
    }
}


pub mod redex {
    use crate::bool::equal;

    use super::*;

    /// Redex removal pass. It will iterate `n` times and remove redexes.
    pub fn redex_transform(e: Value, n: &mut usize) -> Value {
        *n -= 1;
        let new = redex_convert(e);
        if *n == 0 {
            new 
        } else {
            redex_transform(new, n)
        }
    }

    fn redex_convert(e: Value) -> Value {
        if e.pairp() {
            match e.car().strsym() {
                "#%app" => {
                    let proc = e.cadr();
                    let args = e.cddr();

                    let proc = redex_convert(proc);
                    let args = Value::list_map(Thread::current(), args, redex_convert);

                    make_call(proc, args)
                }
                "lambda" => {
                    redex_lambda(e)
                }

                "set!" => {
                    let var = redex_convert(e.cadr());
                    let val = redex_convert(e.caddr());

                    make_assignment(var, val)
                }

                "#%set-box!" => {
                    let var = redex_convert(e.cadr());
                    let val = redex_convert(e.caddr());

                    Value::make_list(
                        Thread::current(),
                        &[
                            intern("#%set-box!"),
                            var,
                            val,
                        ],
                    )
                }

                "if" => {
                    let test = redex_convert(e.cadr());
                    let then = redex_convert(e.caddr());
                    let else_ = redex_convert(e.cadddr());

                    make_conditional(test, then, else_)
                }

                "begin" => {
                    let exprs = Value::list_map(Thread::current(), e.cdr(), redex_convert);
                
                    make_begin2(exprs)
                }

                _ => e
            }
        } else {
            e
        }
    }


    /// 
    /// ```text
    /// (lambda (a1 a2) (foo a1 a2))
    /// ->
    /// (foo a1 a2)
    /// ```
    fn redex_lambda(e: Value) -> Value {
        let formals = e.cadr();
        let body = e.caddr();
        
        if body.is_list() 
            && body.pairp()
            && body.car().strsym() == "#%app"
            
        {
            println!("body: {}", e);
            // check if application arguments are equal to formals:
            if equal(formals, body.cddr()) {
                println!("reduce to: {}", body.cadr());
                body.cadr()
            } else {
                e
            }
        } else {
            e
        }
    }
}

pub mod uncover_assigned {
    //! This pass collects assigned variables and list them inside the binding
    //! constructs that bind them. It works bottom-up and passes on a list of
    //! assigned variables. It only list assigned variables when it is also bound
    //! by the construct.

    use super::*;

    pub fn uncover_assigned(x: Value) -> Value {
        let (x, _) = uncover(x);
        x
    }

    fn uncover_app(x: Value) -> (Value, Value) {
        let proc = x.cadr();
        let mut args = x.cddr();

        let (proc, mut assigned) = uncover(proc);
        let mut newargs = Value::make_null();

        while args.pairp() {
            let arg = args.car();
            let (arg, arg_assigned) = uncover(arg);
            newargs = Value::make_cons(arg, newargs);
            assigned = Value::list_union(Thread::current(), assigned, arg_assigned);
            args = args.cdr();
        }

        (
            make_call(proc, Value::list_reverse(Thread::current(), newargs)),
            assigned,
        )
    }

    fn uncover_lambda(x: Value) -> (Value, Value) {
        let vars = x.cadr();
        let body = x.cddr();

        let mut lsvars = Value::make_null();

        let mut ls = vars;

        while !ls.nullp() {
            if ls.symbolp() {
                lsvars = set_cons(ls, lsvars);
                break;
            }
            lsvars = set_cons(ls.car(), lsvars);
            ls = ls.cdr();
        }

        let (body, assigned) = uncover_body(body);
        let u = Value::list_intersection(Thread::current(), lsvars, assigned);

        (
            Value::make_cons(
                intern("lambda"),
                Value::make_cons(
                    vars,
                    Value::make_cons(Value::make_cons(intern("assigned"), u), body),
                ),
            ),
            assigned,
        )
    }

    fn uncover_body(x: Value) -> (Value, Value) {
        let mut assigned = Value::make_null();

        let newbody = Value::list_map(Thread::current(), x, |exp| {
            let (nx, x_assigned) = uncover(exp);
            assigned = Value::list_union(Thread::current(), assigned, x_assigned);
            nx
        });
        (newbody, assigned)
    }

    fn uncover_begin(x: Value) -> (Value, Value) {
        let (body, assigned) = uncover_body(x.cdr());
        (Value::make_cons(intern("begin"), body), assigned)
    }

    fn uncover_if(x: Value) -> (Value, Value) {
        let (test, test_assigned) = uncover(x.cadr());
        let (conseq, conseq_assigned) = uncover(x.caddr());
        let (alt, alt_assigned) = uncover(x.cadddr());

        (
            Value::make_cons(
                intern("if"),
                Value::make_cons(
                    test,
                    Value::make_cons(conseq, Value::make_cons(alt, Value::make_null())),
                ),
            ),
            Value::list_union(
                Thread::current(),
                test_assigned,
                Value::list_union(Thread::current(), conseq_assigned, alt_assigned),
            ),
        )
    }

    fn uncover_set(x: Value) -> (Value, Value) {
        let (rhs, rhs_assigned) = uncover(x.caddr());
        (
            Value::make_cons(
                intern("set!"),
                Value::make_cons(x.cadr(), Value::make_cons(rhs, Value::make_null())),
            ),
            set_cons(x.cadr(), rhs_assigned),
        )
    }

    fn uncover_set_then(x: Value) -> (Value, Value) {
        let (rhs, rhs_assigned) = uncover(x.caddr());
        let then = x.cadddr();

        let (then, then_assigned) = uncover(then);

        (
            Value::make_cons(
                intern("set-then!"),
                Value::make_cons(x.cadr(), Value::make_cons(rhs, then)),
            ),
            set_cons(
                x.cadr(),
                Value::list_union(Thread::current(), rhs_assigned, then_assigned),
            ),
        )
    }

    fn uncover(x: Value) -> (Value, Value) {
        if x.pairp() {
            if x.car().symbolp() {
                match x.car().strsym() {
                    "#%app" => uncover_app(x),
                    "lambda" => uncover_lambda(x),
                    "begin" => uncover_begin(x),
                    "if" => uncover_if(x),
                    "set!" => uncover_set(x),
                    "quote" => (x, Value::make_null()),
                    "set-then!" => uncover_set_then(x),
                    "#%toplevel-cont" => (x, Value::make_null()),
                    _ => unreachable!("{}", x),
                }
            } else {
                //println!("{}", x);
                unreachable!()
            }
        } else {
            (x, Value::make_null())
        }
    }
}

pub mod convert_assignments {
    //! This pass converts assigned variables into boxes, `set!` into `#%box-set!` and references
    //! to the assigned variables into `#%unbox`. This code is based on <https://github.com/yinwang0/yscheme/blob/master/compiler.ss#L657>.
    //! But there is a difference: we do not create `let` form here since it would not good for performance reasons, instead we just
    //! emit `(set! <binding> (#%box <binding>))` since our compiler *can* handle mutating local variables. This produces code that
    //! is more efficient than the code produced by the original compiler.
    use super::*;

    fn make_bindings(ass: Value, bd: Value) -> (Value, Value) {
        fn rec(bd: Value, bdo: Value, env: Value, ass: Value) -> (Value, Value) {
            if bd.symbolp() && Value::memq(bd, ass).is_true() {
                let new = unique_name();

                let bdo = Value::list_reverse(Thread::current(), bdo);

                return (
                    Value::list_append(Thread::current(), bdo, new),
                    Value::list_reverse(
                        Thread::current(),
                        Value::make_cons(
                            Value::make_cons(
                                bd,
                                Value::make_cons(
                                    Value::make_cons(
                                        intern("#%box"),
                                        Value::make_cons(bd, Value::make_null()),
                                    ),
                                    Value::make_null(),
                                ),
                            ),
                            env,
                        ),
                    ),
                );
            } else if bd.symbolp() {
                let bdo = Value::list_reverse(Thread::current(), bdo);
                let env = Value::list_reverse(Thread::current(), env);
                return (Value::list_append(Thread::current(), bdo, bd), env);
            }

            if bd.nullp() {
                (
                    Value::list_reverse(Thread::current(), bdo),
                    Value::list_reverse(Thread::current(), env),
                )
            } else if !bd.car().pairp() && Value::memq(bd.car(), ass).is_true() {
                let new = unique_name();

                return rec(
                    bd.cdr(),
                    Value::make_cons(new, bdo),
                    Value::make_cons(
                        Value::make_cons(
                            bd.car(),
                            Value::make_cons(
                                Value::make_cons(
                                    intern("#%box"),
                                    Value::make_cons(bd.car(), Value::make_null()),
                                ),
                                Value::make_null(),
                            ),
                        ),
                        env,
                    ),
                    ass,
                );
            } else if bd.car().pairp() && Value::memq(bd.caar(), ass).is_true() {
                let new = unique_name();
                return rec(
                    bd.cdr(),
                    Value::make_cons(
                        Value::make_cons(new, Value::make_cons(bd.cadar(), Value::make_null())),
                        bdo,
                    ),
                    Value::make_cons(
                        Value::make_cons(
                            bd.caar(),
                            Value::make_cons(
                                intern("#%box"),
                                Value::make_cons(bd.caar(), Value::make_null()),
                            ),
                        ),
                        env,
                    ),
                    ass,
                );
            } else {
                rec(bd.cdr(), Value::make_cons(bd.car(), bdo), env, ass)
            }
        }

        rec(bd, Value::make_null(), Value::make_null(), ass)
    }

    fn convert_set(exp: Value, env: Value) -> Value {
        let x = exp.cadr();

        let v = convert(exp.caddr(), env);

        if x.symbolp() {
            if Value::memq(x, env).is_true() {
                Value::make_cons(
                    intern("#%set-box!"),
                    Value::make_cons(x, Value::make_cons(v, Value::make_null())),
                )
            } else {
                make_assignment(x, v)
            }
        } else {
            unreachable!()
        }
    }

    fn convert_body(body: Value, env: Value) -> Value {
        Value::list_map(Thread::current(), body, |x| convert(x, env))
    }

    fn convert_begin(exp: Value, env: Value) -> Value {
        let body = convert_body(exp.cdr(), env);

        Value::make_cons(intern("begin"), body)
    }

    fn convert_if(exp: Value, env: Value) -> Value {
        let test = convert(exp.cadr(), env);
        let conseq = convert(exp.caddr(), env);
        let alt = convert(exp.cadddr(), env);

        Value::make_cons(
            intern("if"),
            Value::make_cons(
                test,
                Value::make_cons(conseq, Value::make_cons(alt, Value::make_null())),
            ),
        )
    }

    fn convert_lambda(x: Value, env: Value) -> Value {
        let formals = x.cadr();
        let assigned = x.caddr().cdr();
        let body = x.cdddr();
        let (_, env_) = make_bindings(assigned, formals);
        if env_.nullp() {
            Value::make_cons(
                intern("lambda"),
                Value::make_cons(
                    formals,
                    convert_body(body, Value::list_append(Thread::current(), assigned, env)),
                ),
            )
        } else {
            let nenv = Value::list_append(Thread::current(), assigned, env);
            let body = convert_body(body, nenv);
            let mut assignments = Value::make_null();
            let mut ls = env_;
            while ls.pairp() {
                let l = ls.car();
                assignments = Value::make_cons(
                    Value::make_cons(
                        intern("set!"),
                        Value::make_cons(l.car(), Value::make_cons(l.cadr(), Value::make_null())),
                    ),
                    assignments,
                );
                ls = ls.cdr();
            }

            let body = Value::list_append(Thread::current(), assignments, body);

            Value::make_cons(intern("lambda"), Value::make_cons(formals, body))
        }
    }

    pub fn convert(x: Value, env: Value) -> Value {
        if !x.pairp() {
            if x.symbolp() {
                if Value::memq(x, env).is_true() {
                    Value::make_cons(intern("#%unbox"), Value::make_cons(x, Value::make_null()))
                } else {
                    x
                }
            } else {
                x
            }
        } else {
            if x.car().symbolp() {
                match x.car().strsym() {
                    "set!" => convert_set(x, env),

                    "#%app" => {
                        let f = convert(x.cadr(), env);
                        let args =
                            Value::list_map(Thread::current(), x.cddr(), |x| convert(x, env));

                        Value::make_cons(intern("#%app"), Value::make_cons(f, args))
                    }

                    "quote" => x,
                    "begin" => convert_begin(x, env),
                    "lambda" => convert_lambda(x, env),
                    "if" => convert_if(x, env),
                    "#%toplevel-cont" => x,
                    _ => unreachable!("applications should be converted to #%app"),
                }
            } else {
                unreachable!("applications should be converted to #%app: {}", x)
            }
        }
    }
}

pub fn lookup(n: Value, r: Value) -> Value {
    if r.nullp() {
        n
    } else {
        fn rec(s: Value, r: Value, n: Value) -> Value {
            if s.nullp() {
                if r.nullp() {
                    n
                } else {
                    rec(r.car(), r.cdr(), n)
                }
            } else {
                let v = s.car();

                if n == v.car() {
                    v.cdr()
                } else {
                    rec(s.cdr(), r, n)
                }
            }
        }

        rec(r.car(), r.cdr(), n)
    }
}

pub fn rename_var(n: Value) -> Value {
    static RENAME: AtomicUsize = AtomicUsize::new(0);

    intern(format!(
        "#%.var{}|{}",
        RENAME.fetch_add(1, Ordering::SeqCst),
        n,
    ))
}

pub fn make_env(vars: Value) -> Value {
    Value::list_map(Thread::current(), vars, |x| {
        Value::make_list(Thread::current(), &[x, rename_var(x)])
    })
}

pub mod meaning {
    //! Transforms expressions into a syntax-tree,
    //! calculates lexical addresses, collect free
    //! and assigned bindings

    use super::*;

    pub fn meaning(e: Value, r: Value, tail: bool) -> Value {
        if e.pairp() {
            match e.car().strsym() {
                "quote" => e,
                "begin" => meaning_begin(e, r, tail),
                "lambda" => meaning_lambda(e.cadr(), e.cddr(), r),
                "set!" => meaning_set(e, r, tail),
                "if" => meaning_if(e, r, tail),
                "#%set-box!" => meaning_set_box(e, r),
                "#%unbox" => meaning_unbox(e, r),
                "#%box" => meaning_box(e, r),
                "#%app" => meaning_app(e.cdr(), r, tail),
                _ => unreachable!("applications should be converted to #%app: {}", e),
            }
        } else if e.symbolp() {
            if e.strsym() == "#%toplevel-cont" {
                let e = Value::make_cons(e, Value::make_null());

                e
            } else {
                meaning_reference(e, r)
            }
        } else {
            e
        }
    }

    fn meaning_set_box(e: Value, r: Value) -> Value {
        let var = e.cadr();
        let exp = e.caddr();

        let var = meaning_reference(var, r);

        let exp = meaning(exp, r, false);

        Value::make_list(Thread::current(), &[intern("#%set-box!"), var, exp])
    }

    fn meaning_unbox(e: Value, r: Value) -> Value {
        let var = e.cadr();

        let var = meaning_reference(var, r);

        Value::make_list(Thread::current(), &[intern("#%unbox"), var])
    }

    fn meaning_box(e: Value, r: Value) -> Value {
        let var = e.cadr();

        let var = meaning_reference(var, r);

        Value::make_list(Thread::current(), &[intern("#%box"), var])
    }

    fn meaning_set(e: Value, r: Value, _tail: bool) -> Value {
        let var = e.cadr();
        let exp = e.caddr();

        let var = meaning_reference(var, r);

        let exp = meaning(exp, r, false);

        Value::make_list(Thread::current(), &[intern("set!"), var, exp])
    }

    fn meaning_app(e: Value, r: Value, _tail: bool) -> Value {
        let mut ls = e.cdr();
        let mut args = Value::make_null();
        while !ls.nullp() {
            args = Value::make_cons(meaning(ls.car(), r, false), args);
            ls = ls.cdr();
        }

        make_call(
            meaning(e.car(), r, false),
            Value::list_reverse(Thread::current(), args),
        )
    }

    fn meaning_reference(n: Value, r: Value) -> Value {
        Value::make_list(
            Thread::current(),
            &[intern("#%ref"), n, lookup(n, r), Value::make_false()],
        )
    }

    fn meaning_begin(e: Value, r: Value, tail: bool) -> Value {
        make_begin2(meaning_body(e.cdr(), r, tail))
    }

    fn meaning_if(e: Value, r: Value, tail: bool) -> Value {
        let test = e.cadr();
        let conseq = e.caddr();
        let alt = e.cadddr();

        make_conditional(
            meaning(test, r, false),
            meaning(conseq, r, tail),
            meaning(alt, r, tail),
        )
    }

    fn meaning_body(e: Value, r: Value, tail: bool) -> Value {
        let mut ls = e;
        let mut res = Value::make_null();

        while !ls.nullp() {
            let exp = ls.car();
            let next = ls.cdr();

            if next.nullp() {
                res = Value::make_cons(meaning(exp, r, tail), res);
            } else {
                res = Value::make_cons(meaning(exp, r, false), res);
            }

            ls = next;
        }

        Value::list_reverse(Thread::current(), res)
    }

    fn meaning_lambda(n_star: Value, e: Value, r: Value) -> Value {
        fn meaning_internal(n_star: Value, n: Value, e: Value, r: Value) -> Value {
            let an_env = Value::list_map(
                Thread::current(),
                if !n.nullp() {
                    Value::list_append(
                        Thread::current(),
                        n_star,
                        Value::make_cons(n, Value::make_null()),
                    )
                } else {
                    n_star
                },
                |n| Value::make_cons(n, rename_var(n)),
            );

            let r2 = Value::make_cons(an_env, r);

            let m = meaning_body(e, r2, true);
            let bound = Value::list_map(Thread::current(), an_env, |n| n.cdr());
            let free = collect_free_body(m, bound);

            let m2 = calculate_addresses_body(m, bound, free);

            let free_refs = Value::list_map(Thread::current(), free, |n| meaning_reference(n, r));
            Value::make_list_star(
                Thread::current(),
                &[
                    intern("lambda"),
                    Value::make_cons(
                        if !n.nullp() {
                            intern(">=")
                        } else {
                            intern("=")
                        },
                        Value::make_int(n_star.list_length() as i32),
                    ),
                    bound,
                    free_refs,
                    m2,
                ],
            )
        }

        fn parse(n_star: Value, regular: Value, r: Value, e: Value) -> Value {
            if n_star.nullp() {
                meaning_internal(
                    Value::list_reverse(Thread::current(), regular),
                    Value::make_null(),
                    e,
                    r,
                )
            } else if n_star.symbolp() {
                meaning_internal(
                    Value::list_reverse(Thread::current(), regular),
                    n_star,
                    e,
                    r,
                )
            } else {
                let n = n_star.car();
                let n_star = n_star.cdr();

                parse(n_star, Value::make_cons(n, regular), r, e)
            }
        }

        parse(n_star, Value::make_null(), r, e)
    }

    pub fn set_union(set1: Value, set2: Value) -> Value {
        fn rec(set1: Value, set2: Value) -> Value {
            if set2.nullp() {
                set1
            } else {
                let item = set2.car();

                if Value::list_member(item, set1).is_true() {
                    return rec(set1, set2.cdr());
                } else {
                    return rec(Value::make_cons(item, set1), set2.cdr());
                }
            }
        }

        rec(set1, set2)
    }

    pub fn index_of(item: Value, list: Value) -> Option<usize> {
        fn rec(item: Value, list: Value, i: usize) -> Option<usize> {
            if list.nullp() {
                None
            } else if list.car() == item {
                Some(i)
            } else {
                rec(item, list.cdr(), i + 1)
            }
        }

        rec(item, list, 0)
    }

    fn collect_free_app(m: Value, bound: Value) -> Value {
        let mut ls = m.cdr();
        let mut res = Value::make_null();

        while !ls.nullp() {
            let exp = ls.car();
            let next = ls.cdr();
            let r = collect_free(exp, bound);
            res = set_union(res, r);

            ls = next;
        }

        let proc = m.car();
        let proc_free = collect_free(proc, bound);
        set_union(res, proc_free)
    }

    fn collect_free_body(m: Value, bound: Value) -> Value {
        let mut ls = m;
        let mut res = Value::make_null();

        while !ls.nullp() {
            let exp = ls.car();
            let next = ls.cdr();

            res = set_union(res, collect_free(exp, bound));

            ls = next;
        }

        res
    }

    pub fn collect_free(m: Value, bound: Value) -> Value {
        if m.pairp() {
            match m.car().strsym() {
                "#%toplevel-cont" => Value::make_null(),
                "#%ref" => {
                    let n = m.cadr();
                    let an = m.caddr();
                    if n == an || Value::memq(an, bound).is_true() {
                        Value::make_null()
                    } else {
                        Value::make_cons(an, Value::make_null())
                    }
                }

                "begin" => {
                    let mut ls = m.cdr();
                    let mut res = Value::make_null();

                    while !ls.nullp() {
                        let exp = ls.car();
                        let next = ls.cdr();

                        res = set_union(res, collect_free(exp, bound));

                        ls = next;
                    }

                    res
                }

                "lambda" => {
                    let lambda_bound = m.caddr();
                    let body = m.cdddr().cdr();

                    collect_free_body(
                        body,
                        Value::list_append(Thread::current(), lambda_bound, bound),
                    )
                }

                "if" => {
                    let test = collect_free(m.cadr(), bound);
                    let conseq = collect_free(m.caddr(), bound);
                    let alt = collect_free(m.cadddr(), bound);

                    set_union(test, set_union(conseq, alt))
                }

                "#%app" => collect_free_app(m.cdr(), bound),
                "#%set-box!" => {
                    let reference = m.cadr();
                    let n = reference.cadr();
                    let an = reference.caddr();

                    let sref = if n == an || Value::memq(an, bound).is_true() {
                        Value::make_null()
                    } else {
                        Value::make_cons(an, Value::make_null())
                    };

                    let value = collect_free(m.caddr(), bound);

                    set_union(sref, value)
                }

                "set!" => {
                    let reference = m.cadr();
                    let n = reference.cadr();
                    let an = reference.caddr();

                    let sref = if n == an || Value::memq(an, bound).is_true() {
                        Value::make_null()
                    } else {
                        Value::make_cons(an, Value::make_null())
                    };

                    let value = collect_free(m.caddr(), bound);
                    set_union(sref, value)
                }

                "#%unbox" => collect_free(m.cadr(), bound),
                "#%box" => collect_free(m.cadr(), bound),
                "quote" => Value::make_null(),
                _ => unreachable!("application should be converted to #%app: {}", m),
            }
        } else {
            assert!(!m.symbolp(), "all references are converted to (refer ...)");
            Value::make_null()
        }
    }

    /// Calculate the real addresses of variables in the given expression, returns
    /// the new expression.
    pub fn calculate_addresses(m: Value, bound: Value, free: Value) -> Value {
        if m.pairp() {
            match m.car().strsym() {
                "#%toplevel-cont" => m,
                "#%ref" => {
                    let n = m.cadr();
                    let an = m.caddr();

                    if let Some(i) = index_of(an, bound) {
                        Value::make_list(
                            Thread::current(),
                            &[
                                intern("#%ref"),
                                n,
                                an,
                                Value::make_cons(intern("bound"), Value::make_int(i as _)),
                            ],
                        )
                    } else if let Some(i) = index_of(an, free) {
                        Value::make_list(
                            Thread::current(),
                            &[
                                intern("#%ref"),
                                n,
                                an,
                                Value::make_cons(intern("free"), Value::make_int(i as _)),
                            ],
                        )
                    } else {
                        Value::make_list(
                            Thread::current(),
                            &[intern("#%ref"), n, an, Value::make_false()],
                        )
                    }
                }

                "set!" => {
                    let var = m.cadr();

                    let n = var.cadr();
                    let an = var.caddr();

                    let var = if let Some(i) = index_of(an, bound) {
                        Value::make_list(
                            Thread::current(),
                            &[
                                intern("#%ref"),
                                n,
                                an,
                                Value::make_cons(intern("bound"), Value::make_int(i as _)),
                            ],
                        )
                    } else if let Some(i) = index_of(an, free) {
                        Value::make_list(
                            Thread::current(),
                            &[
                                intern("#%ref"),
                                n,
                                an,
                                Value::make_cons(intern("free"), Value::make_int(i as _)),
                            ],
                        )
                    } else {
                        Value::make_list(
                            Thread::current(),
                            &[intern("#%ref"), n, an, Value::make_false()],
                        )
                    };

                    let expr = calculate_addresses(m.caddr(), bound, free);

                    Value::make_cons(
                        intern("set!"),
                        Value::make_cons(var, Value::make_cons(expr, Value::make_null())),
                    )
                }

                "#%set-box!" => {
                    let var = m.cadr();

                    let n = var.cadr();
                    let an = var.caddr();

                    let var = if let Some(i) = index_of(an, bound) {
                        Value::make_list(
                            Thread::current(),
                            &[
                                intern("#%ref"),
                                n,
                                an,
                                Value::make_cons(intern("bound"), Value::make_int(i as _)),
                            ],
                        )
                    } else if let Some(i) = index_of(an, free) {
                        Value::make_list(
                            Thread::current(),
                            &[
                                intern("#%ref"),
                                n,
                                an,
                                Value::make_cons(intern("free"), Value::make_int(i as _)),
                            ],
                        )
                    } else {
                        Value::make_list(
                            Thread::current(),
                            &[intern("#%ref"), n, an, Value::make_false()],
                        )
                    };

                    let expr = calculate_addresses(m.caddr(), bound, free);

                    Value::make_cons(
                        intern("#%set-box!"),
                        Value::make_cons(var, Value::make_cons(expr, Value::make_null())),
                    )
                }

                "#%unbox" => {
                    let var = m.cadr();

                    let n = var.cadr();
                    let an = var.caddr();

                    let var = if let Some(i) = index_of(an, bound) {
                        Value::make_list(
                            Thread::current(),
                            &[
                                intern("#%ref"),
                                n,
                                an,
                                Value::make_cons(intern("bound"), Value::make_int(i as _)),
                            ],
                        )
                    } else if let Some(i) = index_of(an, free) {
                        Value::make_list(
                            Thread::current(),
                            &[
                                intern("#%ref"),
                                n,
                                an,
                                Value::make_cons(intern("free"), Value::make_int(i as _)),
                            ],
                        )
                    } else {
                        Value::make_list(
                            Thread::current(),
                            &[intern("#%ref"), n, an, Value::make_false()],
                        )
                    };

                    Value::make_cons(intern("#%unbox"), Value::make_cons(var, Value::make_null()))
                }

                "#%box" => {
                    let var = m.cadr();

                    let n = var.cadr();
                    let an = var.caddr();

                    let var = if let Some(i) = index_of(an, bound) {
                        Value::make_list(
                            Thread::current(),
                            &[
                                intern("#%ref"),
                                n,
                                an,
                                Value::make_cons(intern("bound"), Value::make_int(i as _)),
                            ],
                        )
                    } else if let Some(i) = index_of(an, free) {
                        Value::make_list(
                            Thread::current(),
                            &[
                                intern("#%ref"),
                                n,
                                an,
                                Value::make_cons(intern("free"), Value::make_int(i as _)),
                            ],
                        )
                    } else {
                        Value::make_list(
                            Thread::current(),
                            &[intern("#%ref"), n, an, Value::make_false()],
                        )
                    };

                    Value::make_cons(intern("#%box"), Value::make_cons(var, Value::make_null()))
                }

                "begin" => {
                    let body = calculate_addresses_body(m.cdr(), bound, free);
                    Value::make_cons(intern("begin"), body)
                }

                "lambda" => {
                    let formals = m.cadr();
                    let lambda_bound = m.caddr();
                    let lambda_free = Value::list_map(Thread::current(), m.cadddr(), |x| {
                        calculate_addresses(x, bound, free)
                    });
                    let body = m.cdddr().cdr();
                    //calculate_addresses_body(m.cdddr().cdr(), bound, free);

                    Value::make_list_star(
                        Thread::current(),
                        &[intern("lambda"), formals, lambda_bound, lambda_free, body],
                    )
                }

                "#%app" => calculate_addresses_app(m.cdr(), bound, free),
                "quote" => m,
                "if" => {
                    let test = calculate_addresses(m.cadr(), bound, free);
                    let consequent = calculate_addresses(m.caddr(), bound, free);
                    let alternative = calculate_addresses(m.cadddr(), bound, free);

                    Value::make_list(
                        Thread::current(),
                        &[intern("if"), test, consequent, alternative],
                    )
                }
                _ => unreachable!("applications are converted to (#%app ...)"),
            }
        } else {
            assert!(!m.symbolp(), "all references are converted to (refer ...)");
            m
        }
    }

    fn calculate_addresses_app(app: Value, bound: Value, free: Value) -> Value {
        let proc = calculate_addresses(app.car(), bound, free);
        let args = calculate_addresses_body(app.cdr(), bound, free);

        make_call(proc, args)
    }

    fn calculate_addresses_body(body: Value, bound: Value, free: Value) -> Value {
        if body.nullp() {
            Value::make_null()
        } else {
            Value::make_cons(
                calculate_addresses(body.car(), bound, free),
                calculate_addresses_body(body.cdr(), bound, free),
            )
        }
    }
}

pub mod lambda_lifting {
    use std::collections::hash_map::RandomState;

    use super::*;
    use rsgc::{
        prelude::{Handle, Object},
        system::collections::hashmap::HashMap,
    };
    #[derive(Clone, Copy)]
    pub struct LiftedLambda {
        pub id: i32,
        pub arity: Value,
        pub bound: Value,
        pub free: Value,
        pub body: Value,
    }

    impl Object for LiftedLambda {
        fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
            self.arity.trace(visitor);
            self.bound.trace(visitor);
            self.free.trace(visitor);
            self.body.trace(visitor);
        }
    }

    pub struct LiftingCtx {
        pub next_id: i32,
        pub lambdas: Handle<HashMap<i32, LiftedLambda>>,
    }

    impl LiftingCtx {
        pub fn new() -> Self {
            Self {
                next_id: i32::MIN + 100,
                lambdas: HashMap::with_hasher(RandomState::new()),
            }
        }

        pub fn add(&mut self, lambda: LiftedLambda) -> i32 {
            let id = self.next_id;
            self.next_id += 1;
            self.lambdas.put(Thread::current(), id, lambda);
            id
        }
    }

    pub fn lift_lambdas(exp: Value, seq: bool) -> (Value, Handle<HashMap<i32, LiftedLambda>>) {
        let mut ctx = LiftingCtx::new();
        let exp = if !seq {
            ll(exp, &mut ctx)
        } else {
            ll_body(exp, &mut ctx)
        };
        (exp, ctx.lambdas)
    }

    /// Lifts lambdas in the expression. Returns rewritten expression.
    fn ll(exp: Value, ctx: &mut LiftingCtx) -> Value {
        if !exp.pairp() {
            exp
        } else {
            if !exp.car().symbolp() {
                unreachable!("applications are converted to (#%app ...): {}", exp);
            }

            match exp.car().strsym() {
                "lambda" => {
                    let formals = exp.cadr();
                    let bound = exp.caddr();
                    let free = exp.cadddr();
                    let body = ll_body(exp.cdddr().cdr(), ctx);

                    let id = ctx.add(LiftedLambda {
                        id: ctx.next_id,
                        arity: formals,
                        bound,
                        free,
                        body,
                    });

                    Value::make_list(
                        Thread::current(),
                        &[intern("lambda"), Value::make_int(id as _)],
                    )
                }

                "#%app" => ll_app(exp.cdr(), ctx),
                "begin" => ll_begin(exp, ctx),
                "set!" => ll_set(exp, ctx),
                "#%set-box!" => ll_set_box(exp, ctx),
                "#%box" => exp,
                "#%unbox" => exp,
                "quote" => exp,
                "if" => ll_if(exp, ctx),
                "#%toplevel-cont" => exp,
                "#%ref" => exp,
                _ => unreachable!("applications are converted to (#%app ...): {}", exp),
            }
        }
    }

    fn ll_app(exp: Value, ctx: &mut LiftingCtx) -> Value {
        let mut ls = exp.cdr();
        let mut args = Value::make_null();
        while !ls.nullp() {
            args = Value::make_cons(ll(ls.car(), ctx), args);
            ls = ls.cdr();
        }

        make_call(
            ll(exp.car(), ctx),
            Value::list_reverse(Thread::current(), args),
        )
    }

    fn ll_body(exp: Value, ctx: &mut LiftingCtx) -> Value {
        let body = Value::list_map(Thread::current(), exp, |x| ll(x, ctx));

        body
    }

    fn ll_begin(exp: Value, ctx: &mut LiftingCtx) -> Value {
        let mut exp = exp.cdr();
        let mut body = Value::make_null();

        while !exp.nullp() {
            body = Value::make_cons(ll(exp.car(), ctx), body);
            exp = exp.cdr();
        }

        make_begin2(Value::list_reverse(Thread::current(), body))
    }

    fn ll_if(exp: Value, ctx: &mut LiftingCtx) -> Value {
        let test = ll(exp.cadr(), ctx);
        let consequent = ll(exp.caddr(), ctx);
        let alternative = ll(exp.cadddr(), ctx);

        make_conditional(test, consequent, alternative)
    }

    fn ll_set(exp: Value, ctx: &mut LiftingCtx) -> Value {
        let var = exp.cadr();
        let val = ll(exp.caddr(), ctx);

        make_assignment(var, val)
    }

    fn ll_set_box(exp: Value, ctx: &mut LiftingCtx) -> Value {
        let var = exp.cadr();
        let val = ll(exp.caddr(), ctx);

        Value::make_cons(
            intern("#%set-box!"),
            Value::make_cons(var, Value::make_cons(val, Value::make_null())),
        )
    }
}

pub fn make_null_terminated(formals: Value) -> Value {
    if formals.nullp() {
        Value::make_null()
    } else if formals.symbolp() {
        Value::make_cons(formals, Value::make_null())
    } else {
        Value::make_cons(formals.car(), make_null_terminated(formals.cdr()))
    }
}



pub fn desugar(exp: Value, defs: &mut Value) -> Result<Value, Value> {
    let exp = pass1::desugar_definitions(exp, defs)?;
    Ok(exp)
}

pub fn uncover_assigned(exp: Value) -> Value {
    uncover_assigned::uncover_assigned(exp)
}

pub fn convert_assignments(exp: Value) -> Value {
    convert_assignments::convert(exp, Value::make_null())
}

pub fn cps(exp: Value, k: Value) -> Value {
    cps::t_c(exp, k)
}

pub fn cps_k(exp: Value, k: &dyn Fn(Value) -> Value) -> Value {
    cps::t_k(exp, k)
}

pub fn get_arity_from_lambda(arity: Value) -> (i32, i32) {
    match arity.car().strsym() {
        ">=" => (arity.cdr().int(), SCHEME_MAX_ARGS),
        "=" => (arity.cdr().int(), arity.cdr().int()),
        _ => unreachable!(),
    }
}

pub fn is_ref(exp: Value) -> bool {
    exp.pairp() && exp.car().strsym() == "#%ref"
}

pub fn is_global_ref(exp: Value) -> bool {
    exp.cadddr().falsep()
}

pub fn ref_name(exp: Value) -> Value {
    exp.cadr()
}