use r7rs_parser::expr::{Expr, NoIntern};
use rsgc::thread::Thread;

use crate::{
    list::{scm_cons, scm_is_list, scm_length, scm_list, scm_map, scm_reverse},
    string::make_string,
    symbol::make_symbol,
    value::Value,
    vector::{make_bytevector_from_slice, make_vector},
};

use super::env::{env_bind, lookup};
fn r7rs_to_value_k(thread: &mut Thread, expr: &Expr<NoIntern>, cont: &mut dyn FnMut(Value)) {
    match expr {
        Expr::Bool(x) => cont(Value::encode_bool_value(*x)),
        Expr::Fixnum(i) => cont(Value::encode_int32(*i)),
        Expr::Float(f) => cont(Value::encode_f64_value(*f)),
        Expr::Pair(car, cdr) => r7rs_to_value_k(thread, car, &mut |car| {
            r7rs_to_value_k(Thread::current(), cdr, &mut |cdr| {
                cont(scm_cons(Thread::current(), car, cdr))
            })
        }),

        Expr::ByteVector(x) => cont(make_bytevector_from_slice(thread, x).into()),
        Expr::Str(x) => cont(make_string(thread, x).into()),
        Expr::Symbol(x) => cont(make_symbol(x, true)),
        Expr::GrowableVector(x) | Expr::ImmutableVector(x) => {
            let vec = make_vector(thread, x.len());
            for (i, e) in x.iter().enumerate() {
                r7rs_to_value_k(thread, e, &mut move |e| {
                    let mut vec = vec;
                    Thread::current().write_barrier(vec);
                    vec[i] = e;
                });
            }
            cont(vec.into())
        }

        Expr::Null => cont(Value::encode_null_value()),
        Expr::Syntax(_, e) => r7rs_to_value_k(thread, e, cont),
        _ => unsafe { std::hint::unreachable_unchecked() },
    }
}

pub fn r7rs_to_value(thread: &mut Thread, expr: &Expr<NoIntern>) -> Value {
    let mut ret = Value::encode_null_value();
    r7rs_to_value_k(thread, expr, &mut |x| ret = x);
    ret
}

pub fn is_call(thing: Value) -> bool {
    thing.is_vector() && thing.vector_ref(0).strsym() == "call"
}

pub fn is_var(thing: Value) -> bool {
    thing.is_vector() && thing.vector_ref(0).strsym() == "var"
}

pub fn mkval(val: Value) -> Value {
    make_vector!(make_symbol("value", true).into(), val).into()
}

pub fn mklambda(formals: Value, body: Value) -> Value {
    make_vector!(
        make_symbol("lambda-var", true).into(),
        Value::encode_bool_value(true),
        formals,
        body
    )
    .into()
}

pub fn mkcall(rator: Value, rands: Value) -> Value {
    make_vector!(make_symbol("call", true).into(), rator, rands).into()
}

pub fn mkprim(op: Value, args: Value) -> Value {
    make_vector!(make_symbol("prim", true).into(), op, args).into()
}

pub fn mkvar(name: Value) -> Value {
    make_vector!(make_symbol("var", true).into(), name).into()
}

pub fn check_formals(lst: Value) -> (Value, bool) {
    #[inline(always)]
    fn rec(thread: &mut Thread, lst: Value, out: Value) -> (Value, bool) {
        if lst.is_null() {
            (scm_reverse(thread, out), true)
        } else if lst.is_symbol() {
            let c = scm_cons(thread, lst, out);
            (c, false)
        } else if lst.car().is_symbol() {
            let c = scm_cons(thread, lst.car(), out);
            rec(thread, lst.cdr(), c)
        } else {
            (Value::encode_bool_value(false), false)
        }
    }

    rec(Thread::current(), lst, Value::encode_null_value())
}

pub fn check_fixed_formals(sexp: Value) -> bool {
    let (formals, fixed) = check_formals(sexp);
    !formals.is_false() && fixed
}

pub fn translate_direct_call(exp: Value, env: Value) -> Result<Value, Value> {
    let thing = lookup(env, exp.car());

    if thing.is_vector() && thing.vector_ref(0).is_symbol() {
        let tag = thing.vector_ref(0).strsym();

        match tag {
            "special" => {
                let thing = thing.vector_ref(1).strsym();

                match thing {
                    "quote" => {
                        if scm_length(exp) == Some(2) {
                            return Ok(mkval(exp.cadr()));
                        } else {
                            return Err(
                                make_string(Thread::current(), "quote takes one argument").into()
                            );
                        }
                    }
                    "lambda" => match scm_length(exp) {
                        Some(3) => {
                            let formals = exp.cadr();
                            let body = exp.caddr();
                            let (formals, fixed) = check_formals(formals);

                            if formals.is_false() {
                                return Err(make_string(Thread::current(), "bad formals").into());
                            } else {
                                Ok(make_vector![
                                    make_symbol("lambda-var", true).into(),
                                    Value::encode_bool_value(fixed),
                                    formals,
                                    translate(body, env_bind(env, formals))?
                                ]
                                .into())
                            }
                        }

                        Some(_) => {
                            let formals = exp.cadr();
                            let body = exp.cddr();

                            return translate(
                                scm_list(
                                    Thread::current(),
                                    &[
                                        make_symbol("lambda", true),
                                        formals,
                                        scm_cons(
                                            Thread::current(),
                                            make_symbol("begin", true),
                                            body,
                                        ),
                                    ],
                                ),
                                env,
                            );
                        }

                        _ => {
                            return Err(make_string(
                                Thread::current(),
                                "lambda takes at least two arguments",
                            )
                            .into())
                        }
                    },

                    "let-eval" => {
                        if scm_length(exp) == Some(4) {
                            let formals = exp.cadr();
                            let values = exp.caddr();
                            let body = exp.cdddr();

                            if scm_is_list(values)
                                && check_fixed_formals(formals)
                                && scm_length(formals) == scm_length(values)
                            {
                                let env2 = env_bind(env, formals);
                                let mut err = None;
                                let vals = scm_map(
                                    Thread::current(),
                                    |val| match translate(val, env2) {
                                        Ok(val) => val,
                                        Err(e) => {
                                            err = Some(e);
                                            Value::encode_null_value()
                                        }
                                    },
                                    values,
                                );

                                if let Some(err) = err {
                                    return Err(err);
                                }

                                Ok(make_vector!(
                                    make_symbol("let-eval", true.into()).into(),
                                    formals,
                                    vals,
                                    translate(body, env2)?
                                )
                                .into())
                            } else {
                                return Err(make_string(Thread::current(), "bad let-eval").into());
                            }
                        } else {
                            return Err(make_string(
                                Thread::current(),
                                "let-eval takes three arguments",
                            )
                            .into());
                        }
                    }
                    "ifeq" => {
                        if scm_length(exp) == Some(5) {
                            let a = exp.second();
                            let b = exp.third();
                            let then = exp.fourth();
                            let els = exp.fifth();

                            Ok(make_vector!(
                                make_symbol("ifeq", true).into(),
                                translate(a, env)?,
                                translate(b, env)?,
                                translate(then, env)?,
                                translate(els, env)?
                            )
                            .into())
                        } else {
                            return Err(make_string(
                                Thread::current(),
                                "ifeq takes four arguments",
                            )
                            .into());
                        }
                    }

                    // (brae (lambda-ok) (lambda-else))
                    "brae" => {
                        if scm_length(exp) == Some(3) {
                            let ok = exp.second();
                            let els = exp.third();

                            Ok(make_vector!(
                                make_symbol("brae", true).into(),
                                translate(ok, env)?,
                                translate(els, env)?
                            )
                            .into())
                        } else {
                            return Err(
                                make_string(Thread::current(), "brae takes two arguments").into()
                            );
                        }
                    }

                    "values" | "values-apply" => {
                        let mut err = None;
                        let vals = scm_map(
                            Thread::current(),
                            |val| match translate(val, env) {
                                Ok(val) => val,
                                Err(e) => {
                                    err = Some(e);
                                    Value::encode_null_value()
                                }
                            },
                            exp.cdr(),
                        );

                        if let Some(err) = err {
                            return Err(err);
                        }

                        Ok(make_vector!(make_symbol(thing, true).into(), vals).into())
                    }
                    _ => Err(make_string(Thread::current(), "unknown special form").into()),
                }
            }

            "bound" => {
                let mut err = None;

                let rands = scm_map(
                    Thread::current(),
                    |val| match translate(val, env) {
                        Ok(val) => val,
                        Err(e) => {
                            err = Some(e);
                            Value::encode_null_value()
                        }
                    },
                    exp.cdr(),
                );

                if let Some(err) = err {
                    return Err(err);
                }

                Ok(mkcall(mkvar(exp.car()), rands))
            }

            "defined" => {
                let mut err = None;

                let rands = scm_map(
                    Thread::current(),
                    |val| match translate(val, env) {
                        Ok(val) => val,
                        Err(e) => {
                            err = Some(e);
                            Value::encode_null_value()
                        }
                    },
                    exp.cdr(),
                );

                Ok(mkcall(thing.vector_ref(1), rands))
            }

            err => {
                return Err(make_string(
                    Thread::current(),
                    &format!("unknown value type in conversion: '{}'", err),
                )
                .into())
            }
        }
    } else {
        return Err(make_string(Thread::current(), "weird AST").into());
    }
}

pub fn translate(exp: Value, env: Value) -> Result<Value, Value> {
    if exp.is_null() {
        Ok(mkval(exp))
    } else if scm_is_list(exp) {
        if exp.car().is_symbol() {
            translate_direct_call(exp, env)
        } else {
            let mut err = None;

            let rands = scm_map(
                Thread::current(),
                |val| match translate(val, env) {
                    Ok(val) => val,
                    Err(e) => {
                        err = Some(e);
                        Value::encode_null_value()
                    }
                },
                exp.cdr(),
            );

            Ok(mkcall(translate(exp.car(), env)?, rands))
        }
    } else if exp.is_symbol() {
        let l = lookup(env, exp);

        if l.is_vector() && l.vector_ref(0).is_symbol() {
            match l.vector_ref(0).strsym() {
                "bound" => Ok(mkvar(exp)),
                "defined" => Ok(l.vector_ref(1)),
                "special" => {
                    return Err(make_string(
                        Thread::current(),
                        "special form cannot be used by value",
                    )
                    .into())
                }

                "undefined" => {
                    return Err(make_string(Thread::current(), "undefined variable").into())
                }
                _ => return Err(make_string(Thread::current(), "weird env").into()),
            }
        } else {
            unreachable!("weird env")
        }
    } else {
        Ok(mkval(exp))
    }
}

pub fn sexp_to_ast(expr: Value, env: Value) -> Result<(Value, Value), Value> {
    Ok((translate(expr, env)?, env))
}
