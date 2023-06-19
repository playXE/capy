//! Scheme environment for compilation.
//!
//! It is represented as association list, where each element is a pair of symbol and value.
use once_cell::sync::Lazy;
use rsgc::thread::Thread;

use crate::{
    list::{scm_assq, scm_cons, scm_filter, scm_fold, scm_is_list, scm_length, scm_list, scm_map},
    string::make_string,
    symbol::make_symbol,
    value::Value,
};

static UNDEFINED: Lazy<Value> = Lazy::new(|| make_vector!(make_symbol("undefined", true)).into());

pub fn lookup(env: Value, key: Value) -> Value {
    let kv = scm_assq(key, env);

    if kv.is_false() {
        *UNDEFINED
    } else {
        kv.cdr()
    }
}

pub fn env_get(env: Value, key: Value, def: Value) -> Value {
    let v = lookup(env, key);

    if &*v.vector_ref(0).symbol() == "defined" {
        let val = v.vector_ref(1);

        if &*val.vector_ref(0).symbol() == "value" {
            return val.vector_ref(1);
        } else {
            def
        }
    } else {
        def
    }
}

pub fn env_get_raw(env: Value, key: Value) -> Value {
    scm_assq(key, env)
}

pub fn env_put_raw(env: Value, key: Value, val: Value) -> Value {
    scm_map(
        Thread::current(),
        |kv| {
            if kv.car() == key {
                scm_cons(Thread::current(), key, val)
            } else {
                kv
            }
        },
        env,
    )
}

pub fn env_set(env: Value, key: Value, val: Value) -> Value {
    let val = make_vector!(make_symbol("value", true), val);
    let defined = make_vector!(make_symbol("defined", true), val.into());

    env_put_raw(env, key, defined.into())
}

pub fn env_set_syntax(env: Value, key: Value, transformer: Value) -> Value {
    let defined = make_vector!(make_symbol("syntax", true), transformer);

    env_put_raw(env, key, defined.into())
}

pub fn env_set_macro(env: Value, key: Value, transformer: Value) -> Value {
    let defined = make_vector!(make_symbol("macro", true), transformer);

    env_put_raw(env, key, defined.into())
}

static BOUND: Lazy<Value> = Lazy::new(|| make_vector!(make_symbol("bound", true)).into());

/// mark an argument list (possibly improper list of symbols) as bound
pub fn env_bind(env: Value, keys: Value) -> Value {
    fn rec(env: Value, keys: Value) -> Value {
        if keys.is_null() {
            env
        } else if keys.is_pair() {
            let env = env_put_raw(env, keys.car(), *BOUND);
            rec(env, keys.cdr())
        } else {
            env_put_raw(env, keys, *BOUND)
        }
    }

    rec(env, keys)
}

pub fn value_exp(val: Value) -> Value {
    if val.is_pair() || val.is_symbol() {
        scm_list(Thread::current(), &[make_symbol("quote", true), val])
    } else {
        val
    }
}

pub fn handle_symbol(exp: Value, env: Value) -> Result<Value, Value> {
    let v = lookup(env, exp);

    if v.is_vector() {
        let tag: &str = &v.vector_ref(0).symbol();

        match tag {
            "bound" => Ok(exp),
            "defined" => {
                let defn = v.vector_ref(1);
                let defn_tag: &str = &defn.vector_ref(0).symbol();

                match defn_tag {
                    "value" => Ok(value_exp(defn.vector_ref(1))),
                    err => Err(make_string(
                        Thread::current(),
                        &format!(
                            "The symbol '{}' has non-value definition: '{}'",
                            exp.symbol(),
                            err
                        ),
                    )
                    .into()),
                }
            }

            "undefined" => Err(make_string(
                Thread::current(),
                &format!("The symbol '{}' is undefined", exp.symbol()),
            )
            .into()),

            _ => unreachable!("bad tag: {}", tag),
        }
    } else {
        Err(make_string(
            Thread::current(),
            &format!("The symbol '{}' has a funny value: '{:?}'", exp.symbol(), v),
        )
        .into())
    }
}

pub fn check_formals(call: Value) -> bool {
    let formals = call.cadr();
    #[inline(always)]
    fn rec(formals: Value) -> bool {
        if formals.is_pair() && formals.car().is_symbol() {
            rec(formals.cdr())
        } else if formals.is_string() {
            true
        } else if formals.is_null() {
            true
        } else {
            false
        }
    }

    rec(formals)
}

pub fn walk(exp: Value, env: Value) -> Result<Value, Value> {
    if exp.is_null() {
        Ok(scm_list(
            Thread::current(),
            &[make_symbol("quote", true), exp],
        ))
    } else if scm_is_list(exp) {
        // safety of doing `car` here: `exp.is_null()` is checked above
        match exp.car() {
            x if x.is_symbol() => {
                let key: &str = &x.symbol();

                match key {
                    "quote" => Ok(exp),
                    "lambda" => {
                        if scm_length(exp) == Some(3) && check_formals(exp) {
                            let env2 = env_bind(env, exp.cadr());

                            Ok(scm_list(
                                Thread::current(),
                                &[
                                    make_symbol("lambda", true),
                                    exp.cadr(),
                                    walk(exp.caddr(), env2)?,
                                ],
                            ))
                        } else {
                            Err(
                                make_string(Thread::current(), &format!("Bad lambda: '{:?}'", exp))
                                    .into(),
                            )
                        }
                    }
                    "let-eval" => {
                        if scm_length(exp) == Some(4) && check_formals(exp) {
                            let env2 = env_bind(env, exp.cadr());
                            Ok(scm_list(
                                Thread::current(),
                                &[
                                    make_symbol("let-eval", true),
                                    exp.cadr(),
                                    {
                                        let mut err = None;
                                        let body = exp.caddr();

                                        let res = scm_map(
                                            Thread::current(),
                                            |x| match walk(x, env2) {
                                                Ok(x) => x,
                                                Err(e) => {
                                                    err = Some(e);
                                                    x
                                                }
                                            },
                                            body,
                                        );

                                        if let Some(e) = err {
                                            return Err(e);
                                        } else {
                                            res
                                        }
                                    },
                                    walk(exp.cdddr().car(), env2)?,
                                ],
                            ))
                        } else {
                            Err(make_string(
                                Thread::current(),
                                &format!("Bad let-eval: '{:?}'", exp),
                            )
                            .into())
                        }
                    }

                    "values" | "values-apply" | "ifeq" | "brae" => {
                        let mut err = None;

                        let res = scm_map(
                            Thread::current(),
                            |x| match walk(x, env) {
                                Ok(x) => x,
                                Err(e) => {
                                    err = Some(e);
                                    x
                                }
                            },
                            exp.cdr(),
                        );

                        match err {
                            Some(e) => Err(e),
                            None => Ok(scm_cons(Thread::current(), exp.car(), res)),
                        }
                    }

                    _ => {
                        let mut err = None;

                        let res = scm_map(
                            Thread::current(),
                            |x| match walk(x, env) {
                                Ok(x) => x,
                                Err(e) => {
                                    err = Some(e);
                                    x
                                }
                            },
                            exp,
                        );

                        if let Some(e) = err {
                            Err(e)
                        } else {
                            Ok(res)
                        }
                    }
                }
            }

            _ => {
                let mut err = None;

                let res = scm_map(
                    Thread::current(),
                    |x| match walk(x, env) {
                        Ok(x) => x,
                        Err(e) => {
                            err = Some(e);
                            x
                        }
                    },
                    exp,
                );

                if let Some(e) = err {
                    Err(e)
                } else {
                    Ok(res)
                }
            }
        }
    } else {
        match exp {
            x if x.is_number() => Ok(exp),

            x if x.is_symbol() => handle_symbol(x, env),

            x if x.is_pair() => {
                return Err(
                    make_string(Thread::current(), &format!("Bad form: '{:?}'", exp)).into(),
                );
            }

            _ => Ok(scm_list(
                Thread::current(),
                &[make_symbol("quote", true), exp],
            )),
        }
    }
}

pub fn apply_env(exp: Value, env: Value) -> Result<(Value, Value), Value> {
    Ok((env, walk(exp, env)?))
}

pub fn env_fold(env: Value, func: impl FnMut(Value, Value) -> Value, acc: Value) -> Value {
    scm_fold(func, acc, env)
}

pub fn env_del(env: Value, key: Value) -> Value {
    scm_filter(
        |kv| {
            let kv = kv.car();
            kv.car() != key
        },
        env,
    )
}

pub fn env_keep(env: Value, mut namer: impl FnMut(Value) -> Option<Value>) -> Value {
    scm_fold(
        |acc, kv| {
            if let Some(name) = namer(kv.car()) {
                scm_cons(
                    Thread::current(),
                    scm_cons(Thread::current(), name, kv.cdr()),
                    acc,
                )
            } else {
                acc
            }
        },
        Value::encode_null_value(),
        env,
    )
}

pub fn env_keys(env: Value) -> Value {
    scm_map(Thread::current(), |kv| kv.car(), env)
}
