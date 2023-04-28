use crate::runtime::{
    vm::VM,
    intern,
    list::{append_one, map, memq, proper_list_length, reverse},
    value::ScmValue,
    Runtime, 
};
use num::bigint::Sign;

use r7rs_parser::expr::{Expr, NoIntern};

pub fn expr_to_value(cx: &mut VM, expr: &Expr<NoIntern>) -> ScmValue {
    match expr {
        Expr::BigInt(x) => cx.make_bigint(x.sign() == Sign::Minus, x.to_u32_digits().1),
        Expr::Bool(x) => ScmValue::encode_bool_value(*x),
        Expr::ByteVector(x) => cx.make_bytevector_from(x),
        Expr::Char(x) => cx.make_char(*x),
        Expr::Float(x) => cx.make_float(*x),
        Expr::Fixnum(x) => cx.make_int(*x as i64),
        Expr::Symbol(x) => Runtime::get().unwrap().intern(x).into(),
        Expr::Str(x) => cx.make_string(x),
        Expr::Null => ScmValue::encode_null_value(),
        Expr::Syntax(_, x) => expr_to_value(cx, x),
        Expr::ImmutableVector(x) | Expr::GrowableVector(x) => {
            let vector = cx.make_vector(x.len() as u32, ScmValue::encode_null_value());

            for (i, expr) in x.iter().enumerate() {
                let value = expr_to_value(cx, expr);
                cx.mutator.write_barrier(vector.get_vector());
                vector.vector_set(i as usize, value);
            }

            vector
        }

        Expr::Pair(car, cdr) => {
            let car = expr_to_value(cx, car);
            let cdr = expr_to_value(cx, cdr);

            cx.make_pair(car, cdr)
        }

        _ => todo!(),
    }
}

/// Desugars top level expressions into a list of definitions and a list of expressions.
/// Flattens top level `begin` expressions.
pub fn desugar(cx: &mut VM, expr: ScmValue) -> Result<(ScmValue, ScmValue), ScmValue> {
    fn desugar_definitions(
        cx: &mut VM,
        exp: ScmValue,
        defs: &mut ScmValue,
    ) -> Result<ScmValue, ScmValue> {
        // FIXME: Implement fmt::Display for ScmValue and print the expression in error
        fn redefinition(cx: &mut VM, id: ScmValue, defs: ScmValue) -> Result<(), ScmValue> {
            if id.is_symbol() {
                if memq(cx, id, defs).to_boolean() {
                    eprintln!("redefinition of {}", id.strvalue());
                }

                Ok(())
            } else {
                let err = cx.make_string(&format!("Malformed definition: {}", id));

                Err(err)
            }
        }

        fn desugar_define(
            cx: &mut VM,
            exp: ScmValue,
            defs: &mut ScmValue,
        ) -> Result<ScmValue, ScmValue> {
            if exp.cdr().is_null() {
                return Err(cx.make_string("Malformed definition"));
            } else if exp.cddr().is_null() {
                redefinition(cx, exp.cadr(), *defs)?;
                *defs = cx.make_pair(exp.cadr(), *defs);

                let void = make_constant(cx, ScmValue::encode_undefined_value());
                let global = make_constant(cx, exp.cadr());
                let assignment = make_assignment(cx, exp.cadr(), void);
                let begin = make_begin(cx, &[assignment, global]);

                Ok(begin)
            } else if exp.cadr().is_pair() {
                let def = exp.car();
                let pattern = exp.cadr();
                let f = pattern.car();
                let args = pattern.cdr();
                let body = exp.cddr();

                let args_and_body = cx.make_pair(args, body);
                let f_and_args_and_body = cx.make_pair(f, args_and_body);
                let named_lambda = cx.make_pair(intern("named-lambda"), f_and_args_and_body);
                let named_lambda = cx.make_pair(named_lambda, ScmValue::encode_null_value());
                let f_and_lambda = cx.make_pair(f, named_lambda);

                let new = cx.make_pair(def, f_and_lambda);

                desugar_define(cx, new, defs)
            } else {
                redefinition(cx, exp.cadr(), *defs)?;

                *defs = cx.make_pair(exp.cadr(), *defs);
                let desugared = desugar(cx, exp.caddr())?;
                let assignment = make_assignment(cx, exp.cadr(), desugared);
                let void = make_constant(cx, ScmValue::encode_undefined_value());

                Ok(make_begin(cx, &[assignment, void]))
            }
        }

        fn define_loop(
            cx: &mut VM,
            exp: ScmValue,
            rest: ScmValue,
            first: ScmValue,
            defs: &mut ScmValue,
        ) -> Result<ScmValue, ScmValue> {
            if exp.is_pair() && exp.car().is_symbol_of("begin") && exp.cdr().is_pair() {
                let newrest = append_one(cx, rest, exp.cddr());

                define_loop(cx, exp.cadr(), newrest, first, defs)
            } else if exp.is_pair() && exp.car().is_symbol_of("define") {
                let exp = desugar_define(cx, exp, defs)?;
                println!("desugared define");
                if first.is_null() && rest.is_null() {
                    return Ok(exp);
                } else if rest.is_null() {
                    let exp_and_first = cx.make_pair(exp, first);
                    let reverse = reverse(cx, exp_and_first);

                    return Ok(make_begin2(cx, reverse));
                } else {
                    let exp_and_first = cx.make_pair(exp, first);

                    define_loop(cx, rest.car(), rest.cdr(), exp_and_first, defs)
                }
            } else if rest.is_null() && first.is_null() {
                return desugar(cx, exp);
            } else if rest.is_null() {
                let exp = desugar(cx, exp)?;
                let exp_and_first = cx.make_pair(exp, first);
                let reverse = reverse(cx, exp_and_first);

                return Ok(make_begin2(cx, reverse));
            } else {
                let exp = desugar(cx, exp)?;
                let exp_and_first = cx.make_pair(exp, first);

                define_loop(cx, rest.car(), rest.cdr(), exp_and_first, defs)
            }
        }

        define_loop(
            cx,
            exp,
            ScmValue::encode_null_value(),
            ScmValue::encode_null_value(),
            defs,
        )
    }

    fn desugar(cx: &mut VM, expr: ScmValue) -> Result<ScmValue, ScmValue> {
        if !expr.is_pair() {
            if !expr.is_symbol() {
                return Ok(make_constant(cx, expr));
            } else {
                return Ok(expr);
            }
        } else if !expr.car().is_symbol() {
            return desugar_application(cx, expr);
        } else {
            match expr.car().strvalue() {
                "#%call" => desugar_application(cx, expr.cdr()),
                "begin" => desugar_begin(cx, expr),
                "define" => return Err(cx.make_string("define not allowed in this context")),
                "if" => desugar_if(cx, expr),
                "lambda" => desugar_lambda(cx, expr, false),
                "named-lambda" => desugar_lambda(cx, expr, true),
                "set!" => desugar_set(cx, expr),
                "let" => desugar_let(cx, expr),
                "quote" => Ok(expr),
                _ => desugar_application(cx, expr),
            }
        }
    }

    fn desugar_let(cx: &mut VM, expr: ScmValue) -> Result<ScmValue, ScmValue> {
        let bindings = expr.cadr();
        let body = expr.cddr();

        let mut vars = ScmValue::encode_null_value();

        let mut bindings = bindings;

        while !bindings.is_null() {
            let binding = bindings.car();
            let var = binding.car();
            let val = desugar(cx, binding.cadr())?;
            let val = cx.make_pair(val, ScmValue::encode_null_value());
            let binding = cx.make_pair(var, val);

            vars = cx.make_pair(binding, vars);
            bindings = bindings.cdr();
        }

        if !bindings.is_null() {
            return Err(cx.make_string("Malformed let"));
        }

        let body = desugar_body(cx, body)?;
        let body = cx.make_pair(body, ScmValue::encode_null_value());
        let vars = reverse(cx, vars);

        let vars_and_body = cx.make_pair(vars, body);
        let let_ = cx.make_pair(intern("let"), vars_and_body);
   
        Ok(let_)
    }

    fn desugar_application(cx: &mut VM, expr: ScmValue) -> Result<ScmValue, ScmValue> {
        if let Some(_) = proper_list_length(expr) {
            let proc = desugar(cx, expr.car())?;
            let mut args = expr.cdr();
            let mut newargs = ScmValue::encode_null_value();

            while !args.is_null() {
                let arg = desugar(cx, args.car())?;
                newargs = cx.make_pair(arg, newargs);
                args = args.cdr();
            }
            let newargs = reverse(cx, newargs);
            Ok(make_call2(cx, proc, newargs))
        } else {
            Err(cx.make_string("Malformed application"))
        }
    }

    fn desugar_begin(cx: &mut VM, expr: ScmValue) -> Result<ScmValue, ScmValue> {
        if let Some(l) = proper_list_length(expr).filter(|x| *x >= 1) {
            let mut err = None;

            if l > 1 {
                let newexpr = map(cx, expr, |cx, x| {
                    if err.is_some() {
                        ScmValue::encode_undefined_value()
                    } else {
                        match desugar(cx, x) {
                            Ok(x) => x,
                            Err(e) => {
                                err = Some(e);
                                ScmValue::encode_undefined_value()
                            }
                        }
                    }
                });

                if let Some(err) = err {
                    return Err(err);
                } else {
                    Ok(make_begin2(cx, newexpr))
                }
            } else {
                Ok(make_constant(cx, ScmValue::encode_undefined_value()))
            }
        } else {
            Err(cx.make_string("Malformed begin"))
        }
    }

    fn desugar_body(cx: &mut VM, exprs: ScmValue) -> Result<ScmValue, ScmValue> {
        fn rec(cx: &mut VM, body: ScmValue, defs: ScmValue) -> Result<ScmValue, ScmValue> {
            if body.is_null() {
                return Err(cx.make_string("empty body"));
            }

            let exp = body.car();

            if exp.is_pair() && exp.car().is_symbol_of("begin") {
                let newbody = append_one(cx, body.cdr(), exp.cdr());

                rec(cx, newbody, defs)
            } else if exp.is_pair() && exp.car().is_symbol_of("define") {
                let newdefs = cx.make_pair(exp, defs);

                rec(cx, body.cdr(), newdefs)
            } else {
                finalize_body(cx, body, defs)
            }
        }

        rec(cx, exprs, ScmValue::encode_null_value())
    }

    fn desugar_set(cx: &mut VM, expr: ScmValue) -> Result<ScmValue, ScmValue> {
        if let Some(_) = proper_list_length(expr).filter(|x| *x == 3) {
            let id = expr.cadr();
            let value = desugar(cx, expr.caddr())?;

            Ok(make_assignment(cx, id, value))
        } else {
            Err(cx.make_string("Malformed set!"))
        }
    }

    fn desugar_if(cx: &mut VM, expr: ScmValue) -> Result<ScmValue, ScmValue> {
        if let Some(_) = proper_list_length(expr).filter(|x| *x == 3 || *x == 4) {
            let test = desugar(cx, expr.cadr())?;
            let conseq = desugar(cx, expr.caddr())?;
            let alt = if expr.cdddr().is_null() {
                make_constant(cx, ScmValue::encode_undefined_value())
            } else {
                desugar(cx, expr.cdddr().car())?
            };

            Ok(make_if(cx, test, conseq, alt))
        } else {
            Err(cx.make_string("Malformed if"))
        }
    }

    fn desugar_lambda(cx: &mut VM, expr: ScmValue, named: bool) -> Result<ScmValue, ScmValue> {
        if let Some(_) = proper_list_length(expr).filter(|x| *x >= 2) {
            let name = if named {
                expr.cadr()
            } else {
                ScmValue::encode_undefined_value()
            };

            let args = if named { expr.caddr() } else { expr.cadr() };

            let body = if named { expr.cdddr() } else { expr.cddr() };

            let body = desugar_body(cx, body)?;

            Ok(make_lambda(cx, name, args, body))
        } else {
            Err(cx.make_string("Malformed lambda"))
        }
    }

    fn finalize_body(
        cx: &mut VM,
        body: ScmValue,
        defs: ScmValue,
    ) -> Result<ScmValue, ScmValue> {
        if defs.is_null() {
            let mut err = None;

            let newbody = map(cx, body, |cx, x| {
                if err.is_some() {
                    ScmValue::encode_undefined_value()
                } else {
                    match desugar(cx, x) {
                        Ok(x) => x,
                        Err(e) => {
                            err = Some(e);
                            ScmValue::encode_undefined_value()
                        }
                    }
                }
            });

            if let Some(err) = err {
                return Err(err);
            } else {
                if newbody.cdr().is_null() {
                    Ok(newbody.car())
                } else {
                    Ok(make_begin2(cx, newbody))
                }
            }
        } else {
            // FIXME: Is sort necessary?
            /*fn sort_defs(cx: &mut Context, defs: ScmValue) -> ScmValue {

            }*/

            fn desugar_definition(cx: &mut VM, def: ScmValue) -> Result<ScmValue, ScmValue> {
                if let Some(l) = proper_list_length(def).filter(|x| *x > 2) {
                    if def.cadr().is_pair() {
                        let args = def.cadr().cdr();
                        let body = def.cddr();
                        let args_body = cx.make_pair(args, body);
                        let ls = cx.make_pair(intern("lambda"), args_body);
                        let ls = cx.make_list(&[def.car(), def.cadr().car(), ls]);

                        desugar_definition(cx, ls)
                    } else {
                        if l == 3 && def.cadr().is_symbol() {
                            Ok(def.cdr())
                        } else {
                            Err(cx.make_string("Malformed definition"))
                        }
                    }
                } else {
                    Err(cx.make_string("Malformed definition"))
                }
            }

            fn expand_letrec_star(
                cx: &mut VM,
                bindings: ScmValue,
                body: ScmValue,
            ) -> Result<ScmValue, ScmValue> {
                let args = map(cx, bindings, |_cx, x| x.car());

                let mut lambda = cx.make_list(&[intern("lambda"), args]);

                let assignments = map(cx, bindings, |cx, binding| {
                    make_assignment(cx, binding.car(), binding.cadr())
                });

                lambda = append_one(cx, lambda, assignments);
                lambda = append_one(cx, lambda, body);
                lambda = desugar(cx, lambda)?;

                let args = map(cx, bindings, |cx, _| {
                    make_constant(cx, ScmValue::encode_undefined_value())
                });

                Ok(make_call2(cx, lambda, args))
            }

            let mut err = None;

            let desugared_defs = map(cx, defs, |cx, x| match desugar_definition(cx, x) {
                Ok(x) => x,
                Err(e) => {
                    err = Some(e);
                    ScmValue::encode_undefined_value()
                }
            });

            expand_letrec_star(cx, desugared_defs, body)
        }
    }

    let mut defs = ScmValue::encode_null_value();

    let toplevel = desugar_definitions(cx, expr, &mut defs)?;

    Ok((toplevel, defs))
}

pub fn make_begin(cx: &mut VM, exprs: &[ScmValue]) -> ScmValue {
    if exprs.len() == 1 {
        exprs[0]
    } else {
        let list = cx.make_list(exprs);
        cx.make_pair(intern("begin"), list)
    }
}

pub fn make_begin2(cx: &mut VM, ls: ScmValue) -> ScmValue {
    if ls.is_null() {
        ScmValue::encode_undefined_value()
    } else if ls.cdr().is_null() {
        ls.car()
    } else {
        cx.make_pair(intern("begin"), ls)
    }
}

pub fn make_assignment(cx: &mut VM, id: ScmValue, value: ScmValue) -> ScmValue {
    let val = cx.make_pair(value, ScmValue::encode_null_value());
    let id = cx.make_pair(id, val);
    cx.make_pair(intern("set!"), id)
}

pub fn make_constant(cx: &mut VM, value: ScmValue) -> ScmValue {
    let value = cx.make_pair(value, ScmValue::encode_null_value());
    cx.make_pair(intern("quote"), value)
}

pub fn make_call(cx: &mut VM, callee: ScmValue, args: &[ScmValue]) -> ScmValue {
    let args = cx.make_list(args);
    let callee_args = cx.make_pair(callee, args);
    cx.make_pair(intern("#%call"), callee_args)
}

pub fn make_call2(cx: &mut VM, callee: ScmValue, args: ScmValue) -> ScmValue {
    let callee_args = cx.make_pair(callee, args);
    cx.make_pair(intern("#%call"), callee_args)
}

pub fn make_if(cx: &mut VM, test: ScmValue, conseq: ScmValue, alt: ScmValue) -> ScmValue {
    let alt = cx.make_pair(alt, ScmValue::encode_null_value());
    let conseq = cx.make_pair(conseq, alt);
    let test = cx.make_pair(test, conseq);
    cx.make_pair(intern("if"), test)
}

pub fn make_lambda(cx: &mut VM, name: ScmValue, args: ScmValue, body: ScmValue) -> ScmValue {
    let body = cx.make_pair(body, ScmValue::encode_null_value());
    let args = cx.make_pair(args, body);
    if name.is_symbol() {
        let name_args = cx.make_pair(name, args);
        cx.make_pair(intern("named-lambda"), name_args)
    } else {
        cx.make_pair(intern("lambda"), args)
    }
}
