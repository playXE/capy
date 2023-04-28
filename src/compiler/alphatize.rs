//! Alphatize phase
//!
//! This phase is responsible for assigning a unique name to each variable.

use std::sync::atomic::{AtomicUsize, Ordering};

use crate::{
    compiler::desugar::{make_assignment, make_begin2, make_call2, make_if},
    runtime::{
        vm::VM,

        intern,
        list::{append_one, map, assq, reverse},
        value::ScmValue, 
    },
};

static ID: AtomicUsize = AtomicUsize::new(0);

pub fn alphatize_name(cx: &mut VM, orig: &str) -> ScmValue {
    cx.make_symbol(&format!("v{}|{}", ID.fetch_add(1, Ordering::Relaxed), orig), true)
}

pub fn alphatize(cx: &mut VM, expr: ScmValue) -> ScmValue {
    fn alphatize(cx: &mut VM, expr: ScmValue, replacements: ScmValue) -> ScmValue {
        if !expr.is_pair() {
            if expr.is_symbol() {
               
                let result = assq(cx, expr, replacements);

                if result.is_false() {
                    expr
                } else {
                    result.cdr()
                }
            } else {
                expr
            }
        } else {
            if !expr.car().is_symbol() {
                return alphatize_call(cx, expr, replacements);
            }
           
            match expr.car().strvalue() {
                "begin" => {
                    let body = alphatize_body(cx, expr.cdr(), replacements);

                    make_begin2(cx, body)
                }

                "if" => alphatize_if(cx, expr.cdr(), replacements),

                "set!" => alphatize_set(cx, expr.cdr(), replacements),

                "lambda" => alphatize_lambda(cx, expr.cdr(), replacements),

                "named-lambda" => alphatize_named_lambda(cx, expr.cdr(), replacements),

                "quote" => expr,

                "#%call" => alphatize_call(cx, expr.cdr(), replacements),

                "let" => alphatize_let(cx, expr.cdr(), replacements),

                _ => alphatize_call(cx, expr, replacements),
            }
        }
    }

    fn alphatize_let(cx: &mut VM, expr: ScmValue, replacements: ScmValue) -> ScmValue {
        let mut bindings = expr.car();
        let body = expr.cdr();

        let mut new_bindings = ScmValue::encode_null_value();
        let mut new_replacements = replacements;


        while bindings.is_pair() {
            let binding = bindings.car();
            let var = binding.car();
            let value = binding.cdr().car();

            let new_var = alphatize_name(cx, var.strvalue());
            let kv = cx.make_pair(var, new_var);
            let value = alphatize(cx, value, replacements);
            let value = cx.make_pair(value, ScmValue::encode_null_value());
            new_replacements = cx.make_pair(kv, new_replacements);
            let kv = cx.make_pair(new_var, value);
            new_bindings = cx.make_pair(kv, new_bindings);

            bindings = bindings.cdr();
        }

        let new_bindings = reverse(cx, new_bindings);
        let new_body = alphatize_body(cx, body, new_replacements);

        let nlet = intern("let");
        let bindings_and_body = cx.make_pair(new_bindings, new_body);

        cx.make_pair(nlet, bindings_and_body)
    }

    fn alphatize_call(cx: &mut VM, expr: ScmValue, replacements: ScmValue) -> ScmValue {

        let proc = alphatize(cx, expr.car(), replacements);
        let args = map(cx, expr.cdr(), |cx, arg| alphatize(cx, arg, replacements));

        make_call2(cx, proc, args)
    }

    fn alphatize_if(cx: &mut VM, expr: ScmValue, replacements: ScmValue) -> ScmValue {
        let test = alphatize(cx, expr.car(), replacements);
        let consequent = alphatize(cx, expr.cdr().car(), replacements);
        let alternative = alphatize(cx, expr.cdr().cdr().car(), replacements);

        make_if(cx, test, consequent, alternative)
    }

    fn alphatize_set(cx: &mut VM, expr: ScmValue, replacements: ScmValue) -> ScmValue {
        assert!(
            expr.car().is_symbol(),
            "not a symbol: {:?}",
            expr.get_type()
        );
        let var = alphatize(cx, expr.car(), replacements);
        assert!(var.is_symbol());
        let value = alphatize(cx, expr.cdr().car(), replacements);

        make_assignment(cx, var, value)
    }

    // (lambda <args> <body>)
    fn alphatize_lambda(cx: &mut VM, expr: ScmValue, replacements: ScmValue) -> ScmValue {
        let mut args = expr.car();
        let body = expr.cdr();

        let mut new_args = ScmValue::encode_null_value();
        let mut new_replacements = ScmValue::encode_null_value();

        while args.is_pair() {
            let arg = args.car();
            let new_arg = alphatize_name(cx, arg.strvalue());
            let kv = cx.make_pair(arg, new_arg);
            new_args = cx.make_pair(new_arg, new_args);
            new_replacements = cx.make_pair(kv, new_replacements);

            args = args.cdr();
        }

        if !args.is_null() {
            let new_arg = alphatize_name(cx, args.strvalue());

            new_args = cx.make_pair(new_arg, new_args);
            new_replacements = cx.make_pair(args, new_replacements);
        }
        new_replacements = append_one(cx, replacements, new_replacements);
        let new_body = alphatize_body(cx, body, new_replacements);

        let nlambda = intern("lambda");
        let args_and_body = cx.make_pair(new_args, new_body);

        cx.make_pair(nlambda, args_and_body)
    }

    // (named-lambda <name> <args> <body>)
    fn alphatize_named_lambda(
        cx: &mut VM,
        expr: ScmValue,
        replacements: ScmValue,
    ) -> ScmValue {
        let name = expr.car();
        let mut args = expr.cdr().car();
        let body = expr.cdr().cdr();

        let mut new_args = ScmValue::encode_null_value();
        let mut new_replacements = ScmValue::encode_null_value();

        while args.is_pair() {
            let arg = args.car();
            let new_arg = alphatize_name(cx, arg.strvalue());

            new_args = cx.make_pair(new_arg, new_args);
            let kv = cx.make_pair(arg, new_arg);
            new_replacements = cx.make_pair(kv, new_replacements);

            args = args.cdr();
        }

        if !args.is_null() {
            let new_arg = alphatize_name(cx, args.strvalue());

            new_args = cx.make_pair(new_arg, new_args);
            new_replacements = cx.make_pair(args, new_replacements);
        }
        new_replacements = append_one(cx, replacements, new_replacements);
        let new_body = alphatize_body(cx, body, new_replacements);

        let nlambda = intern("named-lambda");
        let args_and_body = cx.make_pair(new_args, new_body);
        let name_and_args_and_body = cx.make_pair(name, args_and_body);

        cx.make_pair(nlambda, name_and_args_and_body)
    }

    fn alphatize_body(cx: &mut VM, expr: ScmValue, replacements: ScmValue) -> ScmValue {
        if expr.is_null() {
            ScmValue::encode_null_value()
        } else {
            let first = alphatize(cx, expr.car(), replacements);
            let rest = alphatize_body(cx, expr.cdr(), replacements);

            cx.make_pair(first, rest)
        }
    }

    alphatize(cx, expr, ScmValue::encode_null_value())
}
