use std::collections::hash_map::RandomState;

use crate::{
    compiler::{Compiler, LocalEnv},
    data::{
        exception::{Exception, SourcePosition},
        special_form::Form,
    },
    prelude::{code::Ins, eval_error::EvalError, *},
    utilities::arraylist::ArrayList,
};

pub(crate) fn control_flow(_: &mut Runtime) {
    let manager = library_manager();
    let thr = Thread::current();

    let keywords = manager.keyword_module.get_handle_of::<Library>();

    manager.add_definition(
        thr,
        keywords,
        ("begin", Form::Primitive(compile_begin)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        keywords,
        ("let", Form::Primitive(compile_let)),
        true,
        true,
    );
}

pub fn compile_begin(cc: &mut Compiler, ctx: &mut Context, form: Value, tail: bool) -> bool {
    if form.is_pair() {
        let exprs = form.cdr();

        cc.compile_seq(ctx, exprs, tail, false, None)
    } else {
        let exc = Exception::eval(
            ctx,
            EvalError::MalformedArgumentList,
            &[form],
            SourcePosition::unknown(),
        );
        ctx.error(exc);
    }
}

#[allow(dead_code)]
fn split_bindings(ctx: &mut Context, bindings_list: Value) -> (Value, Value) {
    let mut symbols = ArrayList::new(ctx.mutator());
    let mut exprs = ArrayList::new(ctx.mutator());

    let mut bindings = bindings_list;

    while bindings.is_pair() {
        let binding = bindings.car();
        let rest = bindings.cdr();

        if binding.is_pair()
            && binding.car().is_symbol()
            && binding.cdr().is_pair()
            && binding.cdr().cdr().is_null()
        {
            let sym = binding.car().get_symbol();
            let expr = binding.cdr().car();
            symbols.push(ctx.mutator(), Value::new(sym));
            exprs.push(ctx.mutator(), expr);
        } else {
            let exc = Exception::eval(
                ctx,
                EvalError::MalformedBinding,
                &[binding, bindings_list],
                SourcePosition::unknown(),
            );

            ctx.error(exc);
        }

        bindings = rest;
    }

    let fst = Value::make_list_arraylist(ctx, &symbols, Value::nil());
    let snd = Value::make_list_arraylist(ctx, &exprs, Value::nil());

    (fst, snd)
}

pub fn compile_let(cc: &mut Compiler, ctx: &mut Context, form: Value, tail: bool) -> bool {
    if form.is_pair() {
        let rest = form.cdr();
        if rest.is_pair() {
            let first = rest.car();
            let body = rest.cdr();

            let initial_locals = cc.num_locals;
            let res;
            if first.is_null() {
                return cc.compile_seq(ctx, body, tail, true, None);
            } else if first.is_pair() {
                let lenv = cc.env;
                let group = cc.compile_bindings(ctx, first, lenv, true, false, false);

                let old = cc.env;
                cc.env = group;
                res = cc.compile_seq(ctx, body, tail, false, None);
                cc.env = old;
            } else if first.is_symbol() {
                let sym = first.get_symbol();
                if body.is_pair() {
                    let bindings = body.car();
                    let rest = body.cdr();
                    let (params, exprs) = split_bindings(ctx, bindings);

                    let group = LocalEnv {
                        vars: HashMap::with_hasher(RandomState::new()),
                        captures: cc.captures,
                        cc: cc as *const Compiler,
                        parent: Some(cc.env),
                    };
                    let mut env = ctx
                        .mutator()
                        .allocate(crate::compiler::Environment::Local(group));

                    let index = env.add_binding(ctx, sym, || cc.next_local_index());
                    cc.emit(ctx, Ins::PushUndef);
                    cc.emit(ctx, Ins::MakeLocalVariable(index as _));

                    let name_idx = cc.add_constant(ctx, first);

                    let old = cc.env;
                    cc.env = env;
                    println!("let in {:p}", env);
                    cc.compile_lambda(
                        ctx,
                        Some(name_idx),
                        params,
                        rest,
                        false,
                        false,
                        false,
                        false,
                    );
                    cc.env = old;
                    cc.emit(ctx, Ins::SetLocal(index as _));

                    let push_frame_ip = cc.emit(ctx, Ins::MakeFrame);
                    cc.emit(ctx, Ins::PushLocal(index as _));
                    let old = cc.env;
                    cc.env = env;
                    let exprs = cc.compile_exprs(ctx, exprs);
                    cc.env = old;
                    if cc.call(ctx, exprs, tail) {
                        cc.code[push_frame_ip] = Ins::NoOp;
                        res = true;
                    } else {
                        res = false;
                    }

                } else {
                    let exc = Exception::argument_count(
                        ctx,
                        Some("let"),
                        2,
                        2,
                        form,
                        SourcePosition::unknown(),
                    );
                    ctx.error(exc);
                }
            } else {
                todo!()
            }
            if !res && cc.num_locals > initial_locals {
                cc.emit(
                    ctx,
                    Ins::Reset(
                        initial_locals as _,
                        cc.num_locals as u16 - initial_locals as u16,
                    ),
                );
            }
            cc.num_locals = initial_locals;
            return res;
        }
    }

    let exc = Exception::argument_count(ctx, Some("let"), 1, 1, form, SourcePosition::unknown());
    ctx.error(exc);
}
