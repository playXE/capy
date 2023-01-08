use std::collections::hash_map::RandomState;

use crate::{
    compiler::{Compiler, Environment, LocalEnv},
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

    manager.add_definition(
        thr,
        keywords,
        ("let*", Form::Primitive(compile_letstar)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        keywords,
        ("letrec", Form::Primitive(compile_letrec)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        keywords,
        ("letrec*", Form::Primitive(compile_letrecstar)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        keywords,
        ("let-values", Form::Primitive(compile_let_values)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        keywords,
        ("let*-values", Form::Primitive(compile_letstar_values)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        keywords,
        ("letrec-values", Form::Primitive(compile_letrec_values)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        keywords,
        ("if", Form::Primitive(compile_if)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        keywords,
        ("when", Form::Primitive(compile_when)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        keywords,
        ("unless", Form::Primitive(compile_unless)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        keywords,
        ("do", Form::Primitive(compile_do)),
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

pub fn compile_letstar(cc: &mut Compiler, ctx: &mut Context, form: Value, tail: bool) -> bool {
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
                let group = cc.compile_bindings(ctx, first, lenv, false, false, false);

                let old = cc.env;
                cc.env = group;
                res = cc.compile_seq(ctx, body, tail, false, None);
                cc.env = old;
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

    let exc = Exception::argument_count(ctx, Some("let*"), 1, 1, form, SourcePosition::unknown());
    ctx.error(exc);
}

pub fn compile_letrec(cc: &mut Compiler, ctx: &mut Context, form: Value, tail: bool) -> bool {
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
                let group = cc.compile_bindings(ctx, first, lenv, true, true, true);

                let old = cc.env;
                cc.env = group;
                res = cc.compile_seq(ctx, body, tail, false, None);
                cc.env = old;
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

    let exc = Exception::argument_count(ctx, Some("letrec"), 1, 1, form, SourcePosition::unknown());
    ctx.error(exc);
}

pub fn compile_letrecstar(cc: &mut Compiler, ctx: &mut Context, form: Value, tail: bool) -> bool {
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
                let group = cc.compile_bindings(ctx, first, lenv, true, true, false);

                let old = cc.env;
                cc.env = group;
                res = cc.compile_seq(ctx, body, tail, false, None);
                cc.env = old;
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

    let exc =
        Exception::argument_count(ctx, Some("letrec*"), 1, 1, form, SourcePosition::unknown());
    ctx.error(exc);
}

pub fn compile_let_values(cc: &mut Compiler, ctx: &mut Context, form: Value, tail: bool) -> bool {
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
                let group = cc.compile_multi_bindings(ctx, first, lenv, true, false);

                let old = cc.env;
                cc.env = group;
                res = cc.compile_seq(ctx, body, tail, false, None);
                cc.env = old;
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

    let exc = Exception::argument_count(
        ctx,
        Some("let-values"),
        1,
        1,
        form,
        SourcePosition::unknown(),
    );
    ctx.error(exc);
}

pub fn compile_letstar_values(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    tail: bool,
) -> bool {
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
                let group = cc.compile_multi_bindings(ctx, first, lenv, false, false);

                let old = cc.env;
                cc.env = group;
                res = cc.compile_seq(ctx, body, tail, false, None);
                cc.env = old;
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

    let exc = Exception::argument_count(
        ctx,
        Some("let*-values"),
        1,
        1,
        form,
        SourcePosition::unknown(),
    );
    ctx.error(exc);
}

pub fn compile_letrec_values(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    tail: bool,
) -> bool {
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
                let group = cc.compile_multi_bindings(ctx, first, lenv, false, true);

                let old = cc.env;
                cc.env = group;
                res = cc.compile_seq(ctx, body, tail, false, None);
                cc.env = old;
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

    let exc = Exception::argument_count(
        ctx,
        Some("letrec-values"),
        1,
        1,
        form,
        SourcePosition::unknown(),
    );
    ctx.error(exc);
}

pub fn compile_if(cc: &mut Compiler, ctx: &mut Context, form: Value, tail: bool) -> bool {
    if form.is_pair() && form.cdr().is_pair() {
        let cond = form.cdr().car();
        let rest = form.cdr().cdr();
        if rest.is_pair() {
            let thenp = rest.car();
            let alternativep = rest.cdr();

            let elsep = if alternativep.is_pair() {
                alternativep.car()
            } else {
                Value::nil()
            };

            cc.compile(ctx, cond, false);
            let else_jump_ip = cc.emit(ctx, Ins::Branch(0));

            if cc.compile(ctx, elsep, tail) {
                cc.code[else_jump_ip] = Ins::BranchIf(cc.offset_to_next(else_jump_ip as _) as _);
                return cc.compile(ctx, thenp, true);
            }

            let exit_jump_ip = cc.emit(ctx, Ins::Branch(0));
            cc.code[else_jump_ip] = Ins::BranchIf(cc.offset_to_next(else_jump_ip as _) as _);
            if cc.compile(ctx, thenp, tail) {
                cc.code[exit_jump_ip] = Ins::Return;
                return true;
            }

            cc.code[exit_jump_ip] = Ins::Branch(cc.offset_to_next(exit_jump_ip as _) as _);
            return false;
        }
    }
    let exc = Exception::argument_count(
        ctx,
        Some("if"),
        2,
        2,
        Value::nil(),
        SourcePosition::unknown(),
    );
    ctx.error(exc);
}

pub fn compile_unless(cc: &mut Compiler, ctx: &mut Context, form: Value, tail: bool) -> bool {
    if form.is_pair() && form.cdr().is_pair() {
        let cond = form.car().car();
        let exprs = form.car().cdr();

        cc.compile(ctx, cond, false);
        let else_jump_ip = cc.emit(ctx, Ins::Branch(0));
        cc.emit(ctx, Ins::PushVoid);
        let exit_jump_ip = cc.emit(ctx, Ins::Branch(0));
        cc.code[else_jump_ip] = Ins::BranchIfNot(cc.offset_to_next(else_jump_ip as _) as _);
        if cc.compile_seq(ctx, exprs, tail, false, None) {
            cc.code[exit_jump_ip] = Ins::Return;
            return true;
        }

        cc.code[exit_jump_ip] = Ins::Branch(cc.offset_to_next(exit_jump_ip as _) as _);
        return false;
    }
    let exc = Exception::argument_count(
        ctx,
        Some("unless"),
        1,
        usize::MAX,
        Value::nil(),
        SourcePosition::unknown(),
    );
    ctx.error(exc);
}

pub fn compile_when(cc: &mut Compiler, ctx: &mut Context, form: Value, tail: bool) -> bool {
    if form.is_pair() && form.cdr().is_pair() {
        let cond = form.car().car();
        let exprs = form.car().cdr();

        cc.compile(ctx, cond, false);
        let else_jump_ip = cc.emit(ctx, Ins::Branch(0));
        cc.emit(ctx, Ins::PushVoid);
        let exit_jump_ip = cc.emit(ctx, Ins::Branch(0));
        cc.code[else_jump_ip] = Ins::BranchIf(cc.offset_to_next(else_jump_ip as _) as _);
        if cc.compile_seq(ctx, exprs, tail, false, None) {
            cc.code[exit_jump_ip] = Ins::Return;
            return true;
        }

        cc.code[exit_jump_ip] = Ins::Branch(cc.offset_to_next(exit_jump_ip as _) as _);
        return false;
    }
    let exc = Exception::argument_count(
        ctx,
        Some("unless"),
        1,
        usize::MAX,
        Value::nil(),
        SourcePosition::unknown(),
    );
    ctx.error(exc);
}

pub fn compile_do(cc: &mut Compiler, ctx: &mut Context, form: Value, tail: bool) -> bool {
    if form.is_pair() && form.cdr().is_pair() {
        let binding_list = form.cdr().car();
        if form.cdr().cdr().is_pair() {
            let exit = form.cdr().cdr().car();
            let body = form.cdr().cdar().cdr();

            if exit.is_pair() {
                let test = exit.car();
                let terminal = exit.cdr();

                let initial_locals = cc.num_locals;

                let group = LocalEnv {
                    parent: Some(cc.env),
                    captures: cc.captures,
                    vars: HashMap::with_hasher(RandomState::new()),
                    cc: cc as *const Compiler,
                };

                let mut group = ctx.mutator().allocate(Environment::Local(group));

                let mut bindings = binding_list;
                let mut prev_index = -1;
                let mut do_bindings = Vec::<usize>::new();
                let mut step_exprs = ArrayList::new(ctx.mutator());

                while bindings.is_pair() {
                    let binding = bindings.car();
                    let rest = bindings.cdr();

                    if binding.is_pair() && binding.car().is_symbol() && binding.cdr().is_pair() {
                        let sym = binding.car().get_symbol();
                        let start = binding.cdr().car();
                        let opt_step = binding.cdr().cdr();

                        cc.compile(ctx, start, false);

                        let index = group.add_binding(ctx, sym, || cc.next_local_index());

                        if index as isize > prev_index {
                            cc.emit(ctx, Ins::MakeLocalVariable(index as _));

                            if opt_step.is_null() {
                            } else if opt_step.is_pair() && opt_step.cdr().is_null() {
                                let step = opt_step.car();
                                do_bindings.push(index);
                                step_exprs.push(ctx.mutator(), step);
                            } else {
                                let exc = Exception::eval(
                                    ctx,
                                    EvalError::DuplicateBinding,
                                    &[binding, binding_list],
                                    SourcePosition::unknown(),
                                );
                                ctx.error(exc);
                            }

                            prev_index = index as isize;
                        } else {
                            let exc = Exception::eval(
                                ctx,
                                EvalError::DuplicateBinding,
                                &[binding, binding_list],
                                SourcePosition::unknown(),
                            );
                            ctx.error(exc);
                        }
                    } else {
                        let exc = Exception::eval(
                            ctx,
                            EvalError::MalformedBinding,
                            &[binding, binding_list],
                            SourcePosition::unknown(),
                        );

                        ctx.error(exc);
                    }

                    bindings = rest;
                }

                if !bindings.is_null() {
                    let exc = Exception::eval(
                        ctx,
                        EvalError::MalformedBindings,
                        &[binding_list],
                        SourcePosition::unknown(),
                    );

                    ctx.error(exc);
                }

                let test_ip = cc.offset_to_next(0);
                let old = cc.env;
                cc.env = group;
                cc.compile(ctx, test, false);
                let exit_jump_ip = cc.emit(ctx, Ins::Branch(0));

                cc.compile_seq(ctx, body, false, false, None);
                cc.emit(ctx, Ins::Pop);

                for step in step_exprs.iter() {
                    cc.compile(ctx, *step, false);
                }

                for ix in do_bindings.iter().rev() {
                    cc.emit(ctx, Ins::SetLocal(*ix as u16));
                }

                cc.emit(ctx, Ins::Branch((-cc.offset_to_next(test_ip)) as _));
                cc.code[exit_jump_ip] = Ins::BranchIf(cc.offset_to_next(exit_jump_ip as _) as _);
                let res = cc.compile_seq(ctx, terminal, tail, false, None);
                if !res && cc.num_locals > initial_locals {
                    cc.emit(ctx, Ins::Reset(initial_locals as _, (cc.num_locals - initial_locals) as _));
                }
                cc.num_locals = initial_locals;
                cc.env = old;
            } else {
                let exc = Exception::eval(
                    ctx,
                    EvalError::MalformedTest,
                    &[exit],
                    SourcePosition::unknown(),
                );
                ctx.error(exc);
            }
        }
    }

    let exc = Exception::argument_count(
        ctx,
        Some("do"),
        2,
        usize::MAX,
        Value::nil(),
        SourcePosition::unknown(),
    );

    ctx.error(exc);
}
