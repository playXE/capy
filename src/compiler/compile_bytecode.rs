use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::collections::HashSet;

use crate::bytecode::opcodes::CAPY_BYTECODE_MAGIC;
use crate::bytecode::opcodes::OP_NOP;
use crate::bytecode::u24::u24;
use crate::bytecodeassembler::fasl::FASLPrinter;
use crate::bytecodeassembler::*;
use crate::compiler::sexpr::Sexpr;
use crate::runtime::object::TypeId;
use crate::runtime::symbol::scm_intern;
use crate::runtime::value::Value;
use crate::utils::is_aligned;

use super::tree_il::*;
use super::P;

pub fn scan<const RESET_CALL: bool>(
    iform: &P<IForm>,
    fs: &mut Vec<P<LVar>>,
    bs: &mut HashSet<P<LVar>>,
    toplevel: bool,
    labels: &mut Vec<P<IForm>>,
) {
    match &**iform {
        IForm::Define(def) => scan::<RESET_CALL>(&def.value, fs, bs, toplevel, labels),
        IForm::Label(label) => {
            scan::<RESET_CALL>(&label.body, fs, bs, toplevel, labels);
        }
        IForm::LRef(lref) => {
            if !bs.contains(&lref.lvar) && !fs.contains(&lref.lvar) {
                fs.push(lref.lvar.clone());
            }
        }

        IForm::LSet(lvar) => {
            let insert = !bs.contains(&lvar.lvar);
            scan::<RESET_CALL>(&lvar.value, fs, bs, toplevel, labels);
            if insert && !fs.contains(&lvar.lvar) {
                fs.push(lvar.lvar.clone());
            }
        }

        IForm::GSet(gset) => scan::<RESET_CALL>(&gset.value, fs, bs, toplevel, labels),
        IForm::If(cond) => {
            scan::<RESET_CALL>(&cond.cond, fs, bs, toplevel, labels);
            scan::<RESET_CALL>(&cond.consequent, fs, bs, toplevel, labels);
            scan::<RESET_CALL>(&cond.alternative, fs, bs, toplevel, labels);
        }
        IForm::Let(var) => {
            if var.typ == LetType::Rec {
                var.lvars.iter().for_each(|lvar| {
                    bs.insert(lvar.clone());
                });
            }

            var.inits.iter().for_each(|iform| {
                scan::<RESET_CALL>(iform, fs, bs, toplevel, labels);
            });

            if var.typ == LetType::Let {
                var.lvars.iter().for_each(|lvar| {
                    bs.insert(lvar.clone());
                });
            }
            scan::<RESET_CALL>(&var.body, fs, bs, toplevel, labels);
        }

        IForm::Call(_) => {
            let mut iform = iform.clone();
            let IForm::Call(ref mut call) = &mut *iform else {
                unreachable!()
            };
            if RESET_CALL {
                call.flag = CallFlag::None; // reset flag for pass2
            }
            scan::<RESET_CALL>(&call.proc, fs, bs, toplevel, labels);
            call.args.iter().for_each(|arg| {
                scan::<RESET_CALL>(arg, fs, bs, toplevel, labels);
            });
        }

        IForm::Seq(seq) => {
            seq.forms.iter().for_each(|iform| {
                scan::<RESET_CALL>(iform, fs, bs, toplevel, labels);
            });
        }

        IForm::Lambda(lam) => {
            let mut lbs = lam.lvars.iter().cloned().collect();
            let mut lfs = Vec::new();

            scan::<RESET_CALL>(&lam.body, &mut lfs, &mut lbs, false, labels);
            let mut lam = lam.clone();

            if lam.flag != LambdaFlag::Dissolved {
                labels.push(iform.clone());
                if false && toplevel {
                    lam.lifted_var = LiftedVar::Candidate;
                }
            }

            // add free-variables of `lam` to `fs` if they are
            // not bount in current scope
            for inner in lfs.iter() {
                if !bs.contains(inner) && !fs.contains(inner) {
                    fs.push(inner.clone());
                }
            }
            lam.free_lvars = lfs;
            lam.bound_lvars = lbs;
        }

        _ => (),
    }
}

pub fn scan_toplevel<const RESET_CALL: bool>(iform: P<IForm>) -> Vec<P<IForm>> {
    let mut fs = Vec::new();
    let mut bs = HashSet::new();
    let mut labels = Vec::new();
    scan::<RESET_CALL>(&iform, &mut fs, &mut bs, true, &mut labels);
    assert!(
        fs.len() == 0,
        "there should be no free variables in toplevel expressions"
    );
    labels
}

/// Compute a conservative count of how many stack slots will be
/// needed in the frame with for the lambda.
fn compute_frame_size(lam: P<Lambda>) -> usize {
    const TEMPORARY_COUNT: usize = 3;

    fn visit_args(args: &[P<IForm>]) -> usize {
        args.iter().map(|arg| visit(arg)).sum::<usize>()
    }

    fn visit(exp: &IForm) -> usize {
        match exp {
            IForm::LRef(_) | IForm::Const(_) => 1,
            IForm::Define(def) => 1 + visit(&def.value),

            IForm::LSet(lset) => 1 + visit(&lset.value),

            IForm::Call(call) => {
                1 + call.args.iter().map(|arg| visit(arg)).sum::<usize>() + 3 /* call frame size */
            }

            IForm::PrimCall(_, args) => args.iter().map(|arg| visit(arg)).sum::<usize>(),

            IForm::If(cond) => {
                let test = visit(&cond.cond);
                let consequent = visit(&cond.consequent);
                let alternative = visit(&cond.alternative);
                test.max(consequent).max(alternative)
            }

            IForm::Seq(seq) => seq.forms.iter().map(|form| visit(form)).max().unwrap_or(0),

            IForm::Let(var) => {
                let inits = visit_args(&var.inits);
                let body = visit(&var.body) + var.inits.len();
                inits.max(body)
            }

            IForm::Label(label) => visit(&label.body),

            _ => 1,
        }
    }

    1 + lam.lvars.len() + visit(&lam.body) + TEMPORARY_COUNT
}

struct Env {
    prev: Option<P<Self>>,
    name: Sexpr,
    id: P<LVar>,
    idx: u32,
    closure: bool,
    next_local: u32,
}

pub fn compile_closure(asm: &mut Assembler, lam: P<Lambda>, closures: &Vec<(P<Lambda>, usize)>) {
    fn lookup_lexical(sym: P<LVar>, mut env: &P<Env>) -> P<Env> {
        loop {
            if sym.as_ptr() == env.id.as_ptr() {
                return env.clone();
            }

            env = env.prev.as_ref().expect("symbol not found");
        }
    }

    fn push_free_var(sym: P<LVar>, idx: u32, env: P<Env>) -> P<Env> {
        P(Env {
            next_local: env.next_local,
            prev: Some(env),
            name: sym.name.clone(),
            id: sym.clone(),
            idx,
            closure: true,
        })
    }

    fn push_local(name: Sexpr, lvar: P<LVar>, env: P<Env>) -> P<Env> {
        let idx = env.next_local;

        P(Env {
            next_local: idx - 1,
            prev: Some(env),
            name,

            id: lvar,
            idx,
            closure: false,
        })
    }

    fn push_local_alias(name: Sexpr, lvar: P<LVar>, idx: u32, env: P<Env>) -> P<Env> {
        P(Env {
            next_local: env.next_local,
            prev: Some(env),
            name,

            id: lvar,
            idx,
            closure: false,
        })
    }

    fn push_temp(env: P<Env>) -> P<Env> {
        let idx = env.next_local;
        P(Env {
            next_local: idx - 1,
            prev: Some(env),
            name: Sexpr::Undefined,
            id: P(LVar {
                name: Sexpr::Undefined,
                initval: None,
                arg: false,
                boxed: false,
                ref_count: 0,
                set_count: 0,
            }),
            idx,
            closure: false,
        })
    }

    fn push_frame(mut env: P<Env>) -> P<Env> {
        for _ in 0..3 {
            env = push_temp(env);
        }
        env
    }

    fn push_closure(env: P<Env>) -> P<Env> {
        push_local(
            Sexpr::Symbol(scm_intern("closure")),
            P(LVar {
                name: Sexpr::Undefined,
                initval: None,
                arg: false,
                boxed: false,
                ref_count: 0,
                set_count: 0,
            }),
            env,
        )
    }

    fn create_initial_env(lvars: &[P<LVar>], free_lvars: &[P<LVar>], frame_size: u32) -> P<Env> {
        let frame_base = P(Env {
            next_local: frame_size - 1,
            prev: None,
            name: Sexpr::Undefined,
            id: P(LVar {
                name: Sexpr::Undefined,
                initval: None,
                arg: false,
                boxed: false,
                ref_count: 0,
                set_count: 0,
            }),
            idx: 0,
            closure: false,
        });

        let mut free_vars = frame_base;
        for (i, var) in free_lvars.iter().enumerate() {
            free_vars = push_free_var(var.clone(), i as _, free_vars);
        }

        let closure = push_closure(free_vars);

        let mut env = closure;
        for var in lvars.iter() {
            env = push_local(var.name.clone(), var.clone(), env);
            println!("local {} at {}", var.name, env.idx);
        }

        env
    }

    fn init_free_vars(
        asm: &mut Assembler,
        dst: u32,
        free_vars: &[P<LVar>],
        env: &P<Env>,
        tmp0: u32,
        state: &mut State,
    ) {
        let mut free_idx = 0;

        for var in free_vars.iter() {
            let loc = lookup_lexical(var.clone(), env);
            let idx = loc.idx;

            if loc.closure {
                asm.emit_load_free_variable(tmp0 as _, u24::new(state.frame_size - 1), idx);
                asm.emit_store_free_variable(u24::new(dst), tmp0 as _, free_idx);
            } else {
                asm.emit_store_free_variable(u24::new(dst), idx as _, free_idx);
            }
            free_idx += 1;
        }
    }
    #[derive(Clone, Copy)]
    #[allow(dead_code)]
    enum Context {
        Effect,
        Value,
        Tail,
        ValueAt(u32),
        ValuesAt(u32),
    }

    struct State<'a> {
        frame_size: u32,
        labels: HashMap<P<IForm>, usize>,
        closures: &'a Vec<(P<Lambda>, usize)>,
    }
    fn stack_height_under_local(idx: u32, frame_size: u32) -> u32 {
        frame_size - idx - 1
    }

    fn stack_height(env: &P<Env>, state: &State) -> u32 {
        stack_height_under_local(env.idx, state.frame_size)
    }

    fn visit_let(
        var: &Let,
        asm: &mut Assembler,
        env: &P<Env>,
        ctx: Context,
        state: &mut State,
    ) -> Option<P<Env>> {
        match var.typ {
            LetType::Let => {
                let mut env = env.clone();

                for (lvar, init) in var.lvars.iter().zip(var.inits.iter()) {
                    for_push(asm, init, &env, state);
                    env = push_local(lvar.name.clone(), lvar.clone(), env);
                }

                for_context(asm, ctx, &var.body, &env, state)
            }

            _ => {
                // letrec/letrec*
                //
                // push bindings and then compile each init expression

                let mut env = env.clone();

                for lvar in var.lvars.iter() {
                    env = push_local(lvar.name.clone(), lvar.clone(), env);
                }

                for (lvar, init) in var.lvars.iter().zip(var.inits.iter()) {
                    let dst = lookup_lexical(lvar.clone(), &env).idx;
                    for_value_at(asm, init, &env, dst, state); // assign init value to corresponding variable
                }

                for_context(asm, ctx, &var.body, &env, state)
            }
        }
    }

    fn visit_seq(seq: &Seq, asm: &mut Assembler, env: &P<Env>, ctx: Context, state: &mut State) {
        assert!(
            !seq.forms.is_empty(),
            "empty sequence during bytecode generation"
        );
        for i in 0..seq.forms.len() - 1 {
            for_effect(asm, &seq.forms[i], env, state);
        }

        for_context(asm, ctx, &seq.forms[seq.forms.len() - 1], env, state);
    }

    fn visit_if(cond: &If, asm: &mut Assembler, env: &P<Env>, ctx: Context, state: &mut State) {
        let value = for_value(asm, &cond.cond, env, state); // for_effect(asm, &cond.cond, env, state);
        let je = asm.emit_jnz(value.idx as _);
        for_context(asm, ctx, &cond.alternative, env, state);
        match ctx {
            Context::Tail => {
                je(asm);
                for_context(asm, ctx, &cond.consequent, env, state);
            }

            _ => {
                let jexit = asm.emit_j();
                je(asm);
                for_context(asm, ctx, &cond.consequent, env, state);
                jexit(asm);
            }
        }
    }

    fn for_value_at(
        asm: &mut Assembler,
        exp: &P<IForm>,
        env: &P<Env>,
        dst: u32,
        state: &mut State,
    ) {
        match &**exp {
            IForm::LRef(lref) => {
                let l = lookup_lexical(lref.lvar.clone(), env);

                if l.closure {
                    asm.emit_load_free_variable(dst as _, u24::new(state.frame_size - 1), l.idx)
                } else {
                    asm.emit_mov(dst as _, l.idx as _);
                }
            }

            IForm::Const(cons) => match cons {
                Sexpr::Boolean(boolean) => {
                    asm.emit_make_immediate(
                        dst as _,
                        Value::encode_bool_value(*boolean).get_raw() as _,
                    );
                }

                Sexpr::Fixnum(fix) => {
                    asm.emit_make_immediate(dst as _, Value::encode_int32(*fix).get_raw() as _)
                }

                Sexpr::Char(c) => {
                    asm.emit_make_immediate(dst as _, Value::encode_char(*c).get_raw() as _)
                }

                Sexpr::Null => {
                    asm.emit_make_immediate(dst as _, Value::encode_null_value().get_raw() as _)
                }

                Sexpr::Undefined => asm
                    .emit_make_immediate(dst as _, Value::encode_undefined_value().get_raw() as _),

                Sexpr::Flonum(flo) => {
                    asm.emit_make_immediate(dst as _, Value::encode_f64_value(*flo).get_raw() as _)
                }

                _ => {
                    asm.emit_make_non_immediate(u24::new(dst), cons.clone());
                }
            },

            IForm::GRef(gref) => {
                asm.emit_global_ref(u24::new(dst as _), gref.name.unwrap_id());
            }

            IForm::GSet(_) | IForm::Define(_) => {
                for_effect(asm, exp, env, state);
                asm.emit_make_immediate(dst as _, Value::encode_undefined_value().get_raw() as _);
            }

            IForm::Lambda(lam) => {
                let label = state.closures.iter().find(|(l, _)| l == lam).unwrap().1;
                if lam.free_lvars.is_empty() {
                    asm.emit_static_program(u24::new(dst), label as _);
                } else {
                    asm.emit_make_program(dst as _, lam.free_lvars.len() as _, label as _);
                    init_free_vars(asm, 0, &lam.free_lvars, env, 1, state);
                    asm.emit_mov(dst as _, 0);
                }
            }

            IForm::Seq(seq) => {
                visit_seq(seq, asm, env, Context::ValueAt(dst), state);
            }

            IForm::If(cond) => {
                visit_if(cond, asm, env, Context::ValueAt(dst), state);
            }

            IForm::It => {
                asm.emit_make_immediate(dst as _, Value::encode_undefined_value().get_raw() as _);
            }

            IForm::Call(call) => {
                let mut env = push_frame(env.clone());
                for arg in call.args.iter() {
                    env = for_push(asm, arg, &env, state);
                }
                let proc_slot = stack_height(&env, state);

                asm.emit_call(u24::new(proc_slot), call.args.len() as u32 + 1);
                asm.emit_receive(
                    stack_height_under_local(dst, state.frame_size) as _,
                    proc_slot as _,
                    u24::new(state.frame_size),
                );
            }

            IForm::Label(label) => {
                let pos = asm.code.len();
                state.labels.insert(exp.clone(), pos);
                asm.emit_loop_hint();
                for_value_at(asm, &label.body, env, dst, state);
            }

            IForm::Goto(_) => {
                for_effect(asm, exp, env, state);
            }

            IForm::PrimCall(name, args) => match *name {
                "list" => {
                    let args = for_args(asm, state, args, env);
                    asm.emit_make_immediate(0, Value::encode_null_value().get_raw() as _);
                    for arg in args.iter().rev() {
                        asm.emit_cons(0, *arg as u16, 0);
                    }
                    asm.emit_mov(dst as _, 0);
                }

                "vector" => {
                    let args = for_args(asm, state, args, env);
                    asm.emit_make_vector(0, args.len() as _);
                    for (i, arg) in args.iter().enumerate() {
                        asm.emit_vector_set_immediate(0, i as _, *arg as _);
                    }

                    asm.emit_mov(dst as _, 0);
                }

                _ => {
                    let prim = PRIMITIVES.get(name).unwrap();
                    if !prim.has_result {
                        for_effect(asm, exp, env, state);
                        asm.emit_make_immediate(
                            dst as _,
                            Value::encode_undefined_value().get_raw() as _,
                        );
                    } else {
                        let args = for_args(asm, state, args, env);
                        let mut rargs = vec![dst];
                        rargs.extend_from_slice(&args);

                        (prim.emit)(asm, &rargs);
                    }
                }
            },

            _ => todo!(),
        }
    }

    fn for_args(
        asm: &mut Assembler,
        state: &mut State<'_>,
        args: &[P<IForm>],
        env: &P<Env>,
    ) -> Vec<u32> {
        let mut env = env.clone();
        let mut cargs = vec![];
        for arg in args.iter() {
            env = for_value(asm, arg, &env, state);
            cargs.push(env.idx);
        }
        cargs
    }

    fn for_effect(
        asm: &mut Assembler,
        exp: &P<IForm>,
        env: &P<Env>,
        state: &mut State,
    ) -> Option<P<Env>> {
        match &**exp {
            IForm::LSet(lset) => {
                let l = lookup_lexical(lset.lvar.clone(), &env);
                if !l.closure {
                    for_value_at(asm, &lset.value, env, l.idx, state);
                } else {
                    let env = for_value(asm, &lset.value, env, state);

                    //if l.closure {
                    asm.emit_store_free_variable(
                        u24::new(1 - state.frame_size as u32),
                        env.idx as _,
                        l.idx,
                    );
                    //} else {
                    asm.emit_mov(l.idx as _, env.idx);
                    //}
                }
                None
            }

            IForm::Define(def) => {
                let env = for_value(asm, &def.value, env, state);

                asm.emit_global_set(u24::new(env.idx), def.name.unwrap_id());
                None
            }

            IForm::If(cond) => {
                visit_if(cond, asm, env, Context::Effect, state);
                None
            }
            IForm::Seq(seq) => {
                visit_seq(seq, asm, env, Context::Effect, state);
                None
            }
            IForm::Let(var) => visit_let(var, asm, env, Context::Effect, state),
            IForm::Const(_) | IForm::Lambda(_) | IForm::It | IForm::LRef(_) => None,
            IForm::Call(call) => {
                let mut env = push_frame(env.clone());
                for arg in call.args.iter() {
                    env = for_push(asm, arg, &env, state);
                }
                let proc_slot = stack_height(&env, state);

                asm.emit_call(u24::new(proc_slot), call.args.len() as u32 + 1);
                asm.emit_reset_frame(u24::new(state.frame_size));
                None
            }

            IForm::Label(label) => {
                let pos = asm.code.len();
                state.labels.insert(exp.clone(), pos);
                asm.emit_loop_hint();
                for_effect(asm, &label.body, env, state)
            }

            IForm::Goto(goto) => {
                let label = goto.upgrade().expect("label must be alive!");
                let pos = *state.labels.get(&label).unwrap();
                let start = asm.code.len() + 1;
                asm.emit_j_known(0);
                let diff = pos as i32 - asm.code.len() as i32;
                let code = &mut asm.code[start..start + 4];
                code.copy_from_slice(&diff.to_le_bytes());
                None
            }

            IForm::PrimCall(name, args) => {
                let prim = PRIMITIVES.get(*name).unwrap();
                let args = for_args(asm, state, args, env);
                (prim.emit)(asm, &args);
                None
            }
            _ => Some(for_value(asm, exp, env, state)),
        }
    }

    fn for_context(
        asm: &mut Assembler,
        ctx: Context,
        exp: &P<IForm>,
        env: &P<Env>,
        state: &mut State,
    ) -> Option<P<Env>> {
        match ctx {
            Context::Effect => for_effect(asm, exp, env, state),
            Context::Value => Some(for_value(asm, exp, env, state)),
            Context::ValueAt(dst) => {
                for_value_at(asm, exp, env, dst, state);
                None
            }

            Context::Tail => {
                for_tail(asm, exp, env, state);
                None
            }
            _ => todo!(),
        }
    }

    fn for_tail(asm: &mut Assembler, exp: &P<IForm>, env: &P<Env>, state: &mut State) {
        match &**exp {
            IForm::If(cond) => visit_if(cond, asm, env, Context::Tail, state),
            IForm::Seq(seq) => visit_seq(seq, asm, env, Context::Tail, state),
            IForm::Let(var) => {
                visit_let(var, asm, env, Context::Tail, state);
            }
            IForm::Call(call) => {
                let base = stack_height(env, state);
                let mut env = for_push(asm, &call.proc, env, state);
                for arg in call.args.iter() {
                    env = for_push(asm, arg, &env, state);
                }

                fn parallel_moves(asm: &mut Assembler, env: &P<Env>, base: u32, i: i32) {
                    if 0 <= i {
                        parallel_moves(asm, &env.prev.as_ref().unwrap(), base, i - 1);
                        asm.emit_mov(env.idx + base, env.idx);
                    }
                }

                parallel_moves(asm, &env, base, call.args.len() as i32);
                asm.emit_reset_frame(u24::new(1 + call.args.len() as u32));
                asm.emit_tail_call();
            }
            IForm::Goto(_) => {
                for_effect(asm, exp, env, state);
            }
            IForm::Label(label) => {
                let pos = asm.code.len();
                state.labels.insert(exp.clone(), pos);
                asm.emit_loop_hint();
                for_tail(asm, &label.body, env, state);
            }
            _ => {
                for_value_at(asm, exp, env, 0, state);
                asm.emit_return_values();
            }
        }
    }

    fn for_value(asm: &mut Assembler, exp: &P<IForm>, env: &P<Env>, state: &mut State) -> P<Env> {
        match &**exp {
            IForm::LRef(lref) => {
                let lexical = lookup_lexical(lref.lvar.clone(), env);
                if !lexical.closure {
                    return push_local_alias(
                        lexical.name.clone(),
                        lexical.id.clone(),
                        lexical.idx,
                        env.clone(),
                    );
                }
            }

            _ => (),
        }

        for_push(asm, exp, env, state)
    }

    fn for_push(asm: &mut Assembler, exp: &P<IForm>, env: &P<Env>, state: &mut State) -> P<Env> {
        for_value_at(asm, exp, env, env.next_local, state);
        push_temp(env.clone())
    }

    asm.emit_enter();
    let size = compute_frame_size(lam.clone());
    asm.emit_prelude(lam.reqargs + 1, lam.optarg, size);
    let env = create_initial_env(&lam.lvars, &lam.free_lvars, size as _);
    let mut state = State {
        labels: HashMap::new(),
        closures,
        frame_size: size as _,
    };
    for_context(asm, Context::Tail, &lam.body, &env, &mut state);
}

pub fn compile_bytecode(exp: P<IForm>, output: &mut Vec<u8>) {
    let mut asm = Assembler::new();
    let closures = scan_toplevel::<false>(exp.clone())
        .iter()
        .map(|iform| {
            let IForm::Lambda(lam) = &**iform else {
                unreachable!()
            };
            lam.clone()
        })
        .collect::<Vec<_>>();

    let IForm::Lambda(toplevel_lam) = &*exp else {
        unreachable!()
    };

    let toplevel_lam = toplevel_lam.clone();

    let mut label = 0;

    let closures = closures
        .iter()
        .map(|lam| {
            label += 1;
            (lam.clone(), label - 1)
        })
        .collect::<Vec<(P<Lambda>, usize)>>();

    let toplevel_ix = closures
        .iter()
        .position(|(lam, _)| lam.as_ptr() == toplevel_lam.as_ptr())
        .unwrap();
    let toplevel_ix = closures[toplevel_ix].1;

    let mut actual_locations = HashMap::new();

    struct Data {
        start: usize,
        end: usize,
        nargs: u32,
        optarg: bool,
        name: Value,
    }

    let mut closure_data = Vec::new();

    for (closure, label) in closures.iter() {
        let pos = asm.code.len();
        compile_closure(&mut asm, closure.clone(), &closures);
        let end = asm.code.len();
        actual_locations.insert(*label, pos);
        closure_data.push(Data {
            start: pos,
            end,
            nargs: closure.reqargs,
            optarg: closure.optarg,
            name: closure
                .name
                .as_ref()
                .map(|x| scm_intern(x))
                .unwrap_or(Value::encode_null_value()),
        });
    }

    while let Some(reloc) = asm.relocs.pop() {
        match reloc {
            Reloc::Label { code_loc, index } => {
                let label = *actual_locations.get(&(index as usize)).unwrap();

                let diff = label as i32 - code_loc as i32;
                let diff = diff as i32;

                let diff = diff.to_le_bytes();
                let code_loc = code_loc - 4;
                asm.code[code_loc as usize] = diff[0];
                asm.code[code_loc as usize + 1] = diff[1];
                asm.code[code_loc as usize + 2] = diff[2];
                asm.code[code_loc as usize + 3] = diff[3];
            } // TODO: More relocs for constants
        }
    }

    let constants = Sexpr::Vector(P(std::mem::take(&mut asm.constants)));
    let data = Sexpr::Vector(P(closure_data
        .into_iter()
        .map(|data| {
            Sexpr::Vector(P(vec![
                Sexpr::Fixnum(data.start as _),
                Sexpr::Fixnum(data.end as _),
                Sexpr::Fixnum(data.nargs as _),
                Sexpr::Boolean(data.optarg),
                if data.name.is_null() {
                    Sexpr::Null
                } else {
                    Sexpr::Symbol(data.name)
                },
            ]))
        })
        .collect::<Vec<Sexpr>>()));
    let label = actual_locations[&toplevel_ix];
    let entrypoint = Sexpr::Program(label as _);

    let data_section = Sexpr::Vector(P(vec![constants, data, entrypoint]));
    
    
    output.reserve(asm.code.len() + 8 + 128);
    output.extend_from_slice(&CAPY_BYTECODE_MAGIC.to_le_bytes());
    let code_len_pos = output.len();
    output.extend_from_slice(&(asm.code.len() as u32).to_le_bytes());
    let code_start_pos = output.len();
    output.extend_from_slice(&asm.code);
    while !is_aligned(output.len(), 4, 0) {
        output.push(OP_NOP);
    }
    let code_end_pos = output.len();
    let actual_length = code_end_pos - code_start_pos;
    output[code_len_pos..code_len_pos + 4].copy_from_slice(&(actual_length as u32).to_le_bytes());
    let start = output.len();
    FASLPrinter::new(output)
        .put(data_section)
        .expect("write to vector must always succeed");
    println!("constants len: {}", output.len() - start);
}

pub struct Primitive {
    pub nargs: usize,
    pub predicate: bool,
    pub has_result: bool,
    pub emit: fn(&mut Assembler, &[u32]),
    pub immediate_emit: Option<fn(&mut Assembler, &[u32])>,
}

static PRIMITIVES: Lazy<HashMap<&'static str, Primitive>> = Lazy::new(|| {
    let mut map = HashMap::new();

    macro_rules! define_primitives {
        ($(($name: literal, $nargs: literal, $predicate: literal, $has_result: literal, $asm: ident, $args: ident => $b: block))*) => {
            $(map.insert($name, Primitive {
                nargs: $nargs,
                predicate: $predicate,
                has_result: $has_result,
                emit: |$asm: &mut Assembler, $args: &[u32]| {
                    $b
                },
                immediate_emit: None,
            });)*
        };
    }

    define_primitives!(
        ("+", 2, false, true, asm, args => {
            asm.emit_add(args[0] as _, args[1] as _, args[2] as _);
        })
        ("-", 2, false, true, asm, args => {
            asm.emit_sub(args[0] as _, args[1] as _, args[2] as _);
        })

        ("*", 2, false, true, asm, args => {
            asm.emit_mul(args[0] as _, args[1] as _, args[2] as _);
        })

        ("/", 2, false, true, asm, args => {
            asm.emit_div(args[0] as _, args[1] as _, args[2] as _);
        })

        ("<", 2, false, true, asm, args => {
            asm.emit_less(args[0] as _, args[1] as _, args[2] as _);
        })

        ("<=", 2, false, true, asm, args => {
            asm.emit_less_equal(args[0] as _, args[1] as _, args[2] as _);
        })

        (">", 2, false, true, asm, args => {
            asm.emit_greater(args[0] as _, args[1] as _, args[2] as _);
        })

        (">=", 2, false, true, asm, args => {
            asm.emit_greater_equal(args[0] as _, args[1] as _, args[2] as _);
        })

        ("=", 2, false, true, asm, args => {
            asm.emit_numerically_equal(args[0] as _, args[1] as _, args[2] as _);
        })

        ("eq?", 2, true, true, asm, args => {
            asm.emit_eq(args[0] as _, args[1] as _, args[2] as _);
        })

        ("false?", 1, true, true, asm, args => {
            asm.emit_is_false(args[0] as _, args[1] as _);
        })

        ("true?", 1, true, true, asm, args => {
            asm.emit_is_true(args[0] as _, args[1] as _);
        })

        ("fixnum?", 1, true, true, asm, args => {
            asm.emit_is_int32(args[0] as _, args[1] as _);
        })

        ("flonum?", 1, true, true, asm, args => {
            asm.emit_is_flonum(args[0] as _, args[1] as _);
        })

        ("null?", 1, true, true, asm, args => {
            asm.emit_is_null(args[0] as _, args[1] as _);
        })

        ("undefined?", 1, true, true, asm, args => {
            asm.emit_is_undefined(args[0] as _, args[1] as _);
        })

        ("char?", 1, true, true, asm, args => {
            asm.emit_is_char(args[0] as _, args[1] as _);
        })

        ("pair?", 1, true, true, asm, args => {
            asm.emit_heap_tag_eq(args[0] as _, args[1] as _, TypeId::Pair as _);
        })

        ("vector?", 1, true, true, asm, args => {
            asm.emit_heap_tag_eq(args[0] as _, args[1] as _, TypeId::Vector as _);
        })

        ("bytevector?", 1, true, true, asm, args => {
            asm.emit_heap_tag_eq(args[0] as _, args[1] as _, TypeId::Bytevector as _);
        })

        ("string?", 1, true, true, asm, args => {
            asm.emit_heap_tag_eq(args[0] as _, args[1] as _, TypeId::String as _);
        })

        ("symbol?", 1, true, true, asm, args => {
            asm.emit_heap_tag_eq(args[0] as _, args[1] as _, TypeId::Symbol as _);
        })

        ("program?", 1, true, true, asm, args => {
            asm.emit_heap_tag_eq(args[0] as _, args[1] as _, TypeId::Program as _);
        })

        ("make-box", 1, false, true, asm, args => {
            asm.emit_make_box(args[0] as _, args[1] as _);
        })

        ("box-ref", 1, false, true, asm, args => {
            asm.emit_box_ref(args[0] as _, args[1] as _);
        })

        ("box-set!", 2, false, true, asm, args => {
            asm.emit_box_set(args[0], args[1]);
        })
    );

    map
});
