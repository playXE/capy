use std::collections::HashMap;
use std::collections::HashSet;

use crate::bytecode::encode::*;
use crate::bytecode::opcodes::*;
use crate::bytecode::u24::u24;
use crate::bytecodeassembler::*;
use crate::compiler::sexpr::Sexpr;
use crate::runtime::object::scm_symbol_str;
use crate::runtime::symbol::scm_intern;
use crate::runtime::value::Value;

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
        IForm::Define(def) => {
            scan::<RESET_CALL>(&def.value, fs, bs, toplevel, labels)
        }
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
                if let IForm::GRef(gref) = &*call.proc {
                    let sym = gref.name.unwrap_id();

                    if PRIMCALL_TABLE.contains(&scm_symbol_str(sym)) {
                        return call.args.iter().map(|arg| visit(arg)).sum::<usize>();
                    }
                }

                1 + call.args.iter().map(|arg| visit(arg)).sum::<usize>() + 3 /* call frame size */
            }

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

            _ => 1,
        }
    }

    1 + lam.lvars.len() + visit(&lam.body) + TEMPORARY_COUNT
}

const PRIMCALL_TABLE: &[&str] = &["box", "box-ref", "box-set!"];

struct Env {
    prev: Option<P<Self>>,
    name: Sexpr,
    id: P<LVar>,
    idx: u32,
    closure: bool,
    next_local: u32,
}

pub fn compile_closure(asm: &mut Assembler, lam: P<Lambda>, closures: &HashMap<P<Lambda>, usize>) {
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
        closures: &'a HashMap<P<Lambda>, usize>,
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
        for_effect(asm, &cond.cond, env, state);
        let je = asm.emit_je();
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
                    asm.emit_load_free_variable(
                        dst as _,
                        u24::new(1 - state.frame_size as u32),
                        l.idx,
                    )
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
                let label = *state.closures.get(lam).expect("must be defined");
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

            _ => todo!(),
        }
    }

    fn for_effect(
        asm: &mut Assembler,
        exp: &P<IForm>,
        env: &P<Env>,
        state: &mut State,
    ) -> Option<P<Env>> {
        match &**exp {
            IForm::LSet(lset) => {
                let env = for_value(asm, &lset.value, env, state);
                let l = lookup_lexical(lset.lvar.clone(), &env);

                if l.closure {
                    asm.emit_store_free_variable(
                        u24::new(1 - state.frame_size as u32),
                        env.idx as _,
                        l.idx,
                    );
                } else {
                    asm.emit_mov(l.idx as _, env.idx);
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
    println!("frame size: {}", size);
    asm.emit_prelude(lam.reqargs + 1, lam.optarg, size);
    let env = create_initial_env(&lam.lvars, &lam.free_lvars, size as _);
    let mut state = State {
        labels: HashMap::new(),
        closures,
        frame_size: size as _,
    };
    for_context(asm, Context::Tail, &lam.body, &env, &mut state);
}

pub fn compile_bytecode(exp: P<IForm>) -> Assembler {
    let mut asm = Assembler::new();
    let closures = scan_toplevel::<false>(exp.clone()).iter().map(|iform| {
        let IForm::Lambda(lam) = &**iform else {unreachable!()};
        lam.clone()
    }).collect::<Vec<_>>();
    
    let mut label = 0;

    let closures = closures.iter().map(|lam| {
        label += 1;
        (lam.clone(), label - 1)
    }).collect::<HashMap<P<Lambda>, usize>>();

    let mut actual_locations = HashMap::new();

    for (closure,label) in closures.iter() {
        let pos = asm.code.len();
        compile_closure(&mut asm, closure.clone(), &closures);
        actual_locations.insert(*label, pos);
    }

    while let Some(reloc) = asm.relocs.pop() {
        match reloc {
            Reloc::Label { code_loc, index } => {
                let label = *actual_locations.get(&(index as usize)).unwrap();
                let diff = label as isize - code_loc as isize;
                let diff = diff as i32;
                let diff = diff.to_le_bytes();
                asm.code[code_loc as usize] = diff[0];
                asm.code[code_loc as usize + 1] = diff[1];
                asm.code[code_loc as usize + 2] = diff[2];
                asm.code[code_loc as usize + 3] = diff[3];
            }
            // TODO: More relocs for constants
        }
    }

    asm
}