//! Pass 2. Optimization Stage 1
//!
//! Walk down IForm and perform optimizations.
//! The main focus is to lift or inline closures, and eliminate
//! local frames by beta reduction.
//! This pass may modify the tree by changing IForm nodes destructively.

use std::collections::HashMap;

pub struct Pass2Ctx {
    pub lambda_lift: bool,
    pub inline: bool,
    pub changed: bool,
}

use crate::compiler::{expand::expand_inline_procedure, sexpr::Sexpr};

use super::{
    constfold::USUAL_CONSTANT_FOLDING_TABLE,
    loops::recover_loops_rec,
    pass2p2::{pass2_substitute, scan_toplevel},
    primitives::resolve_primitives,
    tree_il::*,
    P,
};

pub fn pass2_rec(
    mut iform: P<IForm>,
    penv: &mut Vec<P<Lambda>>,
    tail: bool,
    ctx: &mut Pass2Ctx,
) -> Result<P<IForm>, String> {
    match &mut *iform {
        IForm::Define(def) => {
            let expr = pass2_rec(def.value.clone(), penv, false, ctx)?;
            def.value = expr;
            Ok(iform)
        }

        IForm::PrimCall(name, args) => {
            for arg in args.iter_mut() {
                *arg = pass2_rec(arg.clone(), penv, false, ctx)?;
            }

            if let Some(entry) = USUAL_CONSTANT_FOLDING_TABLE
                .get(name)
                .filter(|_| args.iter().all(|x| x.is_const()))
                .filter(|entry| {
                    args.len() == entry.predicates.len()
                        && entry
                            .predicates
                            .iter()
                            .zip(args.iter())
                            .all(|(pred, arg)| pred(arg.as_const().unwrap()))
                })
            {
                let consts = args
                    .iter()
                    .map(|x| {
                        if let IForm::Const(x) = &**x {
                            x.clone()
                        } else {
                            unreachable!()
                        }
                    })
                    .collect::<Vec<_>>();
                let res = (entry.fold)(&consts);

                if let Some(res) = res {
                    ctx.changed = true;
                    return Ok(P(IForm::Const(res)));
                }
            }

            Ok(iform)
        }

        IForm::LRef(_) => Ok(pass2_lref_eliminate(iform.clone())),
        IForm::LSet(lset) => {
            let value = pass2_rec(lset.value.clone(), penv, false, ctx)?;
            lset.value = value;
            Ok(iform)
        }

        IForm::GRef(_) => Ok(iform),
        IForm::GSet(gset) => {
            let value = pass2_rec(gset.value.clone(), penv, false, ctx)?;
            gset.value = value;
            Ok(iform)
        }

        IForm::Const(_) => Ok(iform),
        IForm::If(cond) => {
            let test_form = pass2_rec(cond.cond.clone(), penv, false, ctx)?;
            let then_form = pass2_rec(cond.consequent.clone(), penv, tail, ctx)?;
            let else_form = pass2_rec(cond.alternative.clone(), penv, tail, ctx)?;

            if let Some(cut) = pass2_branch_cut(
                iform.clone(),
                test_form.clone(),
                then_form.clone(),
                else_form.clone(),
            ) {
                return Ok(cut);
            } else {
                Ok(pass2_update_if(iform, test_form, then_form, else_form))
            }
        }

        IForm::Let(var) => {
            let iter = var.lvars.iter().zip(var.inits.iter());

            let mut lvars = vec![];
            let mut inits = vec![];

            for (lvar, init) in iter {
                if let IForm::Lambda(lam) = &**init {
                    if lam.flag == LambdaFlag::Used && lvar.is_immutable() && lvar.ref_count == 0 {
                        continue;
                    }
                }

                let init = pass2_rec(init.clone(), penv, false, ctx)?;

                lvars.push(lvar.clone());
                inits.push(init);
            }

            lvars.iter_mut().zip(inits.iter()).for_each(|(lvar, init)| {
                lvar.initval = Some(init.clone());
            });

            let obody = pass2_rec(var.body.clone(), penv, tail, ctx)?;
            for (lvar, init) in lvars.iter().zip(inits.iter()) {
                pass2_optimize_closure(lvar.clone(), init.clone(), ctx)?;
            }

            Ok(pass2_shrink_let_frame(iform.clone(), &lvars, obody, ctx))
        }

        IForm::Call(_) => pass2_call(iform.clone(), penv, tail, ctx),
        IForm::Seq(seq) => {
            if seq.forms.is_empty() {
                return Ok(iform);
            }
            for form in seq.forms.iter_mut() {
                *form = pass2_rec(form.clone(), penv, false, ctx)?;
            }

            if seq.forms.len() == 1 {
                return Ok(seq.forms[0].clone());
            }

            Ok(iform)
        }

        IForm::Lambda(lam) => {
            let mut penv = penv.clone();
            penv.push(lam.clone());
            lam.body = pass2_rec(lam.body.clone(), &mut penv, true, ctx)?;
            Ok(iform)
        }
        _ => Ok(iform.clone()),
    }
}

pub fn pass2_shrink_let_frame(
    mut iform: P<IForm>,
    lvars: &[P<LVar>],
    obody: P<IForm>,
    ctx: &mut Pass2Ctx,
) -> P<IForm> {
    //pass2_intermediate_lrefs_removal(lvars, obody.clone());
    let IForm::Let(var) = &mut *iform else {
        return iform;
    };
    return iform;
    /*let (new_lvars, new_inits, mut removed_init) = pass2_remove_unused_lvars(lvars, var.typ, ctx);

    if new_lvars.is_empty() {
        ctx.changed = true;
        if removed_init.is_empty() {
            return obody;
        } else {
            return P(IForm::Seq(Seq {
                forms: {
                    removed_init.push(obody);
                    removed_init
                },
            }));
        }
    } else {
        var.lvars = new_lvars;
        var.inits = new_inits;

        if !removed_init.is_empty() {
            removed_init.push(obody);
            var.body = P(IForm::Seq(Seq {
                forms: removed_init,
            }));
        } else {
            var.body = obody;
        }

        iform
    }*/
}

pub fn pass2_intermediate_lrefs_removal(lvars: &[P<LVar>], body: P<IForm>) {
    fn intermediate_lrefs(node: P<IForm>, lvars: &[P<LVar>]) -> Option<P<IForm>> {
        let IForm::Call(call) = &*node else {
            return None;
        };
        let mut subcall_node = None;
        let mut ilrefs = vec![];

        for arg in call.args.iter() {
            match &**arg {
                IForm::Const(_) => continue,
                IForm::LRef(lref) => {
                    let lv = lref.lvar.clone();

                    if lvars.iter().any(|l| l.as_ptr() == lref.lvar.as_ptr()) {
                        if lv.ref_count == 1 && lv.is_immutable() {
                            ilrefs.push(lv.clone());
                        }
                    } else if lv.is_immutable() {
                        continue;
                    } else {
                        return None;
                    }
                }

                IForm::Call(_) => {
                    if subcall_node.is_some() {
                        return None;
                    }

                    subcall_node = Some(arg.clone());
                }

                _ => return None,
            }
        }

        if let Some(subcall_node) = subcall_node {
            if !ilrefs.is_empty() {
                return intermediate_lrefs(subcall_node, &ilrefs);
            } else {
                return None;
            }
        } else {
            if !ilrefs.is_empty() {
                return Some(node);
            } else {
                return None;
            }
        }
    }

    if let IForm::Seq(seq) = &*body {
        if !seq.forms.is_empty() {
            let first_expr = seq.forms.first().unwrap();
            if matches!(**first_expr, IForm::Call(_)) {
                if let Some(mut node) = intermediate_lrefs(first_expr.clone(), lvars) {
                    let IForm::Call(call) = &mut *node else {
                        unreachable!()
                    };

                    for arg in call.args.iter_mut() {
                        if let IForm::LRef(lref) = &mut *(arg.clone()) {
                            if lvars.iter().any(|l| l.as_ptr() == lref.lvar.as_ptr()) {
                                if let Some(_) = lref.lvar.initval.clone() {
                                    lref.lvar.ref_count -= 1;
                                    lref.lvar.initval = Some(P(IForm::Const(Sexpr::Undefined)));
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
pub fn pass2_lref_eliminate(mut iform: P<IForm>) -> P<IForm> {
    let mut orig = iform.clone();
    let IForm::LRef(lref) = &mut *iform else {
        unreachable!()
    };
    let mut lvar = lref.lvar.clone();
    if lvar.is_immutable() {
        match lvar.initval.clone() {
            Some(init) => match &*init {
                IForm::Const(value) => {
                    lvar.ref_count -= 1;
                    *orig = IForm::Const(value.clone());

                    return orig;
                }

                IForm::LRef(lref2) if lref.lvar.is_immutable() => {
                    lvar.ref_count -= 1;
                    lref2.lvar.clone().ref_count += 1;
                    lref.lvar = lref2.lvar.clone();
                    return pass2_lref_eliminate(orig);
                }

                _ => return orig,
            },

            None => return orig,
        }
    } else {
        orig
    }
}

/// Scan LVARS and returns three values:
///   - List of needed lvars
///   - List of init expressions, corresponding to the first return value.
///   - List of non-transparent init expressions for removed lvars---they
///     need to be executed at the top of the body of the binding construct.
///
/// We have to be careful optimizing letrec* - we can't reorder init
/// when it can have side effects.  However, we still have to remove lambda
/// form that is no longer used---that means the lambda form is inlined
/// elsewhere, and its body has been modified to suit the inlined environment,
/// so we can no longer compile the $lambda node safely.
///
///
/// NB: By eliminating unused init expressions that contains LREF/LSET, the
/// lvar reference count may change.  However, traversing entire tree could
/// be costly, so we only do adjustment for some trival cases---pass3 runs
/// 'reset-lvars' which correctly sets lref count after pass2-3 optimizations.
/// This is kludge, still, and it may turn out that we should do proper
/// refcount adjustment here.  We'll see.
pub fn pass2_remove_unused_lvars(
    lvars: &[P<LVar>],
    typ: LetType,
    ctx: &mut Pass2Ctx,
) -> (Vec<P<LVar>>, Vec<P<IForm>>, Vec<P<IForm>>) {
    let mut rl = vec![];
    let mut ri = vec![];
    let mut rr = vec![];

    for lvar in lvars.iter() {
        if lvar.ref_count == 0 && lvar.is_immutable() {
            let init = lvar.initval.clone();

            if typ == LetType::RecStar && !init.as_ref().filter(|x| x.is_transparent()).is_some() {
                rl.push(lvar.clone());
                ri.push(init.unwrap().clone());
                continue;
            } else {
                ctx.changed = true;

                match init {
                    None => unreachable!(),
                    Some(mut init) => match &mut *init {
                        IForm::LRef(lref) => {
                            lref.lvar.ref_count -= 1;
                        }
                        x if x.is_transparent() => continue,
                        _ => {
                            rr.push(init.clone());
                            continue;
                        }
                    },
                }
            }
        } else {
            rl.push(lvar.clone());
            ri.push(lvar.initval.clone().unwrap());
        }
    }

    (rl, ri, rr)
}

/// Closure optimization (called from pass2 LET handler)
///
///
///   Determine the strategy to optimize each closure, and modify the nodes
///   accordingly.  We can't afford time to run iterative algorithm to find
///   optimal strategy, so we take a rather simple-minded path to optimize
///   common cases.
///
///
///   By now, we have the information of all call sites of the statically
///   bound closures.   Each call site is marked as either REC, TAIL-REC
///   or LOCAL.  See explanation of pass2/$CALL below.
///
///
///   Our objective is to categorize each call site to one of the following
///   four options:
///
///     - LOCAL: when we can't avoid creating a closure, calls to it are marked
///     as "local".  The call to the local closure becomes a LOCAL-ENV-CALL
///     instruction, which is faster than generic CALL/TAIL-CALL instructions.
///
///     - EMBED: the lambda body is inlined at the call site.  It differs from
///     the normal inlining in a way that we don't run beta-conversion of
///     lrefs, since the body can be entered from other 'jump' call sites.
///
///     - JUMP: the call becomes a LOCAL-ENV-JUMP instruction, i.e. a jump
///     to the lambda body which is generated by the 'embed' call.
///
///   We can inline LAMBDA if the following conditions are met:
///
///     1. The reference count of LVAR is equal to the number of call
///        sites.  This means all use of this LAMBDA is first-order,
///        so we know the closure won't "leak out".
///
///     2. It doesn't have any REC call sites, i.e. non-tail self recursive
///        calls.  (We may be able to convert non-tail self recursive calls
///        to jump with environment adjustment, but it would complicates
///        stack handling a lot.)
///
///     3. It doesn't have any TAIL-REC calls across closure boundary.
///        ```text
///         (letrec ((foo (lambda (...)
///                           ..... (foo ...)) /// ok
///           ...
///
///         (letrec ((foo (lambda (...) ....
///                         (lambda () ... (foo ...)) /// not ok
///           ...
///         ```
///     4. Either:
///         - It has only one LOCAL call, or
///         - It doesn't have TAIL-REC calls and the body of $LAMBDA is
///           small enough to duplicate.
///
///   If we determine LAMBDA to be inlined, all LOCAL calls become EMBED
///   calls, and TAIL-RECs become JUMP.
///
///   Otherwise, if the first condition is met, we can lift LAMBDA to
///   the toplevel by...
///
///     a. Scan the lambda body to find free lvars.  If there's any,
///        transform free lvars to bound lvars by adding new arguments
///        to the LAMBDA node, and modifies all the call sites
///        accordingly.
///
///   Otherwise, all calls become LOCAL calls.
///
pub fn pass2_optimize_closure(
    lvar: P<LVar>,
    mut lambda_node: P<IForm>,
    ctx: &mut Pass2Ctx,
) -> Result<(), String> {
    if lvar.is_immutable() && lvar.ref_count > 0 && matches!(&*lambda_node, IForm::Lambda(_)) {
        let IForm::Lambda(ref mut lam) = &mut *lambda_node else {
            unreachable!()
        };

        if lvar.ref_count as usize == lam.calls.len() && (ctx.lambda_lift || ctx.inline) {
            let (locals, recs, tail_recs) = pass2_classify_calls(&lam.calls, lam.clone());

            if recs.is_empty() && tail_recs.is_empty() {
                ctx.changed = true;
                local_call_inliner(lvar.clone(), lam.clone(), &locals)
            } else {
                let args_to_add = compute_added_arguments(lam.clone());
                if ctx.lambda_lift && !args_to_add.is_empty() && args_to_add.len() < 8 {
                    ctx.changed = true;
                    pass2_substitute(lam.body.clone(), &args_to_add);

                    // prepend arguments to lambda
                    lam.lvars = args_to_add
                        .iter()
                        .map(|(_, var)| var.clone())
                        .chain(lam.lvars.iter().cloned())
                        .collect();

                    for (call, _) in lam.calls.iter() {
                        let IForm::Call(ref mut call) = &mut *call.clone() else {
                            unreachable!()
                        };

                        // prepend the arguments
                        call.args = args_to_add
                            .iter()
                            .map(|(free, _)| {
                                let mut free = free.clone();
                                free.ref_count += 1;
                                P(IForm::LRef(LRef { lvar: free.clone() }))
                            })
                            .chain(call.args.iter().cloned())
                            .collect();
                    }
                }

                Ok(())
            }
        } else {
            local_call_optimizer(lvar, lam.clone())
        }
    } else {
        Ok(())
    }
}

fn compute_added_arguments(lambda_node: P<Lambda>) -> HashMap<P<LVar>, P<LVar>> {
    lambda_node
        .free_lvars
        .iter()
        .map(|free| {
            (
                free.clone(),
                P(LVar {
                    name: free.name.clone(),
                    boxed: false,
                    initval: None,
                    arg: true,
                    ref_count: 0,
                    set_count: 0,
                }),
            )
        })
        .collect()
}

/// Classify the calls into categories.  TAIL-REC call is classified as
/// REC if the call is across the closure boundary.
pub fn pass2_classify_calls(
    call_n_envs: &[(P<IForm>, Vec<P<Lambda>>)],
    lambda_node: P<Lambda>,
) -> (Vec<P<IForm>>, Vec<P<IForm>>, Vec<P<IForm>>) {
    let mut local = vec![];
    let mut rec = vec![];
    let mut trec = vec![];

    fn direct_call_p(env: &[P<Lambda>], lambda_node: P<Lambda>) -> bool {
        // skip dissolved (inlined) lambdas
        env.iter()
            .filter(|lam| lam.flag != LambdaFlag::Dissolved)
            .any(|x| x.as_ptr() == lambda_node.as_ptr())
    }

    for (call, env) in call_n_envs.iter() {
        let node = call.clone();
        let IForm::Call(ref call) = &**call else {
            unreachable!()
        };

        match call.flag {
            CallFlag::TailRec => {
                if direct_call_p(env, lambda_node.clone()) {
                    trec.push(node);
                    continue;
                } else {
                    rec.push(node);
                    continue;
                }
            }

            CallFlag::Rec => {
                rec.push(node);
                continue;
            }
            _ => {
                local.push(node);
            }
        }
    }

    (local, rec, trec)
}

fn local_call_optimizer(_: P<LVar>, mut lambda_node: P<Lambda>) -> Result<(), String> {
    for i in 0..lambda_node.calls.len() {
        let IForm::Call(ref mut call) = &mut *lambda_node.calls[i].0 else {
            unreachable!()
        };

        call.flag = CallFlag::Local;
    }

    lambda_node.calls.clear();
    Ok(())
}

/// Called when the local function (lambda-node) doesn't have recursive
/// calls, can be inlined, and called from multiple places.
/// NB: Here we destructively modify $call node to change it to $seq,
/// in order to hold the $LET node.  It breaks the invariance that $seq
/// contains zero or two or more nodes.
fn local_call_inliner(
    mut lvar: P<LVar>,
    mut lambda_node: P<Lambda>,
    calls: &Vec<P<IForm>>,
) -> Result<(), String> {
    fn inline_it(mut call_node: P<IForm>, lambda_node: P<Lambda>) -> Result<(), String> {
        let IForm::Call(c) = &*call_node else {
            unreachable!()
        };
        let inlined = expand_inline_procedure(P(IForm::Lambda(lambda_node.clone())), &c.args)?;

        if let IForm::Seq(inlined) = &*inlined {
            *call_node = IForm::Seq(Seq {
                forms: inlined.forms.clone(),
            });
        } else {
            *call_node = IForm::Seq(Seq {
                forms: vec![inlined],
            })
        }

        Ok(())
    }

    lvar.ref_count = 0;
    lambda_node.flag = LambdaFlag::Dissolved;

    for call in calls.iter() {
        inline_it(call.clone(), lambda_node.clone())?;
    }

    Ok(())
}

pub fn pass2_branch_cut(
    _: P<IForm>,
    test_form: P<IForm>,
    then_form: P<IForm>,
    else_form: P<IForm>,
) -> Option<P<IForm>> {
    if let IForm::Const(c) = &*test_form {
        let val_form = if c.to_boolean() { then_form } else { else_form };

        Some(if let IForm::It = &*val_form {
            test_form
        } else {
            val_form
        })
    } else {
        None
    }
}

pub fn pass2_update_if(
    _: P<IForm>,
    new_test: P<IForm>,
    new_then: P<IForm>,
    new_else: P<IForm>,
) -> P<IForm> {
    if new_then.as_ptr() == new_else.as_ptr() {
        return P(IForm::Seq(Seq {
            forms: vec![new_test, new_then],
        }));
    } else {
        return P(IForm::If(If {
            cond: new_test,
            consequent: new_then,
            alternative: new_else,
        }));
    }
}

fn pass2_call(
    mut iform: P<IForm>,
    penv: &mut Vec<P<Lambda>>,
    tail: bool,
    ctx: &mut Pass2Ctx,
) -> Result<P<IForm>, String> {
    let IForm::Call(ref mut call) = &mut *iform else {
        unreachable!()
    };

    fn try_inline(
        mut iform: P<IForm>,
        penv: &mut Vec<P<Lambda>>,
        tail: bool,
        ctx: &mut Pass2Ctx,
    ) -> Result<P<IForm>, String> {
        let oform = iform.clone();
        let IForm::Call(ref mut call) = &mut *iform else {
            unreachable!()
        };

        let proc = call.proc.clone();
        let pass2_args = |mut iform: P<IForm>,
                          args: Vec<P<IForm>>,
                          penv: &mut Vec<P<Lambda>>,
                          ctx: &mut Pass2Ctx| {
            let IForm::Call(ref mut call) = &mut *iform else {
                unreachable!()
            };

            call.args = args
                .iter()
                .cloned()
                .map(|x| pass2_rec(x, penv, false, ctx))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(iform)
        };
        match &*proc {
            IForm::Lambda(_) => {
                return pass2_rec(expand_inline_procedure(proc, &call.args)?, penv, tail, ctx)
            }

            IForm::LRef(lref) => {
                if let Some(mut initval) = lref
                    .lvar
                    .initval
                    .clone()
                    .filter(|x| matches!(&**x, IForm::Lambda(_)))
                {
                    let IForm::Lambda(lambda_node) = &*initval else {
                        unreachable!()
                    };
                    let self_recursing = penv.iter().any(|x| x.as_ptr() == lambda_node.as_ptr());

                    let call_flag = if self_recursing {
                        if tail {
                            CallFlag::TailRec
                        } else {
                            CallFlag::Rec
                        }
                    } else if lref.lvar.ref_count == 0 {
                        CallFlag::None
                    } else {
                        CallFlag::Local
                    };
                    if call_flag != CallFlag::None {
                        call.flag = call_flag;
                        let IForm::Lambda(lambda_node) = &mut *initval else {
                            unreachable!()
                        };
                        lambda_node.calls.push((oform.clone(), penv.clone()));

                        return pass2_args(oform, call.args.clone(), penv, ctx);
                    } else {
                        let mut lv = lref.lvar.clone();
                        lv.ref_count -= 1;
                        let IForm::Lambda(lambda_node) = &mut *initval else {
                            unreachable!()
                        };

                        lambda_node.flag = LambdaFlag::Used;
                        call.proc = initval;
                        try_inline(oform, penv, tail, ctx)
                    }
                } else {
                    return pass2_args(oform, call.args.clone(), penv, ctx);
                }
            }

            _ => {
                return pass2_args(oform, call.args.clone(), penv, ctx);
            }
        }
    }

    if call.flag != CallFlag::None {
        return Ok(iform);
    } else {
        let proc = pass2_rec(call.proc.clone(), penv, false, ctx)?;
        call.proc = proc;

        try_inline(iform, penv, tail, ctx)
    }
}

pub fn pass2(mut iform: P<IForm>, recover_loops: bool) -> Result<P<IForm>, String> {
    let mut _passes = 0;
    let mut ctx = Pass2Ctx {
        changed: false,
        inline: false,
        lambda_lift: false,
    };
    loop {
        iform.count_refs();
        scan_toplevel::<true>(iform.clone()); // compute bound and free variables in each lambda

        iform = resolve_primitives(iform.clone());
        if recover_loops {
            ctx.changed = false;
            ctx.inline = true;
            ctx.lambda_lift = false;
            iform = pass2_rec(iform.clone(), &mut Vec::with_capacity(4), true, &mut ctx)?;
            // TODO: recover loops here
            scan_toplevel::<true>(iform.clone()); // compute bound and free variables in each lambda
            iform.count_refs();
            iform = recover_loops_rec(iform, &mut vec![], false, &mut ctx.changed);
            scan_toplevel::<true>(iform.clone()); // compute bound and free variables in each lambda
            iform.count_refs();
            ctx.lambda_lift = true;
            ctx.inline = true;
            iform = pass2_rec(iform.clone(), &mut Vec::with_capacity(4), true, &mut ctx)?;
        } else {
            // can perform lambda lifting and inlining at the same time if we don't need to recover loops
            ctx.changed = false;
            ctx.inline = true;
            ctx.lambda_lift = true;
            iform = pass2_rec(iform.clone(), &mut Vec::with_capacity(4), true, &mut ctx)?;
        }

        if !ctx.changed {
            return Ok(iform);
        }
        _passes += 1;
    }
}
