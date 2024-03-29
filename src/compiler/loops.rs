//! Recover loops
//!
//! This pass finds loops in the Tree IL and turns them into `LABEL` and `GOTO` nodes.
//!
//! Note that most of code is more or less identical to what `pass2.rs` contains.
//! We should in the future try to reuse more code between the two passes.

use crate::compiler::{
    p::Weak,
    tree_il::{LRef, LSet, Seq},
};

use super::{
    sexpr::Sexpr,
    tree_il::{IForm, LVar, Label, Lambda, Let, LetType},
    P,
};

fn is_misused(x: &IForm, tail: bool, lvar: P<LVar>, formals: &[P<LVar>]) -> bool {
    match x {
        IForm::LRef(lref) => lref.lvar.as_ptr() == lvar.as_ptr(),
        IForm::LSet(lset) => lset.lvar.as_ptr() == lvar.as_ptr(),
        IForm::GSet(gset) => is_misused(&gset.value, tail, lvar, formals),
        IForm::Lambda(lam) => is_misused(&lam.body, tail, lvar, formals),
        IForm::Seq(seq) => {
            for i in 0..seq.forms.len() - 1 {
                if is_misused(&seq.forms[i], false, lvar.clone(), formals) {
                    return true;
                }
            }

            is_misused(&seq.forms[seq.forms.len() - 1], tail, lvar, formals)
        }

        IForm::Label(label) => is_misused(&label.body, tail, lvar, formals),

        IForm::Let(let_) => {
            let_.inits
                .iter()
                .any(|x| is_misused(x, false, lvar.clone(), formals))
                || is_misused(&let_.body, tail, lvar, formals)
        }

        IForm::Fix(fix) => {
            fix.rhs.iter().any(|rhs| is_misused(&rhs.body, true, lvar.clone(), formals))
                || is_misused(&fix.body, tail, lvar.clone(), formals)
        }

        IForm::If(if_) => {
            is_misused(&if_.cond, tail, lvar.clone(), formals)
                || is_misused(&if_.consequent, tail, lvar.clone(), formals)
                || is_misused(&if_.alternative, tail, lvar.clone(), formals)
        }

        IForm::Call(call) => {
            // The lvar may only be *directly* in the `proc`
            // position and this must be a tail call.

            let op = call.proc.clone();

            if op.is_lref() && op.lref_lvar().unwrap().as_ptr() == lvar.as_ptr() {
                !tail || call.args.len() != formals.len()
            } else {
                (!op.is_lref() && is_misused(&op, false, lvar.clone(), formals))
                    || call
                        .args
                        .iter()
                        .any(|x| is_misused(x, false, lvar.clone(), formals))
            }
        }
        IForm::PrimCall(_, _, args) => args
            .iter()
            .any(|x| is_misused(x, false, lvar.clone(), formals)),
        IForm::Define(def) => is_misused(&def.value, tail, lvar, formals),
        _ => false,
    }
}

fn is_captured(x: &IForm, in_lambda: bool, lvar: P<LVar>) -> bool {
    match x {
        IForm::LRef(x) => in_lambda && x.lvar.as_ptr() == lvar.as_ptr(),

        IForm::LSet(x) => is_captured(&x.value, in_lambda, lvar),
        IForm::GSet(x) => is_captured(&x.value, in_lambda, lvar),
        IForm::Define(x) => is_captured(&x.value, in_lambda, lvar),
        IForm::Seq(x) => x
            .forms
            .iter()
            .any(|x| is_captured(x, in_lambda, lvar.clone())),
        IForm::Let(x) => {
            x.inits
                .iter()
                .any(|x| is_captured(x, in_lambda, lvar.clone()))
                || is_captured(&x.body, in_lambda, lvar.clone())
        }

        IForm::Call(x) => {
            is_captured(&x.proc, in_lambda, lvar.clone())
                || x.args
                    .iter()
                    .any(|x| is_captured(x, in_lambda, lvar.clone()))
        }

        IForm::If(x) => {
            is_captured(&x.cond, in_lambda, lvar.clone())
                || is_captured(&x.consequent, in_lambda, lvar.clone())
                || is_captured(&x.alternative, in_lambda, lvar.clone())
        }

        IForm::Label(x) => is_captured(&x.body, in_lambda, lvar),

        IForm::Lambda(lam) => is_captured(&lam.body, true, lvar),
        IForm::PrimCall(_, _, args) => args.iter().any(|x| is_captured(x, in_lambda, lvar.clone())),
        IForm::LetValues(lvals) => {
            is_captured(&lvals.init, in_lambda, lvar.clone())
                || is_captured(&lvals.body, in_lambda, lvar.clone())
        }
        _ => false,
    }
}

/// The fix must contain a single procedure and the body must be
/// a call to one of the proccases in that procedure. The name of
/// the procedure must furthermore not be misused. None of the
/// formals of the proccase can be captured in the proccase.

fn is_optimizeable_loop(lhs: &[P<LVar>], rhs: &[P<Lambda>], body: P<IForm>) -> bool {
    if lhs.len() != 1 {
        return false;
    }

    let lvar = lhs[0].clone();
    let init = rhs[0].clone();

    let body = match &*body {
        IForm::Seq(seq) if seq.forms.len() == 1 => seq.forms[0].clone(),
        IForm::Call(_) => body,
        _ => return false,
    };

    match (&*init, &*body) {
        (lam, IForm::Call(call)) => {
            let proc_is_loop_ref =
                call.proc.is_lref() && call.proc.lref_lvar().unwrap().as_ptr() == lvar.as_ptr();
            let var_is_immutable = lvar.is_immutable() && !lvar.boxed;
            let formals_are_immutable = lam
                .lvars
                .iter()
                .all(|lvar| lvar.is_immutable() && !lvar.boxed);

            proc_is_loop_ref
                && var_is_immutable
                && formals_are_immutable
                && !is_misused(&body, true, lvar.clone(), &lam.lvars)
                && !is_misused(&lam.body, true, lvar.clone(), &lam.lvars)
                && lam.lvars.iter().chain(std::iter::once(&lvar)).all(|lvar| {
                    !is_captured(&lam.body, false, lvar.clone())
                        && call
                            .args
                            .iter()
                            .all(|x| !is_captured(x, false, lvar.clone()))
                })
        }

        _ => false,
    }
}

pub fn recover_loops_rec(
    mut iform: P<IForm>,
    penv: &mut Vec<P<Lambda>>,
    tail: bool,
    changed: &mut bool,
) -> P<IForm> {
    let oform = iform.clone();
    match &mut *iform {
        IForm::Define(def) => {
            let expr = recover_loops_rec(def.value.clone(), penv, false, changed);
            def.value = expr;
            iform
        }

        IForm::LRef(_) => iform,
        IForm::LSet(lset) => {
            let value = recover_loops_rec(lset.value.clone(), penv, false, changed);
            lset.value = value;
            iform
        }

        IForm::GRef(_) => iform,
        IForm::GSet(gset) => {
            let value = recover_loops_rec(gset.value.clone(), penv, false, changed);
            gset.value = value;
            iform
        }

        IForm::Const(_) => iform,
        IForm::If(cond) => {
            let test_form = recover_loops_rec(cond.cond.clone(), penv, false, changed);
            let consequent = recover_loops_rec(cond.consequent.clone(), penv, tail, changed);
            let alternative = recover_loops_rec(cond.alternative.clone(), penv, tail, changed);

            cond.cond = test_form;
            cond.consequent = consequent;
            cond.alternative = alternative;

            iform
        }

        IForm::Call(call) => {
            let mut proc = recover_loops_rec(call.proc.clone(), penv, false, changed);
            let args = call
                .args
                .iter_mut()
                .map(|x| recover_loops_rec(x.clone(), penv, false, changed))
                .collect();

            if let IForm::LRef(lref) = &mut *proc {
                if let Some(IForm::Lambda(lambda)) = lref.lvar.initval.as_deref_mut() {
                    lambda.calls.push((oform, penv.clone()));
                }
            }

            call.proc = proc;
            call.args = args;

            iform
        }

        IForm::Seq(seq) => {
            if seq.forms.len() == 0 {
                return P(IForm::Const(Sexpr::Undefined));
            }
            for i in 0..seq.forms.len() - 1 {
                seq.forms[i] = recover_loops_rec(seq.forms[i].clone(), penv, false, changed);
            }
            let lidx = seq.forms.len() - 1;
            seq.forms[lidx] =
                recover_loops_rec(seq.forms[seq.forms.len() - 1].clone(), penv, tail, changed);

            iform
        }

        IForm::Label(label) => {
            let body = recover_loops_rec(label.body.clone(), penv, tail, changed);
            label.body = body;
            iform
        }

        IForm::Lambda(lambda) => {
            lambda.body = recover_loops_rec(lambda.body.clone(), penv, true, changed);
            iform
        }

        IForm::It => iform,
        IForm::Goto(_) => iform,
        IForm::PrimCall(_, _, args) => {
            for arg in args.iter_mut() {
                *arg = recover_loops_rec(arg.clone(), penv, false, changed);
            }

            iform
        }
        IForm::PrimRef(_) => iform,
        IForm::Let(var) => {
            for init in var.inits.iter_mut() {
                *init = recover_loops_rec(init.clone(), penv, false, changed);
            }
            var.body = recover_loops_rec(var.body.clone(), penv, tail, changed);

            iform
        }
        IForm::Fix(fix) => {
            for rhs in fix.rhs.iter_mut() {
                rhs.body = recover_loops_rec(rhs.body.clone(), penv, false, changed);
            }
            if is_optimizeable_loop(&fix.lhs, &fix.rhs, fix.body.clone()) {
                let body = match &*fix.body {
                    IForm::Seq(seq) if seq.forms.len() == 1 => seq.forms[0].clone(),
                    IForm::Call(_) => fix.body.clone(),
                    _ => unreachable!(),
                };

                let args = match &*body {
                    IForm::Call(call) => call.args.clone(),
                    _ => unreachable!(),
                };
                optimize_loop(fix.lhs[0].clone(), fix.rhs[0].clone(), args)
            } else {
                fix.body = recover_loops_rec(fix.body.clone(), penv, tail, changed);
                iform
            }
        }
        IForm::LetValues(lvals) => {
            lvals.init = recover_loops_rec(lvals.init.clone(), penv, false, changed);
            lvals.body = recover_loops_rec(lvals.body.clone(), penv, tail, changed);

            iform
        }
    }
}

fn optimize_loop(lvar: P<LVar>, lambda: P<Lambda>, init: Vec<P<IForm>>) -> P<IForm> {
    let mut label = P(IForm::Label(Label {
        src: lambda.body.src(),
        label: None,
        body: lambda.body.clone(),
    }));

    fn rewrite(mut x: P<IForm>, lvar: &P<LVar>, formals: &[P<LVar>], label: P<IForm>) -> P<IForm> {
        match &mut *x {
            IForm::Call(call) => {
                if call.proc.is_lref() && call.proc.lref_lvar().unwrap().as_ptr() == lvar.as_ptr() {
                    let temp_lvars = formals
                        .iter()
                        .map(|lvar| {
                            let temp_lvar = P(LVar {
                                name: lvar.name.clone(),
                                initval: None,
                                arg: false,
                                boxed: false,
                                ref_count: 0,
                                set_count: 0,
                            });
                            temp_lvar
                        })
                        .collect::<Vec<_>>();

                    let mut set_vars = temp_lvars
                        .iter()
                        .zip(formals.iter())
                        .map(|(temp_lvar, lvar)| {
                            P(IForm::LSet(LSet {
                                lvar: lvar.clone(),
                                value: P(IForm::LRef(LRef {
                                    lvar: temp_lvar.clone(),
                                })),
                            }))
                        })
                        .collect::<Vec<_>>();
                    set_vars.push(P(IForm::Goto(Weak::new(&label))));
                    let inits = call
                        .args
                        .iter()
                        .map(|x| rewrite(x.clone(), lvar, formals, label.clone()))
                        .collect::<Vec<_>>();
                    let temps = P(IForm::Let(Let {
                        src: call.src.clone(),
                        typ: LetType::Let,
                        lvars: temp_lvars,
                        inits,
                        body: P(IForm::Seq(Seq {
                            src: call.src.clone(),
                            forms: set_vars,
                        })),
                    }));

                    temps
                } else {
                    call.proc = rewrite(call.proc.clone(), lvar, formals, label.clone());
                    call.args = call
                        .args
                        .iter()
                        .map(|x| rewrite(x.clone(), lvar, formals, label.clone()))
                        .collect();

                    x
                }
            }

            IForm::Seq(seq) => {
                seq.forms = seq
                    .forms
                    .iter()
                    .map(|x| rewrite(x.clone(), lvar, formals, label.clone()))
                    .collect();
                x
            }

            IForm::Label(lbl) => {
                lbl.body = rewrite(lbl.body.clone(), lvar, formals, label.clone());
                x
            }

            IForm::Lambda(lambda) => {
                lambda.body = rewrite(lambda.body.clone(), lvar, formals, label.clone());
                x
            }

            IForm::Let(var) => {
                var.inits = var
                    .inits
                    .iter()
                    .map(|x| rewrite(x.clone(), lvar, formals, label.clone()))
                    .collect();
                var.body = rewrite(var.body.clone(), lvar, formals, label.clone());
                x
            }

            IForm::GSet(gset) => {
                gset.value = rewrite(gset.value.clone(), lvar, formals, label.clone());
                x
            }

            IForm::Define(def) => {
                def.value = rewrite(def.value.clone(), lvar, formals, label.clone());
                x
            }

            IForm::If(cond) => {
                cond.cond = rewrite(cond.cond.clone(), lvar, formals, label.clone());
                cond.consequent = rewrite(cond.consequent.clone(), lvar, formals, label.clone());
                cond.alternative = rewrite(cond.alternative.clone(), lvar, formals, label.clone());
                x
            }

            IForm::PrimCall(_, _, args) => {
                args.iter_mut().for_each(|x| {
                    *x = rewrite(x.clone(), lvar, formals, label.clone());
                });

                x
            }

            IForm::LetValues(lvals) => {
                lvals.init = rewrite(lvals.init.clone(), lvar, formals, label.clone());
                lvals.body = rewrite(lvals.body.clone(), lvar, formals, label.clone());
                x
            }

            IForm::Fix(fix) => {
                for rhs in fix.rhs.iter_mut() {
                    rhs.body = rewrite(rhs.body.clone(), lvar, formals, label.clone());
                }
                fix.body = rewrite(fix.body.clone(), lvar, formals, label.clone());
                x
            }

            _ => x,
        }
    }

    let body = rewrite(lambda.body.clone(), &lvar, &lambda.lvars, label.clone());

    let IForm::Label(ref mut lbl_) = &mut *label else {
        unreachable!()
    };

    lbl_.body = body;

    let binding = P(IForm::Let(Let {
        src: lambda.src.clone(),
        typ: LetType::Let,
        lvars: lambda.lvars.clone(),
        inits: init,
        body: label,
    }));

    binding
}
