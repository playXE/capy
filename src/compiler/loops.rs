//! Recover loops
//!
//! This pass finds loops in the Tree IL and turns them into `LABEL` and `GOTO` nodes.
//!
//! Note that most of code is more or less identical to what `pass2.rs` contains.
//! We should in the future try to reuse more code between the two passes.

use crate::compiler::{
    p::Weak,
    tree_il::{LSet, Seq},
};

use super::{
    tree_il::{IForm, LVar, Label, Lambda, Let, LetType},
    P,
};

fn is_misused(x: &IForm, tail: bool, lvar: P<LVar>, formals: &[P<LVar>]) -> bool {
    match x {
        IForm::LRef(lref) => lref.lvar.as_ptr() == lvar.as_ptr(),
        IForm::LSet(lset) => lset.lvar.as_ptr() == lvar.as_ptr(),
        IForm::GSet(gset) => is_misused(&gset.value, tail, lvar, formals),
        IForm::Lambda(lam) => is_misused(&lam.body, tail, lvar, formals),
        IForm::Seq(seq) => seq
            .forms
            .iter()
            .any(|x| is_misused(x, tail, lvar.clone(), formals)),

        IForm::Label(label) => is_misused(&label.body, tail, lvar, formals),

        IForm::Let(let_) => {
            let_.inits
                .iter()
                .any(|x| is_misused(x, tail, lvar.clone(), formals))
                || is_misused(&let_.body, tail, lvar, formals)
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
        IForm::PrimCall(_, args) => args
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
        IForm::PrimCall(_, args) => args.iter().any(|x| is_captured(x, in_lambda, lvar.clone())),
        _ => false,
    }
}

/// The binding must contain a single procedure and the body must be
/// a call to that procedure. The name of the procedure must furthermore not be misused. None of the
/// formals of the proccase can be captured in the lambda body..
fn is_optimizeable_loop(binding: &Let) -> bool {
    if binding.typ != LetType::Rec {
        return false;
    }

    if binding.lvars.len() != 1 {
        return false;
    }

    let lvar = binding.lvars[0].clone();

    let init = lvar.initval.clone().unwrap();

    let body = binding.body.clone();

    let body = match &*body {
        IForm::Seq(seq) if seq.forms.len() == 1 => seq.forms[0].clone(),
        IForm::Call(_) => body,
        _ => return false,
    };

    match (&*init, &*body) {
        (IForm::Lambda(lam), IForm::Call(call)) => {
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
            for form in seq.forms.iter_mut() {
                *form = recover_loops_rec(form.clone(), penv, false, changed);
            }

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
        IForm::PrimCall(_, args) => {
            for arg in args.iter_mut() {
                *arg = recover_loops_rec(arg.clone(), penv, false, changed);
            }

            iform
        }
        IForm::PrimRef(_) => iform,
        IForm::Let(var) => {
            let iter = var.lvars.iter().zip(var.inits.iter());

            let mut lvars = vec![];
            let mut inits = vec![];

            for (lvar, init) in iter {
                lvars.push(lvar.clone());
                inits.push(recover_loops_rec(init.clone(), penv, false, changed));
            }

            lvars
                .iter_mut()
                .zip(inits.iter_mut())
                .for_each(|(lvar, init)| {
                    lvar.initval = Some(init.clone());
                });

            let obody = recover_loops_rec(var.body.clone(), penv, tail, changed);

            var.lvars = lvars;

            if is_optimizeable_loop(var) {
                let IForm::Lambda(lambda) = &*var.inits[0] else {
                    unreachable!()
                };

                let body = var.body.clone();

                let body = match &*body {
                    IForm::Seq(seq) if seq.forms.len() == 1 => seq.forms[0].clone(),
                    IForm::Call(_) => body,
                    _ => unreachable!(),
                };

                let args = match &*body {
                    IForm::Call(call) => call.args.clone(),
                    _ => unreachable!(),
                };
                return optimize_loop(
                    oform,
                    var.lvars.first().cloned().unwrap(),
                    lambda.clone(),
                    args,
                );
            }

            var.body = obody;
            var.inits = inits;

            iform
        }
    }
}

fn optimize_loop(
    _binding: P<IForm>,
    lvar: P<LVar>,
    lambda: P<Lambda>,
    init: Vec<P<IForm>>,
) -> P<IForm> {
    let mut label = P(IForm::Label(Label {
        label: None,
        body: lambda.body.clone(),
    }));

    fn rewrite(mut x: P<IForm>, lvar: &P<LVar>, formals: &[P<LVar>], label: P<IForm>) -> P<IForm> {
        match &mut *x {
            IForm::Call(call) => {
                if call.proc.is_lref() && call.proc.lref_lvar().unwrap().as_ptr() == lvar.as_ptr() {
                    let mut set_vars = call
                        .args
                        .iter()
                        .zip(formals.iter())
                        .rev()
                        .map(|(form, lvar)| {
                            P(IForm::LSet(LSet {
                                lvar: lvar.clone(),
                                value: rewrite(form.clone(), lvar, formals, label.clone()),
                            }))
                        })
                        .collect::<Vec<_>>();
                    set_vars.push(P(IForm::Goto(Weak::new(&label))));
                    P(IForm::Seq(Seq { forms: set_vars }))
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

            IForm::PrimCall(_, args) => {
                args.iter_mut().for_each(|x| {
                    *x = rewrite(x.clone(), lvar, formals, label.clone());
                });

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
        typ: LetType::Let,
        lvars: lambda.lvars.clone(),
        inits: init,
        body: label,
    }));

    binding
}
