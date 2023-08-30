use std::collections::{HashMap, HashSet};

use crate::compiler::tree_il::{CallFlag, IForm, LVar};

use super::{
    tree_il::{LambdaFlag, LetType, LiftedVar},
    P,
};

pub fn scan<const RESET_CALL: bool>(
    iform: &P<IForm>,
    fs: &mut Vec<P<LVar>>,
    bs: &mut HashSet<P<LVar>>,
    toplevel: bool,
    labels: &mut Vec<P<IForm>>,
) {
    match &**iform {
        IForm::Define(def) => {
            if !toplevel {
                eprintln!(
                    "[warning] define appears in non-top-level expression: {}",
                    def.name
                )
            }

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
            if insert {
                if !fs.contains(&lvar.lvar) {
                    fs.push(lvar.lvar.clone());
                }
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

        IForm::Fix(fix) => {
            for lhs in fix.lhs.iter() {
                bs.insert(lhs.clone());
            }
            for rhs in fix.rhs.iter() {
                let lam = IForm::Lambda(rhs.clone());
                scan::<RESET_CALL>(&P(lam), fs, bs, toplevel, labels);
            }
            scan::<RESET_CALL>(&fix.body, fs, bs, toplevel, labels);
            
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
            let mut defs = Vec::new();
            scan::<RESET_CALL>(&lam.body, &mut lfs, &mut lbs, false, &mut defs);
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
            lam.defs = defs;
        }

        IForm::PrimCall(_, _, args) => {
            args.iter().for_each(|arg| {
                scan::<RESET_CALL>(arg, fs, bs, toplevel, labels);
            });
        }

        IForm::LetValues(vals) => {
            scan::<RESET_CALL>(&vals.init, fs, bs, toplevel, labels);
            scan::<RESET_CALL>(&vals.body, fs, bs, toplevel, labels);
        }

       
        _ => (),
    }
}

pub fn scan_toplevel<const RESET_CALL: bool>(iform: P<IForm>) {
    let mut fs = Vec::new();
    let mut bs = HashSet::new();
    let mut labels = Vec::new();
    scan::<RESET_CALL>(&iform, &mut fs, &mut bs, true, &mut labels);
}

/// After lambda-lifting we want to remap free-variables to newly created arguments.
pub fn pass2_substitute(mut iform: P<IForm>, map: &HashMap<P<LVar>, P<LVar>>) {
    match &mut *iform {
        IForm::LRef(lref) => {
            if let Some(new_lvar) = map.get(&lref.lvar) {
                lref.lvar.ref_count -= 1;
                lref.lvar = new_lvar.clone();
                lref.lvar.ref_count += 1;
            }
        }

        IForm::LSet(lset) => {
            if let Some(new_lvar) = map.get(&lset.lvar) {
                lset.lvar.ref_count -= 1;
                lset.lvar = new_lvar.clone();
                lset.lvar.set_count += 1;
            }
            pass2_substitute(lset.value.clone(), map);
        }

        IForm::Define(def) => {
            pass2_substitute(def.value.clone(), map);
        }

        IForm::Call(call) => {
            pass2_substitute(call.proc.clone(), map);
            call.args.iter_mut().for_each(|arg| {
                pass2_substitute(arg.clone(), map);
            });
        }

        IForm::GSet(gset) => {
            pass2_substitute(gset.value.clone(), map);
        }

        IForm::If(cond) => {
            pass2_substitute(cond.cond.clone(), map);
            pass2_substitute(cond.consequent.clone(), map);
            pass2_substitute(cond.alternative.clone(), map);
        }

        IForm::Let(var) => {
            var.inits.iter_mut().for_each(|init| {
                pass2_substitute(init.clone(), map);
            });
            pass2_substitute(var.body.clone(), map);
        }

        IForm::Lambda(lam) => {
            pass2_substitute(lam.body.clone(), map);
        }

        IForm::Seq(seq) => {
            seq.forms.iter_mut().for_each(|iform| {
                pass2_substitute(iform.clone(), map);
            });
        }
        IForm::Label(label) => {
            pass2_substitute(label.body.clone(), map);
        }

        IForm::PrimCall(_, _, args) => {
            args.iter_mut().for_each(|arg| {
                pass2_substitute(arg.clone(), map);
            });
        }

        IForm::LetValues(vals) => {
            pass2_substitute(vals.init.clone(), map);
            pass2_substitute(vals.body.clone(), map);
        }

        IForm::Fix(fix) => {
            for rhs in fix.rhs.iter_mut() {
                pass2_substitute(rhs.body.clone(), map);
            }
            pass2_substitute(fix.body.clone(), map);
        }
        _ => (),
    }
}
