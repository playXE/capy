//! Recover let expressions from the TreeIL. Looks for `(funcall (proc ...) ...)` where
//! operands match one of the cases of `proc`. If so, replace the `funcall`` node with a `bind` node.
//!
//! Invoke [pass_let] to recover let expressions from the TreeIL.

use crate::{rc::Rc, tree_il::*};

fn match_formals(op: &Proc, args: &[Rc<TreeNode>]) -> Option<Rc<ProcCase>> {
    fn check(info: &CaseInfo, args: &[Rc<TreeNode>]) -> bool {
        // TODO: Here it is possible to match on formals with rest
        // arguments as well, but the resultant must create a
        // list. For now just handle the cases that restore let
        // expressions.
        info.proper && info.formals.len() == args.len()
    }

    op.cases
        .iter()
        .find(|case| check(&case.info, args))
        .cloned()
}

#[allow(unused_mut, dead_code, unused_variables)]
pub fn pass_let(x: &Rc<TreeNode>) -> Rc<TreeNode> {
    fn pass(x: &Rc<TreeNode>) -> Rc<TreeNode> {
        match &**x {
            TreeNode::Bind(lhs, rhs, body, source) => make_bind(
                lhs.clone(),
                rhs.iter()
                    .zip(lhs.iter())
                    .map(|(x, var)| {
                        let x = pass(&*x);
                        let mut var = var.clone();
                        //var.operand = Some(x.clone());
                        x
                    })
                    .collect(),
                pass(body),
                source.clone(),
            ),
            TreeNode::Rec(vars, body, source) => {
                let vars = vars
                    .iter()
                    .map(|(lhs, rhs)| {
                        let rhs = pass(rhs);
                        let mut lhs = lhs.clone();
                        //lhs.operand = Some(rhs.clone());
                        (lhs, rhs)
                    })
                    .collect();
                make_rec(vars, pass(body), source.clone())
            }

            TreeNode::RecStar(vars, body, source) => {
                let vars = vars
                    .iter()
                    .map(|(lhs, rhs)| {
                        let rhs = pass(rhs);
                        let mut lhs = lhs.clone();
                       // lhs.operand = Some(rhs.clone());
                        (lhs, rhs)
                    })
                    .collect();
                make_rec_star(vars, pass(body), source.clone())
            }

            TreeNode::Fix(lhs, rhs, body, source) => {
                let rhs = rhs
                    .iter()
                    .zip(lhs.iter())
                    .map(|(x, var)| {
                        let mut var = var.clone();

                        let TreeNode::Proc(proc) = &*pass(&Rc::new(TreeNode::Proc(x.clone())))
                        else {
                            unreachable!()
                        };
                        //var.operand = Some(Rc::new(TreeNode::Proc(proc.clone())));
                        proc.clone()
                    })
                    .collect();
                make_fix(lhs.clone(), rhs, pass(body), source.clone())
            }

            TreeNode::Proc(proc) => Rc::new(TreeNode::Proc(make_proc(
                proc.name.clone(),
                proc.cases
                    .iter()
                    .map(|x| {
                        Rc::new(ProcCase {
                            info: x.info.clone(),
                            body: pass(&x.body),
                        })
                    })
                    .collect(),
            ))),

            TreeNode::Seq(seq, source) => {
                make_seq(seq.iter().map(|x| pass(&*x)).collect(), source.clone())
            }

            TreeNode::Mutate(var, exp, source) => {
                make_mutate(var.clone(), pass(exp), source.clone())
            }
            TreeNode::Test(test, consequent, alternative, source) => make_test(
                pass(test),
                pass(consequent),
                pass(alternative),
                source.clone(),
            ),

            TreeNode::FunCall(op, operands, srcloc) => {
                match &**op {
                    TreeNode::Proc(proc) => {
                        if let Some(case) = match_formals(&**proc, &operands) {
                            return make_bind(
                                case.info.formals.clone(),
                                operands.iter().map(|x| pass(&*x)).collect(),
                                pass(&case.body),
                                srcloc.clone(),
                            );
                        }
                    }

                    /*TreeNode::Ref(var, _) => {
                        if !var.mutated {
                            match var.operand {
                                Some(ref operand) => {
                                    if let TreeNode::Proc(proc) = &**operand {
                                        if let Some(case) = match_formals(&**proc, &operands) {
                                            return make_bind(
                                                case.info.formals.clone(),
                                                operands.iter().map(|x| pass(&*x)).collect(),
                                                pass(&case.body),
                                                srcloc.clone(),
                                            );
                                        }
                                    }
                                }

                                _ => ()
                            }
                        }
                    }*/

                    _ => (),
                }

                return make_fun_call(
                    pass(op),
                    &operands.iter().map(|x| pass(&*x)).collect::<Vec<_>>(),
                    srcloc.clone(),
                );
            }

            TreeNode::MultiValueCall(producer, consumer, srcloc) => {
                make_multi_value_call(pass(producer), pass(consumer), srcloc.clone())
            }

            TreeNode::MultiValueLet(expr, lhs, body, source) => {
                make_multi_value_let(pass(expr), lhs.clone(), pass(body), source.clone())
            }

            TreeNode::MultiValues(values, srcloc) => {
                make_multi_values(values.iter().map(|x| pass(&*x)).collect(), srcloc.clone())
            }

            TreeNode::Constant(_, _) => x.clone(),
            TreeNode::Ref(_, _) => x.clone(),
            TreeNode::PrimRef(_, _) => x.clone(),

            _ => unreachable!("Unexpected node for pass-let: {:?}", x),
        }
    }

    pass(&x)
}
