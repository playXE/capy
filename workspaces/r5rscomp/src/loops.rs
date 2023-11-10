//! Recover loops
//!
//! TODO: Contification

use std::borrow::Cow;

use crate::{rc::Rc, sexpr::Symbol, tree_il::*};

pub fn pass_loops(x: &Rc<TreeNode>) -> Rc<TreeNode> {
    fn match_formals(op: &Proc, args: &[Rc<TreeNode>]) -> Option<Rc<ProcCase>> {
        fn check(info: &CaseInfo, args: &[Rc<TreeNode>]) -> bool {
            // TODO: Here it is possible to match on formals with rest
            // arguments as well, but the resultant must create a
            // list.
            info.proper && info.formals.len() == args.len()
        }

        op.cases
            .iter()
            .find(|case| check(&case.info, args))
            .cloned()
    }

    fn is_misused(
        x: &Rc<TreeNode>,
        tail: bool,
        name: &Rc<Variable>,
        formals: &[Rc<Variable>],
    ) -> bool {
        match &**x {
            TreeNode::Bind(_, rhs, body, _) => {
                rhs.iter().any(|x| is_misused(x, false, name, formals))
                    || is_misused(body, tail, name, formals)
            }

            TreeNode::Fix(_, rhs, body, _) => {
                rhs.iter().any(|x| {
                    x.cases
                        .iter()
                        .map(|x| &x.body)
                        .any(|x| is_misused(x, false, name, formals))
                }) || is_misused(body, tail, name, formals)
            }

            TreeNode::Proc(proc) => proc
                .cases
                .iter()
                .any(|case| is_misused(&case.body, true, name, formals)),

            TreeNode::Seq(seq, _) => {
                for i in 0..seq.len() - 1 {
                    if is_misused(&seq[i], false, name, formals) {
                        return true;
                    }
                }

                is_misused(&seq[seq.len() - 1], tail, name, formals)
            }

            TreeNode::Mutate(_, x, _) => is_misused(x, false, name, formals),
            TreeNode::Test(test, consequent, alternate, _) => {
                is_misused(test, false, name, formals)
                    || is_misused(consequent, tail, name, formals)
                    || is_misused(alternate, tail, name, formals)
            }

            TreeNode::FunCall(op, operands, _) => {
                (op.is_ref()
                    && op.ref_variable() == Some(name)
                    && (!tail || !(operands.len() == formals.len())))
                    || (!op.is_ref() && is_misused(op, false, name, formals))
                        && operands.iter().any(|x| is_misused(x, false, name, formals))
            }

            TreeNode::Ref(var, _) => var == name,
            TreeNode::TagBody(body, _) => is_misused(body, tail, name, formals),
            TreeNode::MultiValueCall(producer, consumer, _) => {
                is_misused(producer, false, name, formals)
                    || is_misused(consumer, tail, name, formals)
            }

            TreeNode::MultiValueLet(expr, _, body, _) => {
                is_misused(expr, false, name, formals) || is_misused(body, tail, name, formals)
            }

            TreeNode::MultiValues(vals, _) => {
                vals.iter().any(|x| is_misused(x, false, name, formals))
            }

            _ => false,
        }
    }

    /// This returns true if the variable might be captured (it
    /// appears free in some lambda).
    fn is_captured(x: &Rc<TreeNode>, in_lambda: bool, name: &Rc<Variable>) -> bool {
        match &**x {
            TreeNode::Ref(var, _) => in_lambda && name == var,
            TreeNode::Proc(proc) => proc
                .cases
                .iter()
                .any(|case| is_captured(&case.body, true, name)),

            TreeNode::Bind(_, rhs, body, _) => {
                rhs.iter().any(|x| is_captured(x, in_lambda, name))
                    || is_captured(body, in_lambda, name)
            }
            TreeNode::Fix(_, rhs, body, _) => {
                rhs.iter().any(|x| {
                    x.cases
                        .iter()
                        .map(|x| &x.body)
                        .any(|x| is_captured(x, true, name))
                }) || is_captured(body, in_lambda, name)
            }

            TreeNode::Seq(seq, _) => seq.iter().any(|x| is_captured(x, in_lambda, name)),
            TreeNode::Mutate(_, x, _) => is_captured(x, in_lambda, name),
            TreeNode::Test(test, consequent, alternate, _) => {
                is_captured(test, in_lambda, name)
                    || is_captured(consequent, in_lambda, name)
                    || is_captured(alternate, in_lambda, name)
            }

            TreeNode::FunCall(op, operands, _) => {
                is_captured(op, in_lambda, name)
                    || operands.iter().any(|x| is_captured(x, in_lambda, name))
            }

            TreeNode::TagBody(body, _) => is_captured(body, in_lambda, name),
            TreeNode::MultiValueCall(producer, consumer, _) => {
                is_captured(producer, in_lambda, name) || is_captured(consumer, in_lambda, name)
            }
            TreeNode::MultiValueLet(expr, _, body, _) => {
                is_captured(expr, in_lambda, name) || is_captured(body, in_lambda, name)
            }

            TreeNode::MultiValues(vals, _) => vals.iter().any(|x| is_captured(x, in_lambda, name)),
            
            _ => false,
        }
    }

    /// The fix must contain a single procedure and the body must be
    /// a call to one of the proccases in that procedure. The name of
    /// the procedure must furthermore not be misused. None of the
    /// formals of the proccase can be captured in the proccase.
    fn is_optimizeable_loop(
        lhs: &[Rc<Variable>],
        rhs: &[Rc<Proc>],
        body: &Rc<TreeNode>,
    ) -> Option<Rc<ProcCase>> {
        if lhs.len() == 1 {
            let TreeNode::FunCall(op, operands, _) = &**body else {
                return None;
            };

            if op.is_ref() {
                if let Some(case) = match_formals(&rhs[0], operands) {
                    let name = op.ref_variable().unwrap();
                    if name == &lhs[0]
                        && !name.mutated
                        && !case.info.formals.iter().any(|x| x.mutated)
                        && !is_misused(body, true, name, &case.info.formals)
                        && !is_misused(
                            &Rc::new(TreeNode::Proc(rhs[0].clone())),
                            false,
                            name,
                            &case.info.formals,
                        )
                        && !(is_captured(&case.body, false, name)
                            || operands.iter().any(|x| is_captured(x, false, name)))
                        && !(case.info.formals.iter().any(|var| {
                            is_captured(&case.body, false, var)
                                || operands.iter().any(|x| is_captured(x, false, var))
                        }))
                    {
                        return Some(case);
                    }
                }
            }
        }

        None
    }

    /// This generates a replacement for the fix record. The
    /// replacement is a bind in which there's an tagbody.
    /// The first body of the fix becomes the bind and any later
    /// calls to “name” are replaced with bind, set! and goto.
    fn optimize_loop(
        name: Rc<Variable>,
        proc: Rc<Proc>,
        case: Rc<ProcCase>,
        operands: &[Rc<TreeNode>],
    ) -> Rc<TreeNode> {
        let lhs = &case.info.formals;
        let mut label = Rc::new(TreeNode::TagBody(case.body.clone(), proc.source.clone()));

        fn rewrite(
            x: &Rc<TreeNode>,
            name: &Rc<Variable>,
            formals: &[Rc<Variable>],
            label: &Rc<TreeNode>,
        ) -> Rc<TreeNode> {
            match &**x {
                TreeNode::Bind(lhs, rhs, body, src) => make_bind(
                    lhs.clone(),
                    rhs.iter()
                        .map(|x| rewrite(x, name, formals, label))
                        .collect(),
                    rewrite(body, name, formals, label),
                    src.clone(),
                ),
                TreeNode::Fix(lhs, rhs, body, src) => make_fix(
                    lhs.clone(),
                    rhs.iter()
                        .map(|x| {
                            rewrite(&Rc::new(TreeNode::Proc(x.clone())), name, formals, label)
                                .proc()
                                .unwrap()
                                .clone()
                        })
                        .collect(),
                    rewrite(body, name, formals, label),
                    src.clone(),
                ),

                TreeNode::Proc(proc) => {
                    let mut proc = proc.clone();
                    for case in proc.cases.iter_mut() {
                        case.body = rewrite(&case.body, name, formals, label);
                    }

                    Rc::new(TreeNode::Proc(proc))
                }

                TreeNode::Seq(seq, src) => make_seq(
                    seq.iter()
                        .map(|x| rewrite(x, name, formals, label))
                        .collect(),
                    src.clone(),
                ),

                TreeNode::Mutate(name, x, src) => {
                    make_mutate(name.clone(), rewrite(x, name, formals, label), src.clone())
                }

                TreeNode::Test(test, consequent, alternate, src) => make_test(
                    rewrite(test, name, formals, label),
                    rewrite(consequent, name, formals, label),
                    rewrite(alternate, name, formals, label),
                    src.clone(),
                ),

                TreeNode::FunCall(op, operands, src) => {
                    let operands = operands
                        .iter()
                        .map(|x| rewrite(x, name, formals, label))
                        .collect::<Vec<_>>();
                    if op.is_ref() && op.ref_variable() == Some(name) {
                        // residualize a goto
                        let temps = formals
                            .iter()
                            .map(|_| {
                                make_variable(Rc::new(Symbol::Uninterned(Cow::Borrowed("temp"))))
                            })
                            .collect::<Vec<_>>();
                        let body = {
                            if temps.is_empty() {
                                make_goto(label.clone(), src.clone())
                            } else {
                                let mut seq = vec![];

                                for (lhs, temp) in formals.iter().zip(temps.iter()) {
                                    seq.push(make_mutate(
                                        lhs.clone(),
                                        make_ref(temp.clone(), src.clone()),
                                        src.clone(),
                                    ));
                                }

                                seq.push(make_goto(label.clone(), src.clone()));

                                make_seq(seq, src.clone())
                            }
                        };
                        return make_bind(temps, operands, body, src.clone());
                    }

                    make_fun_call(rewrite(op, name, formals, label), &operands, src.clone())
                }
                TreeNode::MultiValueCall(producer, consumer, src) => make_multi_value_call(
                    rewrite(producer, name, formals, label),
                    rewrite(consumer, name, formals, label),
                    src.clone(),
                ),
                TreeNode::MultiValueLet(expr, vars, body, src) => make_multi_value_let(
                    rewrite(expr, name, formals, label),
                    vars.clone(),
                    rewrite(body, name, formals, label),
                    src.clone(),
                ),

                TreeNode::MultiValues(values, src) => make_multi_values(
                    values
                        .iter()
                        .map(|x| rewrite(x, name, formals, label))
                        .collect(),
                    src.clone(),
                ),

                TreeNode::TagBody(_, _) => {
                    let TreeNode::TagBody(ref mut body, _) = &mut *x.clone() else {
                        unreachable!()
                    };
                    *body = rewrite(body, name, formals, label);
                    return x.clone();
                    //make_tag_body(rewrite(body, name, formals, label), src.clone())
                }
                TreeNode::Ref(_, _)
                | TreeNode::Constant(_, _)
                | TreeNode::PrimRef(_, _)
                | TreeNode::Goto(_, _) => x.clone(),

                _ => unreachable!("rec and rec* must be desugared before pass_loops"),
            }
        }

        let body = rewrite(&case.body, &name, &lhs, &label);

        {
            let TreeNode::TagBody(ref mut lbody, _) = &mut *label else {
                unreachable!()
            };
            *lbody = body;
        }

        make_bind(lhs.clone(), operands.to_vec(), label, proc.source.clone())
    }

    fn pass(x: &Rc<TreeNode>) -> Rc<TreeNode> {
        match &**x {
            TreeNode::Bind(lhs, rhs, body, src) => make_bind(
                lhs.clone(),
                rhs.iter().map(|x| pass(x)).collect(),
                pass(body),
                src.clone(),
            ),

            TreeNode::Fix(lhs, rhs, body, src) => {
                let rhs = rhs
                    .iter()
                    .map(|x| {
                        pass(&Rc::new(TreeNode::Proc(x.clone())))
                            .proc()
                            .unwrap()
                            .clone()
                    })
                    .collect::<Vec<_>>();
                if let Some(case) = is_optimizeable_loop(&lhs, &rhs, &body) {
                    let operands = body
                        .funcall_operands()
                        .iter()
                        .map(|x| pass(x))
                        .collect::<Vec<_>>();
                    optimize_loop(lhs[0].clone(), rhs[0].clone(), case, &operands)
                } else {
                    make_fix(lhs.clone(), rhs, pass(body), src.clone())
                }
            }

            TreeNode::Proc(x) => {
                let mut x = x.clone();
                for case in x.cases.iter_mut() {
                    case.body = pass(&case.body);
                }

                return Rc::new(TreeNode::Proc(x.clone()));
            }

            TreeNode::Seq(seq, src) => make_seq(seq.iter().map(|x| pass(x)).collect(), src.clone()),
            TreeNode::Mutate(name, x, src) => make_mutate(name.clone(), pass(x), src.clone()),
            TreeNode::FunCall(op, operands, src) => make_fun_call(
                pass(op),
                &operands.iter().map(|x| pass(x)).collect::<Vec<_>>(),
                src.clone(),
            ),

            TreeNode::MultiValueCall(producer, consumer, src) => {
                make_multi_value_call(pass(producer), pass(consumer), src.clone())
            }
            TreeNode::MultiValueLet(expr, vars, body, src) => {
                make_multi_value_let(pass(expr), vars.clone(), pass(body), src.clone())
            }
            TreeNode::MultiValues(vals, src) => {
                make_multi_values(vals.iter().map(|x| pass(x)).collect(), src.clone())
            }

            TreeNode::TagBody(_, _) => {
                let TreeNode::TagBody(ref mut body, _) = &mut *x.clone() else {
                    unreachable!()
                };

                *body = pass(body);

                x.clone()
            }

            _ => x.clone(),
        }
    }

    pass(x)
}
