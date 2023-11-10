//! Makes proper ANF tree from Tree IL
/* 
use std::{sync::atomic::AtomicUsize, borrow::Cow};

use crate::{rc::Rc, tree_il::{TreeNode, Variable, make_variable}, sexpr::Symbol};

type K = dyn FnMut(Rc<TreeNode>) -> Rc<TreeNode>;

pub fn pass_anf(x: &Rc<TreeNode>) -> Rc<TreeNode> {
    fn genvar() -> Rc<Variable> {
        
        make_variable(Rc::new(Symbol::Uninterned(Cow::Borrowed("anf"))))
    }


    fn normalize(x: &Rc<TreeNode>, k: &mut K) -> Rc<TreeNode> {
        match &**x {
            TreeNode::Proc(proc) => {
                let mut proc = proc.clone();
                for case in proc.cases.iter_mut() {
                    case.body = normalize_term(&case.body);
                }

                k(Rc::new(TreeNode::Proc(proc)))
            }
            _ if !x.might_cause_side_effects() => {
                k(x.clone())
            }

            TreeNode::Bind(lhs, rhs, body, src) => {
                let mut normalized = Vec::new();

                for rhs in rhs.iter() {
                    normalized.push(normalize_term(rhs));
                }
            }


        }
    }    

    fn normalize_term(x: &Rc<TreeNode>) -> Rc<TreeNode> {
        normalize(x, &mut |x| x)
    }

    normalize_term(x)
}*/