//! Assignment elimination transformation
//!
//! Destructively walks the tree and transforms `lset` to `cell-set!` and `lref` to `cell-ref` for all
//! mutable variables. All mutable variables are boxed with `make-cell` call.

use std::collections::HashMap;

use crate::runtime::symbol::scm_intern;

use super::{
    sexpr::Sexpr,
    tree_il::{Call, CallFlag, GRef, IForm, LRef, LVar, Let, LetType},
    P,
};

pub fn assignment_elimination(mut tree: P<IForm>) -> P<IForm> {
    tree.count_refs();

    assignment_elimination_rec(tree)
}

fn make_cell_ref(lvar: P<LVar>) -> IForm {
    let name = scm_intern("box-ref");
    let lref = P::new(IForm::LRef(LRef { lvar }));
    let name = P::new(IForm::GRef(GRef {
        name: Sexpr::Symbol(name),
    }));

    IForm::Call(Call {
        proc: name,
        args: vec![lref],
        flag: CallFlag::None,
    })
}

fn make_cell_set(lvar: P<LVar>, value: P<IForm>) -> IForm {
    let name = scm_intern("box-set!");
    let lref = P::new(IForm::LRef(LRef { lvar }));
    let name = P::new(IForm::GRef(GRef {
        name: Sexpr::Symbol(name),
    }));

    IForm::Call(Call {
        proc: name,
        args: vec![lref, value],
        flag: CallFlag::None,
    })
}

fn make_make_cell(value: P<IForm>) -> IForm {
    let name = scm_intern("make-box");
    let name = P::new(IForm::GRef(GRef {
        name: Sexpr::Symbol(name),
    }));

    IForm::Call(Call {
        proc: name,
        args: vec![value],
        flag: CallFlag::None,
    })
}

fn assignment_elimination_rec(mut tree: P<IForm>) -> P<IForm> {
    match &mut *tree {
        IForm::LRef(lref) => {
           
            if !lref.lvar.is_immutable() {
                *tree = make_cell_ref(lref.lvar.clone());
            }

            tree
        }

        IForm::LSet(lset) => {
            println!("is mutable {}: {}", lset.lvar.name, !lset.lvar.is_immutable());
            if !lset.lvar.is_immutable() {
                *tree = make_cell_set(
                    lset.lvar.clone(),
                    assignment_elimination_rec(lset.value.clone()),
                );
            }

            tree
        }

        IForm::Call(call) => {
            for arg in &mut call.args {
                *arg = assignment_elimination_rec(arg.clone());
            }

            call.proc = assignment_elimination_rec(call.proc.clone());

            tree
        }

        IForm::Seq(seq) => {
            for form in &mut seq.forms {
                *form = assignment_elimination_rec(form.clone());
            }

            tree
        }

        IForm::Define(def) => {
            def.value = assignment_elimination_rec(def.value.clone());
            tree
        }

        IForm::GSet(gset) => {
            gset.value = assignment_elimination_rec(gset.value.clone());
            tree
        }

        IForm::If(cond) => {
            cond.cond = assignment_elimination_rec(cond.cond.clone());
            cond.consequent = assignment_elimination_rec(cond.consequent.clone());
            cond.alternative = assignment_elimination_rec(cond.alternative.clone());
            tree
        }

        IForm::Lambda(lambda) => {
            let mut substitutes = HashMap::new();
            for lvar in lambda.lvars.iter_mut() {
                if !lvar.is_immutable() {
                    let new_lvar = P::new(LVar {
                        name: lvar.name.clone(),
                        arg: false,
                        initval: None,
                        boxed: true,
                        ref_count: lvar.ref_count,
                        set_count: lvar.set_count,
                    });
                    lvar.ref_count = 1;
                    lvar.set_count = 0;
                    substitutes.insert(lvar.clone(), new_lvar);
                }
            }

            let body = assignment_elimination_substitute(lambda.body.clone(), &substitutes);
            let body = assignment_elimination_rec(body);

            let mut lvars = vec![];
            let mut inits = vec![];

            for (arg, lvar) in substitutes.iter() {
                lvars.push(lvar.clone());
                let lref = P::new(IForm::LRef(LRef { lvar: arg.clone() }));
                let make_cell = make_make_cell(lref);
                inits.push(P(make_cell));
            }

            let bindings = P(IForm::Let(Let {
                typ: LetType::Let,
                lvars,
                inits,
                body,
            }));
            
            lambda.body = bindings;

            tree
        }

        IForm::Label(label) => {
            label.body = assignment_elimination_rec(label.body.clone());
            tree
        }

        IForm::Let(var) => {

            var.lvars.iter_mut().zip(var.inits.iter_mut()).for_each(|(lvar, init)| {
                if !lvar.is_immutable() {
                    let make_cell = make_make_cell(init.clone());
                    lvar.boxed = true;
                    *init = P(make_cell);
                }
            }); 
            var.inits.iter_mut().for_each(|init| {
                *init = assignment_elimination_rec(init.clone());
            });
            var.body = assignment_elimination_rec(var.body.clone());
            tree
        }

        IForm::It | IForm::GRef(_) | IForm::Const(_) | IForm::Goto(_) => tree
    }
}

/// Remaps original variables to new, boxed ones. Used for lambda arguments.
fn assignment_elimination_substitute(
    mut tree: P<IForm>,
    substitute: &HashMap<P<LVar>, P<LVar>>,
) -> P<IForm> {
    match &mut *tree {
        
        IForm::LRef(lref) => {
            if let Some(new_lvar) = substitute.get(&lref.lvar) {
                lref.lvar = new_lvar.clone();
            }

            tree
        }

        IForm::LSet(lset) => {
            if let Some(new_lvar) = substitute.get(&lset.lvar) {
                lset.lvar = new_lvar.clone();
            }

            tree
        }

        IForm::GSet(gset) => {
            gset.value = assignment_elimination_substitute(gset.value.clone(), substitute);
            tree
        }

        IForm::Call(call) => {
            for arg in &mut call.args {
                *arg = assignment_elimination_substitute(arg.clone(), substitute);
            }

            call.proc = assignment_elimination_substitute(call.proc.clone(), substitute);

            tree
        }

        IForm::Seq(seq) => {
            for form in &mut seq.forms {
                *form = assignment_elimination_substitute(form.clone(), substitute);
            }

            tree
        }

        IForm::If(cond) => {
            cond.cond = assignment_elimination_substitute(cond.cond.clone(), substitute);
            cond.consequent =
                assignment_elimination_substitute(cond.consequent.clone(), substitute);
            cond.alternative =
                assignment_elimination_substitute(cond.alternative.clone(), substitute);
            tree
        }

        IForm::Let(var) => {
            var.inits.iter_mut().for_each(|init| {
                *init = assignment_elimination_substitute(init.clone(), substitute);
            });

            var.body = assignment_elimination_substitute(var.body.clone(), substitute);

            tree
        }

        IForm::Lambda(lam) => {
            lam.body = assignment_elimination_substitute(lam.body.clone(), substitute);
            tree
        }

        IForm::Define(def) => {
            def.value = assignment_elimination_substitute(def.value.clone(), substitute);

            tree
        }

        IForm::Label(label) => {
            label.body = assignment_elimination_substitute(label.body.clone(), substitute);

            tree
        }

        IForm::Const(_) | IForm::It | IForm::GRef(_) | IForm::Goto(_) => tree,
    }
}