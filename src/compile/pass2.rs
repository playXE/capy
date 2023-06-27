#![allow(dead_code)]
use rsgc::{prelude::Handle, thread::Thread};

use crate::{runtime::string::make_string, runtime::value::Value};

use super::{make_seq, IForm};

fn pass2_rec(
    t: &mut Thread,
    mut iform: Handle<IForm>,
    penv: Value,
    tail: bool,
) -> Result<Handle<IForm>, Value> {
    let orig = iform;
    match &mut *iform {
        IForm::Define(def) => {
            let expr = pass2_rec(t, def.value, penv, false)?;
            t.write_barrier(orig);
            def.value = expr;

            Ok(iform)
        }

        IForm::LRef(_) => lref_eliminate(t, iform),
        IForm::LSet(lset) => {
            let expr = pass2_rec(t, lset.value, penv, false)?;
            t.write_barrier(orig);
            lset.value = expr;

            Ok(iform)
        }

        IForm::If(if_) => {
            let test_form = pass2_rec(t, if_.cond, penv, false)?;
            let then_form = pass2_rec(t, if_.cons, penv, tail)?;
            let else_form = pass2_rec(t, if_.alt, penv, tail)?;
            if let Some(new_form) = branch_cut(orig, test_form, then_form, else_form) {
                Ok(new_form)
            } else {
                Ok(update_if(iform, test_form, then_form, else_form))
            }
        }

        IForm::Let(_var) => {
            todo!()
        }

        _ => Ok(orig),
    }
}

fn branch_cut(
    _iform: Handle<IForm>,
    test_form: Handle<IForm>,
    then_form: Handle<IForm>,
    else_form: Handle<IForm>,
) -> Option<Handle<IForm>> {
    if let IForm::Const(val) = &*test_form {
        let val_form = if !val.is_false() || !val.is_undefined() || !val.is_empty() {
            then_form
        } else {
            else_form
        };

        Some(if let IForm::It = &*val_form {
            test_form
        } else {
            val_form
        })
    } else {
        None
    }
}

pub fn update_if(
    mut iform: Handle<IForm>,
    new_test: Handle<IForm>,
    new_then: Handle<IForm>,
    new_else: Handle<IForm>,
) -> Handle<IForm> {
    if new_then.as_ptr() == new_else.as_ptr() {
        make_seq(Value::encode_undefined_value(), &[new_test, new_then])
    } else {
        let orig = iform;
        let IForm::If(if_) = &mut *iform else {
            return iform;
        };
        Thread::current().write_barrier(orig);
        if_.cond = new_test;
        if_.cons = new_then;
        if_.alt = new_else;

        iform
    }
}

fn lref_eliminate(t: &mut Thread, mut iform: Handle<IForm>) -> Result<Handle<IForm>, Value> {
    let i = iform;
    let IForm::LRef(lref) = &mut *iform else {
        return Ok(iform);
    };

    let mut lvar = lref.lvar;

    if lvar.is_immutable() {
        let initval = lvar.initval;

        match initval {
            None => Ok(iform),
            Some(mut form) => {
                let f = form;
                match &mut *form {
                    IForm::Const(initval) => {
                        lvar.ref_count -= 1;
                        t.write_barrier(iform);
                        *iform = IForm::Const(initval.clone());
                        return Ok(iform);
                    }

                    IForm::LRef(initval) if initval.lvar.is_immutable() => {
                        if i.as_ptr() == f.as_ptr() {
                            return Err(make_string(
                                t,
                                &format!(
                                    "circular reference appeared in letrec bindings: {:?}",
                                    lvar.name
                                ),
                            )
                            .into());
                        }

                        lvar.ref_count -= 1;
                        initval.lvar.ref_count += 1;
                        t.write_barrier(i);
                        lref.lvar = initval.lvar;

                        return lref_eliminate(t, iform);
                    }

                    _ => Ok(iform),
                }
            }
        }
    } else {
        Ok(iform)
    }
}
