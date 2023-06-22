//! A-Normal Form (ANF) conversion

use crate::{
    object::{ObjectHeader, Type},
    symbol::gensym,
    value::Value,
};

use super::{make_iform, Call, CallFlag, IForm, If, LRef, LVar, Let};
use rsgc::{prelude::Handle, system::arraylist::ArrayList, thread::Thread};

pub fn normalize(
    thread: &mut Thread,
    mut iform: Handle<IForm>,
    mut k: &mut dyn FnMut(&mut Thread, Handle<IForm>) -> Handle<IForm>,
) -> Handle<IForm> {
    let orig = iform;
    match &mut *iform {
        IForm::Lambda(lam) => {
            lam.body = normalize_term(thread, lam.body);

            k(thread, orig)
        }

        IForm::LSet(lset) => normalize(thread, lset.value, &mut move |t, value| {
            t.write_barrier(orig);
            lset.value = value;
            k(t, orig)
        }),

        IForm::GSet(gset) => normalize(thread, gset.value, &mut move |t, value| {
            t.write_barrier(orig);
            gset.value = value;
            k(t, orig)
        }),

        IForm::LRef(_) => k(thread, orig),
        IForm::GRef(_) => k(thread, orig),
        IForm::If(cond) => normalize_name(thread, cond.cond, &mut |thread, t| {
            let cons = normalize_term(thread, cond.cons);
            let alt = normalize_term(thread, cond.alt);

            make_iform(IForm::If(If {
                origin: cond.origin,
                cond: t,
                cons,
                alt,
            }))
        }),

        IForm::Call(call) => normalize_name(thread, call.proc, &mut |thread, aproc| {
            normalize_names(thread, &call.args, |thread, vars| {
                let call = Call {
                    origin: call.origin,
                    proc: aproc,
                    args: {
                        let mut args = ArrayList::with_capacity(thread, vars.len());

                        for &var in vars {
                            args.push(thread, var);
                        }

                        args
                    },
                    flag: CallFlag::None,
                };

                k(thread, make_iform(IForm::Call(call)))
            })
        }),

        IForm::Const(_) => k(thread, orig),
        IForm::Let(var) => {
            todo!()
        }
        _ => todo!(),
    }
}

pub fn normalize_name(
    thread: &mut Thread,
    m: Handle<IForm>,
    k: &mut dyn FnMut(&mut Thread, Handle<IForm>) -> Handle<IForm>,
) -> Handle<IForm> {
    if is_atomic(m) {
        return k(thread, m);
    }

    normalize(thread, m, &mut |thread, n| {
        let lvar = thread.allocate(LVar {
            header: ObjectHeader::new(Type::LVar),
            name: gensym("@"),
            initval: Some(n),
            ref_count: 0,
            set_count: 0,
        });

        let let_ = Let {
            origin: Value::encode_undefined_value(),
            scope: super::LetScope::Let,
            lvars: ArrayList::from_slice(thread, &[lvar]),
            inits: ArrayList::from_slice(thread, &[n]),
            body: k(thread, make_iform(IForm::LRef(LRef { lvar }))),
        };
      
        make_iform(IForm::Let(let_))
    })
}

pub fn normalize_names(
    thread: &mut Thread,
    mstar: &[Handle<IForm>],
    mut k: impl FnMut(&mut Thread, &[Handle<IForm>]) -> Handle<IForm>,
) -> Handle<IForm> {
    if mstar.is_empty() {
        return k(thread, &[]);
    }

    if mstar.iter().all(|&x| is_atomic(x)) {
        return k(thread, mstar);
    }

    let mut vars = ArrayList::with_capacity(thread, mstar.len());
    let mut inits = ArrayList::with_capacity(thread, mstar.len());
    let mut atomic = ArrayList::with_capacity(thread, mstar.len());
    for (i, &m) in mstar.iter().enumerate() {
        if is_atomic(m) {
            atomic.push(thread, m);
            continue;
        }
        let term = normalize_term(thread, m);
        let lvar = thread.allocate(LVar {
            header: ObjectHeader::new(Type::LVar),
            name: gensym("@"),
            initval: Some(term),
            ref_count: 0,
            set_count: 0,
        });

        vars.push(thread, lvar);
        inits.push(thread, term);

        atomic.push(thread, make_iform(IForm::LRef(LRef { lvar })));
    }

    let body = k(thread, &atomic);
    let let_ = Let {
        origin: Value::encode_undefined_value(),
        scope: super::LetScope::Let,
        lvars: vars,
        inits,
        body,
    };

    make_iform(IForm::Let(let_))
}

pub fn normalize_term(thread: &mut Thread, form: Handle<IForm>) -> Handle<IForm> {
    normalize(thread, form, &mut |_, x| x)
}

pub fn is_atomic(iform: Handle<IForm>) -> bool {
    match &*iform {
        IForm::LRef(_) 
        | IForm::Lambda(_) 
        | IForm::GRef(_) 
        | IForm::Const(_) => true,
        _ => false,
    }
}