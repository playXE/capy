use crate::runtime::{value::ScmValue, context::Context};

use super::PRIMITIVE_LIST;
use r7rs_parser::*;
use rsgc::{
    prelude::{Allocation, Handle, Object},
    system::arraylist::ArrayList,
};

pub fn is_value(name: &str) -> bool {
    PRIMITIVE_LIST.contains(name)
}

pub enum Anf {
    Atomic(Atomic),
    Call(Atomic, ArrayList<Atomic>),
    If(Atomic, Handle<Anf>, Handle<Anf>),
    Let(ScmValue, Handle<Anf>, Handle<Anf>),
    Seq(ArrayList<Handle<Anf>>),
    Set(ScmValue, Handle<Anf>),
}

impl Object for Anf {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        match self {
            Anf::Atomic(a) => a.trace(visitor),
            Anf::Call(a, args) => {
                a.trace(visitor);
                args.trace(visitor);
            }
            Anf::If(a, t, f) => {
                a.trace(visitor);
                t.trace(visitor);
                f.trace(visitor);
            }
            Anf::Let(v, e, b) => {
                v.trace(visitor);
                e.trace(visitor);
                b.trace(visitor);
            }

            Anf::Seq(exprs) => exprs.trace(visitor),

            Anf::Set(v, e) => {
                v.trace(visitor);
                e.trace(visitor);
            }
        }
    }
}

impl Allocation for Anf {}

pub enum Atomic {
    Void,
    Int(i64),
    Float(f64),
    BigInt(ScmValue),
    Str(ScmValue),
    Var(ScmValue),
    Bool(bool),
    Lambda(Handle<Lambda>),
}

impl Object for Atomic {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        match self {
            Atomic::BigInt(b) => b.trace(visitor),
            Atomic::Str(s) => s.trace(visitor),
            Atomic::Var(v) => v.trace(visitor),
            Atomic::Lambda(l) => l.trace(visitor),
            _ => {}
        }
    }
}

impl Allocation for Atomic {}

pub struct Lambda {
    pub name: ScmValue,
    pub args: ArrayList<ScmValue>,
    pub variadic: bool,
    pub body: ScmValue,
}

impl Object for Lambda {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.name.trace(visitor);
        self.args.trace(visitor);
        self.body.trace(visitor);
    }
}

impl Allocation for Lambda {}

use r7rs_parser::expr::*;


pub fn alpha_normalize(ctx: &mut Context, expr: Expr<NoIntern>) -> Handle<Anf> {

   

    todo!()
}