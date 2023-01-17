use std::sync::atomic::AtomicUsize;

use crate::{
    compiler::Compiler,
    prelude::{code::Code, libraries::control_flow::Continuation, syntax_rules::SyntaxRules, *},
    utilities::arraylist::ArrayList,
};

use super::structure::{StructType, StructProperty};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Arity {
    Exact(usize),
    AtLeast(usize),
}

impl Object for Arity {}
impl Allocation for Arity {
    const NO_HEAP_PTRS: bool = true;
}

static COUNT: AtomicUsize = AtomicUsize::new(0);

pub struct Procedure {
    pub(crate) id: usize,
    pub(crate) kind: ProcedureKind,
    pub(crate) module: Handle<Library>,
}

impl Procedure {
    pub fn name(&self) -> String {
        match &self.kind {
            ProcedureKind::Primitive(name, _, _) => name.to_string(),
            ProcedureKind::Closure(ClosureType::Named(name), _, _, _) => name.to_string(),
            _ => format!("<closure {}>", self.id),
        }
    }

    pub fn embedded_name(&self) -> String {
        match &self.kind {
            ProcedureKind::Primitive(name, _, _) => name.to_string(),
            ProcedureKind::Closure(ClosureType::Named(name), _, _, _) => name.to_string(),
            _ => format!("{}", self.id),
        }
    }

    pub fn arity(&self) -> &[Arity] {
        match self.kind {
            ProcedureKind::Primitive(_, ref imp, _) => match imp {
                Implementation::Native0(_) => &[Arity::Exact(0)],
                Implementation::Native1(_) => &[Arity::Exact(1)],
                Implementation::Native2(_) => &[Arity::Exact(2)],
                Implementation::Native3(_) => &[Arity::Exact(3)],
                Implementation::Native4(_) => &[Arity::Exact(4)],
                Implementation::Native0O(_) => &[Arity::Exact(0), Arity::Exact(1)],
                Implementation::Native1O(_) => &[Arity::Exact(1), Arity::Exact(2)],
                Implementation::Native2O(_) => &[Arity::Exact(2), Arity::Exact(3)],
                Implementation::Native3O(_) => &[Arity::Exact(3), Arity::Exact(4)],
                Implementation::Native0OO(_) => {
                    &[Arity::Exact(0), Arity::Exact(1), Arity::Exact(2)]
                }
                Implementation::Native1OO(_) => {
                    &[Arity::Exact(1), Arity::Exact(2), Arity::Exact(3)]
                }
                Implementation::Native2OO(_) => {
                    &[Arity::Exact(2), Arity::Exact(3), Arity::Exact(4)]
                }
                Implementation::Native3OO(_) => {
                    &[Arity::Exact(3), Arity::Exact(4), Arity::Exact(5)]
                }
                Implementation::Native0R(_) => &[Arity::AtLeast(0)],
                Implementation::Native1R(_) => &[Arity::AtLeast(1)],
                Implementation::Native2R(_) => &[Arity::AtLeast(2)],
                Implementation::Native3R(_) => &[Arity::AtLeast(3)],
                Implementation::Native4R(_) => &[Arity::AtLeast(4)],
                Implementation::Apply(_) | Implementation::Eval(_) => &[Arity::AtLeast(0)],
            },

            ProcedureKind::Closure(ClosureType::Continuation, _, _, _) => &[Arity::Exact(1)],
            ProcedureKind::Closure(_, _, _, ref code) => &code.arity(),
            ProcedureKind::Parameter(_) => &[Arity::Exact(0), Arity::Exact(1)],
            ProcedureKind::Transformer(_) => &[Arity::Exact(1)],
            ProcedureKind::RawContinuation(_) => &[Arity::Exact(1)],
        }
    }

    pub(crate) fn new_id() -> usize {
        COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
    }
}

impl Object for Procedure {
    fn trace(&self, visitor: &mut dyn Visitor) {
        self.kind.trace(visitor);
        self.module.trace(visitor);
    }
}

impl Allocation for Procedure {}

pub enum ProcedureKind {
    Primitive(Handle<Str>, Implementation, Option<FormCompiler>),
    Parameter(Handle<Pair>),
    Closure(
        ClosureType,
        Value,
        Option<Handle<Array<Handle<Upvalue>>>>,
        Handle<Code>,
    ),
    RawContinuation(Handle<Continuation>),
    Transformer(Handle<SyntaxRules>),
}

impl Object for ProcedureKind {
    fn trace(&self, visitor: &mut dyn Visitor) {
        match self {
            ProcedureKind::Primitive(name, _, _) => name.trace(visitor),
            ProcedureKind::Parameter(ls) => ls.trace(visitor),
            ProcedureKind::Closure(ty, proc, free_vars, code) => {
                match ty {
                    ClosureType::Named(name) => name.trace(visitor),
                    ClosureType::Constructor(ty) | 
                    ClosureType::Mutator(ty)
                    | ClosureType::Accessor(ty)
                    | ClosureType::Predicate(ty) => ty.trace(visitor),
                
                    ClosureType::PropertyAccessor(ty) | ClosureType::PropertyPredicate(ty) => ty.trace(visitor),
                    _ => ()
                }
                proc.trace(visitor);
                free_vars.trace(visitor);
                code.trace(visitor);
            }
            ProcedureKind::RawContinuation(state) => state.trace(visitor),
            ProcedureKind::Transformer(rules) => rules.trace(visitor),
        }
    }
}

/// There are three types of closures:
///    1. Anonymous closures: closures that are not named
///    2. Named closures: Closures that are given a name
///    3. Continuations: These are unnamed closures generated by `call-with-current-continuation`
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ClosureType {
    Anonymous,
    Named(Handle<Str>),
    Continuation,
    Constructor(Handle<StructType>),
    Accessor(Handle<StructType>),
    Mutator(Handle<StructType>),
    Predicate(Handle<StructType>),
    PropertyAccessor(Handle<StructProperty>),
    PropertyPredicate(Handle<StructProperty>),
}

pub enum Implementation {
    Apply(fn(&mut Context, &Arguments) -> ScmResult<(Handle<Procedure>, ArrayList<Value>)>),
    Eval(fn(&mut Context, &Arguments) -> ScmResult<Handle<Code>>),
    Native0(fn(&mut Context) -> ScmResult),
    Native1(fn(&mut Context, Value) -> ScmResult),
    Native2(fn(&mut Context, Value, Value) -> ScmResult),
    Native3(fn(&mut Context, Value, Value, Value) -> ScmResult),
    Native4(fn(&mut Context, Value, Value, Value, Value) -> ScmResult),
    Native0O(fn(&mut Context, Option<Value>) -> ScmResult),
    Native1O(fn(&mut Context, Value, Option<Value>) -> ScmResult),
    Native2O(fn(&mut Context, Value, Value, Option<Value>) -> ScmResult),
    Native3O(fn(&mut Context, Value, Value, Value, Option<Value>) -> ScmResult),
    Native0OO(fn(&mut Context, Option<Value>, Option<Value>) -> ScmResult),
    Native1OO(fn(&mut Context, Value, Option<Value>, Option<Value>) -> ScmResult),
    Native2OO(fn(&mut Context, Value, Value, Option<Value>, Option<Value>) -> ScmResult),
    Native3OO(fn(&mut Context, Value, Value, Value, Option<Value>, Option<Value>) -> ScmResult),
    Native0R(fn(&mut Context, &Arguments) -> ScmResult),
    Native1R(fn(&mut Context, Value, &Arguments) -> ScmResult),
    Native2R(fn(&mut Context, Value, Value, &Arguments) -> ScmResult),
    Native3R(fn(&mut Context, Value, Value, Value, &Arguments) -> ScmResult),
    Native4R(fn(&mut Context, Value, Value, Value, Value, &Arguments) -> ScmResult),
}

impl Into<Implementation>
    for fn(&mut Context, &Arguments) -> ScmResult<(Handle<Procedure>, ArrayList<Value>)>
{
    fn into(self) -> Implementation {
        Implementation::Apply(self)
    }
}

impl Into<Implementation> for fn(&mut Context, &Arguments) -> ScmResult<Handle<Code>> {
    fn into(self) -> Implementation {
        Implementation::Eval(self)
    }
}

impl Into<Implementation> for fn(&mut Context) -> ScmResult {
    fn into(self) -> Implementation {
        Implementation::Native0(self)
    }
}

impl Into<Implementation> for fn(&mut Context, Value) -> ScmResult {
    fn into(self) -> Implementation {
        Implementation::Native1(self)
    }
}

impl Into<Implementation> for fn(&mut Context, Value, Value) -> ScmResult {
    fn into(self) -> Implementation {
        Implementation::Native2(self)
    }
}

impl Into<Implementation> for fn(&mut Context, Value, Value, Value) -> ScmResult {
    fn into(self) -> Implementation {
        Implementation::Native3(self)
    }
}

impl Into<Implementation> for fn(&mut Context, Value, Value, Value, Value) -> ScmResult {
    fn into(self) -> Implementation {
        Implementation::Native4(self)
    }
}

impl Into<Implementation> for fn(&mut Context, Option<Value>) -> ScmResult {
    fn into(self) -> Implementation {
        Implementation::Native0O(self)
    }
}

impl Into<Implementation> for fn(&mut Context, Value, Option<Value>) -> ScmResult {
    fn into(self) -> Implementation {
        Implementation::Native1O(self)
    }
}

impl Into<Implementation> for fn(&mut Context, Value, Value, Option<Value>) -> ScmResult {
    fn into(self) -> Implementation {
        Implementation::Native2O(self)
    }
}

impl Into<Implementation> for fn(&mut Context, Value, Value, Value, Option<Value>) -> ScmResult {
    fn into(self) -> Implementation {
        Implementation::Native3O(self)
    }
}

impl Into<Implementation> for fn(&mut Context, Option<Value>, Option<Value>) -> ScmResult {
    fn into(self) -> Implementation {
        Implementation::Native0OO(self)
    }
}

impl Into<Implementation> for fn(&mut Context, Value, Option<Value>, Option<Value>) -> ScmResult {
    fn into(self) -> Implementation {
        Implementation::Native1OO(self)
    }
}

impl Into<Implementation>
    for fn(&mut Context, Value, Value, Option<Value>, Option<Value>) -> ScmResult
{
    fn into(self) -> Implementation {
        Implementation::Native2OO(self)
    }
}

impl Into<Implementation>
    for fn(&mut Context, Value, Value, Value, Option<Value>, Option<Value>) -> ScmResult
{
    fn into(self) -> Implementation {
        Implementation::Native3OO(self)
    }
}

impl Into<Implementation> for fn(&mut Context, &Arguments) -> ScmResult {
    fn into(self) -> Implementation {
        Implementation::Native0R(self)
    }
}

impl Into<Implementation> for fn(&mut Context, Value, &Arguments) -> ScmResult {
    fn into(self) -> Implementation {
        Implementation::Native1R(self)
    }
}

impl Into<Implementation> for fn(&mut Context, Value, Value, &Arguments) -> ScmResult {
    fn into(self) -> Implementation {
        Implementation::Native2R(self)
    }
}

impl Into<Implementation> for fn(&mut Context, Value, Value, Value, &Arguments) -> ScmResult {
    fn into(self) -> Implementation {
        Implementation::Native3R(self)
    }
}

pub type Arguments = [Value];

pub trait ArgumentsExt {
    fn values(&self, ctx: &mut Context) -> Value;
}

impl ArgumentsExt for Arguments {
    fn values(&self, ctx: &mut Context) -> Value {
        match self.len() {
            0 => Value::void(),
            1 => self[0],
            _ => {
                let mut res = Value::new(Null);
                let mut idx = self.len() as isize;

                while idx > 0 {
                    idx -= 1;
                    res = Value::new(Pair::new(ctx.mutator(), self[idx as usize], res));
                }

                res
            }
        }
    }
}

pub type FormCompiler = fn(&mut Compiler, &mut Context, Value, bool) -> ScmResult<bool>;
