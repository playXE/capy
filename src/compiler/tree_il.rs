use std::{collections::HashSet, hash::Hash};

use crate::{
    gc_protect,
    runtime::{object::scm_vector_set, symbol::scm_intern, value::Value},
    vm::thread::Thread,
};

use super::{
    p::Weak,
    primitives::TRANSPARENT_PRIMITIVE_NAMES,
    sexpr::{sexpr_to_value, Sexpr, SourceLoc},
    P,
};

pub enum IForm {
    Const(Sexpr),
    It,
    LRef(LRef),
    LSet(LSet),
    If(If),
    GRef(GRef),
    GSet(GSet),
    Define(Define),
    Seq(Seq),
    Lambda(P<Lambda>),
    Label(Label),
    Call(Call),
    PrimCall(Option<SourceLoc>, &'static str, Vec<P<IForm>>),
    PrimRef(&'static str),
    Let(Let),
    LetValues(LetValues),
    Fix(Fix),
    Goto(Weak<IForm>),
}

impl PartialEq for IForm {
    fn eq(&self, other: &Self) -> bool {
        self as *const _ == other as *const _
    }
}

impl Eq for IForm {}

impl Hash for IForm {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const Self).hash(state)
    }
}

impl IForm {
    pub fn src(&self) -> Option<SourceLoc> {
        match self {
            Self::Define(def) => def.src,
            Self::If(if_) => if_.src,
            Self::GSet(gset) => gset.src,
            Self::Seq(seq) => seq.src,
            Self::Lambda(lambda) => lambda.src,
            Self::Label(label) => label.src,
            Self::Call(call) => call.src,
            Self::PrimCall(src, _, _) => *src,
            Self::Let(let_) => let_.src,
            Self::LetValues(vals) => vals.src,
            _ => None,
        }
    }

    pub fn lref_lvar(&self) -> Option<&P<LVar>> {
        match self {
            Self::LRef(lref) => Some(&lref.lvar),
            _ => None,
        }
    }

    pub fn is_lref(&self) -> bool {
        matches!(self, Self::LRef(_))
    }

    pub fn is_const(&self) -> bool {
        matches!(self, Self::Const(_))
    }

    pub fn as_const(&self) -> Option<&Sexpr> {
        match self {
            Self::Const(x) => Some(x),
            _ => None,
        }
    }

    pub fn is_transparent(&self) -> bool {
        match self {
            Self::Const(_) | Self::It | Self::Lambda(_) => true,

            Self::LRef(lref) => lref.lvar.is_immutable(),
            Self::GRef(_) => false,
            Self::If(c) => {
                c.cond.is_transparent()
                    && c.consequent.is_transparent()
                    && c.alternative.is_transparent()
            }

            Self::Let(var) => {
                var.inits.iter().all(|x| x.is_transparent()) && var.body.is_transparent()
            }

            Self::LetValues(vals) => vals.init.is_transparent() && vals.body.is_transparent(),
            Self::Seq(seq) => seq.forms.iter().all(|x| x.is_transparent()),
            Self::Call(call) => {
                if !call.args.iter().all(|arg| arg.is_transparent()) {
                    return false;
                }

                match &*call.proc {
                    IForm::LRef(x) => {
                        x.lvar.is_immutable()
                            && x.lvar
                                .initval
                                .as_ref()
                                .filter(|x| match &***x {
                                    IForm::Lambda(lam) => lam.body.is_transparent(),
                                    _ => false,
                                })
                                .is_some()
                    }

                    IForm::Lambda(lam) => lam.body.is_transparent(),
                    _ => false,
                }
            }

            Self::PrimCall(_, name, args) => {
                if !args.iter().all(|arg| arg.is_transparent()) {
                    return false;
                }

                TRANSPARENT_PRIMITIVE_NAMES.contains(name)
            }

            Self::PrimRef(_) => false,
            _ => false,
        }
    }

    /// Counts lvar references and sets.
    pub fn count_refs(&mut self) {
        match self {
            Self::LRef(lref) => lref.lvar.ref_count += 1,
            Self::LSet(lset) => lset.lvar.set_count += 1,
            Self::If(c) => {
                c.cond.count_refs();
                c.consequent.count_refs();
                c.alternative.count_refs();
            }

            Self::Fix(fix) => {
                for lvar in fix.lhs.iter_mut() {
                    // Reset counts
                    lvar.ref_count = 0;
                    lvar.set_count = 0;
                }

                for rhs in fix.rhs.iter_mut() {
                    rhs.body.count_refs();
                }

                fix.body.count_refs();
            }

            Self::Let(var) => {
                for lvar in var.lvars.iter_mut() {
                    // Reset counts
                    lvar.ref_count = 0;
                    lvar.set_count = 0;
                }
                for init in &mut var.inits {
                    init.count_refs();
                }

                var.body.count_refs();
            }

            Self::Seq(seq) => {
                for form in &mut seq.forms {
                    form.count_refs();
                }
            }

            Self::Lambda(lambda) => {
                for lvar in lambda.lvars.iter_mut() {
                    // Reset counts
                    lvar.ref_count = 0;
                    lvar.set_count = 0;
                }
                lambda.body.count_refs();
                lambda.calls.clear();
            }

            Self::Call(call) => {
                call.proc.count_refs();
                for arg in &mut call.args {
                    arg.count_refs();
                }
            }

            Self::Define(define) => {
                define.value.count_refs();
            }

            Self::GSet(gset) => {
                gset.value.count_refs();
            }
            Self::Label(label) => label.body.count_refs(),
            Self::LetValues(lvals) => {
                lvals.lvars.iter_mut().for_each(|lvar| {
                    lvar.ref_count = 0;
                    lvar.set_count = 0;
                });

                lvals.init.count_refs();
                lvals.body.count_refs();
            }
            Self::PrimCall(_, _, args) => {
                for arg in args {
                    arg.count_refs();
                }
            }
            Self::Const(_) | Self::It | Self::GRef(_) | Self::PrimRef(_) | Self::Goto(_) => {}
        }
    }
}

pub struct Define {
    pub src: Option<SourceLoc>,
    pub name: Sexpr,
    pub value: P<IForm>,
}

pub struct LVar {
    pub name: Sexpr,
    pub initval: Option<P<IForm>>,
    pub arg: bool,
    pub boxed: bool,
    pub ref_count: u32,
    pub set_count: u32,
}

impl Hash for LVar {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const Self).hash(state)
    }
}

impl PartialEq for LVar {
    fn eq(&self, other: &Self) -> bool {
        (self as *const Self) == (other as *const Self)
    }
}

impl Eq for LVar {}

impl LVar {
    pub fn is_immutable(&self) -> bool {
        self.set_count == 0
    }
}

impl std::fmt::Debug for LVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#<lvar {}.{:p}>", self.name, self)
    }
}

pub struct LRef {
    pub lvar: P<LVar>,
}

pub struct LSet {
    pub lvar: P<LVar>,
    pub value: P<IForm>,
}

pub struct If {
    pub src: Option<SourceLoc>,
    pub cond: P<IForm>,
    pub consequent: P<IForm>,
    pub alternative: P<IForm>,
}

pub struct GRef {
    pub name: Sexpr,
}

pub struct GSet {
    pub src: Option<SourceLoc>,
    pub name: Sexpr,
    pub value: P<IForm>,
}

pub struct Seq {
    pub src: Option<SourceLoc>,
    pub forms: Vec<P<IForm>>,
}

pub struct Lambda {
    pub src: Option<SourceLoc>,
    pub name: Option<String>,
    pub reqargs: u32,
    pub optarg: bool,
    pub lvars: Vec<P<LVar>>,
    pub body: P<IForm>,
    pub flag: LambdaFlag,
    pub calls: Vec<(P<IForm>, Vec<P<Lambda>>)>,
    pub free_lvars: Vec<P<LVar>>,
    pub bound_lvars: HashSet<P<LVar>>,
    pub defs: Vec<P<IForm>>,
    pub lifted_var: LiftedVar,
}

impl PartialEq for Lambda {
    fn eq(&self, other: &Self) -> bool {
        self as *const Self == other as *const Self
    }
}

impl Eq for Lambda {}

pub enum LiftedVar {
    NotLifted,
    Candidate,
    Lifted(P<LVar>),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum LambdaFlag {
    None,
    Dissolved,
    Used,
}

#[repr(C)]
pub struct Label {
    pub src: Option<SourceLoc>,
    pub label: Option<i64>,
    pub body: P<IForm>,
}

pub struct Call {
    pub src: Option<SourceLoc>,
    pub proc: P<IForm>,
    pub args: Vec<P<IForm>>,
    pub flag: CallFlag,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CallFlag {
    None,
    Local,
    Embed,
    TailRec,
    Rec,
    Jump,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum LetType {
    Let,
    Rec,
    RecStar,
}

pub struct Fix {
    pub src: Option<SourceLoc>,
    pub lhs: Vec<P<LVar>>,
    pub rhs: Vec<P<Lambda>>,
    pub body: P<IForm>,
}

pub struct Let {
    pub src: Option<SourceLoc>,
    pub typ: LetType,
    pub lvars: Vec<P<LVar>>,
    pub inits: Vec<P<IForm>>,
    pub body: P<IForm>,
}

/// Handles `let-values` from Scheme. Note that it does not directly implement `let-values`,
/// instead it is constructed from `call-with-values` forms:
/// ```scm
/// (let-values ((v0 v1 ...) <init>) <body>)
/// ```
/// which is equivalent to:
/// ```scm
/// (call-with-values (lambda () <init>) (lambda (v0 v1 ...) <body>))
/// ```
pub struct LetValues {
    pub src: Option<SourceLoc>,
    pub lvars: Vec<P<LVar>>,
    pub optarg: bool,
    pub init: P<IForm>,
    pub body: P<IForm>,
}

use pretty::{BoxAllocator, DocAllocator, DocBuilder};
use termcolor::{ColorSpec, WriteColor};

impl IForm {
    pub fn pretty<'a, const REF: bool, D>(&self, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        match self {
            IForm::Fix(fix) => {
                // (fix ([l0 (lambda ...)] [l1 (lambda ...)] ...) <body>)

                let vars_pret = allocator.intersperse(
                    fix.lhs.iter().zip(fix.rhs.iter()).map(|(lvar, lambda)| {
                        let lvar_pret = lvar.name.pretty(allocator);
                        let lvar_pret = if REF {
                            lvar_pret
                                .append(allocator.text("."))
                                .append(allocator.text(format!("{:p}", lvar.as_ptr())))
                        } else {
                            lvar_pret
                        };

                        let lambda_pret =
                            P(IForm::Lambda(lambda.clone())).pretty::<REF, D>(allocator);
                        allocator
                            .text("[")
                            .append(lvar_pret)
                            .append(allocator.text(" "))
                            .append(lambda_pret)
                            .append(allocator.text("]"))
                            .group()
                            .parens()
                    }),
                    allocator.line(),
                );

                let body_pret = fix.body.pretty::<REF, _>(allocator);

                allocator
                    .text("fix")
                    .append(allocator.space())
                    .nest(1)
                    .append(vars_pret)
                    .append(allocator.line())
                    .append(body_pret)
                    .nest(1)
                    .group()
                    .parens()
            }
            IForm::PrimCall(_, name, args) => {
                let args_pret = allocator.intersperse(
                    args.iter().map(|arg| arg.pretty::<REF, _>(allocator)),
                    allocator.line(),
                );

                allocator
                    .text("primcall")
                    .append(allocator.space())
                    .append(allocator.text(name.to_owned()))
                    .append(allocator.space())
                    .append(args_pret)
                    .group()
                    .parens()
            }

            IForm::PrimRef(name) => allocator
                .text("primref")
                .append(allocator.space())
                .append(allocator.text(name.to_owned()))
                .group()
                .parens(),
            IForm::Goto(goto) => {
                let label = goto.upgrade().expect("must be alive!");
                allocator
                    .text("goto")
                    .append(allocator.space())
                    .append(allocator.text(format!("{:p}", &*label)))
                    .group()
                    .parens()
            }
            IForm::LRef(lref) => allocator
                .text("lref")
                .append(allocator.space())
                .append(lref.lvar.name.pretty(allocator))
                .append(allocator.text("."))
                .append(allocator.text(format!("{:p}", lref.lvar.as_ptr())))
                .group()
                .parens(),
            IForm::LSet(lset) => allocator
                .text("lset")
                .append(allocator.space())
                .append(lset.lvar.name.pretty(allocator))
                .append(allocator.text("."))
                .append(allocator.text(format!("{:p}", lset.lvar.as_ptr())))
                .append(allocator.space())
                .append(lset.value.pretty::<REF, D>(allocator))
                .group()
                .parens(),

            IForm::Const(sexpr) => allocator
                .text("const")
                .append(allocator.space())
                .append(sexpr.pretty(allocator))
                .group()
                .parens(),

            IForm::GRef(gref) => allocator
                .text("gref")
                .append(allocator.space())
                .append(gref.name.pretty(allocator))
                .group()
                .parens(),

            IForm::GSet(gset) => allocator
                .text("gset")
                .append(allocator.space())
                .append(gset.name.pretty(allocator))
                .append(allocator.space())
                .append(gset.value.pretty::<REF, _>(allocator))
                .group()
                .parens(),
            IForm::It => allocator.text("it").parens(),

            IForm::Seq(seq) => {
                let body_pret = allocator.intersperse(
                    seq.forms
                        .iter()
                        .map(|form| form.pretty::<REF, _>(allocator)),
                    allocator.hardline(),
                );

                allocator
                    .text("seq")
                    .append(allocator.space())
                    .append(body_pret)
                    .nest(1)
                    .group()
                    .parens()
            }

            IForm::Define(define) => allocator
                .text("define")
                .append(allocator.space())
                .append(define.name.pretty(allocator))
                .append(allocator.space())
                .append(define.value.pretty::<REF, _>(allocator))
                .group()
                .parens(),

            IForm::Call(call) => {
                let proc_pret = call.proc.pretty::<REF, _>(allocator);
                let args_pret = allocator.intersperse(
                    call.args.iter().map(|arg| arg.pretty::<REF, _>(allocator)),
                    allocator.line(),
                );

                allocator
                    .text("call")
                    .append(allocator.space())
                    .append(proc_pret)
                    .append(allocator.line())
                    .append(args_pret)
                    .nest(1)
                    .group()
                    .parens()
            }

            IForm::LetValues(vals) => {
                let lvars_pret = allocator
                    .intersperse(
                        vals.lvars.iter().map(|lvar| {
                            allocator.text(format!("{}.{:p}", lvar.name, lvar.as_ptr()))
                        }),
                        allocator.space(),
                    )
                    .group()
                    .parens();

                allocator
                    .text("let-values")
                    .append(allocator.softline())
                    .append(lvars_pret)
                    .append(allocator.line())
                    .nest(1)
                    .append(vals.init.pretty::<REF, _>(allocator))
                    .append(allocator.line())
                    .nest(1)
                    .append(vals.body.pretty::<REF, _>(allocator))
                    .nest(1)
                    .group()
                    .parens()
            }

            IForm::Let(var) => {
                let bindings_pret = allocator.intersperse(
                    var.lvars.iter().zip(var.inits.iter()).map(|(lvar, init)| {
                        lvar.name
                            .pretty(allocator)
                            .append(allocator.text("."))
                            .append(allocator.text(format!("{:p}", lvar.as_ptr())))
                            .append(allocator.space())
                            .append(init.pretty::<REF, _>(allocator))
                            .group()
                            .parens()
                    }),
                    allocator.hardline(),
                );

                let typ = if let LetType::Let = var.typ {
                    "let"
                } else if let LetType::RecStar = var.typ {
                    "rec*"
                } else {
                    "rec"
                };

                allocator
                    .text("let")
                    .append(allocator.space())
                    .append(allocator.text(typ))
                    .append(allocator.line())
                    .append(bindings_pret)
                    .append(allocator.line())
                    .nest(1)
                    .append(var.body.pretty::<REF, _>(allocator))
                    .nest(1)
                    .group()
                    .parens()
            }

            IForm::If(if_) => {
                let cond_pret = if_.cond.pretty::<REF, _>(allocator);
                let consequent_pret = if_.consequent.pretty::<REF, _>(allocator);
                let alternative_pret = if_.alternative.pretty::<REF, _>(allocator);

                allocator
                    .text("if")
                    .append(allocator.space())
                    .append(cond_pret)
                    .append(allocator.line())
                    .append(consequent_pret)
                    .append(allocator.line())
                    .append(alternative_pret)
                    .nest(1)
                    .group()
                    .parens()
            }

            IForm::Lambda(lambda) => {
                let name_pret = lambda
                    .name
                    .as_ref()
                    .map(|name| allocator.text(name.to_owned()))
                    .unwrap_or_else(|| allocator.nil());

                let lvars_pret = allocator.intersperse(
                    lambda.lvars.iter().map(|lvar| {
                        let name = lvar.name.pretty(allocator);
                        let name = if REF {
                            name.append(allocator.text("."))
                                .append(allocator.text(format!("{:p}", lvar.as_ptr())))
                        } else {
                            name
                        };
                        name.group().parens()
                    }),
                    allocator.hardline(),
                );

                let body_pret = lambda.body.pretty::<REF, _>(allocator);

                allocator
                    .text(format!("lambda[{:p}]", lambda.as_ptr()))
                    .append(allocator.space())
                    .append(name_pret)
                    .append(allocator.space())
                    .append(lvars_pret)
                    .append(allocator.line())
                    .append(body_pret)
                    .nest(1)
                    .group()
                    .parens()
            }
            IForm::Label(label) => {
                let body_pret = label.body.pretty::<REF, _>(allocator);

                allocator
                    .text("label")
                    .append(allocator.space())
                    .append(allocator.text(format!("{:p}:{:?}", self, label.label)))
                    .append(allocator.line())
                    .nest(1)
                    .append(body_pret)
                    .nest(1)
                    .group()
                    .parens()
            }
        }
    }

    pub fn pretty_print<const REF: bool>(
        &self,
        writer: &mut impl WriteColor,
    ) -> std::io::Result<()> {
        let allocator = BoxAllocator;
        let doc = self.pretty::<REF, _>(&allocator);
        doc.render_colored(70, writer)
    }
}

impl std::fmt::Display for IForm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut writer = termcolor::Buffer::no_color();
        self.pretty_print::<false>(&mut writer).unwrap();
        write!(f, "{}", String::from_utf8_lossy(writer.as_slice()))
    }
}

impl std::fmt::Debug for IForm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut writer = termcolor::Buffer::no_color();
        self.pretty_print::<false>(&mut writer).unwrap();
        write!(f, "{}", String::from_utf8_lossy(writer.as_slice()))
    }
}

/// Converts Tree IL to core-form. Core form is used to interpret
/// Scheme code when compiling to bytecode is not an option (REPL or `eval` for example).
///
/// - non recursive `Let` nodes are converted to `((lambda (<var> ...) <body>) <init> ...)`
/// - recusive `Let` nodes are converted to `((lambda (<var> ...) (set! <var> <init>) ... <body>) (const <undefined>) ...)`
pub fn il_to_core_form(thread: &mut Thread, iform: &IForm) -> Value {
    match iform {
        IForm::Const(x) => {
            let mut val = sexpr_to_value(thread, x);
            let vec = gc_protect!(thread => val => thread.make_vector::<false>(2, Value::encode_null_value()));
            scm_vector_set(vec, thread, 0, scm_intern("$const"));
            scm_vector_set(vec, thread, 1, val);
            vec
        }
        IForm::Call(call) => {
            let mut vec =
                thread.make_vector::<false>(call.args.len() + 2, Value::encode_null_value());
            scm_vector_set(vec, thread, 0, scm_intern("$call"));
            let proc = gc_protect!(thread => vec => il_to_core_form(thread, &*call.proc));
            scm_vector_set(vec, thread, 1, proc);
            for (i, arg) in call.args.iter().enumerate() {
                let arg = gc_protect!(thread => vec => il_to_core_form(thread, &*arg));
                scm_vector_set(vec, thread, i as u32 + 2, arg);
            }

            vec
        }

        IForm::Define(def) => {
            // conver it to $gset <name> <value>
            let mut vec = thread.make_vector::<false>(3, Value::encode_null_value());
            scm_vector_set(vec, thread, 0, scm_intern("$gset"));
            let name = gc_protect!(thread => vec => sexpr_to_value(thread, &def.name));
            scm_vector_set(vec, thread, 1, name);
            let value = gc_protect!(thread => vec => il_to_core_form(thread, &*def.value));
            scm_vector_set(vec, thread, 2, value);
            vec
        }

        IForm::GSet(gset) => {
            let mut vec = thread.make_vector::<false>(3, Value::encode_null_value());
            scm_vector_set(vec, thread, 0, scm_intern("$gset"));
            let name = gc_protect!(thread => vec => sexpr_to_value(thread, &gset.name));
            scm_vector_set(vec, thread, 1, name);
            let value = gc_protect!(thread => vec => il_to_core_form(thread, &*gset.value));
            scm_vector_set(vec, thread, 2, value);
            vec
        }

        IForm::GRef(gref) => {
            let mut vec = thread.make_vector::<false>(2, Value::encode_null_value());
            scm_vector_set(vec, thread, 0, scm_intern("$gref"));
            let name = gc_protect!(thread => vec => sexpr_to_value(thread, &gref.name));
            scm_vector_set(vec, thread, 1, name);
            vec
        }

        IForm::LRef(lref) => {
            let name = sexpr_to_value(thread, &lref.lvar.name);
            let vec = thread.make_vector::<false>(2, Value::encode_null_value());
            scm_vector_set(vec, thread, 0, scm_intern("$lref"));
            scm_vector_set(vec, thread, 1, name);
            vec
        }

        IForm::LSet(lset) => {
            let mut vec = thread.make_vector::<false>(3, Value::encode_null_value());
            scm_vector_set(vec, thread, 0, scm_intern("$lset"));
            let name = sexpr_to_value(thread, &lset.lvar.name);
            scm_vector_set(vec, thread, 1, name);
            let value = gc_protect!(thread => vec => il_to_core_form(thread, &*lset.value));
            scm_vector_set(vec, thread, 2, value);
            vec
        }

        IForm::If(x) => {
            let mut vec = thread.make_vector::<false>(4, Value::encode_null_value());
            scm_vector_set(vec, thread, 0, scm_intern("$if"));
            let cond = gc_protect!(thread => vec => il_to_core_form(thread, &*x.cond));
            scm_vector_set(vec, thread, 1, cond);
            let consequent = gc_protect!(thread => vec => il_to_core_form(thread, &*x.consequent));
            scm_vector_set(vec, thread, 2, consequent);
            let alternative =
                gc_protect!(thread => vec => il_to_core_form(thread, &*x.alternative));
            scm_vector_set(vec, thread, 3, alternative);
            vec
        }

        IForm::Seq(seq) => {
            let mut vec =
                thread.make_vector::<false>(seq.forms.len() + 1, Value::encode_null_value());
            scm_vector_set(vec, thread, 0, scm_intern("$seq"));
            for (i, form) in seq.forms.iter().enumerate() {
                let form = gc_protect!(thread => vec => il_to_core_form(thread, &*form));
                scm_vector_set(vec, thread, i as u32 + 1, form);
            }

            vec
        }

        IForm::It => {
            let vec = thread.make_vector::<false>(1, Value::encode_null_value());
            scm_vector_set(vec, thread, 0, scm_intern("$it"));
            vec
        }

        IForm::Lambda(lam) => {
            let mut vec = thread.make_vector::<false>(4, Value::encode_null_value());
            let mut args = gc_protect!(thread => vec => thread.make_vector::<false>(lam.lvars.len(), Value::encode_null_value()));

            for (i, lvar) in lam.lvars.iter().enumerate() {
                let name = gc_protect!(thread => vec, args => sexpr_to_value(thread, &lvar.name));
                scm_vector_set(args, thread, i as u32, name);
            }

            let body = gc_protect!(thread => vec, args => il_to_core_form(thread, &*lam.body));
            scm_vector_set(vec, thread, 0, scm_intern("$lambda"));
            scm_vector_set(vec, thread, 1, args);
            scm_vector_set(vec, thread, 2, Value::encode_bool_value(lam.optarg));
            scm_vector_set(vec, thread, 3, body);
            vec
        }

        _ => todo!(),
    }
}
