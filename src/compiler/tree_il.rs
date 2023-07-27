use super::{p::Weak, sexpr::Sexpr, P};

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
    Lambda(Lambda),
    Label(Label),
    Call(Call),
    Let(Let),
}

pub struct Define {
    pub name: Sexpr,
    pub value: P<IForm>,
}

pub struct LVar {
    pub name: Sexpr,
    pub initval: Option<Weak<IForm>>,
    pub arg: bool,
    pub ref_count: u32,
    pub set_count: u32,
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
    pub cond: P<IForm>,
    pub consequent: P<IForm>,
    pub alternative: P<IForm>,
}

pub struct GRef {
    pub name: Sexpr,
}

pub struct GSet {
    pub name: Sexpr,
    pub value: P<IForm>,
}

pub struct Seq {
    pub forms: Vec<P<IForm>>,
}

pub struct Lambda {
    pub name: Option<String>,
    pub reqargs: u32,
    pub optarg: bool,
    pub lvars: Vec<P<LVar>>,
    pub body: P<IForm>,
    pub flag: LambdaFlag,
    pub calls: Vec<(P<Call>, Vec<P<Lambda>>)>,
    pub free_lvars: Vec<P<LVar>>,
    pub lifted_var: LiftedVar,
}

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

pub struct Label {
    pub label: Option<i32>,
    pub body: P<IForm>,
}

pub struct Call {
    pub proc: P<IForm>,
    pub args: Vec<P<IForm>>,
    pub flag: CallFlag,
}

pub enum CallFlag {
    None,
    Local,
    Embed,
    Jump,
}

pub struct Let {
    pub typ: LetType,
    pub lvars: Vec<P<LVar>>,
    pub inits: Vec<P<IForm>>,
    pub body: P<IForm>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum LetType {
    Let,
    Rec,
}

use pretty::{BoxAllocator, DocAllocator, DocBuilder};
use termcolor::{ColorSpec, WriteColor};

impl IForm {
    pub fn pretty<'a, D>(&self, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        match self {
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
                .append(lset.value.pretty(allocator))
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
                .append(gset.value.pretty(allocator))
                .group()
                .parens(),
            IForm::It => allocator.text("it").parens(),

            IForm::Seq(seq) => {
                let body_pret = allocator.intersperse(
                    seq.forms.iter().map(|form| form.pretty(allocator)),
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
                .append(define.value.pretty(allocator))
                .group()
                .parens(),

            IForm::Call(call) => {
                let proc_pret = call.proc.pretty(allocator);
                let args_pret = allocator.intersperse(
                    call.args.iter().map(|arg| arg.pretty(allocator)),
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

            IForm::Let(var) => {
                let bindings_pret = allocator.intersperse(
                    var.lvars.iter().zip(var.inits.iter()).map(|(lvar, init)| {
                        lvar.name
                            .pretty(allocator)
                            .append(allocator.text("."))
                            .append(allocator.text(format!("{:p}", lvar.as_ptr())))
                            .append(allocator.space())
                            .append(init.pretty(allocator))
                            .group()
                            .parens()
                    }),
                    allocator.hardline(),
                );

                let typ = if let LetType::Let = var.typ {
                    "let"
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
                    .append(var.body.pretty(allocator))
                    .nest(1)
                    .group()
                    .parens()
            }

            IForm::If(if_) => {
                let cond_pret = if_.cond.pretty(allocator);
                let consequent_pret = if_.consequent.pretty(allocator);
                let alternative_pret = if_.alternative.pretty(allocator);

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
                        lvar.name
                            .pretty(allocator)
                            .append(allocator.text("."))
                            .append(allocator.text(format!("{:p}", lvar.as_ptr())))
                            .group()
                            .parens()
                    }),
                    allocator.hardline(),
                );

                let body_pret = lambda.body.pretty(allocator);

                allocator
                    .text("lambda")
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
                let body_pret = label.body.pretty(allocator);

                allocator
                    .text("label")
                    .append(allocator.space())
                    .append(allocator.text(format!("{:?}", label.label)))
                    .append(body_pret)
                    .nest(1)
                    .group()
                    .parens()
            }
        }
    }

    pub fn pretty_print(&self, writer: &mut impl WriteColor) -> std::io::Result<()> {
        let allocator = BoxAllocator;
        let doc = self.pretty(&allocator);
        doc.render_colored(70, writer)
    }
}
