//! TreeIL - Tree Intermediate Language
//!
//! This is a tree intermediate language. Scheme code compiles to it, various optimizations
//! are performed over tree-il. All passes that operate on tree-il work in nanopass fashion,
//! that is producing a brand new tree after each pass. This is done to simplify passes and
//! remove the need to do any kind of mutation.

use std::sync::atomic::AtomicU64;

use pretty::{BoxAllocator, DocAllocator, DocBuilder};
use termcolor::{Color, ColorSpec, WriteColor};

use crate::rc::{Rc, Weak};
use crate::reader::{SourceLocation, SymbolInterner};
use crate::sexpr::{Sexpr, Symbol};

#[derive(Debug, Clone)]
pub struct Proc {
    pub name: Option<Rc<Symbol>>,
    pub cases: Vec<Rc<ProcCase>>,
    pub free: Vec<Rc<Variable>>,
    pub source: Option<SourceLocation>,
}

impl Proc {
    pub fn pretty<'a, D>(&self, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        let mut doc = allocator
            .text("case-lambda")
            .annotate(fg(Color::Green))
            .append(allocator.space());

        if let Some(name) = &self.name {
            doc = doc.append(allocator.text(name.to_string()).annotate(kw(Color::Blue)));
        }

        doc = doc.append(allocator.line());

        let cases = allocator
            .intersperse(
                self.cases.iter().map(|case| case.pretty(allocator)),
                allocator.line(),
            );

        doc = doc.append(cases.nest(1));

        doc.align().group().parens()
    }
}

#[derive(Debug, Clone)]
pub struct ProcCase {
    pub info: CaseInfo,
    pub body: Rc<TreeNode>,
}

impl ProcCase {
    pub fn pretty<'a, D>(&self, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        let mut doc = allocator.nil();

        let normal_args = if self.info.rest {
            &self.info.formals[..self.info.formals.len() - 1]
        } else {
            &self.info.formals[..]
        };

        let rest_arg = if self.info.rest {
            Some(&self.info.formals[self.info.formals.len() - 1])
        } else {
            None
        };

        let mut args = allocator.intersperse(
            normal_args.iter().map(|arg| arg.pretty(allocator)),
            allocator.space(),
        );

        if let Some(rest_arg) = rest_arg {
            args = args.append(allocator.space());
            args = args.append(allocator.text("."));
            args = args.append(allocator.space());
            args = args.append(rest_arg.pretty(allocator));
        }

        args = args.parens();

        doc = doc
            .append(args.nest(1))
            .nest(1)
            .append(allocator.line())
            .append(self.body.pretty(allocator))
            .nest(1);

        doc.brackets().align().group()
    }
}

#[derive(Debug, Clone)]
pub struct CaseInfo {
    pub formals: Vec<Rc<Variable>>,
    pub rest: bool,
    pub proper: bool,
}

/// TreeIL node.
///
/// Each node stores source-location if present.
#[derive(Debug, Clone)]
pub enum TreeNode {
    Constant(Sexpr),
    PrimRef(Rc<Symbol>),
    Proc(Rc<Proc>),
    FunCall(Rc<TreeNode>, Vec<Rc<TreeNode>>, Option<SourceLocation>),
    Rec(Vec<(Rc<Variable>, Rc<TreeNode>)>, Rc<TreeNode>),
    RecStar(Vec<(Rc<Variable>, Rc<TreeNode>)>, Rc<TreeNode>),
    Seq(Vec<Rc<TreeNode>>),
    Test(Rc<TreeNode>, Rc<TreeNode>, Rc<TreeNode>),
    Ref(Rc<Variable>),
    Mutate(Rc<Variable>, Rc<TreeNode>),
    Bind(Vec<Rc<Variable>>, Vec<Rc<TreeNode>>, Rc<TreeNode>),
    /// Intorduced after "fixing letrec" pass. Basically a `let` binding
    /// that only contains mutually recursive functions.
    Fix(Vec<Rc<Variable>>, Vec<Rc<Proc>>, Rc<TreeNode>),
    /// Represents `(call-with-values producer consumer)` expression.
    MultiValueCall(Rc<TreeNode>, Rc<TreeNode>, Option<SourceLocation>),
    /// Represents `(let-values ((x1 x2 xN ...) expr) body)``
    MultiValueLet(
        Rc<TreeNode>,
        Vec<Rc<Variable>>,
        Rc<TreeNode>,
        Option<SourceLocation>,
    ),
    /// Represents `(values ...)` expression.
    MultiValues(Vec<Rc<TreeNode>>, Option<SourceLocation>),

    /// Introduced by `recover-loops` pass.
    ///
    /// This node is used to represent a loop start.
    TagBody(Rc<TreeNode>, Option<SourceLocation>),
    /// Introduced by `recover-loops` pass.
    ///
    /// This node is used to represent a loop end.
    ///
    /// weak-reference to the corresponding `TagBody` node is stored inside.
    Goto(Weak<TreeNode>, Option<SourceLocation>),
}

impl TreeNode {
    pub fn is_primref(&self) -> bool {
        match self {
            TreeNode::PrimRef(_) => true,
            _ => false,
        }
    }

    pub fn is_primref_of(&self, name: &str) -> bool {
        match self {
            TreeNode::PrimRef(n) => &***n == name,
            _ => false,
        }
    }

    pub fn is_constant(&self) -> bool {
        match self {
            TreeNode::Constant(_) => true,
            _ => false,
        }
    }

    pub fn primref(&self) -> Option<&Rc<Symbol>> {
        match self {
            TreeNode::PrimRef(n) => Some(n),
            _ => None,
        }
    }

    pub fn constant(&self) -> Option<&Sexpr> {
        match self {
            TreeNode::Constant(c) => Some(c),
            _ => None,
        }
    }

    pub fn is_fun_call(&self) -> bool {
        match self {
            TreeNode::FunCall(_, _, _) => true,
            _ => false,
        }
    }

    pub fn is_var_fun_call(&self, var: &Rc<Variable>) -> bool {
        match self {
            TreeNode::FunCall(f, _, _) => f.is_ref() && f.ref_variable() == Some(var),
            _ => false,
        }
    }

    pub fn is_rec(&self) -> bool {
        match self {
            TreeNode::Rec(_, _) => true,
            _ => false,
        }
    }

    pub fn is_rec_star(&self) -> bool {
        match self {
            TreeNode::RecStar(_, _) => true,
            _ => false,
        }
    }

    pub fn is_seq(&self) -> bool {
        match self {
            TreeNode::Seq(_) => true,
            _ => false,
        }
    }

    pub fn is_test(&self) -> bool {
        match self {
            TreeNode::Test(_, _, _) => true,
            _ => false,
        }
    }

    pub fn is_ref(&self) -> bool {
        match self {
            TreeNode::Ref(_) => true,
            _ => false,
        }
    }

    pub fn ref_variable(&self) -> Option<&Rc<Variable>> {
        match self {
            TreeNode::Ref(v) => Some(v),
            _ => None,
        }
    }

    pub fn is_mutate(&self) -> bool {
        match self {
            TreeNode::Mutate(_, _) => true,
            _ => false,
        }
    }

    pub fn is_bind(&self) -> bool {
        match self {
            TreeNode::Bind(_, _, _) => true,
            _ => false,
        }
    }

    pub fn is_fix(&self) -> bool {
        match self {
            TreeNode::Fix(_, _, _) => true,
            _ => false,
        }
    }

    pub fn is_multi_value_call(&self) -> bool {
        match self {
            TreeNode::MultiValueCall(_, _, _) => true,
            _ => false,
        }
    }

    pub fn is_multi_value_let(&self) -> bool {
        match self {
            TreeNode::MultiValueLet(_, _, _, _) => true,
            _ => false,
        }
    }

    pub fn is_multi_values(&self) -> bool {
        match self {
            TreeNode::MultiValues(_, _) => true,
            _ => false,
        }
    }

    pub fn is_tag_body(&self) -> bool {
        match self {
            TreeNode::TagBody(_, _) => true,
            _ => false,
        }
    }

    pub fn is_goto(&self) -> bool {
        match self {
            TreeNode::Goto(_, _) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Rc<Symbol>,
    /// Used for pretty-printing.
    pub unique_id: u64,
    pub operand: Option<Rc<TreeNode>>,
    pub referenced: bool,
    pub mutated: bool,
    pub residual_referenced: bool,
    pub residual_mutated: bool,
    pub singly_referenced: bool,
    pub residual_single_referenced: bool,
}

impl Variable {
    pub fn pretty<'a, D>(&self, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        allocator
            .text("variable")
            .annotate(fg(Color::Yellow))
            .append(allocator.space())
            .append(
                allocator
                    .text((*self.name).as_ref().to_string())
                    .annotate(kw(Color::Blue)),
            )
            .append(allocator.text("@"))
            .append(allocator.text(format!("{:x}", self.unique_id)))
            .parens()
    }
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        self as *const _ == other as *const _
    }
}

impl Eq for Variable {}

fn fg(color: Color) -> ColorSpec {
    let mut spec = ColorSpec::new();
    spec.set_fg(Some(color));
    spec
}

fn kw(color: Color) -> ColorSpec {
    let mut spec = ColorSpec::new();
    spec.set_fg(Some(color));
    spec.set_bold(true);
    spec
}

impl TreeNode {
    pub fn pretty<'a, D>(&self, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        match self {
            Self::Constant(constant) => allocator
                .text("const")
                .annotate(fg(Color::Green))
                .append(allocator.space())
                .append(constant.pretty(allocator))
                .group()
                .parens(),
            Self::PrimRef(name) => allocator
                .text("primref")
                .append(allocator.space())
                .append(allocator.text(name.to_string()).annotate(kw(Color::Blue)))
                .parens(),
            Self::FunCall(func, operands, _) => {
                let mut doc = allocator
                    .text("funcall")
                    .annotate(fg(Color::Green))
                    .append(allocator.space())
                    .append(func.pretty(allocator))
                    .append(allocator.line());
                let operands = allocator.intersperse(
                    operands.iter().map(|operand| operand.pretty(allocator)),
                    allocator.line(),
                );
                doc = doc.append(operands).nest(1).group().parens();
                doc.align()
            }

            Self::Proc(proc) => {
                proc.pretty(allocator)
            }

            Self::Seq(seq) => {
                let mut doc = allocator
                    .text("seq")
                    .annotate(fg(Color::Green))
                    .append(allocator.line());
                let seq = allocator.intersperse(
                    seq.iter().map(|operand| operand.pretty(allocator)),
                    allocator.line(),
                );
                doc = doc.append(seq).nest(1);
                doc.group().parens()
            }

            Self::Rec(bindings, body) => {
                let mut doc = allocator
                    .text("rec")
                    .annotate(fg(Color::Green))
                    .append(allocator.space());
                let bindings = allocator
                    .intersperse(
                        bindings.iter().map(|(name, value)| {
                            name.pretty(allocator)
                                .append(allocator.line())
                                .append(value.pretty(allocator))
                        }),
                        allocator.line(),
                    )
                    .parens();
                doc = doc
                    .nest(2)
                    .append(allocator.line())
                    .append(bindings.nest(2))
                    .append(allocator.line());
                doc = doc.append(body.pretty(allocator)).nest(1);
                doc.align().group().parens()
            }

            Self::RecStar(bindings, body) => {
                let mut doc = allocator
                    .text("rec*")
                    .annotate(fg(Color::Green))
                    .append(allocator.space());
                let bindings = allocator.intersperse(
                    bindings.iter().map(|(name, value)| {
                        name.pretty(allocator)
                            .append(allocator.space())
                            .append(value.pretty(allocator))
                    }),
                    allocator.line(),
                );
                doc = doc
                    .append(allocator.line())
                    .append(bindings)
                    .nest(1)
                    .append(allocator.line());
                doc = doc.append(body.pretty(allocator)).nest(1).group();
                doc.group().parens()
            }

            Self::Test(cond, then, else_) => {
                let mut doc = allocator
                    .text("test")
                    .annotate(fg(Color::Green))
                    .append(allocator.space())
                    .append(cond.pretty(allocator))
                    .append(allocator.line());
                doc = doc
                    .append(then.pretty(allocator))
                    .append(allocator.line());
                doc = doc
                    .append(else_.pretty(allocator)).nest(1)
                    .group()
                    .parens();
                doc
            }

            Self::Ref(variable) => allocator
                .text("ref")
                .annotate(fg(Color::Green))
                .append(allocator.space())
                .append(variable.pretty(allocator))
                .group()
                .parens(),

            Self::Mutate(variable, value) => allocator
                .text("mutate")
                .annotate(fg(Color::Green))
                .append(allocator.space())
                .append(variable.pretty(allocator))
                .append(allocator.line())
                .append(value.pretty(allocator).nest(1))
                .align()
                .group()
                .parens(),

            Self::Bind(variables, values, body) => {
                let mut doc = allocator
                    .text("bind")
                    .annotate(fg(Color::Green))
                    .append(allocator.line());
                let variables = allocator
                    .intersperse(
                        variables.iter().zip(values.iter()).map(|(var, val)| {
                            allocator
                                .nil()
                                .append(var.pretty(allocator))
                                .append(allocator.space())
                                .append(val.pretty(allocator))
                                .group()
                                .parens()
                        }),
                        allocator.line(),
                    )
                    .parens();
                doc = doc.append(variables).nest(2).append(allocator.line());
                doc = doc.append(body.pretty(allocator).nest(1)).align();
                doc.align().group().parens()
            }

            Self::Fix(variables, procs, body) => {
                let mut doc = allocator
                    .text("fix")
                    .annotate(fg(Color::Green))
                    .append(allocator.space());
                let variables = allocator.intersperse(
                    variables.iter().zip(procs.iter()).map(|(var, proc)| {
                        allocator
                            .nil()
                            .append(var.pretty(allocator))
                            .append(allocator.space())
                            .append(proc.pretty(allocator))
                    }),
                    allocator.line(),
                );
                doc = doc.nest(2).append(variables).append(allocator.line());
                doc = doc.nest(-1).append(body.pretty(allocator)).group().parens();
                doc.align().group().parens()
            }

            Self::MultiValueCall(producer, consumer, _) => allocator
                .text("mv-call")
                .append(allocator.space())
                .append(
                    allocator
                        .nil()
                        .append(producer.pretty(allocator))
                        .append(allocator.space())
                        .append(consumer.pretty(allocator)),
                )
                .parens(),

            Self::MultiValueLet(expr, lhs, body, _) => {
                let lhs = allocator
                    .intersperse(lhs.iter().map(|x| x.pretty(allocator)), allocator.space())
                    .parens();

                let var_and_expr = allocator
                    .nil()
                    .append(lhs)
                    .append(allocator.space())
                    .append(expr.pretty(allocator))
                    .parens();

                let mut doc = allocator
                    .text("mv-let")
                    .annotate(fg(Color::Green))
                    .append(allocator.space())
                    .append(var_and_expr)
                    .append(allocator.line());
                doc = doc.nest(1).append(body.pretty(allocator)).group().parens();
                doc.group().parens()
            }

            _ => todo!(),
        }
    }

    pub fn pretty_print(&self, out: impl WriteColor) -> std::io::Result<()> {
        let allocator = BoxAllocator;

        self.pretty(&allocator).1.render_colored(40, out)?;

        Ok(())
    }
}



pub fn make_variable(name: Rc<Symbol>) -> Rc<Variable> {
    static ID: AtomicU64 = AtomicU64::new(0);
    Rc::new(Variable {
        name,
        unique_id: ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
        operand: None,
        referenced: false,
        mutated: false,
        residual_referenced: false,
        residual_mutated: false,
        singly_referenced: false,
        residual_single_referenced: false,
    })
}

pub fn make_proc(name: Option<Rc<Symbol>>, cases: Vec<Rc<ProcCase>>) -> Rc<Proc> {
    Rc::new(Proc {
        name,
        cases,
        free: Vec::new(),
        source: None,
    })
}

pub fn make_proc_case(info: CaseInfo, body: Rc<TreeNode>) -> Rc<ProcCase> {
    Rc::new(ProcCase { info, body })
}

pub fn make_constant(constant: Sexpr) -> Rc<TreeNode> {
    Rc::new(TreeNode::Constant(constant))
}

pub fn make_primref(name: Rc<Symbol>) -> Rc<TreeNode> {
    Rc::new(TreeNode::PrimRef(name))
}

pub fn make_fun_call(
    func: Rc<TreeNode>,
    operands: &[Rc<TreeNode>],
    source: Option<SourceLocation>,
) -> Rc<TreeNode> {
    Rc::new(TreeNode::FunCall(func, operands.to_vec(), source))
}

pub fn make_rec(bindings: Vec<(Rc<Variable>, Rc<TreeNode>)>, body: Rc<TreeNode>) -> Rc<TreeNode> {
    Rc::new(TreeNode::Rec(bindings, body))
}

pub fn make_rec_star(
    bindings: Vec<(Rc<Variable>, Rc<TreeNode>)>,
    body: Rc<TreeNode>,
) -> Rc<TreeNode> {
    Rc::new(TreeNode::RecStar(bindings, body))
}

pub fn make_seq(seq: Vec<Rc<TreeNode>>) -> Rc<TreeNode> {
    if seq.len() == 1 {
        return seq[0].clone();
    }
    Rc::new(TreeNode::Seq(seq))
}

pub fn make_test(cond: Rc<TreeNode>, then: Rc<TreeNode>, else_: Rc<TreeNode>) -> Rc<TreeNode> {
    Rc::new(TreeNode::Test(cond, then, else_))
}

pub fn make_ref(variable: Rc<Variable>) -> Rc<TreeNode> {
    Rc::new(TreeNode::Ref(variable))
}

pub fn make_mutate(variable: Rc<Variable>, value: Rc<TreeNode>) -> Rc<TreeNode> {
    Rc::new(TreeNode::Mutate(variable, value))
}

pub fn make_bind(
    variables: Vec<Rc<Variable>>,
    values: Vec<Rc<TreeNode>>,
    body: Rc<TreeNode>,
) -> Rc<TreeNode> {
    Rc::new(TreeNode::Bind(variables, values, body))
}

pub fn make_fix(
    variables: Vec<Rc<Variable>>,
    procs: Vec<Rc<Proc>>,
    body: Rc<TreeNode>,
) -> Rc<TreeNode> {
    Rc::new(TreeNode::Fix(variables, procs, body))
}

pub fn make_multi_value_call(
    producer: Rc<TreeNode>,
    consumer: Rc<TreeNode>,
    source: Option<SourceLocation>,
) -> Rc<TreeNode> {
    Rc::new(TreeNode::MultiValueCall(producer, consumer, source))
}

pub fn make_multi_value_let(
    expr: Rc<TreeNode>,
    lhs: Vec<Rc<Variable>>,
    body: Rc<TreeNode>,
    source: Option<SourceLocation>,
) -> Rc<TreeNode> {
    Rc::new(TreeNode::MultiValueLet(expr, lhs, body, source))
}

pub fn make_multi_values(
    values: Vec<Rc<TreeNode>>,
    source: Option<SourceLocation>,
) -> Rc<TreeNode> {
    Rc::new(TreeNode::MultiValues(values, source))
}

pub fn make_tag_body(body: Rc<TreeNode>, source: Option<SourceLocation>) -> Rc<TreeNode> {
    Rc::new(TreeNode::TagBody(body, source))
}

pub fn make_goto(body: Rc<TreeNode>, source: Option<SourceLocation>) -> Rc<TreeNode> {
    Rc::new(TreeNode::Goto(Rc::downgrade(&body), source))
}

pub fn make_global_ref(
    interner: &SymbolInterner,
    name: Rc<Symbol>,
    source: Option<SourceLocation>,
) -> Rc<TreeNode> {
    make_fun_call(
        make_primref(interner.intern("$global-ref")),
        &[make_constant(Sexpr::Symbol(name))],
        source,
    )
}

pub fn make_global_set(
    interner: &SymbolInterner,
    name: Rc<Symbol>,
    value: Rc<TreeNode>,
    source: Option<SourceLocation>,
) -> Rc<TreeNode> {
    make_fun_call(
        make_primref(interner.intern("$global-set!")),
        &[make_constant(Sexpr::Symbol(name)), value],
        source,
    )
}
