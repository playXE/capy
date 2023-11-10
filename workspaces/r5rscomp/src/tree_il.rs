//! TreeIL - Tree Intermediate Language
//!
//! This is a tree intermediate language. Scheme code compiles to it, various optimizations
//! are performed over tree-il. All passes that operate on tree-il work in nanopass fashion,
//! that is producing a brand new tree after each pass. This is done to simplify passes and
//! remove the need to do any kind of mutation.

use std::hash::Hash;
use std::sync::atomic::AtomicU64;

use pretty::{BoxAllocator, DocAllocator, DocBuilder};
use termcolor::{Color, ColorSpec, WriteColor};

use crate::rc::{Rc, Weak};
use crate::reader::SourceLocation;
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

        let cases = allocator.intersperse(
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

        let normal_args = if !self.info.proper {
            &self.info.formals[..self.info.formals.len() - 1]
        } else {
            &self.info.formals[..]
        };

        let rest_arg = if !self.info.proper {
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
    pub proper: bool,
}

/// TreeIL node.
///
/// Each node stores source-location if present.
#[derive(Debug, Clone)]
pub enum TreeNode {
    Constant(Sexpr, Option<SourceLocation>),
    PrimRef(Rc<String>, Option<SourceLocation>),
    Proc(Rc<Proc>),
    FunCall(Rc<TreeNode>, Vec<Rc<TreeNode>>, Option<SourceLocation>),
    Rec(
        Vec<(Rc<Variable>, Rc<TreeNode>)>,
        Rc<TreeNode>,
        Option<SourceLocation>,
    ),
    RecStar(
        Vec<(Rc<Variable>, Rc<TreeNode>)>,
        Rc<TreeNode>,
        Option<SourceLocation>,
    ),
    Seq(Vec<Rc<TreeNode>>, Option<SourceLocation>),
    Test(
        Rc<TreeNode>,
        Rc<TreeNode>,
        Rc<TreeNode>,
        Option<SourceLocation>,
    ),
    Ref(Rc<Variable>, Option<SourceLocation>),
    Mutate(Rc<Variable>, Rc<TreeNode>, Option<SourceLocation>),
    Bind(
        Vec<Rc<Variable>>,
        Vec<Rc<TreeNode>>,
        Rc<TreeNode>,
        Option<SourceLocation>,
    ),
    /// Intorduced after "fixing letrec" pass. Basically a `let` binding
    /// that only contains mutually recursive functions.
    Fix(
        Vec<Rc<Variable>>, /* lhs */
        Vec<Rc<Proc>>,     /* rhs */
        Rc<TreeNode>,      /* body */
        Option<SourceLocation>,
    ),
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

impl PartialEq for TreeNode {
    fn eq(&self, other: &Self) -> bool {
        self as *const Self == other as *const Self
    }
}

impl Eq for TreeNode {}

impl Hash for TreeNode {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const Self as usize).hash(state);
    }
}

impl TreeNode {
    pub fn might_cause_side_effects(&self) -> bool {
        match self {
            Self::Constant(_, _)
            | Self::PrimRef(_, _)
            | Self::Ref(_, _)
            | Self::MultiValues(_, _) => false,
            Self::Proc(_) => false,
            Self::Seq(seq, _) => seq.iter().any(|x| x.might_cause_side_effects()),
            Self::Bind(_, inits, body, _) => {
                inits.iter().any(|x| x.might_cause_side_effects())
                    || body.might_cause_side_effects()
            }
            Self::Fix(_, _, body, _) => body.might_cause_side_effects(),

            _ => true,
        }
    }

    pub fn src(&self) -> Option<&SourceLocation> {
        match self {
            Self::FunCall(_, _, src) => src.as_ref(),
            Self::MultiValueCall(_, _, src) => src.as_ref(),
            Self::MultiValueLet(_, _, _, src) => src.as_ref(),
            Self::MultiValues(_, src) => src.as_ref(),
            Self::TagBody(_, src) => src.as_ref(),
            Self::Goto(_, src) => src.as_ref(),
            _ => None,
        }
    }

    pub fn proc(&self) -> Option<&Rc<Proc>> {
        match self {
            TreeNode::Proc(p) => Some(p),
            _ => None,
        }
    }

    pub fn is_primref(&self) -> bool {
        match self {
            TreeNode::PrimRef(_, _) => true,
            _ => false,
        }
    }

    pub fn is_primref_of(&self, name: &str) -> bool {
        match self {
            TreeNode::PrimRef(n, _) => &***n == name,
            _ => false,
        }
    }

    pub fn is_constant(&self) -> bool {
        match self {
            TreeNode::Constant(_, _) => true,
            _ => false,
        }
    }

    pub fn primref(&self) -> Option<&Rc<String>> {
        match self {
            TreeNode::PrimRef(n, _) => Some(n),
            _ => None,
        }
    }

    pub fn constant(&self) -> Option<&Sexpr> {
        match self {
            TreeNode::Constant(c, _) => Some(c),
            _ => None,
        }
    }

    pub fn is_fun_call(&self) -> bool {
        match self {
            TreeNode::FunCall(_, _, _) => true,
            _ => false,
        }
    }

    pub fn funcall_operands(&self) -> &[Rc<TreeNode>] {
        match self {
            TreeNode::FunCall(_, ops, _) => &ops,
            _ => unreachable!(),
        }
    }

    pub fn funcall_operantor(&self) -> &Rc<TreeNode> {
        match self {
            TreeNode::FunCall(ops, _, _) => &ops,
            _ => unreachable!(),
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
            TreeNode::Rec(_, _, _) => true,
            _ => false,
        }
    }

    pub fn is_rec_star(&self) -> bool {
        match self {
            TreeNode::RecStar(_, _, _) => true,
            _ => false,
        }
    }

    pub fn is_seq(&self) -> bool {
        match self {
            TreeNode::Seq(_, _) => true,
            _ => false,
        }
    }

    pub fn is_test(&self) -> bool {
        match self {
            TreeNode::Test(_, _, _, _) => true,
            _ => false,
        }
    }

    pub fn is_ref(&self) -> bool {
        match self {
            TreeNode::Ref(_, _) => true,
            _ => false,
        }
    }

    pub fn ref_variable(&self) -> Option<&Rc<Variable>> {
        match self {
            TreeNode::Ref(v, _) => Some(v),
            _ => None,
        }
    }

    pub fn is_mutate(&self) -> bool {
        match self {
            TreeNode::Mutate(_, _, _) => true,
            _ => false,
        }
    }

    pub fn is_bind(&self) -> bool {
        match self {
            TreeNode::Bind(_, _, _, _) => true,
            _ => false,
        }
    }

    pub fn is_fix(&self) -> bool {
        match self {
            TreeNode::Fix(_, _, _, _) => true,
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

impl Hash for Variable {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const Self as usize).hash(state);
    }
}

impl Variable {
    pub fn pretty<'a, D>(&self, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        allocator
            .text(self.name.to_string())
            .append(allocator.text("@"))
            .append(allocator.text(format!("{:x}", self.unique_id)))
            .annotate(kw(Color::Blue))
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
            Self::Constant(constant, _) => allocator
                .text("const")
                .annotate(fg(Color::Green))
                .append(allocator.space())
                .append(constant.pretty(allocator))
                .group()
                .parens(),
            Self::PrimRef(name, _) => allocator
                .text("primref")
                .append(allocator.space())
                .append(allocator.text(name.to_string()).annotate(kw(Color::Blue)))
                .parens(),
            Self::FunCall(func, operands, _) => {
                let mut doc = allocator
                    .text("funcall")
                    .annotate(fg(Color::Green))
                    .append(allocator.space())
                    .append(func.pretty(allocator));
                //.append(allocator.line());
                if !operands.is_empty() {
                    let operands = allocator.intersperse(
                        operands.iter().map(|operand| operand.pretty(allocator)),
                        allocator.line(),
                    );
                    doc = doc.append(allocator.softline()).append(operands).nest(1);
                }
                doc.align().group().parens()
            }

            Self::Proc(proc) => proc.pretty(allocator),

            Self::Seq(seq, _) => {
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

            Self::Rec(bindings, body, _) => {
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

            Self::RecStar(bindings, body, _) => {
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

            Self::Test(cond, then, else_, _) => {
                let mut doc = allocator
                    .text("test")
                    .annotate(fg(Color::Green))
                    .append(allocator.space())
                    .append(cond.pretty(allocator))
                    .append(allocator.line());
                doc = doc.append(then.pretty(allocator)).append(allocator.line());
                doc = doc.append(else_.pretty(allocator)).nest(1).group().parens();
                doc
            }

            Self::Ref(variable, _) => allocator
                .text("ref")
                .annotate(fg(Color::Green))
                .append(allocator.space())
                .append(variable.pretty(allocator))
                .group()
                .parens(),

            Self::Mutate(variable, value, _) => allocator
                .text("mutate")
                .annotate(fg(Color::Green))
                .append(allocator.space())
                .append(variable.pretty(allocator))
                .append(allocator.line())
                .append(value.pretty(allocator).nest(1))
                .align()
                .group()
                .parens(),

            Self::Bind(variables, values, body, _) => {
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

            Self::Fix(variables, procs, body, _) => {
                /*let mut doc = allocator
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
                doc.align().group().parens()*/
                let mut doc = allocator
                    .text("fix")
                    .annotate(fg(Color::Green))
                    .append(allocator.line());
                let variables = allocator
                    .intersperse(
                        variables.iter().zip(procs.iter()).map(|(var, val)| {
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

            Self::TagBody(body, _) => {
                let doc = allocator
                    .text("label")
                    .append(allocator.space())
                    .append(format!("{:p}", self))
                    .append(allocator.line())
                    
                    .append(body.pretty(allocator))
                    .nest(1)
                    .parens()
                    .group()
                    .align();

                doc
            }

            Self::Goto(body, _) => {
                let node = body.upgrade().unwrap();
                allocator
                    .text("goto")
                    .append(allocator.space())
                    .append(format!("{:p}", &*node))
                    .parens()
                    .group()
                    .align()
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

pub fn make_constant(constant: Sexpr, source: Option<SourceLocation>) -> Rc<TreeNode> {
    Rc::new(TreeNode::Constant(constant, source))
}

pub fn make_primref(name: Rc<String>, source: Option<SourceLocation>) -> Rc<TreeNode> {
    Rc::new(TreeNode::PrimRef(name, source))
}

pub fn make_fun_call(
    func: Rc<TreeNode>,
    operands: &[Rc<TreeNode>],
    source: Option<SourceLocation>,
) -> Rc<TreeNode> {
    Rc::new(TreeNode::FunCall(func, operands.to_vec(), source))
}

pub fn make_rec(
    bindings: Vec<(Rc<Variable>, Rc<TreeNode>)>,
    body: Rc<TreeNode>,
    source: Option<SourceLocation>,
) -> Rc<TreeNode> {
    Rc::new(TreeNode::Rec(bindings, body, source))
}

pub fn make_rec_star(
    bindings: Vec<(Rc<Variable>, Rc<TreeNode>)>,
    body: Rc<TreeNode>,
    source: Option<SourceLocation>,
) -> Rc<TreeNode> {
    Rc::new(TreeNode::RecStar(bindings, body, source))
}

pub fn make_seq(seq: Vec<Rc<TreeNode>>, source: Option<SourceLocation>) -> Rc<TreeNode> {
    if seq.len() == 1 {
        return seq[0].clone();
    }
    Rc::new(TreeNode::Seq(seq, source))
}

pub fn make_test(
    cond: Rc<TreeNode>,
    then: Rc<TreeNode>,
    else_: Rc<TreeNode>,
    source: Option<SourceLocation>,
) -> Rc<TreeNode> {
    Rc::new(TreeNode::Test(cond, then, else_, source))
}

pub fn make_ref(mut variable: Rc<Variable>, source: Option<SourceLocation>) -> Rc<TreeNode> {
    variable.referenced = true;
    Rc::new(TreeNode::Ref(variable, source))
}

pub fn make_mutate(
    mut variable: Rc<Variable>,
    value: Rc<TreeNode>,
    source: Option<SourceLocation>,
) -> Rc<TreeNode> {
    variable.mutated = true;
    Rc::new(TreeNode::Mutate(variable, value, source))
}

pub fn make_bind(
    variables: Vec<Rc<Variable>>,
    values: Vec<Rc<TreeNode>>,
    body: Rc<TreeNode>,
    source: Option<SourceLocation>,
) -> Rc<TreeNode> {
    Rc::new(TreeNode::Bind(variables, values, body, source))
}

pub fn make_fix(
    variables: Vec<Rc<Variable>>,
    procs: Vec<Rc<Proc>>,
    body: Rc<TreeNode>,
    source: Option<SourceLocation>,
) -> Rc<TreeNode> {
    Rc::new(TreeNode::Fix(variables, procs, body, source))
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

pub fn make_global_ref(name: Rc<Symbol>, source: Option<SourceLocation>) -> Rc<TreeNode> {
    make_fun_call(
        make_primref(Rc::new("$global-ref".to_string()), source.clone()),
        &[make_constant(Sexpr::Symbol(name), source.clone())],
        source,
    )
}

pub fn make_global_set(
    name: Rc<Symbol>,
    value: Rc<TreeNode>,
    source: Option<SourceLocation>,
) -> Rc<TreeNode> {
    make_fun_call(
        make_primref(Rc::new("$global-set!".to_string()), source.clone()),
        &[make_constant(Sexpr::Symbol(name), source.clone()), value],
        source,
    )
}
