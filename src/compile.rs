use std::hash::Hash;

use crate::{
    compaux::{scm_identifier_env, scm_identifier_global_binding, scm_make_identifier},
    op::Opcode,
    runtime::fun::make_procedure,
    runtime::list::scm_acons,
    runtime::module::is_global_identifier_eq,
    runtime::object::{Module, ObjectHeader, Syntax, Type},
    runtime::symbol::make_symbol,
    runtime::value::Value,
    scm_dolist, scm_for_each,
    vm::scm_current_module,
};
use pretty::{BoxAllocator, DocAllocator, DocBuilder};
use rsgc::{
    prelude::{Allocation, Handle, Object},
    system::{arraylist::ArrayList, collections::hashmap::HashMap},
    thread::Thread,
};
use termcolor::{Color, ColorSpec, WriteColor};

pub enum IForm {
    Const(Value),
    LRef(LRef),
    LSet(LSet),
    If(If),
    GSet(GSet),
    GRef(GRef),
    Let(Let),
    Receive(Receive),
    Lambda(Lambda),
    Label(Label),
    Seq(Seq),
    Call(Call),
    Dynenv(Dynenv),
    Cons(Cons),
    List(List),
    Define(Define),
    Asm(Asm),
    It,
    Add(Handle<IForm>, Handle<IForm>),
    Sub(Handle<IForm>, Handle<IForm>),
    Mul(Handle<IForm>, Handle<IForm>),
    Div(Handle<IForm>, Handle<IForm>),
    Mod(Handle<IForm>, Handle<IForm>),
    Eq(Handle<IForm>, Handle<IForm>),
}
#[derive(Copy, Clone)]
pub enum AsmOperand {
    Constant(Value),
    I32(i32),
    I16(i16),
    I8(i8),
}

unsafe impl Object for AsmOperand {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        match self {
            AsmOperand::Constant(v) => v.trace(visitor),
            _ => (),
        }
    }
}

unsafe impl Allocation for AsmOperand {}

pub struct Asm {
    pub op: Opcode,
    pub args: ArrayList<Handle<IForm>>,
    pub operands: Option<ArrayList<AsmOperand>>,
    pub exits: bool,
    pub pushes: bool,
    pub ic: bool,
}

unsafe impl Object for IForm {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        match self {
            IForm::Const(v) => v.trace(visitor),
            IForm::LRef(l) => l.trace(visitor),
            IForm::LSet(l) => l.trace(visitor),
            IForm::GRef(id) => id.trace(visitor),
            IForm::GSet(gset) => {
                gset.trace(visitor);
            }
            IForm::Let(l) => l.trace(visitor),
            IForm::If(i) => i.trace(visitor),
            IForm::Receive(r) => r.trace(visitor),
            IForm::Lambda(l) => l.trace(visitor),
            IForm::Label(l) => l.trace(visitor),
            IForm::Seq(s) => s.trace(visitor),
            IForm::Call(c) => c.trace(visitor),
            IForm::Dynenv(d) => d.trace(visitor),
            IForm::Cons(c) => c.trace(visitor),
            IForm::List(l) => l.trace(visitor),
            IForm::Define(d) => d.trace(visitor),
            IForm::Asm(a) => {
                a.args.trace(visitor);
                if let Some(operands) = &a.operands {
                    operands.trace(visitor);
                }
            },
            _ => (),
        }
    }
}

unsafe impl Allocation for IForm {}

#[repr(C)]
pub struct LVar {
    pub(crate) header: ObjectHeader,
    pub name: Value,
    pub initval: Option<Handle<IForm>>,
    pub arg: bool,
    pub ref_count: usize,
    pub set_count: usize,
}

impl Hash for LVar {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let ptr = self as *const _ as *const u8;
        ptr.hash(state);
    }
}

impl PartialEq for LVar {
    fn eq(&self, other: &Self) -> bool {
        self as *const _ == other as *const _
    }
}

impl Eq for LVar {}

unsafe impl Object for LVar {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.initval.trace(visitor);
        self.name.trace(visitor);
    }
}

unsafe impl Allocation for LVar {}

pub struct If {
    pub origin: Value,
    pub cond: Handle<IForm>,
    pub cons: Handle<IForm>,
    pub alt: Handle<IForm>,
}

unsafe impl Object for If {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.origin.trace(visitor);
        self.cond.trace(visitor);
        self.cons.trace(visitor);
        self.alt.trace(visitor);
    }
}

unsafe impl Allocation for If {}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum LetScope {
    Let,
    Rec,
}

pub struct Let {
    pub origin: Value,
    pub scope: LetScope,
    pub lvars: ArrayList<Handle<LVar>>,
    pub inits: ArrayList<Handle<IForm>>,
    pub body: Handle<IForm>,
}

unsafe impl Object for Let {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.origin.trace(visitor);
        self.lvars.trace(visitor);
        self.inits.trace(visitor);
        self.body.trace(visitor);
    }
}

unsafe impl Allocation for Let {}

pub struct LSet {
    pub origin: Value,
    pub lvar: Handle<LVar>,
    pub value: Handle<IForm>,
}

unsafe impl Object for LSet {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.origin.trace(visitor);
        self.lvar.trace(visitor);
        self.value.trace(visitor);
    }
}

pub struct LRef {
    pub origin: Value,
    pub lvar: Handle<LVar>,
}

unsafe impl Object for LRef {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.lvar.trace(visitor);
        self.origin.trace(visitor);
    }
}

unsafe impl Allocation for LRef {}
unsafe impl Allocation for LSet {}

pub struct GSet {
    pub origin: Value,
    pub id: Value,
    pub value: Handle<IForm>,
}

unsafe impl Object for GSet {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.id.trace(visitor);
        self.value.trace(visitor);
        self.origin.trace(visitor);
    }
}

pub struct GRef {
    pub origin: Value,
    pub id: Value,
}

unsafe impl Object for GRef {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.id.trace(visitor);
        self.origin.trace(visitor);
    }
}

unsafe impl Allocation for GSet {}
unsafe impl Allocation for GRef {}

pub struct Receive {
    pub origin: Value,
    pub reqargs: usize,
    pub optarg: bool,
    pub lvars: ArrayList<Handle<LVar>>,
    pub expr: Handle<IForm>,
    pub body: Handle<IForm>,
}

unsafe impl Object for Receive {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.origin.trace(visitor);
        self.lvars.trace(visitor);
        self.expr.trace(visitor);
        self.body.trace(visitor);
    }
}

unsafe impl Allocation for Receive {}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum LFlag {
    /// This lambda was inlined
    Dissolved,
    /// indicates that this lambda has been already dealt
    /// with, and need to be eliminated.
    Used,
}

pub struct Lambda {
    pub origin: Value,
    pub name: Value,
    pub reqargs: usize,
    pub optarg: bool,
    pub lvars: ArrayList<Handle<LVar>>,
    pub body: Handle<IForm>,
    pub flag: LFlag,
    /// List of call sites
    pub calls: ArrayList<Handle<IForm>>,
    pub free_lvars: ArrayList<Handle<LVar>>,
    /// If this lambda is lifted to the toplevel,
    /// this slot contains an lvar to which the toplevel
    /// closure is to be bound.
    pub lifted_var: Option<Handle<LVar>>,
}

pub struct Label {
    pub origin: Value,
    pub label: Option<i32>,
    pub body: Handle<IForm>,
}

pub struct Seq {
    pub origin: Value,
    pub body: ArrayList<Handle<IForm>>,
}

pub struct Call {
    pub origin: Value,
    pub proc: Handle<IForm>,
    pub args: ArrayList<Handle<IForm>>,
    pub flag: CallFlag,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum CallFlag {
    None,
    Local,
    Embed,
    Jump,
    Rec,
    TailRec,
}

pub struct Dynenv {
    pub origin: Value,
    pub kvs: ArrayList<(Handle<IForm>, Handle<IForm>)>,
    pub body: Handle<IForm>,
}

pub struct Cons {
    pub origin: Value,
    pub car: Handle<IForm>,
    pub cdr: Handle<IForm>,
}

pub struct List {
    pub origin: Value,
    pub elems: ArrayList<Handle<IForm>>,
}

pub struct Define {
    pub origin: Value,
    pub name: Value,
    pub value: Handle<IForm>,
}

unsafe impl Object for Define {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.origin.trace(visitor);
        self.name.trace(visitor);
        self.value.trace(visitor);
    }
}

unsafe impl Object for List {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.origin.trace(visitor);
        self.elems.trace(visitor);
    }
}

unsafe impl Object for Lambda {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.origin.trace(visitor);
        self.name.trace(visitor);
        self.lvars.trace(visitor);
        self.body.trace(visitor);
        self.calls.trace(visitor);
        self.free_lvars.trace(visitor);
        if let Some(lvar) = self.lifted_var {
            lvar.trace(visitor);
        }
    }
}

unsafe impl Object for Label {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.origin.trace(visitor);
        self.body.trace(visitor);
    }
}

unsafe impl Object for Seq {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.origin.trace(visitor);
        self.body.trace(visitor);
    }
}

unsafe impl Object for Call {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.origin.trace(visitor);
        self.proc.trace(visitor);
        self.args.trace(visitor);
    }
}

unsafe impl Object for Dynenv {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.origin.trace(visitor);
        self.kvs.trace(visitor);
        self.body.trace(visitor);
    }
}

unsafe impl Object for Cons {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.origin.trace(visitor);
        self.car.trace(visitor);
        self.cdr.trace(visitor);
    }
}

impl LVar {
    pub fn is_immutable(&self) -> bool {
        self.set_count == 0
    }

    pub fn reset(&mut self) {
        self.set_count = 0;
        self.ref_count = 0;
    }

    pub fn const_value(&self) -> Option<Handle<IForm>> {
        self.initval.filter(|_| self.is_immutable())
    }
}

/// Compile-time environment (cenv)
///
///   Slots:
///     module   - The 'current-module' to resolve global binding.
///     frames   - List of local frames.  Each local frame has a form:
///                (<type> (<name> . <obj>) ...)
///
///                <type>     <obj>
///                ----------------------------------------------
///                LEXICAL    <lvar>     // lexical binding
///                SYNTAX     <macro>    // syntactic binding
///
///     exp-name - The "name" of the current expression, that is, the
///                name of the variable the result of the current
///                expression is to be bound.  This slot may contain
///                an identifier (for global binding) or a lvar (for
///                local binding).   This slot may be #f.
///
///     current-proc - Holds the information of the current
///                compiling procedure.  It accumulates information needed
///                in later stages for the optimization.  This slot may
///                be #f.
///
///     source-path - While processing included file, this slot is set to
///                the full path of the included filename.
/// (define-vector-struct cenv %make-cenv (module frames exp-name current-proc (source-path (current-load-path))))
pub fn make_cenv(module: Handle<Module>, frames: Value) -> Value {
    Value::encode_object_value(make_vector!(
        make_symbol("cenv", true).into(),
        module.into(),
        frames,
        Value::encode_bool_value(false),
        Value::encode_null_value(),
        Value::encode_null_value()
    ))
}

pub fn cenv_make_bottom(maybe_module: Option<Handle<Module>>) -> Value {
    let module = maybe_module.unwrap_or_else(|| scm_current_module().unwrap());

    make_cenv(module, Value::encode_null_value())
}

pub fn cenv_module(cenv: Value) -> Handle<Module> {
    cenv.vector_ref(1).module()
}

pub fn cenv_frames(cenv: Value) -> Value {
    cenv.vector_ref(2)
}

pub fn cenv_exp_name(cenv: Value) -> Value {
    cenv.vector_ref(3)
}

pub fn cenv_sans_name(cenv: Value) -> Value {
    if cenv_exp_name(cenv).is_false() {
        cenv
    } else {
        let cenv = cenv_copy(cenv);
        cenv_set_exp_name(cenv, Value::encode_bool_value(false));
        cenv
    }
}

pub fn cenv_current_proc(cenv: Value) -> Value {
    cenv.vector_ref(4)
}

pub fn cenv_source_path(cenv: Value) -> Value {
    cenv.vector_ref(5)
}

pub fn cenv_set_module(cenv: Value, module: Handle<Module>) {
    cenv.vector_set(1, module.into());
}

pub fn cenv_set_frames(cenv: Value, frames: Value) {
    cenv.vector_set(2, frames);
}

pub fn cenv_set_exp_name(cenv: Value, exp_name: Value) {
    cenv.vector_set(3, exp_name);
}

pub fn cenv_set_current_proc(cenv: Value, current_proc: Value) {
    cenv.vector_set(4, current_proc);
}

pub fn cenv_set_source_path(cenv: Value, source_path: Value) {
    cenv.vector_set(5, source_path);
}

pub fn cenv_copy(cenv: Value) -> Value {
    make_cenv(cenv_module(cenv), cenv_frames(cenv))
}

pub fn cenv_extend(cenv: Value, frame: Value, typ: Value) -> Value {
    let frames = scm_acons(Thread::current(), typ, frame, cenv_frames(cenv));
    let cenv = make_cenv(cenv_module(cenv), frames);
    cenv
}

pub fn has_tag(obj: Value, tag: &str) -> bool {
    if obj.is_xtype(Type::Vector) && obj.vector_len() >= 1 {
        let maybe_tag = obj.vector_ref(0);
        if maybe_tag.is_xtype(Type::Symbol) {
            return &**maybe_tag.symbol() == tag;
        }
    }
    false
}

/// ```
/// env-lookup-int :: Name, Module, [Frame] -> Var
///        where Var = Lvar | Identifier | Macro
/// ```
/// #  PERFORMANCE KLUDGE:
///     - We assume the frame structure is well-formed, so skip some tests.
///
pub fn env_lookup_int(name: Value, module: Handle<Module>, mut frames: Value) -> Value {
    let mut y = name;

    loop {
        scm_for_each!(fp1, frames, {
            let vls = fp1.cdar();

            // inlined assq
            scm_dolist!(vp, vls, {
                if y == vp.car() {
                    return vp.cdr();
                }
            });
        });

        // no match, we strip identifier wrapping and retry
        if y.is_xtype(Type::Identifier) {
            let inner = y.identifier().name;
            if !inner.is_xtype(Type::Identifier) {
                frames = scm_identifier_env(y.identifier());
            }
            y = inner;
        } else {
            break;
        }
    }

    // no local binding, return an identifier
    if name.is_xtype(Type::Symbol) {
        scm_make_identifier(name, Some(module), frames).into()
    } else {
        assert!(
            name.is_xtype(Type::Identifier),
            "name must be an identifier but got {:?}",
            name
        );
        name
    }
}

pub fn env_lookup(name: Value, module: Handle<Module>, frames: Value) -> Value {
    env_lookup_int(name, module, frames)
}

pub fn cenv_lookup(cenv: Value, name: Value) -> Value {
    env_lookup_int(name, cenv_module(cenv), cenv_frames(cenv))
}

pub fn cenv_toplevelp(cenv: Value) -> bool {
    cenv_frames(cenv).is_null()
}

pub fn make_iform(iform: IForm) -> Handle<IForm> {
    Thread::current().allocate(iform)
}

pub fn make_seq(src: Value, seq: &[Handle<IForm>]) -> Handle<IForm> {
    make_iform(IForm::Seq(Seq {
        origin: src,
        body: {
            let mut ls = ArrayList::with_capacity(Thread::current(), seq.len());
            for &f in seq {
                ls.push(Thread::current(), f);
            }

            ls
        },
    }))
}

pub enum GlobalCall {
    Macro(Value),
    Syntax(Handle<Syntax>),
    Inliner(Value),
    Normal,
}

pub fn global_call_type(id: Value, _cenv: Value) -> GlobalCall {
    if let Some(gloc) = scm_identifier_global_binding(id.identifier()) {
        let gval = gloc.value;

        if gval.is_empty() {
            return GlobalCall::Normal;
        }
        if gval.is_syntax() {
            GlobalCall::Syntax(gval.syntax())
        } else if gval.is_macro() {
            GlobalCall::Macro(gval)
        } else if gval.is_native_procedure() && gval.native_procedure().inliner.is_some() {
            GlobalCall::Inliner(gval)
        } else {
            GlobalCall::Normal
        }
    } else {
        GlobalCall::Normal
    }
}

pub(crate) fn init_compiler() {
    pass1::define_syntax();
}

//pub mod anf;
pub mod bytecompiler;
pub mod pass1;
pub mod pass2;

impl IForm {
    pub fn pretty<'a, D>(&self, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        match self {
            IForm::Const(c) => allocator
                .text("const")
                .annotate(ColorSpec::new().set_fg(Some(Color::Blue)).clone())
                .append(allocator.space())
                .append(format!("{:?}", c))
                .group()
                .parens(),
            IForm::LRef(lref) => allocator
                .text("lref")
                .annotate(ColorSpec::new().set_fg(Some(Color::Blue)).clone())
                .append(allocator.space())
                .append(format!("{:?}.{:p}", lref.lvar.name, lref.lvar.as_ptr()))
                .group()
                .parens(),

            IForm::LSet(lset) => {
                let e_pret = lset.value.pretty(allocator);

                allocator
                    .text("lset!")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Blue)).clone())
                    .append(allocator.space())
                    .append(format!("{:?}.{:p}", lset.lvar.name, lset.lvar.as_ptr()))
                    .append(allocator.space())
                    .append(e_pret)
                    .group()
                    .parens()
            }

            IForm::GRef(gref) => allocator
                .text("gref")
                .annotate(ColorSpec::new().set_fg(Some(Color::Blue)).clone())
                .append(allocator.space())
                .append(format!("{:?}", gref.id))
                .group()
                .parens(),

            IForm::GSet(gset) => {
                let e_pret = gset.value.pretty(allocator);

                allocator
                    .text("gset!")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Blue)).clone())
                    .append(allocator.space())
                    .append(format!("{:?}", gset.id))
                    .append(allocator.space())
                    .append(e_pret)
                    .group()
                    .parens()
            }

            IForm::Define(def) => {
                let e_pret = def.value.pretty(allocator);

                allocator
                    .text("define")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Blue)).clone())
                    .append(allocator.space())
                    .append(format!("{:?}", def.name))
                    .append(allocator.hardline().nest(2))
                    .append(e_pret)
                    .align()
                    .parens()
            }

            IForm::Seq(seq) => {
                let body_pret = allocator.intersperse(
                    seq.body.iter().map(|form| form.pretty(allocator)),
                    allocator.hardline(),
                );

                allocator
                    .text("begin")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Blue)).clone())
                    .append(allocator.space())
                    .append(body_pret)
                    .nest(1)
                    .group()
                    .parens()
            }

            IForm::Call(call) => {
                let proc_pret = call.proc.pretty(allocator);
                let args_pret = allocator.intersperse(
                    call.args.iter().map(|arg| arg.pretty(allocator)),
                    allocator.line(),
                );

                allocator
                    .text("call")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Blue)).clone())
                    .append(allocator.space())
                    .append(proc_pret)
                    .append(allocator.space())
                    .append(args_pret)
                    .group()
                    .parens()
            }

            IForm::Lambda(lam) => {
                // (lambda (arg ...) body ...)
                let args_pret = allocator
                    .intersperse(
                        lam.lvars.iter().map(|lvar| {
                            if lvar.set_count != 0 {
                                allocator
                                    .text(format!("&{:?}", lvar.name))
                                    .annotate(ColorSpec::new().set_fg(Some(Color::Red)).clone())
                            } else {
                                allocator
                                    .text(format!("{:?}", lvar.name))
                                    .annotate(ColorSpec::new().set_fg(Some(Color::Red)).clone())
                            }
                        }),
                        allocator.space(),
                    )
                    .parens();

                allocator
                    .text("lambda")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Blue)).clone())
                    .append(allocator.space())
                    .append(args_pret)
                    .append(allocator.line())
                    .append(lam.body.pretty(allocator))
                    .nest(1)
                    .group()
                    .parens()
            }

            IForm::Let(var) => {
                // (let ((var init) ...) body ...)
                let bindings_pret = allocator
                    .intersperse(
                        var.lvars.iter().zip(var.inits.iter()).map(|(n, e)| {
                            allocator
                                .text(format!("{:?}.{:p}", n.name, n.as_ptr()))
                                .annotate(ColorSpec::new().set_fg(Some(Color::Red)).clone())
                                .append(allocator.space())
                                .append(e.pretty(allocator))
                                .group()
                                .brackets()
                        }),
                        allocator.line(),
                    )
                    .parens();

                allocator
                    .text("let")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Blue)).clone())
                    .append(allocator.space())
                    .append(bindings_pret)
                    .append(allocator.line())
                    .append(var.body.pretty(allocator))
                    .nest(1)
                    .group()
                    .parens()
            }

            IForm::If(cond) => allocator
                .text("if")
                .annotate(ColorSpec::new().set_fg(Some(Color::Blue)).clone())
                .append(allocator.space())
                .append(cond.cond.pretty(allocator))
                .append(allocator.softline())
                .append(cond.cons.pretty(allocator))
                .append(allocator.softline())
                .append(cond.alt.pretty(allocator))
                .group()
                .parens(),

            IForm::It => allocator
                .text("it")
                .annotate(ColorSpec::new().set_fg(Some(Color::Blue)).clone()),

            IForm::Asm(asm) => {
                let operands = allocator.intersperse(
                    asm.args.iter().map(|x| x.pretty(allocator)),
                    allocator.space(),
                );
                allocator
                    .text(asm.op.to_string())
                    .append(allocator.space())
                    .append(operands)
                    .group()
                    .parens()
            }
            _ => todo!(),
        }
    }

    pub fn pretty_print(&self, out: impl WriteColor) -> std::io::Result<()> {
        let allocator = BoxAllocator;
        self.pretty(&allocator).1.render_colored(70, out)?;

        Ok(())
    }
}

pub fn ref_count_lvars(mut iform: Handle<IForm>) {
    match &mut *iform {
        IForm::LRef(x) => x.lvar.ref_count += 1,
        IForm::LSet(x) => {
            x.lvar.set_count += 1;
            ref_count_lvars(x.value);
        }

        IForm::Call(call) => {
            for &arg in call.args.iter() {
                ref_count_lvars(arg);
            }

            ref_count_lvars(call.proc);
        }

        IForm::Lambda(l) => {
            for lvar in l.lvars.iter_mut() {
                lvar.ref_count += 1;
            }
            ref_count_lvars(l.body);
        }

        IForm::Define(def) => {
            ref_count_lvars(def.value);
        }

        IForm::If(x) => {
            ref_count_lvars(x.cond);
            ref_count_lvars(x.cons);
            ref_count_lvars(x.alt);
        }

        IForm::GSet(x) => {
            ref_count_lvars(x.value);
        }

        IForm::Seq(x) => {
            for &e in x.body.iter() {
                ref_count_lvars(e);
            }
        }

        IForm::Dynenv(e) => {
            ref_count_lvars(e.body);
            for (k, v) in e.kvs.iter().copied() {
                ref_count_lvars(k);
                ref_count_lvars(v);
            }
        }

        IForm::Asm(asm) => {
            for &arg in asm.args.iter() {
                ref_count_lvars(arg);
            }
        }

        IForm::Let(x) => {
            if x.scope == LetScope::Rec {
                for lvar in x.lvars.iter_mut() {
                    lvar.set_count += 1;
                }
            }
            for init in x.inits.iter().copied() {
                ref_count_lvars(init);
            }

            ref_count_lvars(x.body);
        }

        IForm::Const(_) => (),
        IForm::GRef(_) => (),
        _ => (),
    }
}

/// Compiles a single expression into procedure
pub fn compile(
    expr: Value,
    cenv: Value,
    notes: Option<Handle<HashMap<Value, Value>>>,
) -> Result<Value, Value> {
    let thread = Thread::current();

    let iform = pass1::pass1(expr, cenv)?;
    ref_count_lvars(iform);

    let mut bc = bytecompiler::ByteCompiler::new(thread);
    bc.note = notes;
    bc.compile_body(thread, iform, 0);

    let code_block = bc.finalize(thread);

    Ok(make_procedure(thread, code_block).into())
}

pub fn is_global_eq(var: Value, id: Value, cenv: Value) -> bool {
    if var.is_identifier() {
        let v = cenv_lookup(cenv, var);

        if v.is_wrapped_identifier() {
            return is_global_identifier_eq(v, id);
        }
    }

    false
}
