use std::collections::hash_map::RandomState;
use rsgc::{system::{traits::Object, object::Allocation, collections::hashmap::HashMap}, prelude::Handle};

use crate::{prelude::{*, eval_error::EvalError, code::{Ins, Code}, syntax_rules::SyntaxRules}, data::{special_form::{Macro, SpecialForm, Form}, exception::{Exception, SourcePosition}}, utilities::arraylist::ArrayList};

pub mod env;
pub mod binding_group;

pub enum Environment {
    Global((Handle<Library>, LocalEnv)),
    Local(LocalEnv)
}

impl Object for Environment {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        match self {
            Environment::Global((lib, lenv)) => {
                lib.trace(visitor);
                lenv.trace(visitor);
            }
            Environment::Local(vars) => vars.trace(visitor),
        }
    }
}

impl Allocation for Environment {}

pub struct LocalEnv {
    vars: Handle<HashMap<Handle<Symbol>, Value, RandomState>>, // symbol -> lvar index or syntax
    captures: Handle<HashMap<Handle<Symbol>, Value, RandomState>>, // symbol -> lvar index
    parent: Option<Handle<Environment>>,
}

impl Object for LocalEnv {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.vars.trace(visitor);
        self.parent.trace(visitor);
    }
}

pub enum Access {
    Local(u16),
    Global(Handle<Symbol>),
    Env(u16),
    Macro(Handle<Procedure>),
    Special(Handle<SpecialForm>),
    Syntax(Handle<SyntaxRules>)
}

impl Environment {
    pub fn make_var(&mut self, ctx: &mut Context, sym: Handle<Symbol>, global_on_genv: bool) -> Access {
        match self {
            Self::Global((library, lenv)) => {
                if global_on_genv {
                library_manager().make_binding(*library, sym, Value::UNDEFINED, GlocFlag::BindingMut);
                Access::Global(sym)
                } else {
                    let index = lenv.vars.len() as u16;
                    lenv.vars.put(ctx.mutator(), sym, Value::new(index as i32));

                    Access::Local(index)
                }
                
            }

            Self::Local(env) => {
                let index = env.vars.len() as u16;
                env.vars.put(ctx.mutator(), sym, Value::new(index as i32));

                Access::Local(index)
            }
        }
    }

    pub fn lookup(&mut self, ctx: &mut Context, sym: Handle<Symbol>) -> Access {
        let mut env = self;
        let mut depth = 0;
        loop {
            match env {
                Self::Global((library, local)) => {
                    if let Some(index) = local.vars.get(&sym) {
                        if index.is_int32() {
                            if depth == 0 {
                                return Access::Local(index.get_int32() as u16);
                            } else {
                                match local.captures.get(&sym) {
                                    Some(capture) => {
                                        return Access::Env(capture.get_int32() as u16);
                                    }

                                    None => {
                                        let capture = local.captures.len() as u16;
                                        local.captures.put(ctx.mutator(), sym, Value::new(capture as i32));

                                        return Access::Env(capture);
                                    }
                                }
                            }
                        } else {
                            return Access::Macro(index.get_handle_of());
                        }
                    }
                    let gloc = library_manager().global_variable_ref(ctx, *library, sym, false, false);

                    if gloc.is_none() {
                        return Access::Global(sym);
                    }

                    let gloc = gloc.unwrap();

                    if gloc.is_handle_of::<Macro>() {
                        return Access::Macro(gloc.get_handle_of::<Macro>().0);
                    } else if gloc.is_handle_of::<SpecialForm>() {
                        return Access::Special(gloc.get_handle_of::<SpecialForm>());
                    }

                    return Access::Global(sym);
                }

                Self::Local(local)=> {
                    if let Some(index) = local.vars.get(&sym) {
                        if index.is_int32() {
                            if depth == 0 {
                                return Access::Local(index.get_int32() as u16);
                            } else {
                                match local.captures.get(&sym) {
                                    Some(capture) => {
                                        return Access::Env(capture.get_int32() as u16);
                                    }

                                    None => {
                                        let capture = local.captures.len() as u16;
                                        local.captures.put(ctx.mutator(), sym, Value::new(capture as i32));

                                        return Access::Env(capture);
                                    }
                                }
                            }
                        } else {
                            return Access::Macro(index.get_handle_of());
                        }
                    }

                    env = &mut *local.parent.as_mut().unwrap();
                }
            }

            depth += 1;
        }
    }

     
}


pub struct Compiler {
    env: Handle<Environment>,
    code: ArrayList<Ins>,
    constants: ArrayList<Value>,
    source_directory: Option<Handle<Str>>,
    arguments: Handle<ArrayList<Value>>,
    fragments: ArrayList<Handle<Code>>
}

impl Compiler {
    pub fn add_constant(&mut self, ctx: &mut Context, value: Value) -> usize {
        for (i, constant) in self.constants.iter().enumerate() {
            if constant.raw() == value.raw() {
                return i;
            }
        }
        let index = self.constants.len();
        self.constants.push(ctx.mutator(), value);

        index
    }

    pub fn emit(&mut self, ctx: &mut Context, op: Ins) -> usize {
        let ip = self.code.len();
        self.code.push(ctx.mutator(), op);
        ip 
    }

    pub fn emit_placeholder(&mut self, ctx: &mut Context) -> usize {
        let ip = self.code.len();
        self.code.push(ctx.mutator(), Ins::Branch(0));
        ip
    }

    pub fn compile_exprs(&mut self, ctx: &mut Context, expr: Value) -> usize {
        let mut n = 0;
        let (mut next, _) = Self::desyntax(expr);

        while next.is_pair() {
            self.compile(ctx, next.car(), false);
            n += 1;
            next = next.cdr();
        }

        if !next.is_null() {
            panic!();
        }

        n
    }



    pub fn build(ctx: &mut Context, form: Value, library: Handle<Library>, in_directory: Option<Handle<Str>>) -> Handle<Code> {
        let local = LocalEnv {
            vars: HashMap::with_hasher(RandomState::new()),
            captures: HashMap::with_hasher(RandomState::new()),
            parent: None
        };
        let fragments = ArrayList::new(ctx.mutator());
        let mut this = Compiler {
            env: ctx.mutator().allocate(Environment::Global((library, local))),
            code: ArrayList::new(ctx.mutator()),
            constants: ArrayList::new(ctx.mutator()),
            source_directory: in_directory,
            arguments: ctx.runtime().empty_arraylist,
            fragments
        };

        this.compile_body(ctx, form, Value::nil(), false);

        let code = ctx.mutator().allocate(Code {
            instructions: this.code,
            constants: this.constants,
            fragments: this.fragments,
            arity: Default::default(),
            module: library
        });

        println!("code: {:?}", code.instructions);

        code
    }

    

    pub fn compile_seq(&mut self, ctx: &mut Context, expr: Value, tail: bool, _local_define: bool, in_directory: Option<Handle<Str>>) -> bool {
        if expr.is_null() {
            self.emit(ctx, Ins::PushVoid);
            return false;
        }

        let old_dir = self.source_directory;

        if let Some(dir) = in_directory {
            self.source_directory = Some(dir);
        }

        ctx.try_finally(self,|ctx, cc| {
            let mut next = expr;
            let mut i = 0;
            let mut exit = false;
            let len = expr.length_recsafe();
            while next.is_pair() {
                let car = next.car();
                let cdr = next.cdr();
                if i > 0 {
                    cc.emit(ctx, Ins::Pop);
                }

                exit = cc.compile(ctx, car, tail && (i == len.1));
                i += 1;
                next = cdr; 
            }

            exit
        }, |_, cc| {
            cc.source_directory = old_dir;
        })
    }

    pub fn compile_body(&mut self, ctx: &mut Context, expr: Value, _optionals: Value, local_define: bool) {
        if expr.is_null() {
            self.emit(ctx, Ins::PushVoid);
            self.emit(ctx, Ins::Return);
        } else {
            let reserve_local_ip = self.emit_placeholder(ctx);

            if !self.compile_seq(ctx, expr, true, local_define, None) {
                self.emit(ctx, Ins::Return);
            }

            if self.locals_len() > 0 {
                let n = self.locals_len() - self.arguments.len();
                self.code[reserve_local_ip] = Ins::Alloc(n as _);
            }
        }
    }

    pub fn locals_len(&self) -> usize {
        match &*self.env {
            Environment::Global((_, local)) => local.vars.len(),
            Environment::Local(ref local) => local.vars.len()
        }
    }

    pub fn desyntax(form: Value) -> (Value, SourcePosition) {
        if form.is_handle_of::<Syntax>() {
            let syntax = form.get_handle_of::<Syntax>();
            (syntax.expr, syntax.pos)
        } else {
            (form, SourcePosition::unknown())
        }
    }

    pub fn compile(&mut self, ctx: &mut Context, form: Value, tail: bool) -> bool {
        let (form ,pos) = Self::desyntax(form);

        if form.is_pair() {
            if !form.is_list() {
                let exc = Exception::eval(ctx, EvalError::ProperList, &[form], pos);
                ctx.error(exc);
            }

            let (car , _) = Self::desyntax(form.car());
            let (args, _) = Self::desyntax(form.cdr());

            if !car.is_symbol() {
                println!("car is: {}", car.is_handle_of::<Syntax>());
                let exc = Exception::eval(ctx, EvalError::NonApplicativeValue, &[car], pos);
                ctx.error(exc);
            }

            match self.env.lookup(ctx, car.get_symbol()) {
                Access::Env(up) => {
                    self.emit(ctx, Ins::MakeFrame);
                    self.emit(ctx, Ins::PushCaptured(up));
                    let nargs = self.compile_exprs(ctx, args);
                    self.call(ctx, nargs, tail);
                    return true;
                }
                Access::Local(index) => {
                    self.emit(ctx, Ins::MakeFrame);
                    self.emit(ctx, Ins::PushLocal(index));
                    let nargs = self.compile_exprs(ctx, args);
                    self.call(ctx, nargs, tail);
                    return true;
                }

                Access::Global(sym) => {
                    self.emit(ctx, Ins::MakeFrame);
                    let ix = self.add_constant(ctx, Value::new(sym));
                    self.emit(ctx, Ins::PushGlobal(ix as _));
                    let nargs = self.compile_exprs(ctx, args);
                    self.call(ctx, nargs, tail);
                    return true;
                }

                Access::Special(special) => {
                    match special.kind {
                        Form::Primitive(form_compiler) => {
                            return form_compiler(self, ctx, form, false);
                        }
                        _ => todo!("macro expansion")
                    }
                }

                Access::Macro(_) => todo!("macro expansion"),
                Access::Syntax(_) => todo!("syntax-rules expansion")
            }
        } else if form.is_symbol() {
            match self.env.lookup(ctx, form.get_symbol()) {
                Access::Env(up) => {
                    self.emit(ctx, Ins::PushCaptured(up));
                    return false;
                }
                Access::Local(index) => {
                    self.emit(ctx, Ins::PushLocal(index));
                    return false;
                }

                Access::Global(sym) => {
                    let ix = self.add_constant(ctx, Value::new(sym));
                    self.emit(ctx, Ins::PushGlobal(ix as _));
                    return false;
                }

                Access::Special(special) => {
                    match special.kind {
                        Form::Primitive(form_compiler) => {
                            return form_compiler(self, ctx, form, false);
                        }
                        _ => todo!("macro expansion")
                    }
                }

                Access::Macro(_) => todo!("macro expansion"),
                Access::Syntax(_) => todo!("syntax-rules expansion")
            }
        } else if form.is_int32() {
            let x = form.get_int32();
            self.emit(ctx, Ins::PushFixnum(x));
        } else if form.is_bool() {
            if form.is_true() {
                self.emit(ctx, Ins::PushTrue);
            } else {
                self.emit(ctx, Ins::PushFalse);
            }
        } else if form.is_undefined() {
            self.emit(ctx, Ins::PushUndef);
        } else if form.is_eof() {
            self.emit(ctx, Ins::PushEof);
        } else if form.is_void() {
            self.emit(ctx, Ins::PushVoid);
        } else {
            let ix = self.add_constant(ctx, form);
            self.emit(ctx, Ins::PushConstant(ix as _));
        }

        false
    }

    fn call(&mut self,ctx: &mut Context, n: usize, tail: bool) {
        if tail {
            self.emit(ctx, Ins::TailCall(n as _));
        } else {
            self.emit(ctx, Ins::Call(n as _));
        }
    }
}

impl Object for Compiler {}
impl Allocation for Compiler {}

use r7rs_parser::expr::{Expr, NoIntern};

pub fn r7rs_to_value(ctx: &mut Context, source_id: u32, expr: &Expr<NoIntern>) -> Value {
    match expr {
        Expr::Bool(x) => Value::new(*x),
        Expr::Fixnum(x) => Value::new(*x),
        Expr::Float(x) => Value::new(*x),
        Expr::Null => Value::nil(),
        Expr::Pair(x, y) => {
            let x = r7rs_to_value(ctx, source_id, x);
            let y = r7rs_to_value(ctx, source_id, y);
            ctx.make_pair(x, y)
        }
        Expr::Syntax(pos, expr) => {
            let expr = r7rs_to_value(ctx, source_id, expr);
            let syntax = Syntax {
                pos: SourcePosition {
                    position: *pos,
                    source_id
                },
                expr
            };
            Value::new(ctx.mutator().allocate(syntax))
        }

        Expr::Symbol(x) => {
            let sym = ctx.runtime().symbol_table().intern(x);
            Value::new(sym)
        }
        Expr::Str(x) => {
            let str = Str::new(ctx.mutator(), x);
            Value::new(str)
        }

        // todo: bigint, rational, complex, bvector, gvector, ivector
        _ => todo!("expression not yet supported: {}", expr.to_string(&NoIntern, true))
    }
}