use rsgc::{
    prelude::Handle,
    system::{collections::hashmap::HashMap, object::Allocation, traits::Object},
};
use std::{collections::hash_map::RandomState, ptr::null};

use crate::{
    data::{
        equality::eq,
        exception::{Exception, SourcePosition},
        special_form::{Form, Macro, SpecialForm},
    },
    prelude::{
        code::{Code, Ins},
        eval_error::EvalError,
        syntax_rules::SyntaxRules,
        *,
    },
    utilities::arraylist::ArrayList,
};

pub mod binding_group;
pub mod env;

pub enum Environment {
    Global((Handle<Library>, LocalEnv)),
    Local(LocalEnv),
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
    pub vars: Handle<HashMap<Handle<Symbol>, Value, RandomState>>, // symbol -> lvar index or syntax
    pub captures: Handle<HashMap<Handle<Symbol>, Value, RandomState>>, // symbol -> lvar index
    pub parent: Option<Handle<Environment>>,
    pub cc: *const Compiler,
}

impl Object for LocalEnv {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.vars.trace(visitor);
        self.parent.trace(visitor);
        self.captures.trace(visitor);
    }
}

pub enum Access {
    Local(u16),
    Global((Handle<Identifier>, Value)),
    Env(u16),
    Macro(Handle<Procedure>),
    Special(Handle<SpecialForm>),
    Syntax(Handle<SyntaxRules>),
}

impl Environment {
    pub fn bind_to_cc(&mut self, cc: *const Compiler) {
        match self {
            Self::Global((_, lenv)) => lenv.cc = cc,
            Self::Local(env) => env.cc = cc,
        }
    }

    pub fn add_binding(
        &mut self,
        ctx: &mut Context,
        sym: Handle<Symbol>,
        mut incr: impl FnMut() -> usize,
    ) -> usize {
        match self {
            Self::Global((_, lenv)) => {
                if let Some(binding) = lenv.vars.get(&sym) {
                    return binding.get_int32() as usize;
                }
                let key = incr();
                lenv.vars.put(ctx.mutator(), sym, Value::new(key as i32));
                return key;
            }

            Self::Local(env) => {
                if let Some(binding) = env.vars.get(&sym) {
                    return binding.get_int32() as usize;
                }
                let ix = incr();
                env.vars.put(ctx.mutator(), sym, Value::new(ix as i32));
                return ix;
            }
        }
    }

    pub fn make_var(
        &mut self,
        ctx: &mut Context,
        sym: Handle<Symbol>,
        global_on_genv: bool,
    ) -> Access {
        match self {
            Self::Global((library, lenv)) => {
                if global_on_genv {
                    library_manager().make_binding(
                        *library,
                        sym,
                        Value::UNDEFINED,
                        GlocFlag::BindingMut,
                    );
                    let id = Identifier {
                        name: Value::new(sym),
                        module: *library,
                        env: None,
                    };

                    Access::Global((ctx.mutator().allocate(id), Value::UNDEFINED))
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

    pub fn lookup(
        &mut self,
        ctx: &mut Context,
        sym: Handle<Symbol>,
        mut captures: Handle<HashMap<Handle<Symbol>, Value>>,
    ) -> ScmResult<Access> {
        let mut env = self;
        let initial = match env {
            Self::Global((_, lenv)) => lenv.cc,
            Self::Local(lenv) => lenv.cc,
        };
        loop {
            match env {
                Self::Global((library, local)) => {
                    if let Some(index) = local.vars.get(&sym) {
                        if index.is_int32() {
                            if local.cc == initial {
                                return Ok(Access::Local(index.get_int32() as u16));
                            } else {
                                match captures.get(&sym) {
                                    Some(capture) => {
                                        return Ok(Access::Env(capture.get_int32() as u16));
                                    }

                                    None => {
                                        let capture = captures.len() as u16;
                                        captures.put(
                                            ctx.mutator(),
                                            sym,
                                            Value::new(capture as i32),
                                        );

                                        return Ok(Access::Env(capture));
                                    }
                                }
                            }
                        } else {
                            return Ok(Access::Macro(index.get_handle_of()));
                        }
                    }

                    let gloc =
                        library_manager().global_variable_ref(ctx, *library, sym, false, false)?;

                    if gloc.is_none() {
                        let id = Identifier {
                            name: Value::new(sym),
                            module: *library,
                            env: None,
                        };
                        return Ok(Access::Global((
                            ctx.mutator().allocate(id),
                            Value::UNDEFINED,
                        )));
                    }

                    let gloc = gloc.unwrap();

                    if gloc.is_handle_of::<Macro>() {
                        return Ok(Access::Macro(gloc.get_handle_of::<Macro>().0));
                    } else if gloc.is_handle_of::<SpecialForm>() {
                        return Ok(Access::Special(gloc.get_handle_of::<SpecialForm>()));
                    }

                    let id = Identifier {
                        name: Value::new(sym),
                        module: *library,
                        env: None,
                    };
                    return Ok(Access::Global((ctx.mutator().allocate(id), gloc)));
                }

                Self::Local(local) => {
                    if let Some(index) = local.vars.get(&sym) {
                        if index.is_int32() {
                            if local.cc == initial {
                                return Ok(Access::Local(index.get_int32() as u16));
                            } else {
                                match captures.get(&sym) {
                                    Some(capture) => {
                                        return Ok(Access::Env(capture.get_int32() as u16));
                                    }

                                    None => {
                                        let capture = captures.len() as u16;
                                        captures.put(
                                            ctx.mutator(),
                                            sym,
                                            Value::new(capture as i32),
                                        );

                                        return Ok(Access::Env(capture));
                                    }
                                }
                            }
                        } else {
                            return Ok(Access::Macro(index.get_handle_of()));
                        }
                    }

                    env = &mut *local.parent.as_mut().unwrap();
                }
            }
        }
    }
}

pub struct Compiler {
    pub env: Handle<Environment>,
    pub code: ArrayList<Ins>,
    constants: ArrayList<Value>,
    source_directory: Option<Handle<Str>>,
    arguments: Handle<ArrayList<(Handle<Symbol>, usize)>>,
    fragments: ArrayList<Handle<Code>>,
    pub library: Handle<Library>,
    pub num_locals: usize,
    pub max_locals: usize,
    pub captures: Handle<HashMap<Handle<Symbol>, Value>>,
    pub capture_array: ArrayList<ArrayList<Capture>>,
    pub toplevel: bool,
    pub syntax_sym: Value,
}

impl Compiler {
    pub fn make_identifier(
        &mut self,
        ctx: &mut Context,
        sym: Handle<Symbol>,
    ) -> Handle<Identifier> {
        let id = Identifier {
            name: Value::new(sym),
            module: self.library,
            env: None, //self.env
        };

        ctx.mutator().allocate(id)
    }

    pub fn with_library<R>(
        &mut self,
        library: Handle<Library>,
        ctx: &mut Context,
        callback: impl FnOnce(&mut Context, &mut Self) -> ScmResult<R>,
    ) -> ScmResult<R> {
        let env = Environment::Global((
            library,
            LocalEnv {
                vars: HashMap::with_hasher(RandomState::new()),
                captures: self.captures,
                parent: None,
                cc: self as *const Self,
            },
        ));
        let old = self.library;
        self.library = library;
        let saved = self.env;
        self.env = ctx.mutator().allocate(env);
        ctx.try_finally(
            self,
            |ctx, cc| callback(ctx, cc),
            |_, cc| {
                cc.env = saved;
                cc.library = old;
                Ok(())
            },
        )
    }

    pub fn add_constant(&mut self, ctx: &mut Context, value: impl Into<Value>) -> usize {
        let value = value.into();
        for (i, constant) in self.constants.iter().enumerate() {
            if eq(ctx, value, *constant) {
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
        self.code.push(ctx.mutator(), Ins::NoOp);
        ip
    }

    pub fn compile_exprs(&mut self, ctx: &mut Context, expr: Value) -> ScmResult<usize> {
        let mut n = 0;
        let (mut next, _) = Self::desyntax(expr);

        while next.is_pair() {
            self.compile(ctx, next.car(), false)?;
            n += 1;
            next = next.cdr();
        }

        if !next.is_null() {
            panic!();
        }

        Ok(n)
    }

    pub fn offset_to_next(&self, ip: isize) -> isize {
        self.code.len() as isize - ip
    }

    pub fn next_local_index(&mut self) -> usize {
        let index = self.num_locals;
        self.num_locals += 1;
        if self.num_locals > self.max_locals {
            self.max_locals = self.num_locals;
        }
        index
    }

    pub fn compile_multi_bindings(
        &mut self,
        ctx: &mut Context,
        binding_list: Value,
        lenv: Handle<Environment>,
        atomic: bool,
        predef: bool,
    ) -> ScmResult<Handle<Environment>> {
        let lgroup = LocalEnv {
            vars: HashMap::with_hasher(RandomState::new()),
            captures: self.captures,
            parent: Some(self.env),
            cc: self as *const Self,
        };

        let mut group = ctx.mutator().allocate(Environment::Local(lgroup));
        let env = if atomic && !predef {
            lenv
        } else {
            self.env = group;
            group
        };

        let mut bindings = binding_list;
        if predef {
            while bindings.is_pair() && bindings.car().is_pair() {
                let rest = bindings.cdr();
                let variables = bindings.car().car();

                let mut vars = variables;

                while vars.is_pair() && vars.car().is_symbol() {
                    let binding =
                        group.add_binding(ctx, vars.car().get_symbol(), || self.next_local_index());

                    self.emit(ctx, Ins::PushUndef);
                    self.emit(ctx, Ins::MakeLocalVariable(binding as _));

                    vars = vars.cdr();
                }

                bindings = rest;
            }

            bindings = binding_list
        }

        let mut prev_index = -1;

        while bindings.is_pair() {
            let variables = bindings.car();
            if !bindings.cdr().is_pair() {
                let exc = Exception::eval(
                    ctx,
                    EvalError::MalformedBinding,
                    &[variables, bindings],
                    SourcePosition::unknown(),
                );
                ctx.error(exc)?;
            }

            let init = bindings.cdr().car();
            let rest = bindings.cdr().cdr();

            let old = self.env;
            self.env = env;
            self.compile(ctx, init, false)?;
            self.env = old;

            let mut vars = variables;
            let mut syms = ArrayList::new(ctx.mutator());
            while vars.is_pair() && vars.car().is_symbol() {
                syms.push(ctx.mutator(), vars.car().get_symbol());
                vars = vars.cdr();
            }

            if vars.is_null() {
                self.emit(ctx, Ins::Unpack(syms.len() as _, false));
            } else if vars.is_symbol() {
                self.emit(ctx, Ins::Unpack(syms.len() as _, true));
                let binding = group.add_binding(ctx, vars.get_symbol(), || self.next_local_index());
                self.emit(ctx, Ins::SetLocal(binding as _));
                prev_index = binding as _;
            } else {
                let exc = Exception::eval(
                    ctx,
                    EvalError::MalformedBinding,
                    &[variables, binding_list],
                    SourcePosition::unknown(),
                );
                ctx.error(exc)?;
            }

            for sym in syms.iter().rev() {
                let binding = group.add_binding(ctx, *sym, || self.next_local_index());

                if predef || (binding as isize) > prev_index {
                    self.emit(ctx, Ins::SetLocal(binding as _));
                    prev_index = binding as _;
                } else {
                    let exc = Exception::eval(
                        ctx,
                        EvalError::DuplicateBinding,
                        &[Value::new(*sym), binding_list],
                        SourcePosition::unknown(),
                    );
                    ctx.error(exc)?;
                }
            }

            bindings = rest;
        }

        if !bindings.is_null() {
            let exc = Exception::eval(
                ctx,
                EvalError::MalformedBindings,
                &[binding_list],
                SourcePosition::unknown(),
            );
            ctx.error(exc)?;
        }

        Ok(group)
    }

    pub fn compile_custom<F>(
        ctx: &mut Context,
        library: Handle<Library>,
        clos: F,
    ) -> ScmResult<Handle<Code>>
    where
        F: FnOnce(&mut Context, &mut Self) -> ScmResult<()>,
    {
        let captures = HashMap::with_hasher(RandomState::new());
        let local = LocalEnv {
            vars: HashMap::with_hasher(RandomState::new()),
            captures,
            parent: None,
            cc: null(),
        };

        let fragments = ArrayList::new(ctx.mutator());
        let arguments = ArrayList::new(ctx.mutator());
        let mut this = Compiler {
            toplevel: true,
            env: ctx
                .mutator()
                .allocate(Environment::Global((library, local))),
            code: ArrayList::new(ctx.mutator()),
            constants: ArrayList::new(ctx.mutator()),
            source_directory: None,
            arguments: ctx.mutator().allocate(arguments),
            fragments,
            num_locals: 0,
            library,
            syntax_sym: Value::nil(),
            max_locals: 0,
            captures,
            capture_array: ArrayList::new(ctx.mutator()),
        };
        let p = &this as *const Compiler;
        this.env.bind_to_cc(p);

        clos(ctx, &mut this)?;

        let code = ctx.mutator().allocate(Code {
            instructions: this.code,
            constants: this.constants,
            fragments: this.fragments,
            arity: Default::default(),
            module: library,
            captures: this.capture_array,
        });

        Ok(code)
    }

    pub fn compile_bindings(
        &mut self,
        ctx: &mut Context,
        binding_list: Value,
        lenv: Handle<Environment>,
        atomic: bool,
        predef: bool,
        postset: bool,
    ) -> ScmResult<Handle<Environment>> {
        let lgroup = LocalEnv {
            vars: HashMap::with_hasher(RandomState::new()),
            captures: self.captures,
            parent: Some(self.env),
            cc: self as *const Self,
        };

        let mut group = ctx.mutator().allocate(Environment::Local(lgroup));
        let env = if atomic && !predef {
            lenv
        } else {
            self.env = group;
            group
        };

        let mut bindings = binding_list;

        if predef || postset {
            while bindings.is_pair()
                && (bindings.car().is_pair() && bindings.car().car().is_symbol())
            {
                let sym = bindings.car().car().get_symbol();

                let index = group.add_binding(ctx, sym, || self.next_local_index());

                self.emit(ctx, Ins::PushUndef);
                self.emit(ctx, Ins::MakeLocalVariable(index as _));

                bindings = bindings.cdr();
            }

            bindings = binding_list
        }
        let mut definitions = vec![];
        let mut prev_index = -1;

        while bindings.is_pair() {
            let binding = bindings.car();
            let rest = bindings.cdr();

            if binding.is_pair()
                && binding.car().is_symbol()
                && binding.cdr().is_pair()
                && binding.cdr().cdr().is_null()
            {
                let sym = binding.car().get_symbol();
                let expr = binding.cdr().car();

                let old = self.env;
                self.env = env;
                self.compile(ctx, expr, false)?;
                self.env = old;
                let binding = group.add_binding(ctx, sym, || self.next_local_index());

                if !atomic || (predef && !postset) || binding as isize > prev_index {
                } else {
                    let exc = Exception::eval(
                        ctx,
                        EvalError::DuplicateBinding,
                        &[Value::new(sym)],
                        SourcePosition::unknown(),
                    );
                    ctx.error(exc)?;
                }

                if postset {
                    definitions.push(binding);
                } else {
                    self.emit(ctx, Ins::MakeLocalVariable(binding as _));
                }
                prev_index = binding as isize;
            }

            bindings = rest;
        }

        if !bindings.is_null() {
            let exc = Exception::eval(
                ctx,
                EvalError::MalformedBindings,
                &[binding_list],
                SourcePosition::unknown(),
            );
            ctx.error(exc)?;
        }

        for binding in definitions.drain(..).rev() {
            self.emit(ctx, Ins::SetLocal(binding as _));
        }

        Ok(group)
    }

    pub fn build(
        ctx: &mut Context,
        form: Value,
        library: Handle<Library>,
        in_directory: Option<Handle<Str>>,
    ) -> ScmResult<Handle<Code>> {
        let captures = HashMap::with_hasher(RandomState::new());
        let local = LocalEnv {
            vars: HashMap::with_hasher(RandomState::new()),
            captures,
            parent: None,
            cc: null(),
        };
        let fragments = ArrayList::new(ctx.mutator());
        let arguments = ArrayList::new(ctx.mutator());
        let mut this = Compiler {
            toplevel: true,
            env: ctx
                .mutator()
                .allocate(Environment::Global((library, local))),
            code: ArrayList::new(ctx.mutator()),
            constants: ArrayList::new(ctx.mutator()),
            source_directory: in_directory,
            arguments: ctx.mutator().allocate(arguments),
            fragments,
            num_locals: 0,
            library,
            syntax_sym: Value::nil(),
            max_locals: 0,
            captures,
            capture_array: ArrayList::new(ctx.mutator()),
        };
        let p = &this as *const Compiler;
        this.env.bind_to_cc(p);

        this.compile_body(ctx, form, Value::nil(), false)?;

        let code = ctx.mutator().allocate(Code {
            instructions: this.code,
            constants: this.constants,
            fragments: this.fragments,
            arity: Default::default(),
            module: library,
            captures: this.capture_array,
        });

        println!("top-level:");
        code.dump();

        Ok(code)
    }

    pub fn bundle(self, ctx: &mut Context) -> Handle<Code> {
        let this = self;
        //println!("bundle: {:?}", this.code);
        ctx.mutator().allocate(Code {
            instructions: this.code,
            constants: this.constants,
            fragments: this.fragments,
            arity: Default::default(),
            module: this.library,
            captures: this.capture_array,
        })
    }

    pub fn captures(&self) -> &Handle<HashMap<Handle<Symbol>, Value, RandomState>> {
        &self.captures
    }

    pub fn compile_seq(
        &mut self,
        ctx: &mut Context,
        expr: Value,
        tail: bool,
        _local_define: bool,
        in_directory: Option<Handle<Str>>,
    ) -> ScmResult<bool> {
        if expr.is_null() {
            self.emit(ctx, Ins::PushVoid);
            return Ok(false);
        }

        let old_dir = self.source_directory;

        if let Some(dir) = in_directory {
            self.source_directory = Some(dir);
        }
        let mut exprs = ArrayList::new(ctx.mutator());

        let mut next = expr;

        while next.is_pair() {
            exprs.push(ctx.mutator(), next.car());
            next = next.cdr();
        }

        if !next.is_null() {
            panic!();
        }

        ctx.try_finally(
            self,
            |ctx, cc| {
                let mut exit = false;
                for i in 0..exprs.len() {
                    if i > 0 {
                        cc.emit(ctx, Ins::Pop);
                    }

                    exit = cc.compile(ctx, exprs[i], tail && (i == exprs.len() - 1))?;
                }
                Ok(exit)
            },
            |_, cc| {
                cc.source_directory = old_dir;
                Ok(())
            },
        )
    }

    pub fn compile_body(
        &mut self,
        ctx: &mut Context,
        expr: Value,
        _optionals: Value,
        local_define: bool,
    ) -> ScmResult<()> {
        if expr.is_null() {
            self.emit(ctx, Ins::PushVoid);
            self.emit(ctx, Ins::Return);
        } else {
            let reserve_local_ip = self.emit_placeholder(ctx);

            if !self.compile_seq(ctx, expr, true, local_define, None)? {
                self.emit(ctx, Ins::Return);
            }

            if self.max_locals > self.arguments.len() {
                let n = self.max_locals - self.arguments.len();
                self.code[reserve_local_ip] = Ins::Alloc(n as _);
            }
        }
        Ok(())
    }

    pub fn collect_arguments(
        &mut self,
        ctx: &mut Context,
        arglist: Value,
    ) -> (ArrayList<(Handle<Symbol>, usize)>, Value) {
        let mut arguments = ArrayList::new(ctx.mutator());

        let (mut next, _) = Self::desyntax(arglist);
        while next.is_pair() {
            let (arg, _) = Self::desyntax(next.car());
            let (cdr, _) = Self::desyntax(next.cdr());

            if arg.is_symbol() {
                let ix = arguments.len();
                arguments.push(ctx.mutator(), (arg.get_symbol(), ix));
            } else {
                break;
            }

            next = cdr;
        }

        (arguments, next)
    }

    pub fn add_arguments_to_env(&mut self, ctx: &mut Context) {
        let local = match &mut *self.env {
            Environment::Global((_, ref mut local)) => local,
            Environment::Local(ref mut local) => local,
        };

        let mut j = 0;
        for i in 0..self.arguments.len() {
            let (sym, ix) = self.arguments[i];
            local.vars.put(ctx.mutator(), sym, Value::new(ix as i32));
            j += 1;
        }

        self.num_locals = j;
        if self.num_locals > self.max_locals {
            self.max_locals = self.num_locals;
        }
    }

    pub fn compile_lambda(
        &mut self,
        ctx: &mut Context,
        name_idx: Option<usize>,
        arglist: Value,
        body: Value,
        _optionals: bool,
        _atomic: bool,
        tagged: bool,
        continuation: bool,
    ) -> ScmResult<()> {
        let arguments = ArrayList::new(ctx.mutator());
        let captures = HashMap::with_hasher(RandomState::new());
        let mut closure_cc = Compiler {
            toplevel: false,
            env: ctx.mutator().allocate(Environment::Local(LocalEnv {
                vars: HashMap::with_hasher(RandomState::new()),
                captures,
                parent: Some(self.env.clone()),
                cc: null(),
            })),
            code: ArrayList::new(ctx.mutator()),
            constants: ArrayList::new(ctx.mutator()),
            source_directory: self.source_directory,
            arguments: ctx.mutator().allocate(arguments),
            fragments: ArrayList::new(ctx.mutator()),
            library: self.library,
            num_locals: 0,
            syntax_sym: Value::nil(),
            captures,
            max_locals: 0,
            capture_array: ArrayList::new(ctx.mutator()),
        };

        let p = &closure_cc as *const Compiler;
        closure_cc.env.bind_to_cc(p);

        let origarglist = arglist;
        let (mut arglist, next) = self.collect_arguments(ctx, arglist);

        if next.is_null() {
            closure_cc.emit(ctx, Ins::AssertArgCount(arglist.len() as _));
            ctx.mutator().write_barrier(closure_cc.arguments);
            closure_cc.arguments.replace(arglist);
            closure_cc.add_arguments_to_env(ctx);
            closure_cc.compile_body(ctx, body, Value::nil(), true)?;
        } else if next.is_symbol() {
            if arglist.len() > 0 {
                closure_cc.emit(ctx, Ins::AssertMinArgCount(arglist.len() as _));
            }

            closure_cc.emit(ctx, Ins::CollectRest(arglist.len() as _));
            arglist.push(ctx.mutator(), (next.get_symbol(), arglist.len()));
            ctx.mutator().write_barrier(closure_cc.arguments);
            closure_cc.arguments.replace(arglist);
            closure_cc.add_arguments_to_env(ctx);

            closure_cc.compile_body(ctx, body, Value::nil(), true)?;
        } else {
            // TODO: Optionals: (lambda x . y z) ; y is optional
            let exc = Exception::eval(
                ctx,
                EvalError::MalformedArgumentList,
                &[origarglist],
                SourcePosition::unknown(),
            );
            ctx.error(exc)?;
        }

        let code_index = self.fragments.len();

        let library = closure_cc.library;
        let captures = *closure_cc.captures();
        if !tagged && !continuation && closure_cc.captures().len() == 0 {
            let code = closure_cc.bundle(ctx);

            let typ;
            if let Some(idx) = name_idx {
                let c = self.constants[idx];
                if c.is_symbol() {
                    typ = ClosureType::Named(c.get_symbol().identifier());
                } else {
                    typ = ClosureType::Anonymous;
                }
            } else {
                typ = ClosureType::Anonymous;
            }

            let proc = Procedure {
                id: Procedure::new_id(),
                kind: ProcedureKind::Closure(
                    typ,
                    Value::nil(),
                    None,
                    code,
                ),
                module: library,
            };

            let proc = ctx.mutator().allocate(proc);
            let proc = Value::new(proc);
            let ix = self.add_constant(ctx, proc);
            self.emit(ctx, Ins::PushConstant(ix as _));
            println!("Dump for closure",);
            code.dump();
            Ok(())
        } else {
            let code = closure_cc.bundle(ctx);
            self.fragments.push(ctx.mutator(), code);

            let mut capture_array = ArrayList::new(ctx.mutator());

            for (name, _) in captures.iter() {
                match self.env.lookup(ctx, *name, self.captures)? {
                    Access::Env(env) => capture_array.push(
                        ctx.mutator(),
                        Capture {
                            local: false,
                            index: env,
                        },
                    ),
                    Access::Local(env) => capture_array.push(
                        ctx.mutator(),
                        Capture {
                            local: true,
                            index: env,
                        },
                    ),

                    _ => unreachable!(),
                }
            }
            let ix = self.capture_array.len();
            self.capture_array.push(ctx.mutator(), capture_array);

            if tagged {
                self.emit(
                    ctx,
                    Ins::MakeTaggedClosure(
                        if let Some(name_idx) = name_idx {
                            name_idx as _
                        } else if continuation {
                            -2
                        } else {
                            -1
                        },
                        ix as _,
                        code_index as _,
                    ),
                );
            } else {
                self.emit(
                    ctx,
                    Ins::MakeClosure(
                        if let Some(name_idx) = name_idx {
                            name_idx as _
                        } else if continuation {
                            -2
                        } else {
                            -1
                        },
                        ix as _,
                        code_index as _,
                    ),
                );
            }

            println!("Dump for closure",);
            self.fragments[code_index].dump();
            Ok(())
        }
    }

    pub fn check_toplevel(
        &mut self,
        ctx: &mut Context,
        form: Value,
        pos: SourcePosition,
    ) -> ScmResult<()> {
        if !self.toplevel && !matches!(&*self.env, Environment::Global(_)) {
            let exc = Exception::eval(ctx, EvalError::TopLevelForm, &[form], pos);
            ctx.error(exc)?
        } else {
            Ok(())
        }
    }

    pub fn set_value_of(&mut self, ctx: &mut Context, sym: Handle<Symbol>) -> ScmResult<()> {
        match self.env.lookup(ctx, sym, self.captures)? {
            Access::Env(env) => {
                self.emit(ctx, Ins::SetCaptured(env));
            }

            Access::Local(x) => {
                self.emit(ctx, Ins::SetLocal(x));
            }

            Access::Global((x, _)) => {
                let c = self.add_constant(ctx, x);
                self.emit(ctx, Ins::SetGlobal(c as _));
            }

            _ => {
                let exc = Exception::eval(
                    ctx,
                    EvalError::UnboundVariable,
                    &[Value::new(sym)],
                    SourcePosition::unknown(),
                );
                ctx.error(exc)?;
            }
        }
        Ok(())
    }

    pub fn push_lookup(&mut self, ctx: &mut Context, sym: Handle<Symbol>) -> ScmResult<()> {
        match self.env.lookup(ctx, sym, self.captures)? {
            Access::Env(x) => {
                self.emit(ctx, Ins::PushCaptured(x as _));
            }
            Access::Local(x) => {
                self.emit(ctx, Ins::PushLocal(x as _));
            }

            Access::Global((x, _)) => {
                let c = self.add_constant(ctx, Value::new(x));
                self.emit(ctx, Ins::PushGlobal(c as _));
            }

            _ => todo!(),
        }
        Ok(())
    }

    pub fn locals_len(&self) -> usize {
        match &*self.env {
            Environment::Global((_, local)) => local.vars.len(),
            Environment::Local(ref local) => local.vars.len(),
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

    pub fn desyntax_rec(ctx: &mut Context, form: Value) -> Value {
        if form.is_pair() {
            let car = Self::desyntax_rec(ctx, form.car());
            let cdr = Self::desyntax_rec(ctx, form.cdr());
            ctx.make_pair(car, cdr)
        } else if form.is_vector() {
            let mut vec = ArrayList::new(ctx.mutator());
            let form = form.get_handle_of::<Vector>();
            for x in form.elements.iter() {
                let x = Self::desyntax_rec(ctx, *x);
                vec.push(ctx.mutator(), x);
            }

            Value::new(ctx.mutator().allocate(Vector {
                mutable: form.mutable,
                growable: form.growable,
                elements: vec,
            }))
        } else if form.is_handle_of::<Syntax>() {
            let syntax = form.get_handle_of::<Syntax>();
            syntax.expr
        } else {
            form
        }
    }

    pub fn compile(&mut self, ctx: &mut Context, form: Value, tail: bool) -> ScmResult<bool> {
        let sform = form;
        let (form, pos) = Self::desyntax(form);

        if form.is_pair() {
            if !form.is_list() {
                let exc = Exception::eval(ctx, EvalError::ProperList, &[form], pos);
                ctx.error(exc)?;
            }

            let (car, _) = Self::desyntax(form.car());
            let (args, _) = Self::desyntax(form.cdr());

            if !car.is_symbol() {
                self.emit(ctx, Ins::MakeFrame);
                self.compile(ctx, car, false)?;
                let nargs = self.compile_exprs(ctx, args)?;
                return Ok(self.call(ctx, nargs, tail));
            }

            match self.env.lookup(ctx, car.get_symbol(), self.captures)? {
                Access::Env(up) => {
                    self.emit(ctx, Ins::MakeFrame);
                    self.emit(ctx, Ins::PushCaptured(up));
                    let nargs = self.compile_exprs(ctx, args)?;
                    return Ok(self.call(ctx, nargs, tail));
                }
                Access::Local(index) => {
                    self.emit(ctx, Ins::MakeFrame);
                    self.emit(ctx, Ins::PushLocal(index));
                    let nargs = self.compile_exprs(ctx, args)?;
                    return Ok(self.call(ctx, nargs, tail));
                }

                Access::Global((sym, gloc)) => {
                    if gloc.is_handle_of::<Procedure>() {
                        if let ProcedureKind::Primitive(_, _, Some(ref form_compiler)) =
                            gloc.get_handle_of::<Procedure>().kind
                        {
                            return form_compiler(self, ctx, sform, tail);
                        }
                    }

                    
                    self.emit(ctx, Ins::MakeFrame);
                    let ix = self.add_constant(ctx, Value::new(sym));
                    self.emit(ctx, Ins::PushGlobal(ix as _));
                    let nargs = self.compile_exprs(ctx, args)?;
                    return Ok(self.call(ctx, nargs, tail));
                }

                Access::Special(special) => match special.kind {
                    Form::Primitive(form_compiler) => {
                        return form_compiler(self, ctx, sform, tail);
                    }
                    Form::Macro(transformer) => {
                        let expanded = ctx.apply(transformer.into(), &[form])?;
                        return self.compile(ctx, expanded, tail);
                    }
                },

                Access::Macro(_) => todo!("macro expansion"),
                Access::Syntax(_) => todo!("syntax-rules expansion"),
            }
        } else if form.is_symbol() {
            match self.env.lookup(ctx, form.get_symbol(), self.captures)? {
                Access::Env(up) => {
                    self.emit(ctx, Ins::PushCaptured(up));
                    return Ok(false);
                }
                Access::Local(index) => {
                    self.emit(ctx, Ins::PushLocal(index));
                    return Ok(false);
                }

                Access::Global((sym, _)) => {
                    let ix = self.add_constant(ctx, Value::new(sym));
                    self.emit(ctx, Ins::PushGlobal(ix as _));
                    return Ok(false);
                }

                Access::Special(special) => match special.kind {
                    Form::Primitive(form_compiler) => {
                        return form_compiler(self, ctx, sform, false);
                    }
                    _ => todo!("macro expansion"),
                },

                Access::Macro(_) => todo!("macro expansion"),
                Access::Syntax(_) => todo!("syntax-rules expansion"),
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

        Ok(false)
    }

    pub fn call(&mut self, ctx: &mut Context, n: usize, tail: bool) -> bool {
        if tail {
            self.emit(ctx, Ins::TailCall(n as _));
            true
        } else {
            self.emit(ctx, Ins::Call(n as _));
            false
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
        Expr::Syntax(_pos, expr) => {
            let expr = r7rs_to_value(ctx, source_id, expr);
            expr
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
        _ => todo!(
            "expression not yet supported: {}",
            expr.to_string(&NoIntern, true)
        ),
    }
}

#[derive(Clone, Copy)]
pub struct Capture {
    pub index: u16,
    pub local: bool,
}

impl Object for Capture {}
impl Allocation for Capture {}
