use std::collections::hash_map::RandomState;

use crate::prelude::{eval_error::EvalError, *};

use super::{
    exception::{Exception, SourcePosition},
    special_form::{Form, SpecialForm},
};

#[allow(dead_code)]
pub struct Library {
    pub(crate) name: Value,
    imported: Value,
    export_all: bool,
    parents: Value,
    mpl: Value,
    depended: Value,
    internal: Handle<HashMap<Handle<Symbol>, Value>>,
    external: Handle<HashMap<Handle<Symbol>, Value>>,
    origin: Value,
    prefix: Value,
    info: Value,
    sealed: bool,
    placeholding: bool,
}

impl Object for Library {
    fn trace(&self, visitor: &mut dyn Visitor) {
        self.name.trace(visitor);
        self.imported.trace(visitor);
        self.parents.trace(visitor);
        self.mpl.trace(visitor);
        self.depended.trace(visitor);
        self.internal.trace(visitor);
        self.external.trace(visitor);
        self.origin.trace(visitor);
        self.prefix.trace(visitor);
        self.info.trace(visitor);
    }
}
impl Allocation for Library {}

pub struct LibraryManager {
    libraries: Mutex<Handle<HashMap<Handle<Symbol>, Handle<Library>>>>,
    default_parents: Value,
    default_mpl: Value,
    pub(crate) bootstrap_module: Value,
    pub(crate) null_module: Value,
    pub(crate) user_module: Value,
    pub(crate) scheme_module: Value,
    pub(crate) keyword_module: Value,
}

impl LibraryManager {
    pub fn new() -> Self {
        Self {
            libraries: Mutex::new(HashMap::with_hasher_and_capacity(RandomState::new(), 64)),
            default_parents: Value::nil(),
            default_mpl: Value::nil(),
            bootstrap_module: Value::nil(),
            user_module: Value::nil(),
            scheme_module: Value::nil(),
            keyword_module: Value::nil(),
            null_module: Value::nil(),
        }
    }

    fn _make_module(
        &self,
        name: Value,
        internal: Option<Handle<HashMap<Handle<Symbol>, Value>>>,
    ) -> Handle<Library> {
        let m = Library {
            name,
            imported: Value::nil(),
            export_all: false,
            parents: Value::nil(),
            mpl: Value::nil(),
            depended: Value::nil(),
            internal: if let Some(internal) = internal {
                internal
            } else {
                HashMap::<Handle<Symbol>, Value>::with_hasher(RandomState::new())
            },
            external: { HashMap::<Handle<Symbol>, Value>::with_hasher(RandomState::new()) },
            origin: Value::new(false),
            prefix: Value::new(false),
            info: Value::nil(),
            sealed: false,
            placeholding: false,
        };

        let thread = Thread::current();
        let mut m = thread.allocate(m);
        thread.write_barrier(m);
        m.parents = self.default_parents;
        let mpl = thread.allocate(Pair {
            car: Value::new(m),
            cdr: self.default_mpl,
        });
        thread.write_barrier(m);
        m.mpl = Value::encode_handle_value(mpl);
        m
    }

    pub fn lookup_import(&self, ctx: &mut Context, name: Value) -> Option<Handle<Library>> {
        let mname = self.module_name(ctx, name);
        let name = self.filename(name);
        
        let mname = ctx.runtime().symbol_table().intern(&mname);
        if let Some(lib) = self.find_module(ctx, mname, false, true) {
            return Some(lib);
        }

        let path = ctx
            .runtime()
            .file_manager
            .lock(true)
            .library_file_path(&name, None)
            .unwrap_or(name.to_string());
        match ctx.eval_path(&path, false) {
            Ok(_) => {
                self.find_module(ctx, mname, false, true)
            }
            Err(err) => {
                ctx.error(err);
            }
        }
    }

    pub fn lookup_module(&self, name: Handle<Symbol>) -> Option<Handle<Library>> {
        let libraries = self.libraries.lock(true);
        let res = libraries.get(&name).copied();
        drop(libraries);

        res
    }

    pub fn lookup_module_create(&self, name: Handle<Symbol>) -> (bool, Handle<Library>) {
        let mut libraries = self.libraries.lock(true);
        let res = libraries.get(&name).copied();

        let fin = if let Some(res) = res {
            (false, res)
        } else {
            let m = self._make_module(Value::encode_handle_value(name), None);
            libraries.put(Thread::current(), name, m);
            (true, m)
        };

        drop(libraries);

        fin
    }
    #[allow(unused_mut, unused_variables)]
    fn search_binding(
        module: Handle<Library>,
        sym: Handle<Symbol>,
        stay_in_module: bool,
        external_only: bool,
        exclude_self: bool,
    ) -> Option<Handle<Gloc>> {
        let mut searched = ModuleCache::new();

        if !exclude_self {
            let v = if external_only {
                module.external.get(&sym).copied()
            } else {
                module.internal.get(&sym).copied()
            };

            if let Some(v) = v
                .filter(|x| x.is_handle_of::<Gloc>())
                .map(|x| x.get_handle_of::<Gloc>())
            {
                return Some(v);
            } else {
                if stay_in_module {
                    return None;
                }

                searched.add_visited(Value::new(module));
            }
        }

        let mut mp;
        let mut p;

        scm_for_each!(p, module.imported, {
            let elt = p.car();
            let sym_ = Value::new(sym);
            let mut prefixed = false;

            assert!(elt.is_handle_of::<Library>());
            let elt = elt.get_handle_of::<Library>();
            scm_for_each!(mp, elt.mpl, {
                assert!(mp.car().is_handle_of::<Library>());

                let m = mp.car().get_handle_of::<Library>();
                if !prefixed && searched.module_visitedp(Value::new(m)) {
                    continue;
                }

                if m.prefix.is_symbol() {
                    todo!()
                }

                let v = m.external.get(&sym);
                if let Some(v) = v.map(|x| x.get_handle_of::<Gloc>()) {
                    return Some(v);
                }

                if !prefixed {
                    searched.add_visited(Value::new(m));
                }
            });
        });

        scm_for_each!(mp, module.mpl.cdr(), {
            let m = mp.car().get_handle_of::<Library>();

            let v = if external_only {
                m.external.get(&sym).copied()
            } else {
                m.internal.get(&sym).copied()
            };

            if let Some(v) = v
                .filter(|x| x.is_handle_of::<Gloc>())
                .map(|x| x.get_handle_of::<Gloc>())
            {
                return Some(v);
            }
        });

        None
    }

    pub fn find_binding(
        &self,
        module: Handle<Library>,
        sym: Handle<Symbol>,
        stay_in_module: bool,
        external_only: bool,
    ) -> Option<Handle<Gloc>> {
        let modules = self.libraries.lock(true);
        let res = Self::search_binding(module, sym, stay_in_module, external_only, false);
        drop(modules);
        res
    }

    pub fn global_variable_ref(
        &self,
        ctx: &mut Context,
        module: Handle<Library>,
        sym: Handle<Symbol>,
        stay_in_module: bool,
        external_only: bool,
    ) -> Option<Value> {
        let modules = self.libraries.lock(true);
        let res = Self::search_binding(module, sym, stay_in_module, external_only, false);
        drop(modules);
        res.map(|x| Gloc::get(ctx, x))
    }

    pub fn make_binding(
        &self,
        mut module: Handle<Library>,
        sym: Handle<Symbol>,
        value: Value,
        flag: GlocFlag,
    ) -> Handle<Gloc> {
        let modules = self.libraries.lock(true);

        let gloc = module.internal.get(&sym).copied();
        let mut g;
        if let Some(gloc) = gloc
            .filter(|x| x.is_handle_of::<Gloc>())
            .map(|x| x.get_handle_of::<Gloc>())
        {
            g = gloc;
        } else {
            g = Thread::current().allocate(Gloc::new(sym, module, value));

            module.internal.put(Thread::current(), sym, Value::new(g));
        }
        drop(modules);
        Thread::current().write_barrier(g);
        g.mark(flag);
        g.value = value;
        g
    }

    pub fn define(
        &self,
        module: Handle<Library>,
        sym: Handle<Symbol>,
        value: Value,
        export: bool
    ) -> Handle<Gloc> {
        let gloc = self.make_binding(module, sym, value, GlocFlag::BindingMut);
        if export {
            self.export_symbol(module, sym, sym);
        }
        gloc
    }

    pub fn define_const(
        &self,
        module: Handle<Library>,
        sym: Handle<Symbol>,
        value: Value,
        export: bool
    ) -> Handle<Gloc> {
        let gloc = self.make_binding(module, sym, value, GlocFlag::BindingConst);
        if export {
            self.export_symbol(module, sym, sym);
        }
        gloc
    }

    pub fn insert_binding(
        &self,
        ctx: &mut Context,
        module: Handle<Library>,
        sym: Handle<Symbol>,
        value: Value,
        constant: bool,
    ) -> Option<Handle<Gloc>> {
        if let Some(_) = self.global_variable_ref(ctx, module, sym, true, false) {
            return None;
        } else {
            Some(self.make_binding(
                module,
                sym,
                value,
                if constant {
                    GlocFlag::BindingConst
                } else {
                    GlocFlag::BindingMut
                },
            ))
        }
    }

    pub fn find_module(
        &self,
        ctx: &mut Context,
        sym: Handle<Symbol>,
        create: bool,
        silent: bool,
    ) -> Option<Handle<Library>> {
        if create {

            let (_created, m) = self.lookup_module_create(sym);

            Some(m)
        } else {
            let m = self.lookup_module(sym);

            if let Some(m) = m {
                Some(m)
            } else {
                if !silent {
                    let exc = Exception::eval(
                        ctx,
                        EvalError::LibraryNotFound,
                        &[Value::new(sym)],
                        SourcePosition::unknown(),
                    );
                    ctx.error(exc);
                }

                None
            }
        }
    }

    pub fn import_module(
        &self,
        ctx: &mut Context,
        mut module: Handle<Library>,
        imported: Value,
        prefix: Value,
    ) -> Value {
        let imp;
        if imported.is_handle_of::<Library>() {
            imp = imported.get_handle_of::<Library>();
        } else if imported.is_symbol() {
            imp = self
                .find_module(ctx, imported.get_symbol(), false, false)
                .unwrap();
        } else {
            let exc = Exception::custom(
                ctx,
                "evaluation error",
                "module name or module required, but got $0",
                &[imported],
                SourcePosition::unknown(),
            );
            ctx.error(exc);
        }

        if prefix.is_symbol() {
            todo!()
        }

        let mut p = ctx.mutator().allocate(Pair {
            car: Value::new(imp),
            cdr: Value::nil(),
        });

        let modules = self.libraries.lock(true);

        {
            let mut ms;
            ctx.mutator().write_barrier(p);
            p.cdr = module.imported;
            let mut prev = Value::new(p);

            /* Remove duplicate module, if any.
               NB: We allow to import the same module multiple times if they are
               qualified by :only, :prefix, etc.  Theoretically we should check
               exactly same qualifications, but we hope that kind of duplication
               is rare.
            */
            scm_for_each!(ms, p.cdr(), {
                let m = ms.car().get_handle_of::<Library>();
                if m.as_ptr() == imp.as_ptr() {
                    prev = ms;
                    ms = ms.cdr();
                    continue;
                }

                ctx.mutator().write_barrier(prev.pair());
                prev.pair().cdr = ms.cdr();
                break;
            });

            module.imported = Value::new(p);
        }
        drop(modules);

        Value::new(p)
    }


    pub fn name(&self, ctx: &mut Context, components: &[String]) -> Value {
        let mut res = Value::nil();
        for component in components.iter().rev() {
            let sym = ctx.runtime().symbol_table().intern(component);
            res = ctx.make_pair(Value::new(sym), res);
        }

        res
    }

    pub fn module_name(&self, _ctx: &mut Context, val: Value) -> String {
        let mut components = vec![];
        let mut expr = val;

        while expr.is_pair() {
            let component = expr.car();
            let next = expr.cdr();

            if component.is_symbol() {
                components.push(component.get_symbol().identifier().to_string());
            } else if component.is_int32() {
                components.push(component.get_int32().to_string());
            }
            expr = next;
        }

        std::path::PathBuf::from(components.join("/"))
            .to_str()
            .unwrap()
            .to_string()
    }

    pub fn filename(&self, val: Value) -> String {
        let mut components = vec![];
        let mut expr = val;

        while expr.is_pair() {
            let component = expr.car();
            let next = expr.cdr();

            if component.is_symbol() {
                components.push(component.get_symbol().identifier().to_string());
            } else if component.is_int32() {
                components.push(component.get_int32().to_string());
            }
            expr = next;
        }

        if components.len() > 0 {
            let last = components.len() - 1;
            components[last] = format!("{}.sld", components[components.len() - 1]);
        }

        std::path::PathBuf::from(components.join("/"))
            .to_str()
            .unwrap()
            .to_string()
    }

    pub fn module_name_to_path(name: &str) -> String {
        name.replace(".", "/")
    }

    pub fn path_to_module_name(path: &str) -> String {
        path.replace("/", ".")
    }

    pub fn export_symbol(&self,  mut module: Handle<Library>, name: Handle<Symbol>, exported_name: Handle<Symbol>) {
        let modules = self.libraries.lock(true);
        let mcopy = module;
        let value = match module.internal.entry(name) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(vacant) => {
                let gloc = Thread::current().allocate(Gloc::new(
                    name,
                    mcopy,
                    Value::encode_undefined_value(),
                ));

                
                *vacant.insert(Value::new(gloc))
            }
        };
        module.external.put(Thread::current(), exported_name, value);

        drop(modules);
    }

    /// <spec>  :: <name> | (rename <name> <exported-name>)
    pub fn export_symbols(
        &self,
        ctx: &mut Context,
        mut module: Handle<Library>,
        specs: Value,
    ) -> Value {
        let mut lp;
        //let overwritten = Value::nil();

        scm_for_each!(lp, specs, {
            let spec = lp.car();

            if !(spec.is_symbol()
                || (spec.is_pair()
                    && spec.cdr().is_pair()
                    && spec.cddr().is_pair()
                    && spec.cddr().cdr().is_null()
                    && &*spec.car().get_symbol().identifier() == "rename"
                    && spec.cadr().is_symbol()
                    && spec.cddr().car().is_symbol()))
            {
                let err = Exception::custom(ctx, "evaluation error", "Invalid export-spec; a symbol or (rename <symbol> <symbol>) is expected, but got $0", &[spec], SourcePosition::unknown());
                ctx.error(err);
            }
        });

        let modules = self.libraries.lock(true);

        scm_for_each!(lp, specs, {
            let spec = lp.car();
            let name;
            let exported_name;
           
            if spec.is_symbol() {
                name = spec.get_symbol();
                exported_name = name;
            } else {
                name = spec.cadr().get_symbol();
                exported_name = spec.cddr().car().get_symbol();
                println!("{} -> {}", name.to_string(), exported_name.to_string());
            }

            match module.external.entry(exported_name) {
                Entry::Occupied(entry) => {
                    let g = entry.get().get_handle_of::<Gloc>();
                    if g.name() == name {
                        todo!("handle overwritten symbols");
                    }
                }
                Entry::Vacant(_) => {}
            }
            let mcopy = module;
            let value = match module.internal.entry(name) {
                Entry::Occupied(entry) => *entry.get(),
                Entry::Vacant(vacant) => {
                    let gloc = ctx.mutator().allocate(Gloc::new(
                        name,
                        mcopy,
                        Value::encode_undefined_value(),
                    ));

                    
                    *vacant.insert(Value::new(gloc))
                }
            };
            module.external.put(ctx.mutator(), exported_name, value);
        });

        drop(modules);

        Value::encode_undefined_value()
    }

    pub fn add_definition<'a>(
        &self,
        thread: &mut Thread,
        module: Handle<Library>,
        def: impl Into<Definition<'a>>,
        constant: bool,
        export: bool
    ) -> Value {
        let def = def.into();
        let sym = Runtime::get().symbol_table().intern(def.name());

        let def = match def {
            Definition::NativeProcedure(name, proc) => {
                let name = Str::new(thread, name);
                let proc = Procedure {
                    id: Procedure::new_id(),
                    module,
                    kind: ProcedureKind::Primitive(name, proc, None),
                };

                Value::new(thread.allocate(proc))
            }
            Definition::Special(name, form) => {
                let name = Str::new(thread, name);
                let special = SpecialForm {
                    original_name: Some(name),
                    kind: form,
                };

                Value::new(thread.allocate(special))
            }
            Definition::Value(_, val) => val,
            _ => todo!(),
        };

        if constant {
            self.define_const(module, sym, def, export);
        } else {
            self.define(module, sym, def, export);
        }

        def
    }
}

impl Object for LibraryManager {
    fn trace(&self, visitor: &mut dyn Visitor) {
        unsafe {
            let locked = self.libraries.try_lock(false);
            self.libraries.unsafe_get().trace(visitor);
            drop(locked);

            self.default_mpl.trace(visitor);
            self.default_parents.trace(visitor);
            self.bootstrap_module.trace(visitor);
            self.keyword_module.trace(visitor);
            self.user_module.trace(visitor);
            self.scheme_module.trace(visitor);
            self.user_module.trace(visitor);
        }
    }
}

static LIBRARY_MANAGER: once_cell::sync::Lazy<LibraryManager> = once_cell::sync::Lazy::new(|| {
    let mut m = LibraryManager::new();
    init(&mut m);
    m
});

pub fn library_manager() -> &'static LibraryManager {
    &*LIBRARY_MANAGER
}

struct ModuleCache {
    num_searched: usize,
    searched: [Value; 64],
    more_searched: Value,
}

impl ModuleCache {
    fn module_visitedp(&self, module: Value) -> bool {
        if self.num_searched < 64 {
            for i in 0..self.num_searched {
                if self.searched[i].raw() == module.raw() {
                    return true;
                }
            }
        } else {
            let mut more_searched = self.more_searched;
            while more_searched.is_pair() {
                if more_searched.car() == module {
                    return true;
                }
                more_searched = more_searched.cdr();
            }
        }

        false
    }

    fn add_visited(&mut self, module: Value) {
        if self.num_searched < 64 {
            self.searched[self.num_searched] = module;
            self.num_searched += 1;
        } else {
            let thread = Thread::current();
            self.more_searched = Value::new(thread.allocate(Pair {
                car: module,
                cdr: self.more_searched,
            }));
        }
    }

    fn new() -> Self {
        Self {
            num_searched: 0,
            searched: [Value::nil(); 64],
            more_searched: Value::nil(),
        }
    }
}

pub(crate) fn init(manager: &mut LibraryManager) {
    let rt = Runtime::get();
    let thread = Thread::current();
    let symtab = rt.symbol_table();
    macro_rules! init_mod {
        ($mname: expr, $mpl: ident, $inttab: expr) => {{
            let name = symtab.intern($mname);
            let mut module = manager._make_module(Value::new(name), $inttab);
            module.parents = if $mpl.is_null() {
                Value::nil()
            } else {
                Value::new(thread.allocate(Pair {
                    car: $mpl,
                    cdr: Value::nil(),
                }))
            };
            let new_mpl = Value::new(thread.allocate(Pair {
                car: Value::new(module),
                cdr: $mpl,
            }));

            module.mpl = new_mpl;
            $mpl = new_mpl;
            module
        }};
    }

    let mut mpl = Value::nil();

    manager.null_module = Value::new(init_mod!("null", mpl, None));
    manager.scheme_module = Value::new(init_mod!("scheme", mpl, None));
    manager.keyword_module = Value::new(init_mod!("keyword", mpl, None));
    manager.user_module = Value::new(init_mod!("user", mpl, None));

    mpl = mpl.cdr();
    manager.default_mpl = mpl;
    manager.default_parents = Value::new(thread.allocate(Pair {
        car: mpl.car(),
        cdr: Value::nil(),
    }));
}

pub enum Definition<'a> {
    Special(&'a str, Form),
    NativeProcedure(&'a str, Implementation),
    Value(&'a str, Value),
    ClosureSource(&'a str, String, &'a [&'a str]),
}

impl<'a, T: Into<Implementation>> Into<Definition<'a>> for (&'a str, T) {
    fn into(self) -> Definition<'a> {
        Definition::NativeProcedure(self.0, self.1.into())
    }
}

impl<'a> Into<Definition<'a>> for (&'a str, Form) {
    fn into(self) -> Definition<'a> {
        Definition::Special(self.0, self.1)
    }
}

impl<'a> Into<Definition<'a>> for (&'a str, String, &'a [&'a str]) {
    fn into(self) -> Definition<'a> {
        Definition::ClosureSource(self.0, self.1, self.2)
    }
}

impl<'a> Definition<'a> {
    pub fn name(&self) -> &'a str {
        match self {
            Definition::Special(name, _) => name,
            Definition::NativeProcedure(name, _) => name,
            Definition::ClosureSource(name, _, _) => name,
            Definition::Value(name, _) => name,
        }
    }
}

impl<'a> Into<Definition<'a>> for (&'a str, FormCompiler) {
    fn into(self) -> Definition<'a> {
        Definition::Special(self.0, Form::Primitive(self.1))
    }
}
