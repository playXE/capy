//! # Modules
//!  A module maps symbols to global locations (GLOCs).
//!  The mapping is resolved at the compile time.
//!  Scheme's current-module is therefore a syntax, instead of
//!  a procedure, to capture compile-time information.
//!
//!  Each module has two hashtables; the 'internal' table keeps all the
//!  bindings in the module, while the 'external' table keeps only the
//!  bindings that are exported.  In most cases, the latter is a subset
//!  of the former.  If a binding is renamed on export, however,
//!  two tables map different symbols on the same GLOC.
//!
//!  Modules are registered to a global hash table using their names
//!  as keys, so that the module is retrieved by its name.  The exception
//!  is "anonymous modules", which have #f as the name field
//!  and not registered in the global table.   Anonymous modules are especially
//!  useful for certain applications that need temporary, segregated
//!  namespace---for example, a 'sandbox' environment to evaluate an
//!  expression sent over the network during a session.
//!  The anonymous namespace will be garbage-collected if nobody references
//!  it, recovering its resources.

use crate::{
    hash::{HashTable, Hashing},
    value::{Gloc, Hdr, Type, Value},
    vm::{intern, Vm, vm}, compiler::unwrap_identifier,
};
use once_cell::sync::Lazy;
use rsgc::{
    heap::{heap::heap, root_processor::SimpleRoot},
    prelude::{Allocation, Handle, Object},
    sync::mutex::Mutex,
};

pub struct Modules {
    pub(crate) table: Handle<HashTable>,
    pub(crate) default_parents: Value,
    pub(crate) default_mpl: Value,
    pub(crate) bootstrap_module: Value,

    pub(crate) null_module: Value,
    pub(crate) scheme_module: Value,
    pub(crate) capy_module: Value,
    pub(crate) internal_module: Value,
    pub(crate) gf_module: Value,
    pub(crate) keyword_module: Value,
    pub(crate) capy_keyword_module: Value,
    pub(crate) reqbase_module: Value,
    pub(crate) user_module: Value,
}

/// The global module table.
///
/// # Note on mutex of module operation
///
/// Module operations almost always occur during program loading and
/// interactive session.  Having giant lock for module operations won't
/// affect normal runtime performance.
static MODULES: Lazy<Mutex<Modules>> = Lazy::new(|| {
    let mut m = Modules {
        table: HashTable::with_capacity(Hashing::Eq, 64),
        default_parents: Value::null(),
        default_mpl: Value::null(),
        bootstrap_module: Value::null(),

        null_module: Value::null(),
        scheme_module: Value::null(),
        capy_module: Value::null(),
        internal_module: Value::null(),
        gf_module: Value::null(),
        keyword_module: Value::null(),
        capy_keyword_module: Value::null(),
        reqbase_module: Value::null(),
        user_module: Value::null(),
    };
    let mut mpl = Value::null();
    macro_rules! init_mod { 
        ($name: ident, $mname: expr, $inttab: expr) => {
            {
                let mut module = Module::new(vm(), $mname, $inttab);

                m.table.put(vm(), module.name, unsafe {
                    Value::encode_ptr(module.as_ptr())
                }).unwrap();
                module.parents = if mpl.nullp() {
                    Value::null()
                } else {
                    Value::cons(vm().mutator(), mpl, Value::null())
                };

                mpl = Value::cons(vm().mutator(), unsafe {
                    Value::encode_ptr(module.as_ptr())
                }, mpl);
                module.mpl = mpl;

                m.$name = unsafe {
                    Value::encode_ptr(module.as_ptr())
                };
            }
        };
    }

    init_mod!(null_module, intern("null"), None);
    init_mod!(scheme_module, intern("scheme"), None);
    init_mod!(keyword_module, intern("keyword"), None);
    init_mod!(capy_module, intern("capy"), None);
    init_mod!(gf_module, intern("capy.gf"), None);
    init_mod!(user_module, intern("user"), None);

    mpl = mpl.cdr();

    m.default_parents = Value::cons(vm().mutator(), mpl.car(), Value::null());
    m.default_mpl = mpl;

    mpl = m.default_mpl;

    init_mod!(internal_module, intern("capy.internal"), None);
    init_mod!(reqbase_module, intern("capy.require-base"), None);

    mpl = m.keyword_module.downcast_module().mpl;

    init_mod!(capy_keyword_module, intern("capy.keyword"), Some(m.keyword_module.downcast_module().internal));
    m.capy_keyword_module.downcast_module().export_all = true;

    heap().add_root(SimpleRoot::new("modules", "&m", |root_processor| 
    // SAFETY: root processor is invoked during STW pause so no other thread can access the modules table
    unsafe {
        MODULES.unsafe_get().trace(root_processor.visitor());
    }));
    Mutex::new(m)
});

impl Object for Modules {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.table.trace(visitor);
        self.default_parents.trace(visitor);
        self.default_mpl.trace(visitor);
        self.bootstrap_module.trace(visitor);
        self.null_module.trace(visitor);
        self.scheme_module.trace(visitor);
        self.capy_module.trace(visitor);
        self.internal_module.trace(visitor);
        self.gf_module.trace(visitor);
        self.keyword_module.trace(visitor);
        self.capy_keyword_module.trace(visitor);
        self.reqbase_module.trace(visitor);
    }
}

#[repr(C)]
pub struct Module {
    pub(crate) hdr: Hdr,
    pub(crate) name: Value,
    pub(crate) imported: Value,
    pub(crate) export_all: bool,
    pub(crate) parents: Value,
    pub(crate) mpl: Value,
    pub(crate) depended: Value,
    pub(crate) internal: Handle<HashTable>,
    pub(crate) external: Handle<HashTable>,
    pub(crate) origin: Value,
}

impl Object for Module {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.name.trace(visitor);
        self.imported.trace(visitor);
        self.parents.trace(visitor);
        self.mpl.trace(visitor);
        self.depended.trace(visitor);
        self.internal.trace(visitor);
        self.external.trace(visitor);
        self.origin.trace(visitor);
    }
}

impl Allocation for Module {}

impl Module {
    pub(crate) fn new(
        vm: &mut Vm,
        name: Value,
        internal: Option<Handle<HashTable>>,
    ) -> Handle<Self> {
        let imported = Value::null();
        let depended = imported;
        let parents = unsafe { MODULES.unsafe_get().default_parents };

        let mpl = Value::null();

        let internal = internal.unwrap_or_else(|| HashTable::with_capacity(Hashing::Eq, 0));

        let external = HashTable::with_capacity(Hashing::Eq, 0);
        let mut m = vm.mutator().allocate(Self {
            hdr: Hdr::new(Type::Module),
            name,
            imported,
            export_all: false,
            parents,
            mpl,
            depended,
            internal,
            external,
            origin: Value::null(),
        });

        m.mpl = Value::cons(
            vm.mutator(),
            unsafe { Value::encode_ptr(m.as_ptr()) },
            Value::null(),
        );



        m
    }
}

impl Value {
    pub fn modulep(self) -> bool {
        self.get_type() == Type::Module
    }

    pub fn downcast_module(self) -> Handle<Module> {
        cassert!(self.modulep());
        unsafe { Handle::from_raw(self.handle().as_ptr()) }
    }
}

fn lookup_module(vm: &mut Vm, name: Value) -> Option<Handle<Module>> {
    assert!(name.symbolp(), "module name must be a symbol");

    let modules = MODULES.lock(true);
    let v = modules.table.get(vm, name).unwrap();

    drop(modules);

    v.map(|x| x.downcast_module())
}

fn lookup_module_create(vm: &mut Vm, name: Value) -> Result<Handle<Module>, Handle<Module>> {
    let mut modules = MODULES.lock(true);

    let v = modules.table.get(vm, name).unwrap();

    if let Some(v) = v {
        drop(modules);
        return Err(v.downcast_module());
    } else {
        let m = Module::new(vm, name, None);
        modules
            .table
            .put(vm, name, unsafe { Value::encode_ptr(m.as_ptr()) })
            .unwrap();
        drop(modules);
        Ok(m)
    }
}

pub fn make_module(vm: &mut Vm, name: Value, error_if_exists: bool) -> Result<Value, Value> {
    if !name.symbolp() {
        return Ok(unsafe { Value::encode_ptr(Module::new(vm, name, None).as_ptr()) });
    }

    match lookup_module_create(vm, name) {
        Ok(m) => Ok(unsafe { Value::encode_ptr(m.as_ptr()) }),
        Err(_) => {
            if error_if_exists {
                todo!("error")
            } else {
                Ok(Value::make_false())
            }
        }
    }
}

struct ModuleCache {
    num_searched: usize,
    searched: [Value; 64],
    more_searched: Value,
}

impl ModuleCache {
    pub fn new() -> Self {
        Self {
            num_searched: 0,
            searched: [Value::null(); 64],
            more_searched: Value::null(),
        }
    }

    pub fn visited(&self, m: Handle<Module>) -> bool {
        for i in 0..self.num_searched {
            if self.searched[i] == unsafe { Value::encode_ptr(m.as_ptr()) } {
                return true;
            }
        }

        if !self.more_searched.nullp() {
            let mut l = self.more_searched;
            while !l.nullp() {
                if l.car().downcast_module().as_ptr() == m.as_ptr() {
                    return true;
                }
                l = l.cdr();
            }
        }

        false
    }

    pub fn add_visited(&mut self, vm: &mut Vm, m: Handle<Module>) {
        if self.num_searched < 64 {
            self.searched[self.num_searched] = unsafe { Value::encode_ptr(m.as_ptr()) };
            self.num_searched += 1;
        } else {
            self.more_searched = Value::cons(
                vm.mutator(),
                unsafe { Value::encode_ptr(m.as_ptr()) },
                self.more_searched,
            );
        }
    }
}

fn search_binding(
    vm: &mut Vm,
    module: Handle<Module>,
    sym: Value,
    stay_in_module: bool,
    external_only: bool,
    exclude_self: bool,
) -> Option<Handle<Gloc>> {
    let mut searched = ModuleCache::new();

    if !exclude_self {
        let v = (if external_only {
            module.external.get(vm, sym).unwrap()
        } else {
            module.internal.get(vm, sym).unwrap()
        })
        .unwrap_or(Value::make_null());

        if v.glocp() {
            return Some(v.downcast_gloc());
        }

        if stay_in_module {
            return None;
        }

        searched.add_visited(vm, module);
    }

    let mut p = module.imported;

    while p.pairp() {
        let elt = p.car();

        let m = elt.downcast_module();

        let mut mp = m.mpl;

        while mp.pairp() {
            let m = mp.car().downcast_module();

            if searched.visited(m) {
                mp = mp.cdr();
                continue;
            }

            let v = m.external.get(vm, sym).unwrap().unwrap_or(Value::null());

            if v.glocp() {
                return Some(v.downcast_gloc());
            }

            searched.add_visited(vm, m);

            mp = mp.cdr();
        }

        p = p.cdr();
    }

    let mut mp = module.mpl;

    while mp.pairp() {
        let m = mp.car().downcast_module();

        let v = if external_only {
            m.external
        } else {
            m.internal
        }
        .get(vm, sym)
        .unwrap()
        .unwrap_or(Value::null());

        if v.glocp() {
            return Some(v.downcast_gloc());
        }

        mp = mp.cdr();
    }

    None
}

pub fn find_binding(
    vm: &mut Vm,
    module: Handle<Module>,
    sym: Value,
    stay_in_module: bool,
    external_only: bool,
) -> Option<Handle<Gloc>> {
    let m = MODULES.lock(true);
    let gloc = search_binding(vm, module, sym, stay_in_module, external_only, false);
    drop(m);

    gloc
}

pub fn global_variable_ref(
    vm: &mut Vm,
    module: Handle<Module>,
    sym: Value,
    stay_in_module: bool,
    external_only: bool,
) -> Result<Option<Value>, Value> {
    let g = match find_binding(vm, module, sym, stay_in_module, external_only) {
        Some(g) => g,
        None => {
            return Ok(None);
        }
    };

    if let Some(get) = g.get {
        Ok(Some(get(vm, g)?))
    } else {
        Ok(Some(g.value))
    }
}

fn const_set(vm: &mut Vm, g: Handle<Gloc>, value: Value) -> Result<(), Value> {
    todo!("error: constant")
}

pub fn make_binding(
    vm: &mut Vm,
    mut module: Handle<Module>,
    sym: Value,
    value: Value,
    constant: bool,
) -> Handle<Gloc> {
    let m = MODULES.lock(true);

    let v = module
        .internal
        .get(vm, sym)
        .unwrap()
        .unwrap_or(Value::make_false());
    let existing;
    let mut g = if v.glocp() {
        existing = true;
        v.downcast_gloc()
    } else {
        existing = false;
        let g = vm.mutator().allocate(Gloc {
            hdr: Hdr::new(Type::Gloc),
            value: Value::make_null(),
            set: None,
            get: None,
            name: sym,
            module: unsafe { Value::encode_ptr(module.as_ptr()) },
        });

        module
            .internal
            .put(vm, sym, unsafe { Value::encode_ptr(g.as_ptr()) })
            .unwrap();

        if module.export_all && !sym.downcast_symbol().uninterned {
            module
                .external
                .put(vm, sym, unsafe { Value::encode_ptr(g.as_ptr()) })
                .unwrap();
        }
        g
    };

    drop(m);
    if existing {
        eprintln!("redefining {}#{}", g.module.downcast_module().name, g.name);
    }

    vm.mutator().write_barrier(g);
    g.value = value;

    if constant {
        g.set = Some(const_set);
    } else {
        g.set = None;
        g.get = None;
    }

    g
}

pub fn define(vm: &mut Vm, module: Handle<Module>, sym: &str, value: Value) -> Value {
    let sym = intern(sym);
    let g = make_binding(vm, module, sym, value, false);
    unsafe { Value::encode_ptr(g.as_ptr()) }
}

pub fn define_const(vm: &mut Vm, module: Handle<Module>, sym: &str, value: Value) -> Value {
    let sym = intern(sym);
    let g = make_binding(vm, module, sym, value, true);
    unsafe { Value::encode_ptr(g.as_ptr()) }
}

pub fn alias_binding(
    vm: &mut Vm,
    mut target: Handle<Module>,
    target_name: Value,
    source: Handle<Module>,
    source_name: Value,
) -> bool {
    let g = match find_binding(vm, source, source_name, false, true) {
        Some(g) => g,
        None => return false,
    };

    let m = MODULES.lock(true);
    target
        .external
        .put(vm, target_name, unsafe { Value::encode_ptr(g.as_ptr()) })
        .unwrap();
    target
        .internal
        .put(vm, target_name, unsafe { Value::encode_ptr(g.as_ptr()) })
        .unwrap();
    drop(m);
    true
}


pub fn import_module(
    vm: &mut Vm,
    mut module: Handle<Module>,
    imported: Value,
) -> Result<Value, Value> {
    let imp = if imported.modulep() {
        imported.downcast_module()
    } else if imported.symbolp() {
        find_module(vm, imported, false, false)?.downcast_module()
    } else if imported.identifierp() {
        let id = unwrap_identifier(imported);
        find_module(vm, id, false, false)?.downcast_module()
    } else {
        todo!("error")
    };

    let p = Value::cons(vm.mutator(), unsafe {
        Value::encode_ptr(imp.as_ptr())
    }, Value::null());

    {
        let m = MODULES.lock(true);

        let mut ms;
        let mut prev = p;

        vm.mutator().write_barrier(p.downcast_pair());
        p.set_pair_cdr(module.imported);

        ms = p.cdr();

        while ms.pairp() {
            let m = ms.car().downcast_module();
            if m.as_ptr() != imp.as_ptr() {
                prev = ms;
                ms = ms.cdr();
            } else {
                vm.mutator().write_barrier(prev.downcast_pair());
                prev.set_pair_cdr(ms.cdr());
                break;
            }
        }

        vm.mutator().write_barrier(module);
        module.imported = p;
        drop(m);
    }

    Ok(module.imported)
}

pub fn find_module(vm: &mut Vm, name: Value, create: bool, quiet: bool) -> Result<Value, Value> {
    if create {
        match lookup_module_create(vm, name) {
            Ok(m) | Err(m) => Ok(unsafe { Value::encode_ptr(m.as_ptr()) }),
        }
    } else {
        let m = lookup_module(vm, name);

        if let Some(m) = m {
            Ok(unsafe { Value::encode_ptr(m.as_ptr()) })
        } else {
            if !quiet {
                eprintln!("error: module not found: {}", name);
                todo!("error");
            }
            Err(Value::make_false())
        }
    }
}

pub fn current_module(vm: &mut Vm) -> Value {
    vm.module()
}
