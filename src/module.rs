//!  # Modules
//!
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
//!

use once_cell::sync::Lazy;
use rsgc::{
    prelude::{Handle, Object},
    sync::mutex::Mutex,
};
use rsgc::{system::collections::hashmap::HashMap as HashTable, thread::Thread};
use std::collections::{hash_map::RandomState, HashMap};

use crate::{
    list::{scm_cons, scm_memq},
    object::{Module, ObjectHeader, Symbol, Type, GLOC},
    scm_error, scm_for_each,
    symbol::{make_symbol, scm_symbol_sans_prefix},
    value::Value, compaux::scm_unwrap_identifier,
};
type Modules = Mutex<HashMap<Handle<Symbol>, Value>>;

pub const SCM_BINDING_STAY_IN_MODULE: i32 = 1 << 0;
pub const SCM_BINDING_CONST: i32 = 1 << 1;
pub const SCM_BINDING_INLINABLE: i32 = 1 << 2;
pub const SCM_BINDING_EXTERNAL: i32 = 1 << 3;
pub const SCM_BINDING_DUMMY: i32 = 1 << 4;

static MODULES: Lazy<Modules> = Lazy::new(|| Mutex::new(HashMap::new()));

fn trace_modules(visitor: &mut dyn rsgc::prelude::Visitor) {
    // SAFETY: All threads are paused during
    // root processing so using `unsafe_get` is being accessed by a single thread.
    unsafe {
        MODULES.unsafe_get().iter().for_each(|(k, v)| {
            k.trace(visitor);
            v.trace(visitor);
        })
    }
}

pub(crate) fn init_modules() {
    rsgc::heap::heap::heap().add_root(rsgc::heap::root_processor::SimpleRoot::new(
        "Modules",
        "mod",
        |proc| {
            trace_modules(proc.visitor());

            unsafe {
                let v = proc.visitor();

                DEFAULT_MPL.trace(v);
                DEFAULT_PARENTS.trace(v);
                BOOTSTRAP_MODULE.trace(v);

                NULL_MODULE.trace(v);
                SCHEME_MODULE.trace(v);
                CAPY_MODULE.trace(v);
                INTERNAL_MODULE.trace(v);
                GF_MODULE.trace(v);
                USER_MODULE.trace(v);
                KEYWORD_MODULE.trace(v);
                CKEYWORD_MODULE.trace(v);
                REQBASE_MODULE.trace(v);
            }
        },
    ));
}

static mut DEFAULT_PARENTS: Value = Value::encode_null_value();
static mut DEFAULT_MPL: Value = Value::encode_null_value();
static mut BOOTSTRAP_MODULE: Value = Value::encode_null_value();

macro_rules! def_mod {
    ($($name: ident),*) => {
       $(
        static mut $name: Value = Value::encode_null_value();
       )*
    };
}

def_mod! {
    NULL_MODULE,
    SCHEME_MODULE,
    CAPY_MODULE,
    INTERNAL_MODULE,
    GF_MODULE,
    USER_MODULE,
    KEYWORD_MODULE,
    CKEYWORD_MODULE,
    REQBASE_MODULE
}

fn _make_module(
    name: Value,
    internal: Option<Handle<HashTable<Handle<Symbol>, Value>>>,
) -> Handle<Module> {
    let t = Thread::current();
    let internal = internal.unwrap_or_else(|| HashTable::with_hasher(RandomState::new()));
    let external = HashTable::with_hasher(RandomState::new());

    let mut module = Thread::current().allocate(Module {
        object: ObjectHeader::new(Type::Module),
        name,
        imported: Value::encode_null_value(),
        depended: Value::encode_null_value(),
        export_all: false,
        mpl: Value::encode_null_value(),
        internal,
        parents: unsafe { DEFAULT_PARENTS },
        external,
        origin: Value::encode_null_value(),
        prefix: Value::encode_null_value(),
        sealed: false,
        info: Value::encode_null_value(),
    });

    let mpl = scm_cons(t, Value::encode_object_value(module), unsafe {
        DEFAULT_MPL
    });
    t.write_barrier(module);
    module.mpl = mpl;

    module
}

fn lookup_module(name: Handle<Symbol>) -> Option<Handle<Module>> {
    MODULES.lock(true).get(&name).map(|v| v.module())
}

fn lookup_module_create(name: Handle<Symbol>) -> (Handle<Module>, bool) {
    let mut modules = MODULES.lock(true);
    let mut created = false;
    let module = modules.entry(name).or_insert_with(|| {
        let module = _make_module(Value::encode_object_value(name), None);
        created = true;
        Value::encode_object_value(module)
    });
    (module.module(), created)
}

pub fn scm_make_module(
    name: Option<Handle<Symbol>>,
    error_if_exists: bool,
) -> Option<Handle<Module>> {
    if name.is_none() {
        return Some(_make_module(Value::encode_bool_value(false), None));
    }

    let (r, created) = lookup_module_create(name.unwrap());

    if !created {
        if error_if_exists {
            scm_error!(
                "couldn't create module '{}': named module already exists",
                name.unwrap()
            );
        } else {
            return None;
        }
    }

    Some(r)
}

pub fn scm_find_module(name: Handle<Symbol>, create: bool, quiet: bool) -> Option<Handle<Module>> {
    if create {
        let (m, _created) = lookup_module_create(name);

        Some(m)
    } else {
        if let Some(m) = lookup_module(name) {
            Some(m)
        } else {
            if quiet {
                None
            } else {
                scm_error!("module '{}' not found", name)
            }
        }
    }
}

fn err_sealed(source: Value, target: Handle<Module>) -> ! {
    let what = if source.is_xtype(Type::Module) {
        "import a module"
    } else {
        "create a binding"
    };

    if Value::encode_object_value(target) == unsafe { REQBASE_MODULE } {
        scm_error!(
            "Attempted to {} ({:?}) into capy.require-base. 
            This may be caused by trying to 'use' or 'require' a file in which no module is defined. 
             Make sure the file has define-module/select-module or define-library at the beginning.",
             what, source
        )
    } else {
        scm_error!(
            "Attempted to {} ({:?}) in a sealed module: '{:?}'",
            what,
            source,
            target.name
        )
    }
}

/// Keep record of searched modules.  we use stack array for small # of
/// modules, in order to avoid consing for typical cases.
struct ModuleCache {
    num_searched: usize,
    searched: [Value; 64],
    more_searched: Value,
}

impl ModuleCache {
    pub const fn new() -> Self {
        Self {
            num_searched: 0,
            searched: [Value::encode_null_value(); 64],
            more_searched: Value::encode_null_value(),
        }
    }

    pub fn is_visited(&self, m: Handle<Module>) -> bool {
        for i in 0..self.num_searched {
            if self.searched[i] == Value::encode_object_value(m) {
                return true;
            }
        }

        if !self.more_searched.is_null() {
            if !scm_memq(Value::encode_object_value(m), self.more_searched).is_false() {
                return true;
            }
        }

        false
    }

    pub fn add_visited(&mut self, m: Handle<Module>) {
        if self.num_searched < self.searched.len() {
            self.searched[self.num_searched] = Value::encode_object_value(m);
            self.num_searched += 1;
        } else {
            self.more_searched = scm_cons(
                Thread::current(),
                Value::encode_object_value(m),
                self.more_searched,
            );
        }
    }
}

fn search_binding(
    module: Handle<Module>,
    mut symbol: Handle<Symbol>,
    stay_in_module: bool,
    mut external_only: bool,
    exclude_self: bool,
) -> Option<Handle<GLOC>> {
    let mut searched = ModuleCache::new();

    if !exclude_self {
        let v = if external_only {
            module
                .external
                .get(&symbol)
                .copied()
                .unwrap_or(Value::encode_bool_value(false))
        } else {
            module
                .internal
                .get(&symbol)
                .copied()
                .unwrap_or(Value::encode_bool_value(false))
        };

        if v.is_xtype(Type::GLOC) {
            if v.gloc().value.is_undefined() {
                // If we're here, the symbol is external to MODULE but
                // the real GLOC is somewhere in imported or inherited
                // modules.  We turn off external_only switch so that
                // when we search inherited modules we look into it's
                // internal bindings.
                external_only = false;
                symbol = v.gloc().name.symbol();
            } else {
                return Some(v.gloc());
            }
        } else {
            if stay_in_module {
                return None;
            }

            searched.add_visited(module);
        }
    }

    scm_for_each!(p, module.imported, {
        let elt = p.car();
        let mut sym = Value::encode_object_value(symbol);

        let mut prefixed = false;

        scm_for_each!(mp, elt.module().mpl, {
            let m = mp.car();

            if !prefixed && searched.is_visited(m.module()) {
                continue;
            }

            if m.module().prefix.is_xtype(Type::Symbol) {
                sym = scm_symbol_sans_prefix(sym.symbol(), m.module().prefix.symbol());

                if !sym.is_xtype(Type::Symbol) {
                    break;
                }

                prefixed = true;
            }

            let v = m
                .module()
                .external
                .get(&sym.symbol())
                .copied()
                .unwrap_or(Value::encode_bool_value(false));

            if v.is_xtype(Type::GLOC) {
                let g = v.gloc();
                if g.hidden {
                    break;
                }

                if g.value.is_undefined() {
                    let g2 = search_binding(m.module(), g.name.symbol(), false, false, true);
                    if let Some(g) = g2 {
                        return Some(g);
                    }
                } else {
                    return Some(g);
                }
            }

            if !prefixed {
                searched.add_visited(m.module());
            }
        });
    });

    scm_for_each!(mp, module.mpl, {
        let m = mp.car().module();

        if m.prefix.is_xtype(Type::Symbol) {
            let sym = scm_symbol_sans_prefix(symbol, m.prefix.symbol());
            if !sym.is_xtype(Type::Symbol) {
                return None;
            }

            symbol = sym.symbol();
        }

        let v = if external_only {
            m.external
                .get(&symbol)
                .copied()
                .unwrap_or(Value::encode_bool_value(false))
        } else {
            m.internal
                .get(&symbol)
                .copied()
                .unwrap_or(Value::encode_bool_value(false))
        };

        if v.is_xtype(Type::GLOC) {
            if v.gloc().value.is_undefined() {
                symbol = v.gloc().name.symbol();
                if let Some(g) = search_binding(m, symbol, false, false, true) {
                    return Some(g);
                }

                external_only = false;
            } else {
                return Some(v.gloc());
            }
        }
    });

    None
}

pub fn scm_find_binding(
    module: Handle<Module>,
    symbol: Handle<Symbol>,
    flags: i32,
) -> Option<Handle<GLOC>> {
    let stay_in_module = (flags & SCM_BINDING_STAY_IN_MODULE) != 0;
    let external_only = (flags & SCM_BINDING_EXTERNAL) != 0;

    let mods = MODULES.lock(true);
    let g = search_binding(module, symbol, stay_in_module, external_only, false);
    drop(mods);
    g
}

pub fn scm_global_variable_ref(
    module: Handle<Module>,
    symbol: Handle<Symbol>,
    flags: i32,
) -> Value {
    let g = scm_find_binding(module, symbol, flags);

    g.map(|gloc| gloc.value)
        .unwrap_or(Value::encode_undefined_value())
}

pub fn scm_make_binding(
    mut module: Handle<Module>,
    symbol: Handle<Symbol>,
    value: Value,
    flags: i32,
) -> Handle<GLOC> {
    if module.sealed {
        err_sealed(Value::encode_object_value(symbol), module);
    } else {
        let mut existing = false;

        let mods = MODULES.lock(true);
        let v = module
            .internal
            .get(&symbol)
            .copied()
            .unwrap_or(Value::encode_bool_value(false));

        let mut g = if v.is_xtype(Type::GLOC) {
            existing = true;
            v.gloc()
        } else {
            let g = Thread::current().allocate(GLOC {
                object: ObjectHeader::new(Type::GLOC),
                name: Value::encode_object_value(symbol),
                module: Value::encode_object_value(module),
                value,
                hidden: false,
                getter: None,
                setter: None,
            });

            module
                .internal
                .put(Thread::current(), symbol, Value::encode_object_value(g));

            if module.export_all && symbol.interned {
                module
                    .external
                    .put(Thread::current(), symbol, Value::encode_object_value(g));
            }

            g
        };

        drop(mods);
        if existing {
            eprintln!("warning: redefining global variable: {}", symbol);
        }

        Thread::current().write_barrier(g);
        g.value = value;
        let _ = flags;

        g
    }
}

pub fn scm_define(module: Handle<Module>, name: Handle<Symbol>, value: Value) -> Value {
    let g = scm_make_binding(module, name, value, 0);
    Value::encode_object_value(g)
}

pub fn scm_define_const(module: Handle<Module>, name: Handle<Symbol>, value: Value) -> Value {
    let g = scm_make_binding(module, name, value, SCM_BINDING_CONST);
    Value::encode_object_value(g)
}

/// # Injecting hidden binding
///   This inserts a dummy binding with hidden==true so that
///   the module effectively removes the binding of the given symbol
///   inherited from parent.
///   This is not for genreral use.  It is intended to be used for
///   intermediate anonymous modules, created by import handling
///   routine to implement :except and :rename qualifiers.
///   Since we assume MODULE is for intermediate modules, we only
///   insert bindings to the external table, for those modules are
///   only searched in the 'import' path.
pub fn scm_hide_binding(mut module: Handle<Module>, symbol: Handle<Symbol>) {
    if module.sealed {
        err_sealed(Value::encode_object_value(symbol), module);
    } else {
        let mut err_exists = false;
        let mods = MODULES.lock(true);
        let v = module
            .external
            .get(&symbol)
            .copied()
            .unwrap_or(Value::encode_bool_value(false));

        if !v.is_false() {
            if !v.gloc().hidden {
                err_exists = true;
            }
        } else {
            let g = Thread::current().allocate(GLOC {
                object: ObjectHeader::new(Type::GLOC),
                name: Value::encode_object_value(symbol),
                module: Value::encode_object_value(module),
                value: Value::encode_undefined_value(),
                hidden: true,
                getter: None,
                setter: None,
            });

            module
                .external
                .put(Thread::current(), symbol, Value::encode_object_value(g));
        }

        drop(mods);

        if err_exists {
            scm_error!("hide-binding: binding already exists: {}", symbol);
        }
    }
}

///   # Binding aliasing
///   This is a special operation to realize :only and :rename import option.
///   The name ORIGINNAME is looked up in the module ORIGIN to get a gloc.
///   Then the gloc is directly inserted into the module TARGET under the name
///   TARGETNAME.
///   Since gloc is shared, subsequent changes in the binding are also shared.
///   If the original binding doesn't exist, or isn't exported, noop and
///   FALSE is returned.  Otherwise TRUE is returned.
///   # CAVEATS:
///   - gloc's module remains the same.
///   - autoload won't be resolved.
///   - TARGETNAME shouldn't be bound in TARGET beforehand.  We don't check
///     it and just insert the gloc.  If there is an existing binding,
///     it would become orphaned, possibly causing problems.
///   NB: This is the only operation that causes a gloc to be shared between
///   more than one modules.  I'm not yet clear on the implication of such
///   sharing in general, so this should be used with care.  At least it
///   won't cause much trouble if the target module is an implicit anonymous
///   module created by :only and :rename import options.
pub fn scm_alias_binding(
    mut target: Handle<Module>,
    target_name: Handle<Symbol>,
    origin: Handle<Module>,
    origin_name: Handle<Symbol>,
) -> bool {
    if target.sealed {
        err_sealed(Value::encode_object_value(target_name), target);
    } else {
        let g = scm_find_binding(origin, origin_name, SCM_BINDING_EXTERNAL);
        if let Some(g) = g {
            let mods = MODULES.lock(true);
            target.external.put(
                Thread::current(),
                target_name,
                Value::encode_object_value(g),
            );
            target.internal.put(
                Thread::current(),
                target_name,
                Value::encode_object_value(g),
            );
            drop(mods);
            true
        } else {
            false
        }
    }
}

pub fn scm_import_module(mut module: Handle<Module>, imported: Value, prefix: Value) -> Value {
    if module.sealed {
        err_sealed(imported, module);
    }

    let imp = if imported.is_xtype(Type::Module) {
        Some(imported.module())
    } else if imported.is_xtype(Type::Symbol) {
        scm_find_module(imported.symbol(), false, false)
    } else if imported.is_xtype(Type::Identifier) {
        scm_find_module(scm_unwrap_identifier(imported.identifier()), false, false)
    } else {
        scm_error!("module name or module required, but got {:?}", imported);
    };

    if imp.is_none() {
        scm_error!("module not found: {:?}", imported);
    }
    let imp = imp.unwrap();
    if prefix.is_xtype(Type::Symbol) {
        // TODO: Wrapper module
    }

    let p = scm_cons(Thread::current(), Value::encode_object_value(imp), Value::encode_null_value());

    let mods = MODULES.lock(true);

    {
        
        let mut prev = p;
        Thread::current().write_barrier(p.pair());
        p.pair().cdr = module.imported;

        scm_for_each!(ms, p.cdr(), {
            let m = ms.car().module();
            if m.as_ptr() != imp.as_ptr() {
                prev = ms;
                ms = ms.cdr();
                continue;
            }

            Thread::current().write_barrier(prev.pair());
            prev.pair().cdr = ms.cdr();
            break;
        });

        Thread::current().write_barrier(module);
        module.imported = p;
    }

    drop(mods);

    module.imported
}