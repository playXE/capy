use core::fmt;
use std::{hash::Hash, sync::atomic::AtomicUsize};

use dashmap::DashMap;

use crate::{
    compiler::env::{Env, WeakEnv},
    prelude::*,
};
pub enum Symbol {
    Interned(Handle<Str>),
    Uninterned(Handle<Str>),
    Generated(Handle<Symbol>, WeakEnv),
}

impl Symbol {
    pub fn identifier(&self) -> Handle<Str> {
        match self {
            Symbol::Interned(x) => *x,
            Symbol::Uninterned(x) => *x,
            Symbol::Generated(x, _) => x.identifier(),
        }
    }

    pub fn raw_identifier(&self) -> Handle<Str> {
        match self {
            Symbol::Interned(x) => *x,
            Symbol::Uninterned(x) => *x,
            _ => unreachable!("no interned or uninterned symbol"),
        }
    }

    pub fn is_interned(&self) -> bool {
        match self {
            Symbol::Interned(_) => true,
            _ => false,
        }
    }

    pub fn is_generated(&self) -> bool {
        match self {
            Symbol::Generated(_, _) => true,
            _ => false,
        }
    }

    pub fn lexical(&self) -> Option<(Handle<Symbol>, Env)> {
        match self {
            Symbol::Generated(symbol, weak_env) => Some((*symbol, weak_env.env())),
            _ => None,
        }
    }

    pub fn root(this: Handle<Symbol>) -> Handle<Symbol> {
        match &*this {
            Symbol::Generated(symbol, _) => Symbol::root(*symbol),
            _ => this,
        }
    }

    pub fn root_ref(this: &Symbol) -> &Symbol {
        match this {
            Symbol::Generated(symbol, _) => Symbol::root_ref(&*symbol),
            _ => this,
        }
    }

    /// If symbol `this` has a prefix `p`, returns a symbol without the prefix.
    /// Otherwise, returns None.
    pub fn sans_prefix(this: Handle<Symbol>, p: impl AsRef<str>) -> Option<Handle<Symbol>> {
        let bp = this.identifier();
        let bs = p.as_ref();
        if bp.len() < bs.len() {
            return None;
        }
        if bp.starts_with(bs) {
            let rt = Runtime::get();
            Some(rt.symbol_table().intern(&bp[bs.len()..]))
        } else {
            None
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Interned(x) | Self::Uninterned(x) => write!(f, "{}", x),
            Self::Generated(symbol, env) => write!(
                f,
                "[{} {:p} {}]",
                Symbol::root_ref(self),
                *symbol,
                env.env()
            ),
        }
    }
}

impl Allocation for Symbol {}

impl Object for Symbol {
    fn trace(&self, visitor: &mut dyn Visitor) {
        match self {
            Symbol::Interned(string) => string.trace(visitor),
            Symbol::Uninterned(string) => string.trace(visitor),
            Symbol::Generated(symbol, env) => {
                env.trace(visitor);
                symbol.trace(visitor);
            }
        }
    }
}

impl Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const Symbol).hash(state);
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Eq for Symbol {}

pub struct Uninit(Handle<Symbol>);

impl Uninit {
    pub fn sym(&self) -> Handle<Symbol> {
        self.0
    }
}

impl Object for Uninit {
    fn trace(&self, visitor: &mut dyn Visitor) {
        self.0.trace(visitor);
    }
}

impl Allocation for Uninit {}

pub struct SymbolTable {
    table: DashMap<&'static str, Handle<Symbol>>,
}

impl SymbolTable {
    pub(crate) fn new() -> Self {
        Self {
            table: DashMap::with_capacity(256),
        }
    }

    pub fn intern(&self, name: impl AsRef<str>) -> Handle<Symbol> {
        let name = name.as_ref();
        if let Some(sym) = self.table.get(name) {
            return *sym.value();
        }

        let thread = Thread::current();
        let name = Str::new(thread, name);
        let sym = thread.allocate(Symbol::Interned(name));

        self.table
            .insert(unsafe { std::mem::transmute(&**name) }, sym);
        sym
    }
}

impl Object for SymbolTable {
    fn trace(&self, visitor: &mut dyn Visitor) {
        for val in self.table.iter() {
            val.value().trace(visitor);
        }
    }
}

pub fn gensym(prefix: Option<impl AsRef<str>>) -> Handle<Symbol> {
    static GENSYM_COUNT: AtomicUsize = AtomicUsize::new(0);

    let prefix = prefix.as_ref().map(|x| x.as_ref()).unwrap_or("G");

    let name = format!(
        "{}{}",
        prefix,
        GENSYM_COUNT.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    );

    let sym = Symbol::Uninterned(Str::new(Thread::current(), name));
    Thread::current().allocate(sym)
}
