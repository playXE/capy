use crate::rc::*;
use crate::sexpr::{Sexpr, Symbol};
use std::{cell::RefCell, collections::HashMap};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLocation {
    pub line: u32,
    pub column: u32,
    pub filename: Rc<String>,
}

/// Mapping from S-expressions to source locations
#[derive(Debug, Clone)]
pub struct SourceProvider {
    /// HashMap from S-expressions to source locations, inside RefCell
    /// because we need to mutate it when macro-expanding
    pub sources: Rc<RefCell<HashMap<Sexpr, SourceLocation>>>,
}

impl SourceProvider {
    pub fn new() -> Self {
        Self {
            sources: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn from_sources(sources: HashMap<Sexpr, SourceLocation>) -> Self {
        Self {
            sources: Rc::new(RefCell::new(sources)),
        }
    }

    /// Gets the source location of an S-expression.
    pub fn get(&self, sexpr: &Sexpr) -> Option<SourceLocation> {
        self.sources.borrow().get(sexpr).cloned()
    }

    /// Marks an S-expression with a source location.
    ///
    /// Used when macro-expanding, macro-expansion produces new S-expressions
    /// which are not present in the source code, so we need to mark them with
    /// the source location if the original S-expression has one.
    pub fn mark(&self, sexpr: &Sexpr, location: SourceLocation) {
        self.sources.borrow_mut().insert(sexpr.clone(), location);
    }
}

#[derive(Debug)]
pub struct SymbolInterner {
    pub symbols: RefCell<HashMap<&'static str, Rc<Symbol>>>,
}

impl SymbolInterner {
    pub fn new() -> Rc<Self> {
        Rc::new(Self {
            symbols: RefCell::new(HashMap::new()),
        })
    }

    pub fn intern(&self, name: impl AsRef<str>) -> Rc<Symbol> {
        let symbol = name.as_ref();
        if let Some(interned) = self.symbols.borrow().get(symbol) {
            return interned.clone();
        }

        let interned = Rc::new(Symbol::Interned(symbol.to_owned()));
        let strref = unsafe { std::mem::transmute::<&str, &'static str>(&*interned) };
        self.symbols.borrow_mut().insert(strref, interned.clone());
        interned
    }
}

impl Drop for SymbolInterner {
    fn drop(&mut self) {
        for (_, symbol) in self.symbols.borrow_mut().drain() {
            if Rc::strong_count(&symbol) != 1 {
                panic!(
                    "Symbol {} has {} strong references left",
                    &**symbol,
                    Rc::strong_count(&symbol)
                );
            }
        }
    }
}
