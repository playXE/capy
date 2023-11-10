use std::collections::HashMap;

use crate::{rc::Rc, tree_il::{Variable, TreeNode}, syntaxrules::SyntaxRules, sexpr::{Symbol, Sexpr, ScmError}, expander::Cenv};

#[derive(Debug)]
pub enum Environment {
    Expired,
    Lexical(Lexical),
    Global(Global),
}

impl Environment {
    pub fn lookup<const GLOBAL: bool>(&self, sym: &Rc<Symbol>) -> Definition {
        let mut env = self;

        while let Environment::Lexical(lexical) = env {
            if let Some(binding) = lexical.bindings.get(sym) {
                return binding.clone();
            }

            env = &lexical.parent;
        }

        // If `sym` wasn't found, look into the lexical environment
        if let Some((lexical_sym, lexical_env)) = sym.lexical() {
            // If the lexical environment is a global environment, return that a global lookup is needed
            if let Environment::Global(global) = &*lexical_env {
                return global.bindings.get(&lexical_sym).cloned().unwrap_or(Definition::Undefined);
            }  

            // Otherwise, look into the lexical environment
            lexical_env.lookup::<GLOBAL>(&lexical_sym)
        } else {
            if let Environment::Global(global) = env {
                if GLOBAL {
                    return global.bindings.get(sym).cloned().unwrap_or(Definition::Undefined);
                }
            }
            Definition::Undefined
        }
    }
    
}

#[derive(Debug)]
pub struct Lexical {
    pub parent: Rc<Environment>,
    pub bindings: HashMap<Rc<Symbol>, Definition>,
}

#[derive(Debug)]
pub struct Global {
    pub bindings: HashMap<Rc<Symbol>, Definition>,

}

pub type Special = fn(&Sexpr, &Cenv) -> Result<Rc<TreeNode>, ScmError>;

#[derive(Clone, Debug)]
pub enum Definition {
    Variable(Rc<Variable>),
    Macro(Rc<SyntaxRules>),
    /// Defines a primitive reference.
    ///  
    /// Only found in `Global` environments, used to emit 
    /// optimized code for primitive applications.
    Primitive(Rc<Symbol>),
    /// Expander for `lambda`, `define` etc that produces TreeIL form.
    Special(Rc<Special>),
    /// Used when expanding internal definitions.
    Rec(Sexpr),
    Undefined,
}

impl Definition {
    pub fn special(&self) -> Rc<Special> {
        match self {
            Definition::Special(special) => special.clone(),
            Definition::Undefined => panic!("undef"),
            _ => panic!("Expected special form"),
        }
    }
}