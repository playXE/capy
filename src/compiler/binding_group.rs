use rsgc::system::{object::Handle, collections::hashmap::HashMap, traits::Object};

use crate::{data::symbol::Symbol, prelude::Procedure};

use super::{Compiler, env::Env};

pub struct BindingGroup {
    owner: Handle<Compiler>,
    parent: Option<Handle<Env>>,
    bindings: HashMap<Handle<Symbol>, Definition>
}

impl Object for BindingGroup {
    fn trace(&self, visitor: &mut dyn rsgc::system::traits::Visitor) {
        self.owner.trace(visitor);
        self.parent.trace(visitor);
        self.bindings.trace(visitor);
    }
}

pub enum Definition {
    Value,
    Variable,
    MutatedVariable,
    Macro(Handle<Procedure>)
}

impl Object for Definition {
    fn trace(&self, visitor: &mut dyn rsgc::system::traits::Visitor) {
        match self {
            Definition::Value => (),
            Definition::Variable => (),
            Definition::MutatedVariable => (),
            Definition::Macro(proc) => {
                proc.trace(visitor);
            }
        }
    }
}