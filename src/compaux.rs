use rsgc::prelude::Handle;

use crate::object::{Identifier, Type, Symbol};

pub fn scm_outermost_identifier(mut id: Handle<Identifier>) -> Handle<Identifier> {
    while id.name.is_xtype(Type::Identifier) {
        id = id.name.identifier();
    }

    id 
}

pub fn scm_unwrap_identifier(id: Handle<Identifier>) -> Handle<Symbol> {
    scm_outermost_identifier(id).name.symbol()
}