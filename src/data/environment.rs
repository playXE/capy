use rsgc::system::{object::Handle, string::Str, traits::Object};

use crate::prelude::Value;

pub struct Environment {
    kind: EnvKind
}

impl Object for Environment {
    fn trace(&self, visitor: &mut dyn rsgc::system::traits::Visitor) {
        match self.kind {
            EnvKind::Library(value) => value.trace(visitor),
            EnvKind::Program(string) => string.trace(visitor),
            EnvKind::Repl => (),
            EnvKind::Custom => ()
        }
    }
}


#[allow(dead_code)]
enum EnvKind {
    Library(Value),
    Program(Handle<Str>),
    Repl,
    Custom
}