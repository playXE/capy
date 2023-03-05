//! Simple S-expression evaluator.
//! 
//! This module provides a simple evaluator for S-expressions. It is used for 
//! bootstrapping the compiler. It does not support macros or modules, its entire purpose
//! is to only bootstrap the compiler.

use rsgc::prelude::{Handle, Object, Allocation};

use crate::{hash::HashTable, module::Module, value::Value, vm::Vm, util::arraylist::ArrayList};


struct Frame {
    bindings: Handle<HashTable>,
    parent: Option<Handle<Frame>>
}

impl Object for Frame {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.bindings.trace(visitor);
        if let Some(parent) = &self.parent {
            parent.trace(visitor);
        }
    }
}

impl Allocation for Frame {}
struct Env {
    module: Handle<Module>,
    frames: Option<Handle<Frame>>
}