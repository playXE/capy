use crate::gc_protect;
use crate::vm::thread::Thread;

use super::object::*;
use super::{hashtable::*, value::*};

#[repr(C)]
pub struct ScmModule {
    pub(crate) header: ScmCellHeader,
    pub(crate) obarray: Value,
    pub(crate) uses: Value,
    pub(crate) kind: ModuleKind,
    pub(crate) public_interface: Value,
    pub(crate) module_uses: Value,
}

impl Value {
    pub fn is_module(&self) -> bool {
        self.type_of() == TypeId::Module
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ModuleKind {
    /// Module definition itself
    Module,
    /// Interface to module definition, holds exported symbols
    Interface,
}

pub fn scm_module_export(thread: &mut Thread, mut m: Value, names: Value) {
    let mut public_i = m.cast_as::<ScmModule>().public_interface;

    let mut names = names;
    while names.is_pair() {
        let mut name = scm_car(names);

        let mut internal_name = if name.is_pair() { scm_car(name) } else { name };
        let var = gc_protect!(thread => m, public_i, names, name, internal_name => scm_module_ensure_local_variable(thread, m, internal_name));

        let external_name = if name.is_pair() {
            scm_cdr(name)
        } else {
            internal_name
        };

        // protect only module and names, other values do not matter.
        gc_protect!(thread => m, public_i, names => scm_module_add(thread, public_i, external_name, var));

        names = scm_cdr(names);
    }
}

pub fn scm_module_ensure_local_variable(
    thread: &mut Thread,
    mut m: Value,
    mut key: Value,
) -> Value {
    let gloc = get_hashtable::<true>(m.cast_as::<ScmModule>().obarray, key);

    let gloc = if gloc.is_none() {
        let mut gloc = gc_protect!(thread => m, key => {
            thread.make_gloc(Value::encode_undefined_value(), Value::encode_undefined_value())
        });

        gc_protect!(thread => gloc => scm_module_add(thread, m, key, gloc));

        gloc
    } else {
        gloc.unwrap()
    };

    gloc
}

pub fn scm_module_add(thread: &mut Thread, m: Value, key: Value, var: Value) {
    debug_assert!(var.is_gloc());

    let nsize = put_hashtable::<true>(m.cast_as::<ScmModule>().obarray, key, var);

    if nsize != 0 {
        rehash_hashtable(thread, m.cast_as::<ScmModule>().obarray, nsize);
    }
}

pub fn scm_module_local_variable(m: Value, key: Value) -> Option<Value> {
    get_hashtable::<true>(m.cast_as::<ScmModule>().obarray, key)
}
