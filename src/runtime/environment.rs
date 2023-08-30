use crate::{
    compiler::{Denotation, SyntaxEnv, P},
     gc_protect,
    runtime::hashtable::rehash_hashtable,
    vm::{scm_virtual_machine, sync::mutex::Mutex, thread::Thread}, raise_exn,
};

use super::{
    hashtable::{get_hashtable, hashtable_lock, hashtable_unlock, put_hashtable},
    object::*,
    value::Value, gsubr::{scm_make_subr, Subr, scm_define_subr}, symbol::scm_intern,
};


pub fn scm_define(name: Value, value: Value) {
    let cell = environment_get_cell(scm_virtual_machine().interaction_environment, name).unwrap();
    scm_gloc_set(cell, Thread::current(), value);
}

#[repr(C)]
pub struct ScmEnvironment {
    pub(crate) header: ScmCellHeader,
    pub(crate) syntactic_environment: Mutex<P<SyntaxEnv>>,
    
    pub(crate) name: Value,
    pub(crate) mutable: bool,
    pub(crate) ht: Value,
    pub(crate) synenv: Value,
}

#[derive(Debug, Clone, Copy)]
pub enum EnvironmentError {
    Undefined,
    DenotesMacro,
    ImmutableEnv,
}

impl Value {
    pub fn is_environment(self) -> bool {
        self.type_of() == TypeId::Environment
    }

    pub fn environment<'a>(self) -> &'a mut ScmEnvironment {
        debug_assert!(self.is_environment(), "not an environment {:?}({:?})", self, self.type_of());
        self.cast_as()
    }
}

pub fn environment_get(env: Value, name: Value) -> Result<Value, EnvironmentError> {
    if environment_is_macro(env, name) {
        return Err(EnvironmentError::DenotesMacro);
    }

    let cell = environment_get_cell(env, name)?;

    if cell.cast_as::<ScmGloc>().value.is_undefined() {
        Err(EnvironmentError::Undefined)
    } else {
        Ok(cell.cast_as::<ScmGloc>().value)
    }
}

pub fn environment_get_cell(mut env: Value, mut name: Value) -> Result<Value, EnvironmentError> {
    if environment_is_macro(env, name) {
        return Err(EnvironmentError::DenotesMacro);
    }
    let e = env.environment();
    hashtable_lock(e.ht);
    if let Some(cell) = get_hashtable::<true>(e.ht, name) {
        hashtable_unlock(e.ht);
        Ok(cell)
    } else {
        let thread = Thread::current();
        let mut cell = gc_protect!(thread => env, name => thread.make_gloc(Value::encode_undefined_value(), Value::encode_undefined_value()));
        let nsize = put_hashtable::<true>(env.environment().ht, name, cell);
        if nsize != 0 {
            gc_protect!(thread => cell, env, name => rehash_hashtable(thread, env.environment().ht, nsize));
        }
        hashtable_unlock(e.ht);
        cell.cast_as::<ScmGloc>().name.assign(cell, name);
        cell.cast_as::<ScmGloc>()
            .value
            .assign(cell, Value::encode_undefined_value());
        
        Ok(cell)
    }
}

pub fn environment_set(env: Value, name: Value, value: Value) -> Result<(), EnvironmentError> {
    if !env.environment().mutable {
        return Err(EnvironmentError::ImmutableEnv);
    }

    if environment_is_macro(env, name) {
        env.environment().syntactic_environment.lock(true).env.remove(&name);
    }
    let cell = environment_get_cell(env, name)?;
    cell.cast_as::<ScmGloc>().value.assign(cell, value);
    Ok(())
}

pub fn environment_is_macro(env: Value, name: Value) -> bool {
    env.environment()
        .syntactic_environment
        .lock(true)
        .env
        .get(&name)
        .map_or(false, |x| match &**x {
            Denotation::Macro(_) => true,
            _ => false,
        })
}

pub fn environment_get_macro(env: Value, name: Value) -> Option<P<Denotation>> {
    env.environment()
        .syntactic_environment
        .lock(true)
        .env
        .get(&name)
        .cloned()
}

pub fn environment_define_macro(env: Value, name: Value, macro_: P<Denotation>) {
    env.environment()
        .syntactic_environment
        .lock(true)
        .env
        .insert(name, macro_);
}


pub fn environment_name(env: Value) -> Value {
    env.environment().name
}

extern "C-unwind" fn interaction_environment_subr(_thread: &mut Thread, val: &mut Value) -> Value {
    if val.is_undefined() {
       
        scm_virtual_machine().interaction_environment
    } else {
        if val.is_environment() {
            scm_virtual_machine().interaction_environment = *val;
            Value::encode_undefined_value()
        } else {
            raise_exn!(Fail, &[], "interaction-environment: expected environment, but got {}", val)
        }
    }
}

extern "C-unwind" fn environment_p(_thread: &mut Thread, val: &mut Value) -> Value {
    Value::encode_bool_value(val.is_environment())
}

extern "C-unwind" fn environment_name_subr(_thread: &mut Thread, val: &mut Value) -> Value {
    if !val.is_environment() {
        raise_exn!(Fail, &[], "environment-name: expected environment, but got {}", val);
    }
    environment_name(*val)
}

extern "C-unwind" fn environment_get_cell_proc(_: &mut Thread, env: &mut Value, key: &mut Value) -> Value {
    if !env.is_environment() {
        raise_exn!(Fail, &[], "environment-get-cell: expected environment, but got {}", env);
    }

    if !key.is_symbol() {
        raise_exn!(Fail, &[], "environment-get-cell: expected symbol, but got {}", key);
    }

    let cell = environment_get_cell(*env, *key);
    match cell {
        Ok(cell) => cell,
        Err(err) => match err {
            EnvironmentError::Undefined => raise_exn!(Fail, &[], "environment-get-cell: undefined variable {}", key),
            EnvironmentError::DenotesMacro => raise_exn!(Fail, &[], "environment-get-cell: denotes macro {}", key),
            EnvironmentError::ImmutableEnv => raise_exn!(Fail, &[], "environment-get-cell: immutable environment {}", env),
        },
    }
}

extern "C-unwind" fn variable_ref_p(_: &mut Thread, val: &mut Value) -> Value {
    Value::encode_bool_value(val.is_gloc())
}

extern "C-unwind" fn variable_ref_name(_: &mut Thread, val: &mut Value) -> Value {
    if !val.is_gloc() {
        raise_exn!(Fail, &[], "variable-ref-name: expected variable reference, but got {}", val);
    }
    val.cast_as::<ScmGloc>().name
}

extern "C-unwind" fn variable_ref_value(_: &mut Thread, val: &mut Value) -> Value {
    if !val.is_gloc() {
        raise_exn!(Fail, &[], "variable-ref-value: expected variable reference, but got {}", val);
    }
    val.cast_as::<ScmGloc>().value
}

extern "C-unwind" fn variable_ref_set_value(_: &mut Thread, val: &mut Value, new_value: &mut Value) -> Value {
    if !val.is_gloc() {
        raise_exn!(Fail, &[], "variable-ref-set-value: expected variable reference, but got {}", val);
    }
    val.cast_as::<ScmGloc>().value.assign(*val, *new_value);
    Value::encode_undefined_value()
}



pub(crate) fn init_env() {
    let vm = scm_virtual_machine();
    let thread = Thread::current();

    let r5rs = thread.make_environment(scm_intern("r5rs"));
    vm.interaction_environment = r5rs;

    let subr = scm_make_subr("interaction-environment", 0, 1, 0, Subr::F1(interaction_environment_subr));
    scm_define(scm_intern("interaction-environment"), subr);
    let subr = scm_make_subr("environment?", 1, 0, 0, Subr::F1(environment_p));
    scm_define(scm_intern("environment?"), subr);
    let subr = scm_make_subr("environment-name", 1, 0, 0, Subr::F1(environment_name_subr));
    scm_define(scm_intern("environment-name"), subr);
    let subr = scm_make_subr("environment-get-cell", 2, 0, 0, Subr::F2(environment_get_cell_proc));
    scm_define(scm_intern("environment-get-cell"), subr);
    scm_define_subr("variable-ref?", 1, 0, 0, Subr::F1(variable_ref_p));
    scm_define_subr("variable-ref-name", 1, 0, 0, Subr::F1(variable_ref_name));
    scm_define_subr("variable-ref-value", 1, 0, 0, Subr::F1(variable_ref_value));
    scm_define_subr("variable-ref-set-value!", 2, 0, 0, Subr::F2(variable_ref_set_value));

    
}