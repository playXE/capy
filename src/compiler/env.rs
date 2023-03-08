use std::collections::hash_map::RandomState;

use rsgc::{
    prelude::{Allocation, Handle, Object},
    system::collections::hashmap::{Entry, HashMap},
    thread::Thread,
};

use crate::value::{Hdr, Type, Value};

#[allow(dead_code)]
#[repr(C)]
pub struct Environment {
    pub(crate) hdr: Hdr,
    /// maps symbols to cons cells
    pub(crate) ht: Handle<HashMap<Value, Value>>,
    pub(crate) mutable: bool,
    pub(crate) name: Value,
}

impl Object for Environment {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.ht.trace(visitor);
        self.name.trace(visitor);
    }
}

impl Allocation for Environment {}

pub fn is_environment(obj: Value) -> bool {
    obj.get_type() == Type::Env
}

pub fn make_environment(name: Value) -> Value {
    let ht = HashMap::with_hasher_and_capacity(RandomState::new(), 4);

    unsafe {
        Value::encode_ptr(
            Thread::current()
                .allocate(Environment {
                    hdr: Hdr::new(Type::Env),
                    ht,
                    mutable: true,
                    name,
                })
                .as_ptr(),
        )
    }
}

impl Value {
    pub fn env(self) -> Handle<Environment> {
        cassert!(is_environment(self));
        unsafe { Handle::from_raw(self.raw() as *mut u8) }
    }

    pub fn environmentp(self) -> bool {
        is_environment(self)
    }
}

pub fn environment_name(env: Value) -> Value {
    env.env().name
}

pub fn environment_variables(env: Value) -> Value {
    let mut vars = Value::make_null();

    for (key, _) in env.env().ht.iter() {
        vars = Value::make_cons(*key, vars);
    }

    vars
}

pub fn is_environment_variable(env: Value, var: Value) -> bool {
    env.env()
        .ht
        .get(&var)
        .map(|x| x.cdr() != Value::make_void())
        .unwrap_or(false)
}

pub fn environment_get(env: Value, var: Value) -> Option<Value> {
    let probe = environment_get_cell(env, var);

    if probe.cdr().voidp() {
        None
    } else {
        Some(probe.cdr())
    }
}

pub fn environment_get_cell(env: Value, var: Value) -> Value {
    match env.env().ht.entry(var) {
        Entry::Occupied(entry) => *entry.get(),
        Entry::Vacant(entry) => {
            let cell = Value::make_cons(var, Value::make_void());
            entry.insert(cell);
            cell
        }
    }
}

pub fn environment_set(env: Value, var: Value, val: Value) -> bool {
    if !env.env().mutable {
        return false;
    }

    let cell = environment_get_cell(env, var);
    Thread::current().write_barrier(cell.handle());
    cell.set_pair_cdr(val);

    true
}

pub fn is_environment_mutable(env: Value) -> bool {
    env.env().mutable
}

pub fn environment_copy(env: Value, name: Option<Value>) -> Value {
    let name = name.unwrap_or_else(|| env.env().name);
    let new = make_environment(name);

    let variables = environment_variables(env);

    let mut vs = variables;

    while !vs.nullp() {
        let var = vs.car();
        if is_environment_variable(env, var) {
            let val = environment_get(env, var).unwrap();
            environment_set(new, var, val);
        }
        vs = vs.cdr();
    }

    new
}
