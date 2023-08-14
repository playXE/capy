use std::io::Cursor;

use crate::{
    compiler::{Denotation, SyntaxEnv, P},
    gc_frame, gc_protect,
    runtime::hashtable::rehash_hashtable,
    vm::{scm_virtual_machine, sync::mutex::Mutex, thread::Thread},
};

use super::{
    hashtable::{get_hashtable, hashtable_lock, hashtable_unlock, put_hashtable},
    object::*,
    value::Value,
};

pub struct Environment {
    pub(crate) buckets: Vec<Value>,
    pub(crate) timestamp: u64,
}

fn hash_key(val: Value) -> u32 {
    murmur3::murmur3_32(&mut Cursor::new(val.get_raw().to_le_bytes()), 5731).unwrap()
        % i32::MAX as u32
}

impl Environment {
    fn rehash_buckets(&mut self, thread: &mut Thread) {
        let mut new_buckets =
            vec![Value::encode_null_value(); (self.buckets.capacity() as f64 * 1.5) as usize];

        for i in 0..self.buckets.len() {
            let mut bucket = self.buckets[i];

            while bucket.is_pair() {
                let kv = scm_car(bucket);
                let key = scm_car(kv);

                let hash = hash_key(key);
                let j = hash as usize % new_buckets.len();

                let next = scm_cdr(bucket);
                scm_set_cdr(bucket, thread, new_buckets[j]);
                new_buckets[j] = bucket;

                bucket = next;
            }
        }
    }

    fn maybe_rehash(&mut self) {
        if self.timestamp != scm_virtual_machine().gc_counter {
            self.timestamp = scm_virtual_machine().gc_counter;
            self.rehash_buckets(&mut Thread::current());
        }
    }

    pub fn new() -> Self {
        Self {
            buckets: vec![Value::encode_null_value(); 128],
            timestamp: 0,
        }
    }

    pub fn get(&mut self, key: Value) -> Option<Value> {
        self.maybe_rehash();

        let hash = hash_key(key);
        let mut bucket = self.buckets[hash as usize % self.buckets.len()];

        while bucket.is_pair() {
            let kv = scm_car(bucket);
            let k = scm_car(kv);

            if k == key {
                return Some(scm_cdr(kv));
            }

            bucket = scm_cdr(bucket);
        }

        None
    }

    pub fn set(&mut self, thread: &mut Thread, key: Value, value: Value) {
        self.maybe_rehash();

        let hash = hash_key(key);
        let mut bucket = self.buckets[hash as usize % self.buckets.len()];

        while bucket.is_pair() {
            let kv = scm_car(bucket);
            let k = scm_car(kv);

            if k == key {
                scm_set_cdr(kv, thread, value);
                return;
            }

            bucket = scm_cdr(bucket);
        }

        gc_frame!(thread.stackchain() => key = key, value = value);
        let kv = thread.make_cons::<false>(Value::encode_null_value(), Value::encode_null_value());
        scm_set_car(kv, thread, *key);
        scm_set_cdr(kv, thread, *value);

        gc_frame!(thread.stackchain() => kv = kv);
        let bucket =
            thread.make_cons::<false>(Value::encode_null_value(), Value::encode_null_value());
        scm_set_car(bucket, thread, *kv);
        scm_set_cdr(
            bucket,
            thread,
            self.buckets[hash as usize % self.buckets.len()],
        );
        let pos = hash as usize % self.buckets.len();
        self.buckets[pos] = bucket;
    }

    pub fn get_cell(&mut self, thread: &mut Thread, key: Value) -> Value {
        if let Some(val) = self.get(key) {
            val
        } else {
            gc_frame!(thread.stackchain() => key = key);
            let gloc =
                thread.make_gloc(Value::encode_null_value(), Value::encode_undefined_value());
            let key = *key;
            gloc.cast_as::<ScmGloc>().name = key;
            self.set(thread, key, gloc);
            gloc
        }
    }
}

pub fn scm_define(name: Value, value: Value) {
    gc_frame!(Thread::current().stackchain() => value = value);
    let cell = scm_virtual_machine()
        .toplevel_environment
        .lock(true)
        .get_cell(Thread::current(), name);
    scm_gloc_set(cell, Thread::current(), *value);
}

#[repr(C)]
pub struct ScmEnvironment {
    pub(crate) header: ScmCellHeader,
    syntactic_environment: Mutex<P<SyntaxEnv>>,
    name: Value,
    mutable: bool,
    ht: Value,
}

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
        debug_assert!(self.is_environment());
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