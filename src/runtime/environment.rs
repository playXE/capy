use std::io::Cursor;

use crate::{
    gc_frame,
    vm::{scm_virtual_machine, thread::Thread},
};

use super::{object::*, value::Value};

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
            let gloc = thread.make_gloc(Value::encode_null_value(), Value::encode_undefined_value());
            let key = *key;
            gloc.cast_as::<ScmGloc>().name = key;
            self.set(thread, key, gloc);
            gloc
        }
    }
}

pub fn scm_define(name: Value, value: Value) {
    gc_frame!(Thread::current().stackchain() => value = value);
    let cell = scm_virtual_machine().toplevel_environment.lock(true).get_cell(Thread::current(), name);
    scm_gloc_set(cell, Thread::current(), *value);

}