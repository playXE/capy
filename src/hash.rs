//! # R6RS Hash Table
//!
//! This module implements hash table.

use crate::{
    bool::EQV_PROC,
    list::ASSV_PROC,
    value::{Hdr, Type, Value},
    vm::{intern, Trampoline, Vm},
};
use murmur3::murmur3_32;
use once_cell::sync::Lazy;
use rsgc::prelude::{Allocation, Object};
use std::io::Cursor;

/*
pub enum Hashing {
    Eq,
    Eqv,
    Equal,
    Custom(
        fn(&mut Vm, Value) -> Result<u32, Value>,
        fn(&mut Vm, Value, Value) -> Result<bool, Value>,
    ),
}

const SEED: u32 = 5931;

pub fn eq_hash(vm: &mut Vm, value: Value) -> Result<u32, Value> {
    let hash = murmur3_32(&mut Cursor::new(value.raw().to_le_bytes()), SEED).unwrap();
    Ok(hash)
}

pub fn eqv_hash(vm: &mut Vm, value: Value) -> Result<u32, Value> {
    if value.doublep() {
        let k = murmur3_32(&mut Cursor::new([0]), SEED).unwrap();
        let hash = murmur3_32(&mut Cursor::new(value.double().to_bits().to_le_bytes()), k).unwrap();
        return Ok(hash);
    } else if value.charp() {
        let k = murmur3_32(&mut Cursor::new([1]), SEED).unwrap();
        let hash = murmur3_32(&mut Cursor::new((value.char() as u32).to_le_bytes()), k).unwrap();
        return Ok(hash);
    } else {
        let k = murmur3_32(&mut Cursor::new([2]), SEED).unwrap();
        let hash = murmur3_32(&mut Cursor::new(value.raw().to_le_bytes()), k).unwrap();
        return Ok(hash);
    }
}

pub fn equal_hash(vm: &mut Vm, value: Value, seed: u32) -> Result<u32, Value> {
    if value.doublep() {
        let k = murmur3_32(&mut Cursor::new([0]), seed).unwrap();
        let hash = murmur3_32(&mut Cursor::new(value.double().to_bits().to_le_bytes()), k).unwrap();
        return Ok(hash);
    } else if value.charp() {
        let k = murmur3_32(&mut Cursor::new([1]), seed).unwrap();
        let hash = murmur3_32(&mut Cursor::new((value.char() as u32).to_le_bytes()), k).unwrap();
        return Ok(hash);
    } else if value.strp() {
        let k = murmur3_32(&mut Cursor::new([2]), seed).unwrap();
        let k = murmur3_32(&mut Cursor::new(value.str().len().to_le_bytes()), k).unwrap();
        let hash = murmur3_32(&mut Cursor::new(value.str().as_bytes()), k).unwrap();
        return Ok(hash);
    } else if value.byte_vectorp() {
        let k = murmur3_32(&mut Cursor::new([3]), seed).unwrap();
        let k = murmur3_32(
            &mut Cursor::new(value.byte_vector_as_slice().len().to_le_bytes()),
            k,
        )
        .unwrap();
        let hash = murmur3_32(&mut Cursor::new(value.byte_vector_as_slice()), k).unwrap();
        return Ok(hash);
    } else if value.vectorp() {
        let k = murmur3_32(&mut Cursor::new([4]), seed).unwrap();
        let k = murmur3_32(
            &mut Cursor::new(value.vector_as_slice().len().to_le_bytes()),
            k,
        )
        .unwrap();
        let mut hash = k;
        for i in 0..value.vector_as_slice().len() {
            let k = murmur3_32(&mut Cursor::new([i as u8]), seed).unwrap();
            let h = equal_hash(vm, value.vector_as_slice()[i], k)?;
            hash = murmur3_32(&mut Cursor::new(h.to_le_bytes()), hash).unwrap();
        }
        return Ok(hash);
    } else if value.pairp() {
        let k = murmur3_32(&mut Cursor::new([5]), seed).unwrap();
        let mut hash = k;
        let mut pair = value;
        while pair.pairp() {
            let k = murmur3_32(&mut Cursor::new([0]), seed).unwrap();
            let h = equal_hash(vm, pair.car(), k)?;
            hash = murmur3_32(&mut Cursor::new(h.to_le_bytes()), hash).unwrap();
            pair = pair.cdr();
        }
        if !pair.nullp() {
            let k = murmur3_32(&mut Cursor::new([1]), seed).unwrap();
            let h = equal_hash(vm, pair, k)?;
            hash = murmur3_32(&mut Cursor::new(h.to_le_bytes()), hash).unwrap();
        }
        return Ok(hash);
    } else {
        let k = murmur3_32(&mut Cursor::new([6]), seed).unwrap();
        let hash = murmur3_32(&mut Cursor::new(value.raw().to_le_bytes()), k).unwrap();
        return Ok(hash);
    }
}

#[repr(C)]
pub struct HashTable {
    pub(crate) hdr: Hdr,
    pub(crate) size: u32,
    pub(crate) mod_count: u32,
    pub(crate) threshold: u32,
    pub(crate) load_factor: f32,
    pub(crate) func: Hashing,
    pub(crate) table: Option<Handle<Array<Option<Handle<Node>>>>>,
}

impl Hashing {
    pub fn hash_value(&self, vm: &mut Vm, value: Value) -> Result<u32, Value> {
        match self {
            Hashing::Eq => eq_hash(vm, value),
            Hashing::Eqv => eqv_hash(vm, value),
            Hashing::Equal => equal_hash(vm, value, SEED),
            Hashing::Custom(hash, _) => hash(vm, value),
        }
    }

    pub fn eq_value(&self, vm: &mut Vm, a: Value, b: Value) -> Result<bool, Value> {
        match self {
            Hashing::Eq => Ok(a == b),
            Hashing::Eqv => Ok(eqv(a, b)),
            Hashing::Equal => Ok(equal(a, b)),
            Hashing::Custom(_, eq) => eq(vm, a, b),
        }
    }
}

impl Object for HashTable {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.table.trace(visitor);
    }
}

impl Allocation for HashTable {}

pub(crate) struct Node {
    pub(crate) key: Value,
    pub(crate) value: Value,
    pub(crate) hash: u32,
    pub(crate) next: Option<Handle<Self>>,
}

impl Object for Node {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.key.trace(visitor);
        self.value.trace(visitor);
        self.next.trace(visitor);
    }
}

impl Allocation for Node {}

impl HashTable {
    pub const DEFAULT_INITIAL_CAPACITY: u32 = 1 << 4;
    pub const MAXIMUM_CAPACITY: u32 = 1 << 30;
    pub const DEFAULT_LOAD_FACTOR: f32 = 0.75;

    pub const fn table_size_for(cap: usize) -> usize {
        let n = (-1isize) as usize >> (cap - 1).leading_zeros();

        if n >= Self::MAXIMUM_CAPACITY as usize {
            Self::MAXIMUM_CAPACITY as _
        } else {
            n as usize + 1
        }
    }

    pub fn capacity_from_threshold(&self) -> u32 {
        if self.threshold == 0 {
            0
        } else {
            (self.threshold as usize * 2).next_power_of_two() as u32
        }
    }

    pub fn with_capacity_and_load_factor(
        func: Hashing,
        mut initial_capacity: u32,
        load_factor: f32,
    ) -> Handle<Self> {
        if initial_capacity > Self::MAXIMUM_CAPACITY {
            initial_capacity = Self::MAXIMUM_CAPACITY;
        }
        if load_factor <= 0.0 || load_factor.is_nan() {
            panic!("Illegal load factor");
        }

        Thread::current().allocate(Self {
            hdr: Hdr::new(Type::HashTable),
            size: 0,
            mod_count: 0,
            threshold: Self::table_size_for(initial_capacity as _) as _,
            load_factor,
            func,
            table: None,
        })
    }

    pub fn with_capacity(func: Hashing, initial_capacity: u32) -> Handle<Self> {
        Self::with_capacity_and_load_factor(func, initial_capacity, Self::DEFAULT_LOAD_FACTOR)
    }

    pub fn new(func: Hashing) -> Handle<Self> {
        Self::with_capacity(func, Self::DEFAULT_INITIAL_CAPACITY)
    }

    pub fn get(&self, vm: &mut Vm, key: Value) -> Result<Option<Value>, Value> {
        self.get_node(vm, key).map(|node| node.map(|node| node.value))
    }

    fn get_node(&self, vm: &mut Vm, key: Value) -> Result<Option<&Node>, Value>
    {
        let hash = self.func.hash_value(vm, key)?;

        if let Some(tab) = self.table.as_ref() {
            let n = tab.len();
            if n == 0 {
                return Ok(None);
            }
            let first = &tab[(n - 1) & hash as usize];
            if let Some(first) = first {
                if self.func.eq_value(vm, first.key, key)? && first.hash == hash {
                    return Ok(Some(first));
                }

                let mut e = first.next.as_ref();
                while let Some(node) = e {
                    if self.func.eq_value(vm, node.key, key)? && node.hash == hash {
                        return Ok(Some(node));
                    }
                    e = node.next.as_ref();
                }
            } else {
                return Ok(None);
            }
        }

        Ok(None)
    }

    fn resize(self: &mut Handle<Self>) -> Option<Handle<Array<Option<Handle<Node>>>>> {
        let thread = Thread::current();

        let old_tab = self.table;
        let old_cap = old_tab.as_ref().map(|tab| tab.len()).unwrap_or(0) as u32;
        let mut new_cap = 0;
        let mut new_thr = 0;
        let old_thr = self.threshold;

        if old_cap > 0 {
            if old_cap >= Self::MAXIMUM_CAPACITY {
                self.threshold = std::u32::MAX;
                return old_tab;
            } else if ((old_cap << 1) as u32) < Self::MAXIMUM_CAPACITY
                && old_cap >= Self::DEFAULT_INITIAL_CAPACITY
            {
                new_cap = old_cap << 1;
                new_thr = old_thr << 1;
            }
        } else if old_thr > 0 {
            new_cap = old_thr;
        } else {
            new_cap = Self::DEFAULT_INITIAL_CAPACITY;
            new_thr =
                (Self::DEFAULT_LOAD_FACTOR as f32 * Self::DEFAULT_INITIAL_CAPACITY as f32) as u32;
        }

        if new_thr == 0 {
            let ft = new_cap as f32 * self.load_factor;
            new_thr = if ft < Self::MAXIMUM_CAPACITY as f32 && new_cap < Self::MAXIMUM_CAPACITY {
                ft.round() as u32
            } else {
                std::u32::MAX
            };
        }

        self.threshold = new_thr;
        let mut newtab = Array::new(thread, new_cap as _, |_, _| None);
        thread.write_barrier(*self);
        self.table = Some(newtab);

        if let Some(mut old_tab) = old_tab {
            for j in 0..old_cap {
                let mut e = old_tab[j as usize];
                old_tab[j as usize] = None;
                while let Some(mut n) = e {
                    e = n.next;
                    let i = (n.hash % (new_cap as u32)) as usize;
                    thread.write_barrier(newtab);
                    n.next = newtab[i];
                    newtab[i] = Some(n);
                }
            }
        }

        Some(newtab)
    }

    pub fn clear(self: &mut Handle<Self>) {
        let thread = Thread::current();

        if let Some(mut tab) = self.table {
            thread.write_barrier(tab);
            for i in 0..tab.len() {
                tab[i] = None;
            }
            self.size = 0;
            self.mod_count += 1;
        }
    }

    pub fn delete(
        self: &mut Handle<Self>,
        vm: &mut Vm,
        key: Value,
    ) -> Result<Option<Value>, Value> {
        let hash = self.func.hash_value(vm, key)?;
        let node = self.remove_node(vm, key, hash)?;
        Ok(node.map(|n| n.value))
    }

    fn remove_node(
        self: &mut Handle<Self>,
        vm: &mut Vm,
        key: Value,
        hash: u32,
    ) -> Result<Option<Handle<Node>>, Value> {
        let thread = Thread::current();
        if let Some(mut tab) = self.table {
            let n = tab.len();
            let i = ((n as u32 - 1) & hash) as usize;

            let p = tab[i as usize];

            if let Some(mut p) = p {
                let mut node = None;
                if p.hash == hash && self.func.eq_value(vm, p.key, key)? {
                    node = Some(p);
                } else if p.next.is_some() {
                    let mut e = p.next;
                    while let Some(n) = e {
                        if n.hash == hash && self.func.eq_value(vm, n.key, key)? {
                            node = Some(n);
                            break;
                        }
                        e = n.next;
                    }
                }

                if let Some(node) = node {
                    if Handle::ptr_eq(&node, &p) {
                        thread.write_barrier(tab);
                        tab[i] = node.next;
                    } else {
                        thread.write_barrier(p);
                        p.next = node.next;
                    }
                    self.mod_count += 1;
                    self.size -= 1;
                    return Ok(Some(node));
                }
            }
        }
        Ok(None)
    }

    pub fn update(
        self: &mut Handle<Self>,
        vm: &mut Vm,
        key: Value,
        value: Value,
    ) -> Result<Option<Value>, Value> {
        let hash = self.func.hash_value(vm, key)?;
        self.put_val::<false>(vm, hash, key, value)
    }

    pub fn put(self: &mut Handle<Self>, vm: &mut Vm, key: Value, value: Value) -> Result<Option<Value>, Value> {
        let hash = self.func.hash_value(vm, key)?;
        self.put_val::<true>(vm, hash, key, value)
    }

    fn put_val<'a, const INSERT: bool>(
        self: &'a mut Handle<Self>,
        vm: &mut Vm,
        hash: u32,
        key: Value,
        value: Value,
    ) -> Result<Option<Value>, Value> {
        let mut tab = match self.table {
            Some(tab) => tab,
            None => self.resize().unwrap(),
        };

        let n = tab.len();

        let i = ((n as u32 - 1) & hash) as usize;

        let mut node = tab[i];
        while let Some(mut n) = node {
            if n.hash == hash && self.func.eq_value(vm, n.key, key)? {
                vm.mutator().write_barrier(n);
                return Ok(Some(std::mem::replace(&mut n.value, value)));
            }
            node = n.next
        }
        if INSERT {
            let node = vm.mutator().allocate(Node {
                hash,
                key,
                value,
                next: tab[i],
            });

            tab[i] = Some(node);

            self.mod_count += 1;
            self.size += 1;
            if self.size > self.threshold {
                self.resize();
            }
        }

        Ok(None)
    }

    pub fn contains(self: &Handle<Self>, vm: &mut Vm, key: Value) -> Result<bool, Value> {
        Ok(self.get_node(vm, key)?.is_some())
    }
}
*/

#[repr(C)]
pub struct HashTable {
    pub(crate) hdr: Hdr,
    pub(crate) count: u32,
    pub(crate) hasher: Value,
    pub(crate) equiv: Value,
    pub(crate) searcher: Value,
    pub(crate) htype: Value,
    pub(crate) buckets: Value,
    pub(crate) mutable: bool,
}

const SEED: u32 = 0xdeadbeef;

pub fn eq_hash(x: Value) -> u32 {
    murmur3_32(&mut Cursor::new(x.raw().to_le_bytes()), SEED).unwrap()
}

pub fn hash_on_equal(x: Value, budget: isize, seed: u32) -> u32 {
    if budget > 0 {
        if x.strp() {
            let len = x.str_len();
            let seed = murmur3_32(&mut Cursor::new(len.to_le_bytes()), seed).unwrap();
            return murmur3_32(&mut Cursor::new(x.str()), seed).unwrap();
        } else if x.pairp() {
            let budget = budget / 2;

            let car = hash_on_equal(x.car(), budget, seed);
            let cdr = hash_on_equal(x.cdr(), budget, car);
            cdr
        } else if x.vectorp() {
            let n = x.vector_len();
            let budget = budget / 4;

            if n > 0 {
                let mut hash = seed;
                for i in 0..n {
                    hash = hash_on_equal(x.vector_ref(i), budget / 2, hash);
                }
                hash
            } else {
                murmur3_32(&mut Cursor::new((4003330u32).to_le_bytes()), seed).unwrap()
            }
        } else {
            object_hash(x, seed)
        }
    } else {
        murmur3_32(&mut Cursor::new((2321004u32).to_le_bytes()), seed).unwrap()
    }
}

pub fn object_hash(x: Value, seed: u32) -> u32 {
    if x.symbolp() {
        return murmur3_32(&mut Cursor::new(x.symbol_str()), seed).unwrap();
    } else if x.numberp() {
        return if x.doublep() {
            murmur3_32(&mut Cursor::new(x.double().to_le_bytes()), seed).unwrap()
        } else {
            murmur3_32(&mut Cursor::new(x.raw().to_le_bytes()), seed).unwrap()
        };
    } else if x.charp() {
        return murmur3_32(&mut Cursor::new((x.char() as u32).to_le_bytes()), seed).unwrap();
    } else if x.strp() {
        let len = x.str_len();
        let hash = murmur3_32(&mut Cursor::new(len.to_le_bytes()), seed).unwrap();
        return murmur3_32(&mut Cursor::new((5022200u32).to_le_bytes()), hash).unwrap();
    } else if x.vectorp() {
        let len = x.vector_len();
        let hash = murmur3_32(&mut Cursor::new(len.to_le_bytes()), seed).unwrap();
        return murmur3_32(&mut Cursor::new((4003330u32).to_le_bytes()), hash).unwrap();
    } else if x.pairp() {
        murmur3_32(&mut Cursor::new((2555000u32).to_le_bytes()), seed).unwrap()
    } else {
        murmur3_32(&mut Cursor::new(x.raw().to_le_bytes()), seed).unwrap()
    }
}

pub fn eqv_hash(x: Value) -> u32 {
    if x.numberp() {
        object_hash(x, 5231)
    } else if x.charp() {
        object_hash(x, 5231)
    } else if x.symbolp() {
        object_hash(x, 5231)
    } else {
        eq_hash(x)
    }
}

pub fn equal_hash(x: Value) -> u32 {
    hash_on_equal(x, 32, 5231)
}

impl HashTable {
    /// Copies all entires in the src vector to the dst vector.
    /// rehashing each key using the hash function `hf`.
    fn rehash_buckets(vm: &mut Vm, src: Value, dst: Value, hf: Value) -> Result<(), Value> {
        let m = src.vector_len();
        let n = dst.vector_len();

        for i in 0..m {
            let mut bucket = src.vector_ref(i);

            while !bucket.nullp() {
                let entry = bucket.car();
                let key = entry.car();
                let h = vm.apply(hf, &[key])?.int() as u32;
                let j = h % (n as u32);
                vm.mutator().write_barrier(dst.handle());
                dst.vector_set(
                    j as _,
                    Value::cons(vm.mutator(), entry, dst.vector_ref(j as _)),
                );
                bucket = bucket.cdr();
            }
        }

        Ok(())
    }

    pub fn resize(ht: Value, vm: &mut Vm) -> Result<(), Value> {
        assert!(ht.get_type() == Type::HashTable);

        let lock = ht.lock();
        let mut ht = ht.handle().downcast::<Self>().unwrap();
        let n = 20 + 2 * ht.count;

        if ht.htype == *HTYPE_USUAL {
            let v = Value::make_vector(vm.mutator(), n, Value::null());
            Self::rehash_buckets(vm, ht.buckets, v, ht.hasher)?;
            vm.mutator().write_barrier(ht);
            ht.buckets = v;
            drop(lock);
        } else if ht.htype == *HTYPE_EQ || ht.htype == *HTYPE_EQV {
            todo!();
        } else {
            unreachable!();
        }
        Ok(())
    }

    pub fn fetch(ht: Value, vm: &mut Vm, key: Value, flag: Value) -> Result<Value, Value> {
        assert!(ht.get_type() == Type::HashTable);

        let ht = ht.handle().downcast::<Self>().unwrap();

        let n = ht.buckets.vector_len() as u32;
        let h = vm.apply(ht.hasher, &[key])?.int() as u32;
        let i = (h % n) as u32;
        let b = ht.buckets.vector_ref(i as _);

        let entry = vm.apply(ht.searcher, &[key, b])?;
        Ok(if entry.pairp() { entry.car() } else { flag })
    }

    pub fn put(ht: Value, vm: &mut Vm, key: Value, val: Value) -> Result<(), Value> {
        assert!(ht.get_type() == Type::HashTable);
        let lock = ht.lock();
        let oht = ht;
        let mut ht = ht.handle().downcast::<Self>().unwrap();

        let n = ht.buckets.vector_len() as u32;
        let h = vm.apply(ht.hasher, &[key])?.int() as u32;
        let i = (h % n) as u32;
        let b = ht.buckets.vector_ref(i as _);

        let entry = vm.apply(ht.searcher, &[key, b])?;

        if entry.pairp() {
            vm.mutator().write_barrier(entry.handle());
            entry.set_pair_cdr(val);
            drop(lock);
            Ok(())
        } else {
            vm.mutator().write_barrier(ht.buckets.handle());
            let newb = Value::cons(vm.mutator(), key, val);
            ht.buckets
                .vector_set(i as _, Value::cons(vm.mutator(), newb, b));
            ht.count += 1;
            drop(lock);
            Self::maybe_resize(oht, vm)
        }
    }

    pub fn maybe_resize(ht: Value, vm: &mut Vm) -> Result<(), Value> {
        assert!(ht.get_type() == Type::HashTable);
        let oht = ht;
        let ht = ht.handle().downcast::<Self>().unwrap();

        let k = ht.count;
        let n = ht.buckets.vector_len() as u32;

        if n < k || 3 * (20 * k) < n {
            Self::resize(oht, vm)
        } else {
            Ok(())
        }
    }

    pub fn contains(ht: Value, vm: &mut Vm, key: Value) -> Result<bool, Value> {
        assert!(ht.get_type() == Type::HashTable);
        let ht = ht.handle().downcast::<Self>().unwrap();

        let n = ht.buckets.vector_len() as u32;
        let h = vm.apply(ht.hasher, &[key])?.int() as u32;
        let i = (h % n) as u32;
        let b = ht.buckets.vector_ref(i as _);

        let entry = vm.apply(ht.searcher, &[key, b])?;
        Ok(entry.pairp())
    }

    pub fn remove(ht: Value, vm: &mut Vm, key: Value) -> Result<(), Value> {
        assert!(ht.get_type() == Type::HashTable);
        let lock = ht.lock();
        let oht = ht;
        let mut ht = ht.handle().downcast::<Self>().unwrap();

        let n = ht.buckets.vector_len() as u32;
        let h = vm.apply(ht.hasher, &[key])?.int() as u32;
        let i = (h % n) as u32;
        let b = ht.buckets.vector_ref(i as _);

        let probe = vm.apply(ht.searcher, &[key, b])?;

        if probe.pairp() {
            let mut prev = Value::null();
            let mut bucket = b;

            while !bucket.nullp() {
                let entry = bucket.car();
                if entry == probe {
                    if prev.nullp() {
                        vm.mutator().write_barrier(ht.buckets.handle());
                        ht.buckets.vector_set(i as _, bucket.cdr());
                    } else {
                        vm.mutator().write_barrier(prev.handle());
                        prev.set_pair_cdr(bucket.cdr());
                    }
                    ht.count -= 1;
                    drop(lock);
                    return Self::maybe_resize(oht, vm);
                }
                prev = bucket;
                bucket = bucket.cdr();
            }
            drop(lock);
            Ok(())
        } else {
            drop(lock);
            Ok(())
        }
    }
}

static HTYPE_USUAL: Lazy<Value> = Lazy::new(|| intern("usual"));
static HTYPE_EQ: Lazy<Value> = Lazy::new(|| intern("eq?"));
static HTYPE_EQV: Lazy<Value> = Lazy::new(|| intern("eqv?"));

pub static HASHER_EQ: Lazy<Value> = Lazy::new(|| {
    Vm::make_procedure(
        "hasher-eq?",
        |_vm, _, args| {
            let key = args[0];
            Trampoline::Return(Value::make_int(eq_hash(key) as i32))
        },
        1,
        1,
    )
});

pub static HASHER_EQV: Lazy<Value> = Lazy::new(|| {
    Vm::make_procedure(
        "hasher-eqv?",
        |_vm, _, args| {
            let key = args[0];
            Trampoline::Return(Value::make_int(eqv_hash(key) as i32))
        },
        1,
        1,
    )
});

pub static HAHSER_EQUAL: Lazy<Value> = Lazy::new(|| {
    Vm::make_procedure(
        "hasher-equal?",
        |_vm, _, args| {
            let key = args[0];
            Trampoline::Return(Value::make_int(equal_hash(key) as i32))
        },
        1,
        1,
    )
});

impl Object for HashTable {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.hasher.trace(visitor);
        self.equiv.trace(visitor);
        self.searcher.trace(visitor);
        self.htype.trace(visitor);
        self.buckets.trace(visitor);
    }
}

impl Allocation for HashTable {}

pub fn make_oldstyle_hashtable(
    vm: &mut Vm,
    hashfun: Option<Value>,
    searcher: Option<Value>,
    size: Option<u32>,
) -> Value {
    let n = size.unwrap_or(20).max(1);
    let buckets = Value::make_vector(vm.mutator(), n, Value::null());
    unsafe {
        Value::encode_ptr(
            vm.mutator()
                .allocate(HashTable {
                    hdr: Hdr::new(Type::HashTable),
                    hasher: hashfun.unwrap_or_else(|| HASHER_EQV.clone()),
                    equiv: EQV_PROC.clone(),
                    searcher: searcher.unwrap_or_else(|| ASSV_PROC.clone()),
                    count: 0,
                    htype: HTYPE_USUAL.clone(),
                    buckets,
                    mutable: true,
                })
                .as_ptr(),
        )
    }
}
