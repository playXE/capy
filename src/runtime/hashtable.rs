use std::mem::{size_of, transmute};

use mmtk::util::Address;

use crate::{
    vm::{sync::mutex::RawMutex, thread::Thread}, gc_protect,
};

use super::{
    equality::{equal, eqv},
    object::*,
    value::Value,
};

pub type HashProc = fn(Value, u32) -> u32;
pub type CompareProc = fn(Value, Value) -> bool;

pub const fn hash_busy_threshold(n: u32) -> u32 {
    n.wrapping_sub(n >> 3)
}

pub const fn hash_dense_threshold(n: u32) -> u32 {
    n.wrapping_add(n >> 2)
}

pub const fn hash_sparse_threshold(n: u32) -> u32 {
    n >> 2
}

pub const fn hash_mutable_size(n: u32) -> u32 {
    n + (n >> 1) + (n >> 2)
}

pub const fn hash_immutable_size(n: u32) -> u32 {
    n + (n >> 3)
}

#[repr(C)]
pub struct HashTableRec {
    pub header: ScmCellHeader,
    pub capacity: u32,
    pub used: u32,
    pub live: u32,
    pub elts: [Value; 1],
}

#[repr(C)]
pub struct ScmHashTable {
    pub(crate) hdr: ScmCellHeader,
    pub lock: RawMutex,
    pub hash: HashProc,
    pub equiv: CompareProc,
    pub datum: *mut HashTableRec,
    pub handlers: Value,
    pub typ: HashTableType,
    pub rehash: bool,
}

impl ScmHashTable {
    fn datum(&self) -> &HashTableRec {
        unsafe { &*self.datum }
    }

    fn datum_mut(&mut self) -> &mut HashTableRec {
        unsafe { &mut *self.datum }
    }

    fn elt(&self, i: u32) -> Value {
        unsafe { self.datum().elts.as_ptr().add(i as _).read() }
    }

    fn elt_mut(&mut self, i: u32) -> &mut Value {
        unsafe { &mut *self.datum_mut().elts.as_mut_ptr().add(i as _) }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum HashTableType {
    Eq = 0,
    Eqv,
    Equal,
    String,
    Generic,
}



pub const SCM_HASHTABLE_HANDLER_SIGNATURE: usize = 0;
pub const SCM_HASHTABLE_HANDLER_HASH: usize = 1;
pub const SCM_HASHTABLE_HANDLER_EQUIV: usize = 2;
pub const SCM_HASHTABLE_HANDLER_SIZE: usize = 3;
pub const SCM_HASHTABLE_HANDLER_REF: usize = 4;
pub const SCM_HASHTABLE_HANDLER_SET: usize = 5;
pub const SCM_HASHTABLE_HANDLER_DELETE: usize = 6;
pub const SCM_HASHTABLE_HANDLER_CONTAINS: usize = 7;
pub const SCM_HASHTABLE_HANDLER_COPY: usize = 8;
pub const SCM_HASHTABLE_HANDLER_CLEAR: usize = 9;
pub const SCM_HASHTABLE_HANDLER_HASH_FUNC: usize = 10;
pub const SCM_HASHTABLE_HANDLER_EQUIV_FUNC: usize = 11;
pub const SCM_HASHTABLE_HANDLER_MUTABLE: usize = 12;
pub const SCM_HASHTABLE_HANDLER_ALIST: usize = 13;


pub fn address_hash1(adrs: *const u8, bound: u32) -> u32 {
    ((((adrs as usize) >> 3) * 2654435761 + (adrs as usize & 7)) % bound as usize) as u32
}

pub fn address_hash2(adrs: *const u8, bound: u32) -> u32 {
    let hash = (((adrs as usize) >> 3) * 13845163) % bound as usize;
    hash as u32 + (hash == 0) as u32
}

pub fn u64_hash1(val: u64, bound: u32) -> u32 {
    (((val.wrapping_shr(3)).wrapping_mul(2654435761) + (val & 7)) % bound as u64) as u32
}

pub fn u64_hash2(val: u64, bound: u32) -> u32 {
    let hash = ((val >> 3).wrapping_mul(13845163)) % bound as u64;
    hash as u32 + (hash == 0) as u32
}

pub fn string_hash1(str: &[u8], bound: u32) -> u32 {
    let mut hash = 107u32;
    for b in str.iter() {
        hash = hash
            .wrapping_mul(32)
            .wrapping_sub(hash)
            .wrapping_add(*b as u32);
    }

    hash % bound
}

pub fn string_hash2(str: &[u8], bound: u32) -> u32 {
    let mut hash = 5381u32;
    for b in str.iter() {
        hash = hash
            .wrapping_mul(4)
            .wrapping_add(hash)
            .wrapping_add(*b as u32);
    }

    hash %= bound;

    hash + (hash == 0) as u32
}

pub fn eqv_hash1(val: Value, bound: u32) -> u32 {
    u64_hash1(val.get_raw() as _, bound)
}

pub fn eqv_hash2(val: Value, bound: u32) -> u32 {
    u64_hash2(val.get_raw() as _, bound)
}

pub fn eqv_hash(val: Value, bound: u32) -> u32 {
    eqv_hash1(val, bound)
}

pub fn string_hash(obj: Value, bound: u32) -> u32 {
    string_hash1(scm_string_str(obj).as_bytes(), bound)
}

pub fn symbol_hash(obj: Value, bound: u32) -> u32 {
    string_hash2(scm_symbol_str(obj).as_bytes(), bound)
}

pub fn obj_hash(obj: Value, depth: i32) -> u32 {
    if depth > 100 {
        return 1;
    }

    if obj.is_object() {
        if obj.is_pair() {
            let hash1 = obj_hash(scm_car(obj), depth + 1);
            let hash2 = obj_hash(scm_cdr(obj), depth + 1);

            let x = hash2.wrapping_mul(64);
            return hash1.wrapping_add(x).wrapping_sub(hash2);
        }

        if obj.is_vector() {
            let mut hash = 1u32;
            for i in 0..scm_vector_length(obj) {
                hash = hash
                    .wrapping_mul(32)
                    .wrapping_sub(hash)
                    .wrapping_add(obj_hash(scm_vector_ref(obj, i), depth + 1));
            }

            return hash;
        }

        if obj.is_symbol() {
            return symbol_hash(obj, i32::MAX as _);
        }

        if obj.is_string() {
            return string_hash(obj, i32::MAX as _);
        }
    }

    u64_hash1(obj.get_raw() as _, i32::MAX as _)
}

pub fn eqv_hash_equiv(a: Value, b: Value) -> bool {
    eqv(a, b)
}

pub fn equal_hash_equiv(a: Value, b: Value) -> bool {
    equal(a, b)
}

pub fn equal_hash(a: Value, bound: u32) -> u32 {
    obj_hash(a, 0) % bound
}

pub fn string_hash_equiv(a: Value, b: Value) -> bool {
    if a.is_string() {
        if b.is_string() {
            return scm_string_str(a) == scm_string_str(b);
        }
    }

    false
}

pub fn lookup_hashtable_size(n: u32) -> u32 {
    static PRIMES: &[u32] = &[
        7, 13, 29, 59, 113, 223, 431, 821, 1567, 2999, 5701, 10837, 20593, 39133, 74353, 141277,
        268439, 510047, 969097, 1841291, 3498457, 5247701, 7871573, 11807381, 17711087, 26566649,
        39849977, 59774983, 89662483, 134493731, 201740597, 302610937, 453916423, 680874641,
        1021311983, 1531968019, 2147483647,
    ];

    for i in 0..PRIMES.len() {
        if PRIMES[i] >= n {
            return PRIMES[i];
        }
    }

    panic!("lookup_hashtable_size: too large size")
}

fn put_eq_hashtable(ht: Value, key: Value, value: Value) -> u32 {
    let ht = ht.cast_as::<ScmHashTable>();
    debug_assert!(ht.lock.is_locked());
    let nsize = ht.datum().capacity;
    let hash1 = u64_hash1(key.get_raw() as _, nsize as _);
    let hash2 = u64_hash2(key.get_raw() as _, nsize as _);
    let mut index = hash1;

    let res = loop {
        let tag = ht.elt(index);

        if tag.is_empty() {
            ht.datum_mut().live += 1;
            ht.datum_mut().used += 1;
            break true;
        }

        if tag.is_undefined() {
            ht.datum_mut().live += 1;
            break true;
        }

        if tag == key {
            break true;
        }

        index += hash2;
        if index >= nsize {
            index -= nsize;
        }
        if index == hash1 {
            break false;
        }
    };

    if res {
        unsafe {
            let datum = ht.datum;
            ht.elt_mut(index).assign(transmute(datum), key);
            ht.elt_mut(index + nsize).assign(transmute(datum), value);
        }
       

      
        return lookup_hashtable_size(nsize);
    } else {
        panic!("put_eq_hashtable: table overflow: {}", key)
    }
}

fn get_eq_hashtable(ht: Value, key: Value) -> Option<Value> {
    let ht = ht.cast_as::<ScmHashTable>();
    debug_assert!(ht.lock.is_locked());
    let nsize = ht.datum().capacity;
    let hash1 = u64_hash1(key.get_raw() as _, nsize as _);
    let hash2 = u64_hash2(key.get_raw() as _, nsize as _);
    let mut index = hash1;

    loop {
        let tag = ht.elt(index);

        if tag.is_empty() {
            return None;
        }

        if tag == key {
            return Some(ht.elt(index + nsize));
        }

        index += hash2;
        if index >= nsize {
            index -= nsize;
        }
        if index == hash1 {
            return None;
        }
    }
}

fn remove_eq_hashtable(ht: Value, key: Value) -> u32 {
    let ht = ht.cast_as::<ScmHashTable>();
    debug_assert!(ht.lock.is_locked());
    let nsize = ht.datum().capacity;
    let hash1 = u64_hash1(key.get_raw() as _, nsize as _);
    let hash2 = u64_hash2(key.get_raw() as _, nsize as _);
    let mut index = hash1;

    loop {
        let tag = ht.elt(index);

        if tag.is_empty() {
            return 0;
        }

        if tag == key {
            ht.datum_mut().live -= 1;
            *ht.elt_mut(index) = Value::encode_undefined_value();
            *ht.elt_mut(index + nsize) = Value::encode_undefined_value();
            return if ht.datum().live < hash_sparse_threshold(nsize) {
                lookup_hashtable_size(hash_mutable_size(ht.datum().live))
            } else {
                0
            };
        }

        index += hash2;
        if index >= nsize {
            index -= nsize;
        }
        if index == hash1 {
            return 0;
        }
    }
}

fn simple_hash2(hash: u32, nsize: u32) -> u32 {
    let mut dist = nsize >> 6;
    dist = if dist < 8 {
        if (nsize >> 8) != 0 {
            8
        } else {
            1
        }
    } else {
        dist
    };
    let hash2 = dist - (hash % dist);
    debug_assert!(hash2 != 0 && hash2 < nsize);
    hash2
}

pub fn put_hashtable<const CHECK_REHASH: bool>(ht: Value, key: Value, val: Value) -> u32 {
    // check if rehashing needed
    // `rehash` is true only after GC cycle, it is required
    // because GC may move keys in the hashtable around.
    if CHECK_REHASH {
        if ht.cast_as::<ScmHashTable>().rehash {
            inplace_rehash_hashtable(ht);
        }
    }

    let h = ht.cast_as::<ScmHashTable>();
    if h.typ == HashTableType::Eq {
        return put_eq_hashtable(ht, key, val);
    }

    let nsize = h.datum().capacity;
    let hash1 = (h.hash)(key, nsize);
    let hash2 = simple_hash2(hash1, nsize);
    let mut index = hash1;

    let res = loop {
        let tag = h.elt(index);
        if tag.is_empty() {
            h.datum_mut().live += 1;
            h.datum_mut().used += 1;
            break true;
        }

        if tag.is_undefined() {
            h.datum_mut().live += 1;
            break true;
        }

        if tag == key || (h.equiv)(tag, key) {
            break true;
        }

        index += hash2;
        if index >= nsize {
            index -= nsize;
        }
        if index == hash1 {
            break false;
        }
    };
    if res {
        unsafe {
            let datum = h.datum;
            h.elt_mut(index).assign(transmute(datum), key);
            h.elt_mut(index + nsize).assign(transmute(datum), val);
        }
        if h.datum().used < hash_busy_threshold(nsize) {
            return 0;
        }

        if h.datum().live < hash_dense_threshold(nsize) {
            return nsize;
        }

        return lookup_hashtable_size(nsize);
    } else {
        panic!("put_hashtable: table overflow")
    }
}

pub fn get_hashtable<const CHECK_REHASH: bool>(ht: Value, key: Value) -> Option<Value> {
    if CHECK_REHASH {
        if ht.cast_as::<ScmHashTable>().rehash {
            inplace_rehash_hashtable(ht);
        }
    }
    let h = ht.cast_as::<ScmHashTable>();
    if h.typ == HashTableType::Eq {
        return get_eq_hashtable(ht, key);
    }

    let nsize = h.datum().capacity;
    let hash1 = (h.hash)(key, nsize);
    let hash2 = simple_hash2(hash1, nsize);

    let mut index = hash1;
    loop {
        let tag = h.elt(index);

        if tag.is_empty() {
            return None;
        }

        if tag == key || (h.equiv)(tag, key) {
            return Some(h.elt(index + nsize));
        }

        index += hash2;
        if index >= nsize {
            index -= nsize;
        }
        if index == hash1 {
            return None;
        }
    }
}

pub fn remove_hashtable<const CHECK_REHASH: bool>(ht: Value, key: Value) -> u32 {
    if CHECK_REHASH {
        if ht.cast_as::<ScmHashTable>().rehash {
            inplace_rehash_hashtable(ht);
        }
    }
    let h = ht.cast_as::<ScmHashTable>();
    if h.typ == HashTableType::Eq {
        return remove_eq_hashtable(ht, key);
    }

    let nsize = h.datum().capacity;
    let hash1 = (h.hash)(key, nsize);
    let hash2 = simple_hash2(hash1, nsize);

    let mut index = hash1;
    loop {
        let tag = h.elt(index);

        if tag.is_empty() {
            return 0;
        }

        if tag == key || (h.equiv)(tag, key) {
            h.datum_mut().live -= 1;
            *h.elt_mut(index) = Value::encode_undefined_value();
            *h.elt_mut(index + nsize) = Value::encode_undefined_value();
            return if h.datum().live < hash_sparse_threshold(nsize) {
                lookup_hashtable_size(hash_mutable_size(h.datum().live))
            } else {
                0
            };
        }

        index += hash2;
        if index >= nsize {
            index -= nsize;
        }
        if index == hash1 {
            return 0;
        }
    }
}

pub(crate) fn inplace_rehash_hashtable(ht: Value) {
    let h = ht;
    let ht = ht.cast_as::<ScmHashTable>();

    let nelts = ht.datum().capacity;
    let datum_size =
        size_of::<HashTableRec>() + size_of::<Value>() * ((nelts as usize + nelts as usize) - 1);
    unsafe {
        let save_datum = mmtk::memory_manager::malloc(datum_size).to_mut_ptr::<u8>();
        save_datum.copy_from_nonoverlapping(ht.datum as *const u8, datum_size);
        let save_datum = save_datum.cast::<HashTableRec>();
        clear_volatile_hashtable(ht);
        for i in 0..nelts {
            if (*save_datum).elts.as_ptr().add(i as _).read().is_empty() {
                continue;
            }

            if (*save_datum)
                .elts
                .as_ptr()
                .add(i as _)
                .read()
                .is_undefined()
            {
                continue;
            }

            put_hashtable::<false>(
                h,
                (*save_datum).elts.as_ptr().add(i as _).read(),
                (*save_datum).elts.as_ptr().add((i + nelts) as _).read(),
            );
        }

        mmtk::memory_manager::free(Address::from_mut_ptr(save_datum));
    }
}

pub(crate) fn inplace_rehash_weak_hashtable(ht: Value) {
    let h = ht;
    let datum = ht.cast_as::<WeakHashtable>().datum;

    let nelts = unsafe { (*datum).capacity };
    
    let datum_size = size_of::<WeakHashTableRec>() + size_of::<Value>() * ((nelts as usize ) - 1);
    let save_datum = mmtk::memory_manager::malloc(datum_size).to_mut_ptr::<u8>();
    unsafe {
        save_datum.copy_from_nonoverlapping(datum as *const u8, datum_size);
        let save_datum = save_datum.cast::<WeakHashTableRec>();
        clear_volatile_weak_hashtable(ht.cast_as());
        for i in 0..nelts {
            let elt = (*save_datum).elts.as_ptr().add(i as _).read();
            if elt.is_empty() {
                continue;
            }

            if elt
                .is_undefined()
            {
                continue;
            }
            assert!(elt.type_of() == TypeId::WeakMapping);
            if !elt.cast_as::<ScmWeakMapping>().key.is_object() {
                continue;
            }
            put_weak_hashtable(
                h,
                elt
            );
        }

        mmtk::memory_manager::free(Address::from_mut_ptr(save_datum));
    }
}

pub(crate) fn clear_volatile_hashtable(ht: &mut ScmHashTable) {
    let n = ht.datum().capacity;
    ht.datum_mut().live = 0;
    ht.datum_mut().used = 0;

    for i in 0..(n + n) {
        *ht.elt_mut(i) = Value::encode_empty_value();
    }
}


pub(crate) fn clear_volatile_weak_hashtable(ht: &mut WeakHashtable) {
    unsafe {
        let n = (*ht.datum).capacity;
        (*ht.datum).live = 0;
        (*ht.datum).used = 0;

        for i in 0..n {
            (*ht.datum).elts.as_mut_ptr().add(i as _).write(Value::encode_empty_value());
        }
    }
}
pub fn rehash_hashtable(thread: &mut Thread, mut ht: Value, nsize: u32) -> Value {
    
    let nelts = ht.cast_as::<ScmHashTable>().datum().capacity;

    let ht2 = {
        // protect ht from GC
        let ht2 = gc_protect!(thread => ht => thread.make_hashtable(nsize, ht.cast_as::<ScmHashTable>().typ));
        ht2
    };
    
    ht2.cast_as::<ScmHashTable>().lock.lock(true);
    let h = ht.cast_as::<ScmHashTable>();
    for i in 0..nelts {
        if h.elt(i).is_empty() {
            continue;
        }

        if h.elt(i).is_undefined() {
            continue;
        }

        put_hashtable::<false>(ht2, h.elt(i), h.elt(i + nelts));
    }

    ht2.cast_as::<ScmHashTable>().lock.unlock();
    std::mem::swap(&mut h.datum, &mut ht2.cast_as::<ScmHashTable>().datum);
    ht
}

pub fn rehash_weak_hashtable(thread: &mut Thread, mut ht: Value, nsize: u32) -> Value {
    unsafe {
        let nelts = (*ht.cast_as::<WeakHashtable>().datum).capacity;

        let ht2 = {
            // protect ht from GC
            let ht2 = gc_protect!(thread => ht => thread.make_weak_hashtable(nsize));
            ht2
        };

        ht2.cast_as::<WeakHashtable>().lock.lock(true);

        for i in 0..nelts {
            let elt = (*ht.cast_as::<WeakHashtable>().datum).elts.as_ptr().add(i as _).read();
            if elt.is_empty() {
                continue;
            }

            if elt.is_undefined() {
                continue;
            }

            assert!(elt.type_of() == TypeId::WeakMapping);
            if !elt.cast_as::<ScmWeakMapping>().key.is_object() {
                continue;
            }
            put_weak_hashtable(
                ht2,
                elt
            );
        }

        ht2.cast_as::<WeakHashtable>().lock.unlock();
        std::mem::swap(&mut (*ht.cast_as::<WeakHashtable>().datum), &mut (*ht2.cast_as::<WeakHashtable>().datum));
        ht
    }
}

pub fn dummy_hash(_key: Value, _nsize: u32) -> u32 {
    todo!()
}

pub fn dummy_equiv(_key1: Value, _key2: Value) -> bool {
    todo!()
}

pub fn hashtable_lock(ht: Value) {
    ht.cast_as::<ScmHashTable>().lock.lock(true);
}

pub fn hashtable_unlock(ht: Value) {
    ht.cast_as::<ScmHashTable>().lock.unlock();
}

#[repr(C)]
pub struct WeakHashTableRec {
    pub header: ScmCellHeader,
    pub capacity: u32,
    pub used: u32,
    pub live: u32,
    pub elts: [Value; 1],
}

#[repr(C)]
pub struct WeakHashtable {
    pub header: ScmCellHeader,
    pub lock: RawMutex,
    pub datum: *mut WeakHashTableRec,
}

pub fn lookup_weak_hashtable(ht: Value, key: Value) -> Value {
    let datum = ht.cast_as::<WeakHashtable>().datum;
    unsafe {
        let datum = &mut *datum;
        let nsize = datum.capacity;
        let hash1 = u64_hash1(key.get_raw() as _, nsize as _);
        let hash2 = u64_hash2(key.get_raw() as _, nsize as _);
        let mut index = hash1;
        loop {
            let entry = datum.elts.as_ptr().add(index as _).read();
            if entry.is_empty() {
                return Value::encode_undefined_value();
            }
            
            if entry != Value::encode_undefined_value() {
                let wmap = entry.cast_as::<ScmWeakMapping>();
                if wmap.key.is_false() {
                    datum.elts.as_mut_ptr().add(index as _).write(Value::encode_undefined_value());
                    datum.live -= 1;
                } else {
                    if wmap.key == key {
                        return wmap.value;
                    }
                }
            }

            index = index.wrapping_add(hash2);
            if index >= nsize {
                index -= nsize;
            }
            if index == hash1 {
                panic!("lookup_weak_hashtable: table overflow")
            }
        }
    }
}

pub fn put_weak_hashtable(ht: Value, wmap: Value) -> u32 {
    assert!(wmap.type_of() == TypeId::WeakMapping);
    let key = wmap.cast_as::<ScmWeakMapping>().key;
    unsafe {
        let datum = &mut*ht.cast_as::<WeakHashtable>().datum;
        let nsize = datum.capacity;
        let hash1 = u64_hash1(key.get_raw() as _, nsize as _);
        let hash2 = u64_hash2(key.get_raw() as _, nsize as _);
        let mut index = hash1;

        let found = loop {
            let entry = datum.elts.as_ptr().add(index as _).read();
            if entry.is_empty() {
                datum.live += 1;
                datum.used += 1;
                break true;
            }

            if entry.is_undefined() {
                datum.live += 1;
                break true;
            }

            index = index.wrapping_add(hash2);
            if index >= nsize {
                index -= nsize;
            }

            if index == hash1 {
                break false;
            }
        };

        if found {
           
            datum.elts.as_mut_ptr().add(index as _).write(wmap);
            if datum.used < hash_busy_threshold(nsize) {
                return 0;
            }

            if datum.live < hash_sparse_threshold(nsize) {
                return lookup_hashtable_size(datum.live);
            }

            if datum.live < hash_dense_threshold(nsize) {
                return nsize;
            }

            return lookup_hashtable_size(nsize);
        } else {
            panic!("put_weak_hashtable: table overflow")
        }
    }

}

pub fn remove_weak_hashtable(ht: Value, key: Value) -> u32 {
    unsafe {
        let datum = &mut*ht.cast_as::<WeakHashtable>().datum;
        let nsize = datum.capacity;
        let hash1 = u64_hash1(key.get_raw() as _, nsize as _);
        let hash2 = u64_hash2(key.get_raw() as _, nsize as _);
        let mut index = hash1;

        loop {
            let entry = datum.elts.as_ptr().add(index as _).read();
            if entry.is_empty() {
                return 0;
            }

            if !entry.is_undefined() {
                let wmap = entry.cast_as::<ScmWeakMapping>();
                if !wmap.key.is_object() {
                    datum.elts.as_mut_ptr().add(index as _).write(Value::encode_undefined_value());
                    datum.live -= 1;
                } else {
                    datum.elts.as_mut_ptr().add(index as _).write(Value::encode_undefined_value());
                    datum.live -= 1;
                    if datum.live < hash_sparse_threshold(nsize) {
                        return lookup_hashtable_size(hash_mutable_size(datum.live));
                    } else {
                        return 0;
                    }
                }
            }

            index = index.wrapping_add(hash2);
            if index >= nsize {
                index -= nsize;
            }

            if index == hash1 {
                return 0;
            }
        }
    }
}