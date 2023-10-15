use mmtk::util::Address;
use once_cell::sync::Lazy;

use crate::{
    gc_protect,
    interpreter::scm_call_n,
    runtime::object::ScmPair,
    vm::{scm_virtual_machine, thread::Thread}, gc::ObjEdge,
};

use super::{
    control::{invalid_argument_violation, scheme_raise, wrong_type_argument_violation},
    environment::{environment_get, environment_get_cell},
    gsubr::{scm_define_subr, Subr},
    hashtable::*,
    object::{scm_vector_length, scm_vector_ref, ScmWeakMapping, TypeId, ScmGloc},
    symbol::scm_intern,
    value::Value, arith::scm_to_u32,
};

extern "C-unwind" fn make_weak_hashtable(thread: &mut Thread) -> Value {
    thread.make_weak_hashtable(16)
}

extern "C-unwind" fn weak_hashtable_p(_thread: &mut Thread, obj: &mut Value) -> Value {
    Value::encode_bool_value(obj.type_of() == TypeId::WeakHashTable)
}

extern "C-unwind" fn put_weak_hashtable_proc(
    thread: &mut Thread,
    obj: &mut Value,
    key: &mut Value,
    val: &mut Value,
) -> Value {
    let wmap = thread.make_weakmapping();
    wmap.cast_as::<ScmWeakMapping>().key = *key;
    wmap.cast_as::<ScmWeakMapping>().value = *val;
    obj.cast_as::<WeakHashtable>().lock.lock(true);
    let nsize = put_weak_hashtable(*obj, wmap);
    if nsize != 0 {
        rehash_weak_hashtable(thread, *obj, nsize);
    }
    obj.cast_as::<WeakHashtable>().lock.unlock();
    Value::encode_undefined_value()
}

extern "C-unwind" fn remove_weak_hashtable_proc(
    thread: &mut Thread,
    obj: &mut Value,
    key: &mut Value,
) -> Value {
    obj.cast_as::<WeakHashtable>().lock.lock(true);
    let nsize = remove_weak_hashtable(*obj, *key);
    if nsize != 0 {
        rehash_weak_hashtable(thread, *obj, nsize);
    }
    obj.cast_as::<WeakHashtable>().lock.unlock();
    Value::encode_undefined_value()
}

extern "C-unwind" fn lookup_weak_hashtable_proc(
    _thread: &mut Thread,
    obj: &mut Value,
    key: &mut Value,
) -> Value {
    obj.cast_as::<WeakHashtable>().lock.lock(true);
    let val = lookup_weak_hashtable(*obj, *key);
    obj.cast_as::<WeakHashtable>().lock.unlock();
    val
}

extern "C-unwind" fn weak_hashtable_keys(thread: &mut Thread, obj: &mut Value) -> Value {
    obj.cast_as::<WeakHashtable>().lock.lock(true);
    let mut ls = Value::encode_null_value();
    unsafe {
        let datum = &*obj.cast_as::<WeakHashtable>().datum;
        for i in 0..datum.capacity {
            let mut elt = datum.elts.as_ptr().add(i as _).read();

            if elt.is_undefined() || elt.is_empty() {
                continue;
            }

            assert!(elt.type_of() == TypeId::WeakMapping);
            let wmap = elt.cast_as::<ScmWeakMapping>();
            if wmap.key.is_object() {
                let pair = gc_protect!(thread => ls, elt => thread.make_cons::<false>(Value::encode_null_value(), Value::encode_null_value()));
                let wmap = elt.cast_as::<ScmWeakMapping>();
                pair.cast_as::<ScmPair>().car = wmap.key;
                pair.cast_as::<ScmPair>().cdr = ls;
                ls = pair;
            }
        }
    }
    obj.cast_as::<WeakHashtable>().lock.unlock();
    ls
}

extern "C-unwind" fn make_core_hashtable(
    thread: &mut Thread,
    typ: &mut Value,
    size: &mut Value,
) -> Value {
    let mut nsize = lookup_hashtable_size(0);
    if typ.is_undefined() && size.is_undefined() {
        return thread.make_hashtable(nsize, HashTableType::Eq);
    }

    if typ.is_symbol() {
        if *typ == scm_intern("generic") {
            if size.is_vector() {
                if scm_vector_length(*size) == 14 {
                    return thread.make_hashtable_generic(nsize, *size);
                } else {
                    invalid_argument_violation::<{usize::MAX}>(
                        thread,
                        "make-core-hashtable",
                        "14 element vector",
                        *size,
                        1,
                        2,
                        &[typ, size],
                    );
                }
            } else {
                wrong_type_argument_violation::<{ usize::MAX }>(
                    thread,
                    "make-core-hashtable",
                    1,
                    "vector",
                    *size,
                    1,
                    &[typ, size],
                )
            }
        }

        if !size.is_undefined() {
            if size.is_int32() {
                nsize = lookup_hashtable_size(size.get_int32() as _);
            } else {
                wrong_type_argument_violation::<{ usize::MAX }>(
                    thread,
                    "make-core-hashtable",
                    1,
                    "integer",
                    *size,
                    1,
                    &[typ, size],
                )
            }
        }

        if *typ == scm_intern("eq?") {
            return thread.make_hashtable(nsize, HashTableType::Eq);
        }

        if *typ == scm_intern("eqv?") {
            return thread.make_hashtable(nsize, HashTableType::Eqv);
        }

        if *typ == scm_intern("equal?") {
            return thread.make_hashtable(nsize, HashTableType::Equal);
        }

        if *typ == scm_intern("string=?") {
            return thread.make_hashtable(nsize, HashTableType::String);
        }
    }

    wrong_type_argument_violation::<{ usize::MAX }>(
        thread,
        "make-core-hashtable",
        0,
        "eq?, eqv?, equal?, string=? or generic",
        *typ,
        1 + size.is_undefined() as usize,
        &[typ, size],
    );
}

extern "C-unwind" fn core_hashtable_p(_thread: &mut Thread, obj: &mut Value) -> Value {
    Value::encode_bool_value(
        obj.type_of() == TypeId::HashTable || obj.type_of() == TypeId::WeakHashTable,
    )
}

extern "C-unwind" fn core_hashtable_mutable_p(_thread: &mut Thread, obj: &mut Value) -> Value {
    let typ = obj.type_of();

    if typ == TypeId::HashTable {
        Value::encode_bool_value(!obj.cast_as::<ScmHashTable>().immutable)
    } else if typ == TypeId::WeakHashTable {
        Value::encode_bool_value(!obj.cast_as::<WeakHashtable>().immutable)
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            _thread,
            "core-hashtable-mutable?",
            0,
            "hashtable or weak-hashtaable",
            *obj,
            1,
            &[obj],
        )
    }
}

extern "C-unwind" fn core_hashtable_equivalence_function(
    thread: &mut Thread,
    ht: &mut Value,
) -> Value {
    let typ = ht.type_of();

    if matches!(typ, TypeId::HashTable | TypeId::WeakHashTable) {
        if typ == TypeId::WeakHashTable {
            return environment_get(
                scm_virtual_machine().interaction_environment,
                scm_intern("eq?"),
            )
            .unwrap();
        }
        let orig = *ht;
        let ht = ht.cast_as::<ScmHashTable>();

        if ht.handlers.is_false() {
            match ht.typ {
                HashTableType::Eq => {
                    return environment_get(
                        scm_virtual_machine().interaction_environment,
                        scm_intern("eq?"),
                    )
                    .unwrap();
                }

                HashTableType::Eqv => {
                    return environment_get(
                        scm_virtual_machine().interaction_environment,
                        scm_intern("eqv?"),
                    )
                    .unwrap();
                }

                HashTableType::Equal => {
                    return environment_get(
                        scm_virtual_machine().interaction_environment,
                        scm_intern("equal?"),
                    )
                    .unwrap();
                }

                HashTableType::String => {
                    return environment_get(
                        scm_virtual_machine().interaction_environment,
                        scm_intern("string=?"),
                    )
                    .unwrap();
                }

                _ => unreachable!(),
            }
        }

        let vector = ht.handlers;

        match scm_call_n::<false>(
            thread,
            scm_vector_ref(vector, SCM_HASHTABLE_HANDLER_EQUIV_FUNC as _),
            &[orig],
        ) {
            Ok(val) => val,
            Err(val) => {
                scheme_raise(thread, val);
            }
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "core-hashtable-equivalence-function",
            0,
            "hashtable or weak-hashtaable",
            *ht,
            1,
            &[ht],
        )
    }
}

extern "C-unwind" fn core_hashtable_hash_function(thread: &mut Thread, ht: &mut Value) -> Value {
    let typ = ht.type_of();

    if matches!(typ, TypeId::HashTable | TypeId::WeakHashTable) {
        if typ == TypeId::WeakHashTable {
            return Value::encode_bool_value(false);
        }

        let hash = ht.cast_as::<ScmHashTable>();

        if hash.typ != HashTableType::Generic {
            return Value::encode_bool_value(false);
        }

        let vector = hash.handlers;

        match scm_call_n::<false>(
            thread,
            scm_vector_ref(vector, SCM_HASHTABLE_HANDLER_HASH_FUNC as _),
            &[],
        ) {
            Ok(val) => val,
            Err(val) => {
                scheme_raise(thread, val);
            }
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "core-hashtable-hash-function",
            0,
            "hashtable or weak-hashtaable",
            *ht,
            1,
            &[ht],
        )
    }
}

extern "C-unwind" fn core_hashtable_set(
    thread: &mut Thread,
    ht: &mut Value,
    key: &mut Value,
    val: &mut Value,
) -> Value {
    let typ = ht.type_of();

    if matches!(typ, TypeId::HashTable | TypeId::WeakHashTable) {
        if typ == TypeId::HashTable {
            let hash = ht.cast_as::<ScmHashTable>();

            if !hash.immutable {
                hash.lock.lock(true);
                if hash.typ == HashTableType::String && !key.is_string() {
                    hash.lock.unlock();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "core-hashtable-set!",
                        1,
                        "string",
                        *key,
                        3,
                        &[ht, key, val],
                    )
                }

                let nsize = put_hashtable::<true>(*ht, *key, *val);
                if nsize != 0 {
                    rehash_hashtable(thread, *ht, nsize);
                }
                hash.lock.unlock();
                return Value::encode_undefined_value();
            } else {
                invalid_argument_violation::<{usize::MAX}>(
                    thread,
                    "core-hashtable-set!",
                    "mutable hashtable expected",
                    *ht,
                    0,
                    3,
                    &[ht, key, val],
                );
            }
        }

        let hash = ht.cast_as::<WeakHashtable>();
        let ephemeron = thread.make_weakmapping();
        ephemeron.cast_as::<ScmWeakMapping>().key = *key;
        ephemeron.cast_as::<ScmWeakMapping>().value = *val;
        hash.lock.lock(true);
        let nsize = put_weak_hashtable(*ht, ephemeron);
        if nsize != 0 {
            rehash_weak_hashtable(thread, *ht, nsize);
        }
        hash.lock.unlock();
        Value::encode_undefined_value()
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "core-hashtable-set!",
            0,
            "hashtable or weak-hashtaable",
            *ht,
            3,
            &[ht, key, val],
        )
    }
}

extern "C-unwind" fn core_hashtable_ref(
    thread: &mut Thread,
    ht: &mut Value,
    key: &mut Value,
    default: &mut Value,
) -> Value {
    let typ = ht.type_of();

    if matches!(typ, TypeId::HashTable | TypeId::WeakHashTable) {
        if typ == TypeId::HashTable {
            let hash = ht.cast_as::<ScmHashTable>();
            hash.lock.lock(true);
            let val = get_hashtable::<true>(*ht, *key);
            hash.lock.unlock();
            val.unwrap_or(*default)
        } else {
            let hash = ht.cast_as::<WeakHashtable>();
            hash.lock.lock(true);
            let val = lookup_weak_hashtable(*ht, *key);
            hash.lock.unlock();
            if val.is_undefined() {
                *default
            } else {
                val
            }
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "core-hashtable-ref",
            0,
            "hashtable or weak-hashtaable",
            *ht,
            2,
            &[ht, key],
        )
    }
}

extern "C-unwind" fn core_hashtable_delete(
    thread: &mut Thread,
    ht: &mut Value,
    key: &mut Value,
) -> Value {
    let typ = ht.type_of();

    if matches!(typ, TypeId::HashTable | TypeId::WeakHashTable) {
        if typ == TypeId::HashTable {
            let hash = ht.cast_as::<ScmHashTable>();

            hash.lock.lock(true);
            if hash.immutable {
                hash.lock.unlock();
                invalid_argument_violation::<{usize::MAX}>(
                    thread,
                    "core-hashtable-delete!",
                    "mutable hashtable expected",
                    *ht,
                    0,
                    2,
                    &[ht, key],
                );
            }

            let nsize = remove_hashtable::<true>(*ht, *key);

            if nsize != 0 {
                rehash_hashtable(thread, *ht, nsize);
            }
            hash.lock.unlock();

            return Value::encode_undefined_value();
        }

        let hash = ht.cast_as::<WeakHashtable>();

        hash.lock.lock(true);
        let nsize = remove_weak_hashtable(*ht, *key);
        if nsize != 0 {
            rehash_weak_hashtable(thread, *ht, nsize);
        }

        hash.lock.unlock();
        Value::encode_undefined_value()
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "core-hashtable-delete!",
            0,
            "hashtable or weak-hashtaable",
            *ht,
            2,
            &[ht, key],
        )
    }
}

extern "C-unwind" fn core_hashtable_clear(
    thread: &mut Thread,
    ht: &mut Value,
    nsize: &mut Value,
) -> Value {
    let typ = ht.type_of();

    if matches!(typ, TypeId::HashTable | TypeId::WeakHashTable) {
        let size = if nsize.is_undefined() {
            lookup_hashtable_size(0)
        } else if nsize.is_int32() && nsize.get_int32() >= 0 {
            lookup_hashtable_size(nsize.get_int32() as u32)
        } else {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "core-hashtable-clear!",
                1,
                "non-negative fixnum",
                *nsize,
                2,
                &[ht, nsize],
            )
        };

        if typ == TypeId::HashTable {
            let hash = ht.cast_as::<ScmHashTable>();

            if hash.immutable {
                invalid_argument_violation::<{usize::MAX}>(
                    thread,
                    "core-hashtable-clear!",
                    "mutable hashtable expected",
                    *ht,
                    0,
                    2,
                    &[ht, nsize],
                );
            }

            hash.lock.lock(true);

            clear_hashtable(thread, *ht, size);
            hash.lock.unlock();
        } else {
            let hash = ht.cast_as::<WeakHashtable>();
            hash.lock.lock(true);
            clear_weak_hashtable(thread, *ht, size);
            hash.lock.unlock();
        }

        Value::encode_undefined_value()
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "core-hashtable-clear!",
            0,
            "hashtable or weak-hashtaable",
            *ht,
            1,
            &[ht],
        )
    }
}

extern "C-unwind" fn core_hashtable_size(thread: &mut Thread, ht: &mut Value) -> Value {
    let typ = ht.type_of();

    if matches!(typ, TypeId::HashTable | TypeId::WeakHashTable) {
        if typ == TypeId::HashTable {
            let hash = ht.cast_as::<ScmHashTable>();
            hash.lock.lock(true);
            let size = unsafe { (*hash.datum).live };
            hash.lock.unlock();
            Value::encode_int32(size as _)
        } else {
            let hash = ht.cast_as::<WeakHashtable>();
            hash.lock.lock(true);
            let size = unsafe { (*hash.datum).live };
            hash.lock.unlock();
            Value::encode_int32(size as _)
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "core-hashtable-size",
            0,
            "hashtable or weak-hashtaable",
            *ht,
            1,
            &[ht],
        )
    }
}

extern "C-unwind" fn core_hashtable_contains_p(
    thread: &mut Thread,
    ht: &mut Value,
    key: &mut Value,
) -> Value {
    let typ = ht.type_of();

    if matches!(typ, TypeId::HashTable | TypeId::WeakHashTable) {
        if typ == TypeId::HashTable {
            let hash = ht.cast_as::<ScmHashTable>();
            hash.lock.lock(true);
            let val = get_hashtable::<true>(*ht, *key);
            hash.lock.unlock();
            Value::encode_bool_value(val.is_some())
        } else {
            let hash = ht.cast_as::<WeakHashtable>();
            hash.lock.lock(true);
            let val = lookup_weak_hashtable(*ht, *key);
            hash.lock.unlock();
            Value::encode_bool_value(!val.is_undefined())
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "core-hashtable-contains?",
            0,
            "hashtable or weak-hashtaable",
            *ht,
            2,
            &[ht, key],
        )
    }
}

extern "C-unwind" fn core_hashtable_copy(
    thread: &mut Thread,
    ht: &mut Value,
    immutable_p: &mut Value,
) -> Value {
    let immutable = if immutable_p.is_bool() {
        immutable_p.get_bool()
    } else if immutable_p.is_undefined() {
        false
    } else {
        true
    };

    let typ = ht.type_of();

    if matches!(typ, TypeId::HashTable | TypeId::WeakHashTable) {
        if typ == TypeId::HashTable {
            let hash = ht.cast_as::<ScmHashTable>();
            hash.lock.lock(true);
            let new_copy = copy_hashtable(thread, *ht);
            new_copy.cast_as::<ScmHashTable>().immutable = immutable;
            ht.cast_as::<ScmHashTable>().lock.unlock();
            new_copy
        } else {
            let hash = ht.cast_as::<WeakHashtable>();
            hash.lock.lock(true);
            let new_copy = copy_weak_hashtable(thread, *ht);
            new_copy.cast_as::<WeakHashtable>().immutable = immutable;
            ht.cast_as::<WeakHashtable>().lock.unlock();
            new_copy
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "core-hashtable-copy",
            0,
            "hashtable or weak-hashtaable",
            *ht,
            1,
            &[ht],
        )
    }
}

extern "C-unwind" fn core_hashtable_to_alist(thread: &mut Thread, ht: &mut Value) -> Value {
    let typ = ht.type_of();

    if matches!(typ, TypeId::HashTable | TypeId::WeakHashTable) {
        if typ == TypeId::HashTable {
            ht.cast_as::<ScmHashTable>().lock.lock(true);

            let nsize = ht.cast_as::<ScmHashTable>().datum().capacity;

            let mut ans = Value::encode_null_value();

            for i in 0..nsize {
                let elt = ht.cast_as::<ScmHashTable>().elt(i);
                if elt.is_undefined() || elt.is_empty() {
                    continue;
                }
                let val = ht.cast_as::<ScmHashTable>().elt(i + nsize);

                let mut cons = gc_protect!(thread => ans => thread.make_cons::<false>(elt, val));
                let cons2 = gc_protect!(thread => cons => thread.make_cons::<false>(cons, ans));

                ans = cons2;
            }

            ht.cast_as::<ScmHashTable>().lock.unlock();

            ans
        } else {
            ht.cast_as::<WeakHashtable>().lock.lock(true);

            let nsize = unsafe { (*ht.cast_as::<WeakHashtable>().datum).capacity };

            let mut ans = Value::encode_null_value();

            unsafe {
                for i in 0..nsize {
                    let elt = (*ht.cast_as::<WeakHashtable>().datum)
                        .elts
                        .as_ptr()
                        .add(i as _)
                        .read();
                    if elt.is_undefined() || elt.is_empty() || !elt.is_object() {
                        continue;
                    }
                    let wmap = elt.cast_as::<ScmWeakMapping>();
                    let mut cons = gc_protect!(thread => ans => thread.make_cons::<false>(wmap.key, wmap.value));
                    let cons2 = gc_protect!(thread => cons => thread.make_cons::<false>(cons, ans));

                    ans = cons2;
                }
            }

            ht.cast_as::<WeakHashtable>().lock.unlock();

            ans
        }
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "core-hashtable->alist",
            0,
            "hashtable or weak-hashtaable",
            *ht,
            1,
            &[ht],
        )
    }
}

static mut SCM_LOC_SOURCE_LOCATION_TABLE: Lazy<Value> = Lazy::new(|| {
    let name = scm_intern("source-location-table");
    let env = scm_virtual_machine().interaction_environment;
    let table = environment_get_cell(env, name).unwrap();

    table
});

pub(crate) fn visit_roots(edges: &mut Vec<ObjEdge>) {
    unsafe {
        if let Some(val) = Lazy::get_mut(&mut SCM_LOC_SOURCE_LOCATION_TABLE) {
            if val.is_object() {
                let edge = ObjEdge::from_address(Address::from_mut_ptr(val));
                edges.push(edge);
            }
        }
    }
}

pub fn scm_source_location_table() -> Value {
    unsafe {
        SCM_LOC_SOURCE_LOCATION_TABLE.cast_as::<ScmGloc>().value
    }
}

pub fn get_source_location(expr: Value) -> Option<(Value, u32, u32)> {
    let table = scm_source_location_table();
    if table.type_of() != TypeId::WeakHashTable {
        return None;
    }

    let val = lookup_weak_hashtable(table, expr);
    if val.is_false() {
        return None;
    }

    if !val.is_vector() {
        return None;
    }

    let file = scm_vector_ref(val, 0);
    let line = scm_vector_ref(val, 1);
    let col = scm_vector_ref(val, 2);

    let line = scm_to_u32(line);
    let col = scm_to_u32(col);

    Some((file, line, col))
}

pub(crate) fn init() {
    scm_define_subr(
        "make-core-hashtable",
        0,
        2,
        0,
        Subr::F2(make_core_hashtable),
    );

    scm_define_subr("core-hashtable?", 1, 0, 0, Subr::F1(core_hashtable_p));
    scm_define_subr(
        "core-hashtable-mutable?",
        1,
        0,
        0,
        Subr::F1(core_hashtable_mutable_p),
    );
    scm_define_subr(
        "core-hashtable-equivalence-function",
        1,
        0,
        0,
        Subr::F1(core_hashtable_equivalence_function),
    );
    scm_define_subr(
        "core-hashtable-hash-function",
        1,
        0,
        0,
        Subr::F1(core_hashtable_hash_function),
    );
    scm_define_subr("core-hashtable-set!", 3, 0, 0, Subr::F3(core_hashtable_set));
    scm_define_subr("core-hashtable-ref", 3, 0, 0, Subr::F3(core_hashtable_ref));
    scm_define_subr(
        "core-hashtable-delete!",
        2,
        0,
        0,
        Subr::F2(core_hashtable_delete),
    );
    scm_define_subr(
        "core-hashtable-clear!",
        1,
        1,
        0,
        Subr::F2(core_hashtable_clear),
    );
    scm_define_subr(
        "core-hashtable-size",
        1,
        0,
        0,
        Subr::F1(core_hashtable_size),
    );
    scm_define_subr(
        "core-hashtable-contains?",
        2,
        0,
        0,
        Subr::F2(core_hashtable_contains_p),
    );
    scm_define_subr(
        "core-hashtable-copy",
        1,
        1,
        0,
        Subr::F2(core_hashtable_copy),
    );
    scm_define_subr(
        "core-hashtable->alist",
        1,
        0,
        0,
        Subr::F1(core_hashtable_to_alist),
    );

    scm_define_subr(
        "make-weak-core-hashtable",
        0,
        0,
        0,
        Subr::F0(make_weak_hashtable),
    );
    scm_define_subr("weak-core-hashtable?", 1, 0, 0, Subr::F1(weak_hashtable_p));
    scm_define_subr(
        "weak-core-hashtable-put!",
        3,
        0,
        0,
        Subr::F3(put_weak_hashtable_proc),
    );
    scm_define_subr(
        "weak-core-hashtable-remove!",
        2,
        0,
        0,
        Subr::F2(remove_weak_hashtable_proc),
    );
    scm_define_subr(
        "weak-core-hashtable-ref",
        2,
        0,
        0,
        Subr::F2(lookup_weak_hashtable_proc),
    );

    scm_define_subr(
        "weak-core-hashtable-keys",
        1,
        0,
        0,
        Subr::F1(weak_hashtable_keys),
    );
}
