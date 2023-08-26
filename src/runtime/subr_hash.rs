use crate::{vm::thread::Thread, gc_protect, runtime::object::ScmPair};

use super::{hashtable::*, object::{TypeId, ScmWeakMapping}, value::Value, gsubr::{scm_define_subr, Subr}};

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
    obj.cast_as::<WeakHashtable>().lock.lock(false);
    let val = lookup_weak_hashtable(*obj, *key);
    obj.cast_as::<WeakHashtable>().lock.unlock();
    val
}

extern "C-unwind" fn weak_hashtable_keys(
    thread: &mut Thread,
    obj: &mut Value 
) -> Value {
    obj.cast_as::<WeakHashtable>().lock.lock(false);
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

pub(crate) fn init() {
    scm_define_subr("make-weak-hashtable", 0, 0, 0, Subr::F0(make_weak_hashtable));
    scm_define_subr("weak-hashtable?", 1, 0, 0, Subr::F1(weak_hashtable_p));
    scm_define_subr(
        "weak-hashtable-put!",
        3,
        0,
        0,
        Subr::F3(put_weak_hashtable_proc),
    );
    scm_define_subr(
        "weak-hashtable-remove!",
        2,
        0,
        0,
        Subr::F2(remove_weak_hashtable_proc),
    );
    scm_define_subr(
        "weak-hashtable-ref",
        2,
        0,
        0,
        Subr::F2(lookup_weak_hashtable_proc),
    );

    scm_define_subr(
        "weak-hashtable-keys",
        1,
        0,
        0,
        Subr::F1(weak_hashtable_keys),
    );
}