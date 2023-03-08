use crate::{value::Value, error::{out_of_range, wrong_contract}, compiler::env::environment_set};

pub fn bad_vec_index<T>(name: &str, i: Value, which: &str, vec: Value, bottom: isize, len: isize) -> Result<T, Value> {
    out_of_range(name, Some("vector"), which, i, vec, bottom as _, len as _)
}

pub fn bad_index<T>(name: &str, which: &str, i: Value, vec: Value, bottom: isize) -> Result<T, Value> {
    bad_vec_index(name, i, which, vec, bottom, vec.vector_len() as _)
}

define_proc! {
    extern "vector-ref", vector_ref(_vm, args) 2, 2 => {
        if !args[0].vectorp() {
            return wrong_contract::<()>("vector-ref", "vector?", 0, args.len() as _, args).into();
        }

        if !args[1].intp() {
            return wrong_contract::<()>("vector-ref", "exact-integer?", 1, args.len() as _, args).into()
        }

        let mut i = args[1].int() as isize;

        if i < 0 {
            i = args[0].vector_len() as isize + i;
        }

        if i < 0 || i >= args[0].vector_len() as isize {
            return bad_index::<()>("vector-ref", "index", args[1], args[0], 0).into()
        }

        crate::vm::Trampoline::Return(args[0].vector_ref(i as _))
    }
}

pub fn initialize_vector(env: Value) {
    environment_set(env, *VECTOR_REF_NAME, *VECTOR_REF_PROC);
}