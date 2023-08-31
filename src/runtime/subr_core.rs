use std::collections::HashMap;

use crate::{
    compiler::{
        expand, fix_letrec,
        sexpr::{value_to_sexpr, Sexpr},
        tree_il::il_to_core_form,
        Cenv,
    },
    raise_exn,
    runtime::environment::ScmEnvironment,
    vm::{scm_virtual_machine, thread::Thread},
};

use super::{
    arith::scm_to_u32,
    gsubr::{scm_define_subr, Subr},
    list::scm_length,
    object::{
        scm_car, scm_cdr, scm_string_mut_str, scm_string_str, scm_symbol_str, ScmWeakMapping,
        TypeId, scm_tuple_set, scm_tuple_ref, ScmTuple,
    },
    symbol::scm_intern,
    value::Value,
};

extern "C-unwind" fn make_weakmapping(
    thread: &mut Thread,
    key: &mut Value,
    value: &mut Value,
) -> Value {
    let mapping = thread.make_weakmapping();
    mapping.cast_as::<ScmWeakMapping>().key = *key;
    mapping.cast_as::<ScmWeakMapping>().value = *value;

    mapping
}

extern "C-unwind" fn weakmapping_p(_thread: &mut Thread, obj: &mut Value) -> Value {
    Value::encode_bool_value(obj.type_of() == TypeId::WeakMapping)
}

extern "C-unwind" fn weakmapping_value(_thread: &mut Thread, obj: &mut Value) -> Value {
    obj.cast_as::<ScmWeakMapping>().value
}

extern "C-unwind" fn weakmapping_key(_thread: &mut Thread, obj: &mut Value) -> Value {
    obj.cast_as::<ScmWeakMapping>().key
}

extern "C-unwind" fn weakmapping_live_p(_thread: &mut Thread, obj: &mut Value) -> Value {
    Value::encode_bool_value(obj.cast_as::<ScmWeakMapping>().key.is_object())
}

extern "C-unwind" fn string_append(thread: &mut Thread, rest: &mut Value) -> Value {
    let mut result = String::new();

    while !rest.is_null() {
        let s = scm_car(*rest);
        if !s.is_string() {
            raise_exn!(
                FailContract,
                &[],
                "string-append: argument {} is not a string",
                s
            );
        }
        result.push_str(scm_string_str(s));
        *rest = scm_cdr(*rest);
    }

    thread.make_string::<false>(&result)
}

extern "C-unwind" fn symbol_to_string(thread: &mut Thread, symbol: &mut Value) -> Value {
    if !symbol.is_symbol() {
        raise_exn!(
            FailContract,
            &[],
            "symbol->string: argument {} is not a symbol",
            symbol
        );
    }
    thread.make_string::<false>(scm_symbol_str(*symbol))
}

extern "C-unwind" fn string_to_symbol(_thread: &mut Thread, string: &mut Value) -> Value {
    if !string.is_string() {
        raise_exn!(
            FailContract,
            &[],
            "string->symbol: argument {} is not a string",
            string
        );
    }
    scm_intern(scm_string_str(*string))
}

extern "C-unwind" fn string_p(_thread: &mut Thread, obj: &mut Value) -> Value {
    Value::encode_bool_value(obj.is_string())
}

extern "C-unwind" fn string_length(_thread: &mut Thread, obj: &mut Value) -> Value {
    Value::encode_int32(scm_symbol_str(*obj).len() as _)
}

extern "C-unwind" fn symbol_p(_thread: &mut Thread, obj: &mut Value) -> Value {
    Value::encode_bool_value(obj.is_symbol())
}

extern "C-unwind" fn string_ref(_thread: &mut Thread, obj: &mut Value, index: &mut Value) -> Value {
    if !obj.is_string() {
        raise_exn!(
            FailContract,
            &[],
            "string-ref: argument {} is not a string",
            obj
        );
    }
    let index = scm_to_u32(*index) as usize;
    let s = scm_string_str(*obj);
    if index >= s.len() {
        raise_exn!(
            FailContract,
            &[],
            "string-ref: index {} out of bounds",
            index
        );
    }
    Value::encode_char(s.chars().nth(index as usize).unwrap())
}

extern "C-unwind" fn string_set(
    _thread: &mut Thread,
    obj: &mut Value,
    index: &mut Value,
    ch: &mut Value,
) -> Value {
    if !obj.is_string() {
        raise_exn!(
            FailContract,
            &[],
            "string-set!: argument {} is not a string",
            obj
        );
    }
    if !ch.is_char() {
        raise_exn!(
            FailContract,
            &[],
            "string-set!: argument {} is not a character",
            ch
        );
    }

    let ch = ch.get_char();
    let index = scm_to_u32(*index) as usize;
    let s = scm_string_str(*obj);
    if index >= s.len() {
        raise_exn!(
            FailContract,
            &[],
            "string-set!: index {} out of bounds",
            index
        );
    }

    // find range of bytes to replace
    let mut start = 0;
    let mut end = 0;
    for (i, ch) in s.chars().enumerate() {
        if i == index as usize {
            start = end;
        }
        end += ch.len_utf8();
        if i == index as usize {
            break;
        }
    }

    if end >= s.len() {
        raise_exn!(
            FailContract,
            &[],
            "string-set!: index {} out of bounds",
            index
        );
    } else {
        let s = scm_string_mut_str(*obj);
        unsafe {
            s.as_bytes_mut()[start..end].copy_from_slice(ch.encode_utf8(&mut [0; 4]).as_bytes());
        }
        Value::encode_undefined_value()
    }
}

extern "C-unwind" fn string_eq(_thread: &mut Thread, rest: &mut Value) -> Value {
    let len = scm_length(*rest).unwrap_or_else(|| {
        raise_exn!(
            Fail,
            &[],
            "string=?: wrong number of arguments (given {}, expected at least 2)",
            rest
        )
    });

    if len < 2 {
        raise_exn!(
            Fail,
            &[],
            "string<?: wrong number of arguments (given {}, expected at least 2)",
            rest
        );
    }

    let first = scm_car(*rest);
    if !first.is_string() {
        raise_exn!(
            FailContract,
            &[],
            "string=?: argument {} is not a string",
            first
        );
    }

    *rest = scm_cdr(*rest);

    while rest.is_pair() {
        let s = scm_car(*rest);
        if !s.is_string() {
            raise_exn!(
                FailContract,
                &[],
                "string=?: argument {} is not a string",
                s
            );
        }
        if scm_string_str(first) != scm_string_str(s) {
            return Value::encode_bool_value(false);
        }
        *rest = scm_cdr(*rest);
    }

    Value::encode_bool_value(true)
}

extern "C-unwind" fn string_lt(_thread: &mut Thread, rest: &mut Value) -> Value {
    let len = scm_length(*rest).unwrap_or_else(|| {
        raise_exn!(
            Fail,
            &[],
            "string<?: wrong number of arguments (given {}, expected at least 2)",
            rest
        )
    });

    if len < 2 {
        raise_exn!(
            Fail,
            &[],
            "string<?: wrong number of arguments (given {}, expected at least 2)",
            rest
        );
    }

    let first = scm_car(*rest);
    if !first.is_string() {
        raise_exn!(
            FailContract,
            &[],
            "string<?: argument {} is not a string",
            first
        );
    }

    *rest = scm_cdr(*rest);

    while rest.is_pair() {
        let s = scm_car(*rest);
        if !s.is_string() {
            raise_exn!(
                FailContract,
                &[],
                "string<?: argument {} is not a string",
                s
            );
        }
        if scm_string_str(first) >= scm_string_str(s) {
            return Value::encode_bool_value(false);
        }
        *rest = scm_cdr(*rest);
    }

    Value::encode_bool_value(true)
}

extern "C-unwind" fn string_gt(_thread: &mut Thread, rest: &mut Value) -> Value {
    let len = scm_length(*rest).unwrap_or_else(|| {
        raise_exn!(
            Fail,
            &[],
            "string>?: wrong number of arguments (given {}, expected at least 2)",
            rest
        )
    });

    if len < 2 {
        raise_exn!(
            Fail,
            &[],
            "string>?: wrong number of arguments (given {}, expected at least 2)",
            rest
        );
    }

    let first = scm_car(*rest);
    if !first.is_string() {
        raise_exn!(
            FailContract,
            &[],
            "string>?: argument {} is not a string",
            first
        );
    }

    *rest = scm_cdr(*rest);

    while rest.is_pair() {
        let s = scm_car(*rest);
        if !s.is_string() {
            raise_exn!(
                FailContract,
                &[],
                "string>?: argument {} is not a string",
                s
            );
        }
        if scm_string_str(first) <= scm_string_str(s) {
            return Value::encode_bool_value(false);
        }
        *rest = scm_cdr(*rest);
    }

    Value::encode_bool_value(true)
}

extern "C-unwind" fn string_le(_thread: &mut Thread, rest: &mut Value) -> Value {
    let len = scm_length(*rest).unwrap_or_else(|| {
        raise_exn!(
            Fail,
            &[],
            "string<=?: wrong number of arguments (given {}, expected at least 2)",
            rest
        )
    });

    if len < 2 {
        raise_exn!(
            Fail,
            &[],
            "string<=?: wrong number of arguments (given {}, expected at least 2)",
            rest
        );
    }

    let first = scm_car(*rest);
    if !first.is_string() {
        raise_exn!(
            FailContract,
            &[],
            "string<=?: argument {} is not a string",
            first
        );
    }

    *rest = scm_cdr(*rest);

    while rest.is_pair() {
        let s = scm_car(*rest);
        if !s.is_string() {
            raise_exn!(
                FailContract,
                &[],
                "string<=?: argument {} is not a string",
                s
            );
        }
        if scm_string_str(first) > scm_string_str(s) {
            return Value::encode_bool_value(false);
        }
        *rest = scm_cdr(*rest);
    }

    Value::encode_bool_value(true)
}

extern "C-unwind" fn string_ge(_thread: &mut Thread, rest: &mut Value) -> Value {
    let len = scm_length(*rest).unwrap_or_else(|| {
        raise_exn!(
            Fail,
            &[],
            "string>=?: wrong number of arguments (given {}, expected at least 2)",
            rest
        )
    });

    if len < 2 {
        raise_exn!(
            Fail,
            &[],
            "string>=?: wrong number of arguments (given {}, expected at least 2)",
            rest
        );
    }

    let first = scm_car(*rest);
    if !first.is_string() {
        raise_exn!(
            FailContract,
            &[],
            "string>=?: argument {} is not a string",
            first
        );
    }

    *rest = scm_cdr(*rest);

    while rest.is_pair() {
        let s = scm_car(*rest);
        if !s.is_string() {
            raise_exn!(
                FailContract,
                &[],
                "string>=?: argument {} is not a string",
                s
            );
        }
        if scm_string_str(first) < scm_string_str(s) {
            return Value::encode_bool_value(false);
        }
        *rest = scm_cdr(*rest);
    }

    Value::encode_bool_value(true)
}

extern "C-unwind" fn raw_raise(_thread: &mut Thread, val: &mut Value) -> Value {
    std::panic::resume_unwind(Box::new(*val))
}

extern "C-unwind" fn core_preprocess(thread: &mut Thread, code: &mut Value) -> Value {
    let Some(sexpr) = value_to_sexpr(*code) else {
        raise_exn!(Fail, &[], "%core-preprocess: invalid source code")
    };
    let synenv = scm_virtual_machine()
        .interaction_environment
        .cast_as::<ScmEnvironment>()
        .syntactic_environment
        .lock(true);

    let cenv = Cenv {
        frames: Sexpr::Null,
        source_loc: &HashMap::new(),
        syntax_env: synenv.clone(),
    };
    let il = match expand::pass1(&sexpr, &cenv) {
        Ok(il) => il,
        Err(err) => {
            drop(synenv);
            raise_exn!(Fail, &[], "failed to expand source code: {}", err)
        }
    };
    let fixed = fix_letrec::pass_fix_letrec(il);

    il_to_core_form(thread, &fixed)
}

extern "C-unwind" fn tuple(thread: &mut Thread, args: &mut Value) -> Value {
    let len = scm_length(*args).unwrap();
    let tuple = thread.make_tuple::<false>(len, Value::encode_bool_value(false));
    let mut xs = *args;
    let mut i = 0;
    while !xs.is_null() {
        let x = scm_car(xs);
        scm_tuple_set(tuple, thread, i, x);
        xs = scm_cdr(xs);
        i += 1;
    }

    tuple
}

extern "C-unwind" fn tuple_pred(_thread: &mut Thread, obj: &mut Value) -> Value {
    Value::encode_bool_value(obj.is_tuple())
}

extern "C-unwind" fn tuple_ref(_: &mut Thread, obj: &mut Value, index: &mut Value) -> Value {
    if !obj.is_tuple() {
        raise_exn!(
            FailContract,
            &[],
            "tuple-ref: argument {} is not a tuple",
            obj
        );
    }
    let index = scm_to_u32(*index) as usize;
    let len = obj.cast_as::<ScmTuple>().length;
    if index >= len {
        raise_exn!(
            FailContract,
            &[],
            "tuple-ref: index {} out of bounds",
            index
        );
    }
    scm_tuple_ref(*obj, index as _)
}

extern "C-unwind" fn tuple_set(
    thread: &mut Thread,
    obj: &mut Value,
    index: &mut Value,
    value: &mut Value,
) -> Value {
    if !obj.is_tuple() {
        raise_exn!(
            FailContract,
            &[],
            "tuple-set!: argument {} is not a tuple",
            obj
        );
    }
    let index = scm_to_u32(*index) as usize;
    let len = obj.cast_as::<ScmTuple>().length;
    if index >= len {
        raise_exn!(
            FailContract,
            &[],
            "tuple-set!: index {} out of bounds",
            index
        );
    }
    scm_tuple_set(*obj, thread, index as _, *value);
    *value 
}

extern "C-unwind" fn tuple_length(_thread: &mut Thread, obj: &mut Value) -> Value {
    if !obj.is_tuple() {
        raise_exn!(
            FailContract,
            &[],
            "tuple-length: argument {} is not a tuple",
            obj
        );
    }
    Value::encode_int32(obj.cast_as::<ScmTuple>().length as _)
}

pub(crate) fn init() {
    scm_define_subr("make-weakmapping", 2, 0, 0, Subr::F2(make_weakmapping));
    scm_define_subr("weakmapping?", 1, 0, 0, Subr::F1(weakmapping_p));
    scm_define_subr("weakmapping-value", 1, 0, 0, Subr::F1(weakmapping_value));
    scm_define_subr("weakmapping-key", 1, 0, 0, Subr::F1(weakmapping_key));
    scm_define_subr("weakmapping-live?", 1, 0, 0, Subr::F1(weakmapping_live_p));

    scm_define_subr("string-append", 0, 0, 1, Subr::F1(string_append));
    scm_define_subr("symbol->string", 1, 0, 0, Subr::F1(symbol_to_string));
    scm_define_subr("string->symbol", 1, 0, 0, Subr::F1(string_to_symbol));
    scm_define_subr("string?", 1, 0, 0, Subr::F1(string_p));
    scm_define_subr("string-length", 1, 0, 0, Subr::F1(string_length));
    scm_define_subr("symbol?", 1, 0, 0, Subr::F1(symbol_p));
    scm_define_subr("string-ref", 2, 0, 0, Subr::F2(string_ref));
    scm_define_subr("string-set!", 3, 0, 0, Subr::F3(string_set));
    scm_define_subr("string=?", 0, 0, 1, Subr::F1(string_eq));
    scm_define_subr("string<?", 0, 0, 1, Subr::F1(string_lt));
    scm_define_subr("string>?", 0, 0, 1, Subr::F1(string_gt));
    scm_define_subr("string<=?", 0, 0, 1, Subr::F1(string_le));
    scm_define_subr("string>=?", 0, 0, 1, Subr::F1(string_ge));
    scm_define_subr("%raise", 1, 0, 0, Subr::F1(raw_raise));
    scm_define_subr("%core-preprocess", 1, 0, 0, Subr::F1(core_preprocess));
    scm_define_subr("tuple", 0, 0, 1, Subr::F1(tuple));
    scm_define_subr("tuple?", 1, 0, 0, Subr::F1(tuple_pred));
    scm_define_subr("tuple-ref", 2, 0, 0, Subr::F2(tuple_ref));
    scm_define_subr("tuple-set!", 3, 0, 0, Subr::F3(tuple_set));
    scm_define_subr("tuple-length", 1, 0, 0, Subr::F1(tuple_length));

    super::subr_hash::init();
    super::struct_::init();
    super::list::init();
}
