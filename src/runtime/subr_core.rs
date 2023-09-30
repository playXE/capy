use std::collections::HashMap;

use crate::{
    compiler::{
        expand, fix_letrec,
        sexpr::{value_to_sexpr, Sexpr},
        tree_il::il_to_core_form,
        Cenv,
    },
    raise_exn,
    runtime::{environment::ScmEnvironment, object::scm_bytevector_as_slice},
    vm::{scm_virtual_machine, thread::Thread},
};

use super::{
    arith::scm_to_u32,
    control::{invalid_argument_violation, wrong_type_argument_violation},
    gsubr::{scm_define_subr, Subr},
    list::scm_length,
    object::{
        scm_car, scm_cdr, scm_program_code, scm_string_mut_str, scm_string_str, scm_symbol_str,
        scm_tuple_ref, scm_tuple_set, ScmTuple, ScmWeakMapping, TypeId,
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

extern "C-unwind" fn string_to_symbol(thread: &mut Thread, string: &mut Value) -> Value {
    if !string.is_string() {
        wrong_type_argument_violation(thread, "string->symbol", 0, "string", *string, 1, &[string])
    }
    scm_intern(scm_string_str(*string))
}

extern "C-unwind" fn string_to_bytevector(thread: &mut Thread, string: &mut Value) -> Value {
    if !string.is_string() {
        wrong_type_argument_violation(
            thread,
            "string->bytevector",
            0,
            "string",
            *string,
            1,
            &[string],
        );
    }

    let s = scm_string_str(*string);
    thread.make_bytevector_from_slice::<false>(s.as_bytes())
}

extern "C-unwind" fn bytevector_to_string(thread: &mut Thread, bytevector: &mut Value) -> Value {
    if !bytevector.is_bytevector() {
        wrong_type_argument_violation(
            thread,
            "bytevector->string",
            0,
            "bytevector",
            *bytevector,
            1,
            &[bytevector],
        );
    }

    let bv = scm_bytevector_as_slice(*bytevector);
    let s = match std::str::from_utf8(bv) {
        Ok(s) => s.to_string(),
        Err(_err) => {
            invalid_argument_violation(
                thread,
                "bytevector->string",
                "expected UTF-8 encoded bytevector",
                *bytevector,
                0,
                1,
                &[bytevector],
            );
        }
    };
    thread.make_string::<false>(&s)
}

extern "C-unwind" fn list_to_string(thread: &mut Thread, list: &mut Value) -> Value {
    if let Some(_) = scm_length(*list) {
        let mut chars = vec![];

        let mut xs = *list;
        while xs.is_pair() {
            let x = scm_car(xs);
            if !x.is_char() {
                wrong_type_argument_violation(
                    thread,
                    "list->string",
                    0,
                    "list of characters",
                    *list,
                    1,
                    &[list],
                );
            }
            chars.push(x.get_char());
            xs = scm_cdr(xs);
        }

        let s: String = chars.into_iter().collect();
        thread.make_string::<false>(&s)
    } else {
        wrong_type_argument_violation(
            thread,
            "list->string",
            0,
            "list",
            *list,
            1,
            &[list],
        );
    }
}

extern "C-unwind" fn string_p(_thread: &mut Thread, obj: &mut Value) -> Value {
    Value::encode_bool_value(obj.is_string())
}

extern "C-unwind" fn string_length(_thread: &mut Thread, obj: &mut Value) -> Value {
    if !obj.is_string() {
        wrong_type_argument_violation(
            _thread,
            "string-length",
            0,
            "string",
            *obj,
            1,
            &[obj],
        )
    }
    Value::encode_int32(scm_string_str(*obj).len() as _)
}

extern "C-unwind" fn symbol_p(_thread: &mut Thread, obj: &mut Value) -> Value {
    Value::encode_bool_value(obj.is_symbol())
}

extern "C-unwind" fn make_string(thread: &mut Thread, n: &mut Value, char: &mut Value) -> Value {
    let ch = if char.is_undefined() {
        '\0'
    } else if char.is_char() {
        char.get_char()
    } else {
        wrong_type_argument_violation(thread, "make-string", 1, "character", *char, 2, &[n, char])
    };

    let _n = if n.is_int32() {
        n.get_int32()
    } else {
        wrong_type_argument_violation(thread, "make-string", 0, "fixnum", *n, 2, &[n, char])
    };

    if _n < 0 {
        invalid_argument_violation(
            thread,
            "make-string",
            "length must be non-negative",
            *n,
            0,
            2,
            &[n, char],
        );
    }

    thread.make_string::<false>(&ch.to_string().repeat(_n as usize))
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

extern "C-unwind" fn make_tuple(thread: &mut Thread, len: &mut Value) -> Value {
    if !len.is_int32() {
        wrong_type_argument_violation(thread, "make-tuple", 0, "fixnum", *len, 1, &[len]);
    }

    let length = len.get_int32();
    if length < 0 {
        invalid_argument_violation(
            thread,
            "make-tuple",
            "length must be non-negative",
            *len,
            0,
            1,
            &[len],
        );
    }

    thread.make_tuple::<false>(length as _, Value::encode_bool_value(false))
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

extern "C-unwind" fn current_millis(_thread: &mut Thread) -> Value {
    Value::encode_int32(
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_millis() as _,
    )
}

extern "C-unwind" fn integer_to_char(thread: &mut Thread, val: &mut Value) -> Value {
    if val.is_int32() {
        let int = val.get_int32();
        if let Some(ch) = char::from_u32(int as _) {
            Value::encode_char(ch)
        } else {
            invalid_argument_violation(
                thread,
                "integer->char",
                "not a valid UTF-8 character",
                *val,
                0,
                1,
                &[val],
            );
        }
    } else {
        wrong_type_argument_violation(thread, "integer->char", 0, "integer", *val, 1, &[val])
    }
}

extern "C-unwind" fn char_to_integer(thread: &mut Thread, val: &mut Value) -> Value {
    if val.is_char() {
        let ch = val.get_char();
        Value::encode_int32(ch as u32 as i32)
    } else {
        wrong_type_argument_violation(thread, "char->integer", 0, "character", *val, 1, &[val])
    }
}

extern "C-unwind" fn char_eq_p(_thread: &mut Thread, val: &mut Value, val2: &mut Value) -> Value {
    if val.is_char() && val2.is_char() {
        Value::encode_bool_value(val.get_char() == val2.get_char())
    } else {
        if !val.is_char() {
            wrong_type_argument_violation(
                _thread,
                "char=?",
                0,
                "character",
                *val,
                2,
                &[val, val2],
            )
        } else {
            wrong_type_argument_violation(
                _thread,
                "char=?",
                1,
                "character",
                *val2,
                2,
                &[val, val2],
            )
        }
    }
}

extern "C-unwind" fn procedure_p(_: &mut Thread, val: &mut Value) -> Value {
    Value::encode_bool_value(val.is_program())
}

extern "C-unwind" fn procedure_eq_p(
    thread: &mut Thread,
    val: &mut Value,
    val2: &mut Value,
) -> Value {
    if val.is_program() && val2.is_program() {
        Value::encode_bool_value(scm_program_code(*val) == scm_program_code(*val2))
    } else {
        if !val.is_program() {
            wrong_type_argument_violation(
                thread,
                "procedure=?",
                0,
                "procedure",
                *val,
                2,
                &[val, val2],
            )
        } else {
            wrong_type_argument_violation(
                thread,
                "procedure=?",
                1,
                "procedure",
                *val2,
                2,
                &[val, val2],
            )
        }
    }
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
    scm_define_subr(
        "string->bytevector",
        1,
        0,
        0,
        Subr::F1(string_to_bytevector),
    );
    scm_define_subr(
        "bytevector->string",
        1,
        0,
        0,
        Subr::F1(bytevector_to_string),
    );
    scm_define_subr("list->string", 1, 0, 0, Subr::F1(list_to_string));
    scm_define_subr("string?", 1, 0, 0, Subr::F1(string_p));
    scm_define_subr("string-length", 1, 0, 0, Subr::F1(string_length));
    scm_define_subr("symbol?", 1, 0, 0, Subr::F1(symbol_p));
    scm_define_subr("make-string", 1, 1, 0, Subr::F2(make_string));
    scm_define_subr("string-ref", 2, 0, 0, Subr::F2(string_ref));
    scm_define_subr("string-set!", 3, 0, 0, Subr::F3(string_set));
    scm_define_subr("string=?", 0, 0, 1, Subr::F1(string_eq));
    scm_define_subr("string<?", 0, 0, 1, Subr::F1(string_lt));
    scm_define_subr("string>?", 0, 0, 1, Subr::F1(string_gt));
    scm_define_subr("string<=?", 0, 0, 1, Subr::F1(string_le));
    scm_define_subr("string>=?", 0, 0, 1, Subr::F1(string_ge));
    scm_define_subr("%raise", 1, 0, 0, Subr::F1(raw_raise));
    scm_define_subr("%core-preprocess", 1, 0, 0, Subr::F1(core_preprocess));
    scm_define_subr("make-tuple", 1, 0, 0, Subr::F1(make_tuple));
    scm_define_subr("tuple", 0, 0, 1, Subr::F1(tuple));
    scm_define_subr("tuple?", 1, 0, 0, Subr::F1(tuple_pred));
    scm_define_subr("tuple-ref", 2, 0, 0, Subr::F2(tuple_ref));
    scm_define_subr("tuple-set!", 3, 0, 0, Subr::F3(tuple_set));
    scm_define_subr("tuple-length", 1, 0, 0, Subr::F1(tuple_length));
    scm_define_subr("current-millis", 0, 0, 0, Subr::F0(current_millis));
    scm_define_subr("integer->char", 1, 0, 0, Subr::F1(integer_to_char));
    scm_define_subr("char->integer", 1, 0, 0, Subr::F1(char_to_integer));
    scm_define_subr("char=?", 2, 0, 0, Subr::F2(char_eq_p));
    scm_define_subr("procedure?", 1, 0, 0, Subr::F1(procedure_p));
    scm_define_subr("procedure=?", 2, 0, 0, Subr::F2(procedure_eq_p));
    super::subr_fixnum::init();
    super::subr_hash::init();
    super::struct_::init();
    super::list::init();
    super::bytevector::init();
    super::fileio::init();
}
