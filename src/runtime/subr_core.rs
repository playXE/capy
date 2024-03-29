use once_cell::sync::Lazy;
use std::{cell::RefCell, io::Read, rc::Rc};
use unicode_general_category::GeneralCategory;

use crate::{
    bytecode::image::load_image_from_memory,
    compiler::{
        compile_bytecode::compile_bytecode,
        expand, fix_letrec, pass2,
        sexpr::{Sexpr, SourceInfo, value_to_sexpr_with_source_loc},
        tree_il::{il_to_core_form, IForm, Lambda},
        Cenv, P,
    },
    runtime::{environment::ScmEnvironment, object::scm_bytevector_as_slice},
    vm::{scm_virtual_machine, thread::Thread},
};

use super::{
    arith::{self, scm_to_u32},
    control::{
        invalid_argument_violation, wrong_number_of_arguments_violation,
        wrong_type_argument_violation,
    },
    gsubr::{scm_define_subr, Subr},
    list::scm_length,
    object::{
        scm_car, scm_cdr, scm_program_code, scm_program_num_free_vars, scm_string_mut_str,
        scm_string_str, scm_symbol_str, scm_tuple_ref, scm_tuple_set, ScmProgram, ScmTuple,
        ScmWeakMapping, TypeId, scm_program_free_variable, scm_values_set,
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

extern "C-unwind" fn string_ends_with(
    thread: &mut Thread,
    base: &mut Value,
    suffix: &mut Value,
) -> Value {
    if !base.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "string-ends-with?",
            0,
            "string",
            *base,
            2,
            &[base, suffix],
        )
    }

    if !suffix.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "string-ends-with?",
            1,
            "string",
            *suffix,
            2,
            &[base, suffix],
        )
    }

    let base = scm_string_str(*base);
    let suffix = scm_string_str(*suffix);

    Value::encode_bool_value(base.ends_with(suffix))
}

extern "C-unwind" fn string_append(thread: &mut Thread, rest: &mut Value) -> Value {
    let mut result = String::new();
    let mut pos = 0;
    let length = scm_length(*rest).unwrap();
    while !rest.is_null() {
        let mut s = scm_car(*rest);
        if !s.is_string() {
            wrong_type_argument_violation::<1>(
                thread,
                "string-append",
                pos,
                "string",
                s,
                length,
                &[&mut s, rest],
            );
        }
        result.push_str(scm_string_str(s));
        *rest = scm_cdr(*rest);
        pos += 1;
    }

    thread.make_string::<false>(&result)
}

extern "C-unwind" fn symbol_to_string(thread: &mut Thread, symbol: &mut Value) -> Value {
    if !symbol.is_symbol() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "symbol->string",
            0,
            "symbol",
            *symbol,
            1,
            &[symbol],
        );
    }
    thread.make_string::<false>(scm_symbol_str(*symbol))
}

extern "C-unwind" fn string_to_symbol(thread: &mut Thread, string: &mut Value) -> Value {
    if !string.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "string->symbol",
            0,
            "string",
            *string,
            1,
            &[string],
        )
    }
    scm_intern(scm_string_str(*string))
}

extern "C-unwind" fn substring(
    thread: &mut Thread,
    string: &mut Value,
    start: &mut Value,
    end: &mut Value,
) -> Value {
    if !string.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "substring",
            0,
            "string",
            *string,
            2,
            &[string, start],
        )
    }

    let length = scm_string_str(*string).chars().count() as u32;

    let start_index = if start.is_int32() && start.get_int32() >= 0 {
        start.get_int32() as u32
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "substring",
            1,
            "nonnegative fixnum",
            *start,
            2,
            &[string, start],
        )
    };

    let end_index = if end.is_undefined() {
        length
    } else if end.is_int32() && end.get_int32() >= 0 {
        end.get_int32() as u32
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "substring",
            2,
            "nonnegative fixnum",
            *end,
            2,
            &[string, start],
        )
    };

    if end_index > length {
        invalid_argument_violation::<{usize::MAX}>(
            thread,
            "substring",
            "end out of bounds",
            *end,
            2,
            2,
            &[string, start, end],
        )
    }

    if start_index > end_index {
        invalid_argument_violation::<{usize::MAX}>(
            thread,
            "substring",
            "start is greater than end",
            *start,
            1,
            2,
            &[string, start, end],
        )
    }

    let s = scm_string_str(*string);
    let s = s
        .chars()
        .skip(start_index as usize)
        .take(end_index as usize - start_index as usize)
        .collect::<String>();

    thread.make_string::<false>(&s)
}

extern "C-unwind" fn string_to_bytevector(thread: &mut Thread, string: &mut Value) -> Value {
    if !string.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
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
        wrong_type_argument_violation::<{ usize::MAX }>(
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
            invalid_argument_violation::<{usize::MAX}>(
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
                wrong_type_argument_violation::<{ usize::MAX }>(
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
        wrong_type_argument_violation::<{ usize::MAX }>(
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
        wrong_type_argument_violation::<{ usize::MAX }>(
            _thread,
            "string-length",
            0,
            "string",
            *obj,
            1,
            &[obj],
        )
    }
    Value::encode_int32(scm_string_str(*obj).chars().count() as _)
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
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "make-string",
            1,
            "character",
            *char,
            2,
            &[n, char],
        )
    };

    let _n = if n.is_int32() {
        n.get_int32()
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "make-string",
            0,
            "fixnum",
            *n,
            2,
            &[n, char],
        )
    };

    if _n < 0 {
        invalid_argument_violation::<{usize::MAX}>(
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
        wrong_type_argument_violation::<{ usize::MAX }>(
            _thread,
            "string-ref",
            0,
            "string",
            *obj,
            2,
            &[obj, index],
        )
    }

    if !arith::exact_nonnegative_integer_p(*index) {
        wrong_type_argument_violation::<{ usize::MAX }>(
            _thread,
            "string-ref",
            1,
            "exact nonnegative integer",
            *index,
            2,
            &[obj, index],
        )
    }

    let idx = arith::exact_integer_to_uint32(*index).unwrap_or_else(|| {
        invalid_argument_violation::<{usize::MAX}>(
            _thread,
            "string-ref",
            "32 bit integer expred",
            *index,
            1,
            2,
            &[obj, index],
        )
    }) as usize;
    let s = scm_string_str(*obj);
    if idx >= s.chars().count() {
        invalid_argument_violation::<{usize::MAX}>(
            _thread,
            "string-ref",
            "index out of bounds",
            *index,
            1,
            2,
            &[obj, index],
        );
    }
    Value::encode_char(s.chars().nth(idx as usize).unwrap())
}

extern "C-unwind" fn string_set(
    _thread: &mut Thread,
    obj: &mut Value,
    index: &mut Value,
    ch: &mut Value,
) -> Value {
    if !obj.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            _thread,
            "string-set!",
            0,
            "string",
            *obj,
            3,
            &[obj, index, ch],
        )
    }
    if !ch.is_char() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            _thread,
            "string-set!",
            2,
            "character",
            *ch,
            3,
            &[obj, index, ch],
        )
    }

    if !(arith::exact_nonnegative_integer_p(*index) && index.is_int32()) {
        wrong_type_argument_violation::<{ usize::MAX }>(
            _thread,
            "string-set!",
            1,
            "exact nonnegative fixnum",
            *index,
            3,
            &[obj, index, ch],
        )
    }

    let char = ch.get_char();
    let idx = index.get_int32() as usize;
    let s = scm_string_str(*obj);
    if idx >= s.len() {
        invalid_argument_violation::<{usize::MAX}>(
            _thread,
            "string-set!",
            "index out of bounds",
            *index,
            1,
            3,
            &[obj, index, ch],
        );
    }

    // find range of bytes to replace
    let mut start = 0;
    let mut end = 0;
    for (i, ch) in s.chars().enumerate() {
        if i == idx as usize {
            start = end;
        }
        end += ch.len_utf8();
        if i == idx as usize {
            break;
        }
    }

    if end >= s.len() {
        invalid_argument_violation::<{usize::MAX}>(
            _thread,
            "string-set!",
            "index out of bounds",
            *index,
            1,
            3,
            &[obj, index, ch],
        );
    } else {
        let s = scm_string_mut_str(*obj);
        unsafe {
            s.as_bytes_mut()[start..end].copy_from_slice(char.encode_utf8(&mut [0; 4]).as_bytes());
        }
        Value::encode_undefined_value()
    }
}

extern "C-unwind" fn string_eq(thread: &mut Thread, rest: &mut Value) -> Value {
    let len = scm_length(*rest).unwrap();

    if len < 2 {
        wrong_number_of_arguments_violation::<0>(thread, "string=?", 2, -1, len, &[rest]);
    }

    let mut first = scm_car(*rest);
    if !first.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "string=?",
            0,
            "string",
            first,
            len,
            &[&mut first, rest],
        );
    }

    *rest = scm_cdr(*rest);
    let mut position = 0;
    while rest.is_pair() {
        let s = scm_car(*rest);
        if !s.is_string() {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "string=?",
                position + 1,
                "string",
                s,
                len,
                &[&mut first, rest],
            );
        }
        position += 1;
        if scm_string_str(first) != scm_string_str(s) {
            return Value::encode_bool_value(false);
        }
        *rest = scm_cdr(*rest);
    }

    Value::encode_bool_value(true)
}

extern "C-unwind" fn string_lt(thread: &mut Thread, rest: &mut Value) -> Value {
    let len = scm_length(*rest).unwrap();

    if len < 2 {
        wrong_number_of_arguments_violation::<0>(thread, "string<?", 2, -1, len, &[rest]);
    }

    let mut first = scm_car(*rest);
    if !first.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "string<?",
            0,
            "string",
            first,
            len,
            &[&mut first, rest],
        );
    }

    *rest = scm_cdr(*rest);
    let mut position = 0;
    while rest.is_pair() {
        let s = scm_car(*rest);
        if !s.is_string() {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "string<?",
                position + 1,
                "string",
                s,
                len,
                &[&mut first, rest],
            );
        }
        position += 1;
        if scm_string_str(first) >= scm_string_str(s) {
            return Value::encode_bool_value(false);
        }
        *rest = scm_cdr(*rest);
    }

    Value::encode_bool_value(true)
}

extern "C-unwind" fn string_gt(thread: &mut Thread, rest: &mut Value) -> Value {
    let len = scm_length(*rest).unwrap();

    if len < 2 {
        wrong_number_of_arguments_violation::<0>(thread, "string>?", 2, -1, len, &[rest]);
    }

    let mut first = scm_car(*rest);
    if !first.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "string>?",
            0,
            "string",
            first,
            2,
            &[&mut first, rest],
        );
    }

    *rest = scm_cdr(*rest);
    let mut position = 0;
    while rest.is_pair() {
        let s = scm_car(*rest);
        if !s.is_string() {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "string>?",
                position + 1,
                "string",
                s,
                2,
                &[&mut first, rest],
            );
        }
        position += 1;
        if scm_string_str(first) <= scm_string_str(s) {
            return Value::encode_bool_value(false);
        }
        *rest = scm_cdr(*rest);
    }

    Value::encode_bool_value(true)
}

extern "C-unwind" fn string_le(thread: &mut Thread, rest: &mut Value) -> Value {
    let len = scm_length(*rest).unwrap();

    if len < 2 {
        wrong_number_of_arguments_violation::<0>(thread, "string<=?", 2, -1, len, &[rest]);
    }

    let mut first = scm_car(*rest);
    if !first.is_string() {
        wrong_type_argument_violation::<1>(
            thread,
            "string<=?",
            0,
            "string",
            first,
            2,
            &[&mut first, rest],
        );
    }

    *rest = scm_cdr(*rest);
    let mut position = 0;
    while rest.is_pair() {
        let s = scm_car(*rest);
        if !s.is_string() {
            wrong_type_argument_violation::<1>(
                thread,
                "string<=?",
                position + 1,
                "string",
                s,
                2,
                &[&mut first, rest],
            );
        }
        position += 1;
        if scm_string_str(first) > scm_string_str(s) {
            return Value::encode_bool_value(false);
        }
        *rest = scm_cdr(*rest);
    }

    Value::encode_bool_value(true)
}

extern "C-unwind" fn string_ge(thread: &mut Thread, rest: &mut Value) -> Value {
    let len = scm_length(*rest).unwrap();

    if len < 2 {
        wrong_number_of_arguments_violation::<0>(thread, "string>=?", 2, -1, len, &[rest]);
    }

    let mut first = scm_car(*rest);
    if !first.is_string() {
        wrong_type_argument_violation::<1>(
            thread,
            "string>=?",
            0,
            "string",
            first,
            2,
            &[&mut first, rest],
        );
    }

    *rest = scm_cdr(*rest);
    let mut position = 0;
    while rest.is_pair() {
        let s = scm_car(*rest);
        if !s.is_string() {
            wrong_type_argument_violation::<1>(
                thread,
                "string>=?",
                position + 1,
                "string",
                s,
                2,
                &[&mut first, rest],
            );
        }
        position += 1;
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
    let mut source_info = SourceInfo::new();
    let Some(sexpr) = value_to_sexpr_with_source_loc(*code, &mut source_info) else {
        invalid_argument_violation::<{usize::MAX}>(
            thread,
            "%core-preprocess",
            "invalid source code",
            *code,
            0,
            1,
            &[code],
        )
    };
    let synenv = scm_virtual_machine()
        .interaction_environment
        .cast_as::<ScmEnvironment>()
        .syntactic_environment
        .lock(true);

    let cenv = Cenv {
        frames: Sexpr::Null,
        source_loc: Rc::new(RefCell::new(source_info)),
        syntax_env: synenv.clone(),
        expr_name: Sexpr::Null,
    };
    let il = match expand::pass1(&sexpr, &cenv) {
        Ok(il) => il,
        Err(_) => {
            drop(synenv);
            invalid_argument_violation::<{usize::MAX}>(
                thread,
                "%core-preprocess",
                "failed to convet Scheme to Tree-IL",
                *code,
                0,
                1,
                &[code],
            )
        }
    };
    let fixed = fix_letrec::pass_fix_letrec(il);

    il_to_core_form(thread, &fixed)
}

extern "C-unwind" fn core_compile(thread: &mut Thread, sexpr: &mut Value) -> Value {
    let mut source_info = SourceInfo::new();
    let Some(sexpr_) = value_to_sexpr_with_source_loc(*sexpr, &mut source_info) else {
        invalid_argument_violation::<{usize::MAX}>(
            thread,
            "%core-compile",
            "invalid source code",
            *sexpr,
            0,
            1,
            &[sexpr],
        )
    };
    let synenv = scm_virtual_machine()
        .interaction_environment
        .cast_as::<ScmEnvironment>()
        .syntactic_environment
        .lock(true);

    let cenv = Cenv {
        frames: Sexpr::Null,
        source_loc: Rc::new(RefCell::new(source_info)),
        expr_name: Sexpr::Null,
        syntax_env: synenv.clone(),
    };
    let il = match expand::pass1(&sexpr_, &cenv) {
        Ok(il) => il,
        Err(_) => {
            drop(synenv);
            invalid_argument_violation::<{usize::MAX}>(
                thread,
                "%core-compile",
                "failed to convet Scheme to Tree-IL",
                *sexpr,
                0,
                1,
                &[sexpr],
            )
        }
    };
    let fixed = fix_letrec::pass_fix_letrec(il);
    let opt = pass2::pass2(fixed, true).unwrap();
    let lam = P(Lambda {
        src: None,
        name: None,
        reqargs: 0,
        optarg: false,
        lvars: vec![],
        body: opt,
        flag: crate::compiler::tree_il::LambdaFlag::None,
        calls: vec![],
        free_lvars: vec![],
        bound_lvars: Default::default(),
        defs: Default::default(),
        lifted_var: crate::compiler::tree_il::LiftedVar::Candidate,
    });
    let mut buf = vec![];
    compile_bytecode(P(IForm::Lambda(lam)), &mut buf);

    thread.make_bytevector_from_slice::<false>(&buf)
}

extern "C-unwind" fn load_from_byteimage(thread: &mut Thread, filename: &mut Value) -> Value {
    if !filename.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "load-from-byteimage",
            0,
            "string",
            *filename,
            1,
            &[filename],
        );
    }

    let path = std::path::Path::new(scm_string_str(*filename));

    if !path.exists() {
        invalid_argument_violation::<{usize::MAX}>(
            thread,
            "load-from-byteimage",
            "file not found",
            *filename,
            0,
            1,
            &[filename],
        );
    }

    if path.is_dir() {
        invalid_argument_violation::<{usize::MAX}>(
            thread,
            "load-from-byteimage",
            "file is a directory",
            *filename,
            0,
            1,
            &[filename],
        );
    }

    let mut file = match std::fs::File::open(path) {
        Ok(file) => file,
        Err(_) => {
            invalid_argument_violation::<{usize::MAX}>(
                thread,
                "load-from-byteimage",
                "failed to open file",
                *filename,
                0,
                1,
                &[filename],
            );
        }
    };

    let mut memory = Vec::new();
    match file.read_to_end(&mut memory) {
        Ok(_) => {}
        Err(_) => {
            invalid_argument_violation::<{usize::MAX}>(
                thread,
                "load-from-byteimage",
                "failed to read file",
                *filename,
                0,
                1,
                &[filename],
            );
        }
    }

    match load_image_from_memory(&memory, None) {
        Ok(image) => image.entry_program,
        Err(_) => {
            invalid_argument_violation::<{usize::MAX}>(
                thread,
                "load-from-byteimage",
                "failed to load image",
                *filename,
                0,
                1,
                &[filename],
            );
        }
    }
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
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "make-tuple",
            0,
            "fixnum",
            *len,
            1,
            &[len],
        );
    }

    let length = len.get_int32();
    if length < 0 {
        invalid_argument_violation::<{usize::MAX}>(
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

extern "C-unwind" fn tuple_ref(_thread: &mut Thread, obj: &mut Value, index: &mut Value) -> Value {
    if !obj.is_tuple() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            _thread,
            "tuple-ref",
            0,
            "tuple",
            *obj,
            2,
            &[obj, index],
        )
    }
    let idx = scm_to_u32(*index) as usize;
    let len = obj.cast_as::<ScmTuple>().length;
    if idx >= len {
        invalid_argument_violation::<{usize::MAX}>(
            _thread,
            "tuple-ref",
            "index out of bounds",
            *index,
            1,
            2,
            &[obj, index],
        );
    }
    scm_tuple_ref(*obj, idx as _)
}

extern "C-unwind" fn tuple_set(
    thread: &mut Thread,
    obj: &mut Value,
    index: &mut Value,
    value: &mut Value,
) -> Value {
    if !obj.is_tuple() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "tuple-set!",
            0,
            "tuple",
            *obj,
            3,
            &[obj, index, value],
        )
    }
    let idx = scm_to_u32(*index) as usize;
    let len = obj.cast_as::<ScmTuple>().length;
    if idx >= len {
        invalid_argument_violation::<{usize::MAX}>(
            thread,
            "tuple-set!",
            "index out of bounds",
            *index,
            1,
            3,
            &[obj, index, value],
        );
    }
    scm_tuple_set(*obj, thread, idx as _, *value);
    *value
}

extern "C-unwind" fn tuple_length(_thread: &mut Thread, obj: &mut Value) -> Value {
    if !obj.is_tuple() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            _thread,
            "tuple-length",
            0,
            "tuple",
            *obj,
            1,
            &[obj],
        )
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
            invalid_argument_violation::<{usize::MAX}>(
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
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "integer->char",
            0,
            "integer",
            *val,
            1,
            &[val],
        )
    }
}

extern "C-unwind" fn char_to_integer(thread: &mut Thread, val: &mut Value) -> Value {
    if val.is_char() {
        let ch = val.get_char();
        Value::encode_int32(ch as u32 as i32)
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "char->integer",
            0,
            "character",
            *val,
            1,
            &[val],
        )
    }
}

extern "C-unwind" fn char_whitespace_p(thread: &mut Thread, val: &mut Value) -> Value {
    if val.is_char() {
        return Value::encode_bool_value(val.get_char().is_whitespace());
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "char-whitespace?",
            0,
            "char",
            *val,
            1,
            &[val],
        )
    }
}

extern "C-unwind" fn char_numeric_p(thread: &mut Thread, val: &mut Value) -> Value {
    if val.is_char() {
        return Value::encode_bool_value(val.get_char().is_numeric());
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "char-numeric?",
            0,
            "char",
            *val,
            1,
            &[val],
        )
    }
}

static CATEGORY_MAP: Lazy<[Value; GeneralCategory::UppercaseLetter as usize + 1]> =
    Lazy::new(|| {
        [
            scm_intern("Pe"),
            scm_intern("Pc"),
            scm_intern("Cc"),
            scm_intern("Sc"),
            scm_intern("Pd"),
            scm_intern("Nd"),
            scm_intern("Me"),
            scm_intern("Pf"),
            scm_intern("Cf"),
            scm_intern("Pi"),
            scm_intern("Nl"),
            scm_intern("Zl"),
            scm_intern("Ll"),
            scm_intern("Sm"),
            scm_intern("Lm"),
            scm_intern("Sm"),
            scm_intern("Mn"),
            scm_intern("Ps"),
            scm_intern("Lo"),
            scm_intern("No"),
            scm_intern("Po"),
            scm_intern("So"),
            scm_intern("Zp"),
            scm_intern("Co"),
            scm_intern("Zs"),
            scm_intern("Mc"),
            scm_intern("Cs"),
            scm_intern("Lt"),
            scm_intern("unassigned"),
            scm_intern("Lu"),
        ]
    });

extern "C-unwind" fn char_general_category(thread: &mut Thread, char: &mut Value) -> Value {
    if char.is_char() {
        let ch = char.get_char();
        let category = unicode_general_category::get_general_category(ch);
        CATEGORY_MAP[category as usize]
    } else {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "char-general-category",
            0,
            "character",
            *char,
            1,
            &[char],
        )
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
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "procedure=?",
                0,
                "procedure",
                *val,
                2,
                &[val, val2],
            )
        } else {
            wrong_type_argument_violation::<{ usize::MAX }>(
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

extern "C-unwind" fn program_ref(thread: &mut Thread, proc: &mut Value, ix: &mut Value) -> Value {
    if !proc.is_program() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "program-ref",
            0,
            "procedure",
            *proc,
            2,
            &[proc, ix],
        );
    }

    if !(ix.is_int32() && ix.get_int32() >= 0) {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "program-ref",
            1,
            "exact nonnegative fixnum",
            *ix,
            2,
            &[proc, ix],
        );
    }

    let idx = ix.get_int32() as usize;

    let val = scm_program_free_variable(*proc, idx as _);

    val
}

extern "C-unwind" fn program_num_free_vars(thread: &mut Thread, proc: &mut Value) -> Value {
    if !proc.is_program() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "program-num-free-vars",
            0,
            "procedure",
            *proc,
            1,
            &[proc],
        );
    }

    Value::encode_int32(scm_program_num_free_vars(*proc) as _)
}

extern "C-unwind" fn procedure_to_string(thread: &mut Thread, proc: &mut Value) -> Value {
    if !proc.is_program() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "procedure->string",
            0,
            "procedure",
            *proc,
            1,
            &[proc],
        );
    }

    if let Some(name) = scm_virtual_machine()
        .images
        .program_name(scm_program_code(*proc))
    {
        if scm_program_num_free_vars(*proc) == 0 {
            thread.make_string::<false>(&format!(
                "#<procedure '{}' at {:p}>",
                name,
                proc.get_object().cast_as::<ScmProgram>()
            ))
        } else {
            thread.make_string::<false>(&format!(
                "#<closure '{}' at {:p}>",
                name,
                proc.get_object().cast_as::<ScmProgram>()
            ))
        }
    } else {
        if scm_program_num_free_vars(*proc) == 0 {
            thread.make_string::<false>(&format!(
                "#<procedure {:p}>",
                proc.get_object().cast_as::<ScmProgram>()
            ))
        } else {
            thread.make_string::<false>(&format!(
                "#<closure {:p}>",
                proc.get_object().cast_as::<ScmProgram>()
            ))
        }
    }
}

extern "C-unwind" fn make_condition_uid(_thread: &mut Thread) -> Value {
    let cuid = cuid2::create_id();
    let sym = scm_intern(format!("<{}>", cuid));

    sym
}

extern "C-unwind" fn make_cuid2(_thread: &mut Thread) -> Value {
    let cuid = cuid2::create_id();
    _thread.make_string::<false>(&cuid)
}

extern "C-unwind" fn capture_stacktrace(thread: &mut Thread) -> Value {
    unsafe { super::error::capture_stacktrace(thread) }
}

extern "C-unwind" fn subr_symbol_hash(thread: &mut Thread, symbol: &mut Value) -> Value {
    if !symbol.is_symbol() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "symbol-hash",
            0,
            "symbol",
            *symbol,
            1,
            &[symbol],
        );
    }

    let hash = super::hashtable::address_hash1(symbol.get_raw() as _, i32::MAX as _);

    Value::encode_int32(hash as _)
}

#[allow(dead_code)]
extern "C-unwind" fn subr_values(thread: &mut Thread, args: &mut Value) -> Value {
    let n = scm_length(*args).unwrap();

    if n == 1 {
        return scm_car(*args);
    } else {
        let result = thread.make_values::<false>(n, Value::encode_null_value());

        for i in 0..n {
            scm_values_set(result, thread, i as _, scm_car(*args));
            *args = scm_cdr(*args);
        }

        result
    }
}

pub(crate) fn init() {
    //scm_define_subr("values", 0, 0, 1, Subr::F1(subr_values));
    scm_define_subr("%core-compile", 1, 0, 0, Subr::F1(core_compile));
    scm_define_subr("symbol-hash", 1, 0, 0, Subr::F1(subr_symbol_hash));
    scm_define_subr("%capture-stacktrace", 0, 0, 0, Subr::F0(capture_stacktrace));
    scm_define_subr("make-condition-uid", 0, 0, 0, Subr::F0(make_condition_uid));
    scm_define_subr("make-cuid2", 0, 0, 0, Subr::F0(make_cuid2));
    scm_define_subr(".procedure->string", 1, 0, 0, Subr::F1(procedure_to_string));
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
    scm_define_subr("substring", 2, 1, 0, Subr::F3(substring));
    scm_define_subr("string=?", 0, 0, 1, Subr::F1(string_eq));
    scm_define_subr("string<?", 0, 0, 1, Subr::F1(string_lt));
    scm_define_subr("string>?", 0, 0, 1, Subr::F1(string_gt));
    scm_define_subr("string<=?", 0, 0, 1, Subr::F1(string_le));
    scm_define_subr("string>=?", 0, 0, 1, Subr::F1(string_ge));
    scm_define_subr("string-ends-with?", 2, 0, 0, Subr::F2(string_ends_with));
    scm_define_subr("%raise", 1, 0, 0, Subr::F1(raw_raise));
    scm_define_subr("%core-preprocess", 1, 0, 0, Subr::F1(core_preprocess));
    scm_define_subr(
        "load-thunk-from-file",
        1,
        0,
        0,
        Subr::F1(load_from_byteimage),
    );
    scm_define_subr("make-tuple", 1, 0, 0, Subr::F1(make_tuple));
    scm_define_subr("tuple", 0, 0, 1, Subr::F1(tuple));
    scm_define_subr("tuple?", 1, 0, 0, Subr::F1(tuple_pred));
    scm_define_subr("tuple-ref", 2, 0, 0, Subr::F2(tuple_ref));
    scm_define_subr("tuple-set!", 3, 0, 0, Subr::F3(tuple_set));
    scm_define_subr("tuple-length", 1, 0, 0, Subr::F1(tuple_length));
    scm_define_subr("current-millis", 0, 0, 0, Subr::F0(current_millis));
    scm_define_subr("integer->char", 1, 0, 0, Subr::F1(integer_to_char));
    scm_define_subr("char->integer", 1, 0, 0, Subr::F1(char_to_integer));
    scm_define_subr("char-whitespace?", 1, 0, 0, Subr::F1(char_whitespace_p));
    scm_define_subr("char-numeric?", 1, 0, 0, Subr::F1(char_numeric_p));
    scm_define_subr(
        "char-general-category",
        1,
        0,
        0,
        Subr::F1(char_general_category),
    );
    scm_define_subr("procedure?", 1, 0, 0, Subr::F1(procedure_p));
    scm_define_subr("procedure=?", 2, 0, 0, Subr::F2(procedure_eq_p));
    scm_define_subr("program-ref", 2, 0, 0, Subr::F2(program_ref));
    scm_define_subr("program-num-free-vars", 1, 0, 0, Subr::F1(program_num_free_vars));
    super::env::init();
    super::subr_fixnum::init();
    super::subr_hash::init();
    super::list::init();
    super::bytevector::init();
    super::fileio::init();
    super::subr_arith::init_arith();
    super::load::init_load();
}
