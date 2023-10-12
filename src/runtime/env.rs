use crate::{gc_protect, vm::thread::Thread};

use super::{
    control::wrong_type_argument_violation,
    object::{scm_string_str, scm_vector_set},
    value::Value, gsubr::{scm_define_subr, Subr},
};

extern "C-unwind" fn command_line(thread: &mut Thread) -> Value {
    let args = std::env::args().collect::<Vec<_>>();
    let mut res = thread.make_vector::<false>(args.len(), Value::encode_undefined_value());

    for (i, arg) in args.iter().enumerate() {
        let s = gc_protect!(thread => res => thread.make_string::<false>(arg));
        scm_vector_set(res, thread, i as _, s);
    }

    res
}

extern "C-unwind" fn getenv(thread: &mut Thread, name: &mut Value) -> Value {
    if !name.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "getenv",
            0,
            "string",
            *name,
            1,
            &[name],
        )
    }

    let name = scm_string_str(*name);
    match std::env::var(name) {
        Ok(s) => thread.make_string::<false>(&s),
        Err(_) => Value::encode_bool_value(false),
    }
}

extern "C-unwind" fn setenv(thread: &mut Thread, name: &mut Value, value: &mut Value) -> Value {
    if !name.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "setenv",
            0,
            "string",
            *name,
            2,
            &[name, value],
        )
    }

    if value.is_false() {
        std::env::remove_var(scm_string_str(*name));
    } else {
        if !value.is_string() {
            wrong_type_argument_violation::<{ usize::MAX }>(
                thread,
                "setenv",
                1,
                "string",
                *value,
                2,
                &[name, value],
            )
        }

        let name = scm_string_str(*name);
        let value = scm_string_str(*value);
        std::env::set_var(name, value);
    }
    Value::encode_undefined_value()
}

pub(crate) fn init() {
    scm_define_subr("command-line", 0, 0, 0, Subr::F0(command_line));
    scm_define_subr("getenv", 1, 0, 0, Subr::F1(getenv));
    scm_define_subr("setenv", 2, 0, 0, Subr::F2(setenv));
    
}