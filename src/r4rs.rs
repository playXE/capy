//! List of R4RS procedure definitions


use crate::{
    compiler::env::environment_set,
    value::Value,
    vm::{intern, Trampoline}, number, error::wrong_contract, list, fun
};

define_proc! {
    extern "not", not(_vm, args) 1, 1 => {
        let arg = args[0];
        if arg.falsep() {
            Trampoline::Return(Value::make_true())
        } else {
            Trampoline::Return(Value::make_false())
        }
    }
}

define_proc! {
    extern "boolean?", is_boolean(_vm, args) 1, 1 => {
        let arg = args[0];
        if arg.truep() || arg.falsep() {
            Trampoline::Return(Value::make_true())
        } else {
            Trampoline::Return(Value::make_false())
        }
    }
}

define_proc! {
    extern "pair?", is_pair(_vm, args) 1, 1 => {
        let arg = args[0];
        if arg.pairp() {
            Trampoline::Return(Value::make_true())
        } else {
            Trampoline::Return(Value::make_false())
        }
    }
}

define_proc! {
    extern "cons", cons(_vm, args) 2, 2 => {
        let arg1 = args[0];
        let arg2 = args[1];
        Trampoline::Return(Value::make_cons(arg1, arg2))
    }
}

define_proc! {
    extern "car", car(_vm, args) 1, 1 => {
        let arg = args[0];
        if arg.pairp() {
            Trampoline::Return(arg.car())
        } else {
            wrong_contract("car", "pair?", 0, 1, args).into()
        }
    }
}

define_proc! {
    extern "cdr", cdr(_vm, args) 1, 1 => {
        let arg = args[0];
        if arg.pairp() {
            Trampoline::Return(arg.cdr())
        } else {
            wrong_contract("cdr", "pair?", 0, 1, args).into()
        }
    }
}

define_proc! {
    extern "set-car!", set_car(vm, args) 2, 2 => {
        let arg1 = args[0];
        let arg2 = args[1];
        if arg1.pairp() {
            vm.mutator().write_barrier(arg1.handle());
            arg1.set_pair_car(arg2);
            Trampoline::Return(Value::make_void())
        } else {
            wrong_contract("set-car!", "pair?", 0, 2, args).into()
        }
    }
}

define_proc! {
    extern "set-cdr!", set_cdr(vm, args) 2, 2 => {
        let arg1 = args[0];
        let arg2 = args[1];
        if arg1.pairp() {
            vm.mutator().write_barrier(arg1.handle());
            arg1.set_pair_cdr(arg2);
            Trampoline::Return(Value::make_void())
        } else {
            wrong_contract("set-cdr!", "pair?", 0, 2, args).into()
        }
    }
}

define_proc! {
    extern "null?", is_null(_vm, args) 1, 1 => {
        let arg = args[0];
        if arg.nullp() {
            Trampoline::Return(Value::make_true())
        } else {
            Trampoline::Return(Value::make_false())
        }
    }
}


define_proc! {
    extern "gc", gc(_vm, _args) 0, 0 => {
        rsgc::heap::heap::heap().request_gc();
        Trampoline::Return(Value::make_void())
    }
}

pub fn initialize_r4rs_environment(env: Value) {
    environment_set(env, *NOT_NAME, *NOT_PROC);
    environment_set(env, *IS_BOOLEAN_NAME, *IS_BOOLEAN_PROC);

    environment_set(env, intern("eq?"), *crate::bool::EQ_PROC);
    environment_set(env, intern("eqv?"), *crate::bool::EQV_PROC);
    environment_set(env, intern("equal?"), *crate::bool::EQUAL_PROC);

    environment_set(env, *IS_PAIR_NAME, *IS_PAIR_PROC);
    environment_set(env, *CONS_NAME, *CONS_PROC);
    environment_set(env, *CAR_NAME, *CAR_PROC);
    environment_set(env, *CDR_NAME, *CDR_PROC);
    environment_set(env, *SET_CAR_NAME, *SET_CAR_PROC);
    environment_set(env, *SET_CDR_NAME, *SET_CDR_PROC);
    environment_set(env, *IS_NULL_NAME, *IS_NULL_PROC);
    environment_set(env, *GC_NAME, *GC_PROC);

    number::initialize_env(env);
    list::initialize_list(env);
    fun::initialize_fun(env);
}
