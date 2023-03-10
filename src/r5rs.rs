//! List of R4RS procedure definitions

use once_cell::sync::Lazy;
use rsgc::thread::Thread;

use crate::{
    compiler::env::{environment_set, make_environment},
    error::wrong_contract,
    fun, list, number, ports, string,
    value::Value,
    vm::{intern, Trampoline, Runtime},
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
            wrong_contract::<()>("car", "pair?", 0, 1, args).into()
        }
    }
}

define_proc! {
    extern "cdr", cdr(_vm, args) 1, 1 => {
        let arg = args[0];
        if arg.pairp() {
            Trampoline::Return(arg.cdr())
        } else {
            wrong_contract::<()>("cdr", "pair?", 0, 1, args).into()
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
            wrong_contract::<()>("set-car!", "pair?", 0, 2, args).into()
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
            wrong_contract::<()>("set-cdr!", "pair?", 0, 2, args).into()
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

define_proc! {
    extern "make-parameter", make_parameter(_vm, args) 1, 2 => {
        let arg = args[0];
        let guard = if args.len() == 2 {
            args[1]
        } else {
            Value::make_false()
        };
        Trampoline::Return(Value::make_parameter(_vm.mutator(), arg, guard))
    }
}

define_proc! {
    extern "%compile", compile(vm, args) 1, 2 => {
        let env = if args.len() > 1 {
            if args[1].environmentp() {
                args[1]
            } else {
                return wrong_contract::<()>("%compile", "environment?", 1, 2, args).into()
            }
        } else {
            interaction_environment()
        };

        let code = args[0];

        let mut defs = Value::make_null();
        let desugared = crate::compiler::desugar(code, &mut defs)?;

        let cps = crate::compiler::redex::redex_transform(crate::compiler::cps::t_c(desugared, intern("|%toplevel-cont")), &mut 2);

        let toplevel_code = Value::make_list(
            vm.mutator(),
            &[
                intern("lambda"),
                Value::make_list(Thread::current(), &[intern("|%toplevel-cont")]),
                cps,
            ],
        );

        let uncovered = crate::compiler::uncover_assigned(toplevel_code);
        let assigned = crate::compiler::convert_assignments(uncovered);


        let m = crate::compiler::meaning::meaning(assigned, Value::make_null(), true);
    
        let (toplevel, lambdas) = crate::compiler::lambda_lifting::lift_lambdas(m.cdddr().cdr(), true);
        m.cdddr().set_pair_cdr(toplevel);

        let code = Runtime::get().jit.lock(true).compile(env, defs, lambdas, m);

        Trampoline::Return(code)
    }
}




pub fn initialize_r5rs_environment(env: Value) {
    environment_set(env, *NOT_NAME, *NOT_PROC);
    environment_set(env, *IS_BOOLEAN_NAME, *IS_BOOLEAN_PROC);

    environment_set(env, intern("eq?"), *crate::bool::EQ_PROC);
    environment_set(env, intern("eqv?"), *crate::bool::EQV_PROC);
    environment_set(env, intern("equal?"), *crate::bool::EQUAL_PROC);
    environment_set(env, intern("not"), *crate::bool::NOT_PROC);

    environment_set(env, *IS_PAIR_NAME, *IS_PAIR_PROC);
    environment_set(env, *CONS_NAME, *CONS_PROC);
    environment_set(env, *CAR_NAME, *CAR_PROC);
    environment_set(env, *CDR_NAME, *CDR_PROC);
    environment_set(env, *SET_CAR_NAME, *SET_CAR_PROC);
    environment_set(env, *SET_CDR_NAME, *SET_CDR_PROC);
    environment_set(env, *IS_NULL_NAME, *IS_NULL_PROC);
    environment_set(env, *GC_NAME, *GC_PROC);
    environment_set(env, *MAKE_PARAMETER_NAME, *MAKE_PARAMETER_PROC);

    environment_set(env, *COMPILE_NAME, *COMPILE_PROC);

    ports::initialize_port(env);
    number::initialize_env(env);
    list::initialize_list(env);
    fun::initialize_fun(env);
    string::initialize_string(env);
    super::vector::initialize_vector(env);
}

pub static INTERACTION_ENVIRONMENT: Lazy<Value> = Lazy::new(|| {
    let env = make_environment(intern("r5rs"));
    initialize_r5rs_environment(env);
    let param = Value::make_parameter(Thread::current(), env, Value::make_false());
    crate::vm::Runtime::get().add_global_root(param);
    param
});

pub fn interaction_environment() -> Value {
    *INTERACTION_ENVIRONMENT.parameter_value()
}

pub fn set_interaction_environment(env: Value) {
    INTERACTION_ENVIRONMENT.set_parameter_value(env);
}
