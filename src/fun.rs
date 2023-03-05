use rsgc::system::arraylist::ArrayList;

use crate::{
    error::wrong_contract,
    value::{Type, Value, SCHEME_MAX_ARGS}, compiler::env::environment_set, vm::{intern, Trampoline},
};

pub fn get_arity(val: Value) -> Option<(i32, i32)> {
    if val.get_type() == Type::ReturnCont {
        Some((1, 1))
    } else if val.get_type() == Type::NativeProcedure {
        let mina = *val.nativeprocedure_mina();
        let maxa = *val.nativeprocedure_maxa();

        Some((mina, if maxa == SCHEME_MAX_ARGS { -1 } else { maxa }))
    } else if val.get_type() == Type::PrimitiveProcedure {
        let mina = val.downcast_primitive_proc().mina;
        let maxa = val.downcast_primitive_proc().maxa;

        Some((mina, if maxa == SCHEME_MAX_ARGS { -1 } else { maxa }))
    } else if val.get_type() == Type::ClosedPrimitiveProcedure {
        let mina = val.downcast_closed_primitive_proc().mina;
        let maxa = val.downcast_closed_primitive_proc().maxa;

        Some((mina, if maxa == SCHEME_MAX_ARGS { -1 } else { maxa }))
    } else {
        None
    }
}

pub fn get_proc_name<'a>(val: Value) -> Option<&'a str> {
    if val.get_type() == Type::ReturnCont {
        Some("#<return-continuation>")
    } else if val.get_type() == Type::NativeProcedure {
        None
    } else if val.get_type() == Type::PrimitiveProcedure {
        Some(val.downcast_primitive_proc().name.strsym())
    } else if val.get_type() == Type::ClosedPrimitiveProcedure {
        Some(val.downcast_closed_primitive_proc().name.strsym())
    } else {
        None
    }
}

pub fn check_arity(p: Value, a: i32, _inc_ok: bool) -> bool {
    let mina;
    let mut maxa;

    if p.primitive_procedurep() {
        mina = p.downcast_primitive_proc().mina;
        maxa = p.downcast_primitive_proc().maxa;
    } else if p.closed_primitive_procedurep() {
        mina = p.downcast_closed_primitive_proc().mina;
        maxa = p.downcast_closed_primitive_proc().maxa;
    } else if p.nativeprocedurep() {
        mina = *p.nativeprocedure_mina();
        maxa = *p.nativeprocedure_maxa();
    } else {
        return false;
    }

    if maxa == SCHEME_MAX_ARGS {
        maxa = -1;
    }

    if a < mina || (maxa >= 0 && a > maxa) {
        return false;
    }

    true
}

pub fn check_proc_arity_2(
    where_: &str,
    a: i32,
    which: i32,
    argc: i32,
    args: &[Value],
    false_ok: bool,
) -> Result<bool, Value> {
    let p = if which < 0 {
        args[0]
    } else {
        args[which as usize]
    };

    if false_ok && p.falsep() {
        return Ok(true);
    }

    if !p.procedurep() || !check_arity(p, a, false) {
        if where_.len() != 0 {
            let pre;
            let post;

            if false_ok {
                pre = "(or/c ";
                post = " #f)"
            } else {
                pre = "";
                post = ""
            }

            let expected = match a {
                0 => format!("{}(-> any){}", pre, post),
                1 => format!("{}(any/c . -> . any){}", pre, post),
                2 => format!("{}(any/c any/c . -> . any){}", pre, post),
                3 => format!("{}(any/c any/c any/c . -> . any){}", pre, post),
                _ => {
                    format!("{}(procedure-arity-includes/c {}){}", pre, a, post)
                }
            };

            return wrong_contract(where_, &expected, which, argc, args);
        } else {
            return Ok(false);
        }
    }

    Ok(true)
}

pub fn check_proc_arity(
    where_: &str,
    a: i32,
    which: i32,
    argc: i32,
    args: &[Value],
) -> Result<bool, Value> {
    check_proc_arity_2(where_, a, which, argc, args, false)
}



// (apply proc v ... lst)
// Applies proc using the content of (list* v ... lst) as the (by-position) arguments. 
define_proc! {
    extern "apply", apply(vm, k, args) 1, -1 => {
        let proc = args[0];
       

        let mut last = args[args.len() - 1];
        if args.len() == 1 {
            vm.tail_apply_cont(k, proc, &[])
        } else if args.len() == 2 {
            if !last.is_list() {
                match wrong_contract::<()>("apply", "list?", 1, 2, args) {
                    Err(e) => return Trampoline::Throw(e),
                    _ => unreachable!()
                }
            }

            let mut rands = ArrayList::with_capacity(vm.mutator(), last.list_length());

            while last.pairp() {
                rands.push(vm.mutator(), last.car());
                last = last.cdr();
            }

            vm.tail_apply_cont(k, proc, &rands)
        } else {
            let mut rands = ArrayList::with_capacity(vm.mutator(), args.len() - 2);

            for i in 1..args.len() - 1 {
                rands.push(vm.mutator(), args[i]);
            }

            if !last.is_list() {
                match wrong_contract::<()>("apply", "list?", 1, 2, args) {
                    Err(e) => return Trampoline::Throw(e),
                    _ => unreachable!()
                }
            }

            while last.pairp() {
                rands.push(vm.mutator(), last.car());
                last = last.cdr();
            }

            vm.tail_apply_cont(k, proc, &rands)
        }

    }
}

pub fn initialize_fun(env: Value) {
    environment_set(env, intern("apply"), *APPLY_PROC);
}