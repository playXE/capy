#[derive(Clone, Copy)]
pub enum NumberPair {
    Int(i32, i32),
    Float(f64, f64),
}

use crate::{
    error::wrong_type,
    value::*,
    vm::{Trampoline, Vm, RETURN_CONT}, compiler::env::environment_set,
};
use std::ops::*;

macro_rules! impl_op {
    ($($op:ident),*) => {
        impl NumberPair {$(
            pub fn $op (self) -> Value {
                match self {
                    Self::Int(a, b) => Value::make_int(paste::paste! {
                        a.[<wrapping_ $op>](b) }),
                    Self::Float(a, b) => Value::make_double(rsgc::prelude::Thread::current(), a.$op(b)),
                }
            })*
        }
    };
}

impl_op! {
    add, sub, div, rem, mul
}

impl NumberPair {
    pub fn lt(self) -> Value {
        match self {
            Self::Int(a, b) => (a < b).into(),
            Self::Float(a, b) => (a < b).into(),
        }
    }

    pub fn gt(self) -> Value {
        match self {
            Self::Int(a, b) => (a > b).into(),
            Self::Float(a, b) => (a > b).into(),
        }
    }

    pub fn le(self) -> Value {
        match self {
            Self::Int(a, b) => (a <= b).into(),
            Self::Float(a, b) => (a <= b).into(),
        }
    }

    pub fn ge(self) -> Value {
        match self {
            Self::Int(a, b) => (a >= b).into(),
            Self::Float(a, b) => (a >= b).into(),
        }
    }

    pub fn eq(self) -> Value {
        match self {
            Self::Int(a, b) => (a == b).into(),
            Self::Float(a, b) => (a == b).into(),
        }
    }

    pub fn new(a: Value, b: Value) -> Option<NumberPair> {
        Some(if a.intp() && b.intp() {
            NumberPair::Int(a.int(), b.int())
        } else if a.doublep() && b.doublep() {
            NumberPair::Float(a.double(), b.double())
        } else if a.doublep() && b.intp() {
            NumberPair::Float(a.double(), b.int() as f64)
        } else if a.intp() && b.doublep() {
            NumberPair::Float(a.int() as f64, b.double())
        } else {
            return None;
        })
    }
}

define_proc!(
    extern "+", plus (_vm, args) 1, -1 => {
        let mut acc = args[0];
        for (i, arg) in args[1..].iter().enumerate() {
            acc = match NumberPair::new(acc, *arg) {
                Some(np) => np.add(),
                None => {
                    if acc.numberp() {
                        return Trampoline::Throw(wrong_type::<()>("+", "number?", i as i32, args.len() as _, args).unwrap_err())
                    } else {
                        return Trampoline::Throw(wrong_type::<()>("+", "number?", i as i32 - 1, args.len() as _, args).unwrap_err())
                    }
                }
            };
        }

        Trampoline::Return(acc)
    }
);

define_proc! {
    extern "-", minus (_vm, args) 1, -1 => {
        let mut acc = args[0];
        if args.len() == 1 {
            if acc.intp() {
                return Trampoline::Return(Value::make_int(-acc.int()))
            } else if acc.doublep() {
                return Trampoline::Return(Value::make_double(rsgc::prelude::Thread::current(), -acc.double()))
            } else {
                return Trampoline::Throw(wrong_type::<()>("-", "number?", 0, args.len() as _, args).unwrap_err())
            }
        }
        for (i, arg) in args[1..].iter().enumerate() {
            acc = match NumberPair::new(acc, *arg) {
                Some(np) => np.sub(),
                None => {
                    if acc.numberp() {
                        return Trampoline::Throw(wrong_type::<()>("-", "number?", i as i32, args.len() as _, args).unwrap_err())
                    } else {
                        return Trampoline::Throw(wrong_type::<()>("-", "number?", i as i32 - 1, args.len() as _, args).unwrap_err())
                    }
                }
            };
        }

        Trampoline::Return(acc)
    }
}

define_proc! {
    extern "*", mul (_vm, args) 1, -1 => {
        let mut acc = args[0];

        for (i, arg) in args[1..].iter().enumerate() {
            acc = match NumberPair::new(acc, *arg) {
                Some(np) => np.mul(),
                None => {
                    if acc.numberp() {
                        return Trampoline::Throw(wrong_type::<()>("*", "number?", i as i32, args.len() as _, args).unwrap_err())
                    } else {
                        return Trampoline::Throw(wrong_type::<()>("*", "number?", i as i32 - 1, args.len() as _, args).unwrap_err())
                    }
                }
            };
        }

        Trampoline::Return(acc)
    }
}

define_proc! {
    extern "/", div (_vm, args) 1, -1 => {
        let mut acc = args[0];

        for (i, arg) in args[1..].iter().enumerate() {
            acc = match NumberPair::new(acc, *arg) {
                Some(np) => np.div(),
                None => {
                    if acc.numberp() {
                        return Trampoline::Throw(wrong_type::<()>("/", "number?", i as i32, args.len() as _, args).unwrap_err())
                    } else {
                        return Trampoline::Throw(wrong_type::<()>("/", "number?", i as i32 - 1, args.len() as _, args).unwrap_err())
                    }
                }
            };
        }

        Trampoline::Return(acc)
    }
}

define_proc! {
    extern "remainder", rem (_vm, args) 1, -1 => {
        let mut acc = args[0];

        for (i, arg) in args[1..].iter().enumerate() {
            acc = match NumberPair::new(acc, *arg) {
                Some(np) => np.rem(),
                None => {
                    if acc.numberp() {
                        return Trampoline::Throw(wrong_type::<()>("remainder", "number?", i as i32, args.len() as _, args).unwrap_err())
                    } else {
                        return Trampoline::Throw(wrong_type::<()>("remainder", "number?", i as i32 - 1, args.len() as _, args).unwrap_err())
                    }
                }
            };
        }

        Trampoline::Return(acc)
    }
}

define_proc! {
    extern ">", gt (_vm, args) 2, -1 => {
        let mut acc = args[0];

        for (i, arg) in args[1..].iter().enumerate() {
            acc = match NumberPair::new(acc, *arg) {
                Some(np) => np.gt().into(),
                None => {
                    if acc.numberp() {
                        return Trampoline::Throw(wrong_type::<()>(">", "number?", i as i32, args.len() as _, args).unwrap_err())
                    } else {
                        return Trampoline::Throw(wrong_type::<()>(">", "number?", i as i32 - 1, args.len() as _, args).unwrap_err())
                    }
                }
            };
        }

        Trampoline::Return(acc)
    }
}

define_proc! {
    extern ">=", gte (_vm, args) 2, -1 => {
        let mut acc = args[0];

        for (i, arg) in args[1..].iter().enumerate() {
            acc = match NumberPair::new(acc, *arg) {
                Some(np) => np.ge().into(),
                None => {
                    if acc.numberp() {
                        return Trampoline::Throw(wrong_type::<()>(">=", "number?", i as i32, args.len() as _, args).unwrap_err())
                    } else {
                        return Trampoline::Throw(wrong_type::<()>(">=", "number?", i as i32 - 1, args.len() as _, args).unwrap_err())
                    }
                }
            };
        }

        Trampoline::Return(acc)
    }
}

define_proc! {
    extern "<", lt (_vm, args) 2, -1 => {
        let mut acc = args[0];

        for (i, arg) in args[1..].iter().enumerate() {
            acc = match NumberPair::new(acc, *arg) {
                Some(np) => np.lt().into(),
                None => {
                    if acc.numberp() {
                        return Trampoline::Throw(wrong_type::<()>(">", "number?", i as i32, args.len() as _, args).unwrap_err())
                    } else {
                        return Trampoline::Throw(wrong_type::<()>(">", "number?", i as i32 - 1, args.len() as _, args).unwrap_err())
                    }
                }
            };
        }

        Trampoline::Return(acc)
    }
}

define_proc! {
    extern "<=", lte (_vm, args) 2, -1 => {
        let mut acc = args[0];

        for (i, arg) in args[1..].iter().enumerate() {
            acc = match NumberPair::new(acc, *arg) {
                Some(np) => np.le().into(),
                None => {
                    if acc.numberp() {
                        return Trampoline::Throw(wrong_type::<()>(">=", "number?", i as i32, args.len() as _, args).unwrap_err())
                    } else {
                        return Trampoline::Throw(wrong_type::<()>(">=", "number?", i as i32 - 1, args.len() as _, args).unwrap_err())
                    }
                }
            };
        }

        Trampoline::Return(acc)
    }
}

define_proc! {
    extern "number?", numberp (_vm, args) 1, 1 => {
        Trampoline::Return(args[0].numberp().into())
    }
}

define_proc! {
    extern "=", number_eq (_vm, args) 2, -1 => {
        let mut acc = args[0];

        for (i, arg) in args[1..].iter().enumerate() {
            acc = match NumberPair::new(acc, *arg) {
                Some(np) => np.eq().into(),
                None => {
                    if acc.numberp() {
                        return Trampoline::Throw(wrong_type::<()>("=", "number?", i as i32, args.len() as _, args).unwrap_err())
                    } else {
                        return Trampoline::Throw(wrong_type::<()>("=", "number?", i as i32 - 1, args.len() as _, args).unwrap_err())
                    }
                }
            };
        }

        Trampoline::Return(acc)
    }
}

pub fn bin_lt(vm: &mut Vm, a: Value, b: Value) -> Result<bool, Value> {
    lt(vm, *RETURN_CONT, &[a, b]).into()
}

pub fn bin_gt(vm: &mut Vm, a: Value, b: Value) -> Result<bool, Value> {
    gt(vm, *RETURN_CONT, &[a, b]).into()
}

pub fn bin_lte(vm: &mut Vm, a: Value, b: Value) -> Result<bool, Value> {
    gte(vm, *RETURN_CONT, &[a, b]).into()
}

pub fn bin_gte(vm: &mut Vm, a: Value, b: Value) -> Result<bool, Value> {
    lte(vm, *RETURN_CONT, &[a, b]).into()
}

pub fn bin_eq(vm: &mut Vm, a: Value, b: Value) -> Result<bool, Value> {
    number_eq(vm, *RETURN_CONT, &[a, b]).into()
}

pub fn initialize_env(env: Value) {
    environment_set(env, *PLUS_NAME, *PLUS_PROC);
    environment_set(env, *MINUS_NAME, *MINUS_PROC);
    environment_set(env, *MUL_NAME, *MUL_PROC);
    environment_set(env, *DIV_NAME, *DIV_PROC);
    environment_set(env, *REM_NAME, *REM_PROC);
    environment_set(env, *GT_NAME, *GT_PROC);
    environment_set(env, *GTE_NAME, *GTE_PROC);
    environment_set(env, *LT_NAME, *LT_PROC);
    environment_set(env, *LTE_NAME, *LTE_PROC);
    environment_set(env, *NUMBERP_NAME, *NUMBERP_PROC);
    environment_set(env, *NUMBER_EQ_NAME, *NUMBER_EQ_PROC);
}