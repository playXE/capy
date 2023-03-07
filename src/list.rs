use once_cell::sync::Lazy;
use rsgc::thread::Thread;

use crate::{
    bool::{eq, equal, eqv},
    value::Value,
    vm::{Trampoline, Vm, intern}, error::wrong_contract, compiler::env::environment_set,
};

impl Value {
    pub fn is_list(self) -> bool {
        let mut obj2;
        let mut obj1 = self;

        if obj1.nullp() {
            return true;
        } else if !obj1.pairp() {
            return false;
        }

        obj2 = obj1;

        loop {
            obj1 = obj1.cdr();

            if obj1.nullp() {
                return true;
            }

            if !obj1.pairp() {
                return false;
            }

            obj1 = obj1.cdr();

            if obj1.nullp() {
                return true;
            }

            if !obj1.pairp() {
                return false;
            }

            obj2 = obj2.cdr();
        }
    }

    pub fn list_length(self) -> usize {
        let mut len = 0;
        let mut ls = self;
        while !ls.nullp() {
            len += 1;
            if ls.pairp() {
                ls = ls.cdr();
            } else {
                ls = Value::make_null();
            }
        }

        len
    }

    pub fn list_to_vector(self) -> Value {
        let vec = Value::make_vector(
            Thread::current(),
            self.list_length() as _,
            Value::make_null(),
        );
        let mut ls = self;
        let mut i = 0;
        while !ls.nullp() {
            vec.vector_set(i, ls.car());
            i += 1;
            ls = ls.cdr();
        }

        vec
    }

    pub fn vector_to_list(self) -> Value {
        Self::make_list(Thread::current(), self.vector_as_slice())
    }

    pub fn proper_list_length(self) -> Option<usize> {
        let mut len = 0;
        let mut ls = self;
        while !ls.nullp() {
            len += 1;
            if ls.pairp() {
                ls = ls.cdr();
            } else {
                return None;
            }
        }

        Some(len)
    }

    /// (list 1 2 3 4 5)
    pub fn make_list(thread: &mut Thread, args: &[Value]) -> Value {
        let mut ls = Value::make_null();
        for arg in args.iter().rev() {
            ls = Value::cons(thread, *arg, ls);
        }

        ls
    }

    /// (list* 1 2 3 4 5)
    pub fn make_list_star(thread: &mut Thread, args: &[Value]) -> Value {
        assert!(args.len() > 0, "make_list_star: args.len() == 0");
        let mut ls = args[args.len() - 1];
        for arg in args[0..args.len() - 1].iter().rev() {
            ls = Value::cons(thread, *arg, ls);
        }

        ls
    }

    pub fn cons(thread: &mut Thread, car: Value, cdr: Value) -> Value {
        Value::make_pair(thread, car, cdr)
    }

    pub fn list_append_var(thread: &mut Thread, lists: &[Value]) -> Value {
        let mut first;
        let mut last: Option<Value>;
        let mut v;

        first = None;
        last = None;

        for ls in lists {
            let mut ls = *ls;
            while ls.pairp() {
                v = Value::cons(thread, ls.car(), Value::make_null());

                if first.is_none() {
                    first = Some(v);
                } else {
                    thread.write_barrier(last.unwrap().handle());
                    last.unwrap().set_pair_cdr(v);
                }

                last = Some(v);
                ls = ls.cdr();
            }

            if !ls.nullp() {
                panic!("list_append_var: ls is not a list");
            }
        }

        first.unwrap_or(Value::make_null())
    }

    pub fn list_append(thread: &mut Thread, mut l1: Value, l2: Value) -> Value {
        let mut first;
        let mut last: Option<Value>;
        let mut v;

        first = None;
        last = None;

        while l1.pairp() {
            v = Value::cons(thread, l1.car(), Value::make_null());

            if first.is_none() {
                first = Some(v);
            } else {
                thread.write_barrier(last.unwrap().handle());
                last.unwrap().set_pair_cdr(v);
            }

            last = Some(v);
            l1 = l1.cdr();
        }

        if !l1.nullp() {
            panic!("list_append: l1 is not a list");
        }

        if last.is_none() {
            return l2;
        }

        thread.write_barrier(last.unwrap().handle());
        last.unwrap().set_pair_cdr(l2);

        first.unwrap()
    }

    pub fn list_map2(
        thread: &mut Thread,
        mut ls1: Value,
        mut ls2: Value,
        mut proc: impl FnMut(Value, Value) -> Value,
    ) -> Value {
        let mut first;
        let mut last: Option<Value>;
        let mut v;

        first = None;
        last = None;

        while ls1.pairp() && ls2.pairp() {
            v = Value::cons(thread, proc(ls1.car(), ls2.car()), Value::make_null());

            if first.is_none() {
                first = Some(v);
            } else {
                thread.write_barrier(last.unwrap().handle());
                last.unwrap().set_pair_cdr(v);
            }

            last = Some(v);
            ls1 = ls1.cdr();
            ls2 = ls2.cdr();
        }

        if !ls1.nullp() {
            panic!("list_map2: ls1 is not a list");
        }

        if !ls2.nullp() {
            panic!("list_map2: ls2 is not a list");
        }

        first.unwrap_or(Value::make_null())
    }

    pub fn try_list_map(
        thread: &mut Thread,
        mut ls: Value,
        mut proc: impl FnMut(Value) -> Result<Value, Value>,
    ) -> Result<Value, Value> {
        let mut first;
        let mut last: Option<Value>;

        first = None;
        last = None;

        while ls.pairp() {
            let v = proc(ls.car())?;
            let v = Value::cons(thread, v, Value::make_null());

            if first.is_none() {
                first = Some(v);
            } else {
                thread.write_barrier(last.unwrap().handle());
                last.unwrap().set_pair_cdr(v);
            }

            last = Some(v);
            ls = ls.cdr();
        }

        if !ls.nullp() {
            panic!("list_map: ls is not a list");
        }

        Ok(first.unwrap_or(Value::make_null()))
    }

    pub fn list_map(
        thread: &mut Thread,
        mut ls: Value,
        mut proc: impl FnMut(Value) -> Value,
    ) -> Value {
        let mut first;
        let mut last: Option<Value>;
        let mut v;

        first = None;
        last = None;

        while ls.pairp() {
            v = Value::cons(thread, proc(ls.car()), Value::make_null());

            if first.is_none() {
                first = Some(v);
            } else {
                thread.write_barrier(last.unwrap().handle());
                last.unwrap().set_pair_cdr(v);
            }

            last = Some(v);
            ls = ls.cdr();
        }

        if !ls.nullp() {
            panic!("list_map: ls is not a list");
        }

        first.unwrap_or(Value::make_null())
    }

    pub fn list_reverse(thread: &mut Thread, mut ls: Value) -> Value {
        let mut last = Value::make_null();

        while !ls.nullp() {
            if !ls.pairp() {
                panic!("list_reverse: ls is not a list");
            }

            last = Value::cons(thread, ls.car(), last);
            ls = ls.cdr();
        }

        last
    }

    pub fn list_filter(
        thread: &mut Thread,
        mut ls: Value,
        mut proc: impl FnMut(Value) -> bool,
    ) -> Value {
        let mut first;
        let mut last: Option<Value>;
        let mut v;

        first = None;
        last = None;

        while ls.pairp() {
            if proc(ls.car()) {
                v = Value::cons(thread, ls.car(), Value::make_null());

                if first.is_none() {
                    first = Some(v);
                } else {
                    thread.write_barrier(last.unwrap().handle());
                    last.unwrap().set_pair_cdr(v);
                }

                last = Some(v);
            }

            ls = ls.cdr();
        }

        if !ls.nullp() {
            panic!("list_filter: ls is not a list");
        }

        first.unwrap_or(Value::make_null())
    }

    pub fn copy_alist(thread: &mut Thread, mut ls: Value) -> Value {
        let mut first;
        let mut last: Option<Value>;
        let mut v;

        first = None;
        last = None;

        while ls.pairp() {
            v = Value::cons(thread, ls.car(), Value::make_null());

            if first.is_none() {
                first = Some(v);
            } else {
                thread.write_barrier(last.unwrap().handle());
                last.unwrap().set_pair_cdr(v);
            }

            last = Some(v);
            ls = ls.cdr();
        }

        if !ls.nullp() {
            panic!("copy_alist: ls is not a list");
        }

        first.unwrap()
    }

    pub fn list_union(thread: &mut Thread, set1: Value, set2: Value) -> Value {
        fn rec(thread: &mut Thread, set1: Value, set2: Value) -> Value {
            if set1.nullp() {
                set2
            } else if Value::memq(set1.car(), set2).is_true() {
                rec(thread, set1.cdr(), set2)
            } else {
                let v = rec(thread, set1.cdr(), set2);
                return Value::make_cons(set1.car(), v);
            }
        }
        rec(thread, set1, set2)
    }

    pub fn list_union_many(thread: &mut Thread, sets: &[Value]) -> Value {
        fn rec(thread: &mut Thread, set1: Value, sets: &[Value]) -> Value {
            if sets.is_empty() {
                set1
            } else {
                let v = rec(thread, sets[0], &sets[1..]);
                Value::list_union(thread, set1, v)
            }
        }

        if sets.is_empty() {
            Value::make_null()
        } else {
            rec(thread, sets[0], &sets[1..])
        }
    }

    pub fn list_intersection(thread: &mut Thread, set1: Value, set2: Value) -> Value {
        fn rec(thread: &mut Thread, set1: Value, set2: Value) -> Value {
            if set1.nullp() {
                Value::make_null()
            } else if Value::memq(set1.car(), set2).is_true() {
                let v = rec(thread, set1.cdr(), set2);
                return Value::make_cons(set1.car(), v);
            } else {
                rec(thread, set1.cdr(), set2)
            }
        }
        rec(thread, set1, set2)
    }

    pub fn list_intersection_many(thread: &mut Thread, sets: &[Value]) -> Value {
        fn rec(thread: &mut Thread, set1: Value, sets: &[Value]) -> Value {
            if sets.is_empty() {
                set1
            } else {
                let v = rec(thread, sets[0], &sets[1..]);
                Value::list_intersection(thread, set1, v)
            }
        }

        if sets.is_empty() {
            Value::make_null()
        } else {
            rec(thread, sets[0], &sets[1..])
        }
    }

    pub fn list_difference(thread: &mut Thread, set1: Value, set2: Value) -> Value {
        fn rec(thread: &mut Thread, set1: Value, set2: Value) -> Value {
            if set1.nullp() {
                Value::make_null()
            } else if Value::memq(set1.car(), set2).is_true() {
                rec(thread, set1.cdr(), set2)
            } else {
                let v = rec(thread, set1.cdr(), set2);
                return Value::make_cons(set1.car(), v);
            }
        }

        rec(thread, set1, set2)
    }

    pub fn unproper_to_proper(thread: &mut Thread, mut ls: Value) -> Value {
        let mut nls = Value::make_null();

        while ls.pairp() {
            nls = Value::make_cons(ls.car(), nls);
            ls = ls.cdr();

            if !ls.pairp() && !ls.nullp() {
                nls = Value::make_cons(ls, nls);
            }
        }

        Value::list_reverse(thread, nls)
    }

    pub fn list_member(key: Value, mut list: Value) -> Value {
        if !list.is_list() {
            panic!("list_member: list is not a list");
        }

        loop {
            if list.nullp() {
                return Value::make_false();
            }

            if list.car() == key {
                return list;
            }

            list = list.cdr();
        }
    }
}

macro_rules! gen_ass {
    ($name: ident, $equality: expr) => {
        impl Value {
            pub fn $name(key: Value, ls: Value) -> Value {
                let mut list = ls;
                let mut turtle = ls;
                let mut pair;

                while list.pairp() {
                    pair = list.car();

                    if !pair.pairp() {
                        panic!("{}: list is not a list of pairs", stringify!($name));
                    }

                    if $equality(key, pair.car()) {
                        return pair;
                    }

                    list = list.cdr();

                    if list.pairp() {
                        pair = list.car();

                        if pair.pairp() {
                            if $equality(key, pair.car()) {
                                return pair;
                            }

                            list = list.cdr();
                            if list == turtle {
                                return Value::make_false();
                            }

                            turtle = turtle.cdr();
                        }
                    }
                }

                if !list.nullp() {
                    panic!("{}: list is not a list: {:?}", stringify!($name), list);
                }

                Value::make_false()
            }
        }
    };
}

gen_ass!(assv, eqv);
gen_ass!(assq, eq);
gen_ass!(assoc, equal);

macro_rules! gen_mem {
    ($name: ident, $equality: expr) => {
        impl Value {
            pub fn $name(key: Value, ls: Value) -> Value {
                let mut list = ls;
                let mut turtle = ls;
                let mut pair;

                while list.pairp() {
                    pair = list.car();

                    if $equality(key, pair) {
                        return pair;
                    }

                    list = list.cdr();

                    if list.pairp() {
                        pair = list.car();

                        if $equality(key, pair) {
                            return pair;
                        }

                        list = list.cdr();
                        if list == turtle {
                            return Value::make_false();
                        }

                        turtle = turtle.cdr();
                    }
                }

                if !list.nullp() {
                    panic!("{}: list is not a list", stringify!($name));
                }

                Value::make_false()
            }
        }
    };
}

gen_mem!(memv, eqv);
gen_mem!(memq, eq);

pub static ASSV_PROC: Lazy<Value> = Lazy::new(|| {
    Vm::make_procedure(
        "assv",
        |_vm, _, args| {
            let key = args[0];
            let ls = args[1];
            let val = Value::assv(key, ls);
            Trampoline::Return(val)
        },
        2,
        2,
    )
});

pub static ASSQ_PROC: Lazy<Value> = Lazy::new(|| {
    Vm::make_procedure(
        "assq",
        |_vm, _, args| {
            let key = args[0];
            let ls = args[1];
            let val = Value::assq(key, ls);
            Trampoline::Return(val)
        },
        2,
        2,
    )
});

pub static ASSOC_PROC: Lazy<Value> = Lazy::new(|| {
    Vm::make_procedure(
        "assoc",
        |_vm, _, args| {
            let key = args[0];
            let ls = args[1];
            let val = Value::assoc(key, ls);
            Trampoline::Return(val)
        },
        2,
        2,
    )
});

pub static MEMV_PROC: Lazy<Value> = Lazy::new(|| {
    Vm::make_procedure(
        "memv",
        |_vm, _, args| {
            let key = args[0];
            let ls = args[1];
            let val = Value::memv(key, ls);
            Trampoline::Return(val)
        },
        2,
        2,
    )
});

pub static MEMQ_PROC: Lazy<Value> = Lazy::new(|| {
    Vm::make_procedure(
        "memq",
        |_vm, _, args| {
            let key = args[0];
            let ls = args[1];
            let val = Value::memq(key, ls);
            Trampoline::Return(val)
        },
        2,
        2,
    )
});

pub fn scheme_append(vm: &mut Vm, mut l1: Value, l2: Value) -> Result<Value, Value> {
    let orig = l1;
    let mut first;
    let mut last;
    let mut v;

    first = None::<Value>;
    last = None::<Value>;

    while l1.pairp() {
        v = Value::make_cons(l1.car(), Value::make_null());
        if first.is_none() {
            first = Some(v);
        } else {
            vm.mutator().write_barrier(last.unwrap().handle());
            last.unwrap().set_pair_cdr(v);
        }

        last = Some(v);
        l1 = l1.cdr();
    }

    if !l1.nullp() {
        return wrong_contract("append", "list?", -1, 0, &[orig]);
    }

    if last.is_none() {
        return Ok(l2);
    }

    vm.mutator().write_barrier(last.unwrap().handle());
    last.unwrap().set_pair_cdr(l2);

    Ok(first.unwrap())
}

/* 
When given all list arguments, the result is a list that contains all of the elements of the given lists in order. The last argument is used directly in the tail of the result.
The last argument need not be a list, in which case the result is an “improper list.”

This function takes time proportional to the length of all arguments (added together) except the last argument.
*/
define_proc! {
    extern "append", append(_vm, args) 0, -1 => {
        
        let mut res = Value::make_null();

        if args.is_empty() {
            return Trampoline::Return(res);
        }

        for i in (0..args.len() - 1).rev() {
            res = match scheme_append(_vm, args[i], res) {
                Ok(v) => v,
                Err(e) => return Trampoline::Throw(e),
            }
        }

        Trampoline::Return(res)
    }
}

define_proc! {
    extern "list", list(_vm, args) 0, -1 => {
        Trampoline::Return(Value::make_list(_vm.mutator(), args))
    }
}

define_proc! {
    extern "list?", is_list(_vm, args) 1, 1 => {
        let arg = args[0];
        if arg.is_list() {
            Trampoline::Return(Value::make_true())
        } else {
            Trampoline::Return(Value::make_false())
        }
    }
}

define_proc! {
    extern "length", length(_vm, args) 1, 1 => {
        let arg = args[0];
        if arg.is_list() {
            Trampoline::Return(Value::make_int(arg.list_length() as _))
        } else {
            wrong_contract::<()>("length", "list?", 0, 1, args).into()
        }
    }
}

define_proc! {
    extern "list*", list_star(_vm, args) 0, -1 => {
        Trampoline::Return(Value::make_list_star(_vm.mutator(), args))
    }
}


pub fn initialize_list(env: Value) {
    environment_set(env, intern("assv"), *ASSV_PROC);
    environment_set(env, intern("assq"), *ASSQ_PROC);
    environment_set(env, intern("assoc"), *ASSOC_PROC);
    environment_set(env, intern("memv"), *MEMV_PROC);
    environment_set(env, intern("memq"), *MEMQ_PROC);
    environment_set(env, intern("append"), *APPEND_PROC);
    environment_set(env, intern("list"), *LIST_PROC);
    environment_set(env, intern("list?"), *IS_LIST_PROC);
    environment_set(env, intern("length"), *LENGTH_PROC);
    environment_set(env, intern("list*"), *LIST_STAR_PROC);


}