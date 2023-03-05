use rsgc::thread::Thread;

use crate::{vm::Vm, value::{Value, Type}, raise_exn};



pub unsafe extern "C" fn listify(vm: &mut Vm, argv: *const Value, argc: usize) -> Value {
    let slice = std::slice::from_raw_parts(argv, argc);

    Value::make_list(vm.mutator(), slice)
}

pub unsafe extern "C" fn make_wrong_arity(_vm: &mut Vm, got: usize, maxa: usize, mina: usize, argv: *const Value, _body: Value) -> Value {
    // skip the first argument, which is the continuation. User code does not see it.
    let slice = std::slice::from_raw_parts(argv.add(1), got - 1);
    //println!("arity error in {}", body);
    crate::error::wrong_count::<()>("", mina as i32 - 1, maxa as i32 - 1, got as _, slice).err().unwrap() 
}


pub unsafe extern "C" fn make_boxed(vm: &mut Vm, val: Value) -> Value {
    Value::make_boxed(vm.mutator(), val)
}

pub unsafe extern "C" fn grow_tail_rands(vm: &mut Vm, to: usize) {
    println!("{} {}", to, vm.tail_rands.capacity());
    vm.tail_rands.reserve(Thread::current(), to);
}

pub unsafe extern "C" fn make_closure(vm: &mut Vm, code: extern "C" fn(&mut Vm, Value, *const Value, usize, &mut u8) -> Value, nenv: usize, mina: usize, maxa: usize) -> Value {
    vm.mutator().safepoint();
    let env = Value::make_vector(vm.mutator, nenv as _, Value::make_null());
    let proc = Value::make_nativeprocedure(vm.mutator, code as usize, mina as _, maxa as _, env);

    proc
}

pub unsafe extern "C" fn undefined_global(_: &mut Vm, cell: Value) -> Value {
    let name = cell.car();

    let err: Result<(), Value> = raise_exn!(
        FailContractVariable,
        &[name],
        "undefined global variable: {}",
        name 
    );

    err.err().unwrap()
}

pub unsafe extern "C" fn flush_ssb_and_do_wb(vm: &mut Vm, val: Value) {
    vm.mutator().flush_ssb();
    if val.get_type() > Type::Integer {
        vm.mutator().write_barrier(val.handle());
    }
    vm.mutator().safepoint();
}