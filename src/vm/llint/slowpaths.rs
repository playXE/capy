

use crate::{vm::VM, runtime::value::Value};

pub extern "C" fn llint_write_barrier(vm: &mut VM, val: Value) {
    vm.thread.write_barrier(val.get_object());
}

