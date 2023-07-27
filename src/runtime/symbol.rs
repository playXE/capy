use crate::vm::{scm_virtual_machine, thread::Thread};

use super::{object::scm_symbol_str, value::Value};

/// Interns string into non-movable space
pub fn scm_intern(string: impl AsRef<str>) -> Value {
    let string = string.as_ref();

    let vm = scm_virtual_machine();

    vm.symtab_lock.lock(true);
    if let Some(sym) = vm.symtable.get(string) {
        vm.symtab_lock.unlock();
        return *sym;
    }
    vm.symtab_lock.unlock();

    let sym = Thread::current().make_symbol(string);
    let string = scm_symbol_str(sym);
    vm.symtab_lock.lock(true);
    vm.symtable.insert(string, sym.into());
    vm.symtab_lock.unlock();

    sym.into()
}
