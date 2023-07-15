use crate::vm::callframe::CallFrame;

use super::{object::ScmResult, value::scm_int, module::{scm_capy_module, scm_define}, fun::scm_make_subr, symbol::Intern};



extern "C" fn object_address(cfr: &mut CallFrame) -> ScmResult {
    let obj = cfr.argument(0);
    ScmResult::ok(scm_int(obj.get_raw()))
}

pub fn init_gc() {
    let module = scm_capy_module().module();
    let subr = scm_make_subr("object-address", object_address, 1, 1);
    scm_define(module, "object-address".intern(), subr).unwrap();
}