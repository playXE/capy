use rsgc::heap::heap::heap;
use crate::vm::callframe::CallFrame;

use rsgc::prelude::{Object, Visitor};
use crate::runtime::value::Value;


use super::{
    fun::scm_make_subr,
    module::{scm_capy_module, scm_define},
    object::ScmResult,
    symbol::Intern,
    value::scm_int,
};

extern "C" fn object_address(cfr: &mut CallFrame) -> ScmResult {
    let obj = cfr.argument(0);
    ScmResult::ok(scm_int(obj.get_raw()))
}

extern "C" fn gc(_ :&mut CallFrame) -> ScmResult {
    heap().request_gc();
    ScmResult::ok(Value::encode_undefined_value())
}

pub fn init_gc() {
    let module = scm_capy_module().module();
    let subr = scm_make_subr("object-address", object_address, 1, 1);
    scm_define(module, "object-address".intern(), subr).unwrap();

    let subr = scm_make_subr("gc", gc, 0, 0);
    scm_define(module, "gc".intern(), subr).unwrap();
}

#[repr(C)]
struct ConservativeObject {
    size: usize,
    data: [u8; 0],
}

unsafe impl Object for ConservativeObject {
    fn trace(&self, visitor: &mut dyn Visitor) {
        unsafe {
            visitor.visit_conservative(
                self.data.as_ptr().cast(),
                self.size / std::mem::size_of::<usize>(),
            );
        }
    }
}
