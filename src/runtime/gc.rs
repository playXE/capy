use crate::vm::{callframe::CallFrame, scm_vm};
use rsgc::heap::heap::heap;

use crate::runtime::value::Value;
use rsgc::prelude::{Object, Visitor};

use super::{
    fun::scm_make_subr,
    module::{scm_capy_module, scm_define},
    object::ScmResult,
    symbol::Intern,
    value::scm_int,
    vector::make_values,
};

extern "C" fn object_address(cfr: &mut CallFrame) -> ScmResult {
    let obj = cfr.argument(0);
    ScmResult::ok(scm_int(obj.get_raw()))
}

extern "C" fn gc(_: &mut CallFrame) -> ScmResult {
    heap().request_gc();
    ScmResult::ok(Value::encode_undefined_value())
}

extern "C" fn gc_stats(_: &mut CallFrame) -> ScmResult {
    let heap = heap();
    let used = heap.get_used() / 1024;
    let max = heap.max_capacity() / 1024;
    let available = heap.get_available() / 1024;
    //println!("used: {}, available: {}, max: {}", used, available, max);
    ScmResult::ok(make_values(
        scm_vm().mutator(),
        &[
            scm_int(used as _),
            scm_int(available as _),
            scm_int(max as _),
        ],
    ))
}

pub fn init_gc() {
    let module = scm_capy_module().module();
    let subr = scm_make_subr("object-address", object_address, 1, 1);
    scm_define(module, "object-address".intern(), subr).unwrap();

    let subr = scm_make_subr("gc", gc, 0, 0);
    scm_define(module, "gc".intern(), subr).unwrap();

    let subr = scm_make_subr("gc-stats", gc_stats, 0, 0);
    scm_define(module, "gc-stats".intern(), subr).unwrap();
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
