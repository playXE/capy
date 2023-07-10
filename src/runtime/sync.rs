use crate::vm::callframe::CallFrame;

use super::{error::wrong_contract, object::ScmResult, value::Value, module::{scm_capy_module, scm_define}, fun::scm_make_subr, symbol::Intern};

extern "C" fn acquire(cfr: &mut CallFrame) -> ScmResult {
    let obj = cfr.argument(0);
    let block = if cfr.argument_count() > 1 {
        cfr.argument(1)
    } else {
        false.into()
    };

    if !block.is_bool() {
        return wrong_contract::<()>(
            "acquire",
            "boolean?",
            1,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    if !obj.is_object() {
        return wrong_contract::<()>(
            "acquire",
            "object?",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    let obj = obj.object_header_ref();

    if block.is_true() {
        obj.lock.lock(true);
        ScmResult::ok(true)
    } else {
        ScmResult::ok(obj.lock.try_lock())
    }
}

extern "C" fn release(cfr: &mut CallFrame) -> ScmResult {
    let obj = cfr.argument(0);

    if !obj.is_object() {
        return wrong_contract::<()>(
            "release",
            "object?",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    let obj = obj.object_header_ref();

    if !obj.lock.is_locked() {
        return wrong_contract::<()>(
            "release",
            "locked?",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    obj.lock.unlock();

    ScmResult::ok(Value::encode_undefined_value())
}

extern "C" fn locked_p(cfr: &mut CallFrame) -> ScmResult {
    let obj = cfr.argument(0);

    if !obj.is_object() {
        return wrong_contract::<()>(
            "locked?",
            "object?",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    let obj = obj.object_header_ref();

    ScmResult::ok(obj.lock.is_locked())
}

pub(crate) fn init_sync() {
    let module = scm_capy_module().module();

    let subr = scm_make_subr("acquire", acquire, 1, 2);
    scm_define(module, "acquire".intern().into(), subr).unwrap();

    let subr = scm_make_subr("release", release, 1, 1);
    scm_define(module, "release".intern().into(), subr).unwrap();

    let subr = scm_make_subr("locked?", locked_p, 1, 1);
    scm_define(module, "locked?".intern().into(), subr).unwrap();
}
