use std::intrinsics::likely;

use rsgc::prelude::Handle;

use crate::{
    compaux::{scm_outermost_identifier, scm_identifier_global_ref},
    runtime::{
        module::scm_make_binding,
        object::{CodeBlock, ScmResult, Type},
        value::Value,
    }, vm::scm_vm,
};

pub extern "C" fn baseline_define(name: Value, value: Value) -> ScmResult {
    let id = name.identifier();
    let id = scm_outermost_identifier(id);
    let module = id.module.module();
    let name = id.name.symbol();

    match scm_make_binding(module, name, value, 0) {
        Ok(_) => ScmResult::ok(name),
        Err(x) => ScmResult::jit_err(x),
    }
}

pub extern "C" fn baseline_resolve_global(cb: Handle<CodeBlock>, ix: u16) -> ScmResult {
    let constant = cb.literals.vector_ref(ix as _);

    // If the global location is already resolved then return it.
    // TODO: Investigate into hot-code patching for this instead of always entering this thunk.
    if likely(constant.is_xtype(Type::GLOC)) {
        ScmResult::ok(constant)
    } else {
        let (_, gloc) = match scm_identifier_global_ref(constant.identifier()) {
            Ok(x) => x,
            Err(x) => return ScmResult::jit_err(x),
        };

        
        scm_vm().mutator().write_barrier(cb.literals.vector());
        cb
            .literals
            .vector_set(ix as _, Value::encode_object_value(gloc));
        ScmResult::ok(gloc)
    }
}
