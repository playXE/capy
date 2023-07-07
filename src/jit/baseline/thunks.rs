use b3::{Procedure, BlockId};

use crate::{runtime::{value::Value, object::ScmResult, module::scm_make_binding}, compaux::scm_outermost_identifier};

pub extern "C" fn baseline_define(name: Value, value: Value) -> ScmResult {
    let id = name.identifier();
    let id = scm_outermost_identifier(id);
    let module = id.module.module();
    let name = id.name.symbol();

    match scm_make_binding(module, name, value, 0) {
        Ok(_) => ScmResult::ok(name),
        Err(x) => ScmResult::err(x),
    }
}

pub extern "C" fn baseline_resolve_global(name: Value) -> ScmResult {
    todo!()
}

