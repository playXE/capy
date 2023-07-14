use std::{intrinsics::likely, mem::transmute};

use rsgc::prelude::Handle;

use crate::{
    compaux::{scm_identifier_global_ref, scm_outermost_identifier},
    runtime::{
        error::wrong_count,
        module::scm_make_binding,
        object::{make_box, CodeBlock, ScmResult, Type, MAX_ARITY},
        value::Value,
    },
    vm::{callframe::CallFrame, scm_vm, VM, interpreter::vm_eval},
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
        cb.literals
            .vector_set(ix as _, Value::encode_object_value(gloc));
        ScmResult::ok(gloc)
    }
}

pub unsafe extern "C" fn baseline_assert_argcount_err(cfr: &mut CallFrame) -> Value {
    
    scm_vm().top_call_frame = cfr;
    let code_block: Handle<CodeBlock> = transmute(cfr.code_block());
    let err = wrong_count::<()>(
        &code_block.name.to_string(),
        code_block.mina as _,
        if code_block.maxa >= MAX_ARITY {
            -1
        } else {
            code_block.maxa as i32
        },
        (*cfr).argc.get_int32() as _,
        (*cfr).arguments(),
    );

    err.unwrap_err()
}

pub extern "C" fn baseline_write_barrier(vm: &mut VM, obj: Value) {
    vm.mutator().write_barrier(obj.get_object());
}

pub extern "C" fn baseline_box(vm: &mut VM, obj: Value) -> Value {
    vm.mutator().safepoint();
    make_box(vm.mutator(), obj)
}

pub unsafe extern "C" fn baseline_call(vm: &mut VM) -> ScmResult {
    println!("call? {}", (*vm.top_call_frame).callee());
    for arg in (*vm.top_call_frame).arguments().iter() {
        println!("arg: {}", arg);
    }
    match vm_eval(vm) {
        Ok(x) => ScmResult::ok(x),
        Err(x) => ScmResult::jit_err(x),
    }
}