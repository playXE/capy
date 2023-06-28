use rsgc::{prelude::Handle, thread::Thread};

use crate::{
    compile::IForm,
    runtime::object::{
        ClosedNativeProcedure, CodeBlock, NativeProcedure, ObjectHeader, Procedure, ScmResult, Type,
    },
    runtime::value::Value,
    vm::{callframe::CallFrame, interpreter::apply},
};

use super::{
    module::{scm_define, scm_scheme_module},
    object::MAX_ARITY,
    symbol::{make_symbol, Intern},
    vector::make_values,
};

pub fn make_procedure(t: &mut Thread, code_block: Handle<CodeBlock>) -> Handle<Procedure> {
    t.allocate(Procedure {
        header: ObjectHeader::new(Type::Procedure),
        name: Value::encode_undefined_value(),
        code: code_block,
        env_size: 0,
        captures: [],
    })
}

pub fn make_closed_procedure(
    t: &mut Thread,
    code_block: Handle<CodeBlock>,
    ncaptures: usize,
) -> Handle<Procedure> {
    let mut proc = t.allocate_varsize::<Procedure>(ncaptures);

    unsafe {
        let proc = proc.assume_init_mut();

        proc.header = ObjectHeader::new(Type::Procedure);
        proc.name = Value::encode_undefined_value();
        proc.code = code_block;
        proc.env_size = ncaptures as _;

        for i in 0..ncaptures {
            proc.captures
                .as_mut_ptr()
                .add(i)
                .write(Value::encode_undefined_value());
        }
    }

    unsafe { proc.assume_init() }
}

pub fn scm_make_native_procedure(
    t: &mut Thread,
    name: Value,
    callback: extern "C" fn(&mut CallFrame) -> ScmResult,
    mina: u32,
    maxa: u32,
) -> Handle<NativeProcedure> {
    t.allocate(NativeProcedure {
        header: ObjectHeader::new(Type::NativeProcedure),
        name,
        inliner: None,
        callback,
        mina,
        maxa,
    })
}

pub fn scm_make_closed_native_procedure(
    t: &mut Thread,
    name: Value,
    callback: extern "C" fn(&mut CallFrame) -> ScmResult,
    mina: u32,
    maxa: u32,
    captures: &[Value],
) -> Handle<ClosedNativeProcedure> {
    let mut proc = t.allocate_varsize::<ClosedNativeProcedure>(captures.len());

    unsafe {
        let proc = proc.assume_init_mut();

        proc.header = ObjectHeader::new(Type::ClosedNativeProcedure);
        proc.name = name;
        proc.callback = callback;
        proc.mina = mina;
        proc.maxa = maxa;
        proc.inliner = None;
        proc.env_size = captures.len() as _;
        proc.captures
            .as_mut_ptr()
            .copy_from_nonoverlapping(captures.as_ptr(), captures.len());
    }

    unsafe { proc.assume_init() }
}

pub fn scm_make_subr(
    name: &str,
    proc: extern "C" fn(&mut CallFrame) -> ScmResult,
    mina: u32,
    maxa: u32,
) -> Value {
    let name = make_symbol(name, true);
    let proc = scm_make_native_procedure(&mut Thread::current(), name, proc, mina, maxa);
    proc.into()
}

pub fn scm_make_subr_inliner(
    name: &str,
    proc: extern "C" fn(&mut CallFrame) -> ScmResult,
    mina: u32,
    maxa: u32,
    inliner: fn(&[Handle<IForm>], Value) -> Option<Handle<IForm>>,
) -> Value {
    let proc = scm_make_subr(name, proc, mina, maxa);
    proc.native_procedure().inliner = Some(inliner);
    proc
}

pub fn scm_make_subr_closed_inliner(
    name: &str,
    proc: extern "C" fn(&mut CallFrame) -> ScmResult,
    mina: u32,
    maxa: u32,
    captures: &[Value],
    inliner: fn(&[Handle<IForm>], Value) -> Option<Handle<IForm>>,
) -> Value {
    let name = make_symbol(name, true);
    let mut proc =
        scm_make_closed_native_procedure(&mut Thread::current(), name, proc, mina, maxa, captures);
    proc.inliner = Some(inliner);
    proc.into()
}

fn get_or_check_arity(p: Value, a: usize) -> Option<(i32, i32)> {
    let mina;
    let maxa;
    if p.is_vm_procedure() {
        let p = p.procedure();

        mina = p.code.mina as i32;

        if p.code.maxa >= MAX_ARITY {
            maxa = -1;
        } else {
            maxa = p.code.maxa as i32;
        }
    } else if p.is_native_procedure() {
        let p = p.native_procedure();

        mina = p.mina as i32;

        if p.maxa >= MAX_ARITY {
            maxa = -1;
        } else {
            maxa = p.maxa as i32;
        }
    } else if p.is_xtype(Type::Parameter) {
        mina = 0;
        maxa = 1;
    } else {
        return None;
    }

    if a < mina as usize || (maxa != -1 && a > maxa as usize) {
        None
    } else {
        Some((mina, maxa))
    }
}

pub fn check_proc_arity(a: usize, which: isize, args: &[Value], false_ok: bool) -> bool {
    let p = if which < 0 {
        args[0]
    } else {
        args[which as usize]
    };

    if false_ok && p.is_false() {
        return true;
    }

    if !p.is_procedure() || !get_or_check_arity(p, a).is_none() {
        return false;
    }

    true
}

pub fn check_proc_arity_or(
    _where_: &str,
    a: usize,
    which: isize,
    args: &[Value],
    false_ok: bool,
) -> Result<(), Value> {
    if !check_proc_arity(a, which, args, false_ok) {
        let pre;
        let post;
        if false_ok {
            pre = "(or/c ";
            post = " #f)";
        } else {
            pre = "";
            post = "";
        }
        let buf;
        match a {
            0 => buf = format!("{}(-> any){}", pre, post),

            1 => buf = format!("{}(any/c . -> . any){}", pre, post),

            2 => buf = format!("{}(any/c any/c . -> . any){}", pre, post),

            3 => buf = format!("{}(any/c any/c any/c . -> . any){}", pre, post),

            _ => buf = format!("{}(proceedure-arity-includes/c {}){}", pre, a, post),
        }

        todo!("error: {}", buf)
    }

    Ok(())
}

pub const SCM_PRIM_STRUCT_TYPE_INDEXLESS_GETTER: i32 = 32 | 256;
pub const SCM_PRIM_STRUCT_TYPE_CONSTR: i32 = 128;
pub const SCM_PRIM_STRUCT_TYPE_SIMPLE_CONSTR: i32 = 32 | 64 | 128;
pub const SCM_PRIM_STRUCT_TYPE_INDEXLESS_SETTER: i32 = 256;
pub const SCM_PRIM_STRUCT_TYPE_INDEXED_SETTER: i32 = 128 | 256;
pub const SCM_PRIM_STRUCT_TYPE_BROKEN_INDEXED_SETTER: i32 = 32 | 128;
pub const SCM_PRIM_TYPE_PARAMETER: i32 = 64;
pub const SCM_PRIM_TYPE_STRUCT_PROP_GETTER: i32 = 64 | 128;
pub const SCM_PRIM_STRUCT_TYPE_STRUCT_PROP_PRED: i32 = 64 | 128 | 256;
pub const SCM_PRIM_STRUCT_TYPE_INDEXED_GETTER: i32 = 32;
pub const SCM_PRIM_STRUCT_TYPE_PRED: i32 = 32 | 64;

extern "C" fn call_with_values(cfr: &mut CallFrame) -> ScmResult {
    let v = cfr.argument(0);
    let v1 = cfr.argument(1);
    if !v.is_procedure() {
        todo!("error")
    }

    if !v1.is_procedure() {
        todo!("error")
    }

    let v = match apply(v, &[]) {
        Ok(v) => v,
        Err(err) => return ScmResult::err(err),
    };

    
   
    if v.is_values() {
        ScmResult::tail(v1, &v.values())
    } else {
        ScmResult::tail(v1, &[v])
    }
}

extern "C" fn values(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(make_values(Thread::current(), cfr.arguments()))
}

pub(crate) fn init() {
    let module = scm_scheme_module().module();

    let subr = scm_make_subr("call-with-values", call_with_values, 2, 2);

    scm_define(module, "call-with-values".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("values", values, 0, MAX_ARITY);
    scm_define(module, "values".intern(), subr.into()).unwrap();

}
