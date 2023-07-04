use rsgc::{prelude::Handle, thread::Thread};

use crate::{
    compaux::scm_unwrap_identifier,
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
    vector::make_values, error::wrong_contract,
};

pub fn make_procedure(t: &mut Thread, code_block: Handle<CodeBlock>) -> Handle<Procedure> {
    /*t.allocate(Procedure {
        header: ObjectHeader::new(Type::Procedure),
        code: code_block,
        _pad: 0,
        env_size: 0,
        captures: [],
    })*/
    make_closed_procedure(t, code_block, 0)
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


pub fn check_arity(p: Value, a: i32, _inc_ok: bool) -> bool {
    let mina;
    let mut maxa;

    if p.is_native_procedure() {
        mina = p.native_procedure().mina as i32;
        maxa = p.native_procedure().maxa as i32;
    } else if p.is_vm_procedure() {
        mina = p.procedure().code.mina as i32;
        maxa = p.procedure().code.maxa as i32;
    } else {
        return false;
    }

    if maxa == MAX_ARITY as i32 {
        maxa = -1;
    }

    if a < mina || (maxa >= 0 && a > maxa) {
        return false;
    }

    true
}


pub fn check_proc_arity_2(
    where_: &str,
    a: i32,
    which: i32,
    argc: i32,
    args: &[Value],
    false_ok: bool,
) -> Result<bool, Value> {
    let p = if which < 0 {
        args[0]
    } else {
        args[which as usize]
    };

    if false_ok && p.is_false() {
        return Ok(true);
    }

    if !p.is_procedure() || !check_arity(p, a, false) {
        if where_.len() != 0 {
            let pre;
            let post;

            if false_ok {
                pre = "(or/c ";
                post = " #f)"
            } else {
                pre = "";
                post = ""
            }

            let expected = match a {
                0 => format!("{}(-> any){}", pre, post),
                1 => format!("{}(any/c . -> . any){}", pre, post),
                2 => format!("{}(any/c any/c . -> . any){}", pre, post),
                3 => format!("{}(any/c any/c any/c . -> . any){}", pre, post),
                _ => {
                    format!("{}(procedure-arity-includes/c {}){}", pre, a, post)
                }
            };

            return wrong_contract(where_, &expected, which, argc, args);
        } else {
            return Ok(false);
        }
    }

    Ok(true)
}

pub fn check_proc_arity(
    where_: &str,
    a: i32,
    which: i32,
    argc: i32,
    args: &[Value],
) -> Result<bool, Value> {
    check_proc_arity_2(where_, a, which, argc, args, false)
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

pub fn get_proc_name<'a>(val: Value) -> Option<&'a str> {
    if val.is_procedure() {
        if val.is_vm_procedure() {
            let proc = val.procedure();

            let id = if proc.code.name.is_wrapped_identifier() {
                Value::encode_object_value(scm_unwrap_identifier(proc.code.name.identifier()))
            } else if proc.code.name.is_symbol() {
                proc.code.name
            } else {
                return None;
            };

            Some(id.strsym())
        } else if val.is_native_procedure() {
            Some(val.native_procedure().name.strsym())
        } else {
            None
        }
    } else {
        None
    }
}

extern "C" fn procedure_p(cfr: &mut CallFrame) -> ScmResult {
    let v = cfr.argument(0);
    ScmResult::ok(v.is_procedure())
}

pub(crate) fn init() {
    let module = scm_scheme_module().module();

    let subr = scm_make_subr("call-with-values", call_with_values, 2, 2);

    scm_define(module, "call-with-values".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("values", values, 0, MAX_ARITY);
    scm_define(module, "values".intern(), subr.into()).unwrap();

    let subr = scm_make_subr("procedure?", procedure_p, 1, 1);
    scm_define(module, "procedure?".intern(), subr.into()).unwrap();
}