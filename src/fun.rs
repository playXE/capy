use rsgc::{prelude::Handle, thread::Thread};

use crate::{
    object::{
        ClosedNativeProcedure, CodeBlock, NativeProcedure, ObjectHeader, Procedure, ScmResult, Type,
    },
    value::Value,
    vm::callframe::CallFrame,
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

        proc.env_size = captures.len() as _;
        proc.captures
            .as_mut_ptr()
            .copy_from_nonoverlapping(captures.as_ptr(), captures.len());
    }

    unsafe { proc.assume_init() }
}
