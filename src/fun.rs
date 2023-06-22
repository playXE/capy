use rsgc::{thread::Thread, prelude::Handle};

use crate::{object::{CodeBlock, Procedure, ObjectHeader, Type}, value::Value};

pub fn make_procedure(t: &mut Thread, code_block: Handle<CodeBlock>) -> Handle<Procedure> {
    t.allocate(Procedure {
        header: ObjectHeader::new(Type::Procedure),
        name: Value::encode_undefined_value(),
        code: code_block,
        env_size: 0,
        captures: []
    })
}

pub fn make_closed_procedure(t: &mut Thread, code_block: Handle<CodeBlock>, ncaptures: usize) -> Handle<Procedure> {
    let mut proc = t.allocate_varsize::<Procedure>(ncaptures);

    unsafe {
        let proc = proc.assume_init_mut();

        proc.header = ObjectHeader::new(Type::Procedure);
        proc.name = Value::encode_undefined_value();
        proc.code = code_block;
        proc.env_size = ncaptures as _;

        for i in 0..ncaptures {
            proc.captures.as_mut_ptr().add(i).write(Value::encode_undefined_value());
        }
    }

    unsafe {
        proc.assume_init()
    }
}