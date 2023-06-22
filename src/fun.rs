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