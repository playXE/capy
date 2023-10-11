#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueId(pub u32);

pub struct Value {
    pub id: ValueId,
    pub typ: Type,
    pub children: Vec<ValueId>,
    pub data: ValueData,
}

pub enum ValueData {
    Int32(i32),
    Int64(i64),
    Float(u32),
    Double(u64),
    None,
}

pub enum Type {
    Fixnum,
    Flonum,
    Vector,
    Bytevector,
    String,
    Symbol,
    Pair,
    Program,
    Tuple,
    AnyOf(Box<[Type]>),
    OneOf(Box<[Type]>),
    Any,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Opcode {
    Enter,
    Return,
    Move,
    Load,
    Store,
    Load8,
    Store8,
    Load8Z,
    Store8Z,
    Load16,
    Store16,
    Load16Z,
    Store16Z,
    Load32,
    Store32,
    Load32Z,
    Store32Z,

    /// Load variable from interpreter stack.
    LoadVar,

    GlobalRef,
    GlobalSet,
}

pub struct JITState {
    values: Vec<Value>,
}

impl JITState {
    pub fn add_value(&mut self, val: Value) -> ValueId {
        let id = self.values.len();
        self.values.push(val);
        ValueId(id as u32)
    }
}
