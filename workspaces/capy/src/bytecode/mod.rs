pub mod label;
pub mod conversions;
pub mod virtual_register;


/// Register numbers used in bytecode operations have different meaning according to their ranges:
///      0x80000000-0xFFFFFFFF  Negative indices from the CallFrame pointer are entries in the call frame.
///      0x00000000-0x3FFFFFFF  Forwards indices from the CallFrame pointer are local vars and temporaries with the function's callframe.
///      0x40000000-0x7FFFFFFF  Positive indices from 0x40000000 specify entries in the constant pool on the CodeBlock.
pub const FIRST_CONSTANT_REGISTER_INDEX: i32 = 0x40000000;
pub const FIRST_CONSTANT_REGISTER_INDEX8: i32 = 16;
pub const FIRST_CONSTANT_REGISTER_INDEX16: i32 = 64;
pub const FIRST_CONSTANT_REGISTER_INDEX32: i32 = FIRST_CONSTANT_REGISTER_INDEX;


pub type OpcodeID = u8;



#[macro_use]
pub mod macros {
    include!(concat!(env!("OUT_DIR"), "/macros.rs"));
}


pub mod opcodes;
pub mod instruction_stream;
pub mod dumper;