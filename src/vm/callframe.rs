use std::mem::size_of;

use crate::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct CallerFrameAndPC {
    pub caller_frame: *mut CallFrame,
    pub return_pc: *const u8,
}

impl CallerFrameAndPC {
    pub const SIZE_IN_REGISTERS: usize = 2 * size_of::<usize>() / size_of::<Value>();
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(usize)]
pub enum CallFrameSlot {
    CodeBlock = CallerFrameAndPC::SIZE_IN_REGISTERS,
    Callee = Self::CodeBlock as usize + 1,
    ArgumentCount = Self::Callee as usize + 1,
    FirstArgument = Self::ArgumentCount as usize + 1,
}

///      Layout of CallFrame
/// ```text
///   |          ......            |   |
///   +----------------------------+   |
///   |           argN             |   v  lower address
///   +----------------------------+
///   |           arg1             |
///   +----------------------------+
///   |           arg0             |
///   +----------------------------+
///   |          callee            |
///   +----------------------------+
///   |       argument count       |
///   +----------------------------+
///   |        codeBlock           |
///   +----------------------------+
///   |      return-address        |
///   +----------------------------+
///   |       callerFrame          |
///   +----------------------------+  <- callee's cfr is pointing this address
///   |          sp - 0            |
///   +----------------------------+
///   |          sp - 1            |
///   +----------------------------+
///   |          sp - N            |
///   +----------------------------+
///   |          ......            |
/// ```

#[repr(C)]
pub struct CallFrame {
    pub caller: *mut CallFrame,
    pub return_pc: *const u8,
    pub code_block: Value,
    pub argc: Value,
    pub callee: Value,
    pub args: [Value; 0],
}

impl CallFrame {
    pub fn argument(&self, index: usize) -> Value {
        debug_assert!(index < self.argc.get_int32() as usize);
        unsafe {
            *self.args.as_ptr().add(index)
        }
    }
}