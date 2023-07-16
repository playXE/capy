use std::{marker::PhantomData, mem::size_of};

use crate::runtime::value::Value;

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
    pub(crate) caller: *mut CallFrame,
    pub(crate) return_pc: *const u8,
    pub(crate) code_block: Value,
    pub(crate) argc: Value,
    pub(crate) callee: Value,
    pub(crate) args: [Value; 0],
}

impl CallFrame {
    pub fn argument(&self, index: usize) -> Value {
        debug_assert!(index < self.argc.get_int32() as usize);
        unsafe { *self.args.as_ptr().add(index) }
    }

    pub fn argument_count(&self) -> usize {
        self.argc.get_int32() as usize
    }

    pub fn callee(&self) -> Value {
        self.callee
    }

    pub fn return_pc(&self) -> *const u8 {
        self.return_pc
    }

    pub fn code_block(&self) -> Value {
        self.code_block
    }

    pub fn caller(&self) -> *mut CallFrame {
        self.caller
    }

    pub fn arguments(&self) -> &[Value] {
        unsafe { std::slice::from_raw_parts(self.args.as_ptr(), self.argc.get_int32() as usize) }
    }

    pub fn arguments_mut(&mut self) -> &mut [Value] {
        unsafe {
            std::slice::from_raw_parts_mut(self.args.as_mut_ptr(), self.argc.get_int32() as usize)
        }
    }

    pub const HEADER_SIZE_IN_REGISTERS: usize = 5;
}

#[repr(C)]
pub struct ProtoCallFrame<'a> {
    pub code_block_value: Value,
    pub callee_value: Value,
    pub argcount: Value,
    pub args: *const Value,
    pub marker: PhantomData<&'a [Value]>,
}
