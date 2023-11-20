use std::mem::size_of;

use super::{register::Register, StackElement};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub struct CallerFrameAndPC {
    pub caller_frame: *mut CallFrame,
    pub return_pc: *const u8,
}

impl CallerFrameAndPC {
    pub const SIZE_IN_REGISTERS: usize = size_of::<Self>() / size_of::<usize>();
}

///      Layout of CallFrame
///```text
///   |          ......            |   |
///   +----------------------------+   |
///   |           argN             |   v  lower address
///   +----------------------------+
///   |           arg1             |
///   +----------------------------+
///   |           arg0             |
///   +----------------------------+
///   |           this             |
///   +----------------------------+
///   |       argument count       |
///   +----------------------------+
///   |          callee            |
///   +----------------------------+
///   |        code block          |
///   +----------------------------+
///   |      return-address        |
///   +----------------------------+
///   |       callerFrame          |
///   +----------------------------+  <- callee's cfr is pointing this address
///   |          local0            |
///   +----------------------------+
///   |          local1            |
///   +----------------------------+
///   |          localN            |
///   +----------------------------+
///   |          ......            |
/// ```
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(usize)]
pub enum CallFrameSlot {
    CodeBlock = CallerFrameAndPC::SIZE_IN_REGISTERS,
    Callee = Self::CodeBlock as usize + 1,
    ArgumentCount = Self::Callee as usize + 1,
    FirstArgument = Self::ArgumentCount as usize + 1,
}

/// Represents the current state of program execution.
/// Passed as the first argument to most functions.
#[repr(C)]
pub struct CallFrame(Register);

impl std::ops::Deref for CallFrame {
    type Target = Register;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for CallFrame {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl CallFrame {
    pub const HEADER_SIZE_IN_REGISTERS: usize = CallFrameSlot::FirstArgument as usize;

    pub const fn argument_offset(argument: i32) -> i32 {
        CallFrameSlot::FirstArgument as i32 + argument
    }

    pub const fn first_argument_offset() -> i32 {
        CallFrameSlot::FirstArgument as i32
    }


    pub fn registers(&self) -> *const Register {
        self as *const Self as *const Register
    }

    pub fn registers_mut(&mut self) -> *mut Register {
        self as *mut Self as *mut Register
    }

    fn caller_frame_and_pc(&self) -> *const CallerFrameAndPC {
        self as *const Self as _
    }

    fn caller_frame_and_pc_mut(&mut self) -> *mut CallerFrameAndPC {
        self as *mut Self as _
    }

    fn arg_index_for_register(&self, reg: *const Register) -> isize {
        // The register at 'offset' number of slots from the frame pointer
        // i.e.
        //       reg = frame[offset];
        //   ==> reg = frame + offset;
        //   ==> offset = reg - frame;
        unsafe {
            let offset = reg.offset_from(self.registers()) as isize;
            // The offset is defined (based on argument_fffset()) to be:
            //       offset = CallFrameSlot::FirstArgument - arg_index;
            // Hence:
            //       arg_index = CallFrameSlot::FirstArgument - offset;
            let arg_index = offset - CallFrameSlot::FirstArgument as isize;
            arg_index
        }
    }
}
