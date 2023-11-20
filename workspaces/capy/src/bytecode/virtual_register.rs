use std::mem::size_of;

use crate::interpreter::stackframe::{CallFrame, CallFrameSlot};

use super::FIRST_CONSTANT_REGISTER_INDEX;


#[repr(transparent)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VirtualRegister(pub i32);

pub const fn virtual_register_is_local(operand: i32) -> bool {
    operand < 0 
}

pub const fn virtual_register_is_argument(operand: i32) -> bool {
    operand >= 0
}

pub const fn virtual_register_for_local(local: i32) -> VirtualRegister {
    VirtualRegister(VirtualRegister::local_to_operand(local))
}

pub const fn virtual_register_for_argument(argument: i32, offset: i32) -> VirtualRegister {
    VirtualRegister(VirtualRegister::argument_to_operand(argument + offset))
}

impl VirtualRegister {
    const fn local_to_operand(local: i32) -> i32 {
        -1 - local 
    }

    const fn operand_to_local(operand: i32) -> i32 {
        -1 - operand
    }

    const fn operand_to_argument(operand: i32) -> i32 {
        operand - CallFrame::first_argument_offset()
    }

    const fn argument_to_operand(argument: i32) -> i32 {
        argument + CallFrame::first_argument_offset()
    }

    pub const INVALID_VIRTUAL_REGISTER: i32 = 0x3fffffff;

    pub const fn new(operand: i32) -> Self {
        Self(operand)
    }

    pub const fn is_valid(self) -> bool {
        self.0 != Self::INVALID_VIRTUAL_REGISTER
    }

    pub const fn is_local(self) -> bool {
        virtual_register_is_local(self.0)
    }

    pub const fn is_argument(self) -> bool {
        virtual_register_is_argument(self.0)
    }

    pub const fn is_header(self) -> bool {
        self.0 >= 0 && self.0 < CallFrameSlot::FirstArgument as i32 
    }

    pub const fn is_constant(self) -> bool {
        self.0 >= FIRST_CONSTANT_REGISTER_INDEX
    }

    pub const fn to_local(self) -> i32 {
        Self::operand_to_local(self.0)
    }

    pub const fn to_argument(self) -> i32 {
        Self::operand_to_argument(self.0)
    }

    pub const fn to_constant_index(self) -> i32 {
        self.0 - FIRST_CONSTANT_REGISTER_INDEX
    }

    pub const fn offset(self) -> i32 {
        self.0 
    }

    pub const fn offset_in_bytes(self) -> i32 {
        self.0 * size_of::<usize>() as i32
    }
}