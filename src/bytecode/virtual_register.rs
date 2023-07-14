use std::{fmt::Display, mem::size_of};

use crate::{runtime::value::Value, vm::callframe::CallFrameSlot};

pub const FIRST_CONSTANT_REGISTER: i16 = 512;

/// Virtual register for our bytecode.
///
/// - local operands: `-32768 < 0`
/// - arguments: `0 <= x < 512`
/// - constants: `1024 <= x < 32768`
#[repr(transparent)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VirtualRegister(pub i16);

#[inline(always)]
pub const fn virtual_register_is_local(x: i16) -> bool {
    x < 0
}
#[inline(always)]
pub const fn virtual_register_is_argument(x: i16) -> bool {
    x >= 0
}

impl VirtualRegister {
    fn local_to_operand(local: i16) -> i16 {
        -1 - local
    }

    fn operand_to_local(operand: i16) -> i16 {
        -1 - operand as i16
    }

    fn operand_to_argument(operand: i16) -> i16 {
        operand - CallFrameSlot::FirstArgument as i16
    }

    fn argument_to_operand(argument: i16) -> i16 {
        argument + CallFrameSlot::FirstArgument as i16
    }
    #[inline(always)]
    pub fn is_local(self) -> bool {
        virtual_register_is_local(self.0)
    }
    #[inline(always)]
    pub fn is_argument(self) -> bool {
        virtual_register_is_argument(self.0)
    }

    #[inline(always)]
    pub fn is_constant(self) -> bool {
        self.0 >= FIRST_CONSTANT_REGISTER
    }

    #[inline(always)]
    pub fn is_header(self) -> bool {
        self.0 >= 0 && self.0 < CallFrameSlot::FirstArgument as i16
    }

    #[inline(always)]
    pub fn to_local(self) -> i16 {
        VirtualRegister::operand_to_local(self.0)
    }

    #[inline(always)]
    pub fn to_argument(self) -> i16 {
        VirtualRegister::operand_to_argument(self.0)
    }

    pub fn new_constant(constant_index: i16) -> Self {
        VirtualRegister(FIRST_CONSTANT_REGISTER + constant_index)
    }

    #[inline(always)]
    pub fn to_operand(self) -> i16 {
        self.0
    }

    #[inline(always)]
    pub fn to_constant_index(self) -> i16 {
        self.0 - FIRST_CONSTANT_REGISTER
    }

    #[inline(always)]
    pub fn offset(self) -> i16 {
        self.0
    }

    #[inline(always)]
    pub fn offset_in_bytes(self) -> isize {
        (self.0 as isize) * size_of::<Value>() as isize
    }
}

use std::ops::*;

use super::encode::{Decode, Encode};

impl Add<i16> for VirtualRegister {
    type Output = VirtualRegister;
    #[inline(always)]
    fn add(self, rhs: i16) -> Self::Output {
        VirtualRegister(self.0 + rhs)
    }
}

impl Add<VirtualRegister> for VirtualRegister {
    type Output = VirtualRegister;
    #[inline(always)]
    fn add(self, rhs: VirtualRegister) -> Self::Output {
        VirtualRegister(self.0 + rhs.0)
    }
}

impl AddAssign<i16> for VirtualRegister {
    #[inline(always)]
    fn add_assign(&mut self, rhs: i16) {
        self.0 += rhs;
    }
}

impl AddAssign<VirtualRegister> for VirtualRegister {
    #[inline(always)]
    fn add_assign(&mut self, rhs: VirtualRegister) {
        self.0 += rhs.0;
    }
}

impl Sub<i16> for VirtualRegister {
    type Output = VirtualRegister;
    #[inline(always)]
    fn sub(self, rhs: i16) -> Self::Output {
        VirtualRegister(self.0 - rhs)
    }
}

impl Sub<VirtualRegister> for VirtualRegister {
    type Output = VirtualRegister;
    #[inline(always)]
    fn sub(self, rhs: VirtualRegister) -> Self::Output {
        VirtualRegister(self.0 - rhs.0)
    }
}

pub fn virtual_register_for_local(x: i16) -> VirtualRegister {
    VirtualRegister(VirtualRegister::local_to_operand(x))
}

pub fn virtual_register_for_argument(x: i16) -> VirtualRegister {
    VirtualRegister(VirtualRegister::argument_to_operand(x))
}

impl Display for VirtualRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_local() {
            write!(f, "loc{}", self.to_local())
        } else if self.is_constant() {
            write!(f, "const{}", self.to_constant_index())
        } else if self.is_argument() {
            write!(f, "arg{}", self.to_argument())
        } else if self.is_header() {
            if self.0 == CallFrameSlot::CodeBlock as i16 {
                write!(f, "codeBlock")
            } else if self.0 == CallFrameSlot::Callee as i16 {
                write!(f, "callee")
            } else if self.0 == CallFrameSlot::ArgumentCount as i16 {
                write!(f, "argumentCount")
            } else if self.0 == 0 {
                write!(f, "callerFrame")
            } else {
                write!(f, "returnPC")
            }
        } else {
            unreachable!()
        }
    }
}

impl Encode for VirtualRegister {
    fn write(&self, gen: &mut crate::bytecompiler::bytecodegenerator::BytecodeGenerator) {
        gen.write_u16(self.0 as _);
    }
}

impl Decode for VirtualRegister {
    unsafe fn read(stream: *const u8) -> Self {
        VirtualRegister(*(stream as *const i16))
    }
}
