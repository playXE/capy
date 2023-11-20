#![allow(unused_variables)]

use std::mem::size_of;

use super::conversions::*;
use super::*;
use virtual_register::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
#[repr(C)]
pub struct BaseInstruction(u8);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub struct Impl {
    opcode: u8,
}

macro_rules! def_opcodes {
    (($name: ident, $size: expr) $($rest:tt)*) => {
        pub const $name: OpcodeID = 0;
        def_opcodes!(@parse 1; $($rest)*);
    };
    (@parse $ix: expr; ($name: ident, $size: expr) $($rest:tt)*) => {
        pub const $name: OpcodeID = $ix;
        def_opcodes!(@parse $ix + 1; $($rest)*);
    };
    (@parse $ix: expr;) => {}
}

for_each_bytecode_id!(def_opcodes);

pub const _: () = {
    assert!(size_of::<BaseInstruction>() == 1, "Pointer math");
};

impl BaseInstruction {
    pub unsafe fn narrow(&self) -> &Impl {
        &*(self as *const BaseInstruction as *const Impl)
    }

    pub unsafe fn narrow_mut(&mut self) -> &mut Impl {
        &mut *(self as *mut BaseInstruction as *mut Impl)
    }

    pub unsafe fn wide16(&self) -> *mut Impl {
        (self as *const BaseInstruction as *mut u8).offset(1) as *mut Impl
    }

    pub unsafe fn wide32(&self) -> *mut Impl {
        (self as *const BaseInstruction as *mut u8).offset(1) as *mut Impl
    }

    pub fn is_wide32(&self) -> bool {
        unsafe { self.narrow().opcode == OP_WIDE32 }
    }

    pub fn is_wide16(&self) -> bool {
        unsafe { self.narrow().opcode == OP_WIDE16 }
    }

    pub fn size_shift_amount(&self) -> usize {
        if self.is_wide32() {
            2
        } else if self.is_wide16() {
            1
        } else {
            0
        }
    }

    pub fn opcode_id_width(&self) -> usize {
        if self.is_wide32() {
            OpcodeIDWidthBySize::<{ OpcodeSize::Wide32 }>::VALUE
        } else if self.is_wide16() {
            OpcodeIDWidthBySize::<{ OpcodeSize::Wide16 }>::VALUE
        } else {
            OpcodeIDWidthBySize::<{ OpcodeSize::Narrow }>::VALUE
        }
    }

    pub fn opcode_id(&self) -> OpcodeID {
        if self.is_wide32() {
            unsafe { (*self.wide32()).opcode }
        } else if self.is_wide16() {
            unsafe { (*self.wide16()).opcode }
        } else {
            unsafe { self.narrow().opcode }
        }
    }

    pub fn size(&self) -> usize {
        let size_shift_amount = self.size_shift_amount();
        let prefix_size = if size_shift_amount != 0 { 1 } else { 0 };
        let operand_size = 1 << size_shift_amount;
        let size_of_bytecode = self.opcode_id_width();
        size_of_bytecode + OPCODE_LENGTHS[self.opcode_id() as usize] * operand_size + prefix_size
    }
}

use super::macros::NUMBER_OF_BYTECODE_IDS;
macro_rules! make_opcode_lengths {
    ($(($op: ident, $size: expr))*) => {
        pub const OPCODE_LENGTHS: [usize; NUMBER_OF_BYTECODE_IDS] = [$($size),*];
    };
}

macro_rules! make_opcode_names {
    ($($name :literal)*) => {
        pub const OPCODE_NAMES: &[&str] = &[$($name),*];  
    };
}

pub type OpcodeID = u8;
use crate::bytecompiler::BytecodeGenerator;

for_each_bytecode_id!(make_opcode_lengths);
for_each_bytecode_name!(make_opcode_names);
use super::dumper::*;
include!(concat!(env!("OUT_DIR"), "/structs.rs"));
