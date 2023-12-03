//! rloop
//!
//! Interpreter for Capy bytecode written in Rust.

use super::llint::{OPCODE_ID_NARROW_SIZE, OPCODE_ID_WIDE16_SIZE, OPCODE_ID_WIDE32_SIZE};
use super::{register::Register, stackframe::CallFrame};
use crate::bytecode::conversions::OpcodeSize;
use crate::bytecode::{macros::*, opcodes::*, *};
use crate::gc::ptr_compr::HeapCompressionScheme;
use crate::runtime::thread::Thread;
use std::ptr::null;

#[repr(C)]
pub enum Dispatch {
    Continue,
    Return(Register),
}

#[inline(always)]
unsafe fn get_operand<const SIZE: OpcodeSize>(pb: *const u8, pc: usize, index: usize) -> u32 {
    match SIZE {
        OpcodeSize::Narrow => pb.add(pc).add(index + OPCODE_ID_NARROW_SIZE).read() as _,
        OpcodeSize::Wide16 => pb
            .add(pc)
            .cast::<u16>()
            .add(index * 2 + OPCODE_ID_WIDE16_SIZE)
            .read_unaligned() as _,
        OpcodeSize::Wide32 => pb
            .add(pc)
            .cast::<u32>()
            .add(index * 4 + OPCODE_ID_WIDE32_SIZE)
            .read_unaligned() as _,
    }
}

unsafe fn get_register<const SIZE: OpcodeSize>(vr: u32, cfr: *mut CallFrame) -> Register {
    Register {
        compressed: match SIZE {
            OpcodeSize::Narrow => {
                if vr >= FIRST_CONSTANT_REGISTER_INDEX8 as u32 {
                    (*cfr)
                        .code_block()
                        .constant_pool
                        .get(vr as usize - FIRST_CONSTANT_REGISTER_INDEX8 as usize)
                } else {
                    (*cfr)
                        .registers()
                        .offset(vr as i8 as isize)
                        .read_unaligned()
                        .compressed
                }
            }

            OpcodeSize::Wide16 => {
                if vr >= FIRST_CONSTANT_REGISTER_INDEX16 as u32 {
                    (*cfr)
                        .code_block()
                        .constant_pool
                        .get(vr as usize - FIRST_CONSTANT_REGISTER_INDEX16 as usize)
                } else {
                    (*cfr)
                        .registers()
                        .offset(vr as i16 as isize)
                        .read_unaligned()
                        .compressed
                }
            }

            OpcodeSize::Wide32 => {
                if vr >= FIRST_CONSTANT_REGISTER_INDEX32 as u32 {
                    (*cfr)
                        .code_block()
                        .constant_pool
                        .get(vr as usize - FIRST_CONSTANT_REGISTER_INDEX32 as usize)
                } else {
                    (*cfr)
                        .registers()
                        .offset(vr as i32 as isize)
                        .read_unaligned()
                        .compressed
                }
            }
        },
    }
}

unsafe fn set_register<const SIZE: OpcodeSize>(vr: u32, cfr: *mut CallFrame, value: Register) {
    match SIZE {
        OpcodeSize::Narrow => {
            (*cfr)
                .registers_mut()
                .offset(vr as i8 as isize)
                .write(value);
        }

        OpcodeSize::Wide16 => {
            (*cfr)
                .registers_mut()
                .offset(vr as i16 as isize)
                .write(value);
        }

        OpcodeSize::Wide32 => {
            (*cfr)
                .registers_mut()
                .offset(vr as i32 as isize)
                .write(value);
        }
    }
}

type Handler = unsafe extern "C-unwind" fn(
    pb: *const u8,
    pc: usize,
    sp: *mut Register,
    cfr: *mut CallFrame,
    thread: &mut Thread,
    base: usize,
) -> Dispatch;

pub static mut OPCODE_TABLE_NARROW: [Handler; NUMBER_OF_BYTECODE_IDS] =
    [op_mov::<{ OpcodeSize::Narrow }>; NUMBER_OF_BYTECODE_IDS];
pub static mut OPCODE_TABLE_WIDE16: [Handler; NUMBER_OF_BYTECODE_IDS] =
    [op_mov::<{ OpcodeSize::Wide16 }>; NUMBER_OF_BYTECODE_IDS];
pub static mut OPCODE_TABLE_WIDE32: [Handler; NUMBER_OF_BYTECODE_IDS] =
    [op_mov::<{ OpcodeSize::Wide32 }>; NUMBER_OF_BYTECODE_IDS];

#[inline(always)]
unsafe extern "C-unwind" fn next_instruction(
    pb: *const u8,
    pc: usize,
    sp: *mut Register,
    cfr: *mut CallFrame,
    thread: &mut Thread,
    base: usize,
) -> Dispatch {
    let op = pb.add(pc).read();
    let handler = OPCODE_TABLE_NARROW.get_unchecked(op as usize);
    handler(pb, pc, sp, cfr, thread, base)
}

#[inline(always)]
unsafe extern "C-unwind" fn dispatch_op<const SIZE: OpcodeSize, const OP: u8>(
    pb: *const u8,
    mut pc: usize,
    sp: *mut Register,
    cfr: *mut CallFrame,
    thread: &mut Thread,
    base: usize,
) -> Dispatch {
    let advance = match SIZE {
        OpcodeSize::Narrow => OPCODE_LENGTHS[OP as usize] * 1 + OPCODE_ID_NARROW_SIZE,
        OpcodeSize::Wide16 => OPCODE_LENGTHS[OP as usize] * 2 + OPCODE_ID_WIDE16_SIZE,
        OpcodeSize::Wide32 => OPCODE_LENGTHS[OP as usize] * 4 + OPCODE_ID_WIDE32_SIZE,
    };

    pc += advance;
    next_instruction(pb, pc, sp, cfr, thread, base)
}

#[inline(always)]

unsafe extern "C-unwind" fn op_mov<const SIZE: OpcodeSize>(
    pb: *const u8,
    pc: usize,
    sp: *mut Register,
    cfr: *mut CallFrame,
    thread: &mut Thread,
    base: usize,
) -> Dispatch {
    let src = get_operand::<SIZE>(pb, pc, OP_MOV_SRC_INDEX);
    let src = get_register::<SIZE>(src, cfr);
    let dst = get_operand::<SIZE>(pb, pc, OP_MOV_DEST_INDEX);
    set_register::<SIZE>(dst, cfr, src);

    dispatch_op::<SIZE, OP_MOV>(pb, pc, sp, cfr, thread, base)
}

pub extern "C-unwind" fn rloop(sp: *mut Register, cfr: *mut CallFrame) -> Register {
    let pb = null::<u8>();
    let pc = 0usize;
    let heap_base = if cfg!(feature = "compressed-oops") {
        HeapCompressionScheme::base()
    } else {
        0
    };

    unsafe {
        loop {
            match next_instruction(pb, pc, sp, cfr, &mut Thread::current(), heap_base) {
                Dispatch::Continue => {}
                Dispatch::Return(r) => return r,
            }
        }
    }
}
