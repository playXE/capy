//! A simple Template JIT
//! 
//! We use [macroassembler](https://docs.rs/macroassembler) which allows us to generate
//! code for x86-64, AArch64 and RISC-V. JIT simply maps interpreter opcodes
//! to native code. We use the same stack as VM does.

use macroassembler::assembler::*;
use macroassembler::jit::gpr_info;

pub const VM: u8 = gpr_info::CS0;
pub const SP: u8 = gpr_info::T0;
pub const FP: u8 = gpr_info::T1;
pub const OLD_FP_FOR_RETURN_TRAMPOLINE: u8 = gpr_info::CS1;

pub const T0: u8 = gpr_info::CS1;
pub const T1: u8 = gpr_info::CS2;
pub const T2: u8 = gpr_info::T2;
pub const T3_OR_FP: u8 = gpr_info::T1;
pub const T4_OR_SP: u8 = gpr_info::T0;
pub const T0_PRESERVED: u8 = gpr_info::CS1;
pub const T1_PRESERVERD: u8 = gpr_info::CS2;

pub const SP_IN_REGISTER: u32 = 0x1;
pub const FP_IN_REGISTER: u32 = 0x2;
pub const UNREACHABLE: u32 = 0x4;
pub const SP_CACHE_GPR: u32 = 0x8;
pub const SP_CACHE_FPR: u32 = 0x10;