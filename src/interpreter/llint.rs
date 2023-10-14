use macroassembler::jit::gpr_info;

pub const SP: u8 = gpr_info::CS0;
pub const THREAD: u8 = gpr_info::CS1;
pub const JUMPTABLE: u8 = gpr_info::CS2;
pub const NUMBER_TAG: u8 = gpr_info::CS3;
pub const NOT_CELL_MASK: u8 = gpr_info::CS4;

pub const TMP0: u8 = gpr_info::T0;
pub const TMP1: u8 = gpr_info::T1;
pub const TMP2: u8 = gpr_info::T2;
pub const IP: u8 = gpr_info::T3;
