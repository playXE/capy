#![allow(unused_imports)]
use std::{
    collections::HashMap,
    intrinsics::unreachable,
    mem::{offset_of, size_of},
    ptr::null,
};

use macroassembler::{
    assembler::{
        abstract_macro_assembler::{
            AbsoluteAddress, Address, BaseIndex, Extend, Jump, JumpList, Label, Operand, Scale,
        },
        link_buffer::LinkBuffer,
        x86assembler::INVALID_GPR,
        RelationalCondition, ResultCondition, TargetMacroAssembler,
    },
    jit::fpr_info::*,
    jit::gpr_info::*,
    wtf::executable_memory_handle::CodeRef,
};
use mmtk::util::{
    alloc::{AllocatorInfo, AllocatorSelector},
    metadata::side_metadata::GLOBAL_SIDE_METADATA_VM_BASE_ADDRESS,
};
use once_cell::sync::Lazy;

use crate::{
    bytecode::{
        conversions::OpcodeSize, macros::*, opcodes::*, OpcodeID, FIRST_CONSTANT_REGISTER_INDEX16,
        FIRST_CONSTANT_REGISTER_INDEX32, FIRST_CONSTANT_REGISTER_INDEX8,
    },
    gc::{ptr_compr::HeapCompressionScheme, CapyVM},
    runtime::{
        cell::{CellReference, OBJECT_REF_OFFSET},
        code_block::CodeBlock,
        thread::{MMTKLocalState, Thread},
        value::{
            self, undefined, Tagged, Word, HEAP_OBJECT_TAG, HEAP_OBJECT_TAG_MASK, SMI_SHIFT_SIZE,
            SMI_TAG, SMI_TAG_MASK, SMI_TAG_SIZE,
        },
        Runtime,
    },
};

use super::{
    entry_frame::VMEntryRecord,
    proto_callframe::ProtoCallFrame,
    register::{AsBits, Register},
    stackframe::{CallFrameSlot, CallerFrameAndPC, CALL_SITE_INDEX_OFFSET},
};

pub static LLIN_BASELINE_CALLEE_SAVE_REGISTERS: &[u8] = &[CS1, CS2, CS3, CS4];

const PTR_SIZE: usize = size_of::<usize>();
const MACHINE_REGISTER_SIZE: usize = size_of::<usize>();
const SLOT_SIZE: usize = size_of::<usize>();

const CALL_FRAME_HEADER_SLOTS: usize = 5;

const CALLER_FRAME_AND_PC_SIZE: usize = size_of::<CallerFrameAndPC>();
#[cfg(target_arch = "x86_64")]
const PROLOGUE_STACK_POINTER_DELTA: usize = size_of::<usize>();
#[cfg(not(target_arch = "x86_64"))]
const PROLOGUE_STACK_POINTER_DELTA: usize = 2 * size_of::<usize>();

pub const CALLER_FRAME: i32 = 0;
pub const RETURN_PC: i32 = CALLER_FRAME + MACHINE_REGISTER_SIZE as i32;
pub const CODE_BLOCK: i32 = RETURN_PC + MACHINE_REGISTER_SIZE as i32;
pub const CALLEE: i32 = CODE_BLOCK + MACHINE_REGISTER_SIZE as i32;
pub const ARGUMNET_COUNT: i32 = CALLEE + SLOT_SIZE as i32;
pub const FIRST_ARGUMENT_OFFSET: i32 = ARGUMNET_COUNT + SLOT_SIZE as i32;
pub const CALL_FRAME_HEADER_SIZE: usize = FIRST_ARGUMENT_OFFSET as usize;

pub const CFR: u8 = TargetMacroAssembler::FRAME_POINTER_REGISTER;
pub const SP: u8 = TargetMacroAssembler::STACK_POINTER_REGISTER;

#[cfg(any(target_arch = "arm64", target_arch = "riscv64"))]
pub const LR: u8 = TargetMacroAssembler::LINK_REGISTER;

pub const CALLEE_SAVE_SPACE_AS_VIRTUAL_REGISTERS: usize = 4;
pub const STACK_ALIGNMENT_BYTES: usize = 16;
pub const STACK_ALIGNMENT_SLOTS: usize = STACK_ALIGNMENT_BYTES / SLOT_SIZE;

pub const CALLEE_SAVE_SPACE_STACK_ALIGNED: usize =
    (CALLEE_SAVE_SPACE_AS_VIRTUAL_REGISTERS * SLOT_SIZE + STACK_ALIGNMENT_SLOTS - 1)
        & !(STACK_ALIGNMENT_SLOTS - 1);

pub const PC: u8 = T4;
cfg_if::cfg_if! {
    if #[cfg(any(target_arch="arm64", target_arch="riscv64"))] {
        pub const PB: u8 = CS7;
        pub const HEAP_BASE: u8 = CS8;
        pub const THREAD: u8 = CS9;
    } else if #[cfg(all(not(windows), target_arch="x86_64"))] {
        pub const PB: u8 = CS2;
        pub const HEAP_BASE: u8 = CS3;
        pub const THREAD: u8 = CS4;
        pub const EXTRA_TEMP_REG: u8 = T5;
    } else if #[cfg(all(windows, target_arch="x86_64"))] {
        pub const PB: u8 = CS0;
        pub const HEAP_BASE: u8 = CS1;
        pub const THREAD: u8 = CS2;
        pub const EXTRA_TEMP_REG: u8 = T0;
    } else {
        compile_error!("Unsupported target architecture");
    }
}

pub const SCALE: Scale = Scale::TimesEight;

pub const OPCODE_ID_NARROW_SIZE: usize = 1;
pub const OPCODE_ID_WIDE16_SIZE: usize = 2; // Wide16 prefix + OpcodeID
pub const OPCODE_ID_WIDE32_SIZE: usize = 2; // Wide32 prefix + OpcodeID

pub static mut OPCODE_MAP: [*const (); NUMBER_OF_BYTECODE_IDS] = [null(); NUMBER_OF_BYTECODE_IDS];
pub static mut OPCODE_MAP_WIDE16: [*const (); NUMBER_OF_BYTECODE_IDS] =
    [null(); NUMBER_OF_BYTECODE_IDS];
pub static mut OPCODE_MAP_WIDE32: [*const (); NUMBER_OF_BYTECODE_IDS] =
    [null(); NUMBER_OF_BYTECODE_IDS];

pub const CALLEE_SAVE_REGISTER_COUNT: usize = 0;
pub const CALLEE_SAVE_REGISTER_SIZE: usize = 0;

pub const VM_ENTRY_TOTAL_FRAME_SIZE: usize =
    (CALLEE_SAVE_REGISTER_SIZE + size_of::<VMEntryRecord>() + STACK_ALIGNMENT_BYTES - 1)
        & !(STACK_ALIGNMENT_BYTES - 1);

pub struct InterpreterGenerator {
    asm: TargetMacroAssembler,
    opcode_handlers: HashMap<OpcodeID, (Label, Label, Label)>,
    slowpaths: Vec<Box<dyn FnOnce(&mut Self)>>,
}

impl std::ops::Deref for InterpreterGenerator {
    type Target = TargetMacroAssembler;

    fn deref(&self) -> &Self::Target {
        &self.asm
    }
}

impl std::ops::DerefMut for InterpreterGenerator {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.asm
    }
}

macro_rules! get_operand_narrow {
    ($gen: expr, $field_index: expr, $dst: expr) => {
        $gen.load8(
            BaseIndex::new(
                PB,
                PC,
                Scale::TimesOne,
                $field_index as i32 + OPCODE_ID_NARROW_SIZE as i32,
                Extend::None,
            ),
            $dst,
        )
    };
}

macro_rules! get_operand_wide16 {
    ($gen: expr, $field_index: expr, $dst: expr) => {
        $gen.load16(
            BaseIndex::new(
                PB,
                PC,
                Scale::TimesOne,
                $field_index as i32 * 2 + OPCODE_ID_WIDE16_SIZE as i32,
                Extend::None,
            ),
            $dst,
        )
    };
}

macro_rules! get_operand_wide32 {
    ($gen: expr, $field_index: expr, $dst: expr) => {
        $gen.load32(
            BaseIndex::new(
                PB,
                PC,
                Scale::TimesOne,
                $field_index as i32 * 4 + OPCODE_ID_WIDE32_SIZE as i32,
                Extend::None,
            ),
            $dst,
        )
    };
}

macro_rules! get_operand {
    ($gen: expr, $size: expr, $field_index: expr, $dst: expr) => {
        match $size {
            OpcodeSize::Narrow => get_operand_narrow!($gen, $field_index, $dst),
            OpcodeSize::Wide16 => get_operand_wide16!($gen, $field_index, $dst),
            OpcodeSize::Wide32 => get_operand_wide32!($gen, $field_index, $dst),
        }
    };
}

impl InterpreterGenerator {
    pub fn new() -> Self {
        Self {
            asm: TargetMacroAssembler::new(),
            opcode_handlers: HashMap::new(),
            slowpaths: Vec::new(),
        }
    }

    pub fn next_instruction(&mut self) {
        self.load8(BaseIndex::new(PB, PC, Scale::TimesOne, 0, Extend::None), T0);
        self.mov(unsafe { OPCODE_MAP.as_ptr() as i64 }, T1);
        self.far_jump(BaseIndex::new(T1, T0, Scale::TimesEight, 0, Extend::None))
    }

    pub fn next_instruction_wide16(&mut self) {
        self.load8(BaseIndex::new(PB, PC, Scale::TimesOne, 0, Extend::None), T0);
        self.mov(unsafe { OPCODE_MAP_WIDE16.as_ptr() as i64 }, T1);
        self.far_jump(BaseIndex::new(T1, T0, Scale::TimesEight, 0, Extend::None))
    }

    pub fn next_instruction_wide32(&mut self) {
        self.load8(BaseIndex::new(PB, PC, Scale::TimesOne, 0, Extend::None), T0);
        self.mov(unsafe { OPCODE_MAP_WIDE32.as_ptr() as i64 }, T1);
        self.far_jump(BaseIndex::new(T1, T0, Scale::TimesEight, 0, Extend::None))
    }

    pub fn dispatch(&mut self, advance_reg: Operand) {
        self.add64(advance_reg, PC);
        self.next_instruction();
    }

    pub fn dispatch_indirect(&mut self, offset_reg: Operand) {
        self.dispatch(offset_reg);
    }

    pub fn get_operand(&mut self, size: OpcodeSize, offset: i32, dst: u8) {
        match size {
            OpcodeSize::Narrow => {
                self.load8(
                    BaseIndex::new(
                        PB,
                        PC,
                        Scale::TimesOne,
                        offset + OPCODE_ID_NARROW_SIZE as i32,
                        Extend::SExt32,
                    ),
                    dst,
                );
            }
            OpcodeSize::Wide16 => self.load16(
                BaseIndex::new(
                    PB,
                    PC,
                    Scale::TimesOne,
                    offset * 2 + OPCODE_ID_WIDE16_SIZE as i32,
                    Extend::SExt32,
                ),
                dst,
            ),
            OpcodeSize::Wide32 => self.load32(
                BaseIndex::new(
                    PB,
                    PC,
                    Scale::TimesOne,
                    offset * 4 + OPCODE_ID_WIDE32_SIZE as i32,
                    Extend::SExt32,
                ),
                dst,
            ),
        }
    }

    pub fn get_operand_u(&mut self, size: OpcodeSize, offset: i32, dst: u8) {
        match size {
            OpcodeSize::Narrow => {
                self.load8(
                    BaseIndex::new(
                        PB,
                        PC,
                        Scale::TimesOne,
                        offset + OPCODE_ID_NARROW_SIZE as i32,
                        Extend::None,
                    ),
                    dst,
                );
            }
            OpcodeSize::Wide16 => self.load16(
                BaseIndex::new(
                    PB,
                    PC,
                    Scale::TimesOne,
                    offset * 2 + OPCODE_ID_WIDE16_SIZE as i32,
                    Extend::None,
                ),
                dst,
            ),
            OpcodeSize::Wide32 => self.load32(
                BaseIndex::new(
                    PB,
                    PC,
                    Scale::TimesOne,
                    offset * 4 + OPCODE_ID_WIDE32_SIZE as i32,
                    Extend::None,
                ),
                dst,
            ),
        }
    }

    pub fn dispatch_op(&mut self, size: OpcodeSize, opcode_id: OpcodeID) {
        match size {
            OpcodeSize::Narrow => self.dispatch(Operand::Imm32(
                (OPCODE_LENGTHS[opcode_id as usize] * 1 + OPCODE_ID_NARROW_SIZE) as i32,
            )),
            OpcodeSize::Wide16 => self.dispatch(Operand::Imm32(
                (OPCODE_LENGTHS[opcode_id as usize] * 2 + OPCODE_ID_WIDE16_SIZE) as i32,
            )),
            OpcodeSize::Wide32 => self.dispatch(Operand::Imm32(
                (OPCODE_LENGTHS[opcode_id as usize] * 4 + OPCODE_ID_WIDE32_SIZE) as i32,
            )),
        }
    }

    fn register_opcode(
        &mut self,
        opcode_id: OpcodeID,
        narrow: Label,
        wide16: Label,
        wide32: Label,
    ) {
        self.opcode_handlers
            .insert(opcode_id, (narrow, wide16, wide32));
    }

    pub fn common_op(
        &mut self,
        opcode_id: OpcodeID,
        prologue: impl Fn(&mut Self),
        fun: impl Fn(&mut Self, OpcodeSize),
    ) {
        self.comment(format!("{}:", OPCODE_NAMES[opcode_id as usize]));
        let narrow = self.label();
        prologue(self);
        fun(self, OpcodeSize::Narrow);

        self.comment(format!("{}_wide16:", OPCODE_NAMES[opcode_id as usize]));
        let wide16 = self.label();
        prologue(self);
        fun(self, OpcodeSize::Wide16);

        self.comment(format!("{}_wide32:", OPCODE_NAMES[opcode_id as usize]));
        let wide32 = self.label();
        prologue(self);
        fun(self, OpcodeSize::Wide32);

        self.register_opcode(opcode_id, narrow, wide16, wide32);
    }
    /// Note: Only registers that are in RegisterSetBuilder::calleeSaveRegisters(),
    /// but are not in RegisterSetBuilder::vmCalleeSaveRegisters() need to be saved here,
    /// i.e.: only those registers that are callee save in the C ABI, but are not
    /// callee save in the JIT ABI.
    pub fn push_callee_saves(&mut self) {
        #[cfg(any(target_arch = "x86_64", target_arch = "arm64", target_arch = "riscv64"))]
        {}
    }

    pub fn pop_callee_saves(&mut self) {
        #[cfg(any(target_arch = "x86_64", target_arch = "arm64", target_arch = "riscv64"))]
        {}
    }

    pub fn preserve_caller_pc_and_cfr(&mut self) {
        #[cfg(target_arch = "x86_64")]
        {
            self.push_to_save(CFR);
        }

        #[cfg(any(target_arch = "riscv64", target_arch = "arm64"))]
        {
            self.push_pair(CFR, LR);
        }
    }

    pub fn restore_caller_pc_and_cfr(&mut self) {
        #[cfg(target_arch = "x86_64")]
        {
            self.pop_to_restore(CFR);
        }

        #[cfg(any(target_arch = "riscv64", target_arch = "arm64"))]
        {
            self.pop_pair(LR, CFR);
        }
    }

    pub fn preserve_callee_saves_used_by_llint(&mut self) {
        #[cfg(target_arch = "arm64")]
        {
            self.store_pair64(CS8, CS9, -16, CFR);
            self.store_pair64(CS6, CS7, -32, CFR);
        }

        #[cfg(all(windows, target_arch = "x86_64"))]
        {
            self.store64(CS6, Address::new(CFR, -8));
            self.store64(CS5, Address::new(CFR, -16));
            self.store64(CS4, Address::new(CFR, -24));
            self.store64(CS3, Address::new(CFR, -32));
        }

        #[cfg(all(not(windows), target_arch = "x86_64"))]
        {
            self.store64(CS4, Address::new(CFR, -8));
            self.store64(CS3, Address::new(CFR, -16));
            self.store64(CS2, Address::new(CFR, -24));
            self.store64(CS1, Address::new(CFR, -32));
        }

        #[cfg(target_arch = "riscv64")]
        {
            self.store64(CS9, Address::new(CFR, -8));
            self.store64(CS8, Address::new(CFR, -16));
            self.store64(CS7, Address::new(CFR, -24));
            self.store64(CS6, Address::new(CFR, -32));
        }
    }

    pub fn restore_callee_saves_used_by_llint(&mut self) {
        #[cfg(target_arch = "arm64")]
        {
            self.load_pair64(-32, CFR, CS6, CS7);
            self.load_pair64(-16, CFR, CS8, CS9);
        }

        #[cfg(all(windows, target_arch = "x86_64"))]
        {
            self.load64(Address::new(CFR, -8), CS6);
            self.load64(Address::new(CFR, -16), CS5);
            self.load64(Address::new(CFR, -24), CS4);
            self.load64(Address::new(CFR, -32), CS3);
        }

        #[cfg(all(not(windows), target_arch = "x86_64"))]
        {
            self.load64(Address::new(CFR, -8), CS4);
            self.load64(Address::new(CFR, -16), CS3);
            self.load64(Address::new(CFR, -24), CS2);
            self.load64(Address::new(CFR, -32), CS1);
        }

        #[cfg(target_arch = "riscv64")]
        {
            self.load64(Address::new(CFR, -8), CS9);
            self.load64(Address::new(CFR, -16), CS8);
            self.load64(Address::new(CFR, -24), CS7);
            self.load64(Address::new(CFR, -32), CS6);
        }
    }

    pub fn preserve_return_address_after_call(&mut self, destination_register: u8) {
        #[cfg(any(target_arch = "arm64", target_arch = "riscv64"))]
        {
            self.mov(LR, destination_register);
        }
        #[cfg(target_arch = "x86_64")]
        {
            self.pop(destination_register);
        }
    }

    pub fn function_prologue(&mut self) {
        #[cfg(target_arch = "x86_64")]
        {
            self.push(CFR);
        }

        #[cfg(any(target_arch = "arm64", target_arch = "riscv64"))]
        {
            self.push_pair(CFR, LR);
        }

        self.mov(SP, CFR);
    }

    pub fn function_epilogue(&mut self) {
        #[cfg(target_arch = "x86_64")]
        {
            self.pop(CFR);
        }

        #[cfg(any(target_arch = "arm64", target_arch = "riscv64"))]
        {
            self.pop_pair(CFR, LR);
        }

        self.ret();
    }

    pub fn vm_entry_record(&mut self, entry_fp: u8, result_reg: u8) {
        self.sub64_rrr(entry_fp, VM_ENTRY_TOTAL_FRAME_SIZE as i32, result_reg);
    }

    pub fn copy_callee_saves_to_entry_frame_callee_saves_buffer(&mut self, entry_frame: u8) {
        #[cfg(target_arch = "arm64")]
        {
            self.store_pair64(CS0, CS1, entry_frame, 0);
            self.store_pair64(CS2, CS3, entry_frame, 16);
            self.store_pair64(CS4, CS5, entry_frame, 32);
            self.store_pair64(CS6, CS7, entry_frame, 48);
            self.store_pair64(CS8, CS9, entry_frame, 64);
            self.store_pair_double(FPREG_CS0, FPREG_CS1, entry_frame, 80);
            self.store_pair_double(FPREG_CS2, FPREG_CS3, entry_frame, 96);
            self.store_pair_double(FPREG_CS4, FPREG_CS5, entry_frame, 112);
            self.store_pair_double(FPREG_CS6, FPREG_CS7, entry_frame, 128);
        }

        #[cfg(all(target_arch = "x86_64", not(windows)))]
        {
            self.store64(CS0, Address::new(entry_frame, 0));
            self.store64(CS1, Address::new(entry_frame, 8));
            self.store64(CS2, Address::new(entry_frame, 16));
            self.store64(CS3, Address::new(entry_frame, 24));
            self.store64(CS4, Address::new(entry_frame, 32));
        }

        #[cfg(all(target_arch = "x86_64", windows))]
        {
            self.store64(CS0, Address::new(entry_frame, 0));
            self.store64(CS1, Address::new(entry_frame, 8));
            self.store64(CS2, Address::new(entry_frame, 16));
            self.store64(CS3, Address::new(entry_frame, 24));
            self.store64(CS4, Address::new(entry_frame, 32));
            self.store64(CS5, Address::new(entry_frame, 40));
            self.store64(CS6, Address::new(entry_frame, 48));
        }

        #[cfg(target_arch = "riscv64")]
        {
            self.store64(CS0, Address::new(entry_frame, 0));
            self.store64(CS1, Address::new(entry_frame, 8));
            self.store64(CS2, Address::new(entry_frame, 16));
            self.store64(CS3, Address::new(entry_frame, 24));
            self.store64(CS4, Address::new(entry_frame, 32));
            self.store64(CS5, Address::new(entry_frame, 40));
            self.store64(CS6, Address::new(entry_frame, 48));
            self.store64(CS7, Address::new(entry_frame, 56));
            self.store64(CS8, Address::new(entry_frame, 64));
            self.store64(CS9, Address::new(entry_frame, 72));
            self.store64(CS10, Address::new(entry_frame, 80));
            self.store_double(FPREG_CS0, Address::new(entry_frame, 88));
            self.store_double(FPREG_CS1, Address::new(entry_frame, 96));
            self.store_double(FPREG_CS2, Address::new(entry_frame, 104));
            self.store_double(FPREG_CS3, Address::new(entry_frame, 112));
            self.store_double(FPREG_CS4, Address::new(entry_frame, 120));
            self.store_double(FPREG_CS5, Address::new(entry_frame, 128));
            self.store_double(FPREG_CS6, Address::new(entry_frame, 136));
            self.store_double(FPREG_CS7, Address::new(entry_frame, 144));
            self.store_double(FPREG_CS8, Address::new(entry_frame, 152));
            self.store_double(FPREG_CS9, Address::new(entry_frame, 160));
            self.store_double(FPREG_CS10, Address::new(entry_frame, 168));
            self.store_double(FPREG_CS11, Address::new(entry_frame, 176));
        }
    }

    pub fn copy_callee_saves_to_vm_entry_callee_saves_buffer(&mut self, vm: u8, temp: u8) {
        self.load64(
            Address::new(vm, offset_of!(Thread, top_entry_frame) as i32),
            temp,
        );
        self.copy_callee_saves_to_entry_frame_callee_saves_buffer(temp);
    }

    pub fn resstore_callee_saves_from_vm_entry_frame_callee_saves_buffer(
        &mut self,
        vm: u8,
        temp: u8,
    ) {
        self.load64(
            Address::new(vm, offset_of!(Thread, top_entry_frame) as i32),
            temp,
        );
        self.vm_entry_record(temp, temp);
        self.lea64(
            Address::new(temp, offset_of!(VMEntryRecord, callee_save_buffer) as i32),
            temp,
        );

        #[cfg(target_arch = "arm64")]
        {
            self.load_pair64(0, temp, CS0, CS1);
            self.load_pair64(16, temp, CS2, CS3);
            self.load_pair64(32, temp, CS4, CS5);
            self.load_pair64(48, temp, CS6, CS7);
            self.load_pair64(64, temp, CS8, CS9);
            self.load_pair_double(80, temp, FPREG_CS0, FPREG_CS1);
            self.load_pair_double(96, temp, FPREG_CS2, FPREG_CS3);
            self.load_pair_double(112, temp, FPREG_CS4, FPREG_CS5);
            self.load_pair_double(128, temp, FPREG_CS6, FPREG_CS7);
        }

        #[cfg(all(target_arch = "x86_64", not(windows)))]
        {
            self.load64(Address::new(temp, 0), CS0);
            self.load64(Address::new(temp, 8), CS1);
            self.load64(Address::new(temp, 16), CS2);
            self.load64(Address::new(temp, 24), CS3);
            self.load64(Address::new(temp, 32), CS4);
        }

        #[cfg(all(target_arch = "x86_64", windows))]
        {
            self.load64(Address::new(temp, 0), CS0);
            self.load64(Address::new(temp, 8), CS1);
            self.load64(Address::new(temp, 16), CS2);
            self.load64(Address::new(temp, 24), CS3);
            self.load64(Address::new(temp, 32), CS4);
            self.load64(Address::new(temp, 40), CS5);
            self.load64(Address::new(temp, 48), CS6);
        }

        #[cfg(target_arch = "riscv64")]
        {
            self.load64(Address::new(temp, 0), CS0);
            self.load64(Address::new(temp, 8), CS1);
            self.load64(Address::new(temp, 16), CS2);
            self.load64(Address::new(temp, 24), CS3);
            self.load64(Address::new(temp, 32), CS4);
            self.load64(Address::new(temp, 40), CS5);
            self.load64(Address::new(temp, 48), CS6);
            self.load64(Address::new(temp, 56), CS7);
            self.load64(Address::new(temp, 64), CS8);
            self.load64(Address::new(temp, 72), CS9);
            self.load64(Address::new(temp, 80), CS10);
            self.load_double(Address::new(temp, 88), FPREG_CS0);
            self.load_double(Address::new(temp, 96), FPREG_CS1);
            self.load_double(Address::new(temp, 104), FPREG_CS2);
            self.load_double(Address::new(temp, 112), FPREG_CS3);
            self.load_double(Address::new(temp, 120), FPREG_CS4);
            self.load_double(Address::new(temp, 128), FPREG_CS5);
            self.load_double(Address::new(temp, 136), FPREG_CS6);
            self.load_double(Address::new(temp, 144), FPREG_CS7);
            self.load_double(Address::new(temp, 152), FPREG_CS8);
            self.load_double(Address::new(temp, 160), FPREG_CS9);
            self.load_double(Address::new(temp, 168), FPREG_CS10);
            self.load_double(Address::new(temp, 176), FPREG_CS11);
        }
    }

    pub fn load_word(&mut self, src: impl Into<Operand>, dest: u8) {
        if cfg!(feature = "compressed-oops") {
            self.load32(src, dest);
        } else {
            self.load64(src, dest);
        }
    }

    pub fn store_word(&mut self, src: u8, dest: impl Into<Operand>) {
        if cfg!(feature = "compressed-oops") {
            self.store32(src, dest);
        } else {
            self.store64(src, dest);
        }
    }

    pub fn get_frame_register_size_for_code_block(&mut self, code_block: u8, size: u8) {
        self.load_word(
            Address::new(code_block, offset_of!(CodeBlock, num_callee_locals) as i32),
            size,
        );
        self.lshift64(3i32, size);
        //        self.add64(0i32, size);
    }

    pub fn restore_stack_pointer_after_call(&mut self) {
        self.load64(Address::new(CFR, CODE_BLOCK), T2);
        self.get_frame_register_size_for_code_block(T2, T2);
        self.sub64_rrr(CFR, T2, SP);
    }

    pub fn store_pc(&mut self) {
        self.store32(PC, Address::new(CFR, CALL_SITE_INDEX_OFFSET as i32));
    }

    pub fn load_pc(&mut self) {
        self.load32(Address::new(CFR, CALL_SITE_INDEX_OFFSET as i32), PC);
    }

    pub fn dispatch_after_regular_call(
        &mut self,
        size: OpcodeSize,
        dst_virtual_register: i32,
        dispatch: impl Fn(&mut Self),
    ) {
        self.load_pc();
        self.get_operand(size, dst_virtual_register, T1);
        self.store_word(
            RETURN_VALUE_GPR,
            BaseIndex::new(CFR, T1, SCALE, 0, Extend::None),
        );
        dispatch(self);
    }

    pub fn dispatch_after_tail_call(
        &mut self,
        size: OpcodeSize,
        dst_virtual_register: i32,
        dispatch: impl Fn(&mut Self),
    ) {
        self.load_pc();
        self.get_operand(size, dst_virtual_register, T1);
        self.store_word(
            RETURN_VALUE_GPR,
            BaseIndex::new(CFR, T1, SCALE, 0, Extend::None),
        );
        dispatch(self);
    }

    pub fn load_constant(&mut self, size: OpcodeSize, index: u8, value: u8) {
        self.load64(Address::new(CFR, CODE_BLOCK as i32), value);
        self.untag_object(value, value);
        self.load64(
            Address::new(value, offset_of!(CodeBlock, constant_pool) as i32),
            value,
        );
        self.untag_object(value, value);
        match size {
            OpcodeSize::Narrow => {
                self.load_word(
                    BaseIndex::new(
                        value,
                        index,
                        SCALE,
                        -(FIRST_CONSTANT_REGISTER_INDEX8 as i32 * size_of::<Word>() as i32)
                            + size_of::<Word>() as i32,
                        Extend::None,
                    ),
                    value,
                );
            }

            OpcodeSize::Wide16 => {
                self.load_word(
                    BaseIndex::new(
                        value,
                        index,
                        SCALE,
                        -(FIRST_CONSTANT_REGISTER_INDEX16 as i32 * size_of::<Word>() as i32)
                            + size_of::<Word>() as i32,
                        Extend::None,
                    ),
                    value,
                );
            }

            OpcodeSize::Wide32 => {
                self.sub64(FIRST_CONSTANT_REGISTER_INDEX32 as i32, index);
                self.load_word(BaseIndex::new(value, index, SCALE, 0, Extend::None), value);
            }
        }
    }

    pub fn load_constant_or_variable(&mut self, size: OpcodeSize, index: u8, value: u8) {
        let constant = match size {
            OpcodeSize::Narrow => {
                let j = self.branch64(
                    RelationalCondition::GreaterThanOrEqual,
                    index,
                    FIRST_CONSTANT_REGISTER_INDEX8 as i32,
                );
                self.load64(BaseIndex::new(CFR, index, SCALE, 0, Extend::None), value);
                j
            }

            OpcodeSize::Wide16 => {
                let j = self.branch64(
                    RelationalCondition::GreaterThanOrEqual,
                    index,
                    FIRST_CONSTANT_REGISTER_INDEX16 as i32,
                );
                self.load64(BaseIndex::new(CFR, index, SCALE, 0, Extend::None), value);
                j
            }

            OpcodeSize::Wide32 => {
                let j = self.branch64(
                    RelationalCondition::GreaterThanOrEqual,
                    index,
                    FIRST_CONSTANT_REGISTER_INDEX32 as i32,
                );
                self.load64(BaseIndex::new(CFR, index, SCALE, 0, Extend::None), value);
                j
            }
        };
        let done = self.jump();
        constant.link(&mut self.asm);
        self.load_constant(size, index, value);
        done.link(&mut self.asm);
    }

    pub fn prepare_state_for_ccall(&mut self) {
        self.add64(PB, PC);
    }

    pub fn restore_state_after_ccall(&mut self) {
        self.mov(RETURN_VALUE_GPR, PC);
        self.sub64(PB, PC);
    }

    pub fn ccall3(&mut self, func: impl Into<Operand>) {
        cfg_if::cfg_if! {
            if #[cfg(all(windows, target_arch="x86_64"))]
            {
                self.mov(ARGUMENT_GPR2, ARGUMENT_GPR3);
                self.mov(ARGUMENT_GPR1, ARGUMENT_GPR2);
                self.mov(ARGUMENT_GPR0, ARGUMENT_GPR1);
                self.sub64(64i32, SP);
                self.mov(SP, ARGUMENT_GPR0);
                self.add64(32i32, ARGUMENT_GPR0);
                self.asm.call_op(Some(func));
                self.mov(Address::new(RETURN_VALUE_GPR, 8), RETURN_VALUE_GPR2);
                self.mov(Address::new(RETURN_VALUE_GPR, 0), RETURN_VALUE_GPR);
                self.add64(64i32, SP);
            } else {
                self.asm.call_op(Some(func));
            }
        }
    }

    pub fn ccall2(&mut self, func: impl Into<Operand>) {
        cfg_if::cfg_if! {
            if #[cfg(all(windows, target_arch="x86_64"))]
            {
                self.mov(ARGUMENT_GPR1, ARGUMENT_GPR2);
                self.mov(ARGUMENT_GPR0, ARGUMENT_GPR1);
                self.sub64(64i32, SP);
                self.mov(SP, ARGUMENT_GPR0);
                self.add64(32i32, ARGUMENT_GPR0);
                self.asm.call_op(Some(func));
                self.mov(Address::new(RETURN_VALUE_GPR, 8), RETURN_VALUE_GPR2);
                self.mov(Address::new(RETURN_VALUE_GPR, 0), RETURN_VALUE_GPR);
                self.add64(64i32, SP);
            } else {
                self.asm.call_op(Some(func));
            }
        }
    }

    pub fn call_slow_path(&mut self, slow_path: impl Into<Operand>) {
        self.prepare_state_for_ccall();
        self.mov(THREAD, ARGUMENT_GPR0);
        self.mov(CFR, ARGUMENT_GPR1);
        self.mov(PC, ARGUMENT_GPR2);
        self.ccall3(slow_path);
        self.restore_state_after_ccall();
    }

    /// Call a slow-path for call opcodes
    pub fn call_call_slow_path(&mut self, slow_path: impl Into<Operand>) {
        self.store_pc();
        self.prepare_state_for_ccall();
        self.mov(THREAD, ARGUMENT_GPR0);
        self.mov(CFR, ARGUMENT_GPR1);
        self.mov(PC, ARGUMENT_GPR2);
        self.ccall3(slow_path);
    }

    pub fn load_variable(&mut self, size: OpcodeSize, field_index: usize, value_reg: u8) {
        get_operand!(self, size, field_index, value_reg);
        self.load_word(
            BaseIndex::new(CFR, value_reg, SCALE, 0, Extend::None),
            value_reg,
        );
    }

    pub fn store_variable(&mut self, size: OpcodeSize, field_index: usize, value_reg: u8) {
        get_operand!(self, size, field_index, value_reg);
        self.store_word(
            value_reg,
            BaseIndex::new(CFR, value_reg, SCALE, 0, Extend::None),
        );
    }

    pub fn write_barrier(&mut self, dst: Address, val: u8, tmp1: u8, tmp2: u8) {
        let obj = dst.base;

        // tmp2 = load-byte (GLOBAL_VM_SIDE_METADATA_ADDRESS + (obj >> 6))
        self.mov(obj, tmp1);
        self.rshift64(6i32, tmp1);
        self.mov(GLOBAL_SIDE_METADATA_VM_BASE_ADDRESS.as_usize() as i64, tmp2);
        self.load8(
            BaseIndex::new(tmp2, tmp1, Scale::TimesOne, 0, Extend::None),
            tmp2,
        );
        // tmp1 = (obj >> 3) & 7
        self.mov(obj, tmp1);
        self.rshift64(3i32, tmp1);
        self.and64(7i32, tmp1);
        // tmp2 = tmp2 >> tmp1
        self.rshift64_rrr(tmp2, tmp1, tmp2);
        self.and64(1i32, tmp2);
        // if (tmp2 & 1) == 1 then goto slowpath;
        let done = self.branch64(RelationalCondition::NotEqual, tmp2, 1i32);

        if val == INVALID_GPR {
            self.mov(0i64, ARGUMENT_GPR2);
        } else {
            self.mov(val, ARGUMENT_GPR2);
        }
        self.mov(obj, ARGUMENT_GPR0);
        self.lea64(dst, ARGUMENT_GPR1);

        self.ccall3(AbsoluteAddress::new(
            super::slow_paths::object_reference_write_slow_call as _,
        ));
        done.link(&mut self.asm);
    }

    pub fn bump_allocate(
        &mut self,
        thread: u8,
        obj: u8,
        var_size_in_bytes: u8,
        con_size_in_bytes: usize,
        t1: u8,
    ) -> JumpList {
        let max_non_los_bytes = Thread::current().mmtk().los_threshold;

        let mut jl = JumpList::new();
        let selector = mmtk::memory_manager::get_allocator_mapping(
            Runtime::get().mmtk(),
            mmtk::AllocationSemantics::Default,
        );
        let info = AllocatorInfo::new::<CapyVM>(selector);

        match info {
            AllocatorInfo::None | AllocatorInfo::Unimplemented => {
                jl.push(self.jump());
                return jl;
            }
            _ => (),
        }
        let offset_of_mmtk = offset_of!(Thread, mmtk);
        let offset_of_limit = offset_of_mmtk + offset_of!(MMTKLocalState, bump_limit);
        let offset_of_top = offset_of_mmtk + offset_of!(MMTKLocalState, bump_pointer);

        if var_size_in_bytes == INVALID_GPR {
            if con_size_in_bytes > max_non_los_bytes {
                jl.push(self.jump());
                return jl;
            }
        } else {
            jl.push(self.branch64(
                RelationalCondition::AboveOrEqual,
                var_size_in_bytes,
                max_non_los_bytes as i32,
            ));
        }

        let limit = Address::new(thread, offset_of_limit as i32);
        let top = Address::new(thread, offset_of_top as i32);

        self.load64(top, obj);
        let end = t1;
        if var_size_in_bytes == INVALID_GPR {
            self.lea64(Address::new(obj, con_size_in_bytes as i32), end);
        } else {
            self.add64_rrr(var_size_in_bytes as i32, obj, end);
        }
        // slowpath if end < obj
        jl.push(self.branch64(RelationalCondition::Below, end, obj));
        // slowpath if end > limit
        jl.push(self.branch64(RelationalCondition::Above, end, limit));
        // *cursor = end
        self.store64(end, top);
        self.add64(OBJECT_REF_OFFSET as i32, obj); // object start at obj + 8
        jl
    }

    pub fn tag_object(&mut self, val: u8, dest: u8) {
        self.add64_rrr(HEAP_OBJECT_TAG as i32, val, dest);
    }

    pub fn untag_object(&mut self, src: u8, dest: u8) {
        self.sub64_rrr(src, HEAP_OBJECT_TAG as i32, dest);
    }

    pub fn decompress(&mut self, src: u8, dest: u8) {
        if cfg!(feature = "compressed-oops") {
            self.add64_rrr(src, HEAP_BASE, dest);
        } else {
            self.mov(src, dest);
        }
    }

    pub fn compress(&mut self, src: u8, dest: u8) {
        if cfg!(feature = "compressed-oops") {
            self.move32_if_needed(src, dest);
        } else {
            self.mov(src, dest);
        }
    }

    pub fn untag_and_decompress(&mut self, src: u8, dest: u8) {
        if cfg!(feature = "compressed-oops") {
            cfg_if::cfg_if! {
                if #[cfg(target_arch="x86_64")]
                {
                    self.x86_lea64(BaseIndex::new(HEAP_BASE, src, Scale::TimesOne, -(HEAP_OBJECT_TAG as i32), Extend::None), dest);
                } else {
                    self.add64_rrr(src, HEAP_BASE, dest);
                    self.sub64(HEAP_OBJECT_TAG as i32, dest);
                }
            }
        } else {
            self.sub64_rrr(src, HEAP_OBJECT_TAG as i32, dest);
        }
    }

    pub fn smi_to_int(&mut self, src: u8, dest: u8) {
        let shift_bits = SMI_TAG_SIZE + SMI_SHIFT_SIZE;
        self.rshift32_rrr(src, shift_bits as i32, dest);
    }

    pub fn int_to_smi(&mut self, src: u8, dest: u8) {
        let shift_bits = SMI_TAG_SIZE + SMI_SHIFT_SIZE;
        let tag = SMI_TAG;
        self.lshift32_rrr(src, shift_bits as i32, dest);
        self.or32(tag as i32, dest);
    }

    pub fn branch_smi_tag(&mut self, value: u8) -> Jump {
        if size_of::<Word>() == 4 {
            self.branch_test32(ResultCondition::Zero, value, SMI_TAG_MASK as i32)
        } else {
            self.branch_test64(ResultCondition::Zero, value, SMI_TAG_MASK as i32)
        }
    }

    pub fn branch_heap_object_tag(&mut self, value: u8) -> Jump {
        if size_of::<Word>() == 4 {
            self.branch_test32(ResultCondition::NonZero, value, HEAP_OBJECT_TAG_MASK as i32)
        } else {
            self.branch_test64(ResultCondition::NonZero, value, HEAP_OBJECT_TAG_MASK as i32)
        }
    }

    pub fn branch_not_smi_tag(&mut self, value: u8) -> Jump {
        if size_of::<Word>() == 4 {
            self.branch_test32(ResultCondition::NonZero, value, SMI_TAG_MASK as i32)
        } else {
            self.branch_test64(ResultCondition::NonZero, value, SMI_TAG_MASK as i32)
        }
    }

    pub fn branch_not_heap_object_tag(&mut self, value: u8) -> Jump {
        if size_of::<Word>() == 4 {
            self.branch_test32(ResultCondition::Zero, value, HEAP_OBJECT_TAG_MASK as i32)
        } else {
            self.branch_test64(ResultCondition::Zero, value, HEAP_OBJECT_TAG_MASK as i32)
        }
    }

    pub fn do_vm_entry(&mut self, make_call: impl FnOnce(&mut Self, u8, u8, u8, u8)) {
        self.function_prologue();
        self.push_callee_saves();

        let entry = ARGUMENT_GPR0;
        let thread = ARGUMENT_GPR1;
        let proto_callframe = ARGUMENT_GPR2;

        self.vm_entry_record(CFR, SP);

        self.store64(
            thread,
            Address::new(SP, offset_of!(VMEntryRecord, thread) as i32),
        );

        self.load64(
            Address::new(thread, offset_of!(Thread, top_call_frame) as i32),
            T4,
        );
        self.store64(
            T4,
            Address::new(SP, offset_of!(VMEntryRecord, prev_top_call_frame) as i32),
        );
        self.load64(
            Address::new(thread, offset_of!(Thread, top_entry_frame) as i32),
            T4,
        );
        self.store64(
            T4,
            Address::new(SP, offset_of!(VMEntryRecord, prev_top_entry_frame) as i32),
        );

        self.load32(
            Address::new(
                proto_callframe,
                offset_of!(ProtoCallFrame, padded_arg_count) as i32,
            ),
            T4,
        );
        self.add64(CALL_FRAME_HEADER_SLOTS as i32, T4);
        self.lshift64(3i32, T4);
        self.sub64_rrr(SP, T4, T3);

        // copy header
        self.load64(
            Address::new(
                proto_callframe,
                offset_of!(ProtoCallFrame, code_block_value) as i32,
            ),
            EXTRA_TEMP_REG,
        );
        self.store64(EXTRA_TEMP_REG, Address::new(SP, CODE_BLOCK as i32));
        self.load64(
            Address::new(
                proto_callframe,
                offset_of!(ProtoCallFrame, callee_value) as i32,
            ),
            EXTRA_TEMP_REG,
        );
        self.store64(EXTRA_TEMP_REG, Address::new(SP, CALLEE as i32));
        self.load64(
            Address::new(
                proto_callframe,
                offset_of!(ProtoCallFrame, arg_count_and_code_origin_value) as i32,
            ),
            EXTRA_TEMP_REG,
        );
        self.store64(EXTRA_TEMP_REG, Address::new(SP, ARGUMNET_COUNT as i32));

        self.load32(
            Address::new(
                proto_callframe,
                offset_of!(ProtoCallFrame, arg_count_and_code_origin_value) as i32
                    + offset_of!(AsBits, payload) as i32,
            ),
            T4,
        );
        self.load32(
            Address::new(
                proto_callframe,
                offset_of!(ProtoCallFrame, padded_arg_count) as i32,
            ),
            EXTRA_TEMP_REG,
        );

        let copy_args = self.branch32(RelationalCondition::Equal, T4, EXTRA_TEMP_REG);

        // TODO: Throw exception
        self.illegal_instruction();

        copy_args.link(&mut self.asm);

        self.load64(
            Address::new(proto_callframe, offset_of!(ProtoCallFrame, args) as i32),
            T3,
        );

        let copy_args_loop = self.label();
        let copy_args_done = self.branch32(RelationalCondition::Equal, T4, 0i32);
        self.sub32(1i32, T4);
        self.load_word(
            BaseIndex::new(T3, T4, SCALE, 0, Extend::None),
            EXTRA_TEMP_REG,
        );
        self.store_word(
            EXTRA_TEMP_REG,
            BaseIndex::new(SP, T4, SCALE, 8, Extend::None),
        );
        let j = self.jump();
        j.link_to(&mut self.asm, copy_args_loop);

        copy_args_done.link(&mut self.asm);

        self.store64(
            SP,
            Address::new(thread, offset_of!(Thread, top_call_frame) as i32),
        );
        self.store64(
            CFR,
            Address::new(thread, offset_of!(Thread, top_entry_frame) as i32),
        );

        make_call(self, entry, proto_callframe, T3, T4);

        self.vm_entry_record(CFR, T4);
        self.load64(
            Address::new(T4, offset_of!(VMEntryRecord, thread) as i32),
            thread,
        );
        self.load64(
            Address::new(T4, offset_of!(VMEntryRecord, prev_top_call_frame) as i32),
            T2,
        );
        self.store64(
            T2,
            Address::new(thread, offset_of!(Thread, top_call_frame) as i32),
        );
        self.load64(
            Address::new(T4, offset_of!(VMEntryRecord, prev_top_entry_frame) as i32),
            T2,
        );
        self.store64(
            T2,
            Address::new(thread, offset_of!(Thread, top_entry_frame) as i32),
        );

        self.sub64_rrr(CFR, CALLEE_SAVE_REGISTER_SIZE as i32, SP);

        self.pop_callee_saves();
        self.function_epilogue();
        self.ret();
    }

    pub fn make_scheme_call(&mut self, entry: u8, _proto_cfr: u8, _temp1: u8, _temp2: u8) {
        self.add64(16i32, SP);
        self.call_op(Some(entry));
        self.sub64(16i32, SP);
    }

    pub fn make_native_call(&mut self, entry: u8, _proto_cfr: u8, temp1: u8, _temp2: u8) {
        self.mov(entry, temp1);
        self.store64(CFR, Address::new(SP, 0));
        self.mov(ARGUMENT_GPR1, ARGUMENT_GPR0);
        self.mov(SP, ARGUMENT_GPR1);
        self.call_op(Some(temp1));
    }

    pub fn prologue(&mut self) {
        self.mov(ARGUMENT_GPR1, THREAD);
        self.preserve_caller_pc_and_cfr();
        self.load64(Address::new(CFR, CODE_BLOCK as i32), T1);

        self.preserve_callee_saves_used_by_llint();
        self.load64(
            Address::new(T1, offset_of!(CodeBlock, instructions) as i32),
            PB,
        );
        self.xor64(PC, PC);

        // get new SP in T0
        self.get_frame_register_size_for_code_block(T1, T0);
        self.sub64_rrr(CFR, T0, T0);

        self.mov(SP, T2);
        self.mov(T0, SP);

        let zero_stack_loop = self.label();
        let zero_stack_done = self.branch64(RelationalCondition::Equal, T2, SP);
        self.sub64(8i32, T2);
        self.store64(0i32, Address::new(T2, 0));
        let j = self.jump();
        j.link_to(&mut self.asm, zero_stack_loop);
        zero_stack_done.link(&mut self.asm);
        if cfg!(feature = "compressed-oops") {
            self.mov(HeapCompressionScheme::base() as i64, HEAP_BASE);
        } else {
            self.mov(0i64, HEAP_BASE);
        }

        
    }

    pub fn op_mov(&mut self, size: OpcodeSize) {
        get_operand!(self, size, OP_MOV_SRC_INDEX, T1);
        self.load_constant_or_variable(size, T1, T2);
        get_operand!(self, size, OP_MOV_DEST_INDEX, T1);
        self.store_word(T2, BaseIndex::new(CFR, T1, SCALE, 8, Extend::None));
        self.dispatch_op(size, OP_MOV);
    }

    pub fn op_wide16(&mut self) {
        self.next_instruction_wide16();
    }

    pub fn op_wide32(&mut self) {
        self.next_instruction_wide32();
    }

    pub fn no_wide(&mut self, f: impl Fn(&mut Self), op: OpcodeID) {
        self.comment(format!("{}:", OPCODE_NAMES[op as usize]));
        let narrow = self.label();
        f(self);
        let wide16 = self.label();
        self.asm.illegal_instruction();
        let wide32 = self.label();
        self.asm.illegal_instruction();
        self.register_opcode(op, narrow, wide16, wide32);
    }

    pub fn op_nop(&mut self, size: OpcodeSize) {
        self.dispatch_op(size, OP_NOP);
    }

    pub fn op_enter(&mut self, _: OpcodeSize) {
        self.load_word(Address::new(CFR, CODE_BLOCK), T2);
        self.decompress(T2, T2);
        self.untag_object(T2, T2);

        self.load_word(Address::new(T2, offset_of!(CodeBlock, num_vars) as i32), T2);
        self.sub64(CALLEE_SAVE_SPACE_AS_VIRTUAL_REGISTERS as i32, T2);
        self.mov(CFR, T1);
        self.sub64(CALLEE_SAVE_SPACE_AS_VIRTUAL_REGISTERS as i32 * 8, T1);
        let enter_done = self.branch_test64(ResultCondition::Zero, T2, T2);
        self.mov(undefined().ptr as i32, T0);
        self.neg32(T2);
        self.sign_extend32_to_64(T2, T2);
        let enter_loop = self.label();
        self.store_word(T0, BaseIndex::new(T1, T2, SCALE, 0, Extend::None));
        self.add64(1i32, T2);
        let j = self.branch_test64(ResultCondition::NonZero, T2, T2);
        j.link_to(&mut self.asm, enter_loop);
        enter_done.link(&mut self.asm);
        self.dispatch_op(OpcodeSize::Narrow, OP_ENTER);
    }

    pub fn op_allocate(&mut self, size: OpcodeSize) {
        get_operand!(self, size, OP_ALLOCATE_SIZE_INDEX, T1);
        self.load_constant_or_variable(size, T1, T1);
        let slowpath = self.bump_allocate(THREAD, T2, T1, usize::MAX, T3);
        let init = self.label();
        get_operand!(self, size, OP_ALLOCATE_TAG_INDEX, T1);
        self.store16(T1, Address::new(T2, -(OBJECT_REF_OFFSET as i32)));
        get_operand!(self, size, OP_ALLOCATE_DST_INDEX, T1);
        self.tag_object(T2, T2);
        self.compress(T2, T2);
        self.store_word(T2, BaseIndex::new(CFR, T1, SCALE, 8, Extend::None));

        self.slowpaths.push(Box::new(move |llint| {
            slowpath.link(&mut llint.asm);
            llint.mov(THREAD, ARGUMENT_GPR0);
            llint.mov(T1, ARGUMENT_GPR1);
            llint.ccall2(AbsoluteAddress::new(super::slow_paths::allocate as _));
            let j = llint.jump();
            j.link_to(&mut llint.asm, init);
        }));
    }

    pub fn slowpaths(&mut self) {
        for slowpath in std::mem::take(&mut self.slowpaths) {
            slowpath(self);
        }
    }
}

pub struct Thunk {
    pub code_ref: CodeRef,
}

unsafe impl Send for Thunk {}
unsafe impl Sync for Thunk {}

static VM_ENTRY_TO_SCHEME: Lazy<Thunk> = Lazy::new(|| {
    let mut llint = InterpreterGenerator::new();

    llint.do_vm_entry(InterpreterGenerator::make_scheme_call);

    let mut code = LinkBuffer::from_macro_assembler(&mut llint.asm).unwrap();

    let code = code.finalize_without_disassembly();

    Thunk { code_ref: code }
});

pub fn vm_entry_to_scheme(
    addr: *const (),
    thread: &mut Thread,
    proto_cfr: &mut ProtoCallFrame,
) -> Register {
    unsafe {
        let code = VM_ENTRY_TO_SCHEME.code_ref.start();
        let func: extern "C-unwind" fn(*const (), *mut Thread, *mut ProtoCallFrame) -> Register =
            std::mem::transmute(code);

        func(addr, thread, proto_cfr)
    }
}

static VM_ENTRY_TO_NATIVE: Lazy<Thunk> = Lazy::new(|| {
    let mut llint = InterpreterGenerator::new();

    llint.do_vm_entry(InterpreterGenerator::make_native_call);

    let mut code = LinkBuffer::from_macro_assembler(&mut llint.asm).unwrap();

    let code = code.finalize_without_disassembly();

    Thunk { code_ref: code }
});

pub fn vm_entry_to_native(
    addr: *const (),
    thread: &mut Thread,
    proto_cfr: &mut ProtoCallFrame,
) -> Register {
    unsafe {
        let code = VM_ENTRY_TO_NATIVE.code_ref.start();
        let func: extern "C-unwind" fn(*const (), *mut Thread, *mut ProtoCallFrame) -> Register =
            std::mem::transmute(code);

        func(addr, thread, proto_cfr)
    }
}

static FUNCTION_FOR_CALL_PROLOGUE: Lazy<Thunk> = Lazy::new(|| {
    let mut llint = InterpreterGenerator::new();

    llint.prologue();
    llint.dispatch(Operand::Imm32(0i32));

    let mut code = LinkBuffer::from_macro_assembler(&mut llint.asm).unwrap();

    let code = code.finalize_without_disassembly();

    Thunk { code_ref: code }
});

pub fn function_for_call_prologue_addr() -> *const () {
    FUNCTION_FOR_CALL_PROLOGUE.code_ref.start() as _
}

