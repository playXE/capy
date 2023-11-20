#![allow(unused_imports)]
use std::{collections::HashMap, mem::{size_of, offset_of}, ptr::null};

use macroassembler::{
    assembler::{
        abstract_macro_assembler::{Address, BaseIndex, Extend, Jump, Label, Operand, Scale},
        ResultCondition, TargetMacroAssembler,
    },
    jit::gpr_info::*,
    jit::fpr_info::*,
};

use crate::{
    bytecode::{
        conversions::OpcodeSize,
        macros::NUMBER_OF_BYTECODE_IDS,
        opcodes::{OPCODE_LENGTHS, OPCODE_NAMES},
        OpcodeID,
    },
    gc::ptr_compr::HeapCompressionScheme,
    runtime::{value::{SMI_SHIFT_SIZE, SMI_TAG_SIZE}, thread::Thread, code_block::CodeBlock},
};

use super::{entry_frame::VMEntryRecord, stackframe::{CallerFrameAndPC, CallFrameSlot}};

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
    } else if #[cfg(all(windows, target_arch="x86_64"))] {
        pub const PB: u8 = CS0;
        pub const HEAP_BASE: u8 = CS1;
        pub const THREAD: u8 = CS2;
    } else {
        compile_error!("Unsupported target architecture");
    }
}

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
    slowpaths: Vec<Box<dyn FnOnce(Self)>>,
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

impl InterpreterGenerator {
    pub fn next_instruction(&mut self) {
        self.load8(BaseIndex::new(PB, PC, Scale::TimesOne, 1, Extend::None), T0);
        self.mov(unsafe { OPCODE_MAP.as_ptr() as i64 }, T1);
        self.far_jump(BaseIndex::new(T1, T0, Scale::TimesEight, 0, Extend::None))
    }

    pub fn next_instruction_wide16(&mut self) {
        self.load16(BaseIndex::new(PB, PC, Scale::TimesOne, 1, Extend::None), T0);
        self.mov(unsafe { OPCODE_MAP_WIDE16.as_ptr() as i64 }, T1);
        self.far_jump(BaseIndex::new(T1, T0, Scale::TimesEight, 0, Extend::None))
    }

    pub fn next_instruction_wide32(&mut self) {
        self.load32(BaseIndex::new(PB, PC, Scale::TimesOne, 1, Extend::None), T0);
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
                        offset + OPCODE_ID_NARROW_SIZE as i32 + 1,
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
                    offset * 2 + OPCODE_ID_WIDE16_SIZE as i32 + 1,
                    Extend::SExt32,
                ),
                dst,
            ),
            OpcodeSize::Wide32 => self.load32(
                BaseIndex::new(
                    PB,
                    PC,
                    Scale::TimesOne,
                    offset * 4 + OPCODE_ID_WIDE32_SIZE as i32 + 1,
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
                        offset + OPCODE_ID_NARROW_SIZE as i32 + 1,
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
                    offset * 2 + OPCODE_ID_WIDE16_SIZE as i32 + 1,
                    Extend::None,
                ),
                dst,
            ),
            OpcodeSize::Wide32 => self.load32(
                BaseIndex::new(
                    PB,
                    PC,
                    Scale::TimesOne,
                    offset * 4 + OPCODE_ID_WIDE32_SIZE as i32 + 1,
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
        #[cfg(target_arch="arm64")]
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

        #[cfg(all(target_arch="x86_64", not(windows)))]
        {
            self.store64(CS0, Address::new(entry_frame, 0));
            self.store64(CS1, Address::new(entry_frame, 8));
            self.store64(CS2, Address::new(entry_frame, 16));
            self.store64(CS3, Address::new(entry_frame, 24));
            self.store64(CS4, Address::new(entry_frame, 32));
        }

        #[cfg(all(target_arch="x86_64", windows))]
        {
            self.store64(CS0, Address::new(entry_frame, 0));
            self.store64(CS1, Address::new(entry_frame, 8));
            self.store64(CS2, Address::new(entry_frame, 16));
            self.store64(CS3, Address::new(entry_frame, 24));
            self.store64(CS4, Address::new(entry_frame, 32));
            self.store64(CS5, Address::new(entry_frame, 40));
            self.store64(CS6, Address::new(entry_frame, 48));
        }

        #[cfg(target_arch="riscv64")]
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
        self.load64(Address::new(vm, offset_of!(Thread, top_entry_frame) as i32), temp);
        self.copy_callee_saves_to_entry_frame_callee_saves_buffer(temp);
    }

    pub fn resstore_callee_saves_from_vm_entry_frame_callee_saves_buffer(&mut self, vm: u8, temp: u8) {
        self.load64(Address::new(vm, offset_of!(Thread, top_entry_frame) as i32), temp);
        self.vm_entry_record(temp, temp);
        self.lea64(Address::new(temp, offset_of!(VMEntryRecord, callee_save_buffer) as i32), temp);

        #[cfg(target_arch="arm64")]
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

        #[cfg(all(target_arch="x86_64", not(windows)))]
        {
            self.load64(Address::new(temp, 0), CS0);
            self.load64(Address::new(temp, 8), CS1);
            self.load64(Address::new(temp, 16), CS2);
            self.load64(Address::new(temp, 24), CS3);
            self.load64(Address::new(temp, 32), CS4);
        }

        #[cfg(all(target_arch="x86_64", windows))]
        {
            self.load64(Address::new(temp, 0), CS0);
            self.load64(Address::new(temp, 8), CS1);
            self.load64(Address::new(temp, 16), CS2);
            self.load64(Address::new(temp, 24), CS3);
            self.load64(Address::new(temp, 32), CS4);
            self.load64(Address::new(temp, 40), CS5);
            self.load64(Address::new(temp, 48), CS6);
        }

        #[cfg(target_arch="riscv64")]
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
        if cfg!(feature="compressed-oops") {
            self.load32(src, dest);
        } else {
            self.load64(src, dest);
        }
    }

    pub fn store_word(&mut self, src: u8, dest: impl Into<Operand>) {
        if cfg!(feature="compressed-oops") {
            self.store32(src, dest);
        } else {
            self.store64(src, dest);
        }
    }

    pub fn get_frame_register_size_for_code_block(&mut self, code_block: u8, size: u8) {
        self.load_word(Address::new(code_block, offset_of!(CodeBlock, num_callee_locals) as i32), size);
        self.lshift64(3i32, size);
//        self.add64(0i32, size);
    }

    pub fn restore_stack_pointer_after_call(&mut self) {
        self.load64(Address::new(CFR, CODE_BLOCK), T2);
        self.get_frame_register_size_for_code_block(T2, T2);
        self.sub64_rrr(CFR, T2, SP);
    }



}

