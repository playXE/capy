use std::mem::offset_of;
use std::ops::{Deref, DerefMut};
use std::ptr::null_mut;

use crate::op::Opcode;
use crate::vm::callframe::CallFrame;
use macroassembler::assembler::{
    abstract_macro_assembler::*, RelationalCondition, ResultCondition, TargetAssembler,
    TargetMacroAssembler,
};
use macroassembler::jit::{fpr_info::*, gpr_info::*};
use rsgc::thread::Thread;

use super::slowpaths;

pub struct LLIntGenerator {
    jumptable: *mut *mut u8,
    jumptable_wide: *mut *mut u8,
    pub masm: TargetMacroAssembler,
    pub slowpaths: Vec<Box<dyn FnOnce(&mut Self)>>,
}

const VM: u8 = CS0;
const PC: u8 = CS1;
const JUMPTABLE: u8 = CS2;
const NUMBER_TAG_REGISTER: u8 = CS3;
const NOT_CELL_MASK_REGISTER: u8 = CS4;
const CFR: u8 = TargetMacroAssembler::FRAME_POINTER_REGISTER;
const SP: u8 = TargetMacroAssembler::STACK_POINTER_REGISTER;
const JUMPTABLE_SIZE: usize = Opcode::Count as usize;

impl Deref for LLIntGenerator {
    type Target = TargetMacroAssembler;
    fn deref(&self) -> &Self::Target {
        &self.masm
    }
}

impl DerefMut for LLIntGenerator {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.masm
    }
}

impl LLIntGenerator {

    pub fn new() -> Self {
        Self {
            jumptable: null_mut(),
            jumptable_wide: null_mut(),
            masm: TargetMacroAssembler::new(),
            slowpaths: Vec::new(),
        }
    }

    pub fn push(&mut self, val: u8) {
        self.masm.sub64(8i32, SP);
        self.masm.store64(val, Address::new(SP, 0));
    }

    pub fn pop(&mut self, val: u8) {
        self.masm.load64(Address::new(SP, 0), val);
        self.masm.add64(8i32, SP);
    }

    pub fn safepoint(&mut self) {
        let thread = offset_of!(crate::vm::VM, thread);
        let safepoint_addr = Thread::safepoint_offset();
        self.masm.load64(Address::new(VM, thread as i32), T0);
        self.masm
            .load64(Address::new(T0, safepoint_addr as i32), T0);
        self.masm.load64(Address::new(T0, 0), T0);
    }

    pub fn branch_is_object(&mut self, val: u8) -> Jump {
        self.masm
            .branch_test64(ResultCondition::Zero, val, NOT_CELL_MASK_REGISTER)
    }

    pub fn branch_is_not_object(&mut self, val: u8) -> Jump {
        self.masm
            .branch_test64(ResultCondition::NonZero, val, NOT_CELL_MASK_REGISTER)
    }

    pub fn write_barrier(&mut self, val: u8, check_object: bool) {
        let thread = offset_of!(crate::vm::VM, thread);
        let mut done = JumpList::new();
        let mut slow_path = JumpList::new();
        let cm_in_progress = Thread::cm_in_progress_offset();
        if check_object {
            done.push(self.branch_is_not_object(val));
        }
        if val != T0 {
            self.masm.mov(val, T0);
        }
        self.masm.load64(Address::new(VM, thread as i32), T1);
        done.push(self.masm.branch8(
            RelationalCondition::Equal,
            Address::new(T1, cm_in_progress as i32),
            0,
        ));
        let ssb_index = Thread::satb_index_offset();
        let ssb_buffer = Thread::satb_buffer_offset();

        slow_path.push(self.masm.branch_test64(
            ResultCondition::Zero,
            Address::new(T1, ssb_index as _),
            0i32,
        ));
        self.masm.load64(Address::new(T1, ssb_buffer as _), T2);
        self.masm.load64(Address::new(T1, ssb_index as _), T3);
        self.masm.sub64(1i32, T2);
        self.masm.store64(
            val,
            BaseIndex::new(T2, T3, Scale::TimesEight, 0, Extend::None),
        );
        self.masm.store64(T3, Address::new(T1, ssb_index as _));

        done.link(&mut self.masm);
        let done = self.masm.label();

        self.slowpaths.push(Box::new(move |masm| {
            slow_path.link(masm);
            masm.mov(VM, ARGUMENT_GPR0);
            masm.mov(val, ARGUMENT_GPR1);
            masm.call_op(Some(AbsoluteAddress::new(
                slowpaths::llint_write_barrier as _,
            )));
            masm.jump().link_to(&mut masm.masm, done);
        }));
    }


    pub fn dispatch(&mut self) {
        self.masm.add64(1i32, PC);
        // jmp [jumptable + (PC - 1) * 8]
        self.masm.far_jump(BaseIndex::new(
            JUMPTABLE,
            PC,
            Scale::TimesEight,
            -8,
            Extend::None,
        ));
    }

    fn emit_no_op(&mut self) {
        self.dispatch();
    }

    fn emit_enter(&mut self) {
        self.masm.load64(Address::new(CFR, offset_of!(CallFrame, code_block)), T2);

        self.dispatch();
    }

    pub fn generate(&mut self) {

    }
}
