use std::mem::{offset_of, size_of};
use std::ops::{Deref, DerefMut};
use std::ptr::null_mut;

use crate::op::Opcode;
use crate::runtime::object::{
    CodeBlock, NativeProcedure, Procedure, ScmResult, Type, Vector, GLOC,
};
use crate::runtime::value::Value;
use crate::vm::callframe::CallFrame;
use crate::vm::VMType;
use macroassembler::assembler::{
    abstract_macro_assembler::*, RelationalCondition, ResultCondition,
    TargetMacroAssembler,
};
use macroassembler::jit::gpr_info::*;
use rsgc::system::arraylist::ArrayList;
use rsgc::thread::Thread;

use super::slowpaths;

pub struct LLIntGenerator {
    jumptable: *mut *mut u8,
    jumptable_wide: *mut *mut u8,
    eval_loop: Label,
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
            eval_loop: Label::unset(),
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
        let thread = offset_of!(crate::vm::VMType, thread);
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
        let thread = offset_of!(crate::vm::VMType, thread);
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

    pub fn emit_no_op(&mut self) {
        self.dispatch();
    }

    pub fn emit_enter(&mut self) {
        self.masm.load64(
            Address::new(CFR, offset_of!(CallFrame, code_block) as i32),
            T2,
        );

        self.dispatch();
    }

    pub fn emit_pop(&mut self) {
        self.pop(T0);
        self.dispatch();
    }

    pub fn emit_popn(&mut self) {
        self.masm.load16(Address::new(PC, 0), T0);
        self.masm.add64(2i32, PC);
        self.masm.add64(T0, SP);
        self.dispatch();
    }

    pub fn emit_dup(&mut self) {
        self.masm.load64(Address::new(SP, 0), T0);
        self.push(T0);
        self.dispatch();
    }

    pub fn emit_swap(&mut self) {
        self.masm.load64(Address::new(SP, 0), T0);
        self.masm.load64(Address::new(SP, 8), T1);
        self.masm.store64(T0, Address::new(SP, 8));
        self.masm.store64(T1, Address::new(SP, 0));
        self.dispatch();
    }

    pub fn emit_alloc(&mut self) {
        self.masm.load16(Address::new(PC, 0), T0);
        self.masm.add64(2i32, PC);
        self.masm.lshift64(3i32, T0);
        self.masm.add64(T0, SP);
        self.dispatch();
    }

    pub fn emit_ldarg(&mut self) {
        self.masm.load16(Address::new(PC, 0), T0);
        self.masm.add64(2i32, PC);

        let offset = offset_of!(CallFrame, args) as i32;

        self.masm.load64(
            BaseIndex::new(CFR, T0, Scale::TimesOne, offset, Extend::None),
            T0,
        );
        self.push(T0);
    }

    pub fn emit_setarg(&mut self) {
        self.masm.load16(Address::new(PC, 0), T0);
        self.masm.add64(2i32, PC);
        self.pop(T1);

        let offset = offset_of!(CallFrame, args) as i32;
        self.masm.get_effective_address(
            BaseIndex::new(CFR, T0, Scale::TimesEight, offset, Extend::None),
            T0,
        );
        self.masm.store64(T1, Address::new(T0, 0));
        self.dispatch();
    }

    pub fn emit_push_undef(&mut self) {
        self.masm.mov(Value::encode_undefined_value().get_raw(), T0);
        self.push(T0);
        self.dispatch();
    }

    pub fn emit_push_null(&mut self) {
        self.masm.mov(Value::encode_null_value().get_raw(), T0);
        self.push(T0);
        self.dispatch();
    }

    pub fn emit_push_true(&mut self) {
        self.masm.mov(Value::encode_bool_value(true).get_raw(), T0);
        self.push(T0);
        self.dispatch();
    }

    pub fn emit_push_false(&mut self) {
        self.masm.mov(Value::encode_bool_value(false).get_raw(), T0);
        self.push(T0);
        self.dispatch();
    }

    pub fn emit_push_int32(&mut self) {
        self.masm.load32(Address::new(PC, 0), T0);
        self.masm.add64(4i32, PC);
        self.masm.zero_extend32_to_word(T0, T0);
        self.masm.or64(Value::NUMBER_TAG, T0);
        self.push(T0);
        self.dispatch();
    }

    pub fn emit_push_double(&mut self) {
        self.masm.load64(Address::new(PC, 0), T0);
        self.masm.add64(8i32, PC);
        self.masm.add64(Value::DOUBLE_ENCODE_OFFSET, T0);
        self.push(T0);
        self.dispatch();
    }

    pub fn emit_push_constant(&mut self) {
        self.masm.load16(Address::new(PC, 0), T0);
        self.masm.add64(2i32, PC);
        self.masm.load64(
            Address::new(CFR, offset_of!(CallFrame, code_block) as i32),
            T1,
        );

        self.masm
            .load64(Address::new(T1, offset_of!(CodeBlock, literals) as i32), T1);

        self.masm.load64(
            BaseIndex::new(
                T1,
                T0,
                Scale::TimesEight,
                offset_of!(Vector, data) as _,
                Extend::None,
            ),
            T0,
        );

        self.push(T0);
        self.dispatch();
    }

    pub fn emit_push_procedure(&mut self) {
        self.masm.load16(Address::new(PC, 0), T0);
        self.masm.add64(2i32, PC);
        self.masm.load64(
            Address::new(CFR, offset_of!(CallFrame, code_block) as i32),
            T1,
        );

        self.masm
            .load64(Address::new(T1, offset_of!(CodeBlock, literals) as i32), T1);

        self.masm.load64(
            BaseIndex::new(
                T1,
                T0,
                Scale::TimesEight,
                offset_of!(Vector, data) as _,
                Extend::None,
            ),
            T0,
        );

        self.push(T0);
        self.dispatch();
    }

    fn fetch_constant(&mut self, src: u8, dst: u8) {
        assert_ne!(src, dst);
        self.masm.load64(
            Address::new(CFR, offset_of!(CallFrame, code_block) as i32),
            dst,
        );
        self.masm.load64(
            Address::new(T1, offset_of!(CodeBlock, literals) as i32),
            dst,
        );

        self.masm.load64(
            BaseIndex::new(
                dst,
                src,
                Scale::TimesEight,
                offset_of!(Vector, data) as _,
                Extend::None,
            ),
            dst,
        );
    }

    pub fn emit_global_ref(&mut self) {
        self.masm.load16(Address::new(PC, 0), T0);
        self.masm.add64(2i32, PC);

        self.fetch_constant(T0, T1);

        let _not_gloc = self.branch_if_not_xtype(T1, Type::GLOC);
        self.masm
            .load64(Address::new(T1, offset_of!(GLOC, value) as i32), T0);
        self.push(T0);
        self.dispatch();

        self.slowpaths.push(Box::new(|_gen| {}));
    }

    pub fn branch_if_not_xtype(&mut self, src: u8, ty: Type) -> JumpList {
        let not_cell = self.branch_test64(ResultCondition::Zero, src, NOT_CELL_MASK_REGISTER);
        let not_type = self.branch8(
            RelationalCondition::NotEqual,
            Address::new(src, 0),
            ty as i32,
        );

        let mut jl = JumpList::new();
        jl.push(not_cell);
        jl.push(not_type);

        jl
    }

    pub fn emit_eval_loop(&mut self) {
        let label = self.masm.label();
        self.eval_loop = label;

        self.masm.store64(
            CFR,
            Address::new(VM, offset_of!(VMType, top_call_frame) as i32),
        );

        self.masm
            .load64(Address::new(CFR, offset_of!(CallFrame, callee) as i32), T0);

        // fast path: dispatch to bytecode handler
        let not_procedure = self.branch_if_not_xtype(T0, Type::Procedure);
        self.masm
            .load64(Address::new(T0, offset_of!(Procedure, code) as i32), T0);
        self.masm.store64(
            T0,
            Address::new(CFR, offset_of!(CallFrame, code_block) as i32),
        );
        self.masm.add64(offset_of!(CodeBlock, code) as i32, T0);
        self.masm.mov(T0, PC);
        self.dispatch();

        // slow-path: native call
        not_procedure.link(&mut self.masm);
        // TODO
        let not_native_proc = self.branch_if_not_xtype(T0, Type::NativeProcedure);
        let not_closed_proc = self.branch_if_not_xtype(T0, Type::ClosedNativeProcedure);

        self.masm.mov(CFR, ARGUMENT_GPR0);
        self.masm.call_op(Some(Address::new(
            T0,
            offset_of!(NativeProcedure, callback) as i32,
        )));

        let is_tail = self.branch32(
            RelationalCondition::Equal,
            RETURN_VALUE_GPR,
            ScmResult::TAIL as i32,
        );
        self.masm.ret();

        is_tail.link(&mut self.masm);

        self.masm
            .load64(Address::new(CFR, offset_of!(CallFrame, caller) as i32), T5);
        self.masm.load64(
            Address::new(CFR, offset_of!(CallFrame, return_pc) as i32),
            T4,
        );
        self.masm.load32(Address::new(CFR, offset_of!(CallFrame, argc) as i32), T3);
        // adjust the CFR
        self.masm.get_effective_address(
            BaseIndex::new(CFR, T3, Scale::TimesEight, offset_of!(CallFrame, args) as i32, Extend::None),
            CFR 
        );
       
        self.masm
            .load64(Address::new(VM, offset_of!(VMType, tail_rands) as i32), T2);
        self.masm.load32(
            Address::new(T2, ArrayList::<Value>::len_offset() as i32),
            T1 
        );
        
        let is_zero = self.masm.branch_test32(ResultCondition::Zero, T1, T1);
        let copy = self.masm.label();
        self.masm.sub64(8, CFR);
        self.masm.load64(BaseIndex::new(T2, T1, Scale::TimesEight, ArrayList::<Value>::data_offset() as i32, Extend::None), T0);
        self.masm.store64(T0, Address::new(CFR, 0));
        self.masm.sub64(1, T1);
        let jmp = self.masm.branch_test32(ResultCondition::NonZero, T1, T1);
        jmp.link_to(&mut self.masm, copy);
        is_zero.link(&mut self.masm);
        self.masm.load32(
            Address::new(T2, ArrayList::<Value>::len_offset() as i32),
            T1 
        );
        self.masm.store64(0i32, Address::new(T2, ArrayList::<Value>::len_offset() as i32));

        self.masm.sub64(size_of::<CallFrame>() as i32, CFR);
        self.masm.store64(T5, Address::new(CFR, offset_of!(CallFrame, caller) as i32));
        self.masm.store64(T4, Address::new(CFR, offset_of!(CallFrame, return_pc) as i32));
        self.masm.store64(T1, Address::new(CFR, offset_of!(CallFrame, argc) as i32));
        self.masm.store64(0i32, Address::new(CFR, offset_of!(CallFrame, code_block) as i32));
        self.masm.mov(CFR, SP);

        let jmp = self.masm.jump();
        jmp.link_to(&mut self.masm, self.eval_loop);

    }

    pub fn generate(&mut self) {}
}
