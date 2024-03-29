use std::{collections::HashMap, mem::size_of};

use crate::{
    bytecode::{encode::InstructionStream, encode::*, opcodes::*},
    compiler::{sexpr::{Sexpr, SourceLoc}, tree_il::IForm, P},
    runtime::value::Value,
};

pub struct Assembler {
    /// A list of (pos . source) pairs, indicating source information.  POS
    /// is relative to the beginning of the code section, and SOURCE is in
    /// the same format that source-properties returns.
    ///
    pub sources: Vec<(u32, SourceLoc)>,
    pub asm_start: u32,

    pub relocs: Vec<Reloc>,
    pub code: Vec<u8>,
    pub constants: Vec<Sexpr>,
    pub constant_map: HashMap<Sexpr, u32>,
    pub patch_programs: Vec<(u32, u32)>,
}

impl InstructionStream for Assembler {
    fn write_u8(&mut self, value: u8) {
        self.code.push(value);
    }

    fn write_u16(&mut self, value: u16) {
        let le = value.to_le_bytes();
        self.code.extend_from_slice(&le);
    }

    fn write_u32(&mut self, value: u32) {
        let le = value.to_le_bytes();
        self.code.extend_from_slice(&le);
    }

    fn write_finish(&mut self) {
        self.reset_asm_start();
    }
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            relocs: Vec::new(),
            code: Vec::new(),
            constants: Vec::with_capacity(16),
            constant_map: HashMap::with_capacity(16),
            patch_programs: Vec::new(),
            asm_start: 0,
            sources: Vec::with_capacity(128),
        }
    }

    pub fn maybe_source(&mut self, src: Option<SourceLoc>) {
        if let Some(src) = src {
            self.sources.push((self.asm_start as u32, src));
        }
    }

    pub fn reset_asm_start(&mut self) {
        self.asm_start = self.code.len() as u32;
    }

    pub fn asm_start(&self) -> u32 {
        self.asm_start
    }

    pub fn set_asm_start(&mut self, start: u32) {
        self.asm_start = start;
    }

    pub fn emit_mov(&mut self, dst: u32, src: u32) {
        if dst == src {
            return;
        }
        if dst as u16 as u32 == dst && src as u16 as u32 == src {
            OpMov::new(dst as u16, src as u16).write(self);
        } else {
            OpLongMov::new(dst as _, src as _).write(self);
        }
    }

    pub fn emit_fmov(&mut self, dst: u32, src: u32) {
        OpLongFmov::new(dst as _, src as _).write(self);
    }

    pub fn emit_box(&mut self, dst: u32, src: u32) {
        if src == dst {
            self.emit_mov(1, src);
            self.emit_box(dst, 1);
        } else {
            OpBox::new(dst as _, src as _).write(self);
        }
    }

    pub fn emit_box_set(&mut self, dst: u32, src: u32) {
        OpBoxSet::new(dst as _, src as _).write(self);
    }

    pub fn emit_cons(&mut self, dst: u16, car: u16, cdr: u16) {
        OpCons::new(dst, car, cdr).write(self);
    }

    pub fn emit_make_vector_immediate(&mut self, dst: u16, len: u32) {
        OpMakeVectorImmediate::new(dst, len).write(self);
    }

    pub fn emit_make_vector(&mut self, dst: u16, len: u16) {
        OpMakeVector::new(dst, len).write(self);
    }

    pub fn emit_vector_fill(&mut self, dst: u16, val: u16) {
        OpVectorFill::new(dst, val).write(self);
    }

    pub fn emit_vector_set_immediate(&mut self, dst: u16, imm: u32, val: u16) {
        OpVectorSetImm::new(dst, imm, val).write(self);
    }

    pub fn emit_global_ref(&mut self, dst: u32, name: Value) {
        let name = self.intern_constant(Sexpr::Global(name));
        OpGlobalRef::new(dst as _, name as _).write(self);
    }

    pub fn emit_global_set(&mut self, src: u32, name: Value) {
        let name = self.intern_constant(Sexpr::Global(name));
        OpGlobalSet::new(src as _, name as _).write(self);
    }

    pub fn emit_make_non_immediate(&mut self, dst: u32, data: Sexpr) {
        let index = self.intern_constant(data);
        OpMakeNonImmediate::new(dst as _, index).write(self);
    }

    pub fn emit_make_immediate(&mut self, dst: u16, imm: u64) {
        OpMakeImmediate::new(dst, imm).write(self);
    }

    pub fn emit_make_program(&mut self, dst: u16, nfree: u32, closure_idx: u32) {
        OpMakeProgram::new(dst, nfree, 0).write(self);
        let reloc = Reloc::Label {
            code_loc: self.code.len() as _,
            index: closure_idx,
        };

        self.relocs.push(reloc);
    }

    pub fn intern_constant(&mut self, constant: Sexpr) -> u32 {
        if let Some(&ix) = self.constant_map.get(&constant) {
            return ix;
        }
        let ix = self.constants.len();
        self.constants.push(constant.clone());
        self.constant_map.insert(constant, ix as u32);
        ix as u32
    }

    pub fn emit_static_program(&mut self, dst: u32, closure_idx: u32) {
        let index = self.intern_constant(Sexpr::Program(closure_idx));
        self.patch_programs.push((index, closure_idx));
        OpMakeNonImmediate::new(dst as _, index).write(self);
    }

    pub fn emit_load_free_variable(&mut self, dst: u16, src: u16, idx: u32) {
        OpProgramRefImm::new(dst, src, idx).write(self);
    }

    pub fn emit_store_free_variable(&mut self, dst: u16, src: u16, idx: u32) {
        OpProgramSetImm::new(dst as _, idx, src).write(self);
    }

    pub fn emit_jnz(&mut self, src: u16) -> impl FnOnce(&mut Assembler) {
        let off = self.code.len();
        OpJnz::new(src, i32::MAX).write(self);

        move |this| {
            let diff = this.code.len() - off - 7;
            let diff = diff as u32;

            let diff_bytes = diff.to_le_bytes();
            let off = off + 2;
            this.code[off + 1] = diff_bytes[0];
            this.code[off + 2] = diff_bytes[1];
            this.code[off + 3] = diff_bytes[2];
            this.code[off + 4] = diff_bytes[3];
        }
    }

    pub fn emit_j(&mut self) -> impl FnOnce(&mut Assembler) {
        let off = self.code.len();
        OpJ::new(0).write(self);

        move |this| {
            let diff = this.code.len() - off - 5;
            let diff = diff as u32;

            let diff_bytes = diff.to_le_bytes();
            this.code[off + 1] = diff_bytes[0];
            this.code[off + 2] = diff_bytes[1];
            this.code[off + 3] = diff_bytes[2];
            this.code[off + 4] = diff_bytes[3];
        }
    }

    pub fn emit_j_known(&mut self, off: i32) {
        OpJ::new(off).write(self);
    }

    pub fn emit_return_values(&mut self) {
        OpReturnValues::new().write(self);
    }

    pub fn emit_enter(&mut self, offset: i32) {
        OpEnter::new(offset).write(self);
    }

    pub fn emit_loop_hint(&mut self, offset: i32) {
        OpLoopHint::new(offset).write(self);
    }

    pub fn emit_alloc_frame(&mut self, size: usize) {
        OpAllocFrame::new(size as _).write(self);
    }

    pub fn emit_prelude(&mut self, nreq: u32, nopt: bool, nlocals: usize) {
        if nopt {
            OpAssertNargsGe::new(nreq as _).write(self);

            OpBindRest::new(nreq as _).write(self);
        } else {
            OpAssertNargsEe::new(nreq as _).write(self);
        }

        self.emit_alloc_frame(nlocals);
    }

    pub fn emit_call(&mut self, proc: u16, nlocals: u32) {
        OpCall::new(proc, nlocals as _).write(self);
    }

    pub fn emit_shuffle_down(&mut self, from: u32, to: u32) {
        OpShuffleDown::new(from as _, to as _).write(self);
    }

    pub fn emit_reset_frame(&mut self, nlocals: u16) {
        OpResetFrame::new(nlocals).write(self);
    }

    pub fn emit_receive(&mut self, dst: u16, proc: u16, nlocals: u16) {
        OpReceive::new(dst, proc, nlocals).write(self);
    }

    pub fn emit_tail_call(&mut self) {
        OpTailCall::new().write(self);
    }

    pub fn emit_add(&mut self, dst: u16, src1: u16, src2: u16) {
        OpAdd::new(dst, src1, src2).write(self);
    }

    pub fn emit_sub(&mut self, dst: u16, src1: u16, src2: u16) {
        OpSub::new(dst, src1, src2).write(self);
    }

    pub fn emit_mul(&mut self, dst: u16, src1: u16, src2: u16) {
        OpMul::new(dst, src1, src2).write(self);
    }

    pub fn emit_div(&mut self, dst: u16, src1: u16, src2: u16) {
        OpDiv::new(dst, src1, src2).write(self);
    }

    pub fn emit_quotient(&mut self, dst: u16, src1: u16, src2: u16) {
        OpQuotient::new(dst, src1, src2).write(self);
    }

    pub fn emit_less(&mut self, dst: u16, src1: u16, src2: u16) {
        OpLess::new(dst, src1, src2).write(self);
    }

    pub fn emit_greater(&mut self, dst: u16, src1: u16, src2: u16) {
        OpGreater::new(dst, src1, src2).write(self);
    }

    pub fn emit_less_equal(&mut self, dst: u16, src1: u16, src2: u16) {
        OpLessEqual::new(dst, src1, src2).write(self);
    }

    pub fn emit_greater_equal(&mut self, dst: u16, src1: u16, src2: u16) {
        OpGreaterEqual::new(dst, src1, src2).write(self);
    }

    pub fn emit_numerically_equal(&mut self, dst: u16, src1: u16, src2: u16) {
        OpNumericallyEqual::new(dst, src1, src2).write(self);
    }

    pub fn emit_eq(&mut self, dst: u16, src1: u16, src2: u16) {
        OpEq::new(dst, src1, src2).write(self);
    }

    pub fn emit_heap_tag_eq(&mut self, dst: u16, obj: u16, tag: u32) {
        OpHeapTagEq::new(dst, obj, tag).write(self);
    }

    pub fn emit_immediate_tag_eq(&mut self, dst: u16, obj: u16, tag: u32) {
        OpImmediateTagEq::new(dst, obj, tag).write(self);
    }

    pub fn emit_is_false(&mut self, dst: u16, obj: u16) {
        OpIsFalse::new(dst, obj).write(self);
    }

    pub fn emit_is_int32(&mut self, dst: u16, obj: u16) {
        OpIsInt32::new(dst, obj).write(self);
    }
    pub fn emit_is_number(&mut self, dst: u16, obj: u16) {
        OpIsNumber::new(dst, obj).write(self);
    }
    pub fn emit_is_undefined(&mut self, dst: u16, obj: u16) {
        OpIsUndefined::new(dst, obj).write(self);
    }

    pub fn emit_is_null(&mut self, dst: u16, obj: u16) {
        OpIsNull::new(dst, obj).write(self);
    }

    pub fn emit_is_flonum(&mut self, dst: u16, obj: u16) {
        OpIsFlonum::new(dst, obj).write(self);
    }

    pub fn emit_is_true(&mut self, dst: u16, obj: u16) {
        OpIsTrue::new(dst, obj).write(self);
    }

    pub fn emit_is_char(&mut self, dst: u16, obj: u16) {
        OpIsChar::new(dst, obj).write(self);
    }

    pub fn emit_make_box(&mut self, dst: u16, obj: u16) {
        OpBox::new(dst, obj).write(self);
    }

    pub fn emit_box_ref(&mut self, dst: u16, obj: u16) {
        OpBoxRef::new(dst, obj).write(self);
    }

    pub fn emit_bind_rest(&mut self, dst: u16) {
        OpBindRest::new(dst).write(self);
    }

    pub fn emit_receive_values(&mut self, dst: u16, rest: bool, nreq: u16) {
        OpReceiveValues::new(dst as _, rest, nreq as _).write(self);
    }

    pub fn emit_car(&mut self, dst: u16, obj: u16) {
        OpCar::new(dst, obj).write(self);
    }

    pub fn emit_cdr(&mut self, dst: u16, obj: u16) {
        OpCdr::new(dst, obj).write(self);
    }

    pub fn emit_set_car(&mut self, dst: u16, obj: u16) {
        OpSetCar::new(dst, obj).write(self);
    }

    pub fn emit_set_cdr(&mut self, dst: u16, obj: u16) {
        OpSetCdr::new(dst, obj).write(self);
    }

    pub fn emit_not(&mut self, dst: u16, obj: u16) {
        OpNot::new(dst, obj).write(self);
    }

    pub fn emit_vector_ref(&mut self, dst: u16, src: u16, idx: u16) {
        OpVectorRef::new(dst, src, idx).write(self);
    }

    pub fn emit_vector_set(&mut self, dst: u16, src: u16, idx: u16) {
        OpVectorSet::new(dst, src, idx).write(self);
    }

    pub fn emit_vector_length(&mut self, dst: u16, src: u16) {
        OpVectorLength::new(dst, src).write(self);
    }
}

pub enum Reloc {
    Label { code_loc: u32, index: u32 },
    Jump { start: u32, code_loc: u32, label: P<IForm> },
}

pub fn link_assembler(asm: &mut Assembler) -> Vec<u8> {
    let mut output = Vec::with_capacity(asm.code.len() * asm.constants.len() * size_of::<Value>());

    output.extend_from_slice(&asm.code);

    output
}

pub mod fasl;
