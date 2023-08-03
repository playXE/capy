use std::{collections::HashMap, mem::size_of};

use crate::{
    bytecode::{encode::InstructionStream, encode::*, opcodes::*, u24::*},
    compiler::sexpr::Sexpr,
    runtime::value::Value,
};

pub struct Assembler {
    pub relocs: Vec<Reloc>,
    pub code: Vec<u8>,
    pub constants: HashMap<Sexpr, u32>,
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
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            relocs: Vec::new(),
            code: Vec::new(),
            constants: HashMap::new(),
        }
    }

    pub fn emit_mov(&mut self, dst: u32, src: u32) {
        if dst == src {
            return;
        }
        if dst as u16 as u32 == dst && src as u16 as u32 == src {
            OpMov::new(dst as u16, src as u16).write(self);
        } else {
            OpLongMov::new(u24::new(dst), u24::new(src)).write(self);
        }
    }

    pub fn emit_fmov(&mut self, dst: u32, src: u32) {
        OpLongFmov::new(u24::new(dst), u24::new(src)).write(self);
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

    pub fn emit_global_ref(&mut self, dst: u24, name: Value) {
        let name = self.intern_constant(Sexpr::Global(name));
        OpGlobalRef::new(dst, name as _).write(self);
    }

    pub fn emit_global_set(&mut self, dst: u24, name: Value) {
        let name = self.intern_constant(Sexpr::Global(name));
        OpGlobalSet::new(dst, name as _).write(self);
    }

    pub fn emit_make_non_immediate(&mut self, dst: u24, data: Sexpr) {
        let index = self.intern_constant(data);
        OpMakeNonImmediate::new(dst, index).write(self);
       
    }

    pub fn emit_make_immediate(&mut self, dst: u16, imm: u64) {
        OpMakeImmediate::new(dst, imm).write(self);
    }

    pub fn emit_make_program(&mut self, dst: u16, nfree: u32, closure_idx: u32) {
        OpMakeProgram::new(dst, nfree, 0).write(self);
        let reloc = Reloc::Label {
            code_loc: self.code.len() as u32 - 4,
            index: closure_idx,
        };

        self.relocs.push(reloc);
    }

    pub fn intern_constant(&mut self, constant: Sexpr) -> u32 {
        let ix = self.constants.len();
        self.constants
            .entry(constant.into())
            .or_insert_with(|| ix as _)
            .clone()
    }

    pub fn emit_static_program(&mut self, dst: u24, closure_idx: u32) {
        let index = self.intern_constant(Sexpr::Program(closure_idx));
        OpMakeNonImmediate::new(dst, index).write(self);
    }

    pub fn emit_load_free_variable(&mut self, dst: u16, src: u24, idx: u32) {
        OpProgramRefImm::new(dst, src, idx).write(self);
    }

    pub fn emit_store_free_variable(&mut self, dst: u24, src: u16, idx: u32) {
        OpProgramSetImm::new(dst, idx, src).write(self);
    }

    pub fn emit_je(&mut self) -> impl FnOnce(&mut Assembler) {
        let off = self.code.len();
        OpJe::new(0).write(self);
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

    pub fn emit_return_values(&mut self) {
        OpReturnValues::new().write(self);
    }

    pub fn emit_enter(&mut self) {
        OpEnter::new().write(self);
    }

    pub fn emit_alloc_frame(&mut self, size: usize) {
        OpAllocFrame::new(u24::new(size as _)).write(self);
    }

    pub fn emit_prelude(&mut self, nreq: u32, nopt: bool, nlocals: usize) {
        if nopt {
            OpAssertNargsGe::new(u24::new(nreq as _)).write(self);
        } else {
            OpAssertNargsEe::new(u24::new(nreq as _)).write(self);
        }

        self.emit_alloc_frame(nlocals);
    }
}

pub enum Reloc {
    Label { code_loc: u32, index: u32 },
}

pub fn link_assembler(asm: &mut Assembler) -> Vec<u8> {
    let mut output = Vec::with_capacity(asm.code.len() * asm.constants.len() * size_of::<Value>());

    output.extend_from_slice(&asm.code);

    output
}

pub mod fasl;
