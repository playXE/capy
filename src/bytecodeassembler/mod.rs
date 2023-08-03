use std::mem::size_of;

use crate::{
    bytecode::{encode::InstructionStream, encode::*, opcodes::*, u24::*},
    runtime::value::Value, compiler::sexpr::Sexpr,
};

pub struct Assembler {
    pub relocs: Vec<Reloc>,
    pub code: Vec<u8>,
    pub constants: Vec<Constant>,
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
    pub fn emit_mov(&mut self, dst: u32, src: u32) {
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
        let name = self.intern_constant(Constant::Global(name));
        OpGlobalRef::new(dst, name as _).write(self);
        let reloc = Reloc::Data {
            code_loc: self.code.len() as u32 - 4,
            index: name,
        };
        self.relocs.push(reloc);
    }

    pub fn emit_global_set(&mut self, dst: u24, name: Value) {
        let name = self.intern_constant(Constant::Global(name));
        OpGlobalSet::new(dst, name as _).write(self);
        let reloc = Reloc::Data {
            code_loc: self.code.len() as u32 - 4,
            index: name,
        };
        self.relocs.push(reloc);
    }

    pub fn emit_make_non_immediate(&mut self, dst: u24, data: Constant) {
        let index = self.intern_constant(data);
        OpMakeNonImmediate::new(dst, 0).write(self);
        let reloc = Reloc::Label {
            code_loc : self.code.len() as u32 - 4,
            index,
        };
        self.relocs.push(reloc);
    }

    pub fn emit_make_program(&mut self, dst: u16, nfree: u32, closure_idx: u32) {
        OpMakeProgram::new(dst, nfree, 0).write(self);
        let reloc = Reloc::Label {
            code_loc : self.code.len() as u32 - 4,
            index: closure_idx,
        };

        self.relocs.push(reloc);
    }


    pub fn intern_constant(&mut self, constant: Constant) -> u32 {
        for (i, c) in self.constants.iter().enumerate() {
            if c == &constant {
                return i as u32;
            }
        }

        self.constants.push(constant);
        self.constants.len() as u32 - 1
    }

    pub fn emit_static_program(&mut self, dst: u24, closure_idx: u32) {
        let index = self.intern_constant(Constant::Program(closure_idx));
        OpMakeNonImmediate::new(dst, 0).write(self);
        let reloc = Reloc::Data {
            code_loc: self.code.len() as u32 - 4,
            index,
        };
        self.relocs.push(reloc);
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Constant {
    String(String),
    Symbol(Value),
    Global(Value),
    Program(u32),
    Sexpr(Sexpr),
}

pub enum Reloc {
    Data { code_loc: u32, index: u32 },
    Label { code_loc: u32, index: u32 },
}


pub fn link_assembler(asm: &mut Assembler) -> Vec<u8> {
    let mut output = Vec::with_capacity(asm.code.len() * asm.constants.len() * size_of::<Value>());

    output.extend_from_slice(&asm.code);

    output
}

pub mod fasl;