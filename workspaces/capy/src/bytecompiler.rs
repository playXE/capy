use std::{marker::PhantomData, ptr::NonNull};

use crate::{bytecode::{
    conversions::{OpcodeSize, PaddingBySize, PaddingBySizeTrait},
    instruction_stream::{InstructionStreamWriter, InstructionWrite},
    opcodes::*,
    OpcodeID, virtual_register::virtual_register_for_local,
}, runtime::segmented_vec::SegmentedVec};

use self::register::{RegisterID, RegisterIDRef};


pub mod register;
pub struct BytecodeGenerator<'a> {
    pub writer: InstructionStreamWriter,
    pub last_opcode_id: OpcodeID,
    pub last_instruction_index: usize,
    pub callee_locals: SegmentedVec<RegisterID>,
    pub marker: PhantomData<&'a ()>
}

impl<'a> BytecodeGenerator<'a> {
    pub fn new() -> Self {
        Self {
            writer: InstructionStreamWriter::new(),
            last_instruction_index: 0,
            last_opcode_id: OP_NOP,
            callee_locals: SegmentedVec::new(),
            marker: PhantomData
        }
    }


    fn locals_shrink_to_fit(vec: &mut SegmentedVec<RegisterID>) {
        while !vec.is_empty() && vec.last().unwrap().ref_count() == 0 {
            vec.pop();
        }
    }

    pub fn reclaim_free_registers(&mut self) {
        Self::locals_shrink_to_fit(&mut self.callee_locals);
    }

    pub fn record_opcode(&mut self, id: OpcodeID) {
        self.last_opcode_id = id;
        self.last_instruction_index = self.writer.position();
    }
}

pub enum Writeable {
    Byte(u8),
    Word(u16),
    DWord(u32),
}

impl Into<Writeable> for u8 {
    fn into(self) -> Writeable {
        Writeable::Byte(self)
    }
}

impl Into<Writeable> for u16 {
    fn into(self) -> Writeable {
        Writeable::Word(self)
    }
}

impl Into<Writeable> for u32 {
    fn into(self) -> Writeable {
        Writeable::DWord(self)
    }
}

impl Into<Writeable> for i8 {
    fn into(self) -> Writeable {
        Writeable::Byte(self as u8)
    }
}

impl Into<Writeable> for i16 {
    fn into(self) -> Writeable {
        Writeable::Word(self as u16)
    }
}

impl Into<Writeable> for i32 {
    fn into(self) -> Writeable {
        Writeable::DWord(self as u32)
    }
}

impl<'a> BytecodeGenerator<'a> {
    pub fn new_register(&mut self) -> RegisterIDRef<'a> {
        self.callee_locals.push(RegisterID::new(virtual_register_for_local(self.callee_locals.len() as _)));
        let num_callee_locals = self.callee_locals.len();
        RegisterIDRef {
            marker: Default::default(),
            register: NonNull::new(&mut self.callee_locals[num_callee_locals - 1]).unwrap()
        }
    }

    pub fn align_wide_opcode16(&mut self) {
        if !cfg!(target_arch = "x86_64") {
            let opcode_size = 1;
            let prefix_and_opcode_size =
                opcode_size + PaddingBySize::<{ OpcodeSize::Wide16 }>::VALUE;
            while (self.writer.position() + prefix_and_opcode_size) % OpcodeSize::Wide16 as usize
                != 0
            {
                OpNop::emit(self);
            }
        }
    }

    pub fn align_wide_opcode32(&mut self) {
        if !cfg!(target_arch = "x86_64") {
            let opcode_size = 1;
            let prefix_and_opcode_size =
                opcode_size + PaddingBySize::<{ OpcodeSize::Wide32 }>::VALUE;
            while (self.writer.position() + prefix_and_opcode_size) % OpcodeSize::Wide32 as usize
                != 0
            {
                OpNop::emit(self);
            }
        }
    }

    pub fn write(&mut self, x: impl Into<Writeable>) {
        match x.into() {
            Writeable::Byte(x) => {
                self.writer.write(x);
            }
            Writeable::Word(x) => {
                self.writer.write(x);
            }
            Writeable::DWord(x) => {
                self.writer.write(x);
            }
        }
    }
}
