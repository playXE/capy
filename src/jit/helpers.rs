use b3::macroassembler::assembler::TargetMacroAssembler;

use crate::runtime::vm::VMEntryRecord;

pub trait AssemblyHelpers {
    fn function_prologue(&mut self);
    fn function_epilogue(&mut self);

    fn push_callee_saves(&mut self);
    fn vm_entry_record(&mut self, entry_frame_pointer: u8, result_reg: u8);
}

impl AssemblyHelpers for TargetMacroAssembler {
    fn function_epilogue(&mut self) {
        self.pop(TargetMacroAssembler::FRAME_POINTER_REGISTER);
    }

    fn function_prologue(&mut self) {
        self.push(TargetMacroAssembler::FRAME_POINTER_REGISTER);
        self.mov(
            TargetMacroAssembler::STACK_POINTER_REGISTER,
            TargetMacroAssembler::FRAME_POINTER_REGISTER,
        );
    }

    fn push_callee_saves(&mut self) {}

    fn vm_entry_record(&mut self, entry_frame_pointer: u8, result_reg: u8) {
        self.sub64_rrr(entry_frame_pointer, VMEntryRecord::VM_ENTRY_TOTAL_FRAME_SIZE as i32, result_reg);
    }
}
