use std::mem::{size_of, offset_of};

use macroassembler::jit::gpr_info::NUMBER_OF_CALLEE_SAVED_REGISTERS;

use super::stackframe::StackFrame;


/// Opaque type representing VM entry frame in low-level interpreter
pub struct EntryFrame;

impl EntryFrame {
    pub unsafe fn vm_entry_record_offset() -> isize {
        let fake_entry_frame = std::mem::transmute::<_, *mut EntryFrame>(0x1000usize);
        let record = vm_entry_record(fake_entry_frame);
        let offset = record.cast::<u8>().offset_from(fake_entry_frame.cast());
        offset
    }

    pub unsafe fn callee_save_registers_buffer_offset() -> isize {
        Self::vm_entry_record_offset() + offset_of!(VMEntryRecord, callee_save_registers_buffer) as isize
    }
}

#[repr(C)]
pub struct VMEntryRecord {
    pub prev_top_call_frame: *mut StackFrame,
    pub prev_top_entry_frame: *mut VMEntryRecord,
    pub callee_save_registers_buffer: [usize; NUMBER_OF_CALLEE_SAVED_REGISTERS],
}

pub unsafe extern "C" fn vm_entry_record(entry_frame: *mut EntryFrame) -> *mut VMEntryRecord {
    let stack_alignment = 16;
    let vm_entry_total_frame_size = (size_of::<VMEntryRecord>() + (stack_alignment - 1)) & !(stack_alignment - 1);
    entry_frame.cast::<u8>().sub(vm_entry_total_frame_size).cast()
}