use std::mem::{size_of, offset_of};

use macroassembler::jit::gpr_info::NUMBER_OF_CALLEE_SAVED_REGISTERS;

use crate::runtime::{thread::Thread, value::{Tagged, Value}};

use super::{stackframe::CallFrame, llint::STACK_ALIGNMENT_BYTES};

pub struct EntryFrame {
    
}

impl EntryFrame {
    pub fn vm_entry_record_offset() -> isize {
        let fake_entry_frame = 0x1000 as *mut EntryFrame;
        unsafe {
            let record = vm_entry_record(fake_entry_frame);
            record.cast::<u8>().offset_from(fake_entry_frame.cast::<u8>()) 
        }   
    }

    pub fn callee_save_register_buffer_offset() -> isize {
        Self::vm_entry_record_offset() + offset_of!(VMEntryRecord, callee_save_buffer) as isize
    }
}

///
/// This record stored in a `vm_entry_to_{scheme,host}` allocated frame. It is allocated on the stack
/// after callee save registers where local variables would go.
///
#[repr(C)]
pub struct VMEntryRecord {
    pub thread: *mut Thread,
    pub prev_top_call_frame: *mut CallFrame,
    pub prev_top_entry_frame: *mut EntryFrame,
    pub callee: Tagged<Value>,
    pub callee_save_buffer: [usize; NUMBER_OF_CALLEE_SAVED_REGISTERS]
}

/// Fetches the VMEntryRecord from the current thread's entry frame.
/// 
/// # Safety
/// 
/// `entry_frame` must be a valid pointer to an `EntryFrame` allocated
/// on stack
pub unsafe fn vm_entry_record(entry_frame: *mut EntryFrame) -> *mut VMEntryRecord {
    let stack_alignment = STACK_ALIGNMENT_BYTES;
    let vm_entry_total_size = (size_of::<VMEntryRecord>() + (stack_alignment-1)) & !(stack_alignment-1);
    entry_frame.cast::<u8>().sub(vm_entry_total_size).cast()
}

