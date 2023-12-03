use std::mem::size_of;

use crate::{
    bytecode::{opcodes::BaseInstruction, virtual_register::VirtualRegister},
    runtime::{
        cell::CellReference,
        code_block::CodeBlock,
        value::{Tagged, TaggedValue},
    },
};

use super::{
    entry_frame::{vm_entry_record, EntryFrame},
    register::Register,
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub struct CallerFrameAndPC {
    pub caller_frame: *mut CallFrame,
    pub return_pc: *const u8,
}

impl CallerFrameAndPC {
    pub const SIZE_IN_REGISTERS: usize = size_of::<Self>() / size_of::<usize>();
}

///      Layout of CallFrame
///```text
///   |          ......            |   |
///   +----------------------------+   |
///   |           argN             |   v  lower address
///   +----------------------------+
///   |           arg1             |
///   +----------------------------+
///   |           arg0             |
///   +----------------------------+
///   |           this             |
///   +----------------------------+
///   |       argument count       |
///   +----------------------------+
///   |          callee            |
///   +----------------------------+
///   |        code block          |
///   +----------------------------+
///   |      return-address        |
///   +----------------------------+
///   |       callerFrame          |
///   +----------------------------+  <- callee's cfr is pointing this address
///   |          local0            |
///   +----------------------------+
///   |          local1            |
///   +----------------------------+
///   |          localN            |
///   +----------------------------+
///   |          ......            |
/// ```
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(usize)]
pub enum CallFrameSlot {
    CodeBlock = CallerFrameAndPC::SIZE_IN_REGISTERS,
    Callee = Self::CodeBlock as usize + 1,
    ArgumentCount = Self::Callee as usize + 1,
    FirstArgument = Self::ArgumentCount as usize + 1,
}

pub const CALL_SITE_INDEX_OFFSET: usize =
    CallFrameSlot::ArgumentCount as usize * size_of::<u64>() + size_of::<u32>();

/// Represents the current state of program execution.
/// Passed as the first argument to most functions.
#[repr(C)]
pub struct CallFrame(Register);

impl std::ops::Deref for CallFrame {
    type Target = Register;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for CallFrame {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl CallFrame {
    pub const HEADER_SIZE_IN_REGISTERS: usize = CallFrameSlot::FirstArgument as usize;

    pub const fn argument_offset(argument: i32) -> i32 {
        CallFrameSlot::FirstArgument as i32 + argument
    }

    pub const fn first_argument_offset() -> i32 {
        CallFrameSlot::FirstArgument as i32
    }

    pub fn registers(&self) -> *const Register {
        self as *const Self as *const Register
    }

    pub fn registers_mut(&mut self) -> *mut Register {
        self as *mut Self as *mut Register
    }

    fn caller_frame_and_pc(&self) -> *const CallerFrameAndPC {
        self as *const Self as _
    }

    fn caller_frame_and_pc_mut(&mut self) -> *mut CallerFrameAndPC {
        self as *mut Self as _
    }

    fn arg_index_for_register(&self, reg: *const Register) -> isize {
        // The register at 'offset' number of slots from the frame pointer
        // i.e.
        //       reg = frame[offset];
        //   ==> reg = frame + offset;
        //   ==> offset = reg - frame;
        unsafe {
            let offset = reg.offset_from(self.registers()) as isize;
            // The offset is defined (based on argument_fffset()) to be:
            //       offset = CallFrameSlot::FirstArgument - arg_index;
            // Hence:
            //       arg_index = CallFrameSlot::FirstArgument - offset;
            let arg_index = offset - CallFrameSlot::FirstArgument as isize;
            arg_index
        }
    }

    pub fn code_block(&self) -> Tagged<CellReference<CodeBlock>> {
        unsafe {
            Tagged::from(
                self.registers()
                    .add(CallFrameSlot::CodeBlock as usize)
                    .read()
                    .code_block
                    .downcast_unchecked(),
            )
        }
    }

    pub fn r(&self, reg: VirtualRegister) -> Register {
        if reg.is_constant() {
            Register {
                compressed: self
                    .code_block()
                    .cell()
                    .constant_pool
                    .get(reg.to_constant_index() as _),
            }
        } else {
            unsafe { self.registers().offset(reg.offset() as _).read_unaligned() }
        }
    }

    pub fn r_set(&mut self, reg: VirtualRegister, value: Register) {
        if reg.is_constant() {
            panic!("Cannot set constant register");
        } else {
            unsafe {
                self.registers_mut()
                    .offset(reg.offset() as _)
                    .write_unaligned(value);
            }
        }
    }

    pub unsafe fn unchecked_r<'a>(&self, reg: VirtualRegister) -> Register {
        debug_assert!(!reg.is_constant());
        self.registers().offset(reg.offset() as _).read_unaligned()
    }

    pub unsafe fn unchecked_r_set(&mut self, reg: VirtualRegister, value: Register) {
        debug_assert!(!reg.is_constant());
        self.registers_mut()
            .offset(reg.offset() as _)
            .write_unaligned(value);
    }

    pub fn callee(&self) -> TaggedValue {
        unsafe {
            self.registers()
                .add(CallFrameSlot::Callee as usize)
                .read_unaligned()
                .compressed
        }
    }

    pub fn set_callee(&mut self, callee: TaggedValue) {
        unsafe {
            self.registers_mut()
                .add(CallFrameSlot::Callee as usize)
                .write_unaligned(Register { compressed: callee });
        }
    }

    pub fn argument_count(&self) -> usize {
        unsafe {
            self.registers()
                .add(CallFrameSlot::ArgumentCount as _)
                .read()
                .payload() as _
        }
    }

    pub fn call_site_bits_as_raw_bits(&self) -> usize {
        unsafe {
            self.registers()
                .add(CallFrameSlot::ArgumentCount as _)
                .read()
                .tag() as _
        }
    }

    pub fn current_vpc(&self) -> *const BaseInstruction {
        unsafe {
            self.code_block()
                .instructions
                .add(self.call_site_bits_as_raw_bits())
                .cast()
        }
    }

    pub fn set_current_vpc(&mut self, vpc: *const BaseInstruction) {
        unsafe {
            let bytecode_index =
                vpc.cast::<u8>().offset_from(self.code_block().instructions) as usize;
            *(&mut *self.registers_mut().add(CallFrameSlot::ArgumentCount as _)).tag_mut() =
                bytecode_index as _;
        }
    }

    pub fn bytecode_index(&self) -> usize {
        let cb = self.code_block();

        if cb.ptr() == 0 {
            return 0;
        }

        self.call_site_bits_as_raw_bits()
    }

    pub unsafe fn caller_frame_cur(&self, cur_entry_frame: &mut *mut EntryFrame) -> *mut CallFrame {
        if self.caller_frame_or_entry_frame() == cur_entry_frame.cast::<CallFrame>() {
            let curr_vm_entry_record = vm_entry_record(*cur_entry_frame);
            *cur_entry_frame = (*curr_vm_entry_record).prev_top_entry_frame;
            return (*curr_vm_entry_record).prev_top_call_frame;
        }

        self.caller_frame_or_entry_frame()
    }

    pub fn caller_frame(&self) -> *mut CallFrame {
        self.caller_frame_or_entry_frame()
    }

    pub fn caller_frame_or_entry_frame(&self) -> *mut CallFrame {
        unsafe { (*self.caller_frame_and_pc()).caller_frame }
    }
}
