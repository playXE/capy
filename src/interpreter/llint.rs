#![allow(dead_code)]
//! Low-level interpreter for CapyVM bytecode.

use std::{mem::MaybeUninit, ptr::null};

use macroassembler::{
    jit::gpr_info::*,
    jit::{fpr_info::*, helpers::AssemblyHelpers},
    assembler::{*, abstract_macro_assembler::AbsoluteAddress, link_buffer::LinkBuffer}, wtf::executable_memory_handle::CodeRef,
};

use crate::{runtime::value::Value, vm::thread::Thread};

static mut LLINT_TRAMPOLINE: MaybeUninit<CodeRef> = MaybeUninit::uninit();
static mut LLINT_ENTRYPOINT: *const u8 = null();
static mut LLINT_RETURN_ADDRESS: *const u8 = null();

pub const THREAD: u8 = CS1;
pub const PB: u8 = CS2;
pub const NUMBER_TAG: u8 = CS3;
pub const NOT_CELL_MASK: u8 = CS4;


/// Generates the LLInt trampoline. This piece of code will save all callee-saves
/// and set `NOT_CELL_MASK_REGISTER` and `NUMBER_TAG_REGISTER` to their respective values.
/// Then it jumps to the LLInt entrypoint.
pub fn generate_trampoline() {
    let mut masm = TargetMacroAssembler::new();
    
    masm.emit_function_prologue();
    masm.push(CS0);
    masm.push(CS1);
    masm.push(CS2);
    masm.push(CS3);
    masm.push(CS4);

    masm.mov(Value::NOT_CELL_MASK, NOT_CELL_MASK);
    masm.mov(Value::NUMBER_TAG, NUMBER_TAG);
    
    masm.load64(AbsoluteAddress::new(unsafe {&LLINT_ENTRYPOINT as *const *const u8 as _}), T0);
    masm.far_jump(T0);

    let ret = masm.label();
    masm.pop(CS4);
    masm.pop(CS3);  
    masm.pop(CS2);
    masm.pop(CS1);
    masm.pop(CS0);
    masm.emit_function_epilogue();
    masm.ret();
    unsafe {
        let mut lb = LinkBuffer::from_macro_assembler(&mut masm).unwrap();
        let addr = lb.rx_location_of(ret);
        LLINT_RETURN_ADDRESS = addr;
        let fin = lb.finalize_without_disassembly();
        LLINT_TRAMPOLINE = MaybeUninit::new(fin);
    }
}

pub struct LLIntGenerator {
    masm: TargetMacroAssembler,
    slowpaths: Vec<Box<dyn FnOnce(&mut TargetMacroAssembler)>>,
}

impl std::ops::Deref for LLIntGenerator {
    type Target = TargetMacroAssembler;

    fn deref(&self) -> &Self::Target {
        &self.masm
    }
}

impl std::ops::DerefMut for LLIntGenerator {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.masm
    }
}

impl LLIntGenerator {
    /// Entry trampoline from Rust code to VM code. Will setup corresponding values in local thread state.
    pub fn do_vm_entry(&mut self, make_call: impl FnOnce(&mut Self)) {
        let entry = ARGUMENT_GPR0;
        let thread = ARGUMENT_GPR1;
        let proto_call_frame = ARGUMENT_GPR2;

        
    }
}