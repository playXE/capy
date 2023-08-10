/*//! Low-level interpreter for CapyVM bytecode.

use std::mem::MaybeUninit;

use macroassembler::{
    jit::gpr_info::*,
    jit::{fpr_info::*, helpers::AssemblyHelpers},
    assembler::{*, abstract_macro_assembler::AbsoluteAddress, link_buffer::LinkBuffer}, wtf::executable_memory_handle::CodeRef,
};

use crate::runtime::value::Value;

static mut LLINT_TRAMPOLINE: MaybeUninit<CodeRef> = MaybeUninit::uninit();
static mut LLINT_ENTRYPOINT: *const u8 = null();
static mut LLINT_RETURN_ADDRESS: *const u8 = null();

pub fn generate_entrypoint() -> unsafe extern "C" fn(&mut Thread) {
    let mut masm = TargetMacroAssembler::new();
    
    masm.emit_function_prologue();
    masm.push(CS0);
    masm.push(CS1);
    masm.push(CS2);
    masm.push(CS3);
    masm.push(CS4);

    masm.mov(Value::NOT_CELL_MASK, NOT_CELL_MASK_REGISTER);
    masm.mov(Value::NUMBER_TAG, NUMBER_TAG_REGISTER);
    
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
    let a = 42;
    unsafe {
        let mut lb = LinkBuffer::from_macro_assembler(&mut masm).unwrap();
        let addr = lb.rx_location_of(ret);
        LLINT_RETURN_ADDRESS = addr;
        let fin = lb.finalize_without_disassembly();
        LLINT_TRAMPOLINE = MaybeUninit::new(fin);
    }
}*/