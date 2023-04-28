use b3::macroassembler::assembler::link_buffer::LinkBuffer;
use b3::macroassembler::assembler::macro_assembler_x86_common::{
    RelationalCondition, ResultCondition,
};
use b3::macroassembler::assembler::{abstract_macro_assembler::*, *};
use b3::macroassembler::jit::gpr_info::*;
use capy::runtime::callframe::{CallFrame, generate_vm_entry};

fn main() {
    let mut link_buffer = generate_vm_entry(|masm, entry, _, _, _| {
        masm.call_op(Some(entry));
    });

    let mut out = String::new();

    let code = link_buffer.finalize_with_disassembly(true, "", &mut out).unwrap();

    println!("{}", out);
}