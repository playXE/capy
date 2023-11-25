#![allow(unused_imports)]
use capy::{
    bytecode::{
        conversions::{Fits, OpcodeSize},
        opcodes::*,
        virtual_register::{virtual_register_for_local, VirtualRegister}, dumper::BytecodeDumper,
    },
    bytecompiler::BytecodeGenerator,
    gc::CapyVM,
    gc_protect,
    runtime::{
        cell::{CellReference, Pair},
        enter_scheme,
        factory::align_allocation,
        flags,
        smi::Smi,
        utils::is_aligned,
        value::{Tagged, Value, *},
        Runtime,
    }, interpreter::llint::InterpreterGenerator,
};
use capy::interpreter::llint::*;
use macroassembler::{assembler::{link_buffer::LinkBuffer, x86assembler::*, abstract_macro_assembler::Address}, jit::gpr_info::{T0, T1, T2, T3}};

fn main() {
    env_logger::init();
    flags::parse(std::env::args()).unwrap();
    std::hint::black_box(Runtime::get());

    enter_scheme(|thread| {
        unsafe { thread.initialize_main() }
        
        let mut llint = InterpreterGenerator::new();

        llint.bump_allocate(THREAD, T0, INVALID_GPR, 48, T1);

        let mut code = LinkBuffer::from_macro_assembler(&mut *llint).unwrap();

        let mut out = String::new();

        code.finalize_with_disassembly(true, "", &mut out).unwrap();

        println!("{}", out);


    });
}
