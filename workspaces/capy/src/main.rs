#![allow(unused_imports)]
use capy::interpreter::llint::*;
use capy::interpreter::proto_callframe::ProtoCallFrame;
use capy::interpreter::register::Register;
use capy::interpreter::stackframe::CallFrame;
use capy::runtime::code_block::CodeBlock;
use capy::runtime::thread::Thread;
use capy::{
    bytecode::{
        conversions::{Fits, OpcodeSize},
        dumper::BytecodeDumper,
        opcodes::*,
        virtual_register::{virtual_register_for_local, VirtualRegister},
    },
    bytecompiler::BytecodeGenerator,
    gc::CapyVM,
    gc_protect,
    interpreter::llint::InterpreterGenerator,
    runtime::{
        cell::{CellReference, Pair},
        enter_scheme,
        factory::align_allocation,
        flags,
        smi::Smi,
        utils::is_aligned,
        value::{Tagged, Value, *},
        Runtime,
    },
};
use macroassembler::{
    assembler::{abstract_macro_assembler::Address, link_buffer::LinkBuffer, x86assembler::*},
    jit::gpr_info::{ARGUMENT_GPR0, RETURN_VALUE_GPR, T0, T1, T2, T3},
};

extern "C-unwind" fn foo(thread: &mut Thread, cfr: &mut CallFrame) -> Register {
    println!("Call from Scheme!");
    Register {
        compressed: undefined(),
    }
}

fn main() {
    env_logger::init();
    flags::parse(std::env::args()).unwrap();
    std::hint::black_box(Runtime::get());
  
    enter_scheme(|thread| {
        unsafe { thread.initialize_main() }
        let mut args = [undefined()];
        let mut cfr = ProtoCallFrame::new(
            unsafe { Tagged::<CellReference<CodeBlock>>::from(undefined().get_cell_reference()) },
            undefined(),
            1,
            &mut args,
        );
        vm_entry_to_native(foo as _, thread, &mut cfr);

        /*         let f: extern "C" fn(&mut Thread) -> Tagged<CellReference> = unsafe { std::mem::transmute(code.start()) };

               let x = f(thread);
        run
               println!("{:x}", x.0.ptr());*/
    });
}
