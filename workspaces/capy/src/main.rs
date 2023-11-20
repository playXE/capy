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
    },
};
use macroassembler::assembler::{link_buffer::LinkBuffer, x86assembler::*};

fn main() {
    env_logger::init();
    flags::parse(std::env::args()).unwrap();
    std::hint::black_box(Runtime::get());

    enter_scheme(|thread| {
        unsafe { thread.initialize_main() }
        
        let x = thread.make_flonum::<true>(42.42).to_value();

        mmtk::memory_manager::handle_user_collection_request(Runtime::get().mmtk(), thread.to_mmtk());

        println!("{:p}->{:x}", &x, x.ptr);
    });
}
