#![allow(unused_imports)]
use b3::BasicBlockBuilder;
use capy::{
    repl::repl,
    runtime::{
        error::{Exception, EXN_TABLE},
        structure::{is_struct_instance, struct_ref},
    },
    runtime::{load::scm_vm_load, module::scm_user_module}, compile::bytecompiler::ByteCompiler, Thread, op::Opcode, jit::baseline::stack2ssa,
};
use macroassembler::jit::gpr_info::ARGUMENT_GPR0;
use rsgc::{prelude::HeapArguments, thread::main_thread};
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(StructOpt)]
pub struct Args {
    #[structopt(parse(from_os_str))]
    pub file: Option<PathBuf>,
}

fn main() {
    env_logger::init();

    let _ = main_thread(HeapArguments::from_env(), |heap| {
        heap.add_core_root_set();
        capy::vm::scm_init_vm();
        capy::init();

        let args = Args::from_args();

        if let Some(file) = args.file.as_ref() {
            let scriptfile = if file.starts_with("/") {
                file.display().to_string()
            } else {
                format!("./{}", file.display().to_string())
            };

            match scm_vm_load(&scriptfile, None, None, true, scm_user_module()) {
                Ok(x) => {
                    println!("Ok: {}", x);
                }
                Err(e) => {
                    if is_struct_instance(EXN_TABLE[Exception::Exn as usize].typ, e) {
                        let msg = struct_ref(e, 0);
                        println!("Error: {}", msg);
                        println!("typ: {:?}", e);
                        let cmark = struct_ref(e, 1);
                        println!("cmarks: \n{:?}", cmark);
                    } else {
                        println!("Error: {:?}", e);
                    }
                }
            }
        } else {
            repl();
        }
        /* 
        let t = Thread::current();
        let mut bc = ByteCompiler::new(t);

        bc.emit_simple(Opcode::Enter);
        bc.emit_ldarg(0);
        bc.emit_simple(Opcode::StackGet);
        bc.emit_u16(0);
        bc.emit_simple(Opcode::Return);
        let cb = bc.finalize(t);
        let mut proc = b3::Procedure::new(Default::default());

        let entry = proc.add_block(1.0);
        let mut builder = BasicBlockBuilder::new(&mut proc, entry);
        let cfr = builder.argument(b3::Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int64);
        
        let mut stack2ssa = stack2ssa::Stack2SSA::new(&mut proc, cb, cfr, entry);

        stack2ssa.generate();

        println!("{}", proc.display());
        */

        Ok(())
    });
}
