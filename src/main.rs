#![allow(unused_imports)]
use b3::BasicBlockBuilder;
use capy::{
    bytecode::{
        disassembler::disassemble,
        opcodes::{OpAdd, OpEnter, OpReturn, OP_LAST},
        virtual_register::{
            virtual_register_for_argument, virtual_register_for_local, VirtualRegister,
        },
    },
    bytecompiler::bytecodegenerator::BytecodeGenerator,
    compile::{cenv_make_bottom, pass1::pass1, ref_count_lvars},
    op::Opcode,
    runtime::{
        error::{Exception, EXN_TABLE},
        structure::{is_struct_instance, struct_ref},
    },
    runtime::{
        fun::make_procedure,
        load::scm_vm_load,
        module::scm_user_module,
        port::{
            port_open_file, Port, SCM_PORT_BUFFER_MODE_LINE, SCM_PORT_DIRECTION_IN,
            SCM_PORT_FILE_OPTION_NONE,
        },
        reader::Reader,
        string::make_string,
    },
    vm::{engine::vm_entry, scm_vm},
    Thread,
};
use macroassembler::{assembler::link_buffer::LinkBuffer, jit::gpr_info::ARGUMENT_GPR0};
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
                        let cmark = struct_ref(e, 1);
                        println!("cmarks: \n{:?}", cmark);
                    } else {
                        println!("Error: {:?}", e);
                    }
                }
            }
        } else {
            let vm = scm_vm();
            let port = Port::new(vm.mutator());
            port_open_file(
                port,
                make_string(vm.mutator(), "test.scm").into(),
                SCM_PORT_DIRECTION_IN,
                SCM_PORT_FILE_OPTION_NONE,
                SCM_PORT_BUFFER_MODE_LINE,
                false.into(),
            )
            .unwrap();

            let mut r = Reader::new(scm_vm(), port, false);

            loop {
                let expr = r.read().unwrap();
                if expr.is_eof_object() {
                    break;
                }
                let mut bc = BytecodeGenerator::new();

                let cenv = cenv_make_bottom(Some(scm_user_module().module()));

                let iform = pass1(expr, cenv).unwrap();

                ref_count_lvars(iform);

                bc.compile_body(&iform);

                let code = bc.finalize();
                let proc = make_procedure(vm.mutator(), code);
                unsafe {
                    let res = vm_entry(vm, proc.into(), &[], Some(scm_user_module().module()));
                    println!("res: {:?}", res);
                }
            }
        }
        #[cfg(feature = "profile-opcodes")]
        {
            capy::vm::interpreter::print_profiles();
        }

        Ok(())
    });
}
