use capy::{
    repl::repl,
    runtime::load::scm_vm_load,
    runtime::{
        error::{Exception, EXN_TABLE},
        structure::{is_struct_instance, struct_ref},
        value::Value,
    },
};
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
            match scm_vm_load(
                &file.display().to_string(),
                Value::encode_bool_value(false),
                None,
            ) {
                Ok(x) => {
                    println!("Ok: {:?}", x);
                }
                Err(e) => {
                    if is_struct_instance(EXN_TABLE[Exception::Exn as usize].typ, e) {
                        let msg = struct_ref(e, 0);
                        println!("Error: {}", msg);
                    } else {
                        println!("Error: {:?}", e);
                    }
                }
            }
        } else {
            repl();
        }
        Ok(())
    });
}
