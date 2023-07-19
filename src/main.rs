use capy::{
    runtime::{
        error::{Exception, EXN_TABLE},
        structure::{is_struct_instance, struct_ref},
    },
    runtime::{load::scm_vm_load, module::scm_user_module},
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
        }
        #[cfg(feature = "profile-opcodes")]
        {
            capy::vm::interpreter::print_profiles();
        }

        Ok(())
    });
}
