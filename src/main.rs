use std::path::PathBuf;

use capy::{load::scm_vm_load, repl::repl, value::Value};

use rsgc::{
    prelude::HeapArguments,
    thread::{main_thread, Thread},
};

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

        capy::init();
        capy::vm::scm_init_vm();

        let args = Args::from_args();
        let t = Thread::current();
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
                    println!("Error: {:?}", e);
                }
            }
        } else {
            repl();
        }

        Ok(())
    });
}
