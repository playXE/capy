use capy::{
    fasl::FaslReader, r5rs::interaction_environment, structure::struct_ref, value::Value,
    vm::Runtime,
};

use rsgc::prelude::*;

fn main() {
    env_logger::init();
    let args = HeapArguments::from_env();
    //args.heuristics = GCHeuristic::Static;
    let _ = rsgc::prelude::main_thread(args, |heap| {
        heap.add_core_root_set();

        let slib_path = std::env::var("SCHEME_LIBRARY_PATH").unwrap_or_else(|_| {
            std::env::current_dir()
                .unwrap()
                .join("lib")
                .to_str()
                .unwrap()
                .to_string()
        });
        if !std::path::Path::new(&format!("{}/init.fasl", slib_path)).exists() {
            eprintln!("init.fasl not found, run `cargo run --bin bootstrap` to build it");
            return Ok(());
        }

        let file = std::fs::File::open(&format!("{}/init.fasl", slib_path)).unwrap();

        let mut reader = FaslReader::new(file);

        let (top, defs, lambdas) = reader.start().expect("invalid boot fasl");

        let vm = capy::vm::vm();

        let clos =
            Runtime::get()
                .jit
                .lock(true)
                .compile(interaction_environment(), defs, lambdas, top);

        let main = match vm.apply(clos, &[]) {
            Ok(v) => v,
            Err(e) => {
                println!("failed to invoke init.scm: {}", struct_ref(e, 0));
                return Ok(());
            }
        };

        let argv = std::env::args().skip(1).collect::<Vec<_>>();

        let argv_vector = Value::make_vector(vm.mutator(), argv.len() as _, Value::make_null());

        for (i, arg) in argv.iter().enumerate() {
            argv_vector.vector_set(i as _, Value::make_string(vm.mutator(), arg));
        }

        match vm.apply(main, &[argv_vector]) {
            Ok(v) => {
                println!(">returned: \n{}", v);
            }
            Err(e) => {
                println!("{}", struct_ref(e, 0));
            }
        }

        Ok(())
    });
}
