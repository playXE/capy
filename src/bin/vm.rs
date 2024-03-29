use capy::{
    bytecode::image::load_image_from_memory,
    interpreter::scm_call_n,
    runtime::{
        gsubr::{scm_define_subr, Subr},
        object::{scm_car, scm_cdr},
        value::Value, environment::environment_get, symbol::scm_intern,
    },
    vm::{
        options::{pretty_print_bytes, VMOptions},
        scm_init,
        thread::Thread, scm_virtual_machine,
    },
};

fn main() {
    env_logger::init();
    let opts = match VMOptions::parse() {
        Ok(opts) => opts,
        Err(err) => {
            eprintln!("Error: {}", err);
            std::process::exit(1);
        }
    };

    let mut mmtk = mmtk::MMTKBuilder::new();
    mmtk.set_option("plan", opts.gc_plan.as_ref());
    mmtk.set_option(
        "gc_trigger",
        &format!(
            "DynamicHeapSize:{},{}",
            opts.gc_min_heap_size, opts.gc_max_heap_size
        ),
    );
    println!(
        "GC size: {}..{}",
        pretty_print_bytes(opts.gc_min_heap_size as _),
        pretty_print_bytes(opts.gc_max_heap_size as _)
    );
    mmtk.set_option("threads", "4");

    let vm = scm_init(mmtk.build(), opts.gc_plan);
    vm.disassemble = opts.disassemble;

    scm_define_subr(
        "print-raw",
        0,
        0,
        1,
        Subr::F1({
            extern "C-unwind" fn print(_: &mut Thread, rest: &mut Value) -> Value {
                while rest.is_pair() {
                    let car = scm_car(*rest);
                    print!("{} ", car);
                    *rest = scm_cdr(*rest);
                }
                println!();
                Value::encode_null_value()
            }

            print
        }),
    );
    if let Some(file) = opts.filename {
        let memory = std::fs::read(file).unwrap();
        let image = match load_image_from_memory(&memory, None) {
            Ok(image) => image,
            Err(err) => {
                eprintln!("Error: {}", err);
                std::process::exit(1);
            }
        };
        let result = scm_call_n::<true>(Thread::current(), image.entry_program, &[]);
        match result {
            Ok(val) => {
                println!("Ok: {}", val);
            }
            Err(err) => {
                println!("Unhandled exception");
                let display_proc = environment_get(scm_virtual_machine().interaction_environment, scm_intern("display"));
                match display_proc {
                    Ok(proc) if proc.is_program() => {
                        let _ = scm_call_n::<true>(Thread::current(), proc, &[err]);
                    }
                    _ => {
                        eprintln!("Error: {}", err);
                    }
                }
                std::process::exit(1);
            }
        }
    } else {
        eprintln!("no bytecode image file specified");
        std::process::exit(1);
    }
}
