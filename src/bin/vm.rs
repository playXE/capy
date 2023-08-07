use capy::{vm::{options::VMOptions, scm_init, thread::Thread}, bytecode::image::load_image_from_memory, interpreter::scm_call_n};

fn main() {
    let opts = match VMOptions::parse() {
        Ok(opts) => opts,
        Err(err) => {
            eprintln!("Error: {}", err);
            std::process::exit(1);
        }
    };
    

    let mut mmtk = mmtk::MMTKBuilder::new();
    mmtk.set_option("plan", opts.gc_plan.as_ref());
    mmtk.set_option("gc_trigger", &format!("DynamicHeapSize:{},{}", opts.gc_min_heap_size, opts.gc_max_heap_size));
    mmtk.set_option("threads", "4");

    let vm = scm_init(mmtk.build());
    vm.disassemble = opts.disassemble;
    if let Some(file) = opts.filename {
        let memory = std::fs::read(file).unwrap();
        let image = match load_image_from_memory(&memory, None) {
            Ok(image) => image,
            Err(err) => {
                eprintln!("Error: {}", err);
                std::process::exit(1);
            }
        };
        let result = scm_call_n(Thread::current(), image.entry_program, &[]);
        match result {
            Ok(val) => {
                println!("Ok: {}", val);
            }
            Err(err) => {
                eprintln!("Error: {}", err);
                std::process::exit(1);
            }
        }
    } else {
        eprintln!("no bytecode image file specified");
        std::process::exit(1);
    }
}