use capy::{
    runtime::arith::ScmBigInteger,
    vm::{options::VMOptions, thread::Thread},
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
        "GC size: {}..{}M",
        opts.gc_min_heap_size / 1024,
        opts.gc_max_heap_size / 1024
    );
    mmtk.set_option("threads", "4");

    let vm = capy::vm::scm_init(mmtk.build(), opts.gc_plan);

    let thread = Thread::current();
    println!("{:b}", 42i64);
    let bn = thread.make_bignum_from_i64(42);

    for word in bn.get_bignum().digits() {
        print!("{:b} ", word);
    }

    println!();

    println!(
        "{}",
        bn.get_bignum().to_string_base(&ScmBigInteger::DEC_BASE)
    );
}
