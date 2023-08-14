use capy::{
    gc_protect,
    runtime::{
        hashtable::{put_hashtable, HashTableType, ScmHashTable},
        symbol::scm_intern,
        value::Value,
    },
    vm::{options::VMOptions, scm_init, scm_virtual_machine, thread::Thread}, gc::objstorage::{ObjStorage, ParState},
};

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
    mmtk.set_option(
        "gc_trigger",
        &format!(
            "DynamicHeapSize:{},{}",
            opts.gc_min_heap_size, opts.gc_max_heap_size
        ),
    );
    mmtk.set_option("threads", "4");

    let _vm = scm_init(mmtk.build());

    let storage = ObjStorage::new("objects");

    let p = storage.allocate();

    println!("allocated {:p}", p);

    ParState::<false>::new(storage.clone(), 1).iterate(|slot| {
        println!("slot: {:p}", slot);
    });

}
