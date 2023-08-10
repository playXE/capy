use capy::{
    runtime::{
        hashtable::{put_hashtable, HashTableType, ScmHashTable, get_hashtable},
        symbol::scm_intern,
        value::Value,
    },
    vm::{options::VMOptions, scm_init, thread::Thread},
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

    let thread = Thread::current();

    let ht = thread.make_hashtable(64, HashTableType::Eq);
    ht.cast_as::<ScmHashTable>().lock.lock(true);

    put_hashtable::<false>(ht, Value::encode_int32(1), Value::encode_int32(2));
    put_hashtable::<false>(ht, scm_intern("hello, world!"), Value::encode_int32(4));
    put_hashtable::<false>(ht, Value::encode_int32(2), Value::encode_int32(3));
    put_hashtable::<false>(ht, Value::encode_int32(3), Value::encode_int32(4));
    put_hashtable::<false>(ht, Value::encode_int32(4), Value::encode_int32(5));

    println!("{}", get_hashtable::<false>(ht, scm_intern("hello, world!")).unwrap());
}
