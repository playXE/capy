use capy::{runtime::{Runtime, enter_scheme, factory::make_list_of_n, cell::Pair, flags}, gc_protect};


fn main() {
    env_logger::init();
    flags::parse(std::env::args()).unwrap();
    println!("{:p}", std::hint::black_box(Runtime::get()));

    enter_scheme(|thread| {
        unsafe { thread.initialize_main() }
        let mut xs = make_list_of_n(2, thread);
        let hashcode = xs.get_cell().hashcode();
        gc_protect!(thread => xs => thread.request_gc());
        std::thread::sleep(std::time::Duration::from_millis(10));
        gc_protect!(thread => xs => thread.safepoint());
        println!("{:x} {:x}", hashcode, xs.get_cell().hashcode());
    });
}
