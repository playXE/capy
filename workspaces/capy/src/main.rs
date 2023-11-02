use capy::runtime::{enter_scheme, flags, Runtime};

fn main() {
    env_logger::init();
    flags::parse(std::env::args()).unwrap();
    std::hint::black_box(Runtime::get());

    enter_scheme(|thread| unsafe { thread.initialize_main() });
}
