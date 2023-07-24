use std::mem::transmute;

use fscheme::{
    gc_frame,
    runtime::value::Value,
    vm::{scm_init, scm_virtual_machine, thread::Thread}, gc::object::scm_car,
};

fn main() {
    let _ = scm_init(mmtk::MMTKBuilder::new().build());

    let thread = Thread::current();
    println!("tls: {:p}", thread);
    let pair = Value::encode_object_value(
        thread.make_cons(Value::encode_int32(1), Value::encode_int32(2)),
    );
    gc_frame!(thread.stackchain() => pair = pair);

    println!("{:?}", *pair);

    mmtk::memory_manager::handle_user_collection_request(&scm_virtual_machine().mmtk, unsafe {
        transmute(thread)
    });

    println!("{:?}", *pair);

    println!("{}", scm_car(pair.get_object()).get_int32());
}
