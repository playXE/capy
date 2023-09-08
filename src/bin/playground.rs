use std::mem::transmute;

use capy::{
    runtime::{
        object::{scm_bytevector_is_mapping, scm_bytevector_set_mapping, ScmCellHeader, TypeId},
        value::Value,
    },
    vm::{options::VMOptions, scm_init},
};
use mmtk::{
    util::{Address, ObjectReference},
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

    let _vm = scm_init(mmtk.build(), opts.gc_plan);

    unsafe {
        let mut obj = ScmCellHeader::new(TypeId::Bytevector);

        let obj_ref = ObjectReference::from_raw_address(Address::from_mut_ptr(&mut obj));
        scm_bytevector_set_mapping(Value::encode_object_value(transmute(obj_ref)));
        assert!(scm_bytevector_is_mapping(Value::encode_object_value(
            transmute(obj_ref)
        )));
    }
}
