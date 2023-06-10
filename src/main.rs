use capy::{
    list::{scm_append, scm_list, scm_list_ref},
    value::Value,
};
use rsgc::thread::{main_thread, Thread};

fn main() {
    let _ = main_thread(Default::default(), |_| {
        let ls1 = scm_list(
            Thread::current(),
            &[
                Value::encode_int32(1),
                Value::encode_int32(2),
                Value::encode_int32(3),
            ],
        );
        let ls2 = scm_list(
            Thread::current(),
            &[
                Value::encode_int32(4),
                Value::encode_int32(5),
                Value::encode_int32(6),
            ],
        );

        let ls3 = scm_append(Thread::current(), ls1, ls2);

        println!("{:?}", scm_list_ref(ls3, 5));

        Ok(())
    });
}
