use rsgc::thread::Thread;

use super::{value::Value, string::make_string};

pub fn raise_error(who: &str, description: &str, code: i32) -> Value {
    if code > 0 {
        return make_string(Thread::current(), format!("{}({}): {}", who, code, description)).into()
    }
    return make_string(Thread::current(), format!("{}: {}", who, description)).into()
}

pub fn raise_argument_error(who: &str, expected: &str, got: Value) -> Value {
    make_string(Thread::current(), format!("{}: argument error, expected {}, got {:?}", who, expected, got)).into()
}