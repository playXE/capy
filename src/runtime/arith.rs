use crate::raise_exn;

use super::value::Value;

pub fn scm_to_u64(arg: Value) -> u64 {
    if arg.is_int32() {
        arg.get_int32() as u64
    } else {
        raise_exn!(FailContract, &[], "scm_to_uint64: not an exact integer: {}", arg)
    }
}

pub fn scm_to_u32(arg: Value) -> u32 {
    if arg.is_int32() {
        arg.get_int32() as u32
    } else {
        raise_exn!(FailContract, &[], "scm_to_u32: not an exact integer: {}", arg)
    }
}

pub fn scm_to_usize(arg: Value) -> usize {
    if arg.is_int32() {
        arg.get_int32() as usize
    } else {
        raise_exn!(FailContract, &[], "scm_to_usize: not an exact integer: {}", arg)
    }
}