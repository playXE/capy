use crate::{interpreter::stackframe::{frame_local, frame_virtual_return_address, frame_num_locals}, runtime::{object::{ScmProgram, ScmPair}, value::Value}, gc_frame};

use super::thread::Thread;

pub unsafe extern "C" fn get_callee_vcode(thread: &mut Thread) -> *const u8 {
    let proc = *frame_local(thread.interpreter().fp, 0);

    if proc.is_program() {
        return proc.get_object().cast_as::<ScmProgram>().vcode;
    }

    thread.interpreter().ip = frame_virtual_return_address(thread.interpreter().fp);

    todo!("throw error");
}

pub unsafe extern "C" fn cons_rest(thread: &mut Thread, base: u32) -> Value {
    let rest = Value::encode_null_value();
    gc_frame!(thread.stackchain() => rest = rest);
    let mut n = frame_num_locals(thread.interpreter().fp, thread.interpreter().sp) - base as isize;

    while n != 0 {
        let obj = thread.make_cons::<false>(Value::encode_null_value(), Value::encode_null_value());
        obj.get_object().cast_as::<ScmPair>().car = *frame_local(thread.interpreter().fp, base as isize + n);
        obj.get_object().cast_as::<ScmPair>().cdr = *rest;
        *rest = obj;
        n -= 1;
    }

    *rest
}