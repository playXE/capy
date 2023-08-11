use crate::{
    gc_frame,
    interpreter::stackframe::{frame_local, frame_num_locals, frame_virtual_return_address},
    runtime::{
        object::{ScmPair, ScmProgram},
        value::Value,
    },
};

use super::thread::Thread;
#[inline(never)]
pub unsafe extern "C" fn get_callee_vcode(thread: &mut Thread) -> *const u8 {
    let proc = *frame_local(thread.interpreter().fp, 0);

    if proc.is_program() {
        //disassemble_program(proc);
        return proc.get_object().cast_as::<ScmProgram>().vcode;
    }

    thread.interpreter().ip = frame_virtual_return_address(thread.interpreter().fp);

    todo!("throw error: {}", proc);
}

pub unsafe extern "C" fn cons_rest(thread: &mut Thread, base: u32) -> Value {
    let rest = Value::encode_null_value();
    // root `rest` for GC to move it around if necessary
    gc_frame!(thread.stackchain() => rest = rest);

    let mut n =
        frame_num_locals(thread.interpreter().fp, thread.interpreter().sp) as usize - base as usize;

    while n != 0 {
        n -= 1;

        let obj = thread.make_cons::<false>(Value::encode_null_value(), Value::encode_null_value());
        // can't assign value to `obj` in `make_cons<>` because it can potentially
        // contain GC object and MMTk is allowed to trigger GC at allocation.
        // No write barrier needed here because `obj` is a new object.
        let pair = obj.cast_as::<ScmPair>();
        pair.car = *frame_local(thread.interpreter().fp, base as usize + n);
        pair.cdr = *rest;
        *rest = obj;
    }

    *rest
}
