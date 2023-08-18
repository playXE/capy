use crate::{
    gc_frame,
    interpreter::stackframe::{frame_local, frame_num_locals, frame_virtual_return_address, StackElement},
    runtime::{
        object::{ScmPair, ScmProgram, scm_car, scm_cdr},
        value::Value, environment::scm_define, symbol::scm_intern, list::scm_length, gsubr::{scm_define_subr, Subr},
    }, bytecode::opcodes::{OP_SHUFFLE_DOWN, OP_RETURN_VALUES, OP_ASSERT_NARGS_GE, OP_EXPAND_APPLY_ARGUMENT, OP_TAIL_CALL},
};

use super::{thread::Thread, scm_virtual_machine};
#[inline(never)]
pub unsafe extern "C" fn get_callee_vcode(thread: &mut Thread) -> *const u8 {
    let proc = *frame_local(thread.interpreter().fp, 0);

    if proc.is_program() {
        return proc.get_object().cast_as::<ScmProgram>().vcode;
    }

    thread.interpreter().ip = frame_virtual_return_address(thread.interpreter().fp);

    todo!("throw error: {}", proc);
}

pub unsafe extern "C-unwind" fn cons_rest(thread: &mut Thread, base: u32) -> Value {
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

#[inline(never)]
pub unsafe extern "C-unwind" fn expand_apply_argument(thread: &mut Thread) {
    let mut x = thread.interpreter().sp.read().as_value;
    let len = scm_length(x);
    println!("{}", x);
    if let Some(mut len) = len {
        let n = frame_num_locals(thread.interpreter().fp, thread.interpreter().sp);
        thread.interpreter().alloc_frame(n as u32 - 1 + len as u32);
        while len != 0 {
            len -= 1;
            thread.interpreter().sp.add(len).write(StackElement {
                as_value: scm_car(x)
            });
            x = scm_cdr(x);
        }

    } else {
        todo!("throw error");
    }
}

extern "C-unwind" fn do_gc(thread: &mut Thread) -> Value {
    mmtk::memory_manager::handle_user_collection_request(&scm_virtual_machine().mmtk, thread.to_mmtk());
    Value::encode_undefined_value()
}

static VALUES_CODE: &'static [u8] = &[
    OP_SHUFFLE_DOWN, 1, 0, 0, 0,
    OP_RETURN_VALUES,
];

static APPLY_CODE: &'static [u8] = &[
    OP_ASSERT_NARGS_GE, 3, 0, 0, /* (assert-nargs-ge 3) */
    OP_SHUFFLE_DOWN, 1, 0, 0, 0, /* (shuffle-down 1 0) */
    OP_EXPAND_APPLY_ARGUMENT, /* (expand-apply-argument) */
    OP_TAIL_CALL, /* (tail-call) */
];

pub(crate) fn init() {
    let program = Thread::current().make_program::<true>(VALUES_CODE.as_ptr(), 0);
    scm_define(scm_intern("values"), program);

    let program = Thread::current().make_program::<true>(APPLY_CODE.as_ptr(), 0);
    scm_define(scm_intern("apply"), program);

    scm_define_subr("garbage-collect", 0, 0, 0, Subr::F0(do_gc));
}