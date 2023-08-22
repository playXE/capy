
use crate::{
    bytecode::opcodes::{
        OP_ASSERT_NARGS_GE, OP_EXPAND_APPLY_ARGUMENT, OP_RETURN_VALUES, OP_SHUFFLE_DOWN,
        OP_TAIL_CALL,
    },
    gc_protect,
    interpreter::stackframe::{
        frame_local, frame_num_locals, frame_virtual_return_address, StackElement,
    },
    runtime::{
        control::{ScmContinuation, VMCont},
        environment::scm_define,
        gsubr::{scm_define_subr, Subr},
        list::scm_length,
        object::{scm_car, scm_cdr, ScmPair, ScmProgram},
        symbol::scm_intern,
        value::Value,
    },
};

use super::{scm_virtual_machine, thread::Thread};
#[inline(never)]
pub unsafe extern "C" fn get_callee_vcode(thread: &mut Thread) -> *const u8 {
    let proc = *frame_local(thread.interpreter().fp, 0);

    if proc.is_program() {
        
        return proc.get_object().cast_as::<ScmProgram>().vcode;
    }
    let ip = thread.interpreter().ip;
    thread.interpreter().ip = frame_virtual_return_address(thread.interpreter().fp);

    todo!("throw error: {} at {:p}<>{:p}", proc, thread.interpreter().ip, ip);
}

pub unsafe extern "C-unwind" fn cons_rest(thread: &mut Thread, base: u32) -> Value {
    let mut rest = Value::encode_null_value();

    let mut n =
        frame_num_locals(thread.interpreter().fp, thread.interpreter().sp) as usize - base as usize;

    while n != 0 {
        n -= 1;
        let obj = gc_protect!(thread => rest => thread.make_cons::<false>(Value::encode_null_value(), Value::encode_null_value()));
        let pair = obj.cast_as::<ScmPair>();
        pair.car = *frame_local(thread.interpreter().fp, base as usize + n);
        pair.cdr = rest;
        rest = obj;
    }

    rest
}

#[inline(never)]
pub unsafe extern "C-unwind" fn expand_apply_argument(thread: &mut Thread) {
    let mut x = thread.interpreter().sp.read().as_value;
    let len = scm_length(x);
    if let Some(mut len) = len {
        let n = frame_num_locals(thread.interpreter().fp, thread.interpreter().sp);
        thread.interpreter().alloc_frame(n as u32 - 1 + len as u32);
        while len != 0 {
            len -= 1;
            thread.interpreter().sp.add(len).write(StackElement {
                as_value: scm_car(x),
            });
            x = scm_cdr(x);
        }
    } else {
        todo!("throw error");
    }
}
/// Jumps to the given continuation by unwinding to `scm_call_n` location.
pub struct UnwindAndContinue(pub Value);

pub(crate) unsafe extern "C-unwind" fn reinstate_continuation_x(
    thread: &mut Thread,
    cont: Value,
) -> ! {
    let continuation = cont.cast_as::<ScmContinuation>();

    if continuation.vm_cont.cast_as::<VMCont>().id != thread.interpreter().entry_id {
        todo!("throw error: invoking continuation would cross continuation barrier");
    }
    let n = frame_num_locals(thread.interpreter().fp, thread.interpreter().sp) - 1;

    let mut argv = vec![Value::encode_undefined_value(); n as usize];
    for i in 0..n {
        let arg = thread.interpreter().sp.add(i as usize).read().as_value;
        argv[i as usize] = arg;
    }
    

    return_to_continuation_inner(thread, continuation.vm_cont.cast_as());

    let new_sp = thread.interpreter().sp.sub(3).offset(-n);
    thread.interpreter().push_sp(new_sp);

    for i in 0..3 {
        thread
            .interpreter()
            .sp
            .add(n as usize + i)
            .cast::<Value>()
            .write(Value::encode_bool_value(false));
    }

    for i in 0..n {
        thread
            .interpreter()
            .sp
            .add(i as usize)
            .cast::<Value>()
            .write(argv[i as usize]);
    }

    thread.interpreter().ip = continuation.vm_cont.cast_as::<VMCont>().vra;

    std::panic::resume_unwind(Box::new(UnwindAndContinue(cont)))
}

unsafe fn return_to_continuation_inner(thread: &mut Thread, cont: &mut VMCont) {
    std::ptr::copy_nonoverlapping(
        cont.stack_bottom,
        thread.interpreter().stack_top.sub(cont.stack_size),
        cont.stack_size,
    );
    thread.interpreter().fp = thread.interpreter().stack_top.offset(-cont.fp_offset);
    let new_sp = thread.interpreter().stack_top.sub(cont.stack_size);
    thread.interpreter().restore_sp(new_sp);
}

extern "C-unwind" fn do_gc(thread: &mut Thread) -> Value {
    mmtk::memory_manager::handle_user_collection_request(
        &scm_virtual_machine().mmtk,
        thread.to_mmtk(),
    );
    Value::encode_undefined_value()
}

static VALUES_CODE: &'static [u8] = &[OP_SHUFFLE_DOWN, 1, 0, 0, 0, OP_RETURN_VALUES];

static APPLY_CODE: &'static [u8] = &[
    OP_ASSERT_NARGS_GE,
    3,
    0,
    0, /* (assert-nargs-ge 3) */
    OP_SHUFFLE_DOWN,
    1,
    0,
    0,
    0,                        /* (shuffle-down 1 0) */
    OP_EXPAND_APPLY_ARGUMENT, /* (expand-apply-argument) */
    OP_TAIL_CALL,             /* (tail-call) */
];

pub(crate) fn init() {
    let program = Thread::current().make_program::<true>(VALUES_CODE.as_ptr(), 0);
    scm_define(scm_intern("values"), program);

    let program = Thread::current().make_program::<true>(APPLY_CODE.as_ptr(), 0);
    scm_define(scm_intern("apply"), program);

    scm_define_subr("garbage-collect", 0, 0, 0, Subr::F0(do_gc));
}
