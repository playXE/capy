use std::ptr::null_mut;

use crate::{
    interpreter::stackframe::{FrameKind, ScmFrame, subr_frame_previous},
    vm::{scm_virtual_machine, thread::Thread},
};

use super::{
    arith::scm_to_usize_who,
    control::{wrong_type_argument_violation, ScmContinuation, VMCont, invalid_argument_violation},
    gsubr::{scm_define_subr, Subr},
    object::{
        scm_car, scm_cdr, scm_program_code, scm_program_free_variable, scm_tuple_ref,
        scm_tuple_set, ScmCellHeader, TypeId,
    },
    symbol::scm_intern,
    value::Value,
};

unsafe fn stack_depth(frame: &ScmFrame) -> usize {
    let mut n = 1;
    let mut tmp = *frame;
    while let Some(f) = tmp.previous() {
        n += 1;
        tmp = f;
    }

    n
}

fn narrow_stack(mut len: isize, frame: &mut ScmFrame, inner_cut: Value, outer_cut: Value) -> isize {
    let mut inner_cut_range = None;
    let mut outer_cut_range = None;
    let mut inner_cut_n = None;
    let mut outer_cut_n = None::<usize>;
    if inner_cut.is_program() {
        inner_cut_range = scm_virtual_machine()
            .images
            .program_address_range(scm_program_code(inner_cut));
    } else if inner_cut.is_pair() {
        inner_cut_range = Some((
            scm_to_usize_who("make-stack", scm_car(inner_cut)) as _,
            scm_to_usize_who("make-stack", scm_car(scm_car(inner_cut))) as _,
        ));
    } else if inner_cut.is_int32() {
        inner_cut_n = Some(scm_to_usize_who("make-stack", inner_cut));
    }

    if outer_cut.is_program() {
        outer_cut_range = scm_virtual_machine()
            .images
            .program_address_range(scm_program_code(outer_cut));
    } else if outer_cut.is_pair() {
        outer_cut_range = Some((
            scm_to_usize_who("make-stack", scm_car(outer_cut)) as _,
            scm_to_usize_who("make-stack", scm_car(scm_car(outer_cut))) as _,
        ));
    } else if outer_cut.is_int32() {
        outer_cut_n = Some(scm_to_usize_who("make-stack", outer_cut) as _);
    }

    if let Some((low_pc, high_pc)) = inner_cut_range {
        while len != 0 {
            let pc = frame.ip;
            len -= 1;
            let tmp = unsafe { frame.previous() };
            if let Some(tmp) = tmp {
                frame.stack_holder = tmp.stack_holder;
                frame.ip = tmp.ip;
                frame.fp_offset = tmp.fp_offset;
                frame.sp_offset = tmp.sp_offset;
            }

            if low_pc <= pc && pc < high_pc {
                break;
            }
        }
    } else if let Some(inner) = inner_cut_n {
        while inner != 0 && len != 0 {
            len -= 1;
            let tmp = unsafe { frame.previous() };
            if let Some(tmp) = tmp {
                frame.stack_holder = tmp.stack_holder;
                frame.ip = tmp.ip;
                frame.fp_offset = tmp.fp_offset;
                frame.sp_offset = tmp.sp_offset;
            }
        }
    }

    if let Some((low_pc, high_pc)) = outer_cut_range {
        let mut tmp = *frame;

        let mut new_len = 0;
        for i in 0..len {
            let pc = tmp.ip;
            if low_pc <= pc && pc < high_pc {
                new_len = i;
            }

            tmp = unsafe { tmp.previous() }.unwrap();
        }

        len = new_len;
    } else if let Some(outer) = outer_cut_n {
        if (outer as isize) < len {
            len -= outer as isize;
        } else {
            len = 0;
        }
    }

    len
}

extern "C-unwind" fn stack_p(_thread: &mut Thread, val: &mut Value) -> Value {
    Value::encode_bool_value(val.is_tuple() && scm_tuple_ref(*val, 0) == scm_intern("type:stack"))
}

impl Value {
    pub fn is_stack(&self) -> bool {
        self.is_tuple() && scm_tuple_ref(*self, 0) == scm_intern("type:stack")
    }
}

extern "C-unwind" fn make_stack(thread: &mut Thread, obj: &mut Value, args: &mut Value) -> Value {
    let mut frame = ScmFrame {
        fp_offset: 0,
        sp_offset: 0,
        header: ScmCellHeader::new(TypeId::Frame),
        kind: FrameKind::VM,
        stack_holder: null_mut(),
        ip: null_mut(),
    };
    unsafe {
        if obj.is_bool() && obj.is_true() {
            let cont = thread.capture_current_stack();

            frame.kind = FrameKind::Cont;
            frame.stack_holder = cont.get_object().0 as _;
            frame.fp_offset = cont.cast_as::<VMCont>().fp_offset;
            frame.sp_offset = cont.cast_as::<VMCont>().stack_size as _;
            frame.ip = cont.cast_as::<VMCont>().vra;
        } else if obj.is_frame() {
            let obj = obj.frame();
            frame.stack_holder = obj.stack_holder;
            frame.fp_offset = obj.fp_offset;
            frame.sp_offset = obj.sp_offset;
            frame.ip = obj.ip;
            frame.kind = obj.kind;
        } else if obj.is_continuation() {
            let cont = scm_program_free_variable(*obj, 0).cast_as::<ScmContinuation>();
            frame.stack_holder = cont.vm_cont.get_object().0 as _;
            frame.fp_offset = cont.vm_cont.cast_as::<VMCont>().fp_offset;
            frame.sp_offset = cont.vm_cont.cast_as::<VMCont>().stack_size as _;
            frame.ip = cont.vm_cont.cast_as::<VMCont>().vra;
            frame.kind = FrameKind::Cont;
        } else {
            println!("ayo {}", obj);
            wrong_type_argument_violation::<1>(
                thread,
                "make-stack",
                0,
                "frame, continuation or #t",
                *obj,
                2,
                &[obj, args],
            )
        }

        /* Skip initial boot frame, if any.  This is possible if the frame
        originates from a captured continuation.  */
        if frame.ip == scm_program_code(scm_virtual_machine().boot_continuation)
            && frame.previous().is_none()
        {
            return Value::encode_bool_value(false);
        }

        let mut n = stack_depth(&frame) as isize;

        while n > 0 && !args.is_null() {
            let inner_cut;
            let outer_cut;
            inner_cut = scm_car(*args);
            *args = scm_cdr(*args);

            if args.is_null() {
                outer_cut = Value::encode_int32(0);
            } else {
                outer_cut = scm_car(*args);
                *args = scm_cdr(*args);
            }

            n = narrow_stack(n, &mut frame, inner_cut, outer_cut);
        }

        if n > 0 {
            let stack = thread.make_tuple::<false>(3, Value::encode_undefined_value());
            scm_tuple_set(stack, thread, 0, scm_intern("type:stack"));
            scm_tuple_set(stack, thread, 1, Value::encode_int32(n as _));
            let frame = thread.make_frame(frame.kind, &frame);
            scm_tuple_set(stack, thread, 2, frame);

            return stack;
        } else {
            Value::encode_bool_value(false)
        }
    }
}

extern "C-unwind" fn stack_ref(thread: &mut Thread, stack: &mut Value, index: &mut Value) -> Value {
    if !stack.is_stack() {
        wrong_type_argument_violation::<{usize::MAX}>(
            thread,
            "stack-ref",
            0,
            "stack",
            *stack,
            2,
            &[stack, index],
        )
    } else if !index.is_int32() || index.get_int32() < 0 {
        wrong_type_argument_violation::<{usize::MAX}>(
            thread,
            "stack-ref",
            1,
            "nonnegative fixnum",
            *index,
            2,
            &[stack, index],
        )
    } else {
        let mut c_index = scm_to_usize_who("stack-ref", *index);
       
        let n = scm_tuple_ref(*stack, 1).get_int32() as usize;
        if n <= c_index {
            invalid_argument_violation::<{usize::MAX}>(
                thread,
                "stack-ref",
                "index out of range",
                *index,
                1,
                2,
                &[stack, index],
            );
        }
        let mut frame = scm_tuple_ref(*stack, 2);
        while c_index != 0 {
            c_index -= 1;
            frame = subr_frame_previous(thread, &mut frame);
        }

        frame
    }
}

pub(crate) fn init_stack_builtins() {
    scm_define_subr("stack?", 1, 0, 0, Subr::F1(stack_p));
    scm_define_subr("make-stack", 1, 0, 1, Subr::F2(make_stack));
    scm_define_subr("stack-ref", 2, 0, 0, Subr::F2(stack_ref));
}
