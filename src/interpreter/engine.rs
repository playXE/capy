use std::cmp::Ordering;
use std::intrinsics::unlikely;
use std::mem::{size_of, transmute};

use crate::bytecode::encode::Decode;
use crate::gc::ObjEdge;
use crate::runtime::control::{
    invalid_argument_violation, raise_unbound_variable, wrong_number_of_arguments_violation_at,
    wrong_number_of_values_violation_at, wrong_type_argument_violation, raise_error_ip,
};
use crate::runtime::equality::{equal, eqv};
use crate::runtime::gsubr::scm_apply_subr;
use crate::runtime::subr_arith::{
    intrinsic_add, intrinsic_div, intrinsic_mul, intrinsic_quotient, intrinsic_remainder,
    intrinsic_sub, scm_n_compare,
};
use crate::runtime::value::Value;
use crate::runtime::{arith, object::*};
use crate::vm::intrinsics::{self, cons_rest, get_callee_vcode, reinstate_continuation_x};
use crate::vm::thread::Thread;
use mmtk::util::metadata::side_metadata::GLOBAL_SIDE_METADATA_VM_BASE_ADDRESS;
use mmtk::util::ObjectReference;
use mmtk::MutatorContext;

use super::stackframe::StackElement;
use super::stackframe::*;
use crate::bytecode::opcodes::*;

#[derive(std::marker::ConstParamTy, PartialEq, Eq)]
pub struct EngineConstParams {
    /// Whether we need to inline write barrier in interpreter loop.
    ///
    /// If set to `false` all the opcodes that write to heap objects will not do
    /// extra write-barrier work. Note: intrinsics are not aware of this switch,
    /// so they would still go through slowpath.
    pub needs_write_barrier: bool,
    /// Offset to `BumpPointer` type inside `Thread` if the selected GC plan supports
    /// fast-path bump-pointer allocation. `usize::MAX` if not supported.
    pub bump_pointer_offset: usize,
}

#[cold]
#[inline(never)]
fn stack_overflow(thread: &mut Thread) {
    let ip = thread.interpreter().ip;
    raise_error_ip(thread, ip, "stack overflow", 0);
}

pub unsafe extern "C-unwind" fn rust_engine<const CONST_PARAMS: EngineConstParams>(
    thread: &mut Thread,
) -> Value {
    let mut ip: *const u8 = thread.interpreter().ip;
    let mut sp: *mut StackElement = thread.interpreter().sp;
    let mut op: u8;

    macro_rules! cache_sp {
        () => {
            sp = thread.interpreter().sp;
        };
    }
    #[allow(unused_macros)]
    macro_rules! cache_registers {
        () => {
            ip = thread.interpreter().ip;
            cache_sp!();
        };
    }

    #[allow(unused_macros)]
    macro_rules! fp_slot {
        ($i: expr) => {
            frame_slot(thread.interpreter().fp, $i as isize)
        };
    }

    macro_rules! current_program {
        () => {
            *frame_local(thread.interpreter().fp, 0)
        };
    }

    macro_rules! fp_ref {
        ($i: expr) => {
            frame_local(thread.interpreter().fp, $i as usize)
        };
    }

    macro_rules! fp_set {
        ($i: expr, $val: expr) => {
            *frame_local(thread.interpreter().fp, $i as usize) = $val
        };
    }
    #[allow(unused_macros)]
    macro_rules! sp_slot {
        ($i: expr) => {
            sp.offset($i as _)
        };
    }

    macro_rules! sp_ref {
        ($i: expr) => {
            sp.offset($i as _).read().as_value
        };
    }

    macro_rules! sp_set {
        ($i: expr, $val: expr) => {
            sp.offset($i as _).write(StackElement { as_value: $val });
        };
    }

    macro_rules! alloc_frame {
        ($n: expr) => {
            sp = thread.interpreter().fp.sub($n as _);
            if sp < thread.interpreter().stack_limit {
                sync_ip!();
                sync_sp!();
                stack_overflow(thread);
                // TODO: Expand stack
            } else {
                thread.interpreter().sp = sp;
            }
        };
    }

    macro_rules! reset_frame {
        ($n: expr) => {
            sp = thread.interpreter().fp.sub($n as _);
            thread.interpreter().sp = sp;
        };
    }

    macro_rules! frame_locals_count {
        () => {
            thread.interpreter().fp.offset_from(sp)
        };
    }

    macro_rules! frame_locals_count_from {
        ($slot: expr) => {
            frame_locals_count!() - $slot
        };
    }

    macro_rules! sync_ip {
        () => {
            thread.interpreter().ip = ip;
        };
    }

    macro_rules! sync_sp {
        () => {
            thread.interpreter().sp = sp;
        };
    }

    loop {
        op = ip.read();

        match op {
            OP_HALT => {
                let frame_size = 2;
                let first_value = frame_size;

                let nvals = frame_locals_count_from!(first_value);
                let ret = if nvals == 1 {
                    *fp_ref!(first_value)
                } else {
                    sync_ip!();
                    // todo: values
                    ////todo!()
                    Value::encode_null_value()
                };

                let fp = thread.interpreter().fp;
                thread.interpreter().fp = frame_dynamic_link(fp).cast();
                thread.interpreter().ip = frame_virtual_return_address(fp);
                thread.interpreter().sp = frame_previous_sp(fp);

                return ret;
            }

            OP_ENTER => {
                let _enter = OpEnter::read(ip);

                thread.interpreter().sp = sp;
                thread.interpreter().ip = ip;
                thread.safepoint();
                sp = thread.interpreter().sp;
                ip = thread.interpreter().ip;
                ip = ip.add(size_of::<OpEnter>());
            }

            OP_LOOP_HINT => {
                let _loop_hint = OpLoopHint::read(ip);

                thread.interpreter().sp = sp;
                thread.interpreter().ip = ip;
                thread.safepoint();
                sp = thread.interpreter().sp;
                ip = thread.interpreter().ip;
                ip = ip.add(size_of::<OpLoopHint>());
            }

            OP_CALL => {
                let call = OpCall::read(ip);
                ip = ip.add(size_of::<OpCall>());

                let old_fp = thread.interpreter().fp;
                let new_fp = frame_slot(old_fp, call.proc() as usize - 1);
                set_frame_dynamic_link(new_fp, old_fp);
                set_frame_virtual_return_address(new_fp, ip);
                thread.interpreter().fp = new_fp;

                reset_frame!(call.nlocals() as isize);
                thread.interpreter().ip = ip;

                ip = get_callee_vcode(thread);

                cache_sp!();
            }

            OP_TAIL_CALL => {
                thread.interpreter().ip = ip;
                ip = get_callee_vcode(thread);

                cache_sp!();
            }

            OP_RETURN_VALUES => {
                let old_fp = thread.interpreter().fp;
                thread.interpreter().fp = frame_dynamic_link(old_fp).cast();

                ip = frame_virtual_return_address(old_fp);
            }

            OP_RECEIVE => {
                let receive = OpReceive::read(ip);

                let dst = receive.dest();
                let proc = receive.proc();
                let nlocals = receive.nlocals();
                if unlikely(frame_locals_count!() <= proc as isize) {
                    sync_sp!();
                    sync_ip!();
                    wrong_number_of_values_violation_at(thread, ip, 1, 1, 0, &[]);
                }

                fp_set!(dst, *fp_ref!(proc));
                reset_frame!(nlocals);
                ip = ip.add(size_of::<OpReceive>());
            }

            OP_RECEIVE_VALUES => {
                let receive_values = OpReceiveValues::read(ip);

                let proc = receive_values.proc();
                let nvalues = receive_values.nvalues();
                let rest = receive_values.allow_extra();
                let count = frame_locals_count!();
                if rest {
                    if unlikely(count < proc as isize + nvalues as isize) {
                        sync_sp!();
                        sync_ip!();

                        wrong_number_of_values_violation_at(
                            thread,
                            ip,
                            nvalues as _,
                            -1,
                            (count - proc as isize - 1) as _,
                            &[],
                        );
                    }
                } else {
                    if unlikely(count != proc as isize + nvalues as isize) {
                        sync_sp!();
                        sync_ip!();
                        wrong_number_of_values_violation_at(
                            thread,
                            ip,
                            nvalues as _,
                            nvalues as _,
                            count as _,
                            &[],
                        );
                    }
                }

                ip = ip.add(size_of::<OpReceiveValues>());
            }

            OP_ASSERT_NARGS_EE => {
                let assert_nargs_ee = OpAssertNargsEe::read(ip);

                if unlikely(frame_locals_count!() != assert_nargs_ee.n() as isize) {
                    sync_sp!();
                    sync_ip!();

                    let count = frame_locals_count!() - 1;
                    let expected = assert_nargs_ee.n() as u32 - 1;
                    wrong_number_of_arguments_violation_at(
                        thread,
                        ip,
                        expected as _,
                        expected as _,
                        count as _,
                        &[],
                    );
                }

                ip = ip.add(size_of::<OpAssertNargsEe>());
            }

            OP_ASSERT_NARGS_GE => {
                let assert_nargs_ge = OpAssertNargsGe::read(ip);

                if unlikely(frame_locals_count!() < assert_nargs_ge.n() as isize) {
                    sync_sp!();
                    sync_ip!();
                    let count = frame_locals_count!() - 1;
                    let expected = assert_nargs_ge.n() as u32 - 1;

                    wrong_number_of_arguments_violation_at(
                        thread,
                        ip,
                        expected as _,
                        -1,
                        count as _,
                        &[],
                    );
                }

                ip = ip.add(size_of::<OpAssertNargsGe>());
            }

            OP_ASSERT_NARGS_LE => {
                let assert_nargs_le = OpAssertNargsLe::read(ip);

                if unlikely(frame_locals_count!() > assert_nargs_le.n() as isize) {
                    sync_sp!();
                    sync_ip!();
                    let count = frame_locals_count!() - 1;
                    let expected = assert_nargs_le.n() as u32 - 1;

                    wrong_number_of_arguments_violation_at(
                        thread,
                        ip,
                        0,
                        expected as _,
                        count as _,
                        &[],
                    );
                }

                ip = ip.add(size_of::<OpAssertNargsLe>());
            }

            /* bind-rest dst:24
             *
             * Collect any arguments at or above DST into a list, and store that
             * list at DST.
             */
            OP_BIND_REST => {
                let bind_rest = OpBindRest::read(ip);

                let dst = bind_rest.dst();
                let nargs = frame_locals_count!();

                if nargs <= dst as isize {
                    alloc_frame!(dst + 1);
                    sp_set!(0, Value::encode_null_value());
                } else {
                    sync_ip!();
                    sync_sp!();
                    let rest = cons_rest(thread, dst as _);
                    reset_frame!(dst + 1);

                    sp_set!(0, rest);
                }
                ip = ip.add(size_of::<OpBindRest>());
            }

            OP_ALLOC_FRAME => {
                let alloc_frame = OpAllocFrame::read(ip);

                alloc_frame!(alloc_frame.nlocals());
                ip = ip.add(size_of::<OpAllocFrame>());
            }

            OP_RESET_FRAME => {
                let reset_frame = OpResetFrame::read(ip);

                reset_frame!(reset_frame.nlocals());
                ip = ip.add(size_of::<OpResetFrame>());
            }

            OP_MOV => {
                let mov = OpMov::read(ip);

                let src = sp_ref!(mov.src());
                sp_set!(mov.dst(), src);
                ip = ip.add(size_of::<OpMov>());
            }

            OP_LONG_MOV => {
                let long_mov = OpLongMov::read(ip);

                let src = sp_ref!(long_mov.src());
                sp_set!(long_mov.dst(), src);
                ip = ip.add(size_of::<OpLongMov>());
            }

            OP_LONG_FMOV => {
                let long_fmov = OpLongFmov::read(ip);

                let src = *fp_ref!(long_fmov.src());
                fp_set!(long_fmov.dst(), src);
                ip = ip.add(size_of::<OpLongFmov>());
            }

            OP_PUSH => {
                let push = OpPush::read(ip);

                let src = sp_ref!(push.src());
                alloc_frame!(frame_locals_count!() + 1);
                sp_set!(0, src);
                ip = ip.add(size_of::<OpPush>());
            }

            OP_POP => {
                let pop = OpPop::read(ip);

                let val = sp_ref!(0);
                sp = sp.add(1);
                thread.interpreter().sp = sp;
                sp_set!(pop.dst(), val);
                ip = ip.add(size_of::<OpPop>());
            }

            OP_DROP => {
                let drop = OpDrop::read(ip);

                sp = sp.add(drop.n() as _);
                thread.interpreter().sp = sp;
                ip = ip.add(size_of::<OpDrop>());
            }

            OP_SHUFFLE_DOWN => {
                let shuffle_down = OpShuffleDown::read(ip);

                let nlocals = frame_locals_count!();
                let from = shuffle_down.from() as u32;
                let to = shuffle_down.to() as u32;

                let mut n = 0;
                while from + n < nlocals as u32 {
                    fp_set!(to + n, *fp_ref!(from + n));
                    n += 1;
                }

                reset_frame!(to + n);
                ip = ip.add(size_of::<OpShuffleDown>());
            }

            OP_J => {
                let j = OpJ::read(ip);
                ip = ip.add(size_of::<OpJ>());

                ip = ip.offset(j.offset() as isize);
            }

            OP_JNZ => {
                let jnz = OpJnz::read(ip);
                ip = ip.add(size_of::<OpJnz>());

                let val = sp_ref!(jnz.src());

                if !val.is_false() {
                    ip = ip.offset(jnz.offset() as isize);
                }
            }

            OP_JZ => {
                let jz = OpJz::read(ip);
                ip = ip.add(size_of::<OpJz>());

                let val = sp_ref!(jz.src());
                if val.is_false() {
                    ip = ip.offset(jz.offset() as isize);
                }
            }

            OP_MAKE_IMMEDIATE => {
                let make_immediate = OpMakeImmediate::read(ip);

                let value = Value(crate::runtime::value::EncodedValueDescriptor {
                    as_int64: make_immediate.value() as i64,
                });

                sp_set!(make_immediate.dst(), value);
                ip = ip.add(size_of::<OpMakeImmediate>());
            }

            OP_MAKE_NON_IMMEDIATE => {
                let make_non_immediate = OpMakeNonImmediate::read(ip);

                let program = current_program!();
                let constants = scm_vector_ref(
                    program.cast_as::<ScmProgram>().constants,
                    make_non_immediate.offset(),
                );

                sp_set!(make_non_immediate.dst(), constants);
                ip = ip.add(size_of::<OpMakeNonImmediate>());
            }

            OP_GLOBAL_REF => {
                let global_ref = OpGlobalRef::read(ip);

                let program = current_program!();
                let global = scm_vector_ref(
                    program.cast_as::<ScmProgram>().constants,
                    global_ref.offset(),
                );
                let value = global.cast_as::<ScmGloc>().value;
                if value.is_undefined() {
                    sync_ip!();
                    raise_unbound_variable(thread, ip, global.cast_as::<ScmGloc>().name);
                }

                sp_set!(global_ref.dst(), value);
                ip = ip.add(size_of::<OpGlobalRef>());
            }

            OP_GLOBAL_SET => {
                let global_set = OpGlobalSet::read(ip);

                let program = current_program!();

                let global = scm_vector_ref(
                    program.cast_as::<ScmProgram>().constants,
                    global_set.offset(),
                );
                let gloc = global.cast_as::<ScmGloc>();
                let src = sp_ref!(global_set.src());
                if src.is_object() {
                    reference_write::<CONST_PARAMS>(
                        thread,
                        transmute(gloc as *mut ScmGloc),
                        transmute(&mut gloc.value),
                        transmute(src),
                    );
                } else {
                    gloc.value = src;
                }

                ip = ip.add(size_of::<OpGlobalSet>());
            }

            OP_BOX => {
                let box_ = OpBox::read(ip);

                sync_sp!();
                sync_ip!();

                let boxed = thread.make_box();
                let src = sp_ref!(box_.src());
                boxed.cast_as::<ScmBox>().value = src;
                sp_set!(box_.dst(), boxed);
                ip = ip.add(size_of::<OpBox>());
            }

            OP_BOX_SET => {
                let box_set = OpBoxSet::read(ip);

                let boxed = sp_ref!(box_set.dst());
                let src = sp_ref!(box_set.src());

                if src.is_object() {
                    reference_write::<CONST_PARAMS>(
                        thread,
                        transmute(boxed.cast_as::<ScmBox>() as *mut ScmBox),
                        transmute(&mut boxed.cast_as::<ScmBox>().value),
                        transmute(src),
                    );
                } else {
                    boxed.cast_as::<ScmBox>().value = src;
                }

                ip = ip.add(size_of::<OpBoxSet>());
            }

            OP_BOX_REF => {
                let box_ref = OpBoxRef::read(ip);

                let boxed = sp_ref!(box_ref.src());
                let value = boxed.cast_as::<ScmBox>().value;
                sp_set!(box_ref.dst(), value);
                ip = ip.add(size_of::<OpBoxRef>());
            }

            OP_CONS => {
                let cons = OpCons::read(ip);

                sync_sp!();
                sync_ip!();

                let cell = thread
                    .make_cons::<false>(Value::encode_null_value(), Value::encode_null_value());

                let pair = cell.cast_as::<ScmPair>();
                pair.car = sp_ref!(cons.car());
                pair.cdr = sp_ref!(cons.cdr());

                sp_set!(cons.dst(), cell);
                ip = ip.add(size_of::<OpCons>());
            }

            OP_MAKE_VECTOR_IMMEDIATE => {
                let make_vector = OpMakeVectorImmediate::read(ip);

                sync_sp!();
                sync_ip!();

                let v = thread
                    .make_vector::<false>(make_vector.len() as _, Value::encode_bool_value(false));
                sp_set!(make_vector.dst(), v);
                ip = ip.add(size_of::<OpMakeVectorImmediate>());
            }

            OP_MAKE_VECTOR => {
                let make_vector = OpMakeVector::read(ip);

                sync_sp!();
                sync_ip!();
                let mut len = sp_ref!(make_vector.len());
                if unlikely(!len.is_int32() || len.get_int32() < 0) {
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "make-vector",
                        0,
                        "exact nonnegative fixnum",
                        len,
                        1,
                        &[&mut len],
                    )
                }
                let len = len.get_int32();

                let v = thread.make_vector::<false>(len as _, Value::encode_bool_value(false));
                sp_set!(make_vector.dst(), v);
                ip = ip.add(size_of::<OpMakeVector>());
            }

            OP_VECTOR_FILL => {
                let fill_vector = OpVectorFill::read(ip);

                let mut vector = sp_ref!(fill_vector.dst());
                let mut fill = sp_ref!(fill_vector.fill());

                if unlikely(!vector.is_vector()) {
                    sync_ip!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "vector-fill!",
                        0,
                        "vector",
                        vector,
                        1,
                        &[&mut vector, &mut fill],
                    )
                }

                let len = scm_vector_length(vector);
                for i in 0..len {
                    scm_vector_set(vector, thread, i, fill);
                }

                ip = ip.add(size_of::<OpVectorFill>());
            }

            OP_VECTOR_REF_IMM => {
                let vector_ref_imm = OpVectorRefImm::read(ip);

                let mut vector = sp_ref!(vector_ref_imm.src());
                if unlikely(!vector.is_vector()) {
                    sync_ip!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "vector-ref",
                        0,
                        "vector",
                        vector,
                        1,
                        &[
                            &mut vector,
                            &mut Value::encode_int32(vector_ref_imm.idx() as _),
                        ],
                    )
                }
                let imm = vector_ref_imm.idx();

                if unlikely(scm_vector_length(vector) <= imm) {
                    sync_ip!();
                    invalid_argument_violation::<{ usize::MAX }>(
                        thread,
                        "vector-ref",
                        "out of bounds",
                        Value::encode_int32(imm as _),
                        1,
                        2,
                        &[&mut vector, &mut Value::encode_int32(imm as _)],
                    )
                }

                let value = scm_vector_ref(vector, imm);
                sp_set!(vector_ref_imm.dst(), value);
                ip = ip.add(size_of::<OpVectorRefImm>());
            }

            OP_VECTOR_SET_IMM => {
                let vector_set_imm = OpVectorSetImm::read(ip);

                let mut vector = sp_ref!(vector_set_imm.dst());
                let mut src = sp_ref!(vector_set_imm.src());
                if unlikely(!vector.is_vector()) {
                    sync_ip!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "vector-set!",
                        0,
                        "vector",
                        vector,
                        1,
                        &[
                            &mut vector,
                            &mut Value::encode_int32(vector_set_imm.idx() as _),
                            &mut src,
                        ],
                    )
                }
                let imm = vector_set_imm.idx();

                if unlikely(scm_vector_length(vector) <= imm) {
                    sync_ip!();
                    invalid_argument_violation::<{ usize::MAX }>(
                        thread,
                        "vector-set!",
                        "out of bounds",
                        Value::encode_int32(imm as _),
                        1,
                        3,
                        &[&mut vector, &mut Value::encode_int32(imm as _), &mut src],
                    )
                }

                let vector = vector.cast_as::<ScmVector>();
                let slot = transmute::<_, ObjEdge>(vector.values.as_mut_ptr().add(imm as usize));

                if src.is_object() {
                    reference_write::<CONST_PARAMS>(
                        thread,
                        transmute(vector),
                        slot,
                        transmute(src),
                    );
                } else {
                    vector.values.as_mut_ptr().add(imm as usize).write(src);
                }

                ip = ip.add(size_of::<OpVectorSetImm>());
            }

            OP_VECTOR_REF => {
                let vector_ref = OpVectorRef::read(ip);

                let mut vector = sp_ref!(vector_ref.src());
                let mut idx = sp_ref!(vector_ref.idx());
                if unlikely(!vector.is_vector()) {
                    sync_ip!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "vector-ref",
                        0,
                        "vector",
                        vector,
                        1,
                        &[&mut vector, &mut idx],
                    )
                }

                if unlikely(!idx.is_int32() || idx.get_int32() < 0) {
                    sync_ip!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "vector-ref",
                        1,
                        "exact nonnegative fixnum",
                        idx,
                        1,
                        &[&mut vector, &mut idx],
                    )
                }
                let idxx = idx.get_int32();
                if unlikely(idxx as u32 >= scm_vector_length(vector)) {
                    sync_ip!();
                    invalid_argument_violation::<{ usize::MAX }>(
                        thread,
                        "vector-ref",
                        "out of bounds",
                        idx,
                        1,
                        2,
                        &[&mut vector, &mut idx],
                    )
                }
                let value = scm_vector_ref(vector, idxx as _);
                sp_set!(vector_ref.dst(), value);
                ip = ip.add(size_of::<OpVectorRef>());
            }

            OP_VECTOR_SET => {
                let vector_set = OpVectorSet::read(ip);

                let mut vector = sp_ref!(vector_set.dst());
                let mut idx = sp_ref!(vector_set.idx());
                let mut src = sp_ref!(vector_set.src());
                if unlikely(!vector.is_vector()) {
                    sync_ip!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "vector-set!",
                        0,
                        "vector",
                        vector,
                        1,
                        &[&mut vector, &mut idx, &mut src],
                    )
                }

                if unlikely(!idx.is_int32() || idx.get_int32() < 0) {
                    sync_ip!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "vector-set!",
                        1,
                        "exact nonnegative fixnum",
                        idx,
                        1,
                        &[&mut vector, &mut idx, &mut src],
                    )
                }
                let idxx = idx.get_int32();

                if unlikely(idxx as u32 >= scm_vector_length(vector)) {
                    sync_ip!();
                    invalid_argument_violation::<{ usize::MAX }>(
                        thread,
                        "vector-set!",
                        "out of bounds",
                        idx,
                        1,
                        3,
                        &[&mut vector, &mut idx, &mut src],
                    )
                }

                let vector = vector.cast_as::<ScmVector>();
                let slot = transmute::<_, ObjEdge>(vector.values.as_mut_ptr().add(idxx as usize));

                if src.is_object() {
                    reference_write::<CONST_PARAMS>(
                        thread,
                        transmute(vector),
                        slot,
                        transmute(src),
                    );
                } else {
                    vector.values.as_mut_ptr().add(idxx as usize).write(src);
                }

                ip = ip.add(size_of::<OpVectorSet>());
            }

            OP_VECTOR_LENGTH => {
                let vector_length = OpVectorLength::read(ip);

                let mut vector = sp_ref!(vector_length.src());
                if unlikely(!vector.is_vector()) {
                    sync_ip!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "vector-length",
                        0,
                        "vector",
                        vector,
                        1,
                        &[&mut vector],
                    )
                }
                let len = scm_vector_length(vector);
                sp_set!(vector_length.dst(), Value::encode_int32(len as _));
                ip = ip.add(size_of::<OpVectorLength>());
            }

            OP_PROGRAM_REF_IMM => {
                let program_ref_imm = OpProgramRefImm::read(ip);

                let program = sp_ref!(program_ref_imm.src());
                let var = scm_program_free_variable(program, program_ref_imm.idx());
                //println!("{:p}: get {} at {} from {} to {}", ip, var, program_ref_imm.idx(), program_ref_imm.src(), program_ref_imm.dst());
                sp_set!(program_ref_imm.dst(), var);
                ip = ip.add(size_of::<OpProgramRefImm>());
            }

            OP_PROGRAM_SET_IMM => {
                let program_set_imm = OpProgramSetImm::read(ip);

                let program = sp_ref!(program_set_imm.dst());
                let var = sp_ref!(program_set_imm.src());
                scm_program_set_free_variable(program, thread, program_set_imm.idx(), var);
                ip = ip.add(size_of::<OpProgramSetImm>());
            }

            OP_MAKE_PROGRAM => {
                let make_program = OpMakeProgram::read(ip);
                ip = ip.add(size_of::<OpMakeProgram>());

                let nfree = make_program.nfree();
                let offset = make_program.offset();
                let label = ip.offset(offset as isize);
                // allocate program object with `nfree` slots for captured variables
                let program = thread.make_program::<false>(label, nfree);
                program.cast_as::<ScmProgram>().constants =
                    current_program!().cast_as::<ScmProgram>().constants;
                sp_set!(make_program.dst(), program);
            }

            OP_ADD => {
                let add = OpAdd::read(ip);

                let a = sp_ref!(add.a());
                let b = sp_ref!(add.b());

                if a.is_int32() && b.is_int32() {
                    if let Some(res) = a.get_int32().checked_add(b.get_int32()) {
                        sp_set!(add.dst(), Value::encode_int32(res));
                        ip = ip.add(size_of::<OpAdd>());

                        continue;
                    }
                }
                {
                    if a.is_inline_number() && b.is_inline_number() {
                        let lhs = a.get_number();
                        let rhs = b.get_number();

                        sp_set!(add.dst(), Value::encode_f64_value(lhs + rhs));
                    } else {
                        sync_ip!();
                        sync_sp!();
                        sp_set!(add.dst(), intrinsic_add(thread, a, b));
                    }
                }

                ip = ip.add(size_of::<OpAdd>());
            }

            OP_SUB => {
                let sub = OpSub::read(ip);

                let a = sp_ref!(sub.a());
                let b = sp_ref!(sub.b());

                if a.is_int32() && b.is_int32() {
                    if let Some(res) = a.get_int32().checked_sub(b.get_int32()) {
                        sp_set!(sub.dst(), Value::encode_int32(res));
                        ip = ip.add(size_of::<OpSub>());
                        continue;
                    }
                }
                {
                    if a.is_inline_number() && b.is_inline_number() {
                        let lhs = a.get_number();
                        let rhs = b.get_number();

                        sp_set!(sub.dst(), Value::encode_f64_value(lhs - rhs));
                    } else {
                        sync_sp!();
                        sync_ip!();

                        sp_set!(sub.dst(), intrinsic_sub(thread, a, b));
                    }
                }

                ip = ip.add(size_of::<OpSub>());
            }

            OP_NEG => {
                let neg = OpNeg::read(ip);

                let mut src = sp_ref!(neg.src());

                if src.is_int32() && src.get_int32().checked_neg().is_some() {
                    sp_set!(neg.dst(), Value::encode_int32(-src.get_int32()));
                    ip = ip.add(size_of::<OpNeg>());
                } else if src.is_double() {
                    sp_set!(neg.dst(), Value::encode_f64_value(-src.get_double()));
                    ip = ip.add(size_of::<OpNeg>());
                } else if src.is_number() {
                    sync_ip!();
                    sync_sp!();
                    sp_set!(neg.dst(), arith::arith_negate(thread, src));
                    ip = ip.add(size_of::<OpNeg>());
                } else {
                    sync_ip!();
                    sync_sp!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "--",
                        0,
                        "number",
                        src,
                        1,
                        &[&mut src],
                    )
                }
            }

            OP_MUL => {
                let mul = OpMul::read(ip);

                let a = sp_ref!(mul.a());
                let b = sp_ref!(mul.b());
                if a.is_int32() && b.is_int32() {
                    if let Some(res) = a.get_int32().checked_mul(b.get_int32()) {
                        sp_set!(mul.dst(), Value::encode_int32(res));
                        ip = ip.add(size_of::<OpMul>());
                        continue;
                    }
                }
                {
                    if a.is_inline_number() && b.is_inline_number() {
                        let lhs = a.get_number();
                        let rhs = b.get_number();

                        sp_set!(mul.dst(), Value::encode_f64_value(lhs * rhs));
                    } else {
                        sync_sp!();
                        sync_ip!();
                        sp_set!(mul.dst(), intrinsic_mul(thread, a, b));
                    }
                }

                ip = ip.add(size_of::<OpMul>());
            }

            OP_DIV => {
                let div = OpDiv::read(ip);

                let a = sp_ref!(div.a());
                let b = sp_ref!(div.b());
                sync_sp!();
                sync_ip!();
                sp_set!(div.dst(), intrinsic_div(thread, a, b));
                ip = ip.add(size_of::<OpDiv>());
            }

            OP_QUOTIENT => {
                let quotient = OpQuotient::read(ip);

                let a = sp_ref!(quotient.a());
                let b = sp_ref!(quotient.b());

                if a.is_int32() && b.is_int32() && b.get_int32() != 0 {
                    sp_set!(
                        quotient.dst(),
                        Value::encode_int32(a.get_int32().wrapping_div(b.get_int32()))
                    );
                } else {
                    sync_ip!();
                    sync_sp!();
                    sp_set!(quotient.dst(), intrinsic_quotient(thread, a, b));
                }

                ip = ip.add(size_of::<OpQuotient>());
            }

            OP_REMAINDER => {
                let remainder = OpRemainder::read(ip);

                let a = sp_ref!(remainder.a());
                let b = sp_ref!(remainder.b());

                if a.is_int32() && b.is_int32() && b.get_int32() != 0 {
                    sp_set!(
                        remainder.dst(),
                        Value::encode_int32(a.get_int32().wrapping_rem(b.get_int32()))
                    );
                } else {
                    sync_ip!();
                    sync_sp!();
                    sp_set!(remainder.dst(), intrinsic_remainder(thread, a, b));
                }

                ip = ip.add(size_of::<OpRemainder>());
            }

            OP_LESS => {
                let less = OpLess::read(ip);

                let a = sp_ref!(less.a());
                let b = sp_ref!(less.b());

                if a.is_int32() && b.is_int32() {
                    sp_set!(
                        less.dst(),
                        Value::encode_bool_value(a.get_int32() < b.get_int32())
                    );
                    ip = ip.add(size_of::<OpLess>());
                    continue;
                }
                sync_ip!();
                sync_sp!();
                let ord = scm_n_compare(thread, a, b, "<");
                sp_set!(less.dst(), Value::encode_bool_value(ord == Ordering::Less));
                ip = ip.add(size_of::<OpLess>());
            }

            OP_GREATER => {
                let greater = OpGreater::read(ip);

                let a = sp_ref!(greater.a());
                let b = sp_ref!(greater.b());

                if a.is_int32() && b.is_int32() {
                    sp_set!(
                        greater.dst(),
                        Value::encode_bool_value(a.get_int32() > b.get_int32())
                    );
                    ip = ip.add(size_of::<OpGreater>());
                    continue;
                }
                sync_ip!();
                sync_sp!();
                let ord = scm_n_compare(thread, a, b, ">");
                sp_set!(
                    greater.dst(),
                    Value::encode_bool_value(ord == Ordering::Greater)
                );
                ip = ip.add(size_of::<OpGreater>());
            }

            OP_LESS_EQUAL => {
                let less_equal = OpLessEqual::read(ip);

                let a = sp_ref!(less_equal.a());
                let b = sp_ref!(less_equal.b());

                if a.is_int32() && b.is_int32() {
                    sp_set!(
                        less_equal.dst(),
                        Value::encode_bool_value(a.get_int32() <= b.get_int32())
                    );
                    ip = ip.add(size_of::<OpLessEqual>());

                    continue;
                }
                sync_ip!();
                sync_sp!();
                let ord = scm_n_compare(thread, a, b, "<=");
                sp_set!(
                    less_equal.dst(),
                    Value::encode_bool_value(ord != Ordering::Greater)
                );
                ip = ip.add(size_of::<OpLessEqual>());
            }

            OP_GREATER_EQUAL => {
                let greater_equal = OpGreaterEqual::read(ip);

                let a = sp_ref!(greater_equal.a());
                let b = sp_ref!(greater_equal.b());

                if a.is_int32() && b.is_int32() {
                    sp_set!(
                        greater_equal.dst(),
                        Value::encode_bool_value(a.get_int32() >= b.get_int32())
                    );
                    ip = ip.add(size_of::<OpGreaterEqual>());
                    continue;
                }
                sync_ip!();
                sync_sp!();
                let ord = scm_n_compare(thread, a, b, ">=");

                sp_set!(
                    greater_equal.dst(),
                    Value::encode_bool_value(ord != Ordering::Less)
                );

                ip = ip.add(size_of::<OpGreaterEqual>());
            }
            OP_NUMERICALLY_EQUAL => {
                let numerically_equal = OpNumericallyEqual::read(ip);

                let a = sp_ref!(numerically_equal.a());
                let b = sp_ref!(numerically_equal.b());

                if a.is_int32() && b.is_int32() {
                    sp_set!(
                        numerically_equal.dst(),
                        Value::encode_bool_value(a.get_int32() == b.get_int32())
                    );
                    ip = ip.add(size_of::<OpNumericallyEqual>());
                    continue;
                }

                sync_ip!();
                sync_sp!();
                let ord = scm_n_compare(thread, a, b, "==");

                sp_set!(
                    numerically_equal.dst(),
                    Value::encode_bool_value(ord == Ordering::Equal)
                );

                ip = ip.add(size_of::<OpNumericallyEqual>());
            }

            OP_EQ => {
                let eq = OpEq::read(ip);

                let a = sp_ref!(eq.a());
                let b = sp_ref!(eq.b());

                sp_set!(eq.dst(), Value::encode_bool_value(a == b));
                ip = ip.add(size_of::<OpEq>());
            }

            OP_EQV => {
                let eqv_ = OpEqv::read(ip);

                let a = sp_ref!(eqv_.a());
                let b = sp_ref!(eqv_.b());

                sp_set!(eqv_.dst(), Value::encode_bool_value(eqv(a, b)));
                ip = ip.add(size_of::<OpEqv>());
            }

            OP_EQUAL => {
                let equal_ = OpEqual::read(ip);

                let a = sp_ref!(equal_.a());
                let b = sp_ref!(equal_.b());

                sp_set!(equal_.dst(), Value::encode_bool_value(equal(a, b)));
                ip = ip.add(size_of::<OpEqual>());
            }

            OP_HEAP_TAG_EQ => {
                let heap_tag_eq = OpHeapTagEq::read(ip);

                let src = sp_ref!(heap_tag_eq.src());
                let tag = heap_tag_eq.tag();

                sp_set!(
                    heap_tag_eq.dst(),
                    Value::encode_bool_value(
                        src.is_object() && src.get_object().header().type_id() as u32 == tag
                    )
                );
                ip = ip.add(size_of::<OpHeapTagEq>());
            }

            OP_IS_FALSE => {
                let is_false = OpIsFalse::read(ip);

                let src = sp_ref!(is_false.src());

                sp_set!(is_false.dst(), Value::encode_bool_value(src.is_false()));
                ip = ip.add(size_of::<OpIsFalse>());
            }

            OP_NOT => {
                let not = OpNot::read(ip);

                let src = sp_ref!(not.src());

                sp_set!(not.dst(), Value::encode_bool_value(src.is_false()));
                ip = ip.add(size_of::<OpNot>());
            }

            OP_IS_TRUE => {
                let is_true = OpIsTrue::read(ip);

                let src = sp_ref!(is_true.src());

                sp_set!(is_true.dst(), Value::encode_bool_value(src.is_true()));
                ip = ip.add(size_of::<OpIsTrue>());
            }

            OP_IS_NULL => {
                let is_null = OpIsNull::read(ip);

                let src = sp_ref!(is_null.src());

                sp_set!(is_null.dst(), Value::encode_bool_value(src.is_null()));
                ip = ip.add(size_of::<OpIsNull>());
            }

            OP_IS_UNDEFINED => {
                let is_undefined = OpIsUndefined::read(ip);

                let src = sp_ref!(is_undefined.src());

                sp_set!(
                    is_undefined.dst(),
                    Value::encode_bool_value(src.is_undefined())
                );
                ip = ip.add(size_of::<OpIsUndefined>());
            }

            OP_IS_INT32 => {
                let is_int32 = OpIsInt32::read(ip);

                let src = sp_ref!(is_int32.src());

                sp_set!(is_int32.dst(), Value::encode_bool_value(src.is_int32()));
                ip = ip.add(size_of::<OpIsInt32>());
            }

            OP_IS_CHAR => {
                let is_char = OpIsChar::read(ip);

                let src = sp_ref!(is_char.src());

                sp_set!(is_char.dst(), Value::encode_bool_value(src.is_char()));
                ip = ip.add(size_of::<OpIsChar>());
            }

            OP_IS_FLONUM => {
                let is_flonum = OpIsFlonum::read(ip);

                let src = sp_ref!(is_flonum.src());

                sp_set!(is_flonum.dst(), Value::encode_bool_value(src.is_double()));
                ip = ip.add(size_of::<OpIsFlonum>());
            }

            OP_IS_NUMBER => {
                let is_number = OpIsNumber::read(ip);

                let src = sp_ref!(is_number.src());

                sp_set!(is_number.dst(), Value::encode_bool_value(src.is_number()));
                ip = ip.add(size_of::<OpIsNumber>());
            }

            OP_CAR => {
                let car = OpCar::read(ip);

                let mut src = sp_ref!(car.src());

                if unlikely(!src.is_pair()) {
                    sync_ip!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "car",
                        0,
                        "pair",
                        src,
                        1,
                        &[&mut src],
                    )
                }

                sp_set!(car.dst(), scm_car(src));
                ip = ip.add(size_of::<OpCar>());
            }

            OP_CDR => {
                let cdr = OpCdr::read(ip);

                let mut src = sp_ref!(cdr.src());

                if unlikely(!src.is_pair()) {
                    sync_ip!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "cdr",
                        0,
                        "pair",
                        src,
                        1,
                        &[&mut src],
                    )
                }

                sp_set!(cdr.dst(), scm_cdr(src));
                ip = ip.add(size_of::<OpCdr>());
            }

            OP_SET_CAR => {
                let set_car = OpSetCar::read(ip);

                let mut src = sp_ref!(set_car.src());
                let mut dst = sp_ref!(set_car.dst());

                if unlikely(!dst.is_pair()) {
                    sync_ip!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "set-car!",
                        0,
                        "pair",
                        dst,
                        1,
                        &[&mut dst, &mut src],
                    )
                }

                let pair = dst.cast_as::<ScmPair>();

                if src.is_object() {
                    reference_write::<CONST_PARAMS>(
                        thread,
                        transmute(pair as *mut ScmPair),
                        transmute(&mut pair.car),
                        transmute(src),
                    );
                } else {
                    pair.car = src;
                }

                ip = ip.add(size_of::<OpSetCar>());
            }

            OP_SET_CDR => {
                let set_cdr = OpSetCdr::read(ip);

                let mut src = sp_ref!(set_cdr.src());
                let mut dst = sp_ref!(set_cdr.dst());

                if unlikely(!dst.is_pair()) {
                    sync_ip!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "set-cdr!",
                        0,
                        "pair",
                        dst,
                        1,
                        &[&mut dst, &mut src],
                    )
                }

                let pair = dst.cast_as::<ScmPair>();

                if src.is_object() {
                    reference_write::<CONST_PARAMS>(
                        thread,
                        transmute(pair as *mut ScmPair),
                        transmute(&mut pair.cdr),
                        transmute(src),
                    );
                } else {
                    pair.cdr = src;
                }
                ip = ip.add(size_of::<OpSetCdr>());
            }

            OP_CHAR_EQUAL => {
                let char_equal = OpCharEqual::read(ip);

                let mut a = sp_ref!(char_equal.a());
                let mut b = sp_ref!(char_equal.b());
                if !a.is_char() {
                    sync_ip!();
                    sync_sp!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "char=?",
                        0,
                        "char",
                        a,
                        2,
                        &[&mut a, &mut b],
                    );
                }

                if !b.is_char() {
                    sync_ip!();
                    sync_sp!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "char=?",
                        1,
                        "char",
                        b,
                        2,
                        &[&mut a, &mut b],
                    );
                }

                sp_set!(
                    char_equal.dst(),
                    Value::encode_bool_value(a.get_char() == b.get_char())
                );
                ip = ip.add(size_of::<OpCharEqual>());
            }

            OP_CHAR_LESS => {
                let char_less = OpCharLess::read(ip);

                let mut a = sp_ref!(char_less.a());
                let mut b = sp_ref!(char_less.b());
                if !a.is_char() {
                    sync_ip!();
                    sync_sp!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "char<?",
                        0,
                        "char",
                        a,
                        2,
                        &[&mut a, &mut b],
                    );
                }

                if !b.is_char() {
                    sync_ip!();
                    sync_sp!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "char<?",
                        1,
                        "char",
                        b,
                        2,
                        &[&mut a, &mut b],
                    );
                }

                sp_set!(
                    char_less.dst(),
                    Value::encode_bool_value(a.get_char() < b.get_char())
                );
                ip = ip.add(size_of::<OpCharLess>());
            }

            OP_CHAR_GREATER => {
                let char_greater = OpCharGreater::read(ip);

                let mut a = sp_ref!(char_greater.a());
                let mut b = sp_ref!(char_greater.b());
                if !a.is_char() {
                    sync_ip!();
                    sync_sp!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "char>?",
                        0,
                        "char",
                        a,
                        2,
                        &[&mut a, &mut b],
                    );
                }

                if !b.is_char() {
                    sync_ip!();
                    sync_sp!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "char>?",
                        1,
                        "char",
                        b,
                        2,
                        &[&mut a, &mut b],
                    );
                }

                sp_set!(
                    char_greater.dst(),
                    Value::encode_bool_value(a.get_char() > b.get_char())
                );
                ip = ip.add(size_of::<OpCharGreater>());
            }

            OP_CHAR_LESS_EQUAL => {
                let char_less_equal = OpCharLessEqual::read(ip);

                let mut a = sp_ref!(char_less_equal.a());
                let mut b = sp_ref!(char_less_equal.b());
                if !a.is_char() {
                    sync_ip!();
                    sync_sp!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "char<=?",
                        0,
                        "char",
                        a,
                        2,
                        &[&mut a, &mut b],
                    );
                }

                if !b.is_char() {
                    sync_ip!();
                    sync_sp!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "char<=?",
                        1,
                        "char",
                        b,
                        2,
                        &[&mut a, &mut b],
                    );
                }

                sp_set!(
                    char_less_equal.dst(),
                    Value::encode_bool_value(a.get_char() <= b.get_char())
                );
                ip = ip.add(size_of::<OpCharLessEqual>());
            }

            OP_CHAR_GREATER_EQUAL => {
                let char_greater_equal = OpCharGreaterEqual::read(ip);

                let mut a = sp_ref!(char_greater_equal.a());
                let mut b = sp_ref!(char_greater_equal.b());
                if !a.is_char() {
                    sync_ip!();
                    sync_sp!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "char>=?",
                        0,
                        "char",
                        a,
                        2,
                        &[&mut a, &mut b],
                    );
                }

                if !b.is_char() {
                    sync_ip!();
                    sync_sp!();
                    wrong_type_argument_violation::<{ usize::MAX }>(
                        thread,
                        "char>=?",
                        1,
                        "char",
                        b,
                        2,
                        &[&mut a, &mut b],
                    );
                }

                sp_set!(
                    char_greater_equal.dst(),
                    Value::encode_bool_value(a.get_char() >= b.get_char())
                );
                ip = ip.add(size_of::<OpCharGreaterEqual>());
            }

            OP_BIND_OPTIONALS => {
                let bind_optionals = OpBindOptionals::read(ip);

                let nlocals = bind_optionals.nargs() as isize;
                let mut nargs = frame_locals_count!();
                if nargs < nlocals {
                    alloc_frame!(nlocals);
                    while nargs < nlocals {
                        fp_set!(nargs, Value::encode_undefined_value());
                        nargs += 1;
                    }
                }
                ip = ip.add(size_of::<OpBindOptionals>());
            }

            OP_SUBR_CALL => {
                let subr_call = OpSubrCall::read(ip);

                let idx = subr_call.idx();

                let ret = scm_apply_subr(thread, sp, idx);

                reset_frame!(1);
                sp_set!(0, ret);
                ip = ip.add(size_of::<OpSubrCall>());
            }

            OP_EXPAND_APPLY_ARGUMENT => {
                let _ = OpExpandApplyArgument::read(ip);

                thread.interpreter().ip = ip;
                thread.interpreter().sp = sp;
                intrinsics::expand_apply_argument(thread);
                ip = thread.interpreter().ip;
                sp = thread.interpreter().sp;
                ip = ip.add(size_of::<OpExpandApplyArgument>());
            }

            OP_CONTINUATION_CALL => {
                let continuation_call = OpContinuationCall::read(ip);
                let cont =
                    scm_program_free_variable(current_program!(), continuation_call.contregs as _);
                thread.interpreter().ip = ip;
                thread.interpreter().sp = sp;

                reinstate_continuation_x(thread, cont);
            }

            OP_CAPTURE_CONTINUATION => {
                let capture_continuation = OpCaptureContinuation::read(ip);
                ip = ip.add(size_of::<OpCaptureContinuation>());

                let dst = capture_continuation.dst();
                thread.interpreter().ip = ip;
                thread.interpreter().sp = sp;
                let cont = crate::runtime::control::capture_continuation(thread);
                sp = thread.interpreter().sp;
                ip = thread.interpreter().ip;
                sp_set!(dst, cont);
            }

            _ => {
                #[cfg(debug_assertions)]
                {
                    unreachable!("unknown opcode: {}", ip.read())
                }
                #[cfg(not(debug_assertions))]
                {
                    std::hint::unreachable_unchecked();
                }
            }
        }
    }
}

#[inline(always)]
unsafe fn reference_write<const PARAMS: EngineConstParams>(
    thread: &mut Thread,
    src: ObjectReference,
    slot: ObjEdge,
    target: ObjectReference,
) {
    // use transmutes here because MMTk calls are inlined only with PGO enabled builds
    // and we still want somewhat decent performance when developing without PGO.
    let addr = transmute::<_, *mut ObjectReference>(slot);
    addr.write(target);
    if PARAMS.needs_write_barrier {
        // load unlogged bit from side-metadata.
        // if it is set then we invoke slow-path from MMTk.
        let addr = transmute::<_, usize>(src);
        let meta_addr = transmute::<_, usize>(GLOBAL_SIDE_METADATA_VM_BASE_ADDRESS) + (addr >> 6);
        let shift = (addr >> 3) & 0b111;

        let byte_val = (meta_addr as *const u8).read();

        if unlikely((byte_val >> shift) & 1 == 1) {
            thread
                .mutator()
                .barrier()
                .object_reference_write_slow(src, slot, target);
        }
    }
}

pub unsafe fn fast_reference_write(
    thread: &mut Thread,
    src: ObjectReference,
    slot: ObjEdge,
    target: ObjectReference,
) {
    reference_write::<
        {
            EngineConstParams {
                needs_write_barrier: true,
                bump_pointer_offset: usize::MAX,
            }
        },
    >(thread, src, slot, target)
}
