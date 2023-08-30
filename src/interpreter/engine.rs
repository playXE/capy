use std::cmp::Ordering;
use std::intrinsics::unlikely;
use std::mem::{size_of, transmute};
use std::ptr::null;

use mmtk::memory_manager::object_reference_write;
use crate::bytecode::encode::Decode;
use crate::raise_exn;
use crate::runtime::equality::{equal, eqv, scm_compare};
use crate::runtime::gsubr::scm_apply_subr;
use crate::runtime::object::*;
use crate::runtime::value::Value;
use crate::vm::intrinsics::{self, cons_rest, get_callee_vcode, reinstate_continuation_x};
use crate::vm::thread::Thread;

use super::stackframe::StackElement;
use super::stackframe::*;
use crate::bytecode::opcodes::*;
pub unsafe extern "C-unwind" fn rust_engine(thread: &mut Thread) -> Value {
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
            std::hint::black_box(sp.offset($i as _).write(StackElement { as_value: $val }));
        };
    }

    macro_rules! alloc_frame {
        ($n: expr) => {
            sp = thread.interpreter().fp.sub($n as _);
            if sp < thread.interpreter().stack_limit {
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
        ip = ip.add(1);

        match op {
            OP_HALT => {
                let frame_size = 3;
                let first_value = frame_size;

                let nvals = frame_locals_count_from!(first_value);
                let ret = if nvals == 1 {
                    *fp_ref!(first_value)
                } else {
                    sync_ip!();
                    // todo: values
                    todo!()
                };

                let fp = thread.interpreter().fp;
                thread.interpreter().fp = frame_dynamic_link(fp);
                thread.interpreter().ip = frame_virtual_return_address(fp);
                thread.interpreter().sp = frame_previous_sp(fp);

                return ret;
            }

            OP_ENTER => {
                let _enter = OpEnter::read(ip);
                ip = ip.add(size_of::<OpEnter>());
                thread.interpreter().sp = sp;
                thread.interpreter().ip = ip;
                thread.safepoint();
                sp = thread.interpreter().sp;
                ip = thread.interpreter().ip;
            }

            OP_LOOP_HINT => {
                let _loop_hint = OpLoopHint::read(ip);
                ip = ip.add(size_of::<OpLoopHint>());
                thread.interpreter().sp = sp;
                thread.interpreter().ip = ip;
                thread.safepoint();
                sp = thread.interpreter().sp;
                ip = thread.interpreter().ip;
            }

            OP_CALL => {
                let call = OpCall::read(ip);
                ip = ip.add(size_of::<OpCall>());

                let old_fp = thread.interpreter().fp;
                let new_fp = frame_slot(old_fp, call.proc().value() as usize - 1);
                set_frame_dynamic_link(new_fp, old_fp);
                set_frame_virtual_return_address(new_fp, ip);
                set_frame_machine_return_address(new_fp, null());
                thread.interpreter().fp = new_fp;

                reset_frame!(call.nlocals().value() as isize);
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
                thread.interpreter().fp = frame_dynamic_link(old_fp);

                ip = frame_virtual_return_address(old_fp);
            }

            OP_RECEIVE => {
                let receive = OpReceive::read(ip);
                ip = ip.add(size_of::<OpReceive>());

                let dst = receive.dest();
                let proc = receive.proc();
                let nlocals = receive.nlocals();
                if unlikely(frame_locals_count!() <= proc as isize) {
                    sync_sp!();
                    sync_ip!();
                    raise_exn!(
                        FailContractArity,
                        &[],
                        "expected one value, got {} ({:p})",
                        frame_locals_count!() - 1,
                        ip
                    );
                }

                fp_set!(dst, *fp_ref!(proc));
                reset_frame!(nlocals.value());
            }

            OP_RECEIVE_VALUES => {
                let receive_values = OpReceiveValues::read(ip);
                ip = ip.add(size_of::<OpReceiveValues>());
                let proc = receive_values.proc();
                let nvalues = receive_values.nvalues();
                let rest = receive_values.allow_extra();

                if rest {
                    if unlikely(
                        frame_locals_count!() < proc.value() as isize + nvalues.value() as isize,
                    ) {
                        sync_sp!();
                        sync_ip!();
                        raise_exn!(
                            FailContractArity,
                            &[],
                            "expected at least {}, got {}",
                            nvalues.value() - 1,
                            frame_locals_count!() - proc.value() as isize - 1
                        );
                    }
                } else {
                    if unlikely(
                        frame_locals_count!() != proc.value() as isize + nvalues.value() as isize,
                    ) {
                        sync_sp!();
                        sync_ip!();
                        raise_exn!(
                            FailContractArity,
                            &[],
                            "expected {}, got {}",
                            nvalues.value() - 1,
                            frame_locals_count!() - proc.value() as isize - 1
                        );
                    }
                }
            }

            OP_ASSERT_NARGS_EE => {
                let assert_nargs_ee = OpAssertNargsEe::read(ip);
                ip = ip.add(size_of::<OpAssertNargsEe>());

                if unlikely(frame_locals_count!() != assert_nargs_ee.n().value() as isize) {
                    sync_sp!();
                    sync_ip!();
                    raise_exn!(
                        FailContractArity,
                        &[],
                        "expected {}, got {}",
                        assert_nargs_ee.n().value() - 1,
                        frame_locals_count!() - 1
                    );
                    //todo!("no values error, expected {}, got {}", assert_nargs_ee.n().value(), frame_locals_count!()); // FIXME: Throw error
                }
            }

            OP_ASSERT_NARGS_GE => {
                let assert_nargs_ge = OpAssertNargsGe::read(ip);
                ip = ip.add(size_of::<OpAssertNargsGe>());

                if unlikely(frame_locals_count!() < assert_nargs_ge.n().value() as isize) {
                    sync_sp!();
                    sync_ip!();
                    raise_exn!(
                        FailContractArity,
                        &[],
                        "expected at least {}, got {}",
                        assert_nargs_ge.n().value() - 1,
                        frame_locals_count!() - 1
                    );
                }
            }

            OP_ASSERT_NARGS_LE => {
                let assert_nargs_le = OpAssertNargsLe::read(ip);
                ip = ip.add(size_of::<OpAssertNargsLe>());

                if unlikely(frame_locals_count!() > assert_nargs_le.n().value() as isize) {
                    sync_sp!();
                    sync_ip!();
                    raise_exn!(
                        FailContractArity,
                        &[],
                        "expected at most {}, got {}",
                        assert_nargs_le.n().value() - 1,
                        frame_locals_count!() - 1
                    );
                }
            }

            /* bind-rest dst:24
             *
             * Collect any arguments at or above DST into a list, and store that
             * list at DST.
             */
            OP_BIND_REST => {
                let bind_rest = OpBindRest::read(ip);
                ip = ip.add(size_of::<OpBindRest>());

                let dst = bind_rest.dst().value();
                let nargs = frame_locals_count!();

                if nargs <= dst as isize {
                    alloc_frame!(dst + 1);
                    sp_set!(0, Value::encode_null_value());
                } else {
                    sync_ip!();
                    sync_sp!();
                    let rest = cons_rest(thread, dst);
                    reset_frame!(dst + 1);

                    sp_set!(0, rest);
                }
            }

            OP_ALLOC_FRAME => {
                let alloc_frame = OpAllocFrame::read(ip);
                ip = ip.add(size_of::<OpAllocFrame>());

                alloc_frame!(alloc_frame.nlocals().value());
            }

            OP_RESET_FRAME => {
                let reset_frame = OpResetFrame::read(ip);
                ip = ip.add(size_of::<OpResetFrame>());

                reset_frame!(reset_frame.nlocals().value());
            }

            OP_MOV => {
                let mov = OpMov::read(ip);
                ip = ip.add(size_of::<OpMov>());

                let src = sp_ref!(mov.src());
                sp_set!(mov.dst(), src);
            }

            OP_LONG_MOV => {
                let long_mov = OpLongMov::read(ip);
                ip = ip.add(size_of::<OpLongMov>());

                let src = sp_ref!(long_mov.src().value());
                sp_set!(long_mov.dst().value(), src);
            }

            OP_LONG_FMOV => {
                let long_fmov = OpLongFmov::read(ip);
                ip = ip.add(size_of::<OpLongFmov>());

                let src = *fp_ref!(long_fmov.src().value());
                fp_set!(long_fmov.dst().value(), src);
            }

            OP_PUSH => {
                let push = OpPush::read(ip);
                ip = ip.add(size_of::<OpPush>());

                let src = sp_ref!(push.src().value());
                alloc_frame!(frame_locals_count!() + 1);
                sp_set!(0, src);
            }

            OP_POP => {
                let pop = OpPop::read(ip);
                ip = ip.add(size_of::<OpPop>());
                let val = sp_ref!(0);
                sp = sp.add(1);
                thread.interpreter().sp = sp;
                sp_set!(pop.dst().value(), val);
            }

            OP_DROP => {
                let drop = OpDrop::read(ip);
                ip = ip.add(size_of::<OpDrop>());

                sp = sp.add(drop.n().value() as _);
                thread.interpreter().sp = sp;
            }

            OP_SHUFFLE_DOWN => {
                let shuffle_down = OpShuffleDown::read(ip);
                ip = ip.add(size_of::<OpShuffleDown>());

                let nlocals = frame_locals_count!();
                let from = shuffle_down.from() as u32;
                let to = shuffle_down.to() as u32;

                let mut n = 0;
                while from + n < nlocals as u32 {
                    fp_set!(to + n, *fp_ref!(from + n));
                    n += 1;
                }

                reset_frame!(to + n);
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
                ip = ip.add(size_of::<OpMakeImmediate>());

                let value = Value(crate::runtime::value::EncodedValueDescriptor {
                    as_int64: make_immediate.value() as i64,
                });

                sp_set!(make_immediate.dst(), value);
            }

            OP_MAKE_NON_IMMEDIATE => {
                let make_non_immediate = OpMakeNonImmediate::read(ip);
                ip = ip.add(size_of::<OpMakeNonImmediate>());

                let program = current_program!();
                let constants = scm_vector_ref(
                    program.cast_as::<ScmProgram>().constants,
                    make_non_immediate.offset(),
                );

                sp_set!(make_non_immediate.dst().value(), constants);
            }

            OP_GLOBAL_REF => {
                let global_ref = OpGlobalRef::read(ip);
                ip = ip.add(size_of::<OpGlobalRef>());

                let program = current_program!();
                let global = scm_vector_ref(
                    program.cast_as::<ScmProgram>().constants,
                    global_ref.offset(),
                );
                let value = global.cast_as::<ScmGloc>().value;
                if value.is_undefined() {
                    sync_ip!();
                    raise_exn!(
                        Exn,
                        &[],
                        "unbound variable: {}",
                        global.cast_as::<ScmGloc>().name
                    );
                }

                sp_set!(global_ref.dst().value(), value);
            }

            OP_GLOBAL_SET => {
                let global_set = OpGlobalSet::read(ip);
                ip = ip.add(size_of::<OpGlobalSet>());

                let program = current_program!();

                let global = scm_vector_ref(
                    program.cast_as::<ScmProgram>().constants,
                    global_set.offset(),
                );
                let gloc = global.cast_as::<ScmGloc>();
                let src = sp_ref!(global_set.src().value());
                if src.is_object() {
                    object_reference_write(
                        thread.mutator(),
                        transmute(gloc as *mut ScmGloc),
                        transmute(&mut gloc.value),
                        transmute(src),
                    );
                } else {
                    gloc.value = src;
                }
            }

            OP_BOX => {
                let box_ = OpBox::read(ip);
                ip = ip.add(size_of::<OpBox>());
                sync_sp!();
                sync_ip!();

                let boxed = thread.make_box();
                let src = sp_ref!(box_.src());
                boxed.cast_as::<ScmBox>().value = src;
                sp_set!(box_.dst(), boxed);
            }

            OP_BOX_SET => {
                let box_set = OpBoxSet::read(ip);
                ip = ip.add(size_of::<OpBoxSet>());

                let boxed = sp_ref!(box_set.dst());
                let src = sp_ref!(box_set.src());

                if src.is_object() {
                    object_reference_write(
                        thread.mutator(),
                        transmute(boxed.cast_as::<ScmBox>() as *mut ScmBox),
                        transmute(&mut boxed.cast_as::<ScmBox>().value),
                        transmute(src),
                    );
                } else {
                    boxed.cast_as::<ScmBox>().value = src;
                }
            }

            OP_BOX_REF => {
                let box_ref = OpBoxRef::read(ip);
                ip = ip.add(size_of::<OpBoxRef>());

                let boxed = sp_ref!(box_ref.src());
                let value = boxed.cast_as::<ScmBox>().value;
                sp_set!(box_ref.dst(), value);
            }

            OP_CONS => {
                let cons = OpCons::read(ip);
                ip = ip.add(size_of::<OpCons>());
                sync_sp!();
                sync_ip!();
                let cell = thread
                    .make_cons::<false>(Value::encode_null_value(), Value::encode_null_value());
                let pair = cell.cast_as::<ScmPair>();
                pair.car = sp_ref!(cons.car());
                pair.cdr = sp_ref!(cons.cdr());
              
                sp_set!(cons.dst(), cell);
            }

            OP_MAKE_VECTOR_IMMEDIATE => {
                let make_vector = OpMakeVectorImmediate::read(ip);
                ip = ip.add(size_of::<OpMakeVectorImmediate>());
                sync_sp!();
                sync_ip!();
                let v = thread
                    .make_vector::<false>(make_vector.len() as _, Value::encode_bool_value(false));
                sp_set!(make_vector.dst(), v);
            }

            OP_MAKE_VECTOR => {
                let make_vector = OpMakeVector::read(ip);
                ip = ip.add(size_of::<OpMakeVector>());
                sync_sp!();
                sync_ip!();
                let len = sp_ref!(make_vector.len());
                if !len.is_int32() {
                    sync_ip!();
                    raise_exn!(Exn, &[], "make-vector: expected integer, got {}", len);
                }
                let len = len.get_int32();

                let v = thread.make_vector::<false>(len as _, Value::encode_bool_value(false));
                sp_set!(make_vector.dst(), v);
            }

            OP_VECTOR_FILL => {
                let fill_vector = OpVectorFill::read(ip);
                ip = ip.add(size_of::<OpVectorFill>());

                let vector = sp_ref!(fill_vector.dst());
                let fill = sp_ref!(fill_vector.fill());

                if !vector.is_vector() {
                    sync_ip!();
                    raise_exn!(Exn, &[], "vector-fill!: expected vector, got {}", vector);
                }

                let len = scm_vector_length(vector);
                for i in 0..len {
                    scm_vector_set(vector, thread, i, fill);
                }
            }

            OP_VECTOR_REF_IMM => {
                let vector_ref_imm = OpVectorRefImm::read(ip);
                ip = ip.add(size_of::<OpVectorRefImm>());

                let vector = sp_ref!(vector_ref_imm.src());
                if !vector.is_vector() {
                    sync_ip!();
                    raise_exn!(Exn, &[], "vector-ref: expected vector, got {}", vector);
                }
                let imm = vector_ref_imm.idx();

                if scm_vector_length(vector) <= imm {
                    sync_ip!();
                    raise_exn!(
                        Exn,
                        &[],
                        "vector-ref: out of bounds: {} >= {}",
                        imm,
                        scm_vector_length(vector)
                    );
                }

                let value = scm_vector_ref(vector, imm);
                sp_set!(vector_ref_imm.dst(), value);
            }

            OP_VECTOR_SET_IMM => {
                let vector_set_imm = OpVectorSetImm::read(ip);
                ip = ip.add(size_of::<OpVectorSetImm>());

                let vector = sp_ref!(vector_set_imm.dst());
                if !vector.is_vector() {
                    sync_ip!();
                    raise_exn!(Exn, &[], "vector-set!: expected vector, got {}", vector);
                }
                let imm = vector_set_imm.idx();
                let src = sp_ref!(vector_set_imm.src());

                if scm_vector_length(vector) <= imm {
                    sync_ip!();
                    raise_exn!(Exn, &[], "vector-set!: out of bounds");
                }

                scm_vector_set(vector, thread, imm, src);
            }

            OP_VECTOR_REF => {
                let vector_ref = OpVectorRef::read(ip);
                ip = ip.add(size_of::<OpVectorRef>());

                let vector = sp_ref!(vector_ref.src());
                if !vector.is_vector() {
                    sync_ip!();
                    raise_exn!(Exn, &[], "vector-ref: expected vector, got {}", vector);
                }
                let idx = sp_ref!(vector_ref.idx());
                if !idx.is_int32() {
                    sync_ip!();
                    raise_exn!(Exn, &[], "vector-ref: expected integer, got {}", idx);
                }
                let idx = idx.get_int32();
                if idx as u32 >= scm_vector_length(vector) {
                    sync_ip!();
                    raise_exn!(Exn, &[], "vector-ref: out of bounds")
                }
                let value = scm_vector_ref(vector, idx as _);
                sp_set!(vector_ref.dst(), value);
            }

            OP_VECTOR_SET => {
                let vector_set = OpVectorSet::read(ip);
                ip = ip.add(size_of::<OpVectorSet>());

                let vector = sp_ref!(vector_set.dst());
                if !vector.is_vector() {
                    sync_ip!();
                    raise_exn!(Exn, &[], "vector-set!: expected vector, got {:?}", vector);
                }
                let idx = sp_ref!(vector_set.idx());
                if !idx.is_int32() {
                    sync_ip!();
                    raise_exn!(Exn, &[], "vector-set!: expected integer, got {}", idx);
                }
                let idx = idx.get_int32();
                let src = sp_ref!(vector_set.src());

                if idx as u32 >= scm_vector_length(vector) {
                    sync_ip!();
                    raise_exn!(Exn, &[], "vector-set!: out of bounds")
                }

                scm_vector_set(vector, thread, idx as _, src);
            }

            OP_VECTOR_LENGTH => {
                let vector_length = OpVectorLength::read(ip);
                ip = ip.add(size_of::<OpVectorLength>());

                let vector = sp_ref!(vector_length.src());
                if !vector.is_vector() {
                    sync_ip!();
                    raise_exn!(Exn, &[], "vector-length: expected vector, got {}", vector);
                }
                let len = scm_vector_length(vector);
                sp_set!(vector_length.dst(), Value::encode_int32(len as _));
            }

            OP_PROGRAM_REF_IMM => {
                let program_ref_imm = OpProgramRefImm::read(ip);
                ip = ip.add(size_of::<OpProgramRefImm>());

                let program = sp_ref!(program_ref_imm.src().value());
                let var = scm_program_free_variable(program, program_ref_imm.idx());
                //println!("{:p}: get {} at {} from {} to {}", ip, var, program_ref_imm.idx(), program_ref_imm.src(), program_ref_imm.dst());
                sp_set!(program_ref_imm.dst(), var);
            }

            OP_PROGRAM_SET_IMM => {
                let program_set_imm = OpProgramSetImm::read(ip);
                ip = ip.add(size_of::<OpProgramSetImm>());

                let program = sp_ref!(program_set_imm.dst().value());
                let var = sp_ref!(program_set_imm.src());
                //println!("{:p}: set {} at {} from {}", ip, var, program_set_imm.idx(), program_set_imm.src());
                scm_program_set_free_variable(program, thread, program_set_imm.idx(), var);
            }

            OP_MAKE_PROGRAM => {
                let make_program = OpMakeProgram::read(ip);
                ip = ip.add(size_of::<OpMakeProgram>());

                let nfree = make_program.nfree();
                let offset = make_program.offset();
                let label = ip.offset(offset as isize);

                let program = thread.make_program::<false>(label, nfree);
                program.cast_as::<ScmProgram>().constants =
                    current_program!().cast_as::<ScmProgram>().constants;
                sp_set!(make_program.dst(), program);
            }

            OP_ADD => {
                let add = OpAdd::read(ip);
                ip = ip.add(size_of::<OpAdd>());

                let a = sp_ref!(add.a());
                let b = sp_ref!(add.b());

                if a.is_int32() && b.is_int32() {
                    if let Some(result) = a.get_int32().checked_add(b.get_int32()) {
                        sp_set!(add.dst(), Value::encode_int32(result));
                        continue;
                    } else {
                        todo!("ovf");
                    }
                } else {
                    raise_exn!(Fail, &[], "NYI: Slow add: {}, {}", a,b);
                    //todo!("slow add {}, {}", a, b);
                }
            }

            OP_SUB => {
                let sub = OpSub::read(ip);
                ip = ip.add(size_of::<OpSub>());

                let a = sp_ref!(sub.a());
                let b = sp_ref!(sub.b());

                if a.is_int32() && b.is_int32() {
                    if let Some(result) = a.get_int32().checked_sub(b.get_int32()) {
                        sp_set!(sub.dst(), Value::encode_int32(result));
                        continue;
                    }
                } else {
                    todo!("slow sub")
                }
            }

            OP_MUL => {
                let mul = OpMul::read(ip);
                ip = ip.add(size_of::<OpMul>());

                let a = sp_ref!(mul.a());
                let b = sp_ref!(mul.b());
                if a.is_int32() && b.is_int32() {
                    if let Some(result) = a.get_int32().checked_mul(b.get_int32()) {
                        sp_set!(mul.dst(), Value::encode_int32(result));
                        continue;
                    }
                } else {
                    todo!("slow mul")
                }
            }

            OP_DIV => {
                let div = OpDiv::read(ip);
                ip = ip.add(size_of::<OpDiv>());

                let a = sp_ref!(div.a());
                let b = sp_ref!(div.b());

                if a.is_int32() && b.is_int32() {
                    if let Some(result) = a.get_int32().checked_div(b.get_int32()) {
                        sp_set!(div.dst(), Value::encode_int32(result));
                        continue;
                    }
                    // TODO: Load `ratnum.scm` file and invoke 
                } else {
                    todo!("slow div")
                }
            }

            OP_QUOTIENT => {
                let quotient = OpQuotient::read(ip);
                ip = ip.add(size_of::<OpQuotient>());

                let a = sp_ref!(quotient.a());
                let b = sp_ref!(quotient.b());

                if a.is_int32() && b.is_int32() {
                    if let Some(result) = a.get_int32().checked_div(b.get_int32()) {
                        sp_set!(quotient.dst(), Value::encode_int32(result));
                        continue;
                    }
                } else {
                    todo!("slow quotient")
                }
            }

            OP_MOD => {
                let modulo = OpMod::read(ip);
                ip = ip.add(size_of::<OpMod>());

                let a = sp_ref!(modulo.a());
                let b = sp_ref!(modulo.b());

                if a.is_int32() && b.is_int32() {
                    if let Some(result) = a.get_int32().checked_rem(b.get_int32()) {
                        sp_set!(modulo.dst(), Value::encode_int32(result));
                        continue;
                    }
                } else {
                    todo!("slow modulo")
                }
            }

            OP_LESS => {
                let less = OpLess::read(ip);
                ip = ip.add(size_of::<OpLess>());

                let a = sp_ref!(less.a());
                let b = sp_ref!(less.b());

                if a.is_int32() && b.is_int32() {
                    sp_set!(
                        less.dst(),
                        Value::encode_bool_value(a.get_int32() < b.get_int32())
                    );
                    continue;
                }

                let cmp = scm_compare(a, b);

                match cmp {
                    Some(Ordering::Less) => {
                        sp_set!(less.dst(), Value::encode_bool_value(true));
                        continue;
                    }

                    _ => {
                        sp_set!(less.dst(), Value::encode_bool_value(false));
                        continue;
                    }
                }
            }

            OP_GREATER => {
                let greater = OpGreater::read(ip);
                ip = ip.add(size_of::<OpGreater>());

                let a = sp_ref!(greater.a());
                let b = sp_ref!(greater.b());

                let cmp = scm_compare(a, b);

                match cmp {
                    Some(Ordering::Greater) => {
                        sp_set!(greater.dst(), Value::encode_bool_value(true));
                        continue;
                    }

                    _ => {
                        sp_set!(greater.dst(), Value::encode_bool_value(false));
                        continue;
                    }
                }
            }

            OP_LESS_EQUAL => {
                let less_equal = OpLessEqual::read(ip);
                ip = ip.add(size_of::<OpLessEqual>());

                let a = sp_ref!(less_equal.a());
                let b = sp_ref!(less_equal.b());

                let cmp = scm_compare(a, b);

                match cmp {
                    Some(Ordering::Less) | Some(Ordering::Equal) => {
                        sp_set!(less_equal.dst(), Value::encode_bool_value(true));
                        continue;
                    }

                    _ => {
                        sp_set!(less_equal.dst(), Value::encode_bool_value(false));
                        continue;
                    }
                }
            }

            OP_GREATER_EQUAL => {
                let greater_equal = OpGreaterEqual::read(ip);
                ip = ip.add(size_of::<OpGreaterEqual>());

                let a = sp_ref!(greater_equal.a());
                let b = sp_ref!(greater_equal.b());

                let cmp = scm_compare(a, b);

                match cmp {
                    Some(Ordering::Greater) | Some(Ordering::Equal) => {
                        sp_set!(greater_equal.dst(), Value::encode_bool_value(true));
                        continue;
                    }

                    _ => {
                        sp_set!(greater_equal.dst(), Value::encode_bool_value(false));
                        continue;
                    }
                }
            }
            OP_NUMERICALLY_EQUAL => {
                let numerically_equal = OpNumericallyEqual::read(ip);
                ip = ip.add(size_of::<OpNumericallyEqual>());

                let a = sp_ref!(numerically_equal.a());
                let b = sp_ref!(numerically_equal.b());

                let cmp = scm_compare(a, b);

                match cmp {
                    Some(Ordering::Equal) => {
                        sp_set!(numerically_equal.dst(), Value::encode_bool_value(true));
                        continue;
                    }

                    _ => {
                        sp_set!(numerically_equal.dst(), Value::encode_bool_value(false));
                        continue;
                    }
                }
            }

            OP_EQ => {
                let eq = OpEq::read(ip);
                ip = ip.add(size_of::<OpEq>());

                let a = sp_ref!(eq.a());
                let b = sp_ref!(eq.b());

                sp_set!(eq.dst(), Value::encode_bool_value(a == b));
            }

            OP_EQV => {
                let eqv_ = OpEqv::read(ip);
                ip = ip.add(size_of::<OpEqv>());

                let a = sp_ref!(eqv_.a());
                let b = sp_ref!(eqv_.b());

                sp_set!(eqv_.dst(), Value::encode_bool_value(eqv(a, b)));
            }

            OP_EQUAL => {
                let equal_ = OpEqual::read(ip);
                ip = ip.add(size_of::<OpEqual>());

                let a = sp_ref!(equal_.a());
                let b = sp_ref!(equal_.b());

                sp_set!(equal_.dst(), Value::encode_bool_value(equal(a, b)));
            }

            OP_HEAP_TAG_EQ => {
                let heap_tag_eq = OpHeapTagEq::read(ip);
                ip = ip.add(size_of::<OpHeapTagEq>());

                let src = sp_ref!(heap_tag_eq.src().value());
                let tag = heap_tag_eq.tag();

                sp_set!(
                    heap_tag_eq.dst(),
                    Value::encode_bool_value(
                        src.is_object() && src.get_object().header().type_id() as u32 == tag
                    )
                );
            }

            OP_IS_FALSE => {
                let is_false = OpIsFalse::read(ip);
                ip = ip.add(size_of::<OpIsFalse>());

                let src = sp_ref!(is_false.src());

                sp_set!(is_false.dst(), Value::encode_bool_value(src.is_false()));
            }

            OP_NOT => {
                let not = OpNot::read(ip);
                ip = ip.add(size_of::<OpNot>());

                let src = sp_ref!(not.src());

                sp_set!(not.dst(), Value::encode_bool_value(src.is_false()));
            }

            OP_IS_TRUE => {
                let is_true = OpIsTrue::read(ip);
                ip = ip.add(size_of::<OpIsTrue>());

                let src = sp_ref!(is_true.src());

                sp_set!(is_true.dst(), Value::encode_bool_value(src.is_true()));
            }

            OP_IS_NULL => {
                let is_null = OpIsNull::read(ip);
                ip = ip.add(size_of::<OpIsNull>());

                let src = sp_ref!(is_null.src());

                sp_set!(is_null.dst(), Value::encode_bool_value(src.is_null()));
            }

            OP_IS_UNDEFINED => {
                let is_undefined = OpIsUndefined::read(ip);
                ip = ip.add(size_of::<OpIsUndefined>());

                let src = sp_ref!(is_undefined.src());

                sp_set!(
                    is_undefined.dst(),
                    Value::encode_bool_value(src.is_undefined())
                );
            }

            OP_IS_INT32 => {
                let is_int32 = OpIsInt32::read(ip);
                ip = ip.add(size_of::<OpIsInt32>());

                let src = sp_ref!(is_int32.src());

                sp_set!(is_int32.dst(), Value::encode_bool_value(src.is_int32()));
            }

            OP_IS_CHAR => {
                let is_char = OpIsChar::read(ip);
                ip = ip.add(size_of::<OpIsChar>());

                let src = sp_ref!(is_char.src());

                sp_set!(is_char.dst(), Value::encode_bool_value(src.is_char()));
            }

            OP_IS_FLONUM => {
                let is_flonum = OpIsFlonum::read(ip);
                ip = ip.add(size_of::<OpIsFlonum>());

                let src = sp_ref!(is_flonum.src());

                sp_set!(is_flonum.dst(), Value::encode_bool_value(src.is_double()));
            }

            OP_CAR => {
                let car = OpCar::read(ip);
                ip = ip.add(size_of::<OpCar>());

                let src = sp_ref!(car.src());

                if unlikely(!src.is_pair()) {
                    sync_ip!();
                    raise_exn!(Exn, &[], "car: expected pair, got {}", src)
                }

                sp_set!(car.dst(), scm_car(src));
            }

            OP_CDR => {
                let cdr = OpCdr::read(ip);
                ip = ip.add(size_of::<OpCdr>());

                let src = sp_ref!(cdr.src());

                if unlikely(!src.is_pair()) {
                    sync_ip!();
                    raise_exn!(Exn, &[], "cdr: expected pair, got {}", src)
                }

                sp_set!(cdr.dst(), scm_cdr(src));
            }

            OP_SET_CAR => {
                let set_car = OpSetCar::read(ip);
                ip = ip.add(size_of::<OpSetCar>());

                let src = sp_ref!(set_car.src());
                let dst = sp_ref!(set_car.dst());

                if unlikely(!dst.is_pair()) {
                    sync_ip!();
                    raise_exn!(Exn, &[], "set-car!: expected pair, got {}", dst)
                }

                scm_set_car(dst, thread, src);
            }

            OP_SET_CDR => {
                let set_cdr = OpSetCdr::read(ip);
                ip = ip.add(size_of::<OpSetCdr>());

                let src = sp_ref!(set_cdr.src());
                let dst = sp_ref!(set_cdr.dst());

                if unlikely(!dst.is_pair()) {
                    sync_ip!();
                    raise_exn!(Exn, &[], "set-cdr!: expected pair, got {}", dst)
                }

                scm_set_cdr(dst, thread, src);
            }

            OP_BIND_OPTIONALS => {
                let bind_optionals = OpBindOptionals::read(ip);
                ip = ip.add(size_of::<OpBindOptionals>());
                let nlocals = bind_optionals.nargs().value() as isize;
                let mut nargs = frame_locals_count!();
                if nargs < nlocals {
                    alloc_frame!(nlocals);
                    while nargs < nlocals {
                        fp_set!(nargs, Value::encode_undefined_value());
                        nargs += 1;
                    }
                }
            }

            OP_SUBR_CALL => {
                let subr_call = OpSubrCall::read(ip);
                ip = ip.add(size_of::<OpSubrCall>());

                let idx = subr_call.idx().value();

                let ret = scm_apply_subr(thread, sp, idx);

                reset_frame!(1);
                sp_set!(0, ret);
            }

            OP_EXPAND_APPLY_ARGUMENT => {
                let _ = OpExpandApplyArgument::read(ip);
                ip = ip.add(size_of::<OpExpandApplyArgument>());

                thread.interpreter().ip = ip;
                thread.interpreter().sp = sp;
                intrinsics::expand_apply_argument(thread);
                ip = thread.interpreter().ip;
                sp = thread.interpreter().sp;
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

            _ => (),
        }
    }
}
