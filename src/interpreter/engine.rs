use std::intrinsics::unlikely;
use std::mem::size_of;
use std::ptr::null;

use crate::bytecode::encode::Decode;
use crate::runtime::value::Value;
use crate::vm::intrinsics::{get_callee_vcode, cons_rest};
use crate::vm::thread::Thread;

use super::stackframe::StackElement;
use super::stackframe::*;
use crate::bytecode::opcodes::*;
pub unsafe  extern "C-unwind" fn rust_engine(thread: &mut Thread) -> Value {
    let mut ip: *const u8 = thread.interpreter().ip;
    let mut sp: *mut StackElement = thread.interpreter().sp;
    let mut op: u8;

    macro_rules! cache_sp {
        () => {
            sp = thread.interpreter().sp; 
        };
    }

    macro_rules! cahce_registers {
        () => {
            ip = thread.interpreter().ip;
            cache_sp!();
        };
    }

    macro_rules! fp_slot {
        ($i: expr) => {
            frame_slot(thread.interpreter().vp, $i as isize)
        }
    }

    macro_rules! fp_ref {
        ($i: expr) => {
            frame_local(thread.interpreter().fp, $i as isize)
        }
    }

    macro_rules! fp_set {
        ($i: expr, $val: expr) => {
            *frame_local(thread.interpreter().fp, $i as isize) = $val
        };
    }

    macro_rules! sp_ref {
        ($i: expr) => {
            sp.offset($i as _).read().as_value 
        };
    }

    macro_rules! sp_set {
        ($i: expr, $val: expr) => {
            sp.offset($i as _).write(StackElement { as_value: $val })
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
                thread.interpreter().sp = sp;
                thread.interpreter().ip = ip;
                thread.safepoint();
                sp = thread.interpreter().sp;
                ip = thread.interpreter().ip;
            }

            OP_LOOP_HINT => {
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
                let new_fp = frame_slot(old_fp, call.proc().value() as isize - 1);
                set_frame_dynamic_link(new_fp, old_fp);
                set_frame_virtual_return_address(new_fp, ip);
                set_frame_machine_return_address(new_fp, null());
                thread.interpreter().fp = new_fp;

                reset_frame!(call.nlocals().value() as isize);
                ip = get_callee_vcode(thread);
                cache_sp!();
            }

            OP_TAIL_CALL => {
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
                    todo!("no values error"); // FIXME: Throw error
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
                    if unlikely(frame_locals_count!() < proc.value() as isize + nvalues.value() as isize) {
                        sync_sp!();
                        sync_ip!();
                        todo!("no values error"); // FIXME: Throw error
                    }
                } else {
                    if unlikely(frame_locals_count!() != proc.value() as isize + nvalues.value() as isize) {
                        sync_sp!();
                        sync_ip!();
                        todo!("no values error"); // FIXME: Throw error
                    }
                }
            }

            OP_ASSERT_NARGS_EE => {
                let assert_nargs_ee = OpAssertNargsEe::read(ip);
                ip = ip.add(size_of::<OpAssertNargsEe>());

                if unlikely(frame_locals_count!() != assert_nargs_ee.n().value() as isize) {
                    sync_sp!();
                    sync_ip!();
                    todo!("no values error"); // FIXME: Throw error
                }
            }

            OP_ASSERT_NARGS_GE => {
                let assert_nargs_ge = OpAssertNargsGe::read(ip);
                ip = ip.add(size_of::<OpAssertNargsGe>());

                if unlikely(frame_locals_count!() < assert_nargs_ge.n().value() as isize) {
                    sync_sp!();
                    sync_ip!();
                    todo!("no values error"); // FIXME: Throw error
                }
            }

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

            OP_MOV =>{ 
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
                    fp_set!(to+n, *fp_ref!(from + n));
                    n += 1;
                }

                reset_frame!(to+n);
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

            

            _ => ()

        }
    }
}