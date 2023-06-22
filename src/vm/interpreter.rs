use rsgc::prelude::Handle;
use std::intrinsics::unlikely;
use std::mem::{offset_of, size_of, transmute};
use std::ptr::{null, null_mut};

use super::callframe::CallFrame;
use super::VM;
use crate::object::{
    check_arity, wrong_arity, ClosedNativeProcedure, CodeBlock, NativeProcedure, Procedure, Type,
};
use crate::op::Opcode;
use crate::string::make_string;
use crate::value::Value;
use crate::vector::make_vector;

/// Virtual machine interpreter loop.
///
/// Conventions:
/// - `cfr` is the current call frame, assume `vm.sp` is the current call frame.
/// - `pc` is the current program counter, `pc` argument is used to initialize it based on code-block.
/// - `vm` is the virtual machine.
/// - `sp` is the current stack pointer.
/// - stack grows from high address to low address.
pub unsafe fn vm_eval(vm: &mut VM, cfr: *mut CallFrame, entry_pc: usize) -> Result<Value, Value> {
    // actual stack pointer.
    let mut sp = cfr.sub(1).cast::<Value>();
    let mut cfr = cfr.cast::<CallFrame>();
    let mut pc;

    macro_rules! pop {
        () => {{
            let val = sp.read();
            sp = sp.add(1);
            val
        }};

        (times $n: expr) => {{
            for _ in $n {
                pop!();
            }
        }};
    }

    macro_rules! push {
        ($val: expr) => {{
            sp = sp.sub(1);
            sp.write($val);
        }};

        (cfr $val: expr) => {
            sp = sp.cast::<CallFrame>().sub(1).cast();
            sp.cast::<CallFrame>().write($val);
        };
    }

    macro_rules! leave_frame {
        ($val: expr) => {
            if (*cfr).caller.is_null() {
                return Ok($val);
            } else {
                pc = (*cfr).return_pc;
                sp = cfr.add(1).cast::<Value>().add((*cfr).argc.get_int32() as _);
                cfr = (*cfr).caller;
                push!($val);
            }
        };

        (=> $val: expr) => {
            if (*cfr).caller.is_null() {
                return $val;
            } else {
                pc = (*cfr).return_pc;
                sp = cfr.add(1).cast::<Value>().add((*cfr).argc.get_int32() as _);
                cfr = (*cfr).caller;

                push!($val?);
            }
        };
    }

    macro_rules! code_block {
        () => {
            transmute::<_, Handle<CodeBlock>>((*cfr).code_block)
        };
    }

    'eval: loop {
        let callee = (*cfr).callee;

        if callee.is_xtype(Type::Procedure) {
            let proc: Handle<Procedure> = transmute(callee);
            (*cfr).code_block = Value::encode_object_value(proc.code);
            pc = proc.code.start_ip().add(entry_pc);
        } else if callee.is_native_procedure() {
            // safety of transmute: NativeProcedure and ClosedProcedure have the same internal layout
            let proc: Handle<NativeProcedure> = transmute(callee);

            if unlikely(!check_arity(
                proc.mina,
                proc.maxa,
                (*cfr).argc.get_int32() as _,
            )) {
                return Err(wrong_arity(
                    proc.name,
                    (*cfr).argc.get_int32() as _,
                    proc.mina,
                    proc.maxa,
                ));
            }

            let result = if !callee.is_closed_native_procedure() {
                (proc.callback)(&mut *cfr)
            } else {
                let mut closed_proc: Handle<ClosedNativeProcedure> = transmute(callee);
                (closed_proc.callback)(
                    &mut *cfr,
                    std::slice::from_raw_parts_mut(
                        closed_proc.captures.as_mut_ptr(),
                        closed_proc.env_size as _,
                    ),
                )
            };

            if result.is_ok() {
                leave_frame!(=> Ok(result.value()))
            } else if result.is_err() {
                leave_frame!(=> Err(result.value()))
            } else {
                pc = null();
                // TODO: tail-call
                // 1) rator and rands are in special VM vector
                // 2) build a new call-frame
                // 3) again enter the loop
                //
                todo!()
            }
        } else {
            fn not_a_function(vm: &mut VM, callee: Value) -> Result<Value, Value> {
                Err(Value::encode_object_value(make_string(
                    vm.thread,
                    &format!("'{:?}' is not a function", callee),
                )))
            }

            return not_a_function(vm, callee);
        }

        // `pc` is initialized here
        debug_assert!(!pc.is_null(), "pc should be initialized at vm_eval entry");

        loop {
            macro_rules! read1 {
                () => {{
                    let val = pc.read();
                    pc = pc.add(1);
                    val
                }};
            }

            macro_rules! read2 {
                () => {{
                    let val = pc.cast::<u16>().read();
                    pc = pc.add(2);
                    val
                }};
            }

            macro_rules! read4 {
                () => {{
                    let val = pc.cast::<u32>().read();
                    pc = pc.add(4);
                    val
                }};
            }

            macro_rules! read8 {
                () => {{
                    let val = pc.cast::<u64>().read();
                    pc = pc.add(8);
                    val
                }};
            }

            macro_rules! readv {
                () => {{
                    let val: Value = pc.cast::<Value>().read();
                    pc = pc.add(size_of::<Value>());
                    val
                }};
            }

            macro_rules! readt {
                ($t: ty) => {{
                    let val: $t = pc.cast::<$t>().read();
                    pc = pc.add(size_of::<$t>());
                    val
                }};
            }

            let op = readt!(Opcode);

            match op {
                Opcode::NoOp => {}
                Opcode::Pop => {
                    pop!();
                }

                Opcode::Dup => {
                    let val = pop!();
                    push!(val);
                    push!(val);
                }

                Opcode::Swap => {
                    let val1 = pop!();
                    let val2 = pop!();
                    push!(val1);
                    push!(val2);
                }

                Opcode::LdArg => {
                    let n = read2!();
                    let arg = (*cfr).args.as_ptr().add(n as usize).read();

                    push!(arg);
                }

                Opcode::Alloc => {
                    let n = read2!();

                    for _ in 0..n {
                        push!(Value::encode_undefined_value());
                    }
                }

                Opcode::AllocBelow => {
                    let top = pop!();
                    let n = read2!();

                    for _ in 0..n {
                        push!(Value::encode_undefined_value());
                    }

                    push!(top);
                }

                Opcode::PushUndef => {
                    push!(Value::encode_undefined_value());
                }

                Opcode::PushNull => {
                    push!(Value::encode_null_value());
                }

                Opcode::PushTrue => {
                    push!(Value::encode_bool_value(true));
                }

                Opcode::PushFalse => {
                    push!(Value::encode_bool_value(false));
                }

                Opcode::PushInt32 => {
                    let val = read4!();
                    push!(Value::encode_int32(val as i32));
                }

                Opcode::PushDouble => {
                    let val = read8!();
                    push!(Value::encode_untrusted_f64_value(val as f64));
                }

                Opcode::PushConstant => {
                    let ix = read2!();

                    let val = code_block!().literals.vector_ref(ix as _);
                    push!(val);
                }

                Opcode::Pack => {
                    let n = read2!();

                    let mut list = make_vector(vm.thread, n as _);
                    list.object.typ = Type::Values;

                    for i in (0..n).rev() {
                        vm.thread.write_barrier(list);
                        list[i as usize] = pop!();
                    }

                    push!(Value::encode_object_value(list));
                }

                Opcode::Call => {
                    let argc = read2!();
                    let callee = pop!();
                    push!(cfr CallFrame {
                        return_pc: pc,
                        caller: cfr,
                        code_block: Value::encode_undefined_value(),
                        argc: Value::encode_int32(argc as _),
                        callee,
                        args: []
                    });
                    cfr = sp.cast();
                    continue 'eval;
                }

                Opcode::TailCall => {
                    let argc = read2!();
                    let callee = pop!();
                    let caller = (*cfr).caller;
                    let return_pc = (*cfr).return_pc;

                    // start of arguments pushed by caller for current frame
                    let arg_start = (*cfr)
                        .args
                        .as_mut_ptr()
                        .add((*cfr).argc.get_int32() as usize);

                    // start of arguments pushed by current frame for the call
                    let mut tail_arg = sp.add(argc as usize);

                    let mut cursor = arg_start;
                   
                    for _ in 0..argc {
                        tail_arg = tail_arg.sub(1);
                        cursor = cursor.sub(1);

                        cursor.write(tail_arg.read());
                    }

                    sp = cursor;
                    
                    push!(cfr CallFrame {
                        return_pc,
                        caller,
                        code_block: Value::encode_undefined_value(),
                        argc: Value::encode_int32(argc as _),
                        callee,
                        args: []
                    });

                    cfr = sp.cast();

                    continue 'eval;
                }

                Opcode::Return => {
                    let val = pop!();

                    leave_frame!(val);
                }

                Opcode::Add => {
                    let val2 = pop!();
                    let val1 = pop!();

                    let result = if val1.is_int32() && val2.is_int32() {
                        Value::encode_int32(val1.get_int32().wrapping_add(val2.get_int32()))
                    } else {
                        Value::encode_undefined_value()
                    };

                    push!(result);
                }

                _ => todo!(),
            }
        }
    }
}

pub unsafe fn _vm_entry_trampoline(
    vm: &mut VM,
    callee: Value,
    args: &[Value],
) -> Result<Value, Value> {
    let mut sp = vm.sp;
    let start = vm.sp;
    for &arg in args {
        sp = sp.sub(1);
        sp.write(arg);
    }

    sp = sp.sub(5);
    let cfr = sp.cast::<CallFrame>();
    (*cfr).caller = null_mut();
    (*cfr).return_pc = null();
    (*cfr).callee = callee;
    (*cfr).argc = Value::encode_int32(args.len() as _);

    println!("trampoline to {:?}", callee);
    vm_eval(vm, cfr, 0)
}
