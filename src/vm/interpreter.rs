#![allow(unused_variables, unused_assignments, unused_labels)]
use rsgc::prelude::Handle;
use rsgc::system::arraylist::ArrayList;
use std::cmp::Ordering;
use std::intrinsics::{likely, unlikely};
use std::mem::{size_of, transmute};
use std::panic::AssertUnwindSafe;
use std::ptr::{null, null_mut};

use super::callframe::CallFrame;
use super::{scm_current_module, scm_vm, VM};
use crate::compaux::{
    scm_identifier_global_ref, scm_identifier_global_set, scm_outermost_identifier,
};
use crate::op::Opcode;
use crate::runtime::error::{wrong_count, wrong_contract};
use crate::runtime::fun::{make_closed_procedure, get_proc_name};
use crate::runtime::list::{scm_cons, scm_is_list};
use crate::runtime::module::{scm_make_binding, SCM_BINDING_CONST};
use crate::runtime::arith::*;
use crate::runtime::object::{
    check_arity, make_box, wrong_arity, ClosedNativeProcedure, CodeBlock, Module, NativeProcedure,
    Procedure, Type, MAX_ARITY,
};
use crate::runtime::string::make_string;
use crate::runtime::value::Value;
use crate::runtime::vector::{make_vector, make_vector_from_slice};
use crate::vm::stacktrace::StackTrace;

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
    let mut sp = cfr.cast::<Value>();
    let mut cfr = cfr.cast::<CallFrame>();
    let mut pc;

    macro_rules! pop {
        () => {{
            debug_assert!(sp < cfr.cast::<Value>());
            let val = sp.read();
            sp.write(Value::encode_empty_value());
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
            let val = $val;
            sp = sp.sub(1);
            debug_assert!(sp < cfr.cast::<Value>());
            sp.write(val);
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
        vm.thread.safepoint();
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
            vm.sp = sp;
            vm.top_call_frame = cfr;
            let result = if !callee.is_closed_native_procedure() {
                (proc.callback)(&mut *cfr)
            } else {
                let closed_proc: Handle<ClosedNativeProcedure> = transmute(callee);
                (closed_proc.callback)(&mut *cfr)
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
                let caller = (*cfr).caller;
                let return_pc = (*cfr).return_pc;

                // start of arguments pushed by caller for current frame
                let arg_start = (*cfr)
                    .args
                    .as_mut_ptr()
                    .add((*cfr).argc.get_int32() as usize);

                let mut cursor = arg_start;

                // copy arguments to the new frame
                for i in (0..vm.tail_rands.len()).rev() {
                    cursor = cursor.sub(1);
                    cursor.write(vm.tail_rands[i]);
                }

                sp = cursor;

                push!(cfr CallFrame {
                    caller,
                    return_pc,
                    callee: vm.tail_rator,
                    argc: Value::encode_int32(vm.tail_rands.len() as _),
                    code_block: Value::encode_undefined_value(),
                    args: []
                });
                vm.tail_rands.clear();
                vm.tail_rator = Value::encode_undefined_value();

                cfr = sp.cast::<CallFrame>();

                continue 'eval;
            }
        } else {
            #[cold]
            fn not_a_function(vm: &mut VM, callee: Value) -> Result<Value, Value> {
                /*Err(Value::encode_object_value(make_string(
                    vm.thread,
                    &format!("'{:?}' is not a function", callee),
                )))*/

                eprintln!("tried to invoke not a function: {:?}", callee);

                let st = StackTrace::new(vm);
                for frame in st {
                    let callee = frame.callee();
                    let ip = frame.return_pc();
                    let code_block = frame.code_block();

                    let name = get_proc_name(callee);

                    if let Some(name) = name {
                        eprint!("  at {}", name)
                    } else {
                        eprint!("  at <unknown>")
                    }

                    if !ip.is_null() {
                        eprint!(":{:p} {}", ip, unsafe { std::mem::transmute::<_, Opcode>(ip.read()) });
                    } else {
                        eprint!("<entrypoint>");
                    }

                    eprintln!();

                }

                Err(Value::encode_int32(0))
            }

            return not_a_function(vm, callee);
        }

        // `pc` is initialized here
        debug_assert!(!pc.is_null(), "pc should be initialized at vm_eval entry");

        'interp: loop {
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
                Opcode::Enter
                | Opcode::EnterCompiling
                | Opcode::EnterBlacklisted
                | Opcode::EnterJit => {
                    vm.mutator().safepoint();
                    // TODO: Check for JIT trampoline
                }
                Opcode::Pop => {
                    pop!();
                }
                Opcode::Popn => {
                    let n = read2!();
                    sp = sp.add(n as usize);
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
                    let prev = sp;
                   
                    push!(arg);
                }

                Opcode::Alloc => {
                    let n = read2!();

                    for x in 0..n {
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

                Opcode::PushConstant | Opcode::PushProcedure => {
                    let ix = read2!();

                    let val = code_block!().literals.vector_ref(ix as _);
                    push!(val);
                }

                Opcode::Add => {
                    let y = pop!();
                    let x = pop!();

                    if x.is_int32() && y.is_int32() {
                        if let Some(res) = x.get_int32().checked_add(y.get_int32()) {
                            push!(Value::encode_int32(res));
                            continue 'interp;
                        } 
                    }

                    if x.is_double() && y.is_double() {
                        push!(Value::encode_untrusted_f64_value(x.get_double() + y.get_double()));
                        continue 'interp;
                    }

                    if unlikely(!scm_is_number(x)) {
                        return wrong_contract("+", "number?", 0, 2, &[x, y]);
                    }

                    if unlikely(!scm_is_number(y)) {
                        return wrong_contract("+", "number?", 1, 2, &[x, y]);
                    }
                    
                    let res = arith_add(vm, x, y).unwrap();

                    push!(res);
                }

                Opcode::Sub => {
                    let y = pop!();
                    let x = pop!();

                    if x.is_int32() && y.is_int32() {
                        if let Some(res) = x.get_int32().checked_sub(y.get_int32()) {
                            push!(Value::encode_int32(res));
                            continue 'interp;
                        } 
                    }

                    if x.is_double() && y.is_double() {
                        push!(Value::encode_untrusted_f64_value(x.get_double() - y.get_double()));
                        continue 'interp;
                    }

                    if unlikely(!scm_is_number(x)) {
                        return wrong_contract("-", "number?", 0, 2, &[x, y]);
                    }

                    if unlikely(!scm_is_number(y)) {
                        return wrong_contract("-", "number?", 1, 2, &[x, y]);
                    }

                    let res = arith_sub(vm, x, y).unwrap();

                    push!(res);
                }

                Opcode::Div => {
                    let y = pop!();
                    let x = pop!();

                    if x.is_int32() && y.is_int32() {
                        if let Some(res) = x.get_int32().checked_div(y.get_int32()) {
                            push!(Value::encode_int32(res));
                            continue 'interp;
                        } 
                    }

                    if x.is_double() && y.is_double() {
                        push!(Value::encode_untrusted_f64_value(x.get_double() / y.get_double()));
                        continue 'interp;
                    }

                    if unlikely(!scm_is_number(x)) {
                        return wrong_contract("/", "number?", 0, 2, &[x, y]);
                    }

                    if unlikely(!scm_is_number(y)) {
                        return wrong_contract("/", "number?", 1, 2, &[x, y]);
                    }
                    let res = arith_div(vm, x, y).unwrap();

                    push!(res);
                }

                Opcode::Mul => {
                    let y = pop!();
                    let x = pop!();

                    if x.is_int32() && y.is_int32() {
                        if let Some(res) = x.get_int32().checked_mul(y.get_int32()) {
                            push!(Value::encode_int32(res));
                            continue 'interp;
                        } 
                    }

                    if x.is_double() && y.is_double() {
                        push!(Value::encode_untrusted_f64_value(x.get_double() * y.get_double()));
                        continue 'interp;
                    }

                    if unlikely(!scm_is_number(x)) {
                        return wrong_contract("*", "number?", 0, 2, &[x, y]);
                    }

                    if unlikely(!scm_is_number(y)) {
                        return wrong_contract("*", "number?", 1, 2, &[x, y]);
                    }

                    let res = arith_mul(vm, x, y).unwrap();

                    push!(res);
                }

                Opcode::NumberEqual => {
                    let y = pop!();
                    let x = pop!();

                    if x.is_int32() && y.is_int32() {
                        push!(Value::encode_bool_value(x.get_int32() == y.get_int32()));
                        continue 'interp;
                    }

                    if x.is_double() && y.is_double() {
                        push!(Value::encode_bool_value(x.get_double() == y.get_double()));
                        continue 'interp;
                    }

                    if unlikely(!scm_is_real(x)) {
                        return wrong_contract("=", "number?", 0, 2, &[x, y]);
                    }

                    if unlikely(!scm_is_real(y)) {
                        return wrong_contract("=", "number?", 1, 2, &[x, y]);
                    }

                    let res = scm_is_number_equal(x, y).unwrap();

                    push!(res.into());
                }

                Opcode::Less => {
                    let y = pop!();
                    let x = pop!();
                    
                    if x.is_int32() && y.is_int32() {
                        push!(Value::encode_bool_value(x.get_int32() < y.get_int32()));
                        continue 'interp;
                    }

                    if x.is_double() && y.is_double() {
                        push!(Value::encode_bool_value(x.get_double() < y.get_double()));
                        continue 'interp;
                    }

                    if unlikely(!scm_is_number(x)) {
                        return wrong_contract("<", "number?", 0, 2, &[x, y]);
                    }

                    if unlikely(!scm_is_number(y)) {
                        return wrong_contract("<", "number?", 1, 2, &[x, y]);
                    }

                    let cmp = scm_n_compare(x, y).unwrap();

                    match cmp {
                        Ordering::Less => push!(Value::encode_bool_value(true)),
                        _ => push!(Value::encode_bool_value(false)),
                    }
                }

                Opcode::LessEqual => {
                    let y = pop!();
                    let x = pop!();

                    if x.is_int32() && y.is_int32() {
                        push!(Value::encode_bool_value(x.get_int32() <= y.get_int32()));
                        continue 'interp;
                    }

                    if x.is_double() && y.is_double() {
                        push!(Value::encode_bool_value(x.get_double() <= y.get_double()));
                        continue 'interp;
                    }

                    if unlikely(!scm_is_number(x)) {
                        return wrong_contract("<=", "number?", 0, 2, &[x, y]);
                    }

                    if unlikely(!scm_is_number(y)) {
                        return wrong_contract("<=", "number?", 1, 2, &[x, y]);
                    }

                    let cmp = scm_n_compare(x, y).unwrap();

                    match cmp {
                        Ordering::Less | Ordering::Equal => push!(Value::encode_bool_value(true)),
                        _ => push!(Value::encode_bool_value(false)),
                    }
                }

                Opcode::Greater => {
                    let y = pop!();
                    let x = pop!();

                    if x.is_int32() && y.is_int32() {
                        push!(Value::encode_bool_value(x.get_int32() > y.get_int32()));
                        continue 'interp;
                    }

                    if x.is_double() && y.is_double() {
                        push!(Value::encode_bool_value(x.get_double() > y.get_double()));
                        continue 'interp;
                    }

                    if unlikely(!scm_is_number(x)) {
                        return wrong_contract(">", "number?", 0, 2, &[x, y]);
                    }

                    if unlikely(!scm_is_number(y)) {
                        return wrong_contract(">", "number?", 1, 2, &[x, y]);
                    }

                    let cmp = scm_n_compare(x, y).unwrap();

                    match cmp {
                        Ordering::Greater => push!(Value::encode_bool_value(true)),
                        _ => push!(Value::encode_bool_value(false)),
                    }
                }

                Opcode::GreaterEqual => {
                    let y = pop!();
                    let x = pop!();

                    if x.is_int32() && y.is_int32() {
                        push!(Value::encode_bool_value(x.get_int32() >= y.get_int32()));
                        continue 'interp;
                    }

                    if x.is_double() && y.is_double() {
                        push!(Value::encode_bool_value(x.get_double() >= y.get_double()));
                        continue 'interp;
                    }

                    if unlikely(!scm_is_number(x)) {
                        return wrong_contract(">=", "number?", 0, 2, &[x, y]);
                    }

                    if unlikely(!scm_is_number(y)) {
                        return wrong_contract(">=", "number?", 1, 2, &[x, y]);
                    }

                    let cmp = scm_n_compare(x, y).unwrap();

                    match cmp {
                        Ordering::Greater | Ordering::Equal => push!(Value::encode_bool_value(true)),
                        _ => push!(Value::encode_bool_value(false)),
                    }
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
                    if callee.is_vm_procedure() {
                        (*cfr).code_block = callee.procedure().code.into();
                        pc = callee.procedure().code.start_ip();
              
                        continue 'interp;
                    }
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

                    if callee.is_vm_procedure() {
                        (*cfr).code_block = callee.procedure().code.into();
                        pc = callee.procedure().code.start_ip();
                        continue 'interp;
                    }

                    continue 'eval;
                }

                Opcode::Return => {
                    let val = pop!();

                    leave_frame!(val);
                }

                Opcode::StackGet => {
                    let off = read2!();

                    let val = cfr.cast::<Value>().sub(off as usize + 1).read();
              
                    push!(val);
                }

                Opcode::StackSet => {
                    let off = read2!();
                    let val = pop!();
                    let slot = cfr.cast::<Value>().sub(off as usize + 1);

                    debug_assert!(slot < cfr.cast::<Value>());
                    slot.write(val);
                }

                Opcode::StackBox => {
                    let off = read2!();
                    let value = pop!();

                    let val = make_box(vm.thread, value);
                    cfr.cast::<Value>().sub(off as usize + 1).write(val);
                }

                Opcode::Box => {
                    let value = pop!();

                    let val = make_box(vm.thread, value);
                    push!(val);
                }

                Opcode::BoxRef => {
                    let val = pop!();
                    let val = val.box_ref();
                    push!(val);
                }

                Opcode::BoxSet => {
                    let val = pop!();

                    let value = pop!();
                    vm.thread.write_barrier(val.get_object());
                    val.box_set(value);
                }

                Opcode::MakeClosure => {
                    let ncaptures = read2!();

                    let code_block = pop!();

                    let code_block: Handle<CodeBlock> = transmute(code_block);

                    let captures = std::slice::from_raw_parts(sp, ncaptures as usize);

                    let mut closure = make_closed_procedure(vm.thread, code_block, ncaptures as _);

                    closure
                        .captures
                        .as_mut_ptr()
                        .copy_from_nonoverlapping(captures.as_ptr(), ncaptures as _);
                    for _ in 0..ncaptures {
                        sp = sp.add(1);
                    }
                    push!(Value::encode_object_value(closure));
                }

                Opcode::Define => {
                    let value = pop!();
                    let ix = read2!();
                    let constant = read1!();

                    let id = code_block!().literals.vector_ref(ix as _).identifier();
                    let id = scm_outermost_identifier(id);
                    let module = id.module.module();
                    let name = id.name.symbol();

                    scm_make_binding(
                        module,
                        name,
                        value,
                        if constant != 0 { SCM_BINDING_CONST } else { 0 },
                    )?;

                    push!(Value::encode_object_value(name));
                }

                Opcode::GlobalRef => {
                    let ix = read2!() as usize;

                    let constant = code_block!().literals.vector_ref(ix as _);

                    if likely(constant.is_xtype(Type::GLOC)) {
                        push!(constant.gloc().value);
                    } else {
                        let (value, gloc) = scm_identifier_global_ref(constant.identifier())?;

                        push!(value);
                        vm.thread.write_barrier(code_block!().literals.vector());
                        code_block!()
                            .literals
                            .vector_set(ix as _, Value::encode_object_value(gloc));
                    }
                }

                Opcode::ClosureRef => {
                    let ix = read2!();

                    let callee = (*cfr).callee.procedure();

                    push!(callee.captures.as_ptr().add(ix as _).read());
                }

                Opcode::SetArg => {
                    let val = pop!();
                    let ix = read2!();

                    let arg = (*cfr).args.as_mut_ptr().add(ix as _);
                    arg.write(val);
                }

                Opcode::Reset => { /* no-op for now */ }

                Opcode::GlobalSet => {
                    let ix = read2!() as usize;

                    let constant = code_block!().literals.vector_ref(ix as _);
                    let value = pop!();
                    if likely(constant.is_xtype(Type::GLOC)) {
                        // set the value directly since GLOC is already resolved.
                        constant.gloc().value = value;
                    } else {
                        // try to resolve the GLOC first and then set the value.
                        // also caches the resolved GLOC in the literal vector.
                        let gloc = scm_identifier_global_set(constant.identifier(), value)?;
                        vm.thread.write_barrier(code_block!().literals.vector());
                        code_block!()
                            .literals
                            .vector_set(ix as _, Value::encode_object_value(gloc));
                    }
                }

                Opcode::Flatpack => {
                    let n = read2!();
                    let mut list = ArrayList::with_capacity(vm.thread, n as _);

                    for i in 0..n {
                        let e = pop!();

                        if e.is_values() {
                            for &e in e.values().iter() {
                                list.push(vm.thread, e);
                            }
                        } else {
                            list.push(vm.thread, e);
                        }
                    }

                    let mut x = make_vector_from_slice(vm.thread, &list);
                    x.object.typ = Type::Values;

                    push!(Value::encode_object_value(x));
                }

                Opcode::Pack => {
                    let n = read2!();

                    let mut list = make_vector(vm.thread, n as _);

                    for i in 0..n {
                        let e = pop!();
                        vm.thread.write_barrier(list);
                        list[i as usize] = e;
                    }

                    push!(Value::encode_object_value(list));
                }

                Opcode::Unpack => {
                    let _n = read2!();
                    let _ovf = read1!() != 0;
                    todo!()
                }

                Opcode::Apply => {
                    let rator = pop!();
                    let rands = pop!();
                    if unlikely(!scm_is_list(rands)) {
                        return Err(make_string(vm.thread, "apply: expected list").into());
                    }

                    let mut argc = 0;
                    scm_dolist!(val, rands, {
                        push!(val);
                        argc += 1;
                    });

                    push!(rator);

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

                Opcode::AssertArgCount => {
                    let argc = read2!();
                    if unlikely(argc != (*cfr).argc.get_int32() as u16) {
                        return wrong_count(
                            code_block!().name.strsym(),
                            code_block!().mina as _, 
                            if code_block!().maxa >= MAX_ARITY {
                                -1
                            } else {
                                code_block!().maxa as i32
                            },
                            (*cfr).argc.get_int32() as _,
                            (*cfr).arguments()
                        );
                    }
                }

                Opcode::AssertMinArgCount => {
                    let argc = read2!();
                    if unlikely(argc > (*cfr).argc.get_int32() as u16) {
                        return wrong_count(
                            code_block!().name.strsym(),
                            code_block!().mina as _, 
                            if code_block!().maxa >= MAX_ARITY {
                                -1
                            } else {
                                code_block!().maxa as i32
                            },
                            (*cfr).argc.get_int32() as _,
                            (*cfr).arguments()
                        );
                    }
                }

                Opcode::NoMatchingArgCount => {
                    return Err(make_string(vm.thread, "wrong number of arguments").into());
                }

                Opcode::CollectRest => {
                    let n = read2!();
                    let rest = (*cfr).args.as_mut_ptr().add(n as _);
                    let mut list = Value::encode_null_value();

                    for i in (0..(*cfr).argc.get_int32() as usize - n as usize).rev() {
                        let e = rest.add(i as _).read();
                        list = scm_cons(vm.thread, e, list);
                    }

                    push!(list);
                }

                Opcode::ClosureSet => {
                    let ix = read2!();
                    let val = pop!();

                    let mut callee = (*cfr).callee.procedure();
                    vm.thread.write_barrier(callee);
                    callee.captures.as_mut_ptr().add(ix as _).write(val);
                }

                Opcode::Branch => {
                    let offset = read4!();
                    vm.mutator().safepoint();
                    pc = pc.offset(offset as _);
                }

                Opcode::BranchIf => {
                    let offset = read4!();
                    let val = pop!();

                    if val.to_bool() {
                        pc = pc.offset(offset as _);
                    }
                }

                Opcode::BranchIfNot => {
                    let offset = read4!();
                    let val = pop!();

                    if !val.to_bool() {
                        pc = pc.offset(offset as _);
                    }
                }

                Opcode::BranchIfArgMismatch | Opcode::BranchIfMinArgMismatch => {
                    todo!()
                }

                Opcode::KeepBranchIfNot => {
                    let val = sp.read();

                    if !val.to_bool() {
                        let offset = read4!();
                        pc = pc.offset(offset as _);
                    }
                }

                Opcode::IsNull => {
                    push!(Value::encode_bool_value(pop!().is_null()));
                }

                Opcode::IsNumber => {
                    push!(Value::encode_bool_value(pop!().is_number()));
                }

                Opcode::IsComplex => {
                    push!(Value::encode_bool_value(pop!().is_complex()));
                }

                Opcode::IsReal => {
                    push!(Value::encode_bool_value(pop!().is_real()));
                }

                Opcode::IsRational => {
                    push!(Value::encode_bool_value(pop!().is_rational()));
                }

                Opcode::IsInteger => {
                    push!(Value::encode_bool_value(scm_is_integer(pop!())));
                }

                Opcode::IsExactInteger => {
                    push!(Value::encode_bool_value(pop!().is_exact_integer()));
                }

                Opcode::IsExactNonnegativeInteger => {
                    let n = pop!();

                    if n.is_int32() {
                        push!(Value::encode_bool_value(n.get_int32() >= 0));
                    } else if n.is_bignum() {
                        push!(Value::encode_bool_value(!n.bignum().is_negative()))
                    } else {
                        push!(Value::encode_bool_value(false))
                    }
                }

                Opcode::IsExactPositiveInteger => {
                    let n = pop!();

                    if n.is_int32() {
                        push!(Value::encode_bool_value(n.get_int32() > 0));
                    } else if n.is_bignum() {
                        push!(Value::encode_bool_value(
                            !n.bignum().is_negative() && !n.bignum().is_zero()
                        ))
                    } else {
                        push!(Value::encode_bool_value(false))
                    }
                }

                Opcode::IsFixnum => {
                    push!(Value::encode_bool_value(pop!().is_int32()));
                }

                Opcode::IsInexactReal => {
                    push!(Value::encode_bool_value(pop!().is_double()));
                }

                Opcode::IsFlonum => {
                    push!(Value::encode_bool_value(pop!().is_double()));
                }

                Opcode::IsExact => {
                    let val = pop!();

                    if let Some(x) = scm_is_exact(val) {
                        push!(Value::encode_bool_value(x));
                    } else {
                        return Err(make_string(vm.thread, "exact?: expected number").into());
                    }
                }

                Opcode::Car => {
                    let val = pop!();

                    if unlikely(!val.is_pair()) {
                        return wrong_contract("car", "pair?", 0, 1, &[val]);
                    }

                    push!(val.pair().car);
                }

                Opcode::Cdr => {
                    let val = pop!();

                    if unlikely(!val.is_pair()) {
                        return wrong_contract("cdr", "pair?", 0, 1, &[val]);
                    }

                    push!(val.pair().cdr);
                }

                Opcode::SetCar => {
                    let val = pop!();
                    let cell = pop!();
                    if unlikely(!cell.is_pair()) {
                        return wrong_contract("set-car!", "pair?", 0,2, &[cell, val]);
                    }

                    vm.thread.write_barrier(cell.pair());
                    cell.pair().car = val;
                }

                Opcode::SetCdr => {
                    let val = pop!();
                    let cell = pop!();
                    if unlikely(!cell.is_pair()) {
                        return wrong_contract("set-cdr!", "pair?", 0,2, &[cell, val]);
                    }

                    vm.thread.write_barrier(cell.pair());
                    cell.pair().cdr = val;
                }

                Opcode::Cons => {
                    let cdr = pop!();
                    let car = pop!();

                    push!(scm_cons(vm.thread, car, cdr));
                }

                Opcode::List => {
                    let mut list = Value::encode_null_value();

                    for _ in 0..read2!() {
                        list = scm_cons(vm.thread, pop!(), list);
                    }

                    push!(list);
                }

                Opcode::IsPair => {
                    let val = pop!();

                    push!(Value::encode_bool_value(val.is_pair()));
                }

                Opcode::IsList => {
                    let val = pop!();
                    push!(Value::encode_bool_value(scm_is_list(val)));
                }

                Opcode::IsVector => {
                    let val = pop!();
                    push!(Value::encode_bool_value(val.is_vector()));
                }

                Opcode::IsUndef => {
                    let val = pop!();
                    push!(Value::encode_bool_value(val.is_undefined()));
                }

                /*Opcode::Tuple => {
                    let n = read2!();

                    let mut tuple = scm_make_tuple(vm.thread, n as _);

                    for i in (0..n).rev() {
                        vm.thread.write_barrier(tuple);
                        tuple[i as usize] = pop!();
                    }

                    push!(tuple.into());
                }

                Opcode::TupleRefI => {
                    let n = read2!();
                    let tuple = pop!();

                    if unlikely(!tuple.is_tuple()) {
                        return Err();
                    }

                    if unlikely((n as usize) >= tuple.tuple().len()) {
                        return Err(raise_out_of_bounds_error(
                            "tuple-ref",
                            n as _,
                            tuple.tuple().len() as _,
                        ));
                    }

                    push!(tuple.tuple()[n as usize]);
                }

                Opcode::TupleSetI => {
                    let n = read2!();
                    let tuple = pop!();

                    if unlikely(!tuple.is_tuple()) {
                        return Err(raise_argument_error("tuple-set!", "tuple?", tuple));
                    }

                    if unlikely((n as usize) >= tuple.tuple().len()) {
                        return Err(raise_out_of_bounds_error(
                            "tuple-set!",
                            n as _,
                            tuple.tuple().len() as _,
                        ));
                    }

                    vm.thread.write_barrier(tuple.tuple());
                    tuple.tuple()[n as usize] = pop!();
                }

                Opcode::TupleRef => {
                    let tuple = pop!();
                    let n = pop!();

                    if unlikely(!tuple.is_tuple()) {
                        return Err(raise_argument_error("tuple-ref", "tuple?", tuple));
                    }

                    if unlikely(!n.is_int32()) {
                        return Err(raise_argument_error("tuple-ref", "exact integer?", n));
                    }

                    let n = n.get_int32();

                    if unlikely((n as usize) >= tuple.tuple().len()) || n < 0 {
                        return Err(raise_out_of_bounds_error(
                            "tuple-ref",
                            n as _,
                            tuple.tuple().len() as _,
                        ));
                    }

                    push!(tuple.tuple()[n as usize]);
                }

                Opcode::TupleSet => {
                    let tuple = pop!();
                    let n = pop!();

                    if unlikely(!tuple.is_tuple()) {
                        return Err(raise_argument_error("tuple-set!", "tuple?", tuple));
                    }

                    if unlikely(!n.is_int32()) {
                        return Err(raise_argument_error("tuple-set!", "exact integer?", n));
                    }

                    let n = n.get_int32();

                    if unlikely((n as usize) >= tuple.tuple().len()) || n < 0 {
                        return Err(raise_out_of_bounds_error(
                            "tuple-set!",
                            n as _,
                            tuple.tuple().len() as _,
                        ));
                    }

                    vm.thread.write_barrier(tuple.tuple());
                    tuple.tuple()[n as usize] = pop!();
                }*/

                /*Opcode::IsProperty => {
                    let v = pop!();
                    let c = read2!();

                    let prop = code_block!().literals.vector_ref(c as _);

                    let stype = if v.is_structure() {
                        v.structure().type_
                    } else if v.is_struct_type() {
                        v.structure_type()
                    } else {
                        push!(Value::encode_bool_value(false));
                        continue;
                    };

                    for i in 0..stype.props.len() {
                        if prop == stype.props[i].0 {
                            push!(Value::encode_bool_value(true));
                            continue 'interp;
                        }
                    }

                    push!(Value::encode_bool_value(false));
                }*/
                _ => todo!(),
            }
        }
    }
}

pub unsafe fn _vm_entry_trampoline(
    vm: &mut VM,
    module: Option<Handle<Module>>,
    callee: Value,
    args: &[Value],
) -> Result<Value, Value> {
    let saved = vm.next_entry();
    let sp = vm.sp;
    let saved_module = vm.module;
    let mut t4 = vm.top_call_frame;
    vm.prev_top_call_frame = t4;
    t4 = vm.top_entry_frame;
    vm.prev_top_entry_frame = t4;

    vm.module = module;

    let cb = AssertUnwindSafe(|| {
        let mut sp = vm.sp;
        let start = vm.sp;
        for &arg in args.iter().rev() {
            sp = sp.sub(1);
            sp.write(arg);
        }

        sp = sp.sub(5);
        let cfr = sp.cast::<CallFrame>();
        (*cfr).caller = null_mut();
        (*cfr).return_pc = null();
        (*cfr).callee = callee;
        (*cfr).argc = Value::encode_int32(args.len() as _);
        vm.top_entry_frame = cfr;
        vm_eval(vm, cfr, 0)
    });

    let result = std::panic::catch_unwind(|| cb());

    vm.top_call_frame = vm.prev_top_call_frame;
    vm.top_entry_frame = vm.prev_top_entry_frame;
    vm.module = saved_module;
    vm.entry = saved;
    vm.sp = sp;
    match result {
        Ok(val) => val,
        Err(err) => std::panic::resume_unwind(err),
    }
}

pub fn apply(rator: Value, rands: &[Value]) -> Result<Value, Value> {
    let vm = scm_vm();

    unsafe { _vm_entry_trampoline(vm, scm_current_module(), rator, rands) }
}

pub fn apply_in(module: Handle<Module>, rator: Value, rands: &[Value]) -> Result<Value, Value> {
    let vm = scm_vm();

    unsafe { _vm_entry_trampoline(vm, Some(module), rator, rands) }
}
