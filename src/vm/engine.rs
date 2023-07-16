#![allow(unused_assignments)]
use std::intrinsics::{likely, unlikely};
use std::mem::size_of;
use std::panic::AssertUnwindSafe;
use std::ptr::{null, null_mut};

use rsgc::prelude::Handle;

use super::callframe::CallFrame;
use super::VM;
use crate::bytecode::{encode::*, opcodes::*};
use crate::compaux::{scm_identifier_global_ref, scm_outermost_identifier};
use crate::runtime::fun::make_closed_procedure;
use crate::runtime::list::scm_cons;
use crate::runtime::module::scm_make_binding;
use crate::runtime::{object::*, value::*};
pub unsafe fn vm_eval(vm: &mut VM) -> Result<Value, Value> {
    let mut fp = vm.sp.cast::<CallFrame>();
    let mut sp = vm.sp;
    println!("entry: fp={:p}, sp={:p}", fp, sp);
    let mut pc;
    'tail_call: loop {
        vm.sp = sp;
        vm.top_call_frame = fp;
        vm.mutator().safepoint();

        macro_rules! get {
            ($reg: expr) => {
                if !$reg.is_constant() {
                    fp.cast::<Value>().offset($reg.offset() as isize).read()
                } else {
                    (*fp)
                        .code_block()
                        .code_block()
                        .literals
                        .vector_ref($reg.to_constant_index() as _)
                }
            };
        }

        macro_rules! set {
            ($reg: expr, $val: expr) => {
                fp.cast::<Value>()
                    .offset($reg.offset() as isize)
                    .write($val);
            };
        }

        if likely((*fp).callee().is_vm_procedure()) {
            let cb = (*fp).callee().procedure().code;
            (*fp).code_block = cb.into();
            pc = cb.start_ip();
        } else if likely((*fp).callee().is_native_procedure()) {
            let proc = (*fp).callee().native_procedure();

            let result = (proc.callback)(&mut *fp);

            if likely(result.is_ok()) {
                let caller = (*fp).caller();
                let return_pc = (*fp).return_pc();
                if (*fp).return_pc.is_null() {
                    return Ok(result.value());
                } else {
                    fp = caller.cast::<CallFrame>();
                    pc = return_pc;

                    let call_inst = OpCall::read(pc);
                    pc = pc.add(size_of::<OpCall>());
                    let dst = call_inst.dst();
                    set!(dst, result.value());
                    println!("ret to {}, {}", dst, result.value());
                }
            } else if unlikely(result.is_err()) {
                todo!()
            } else {
                let callee = vm.tail_rator;
                if callee.is_native_procedure() {
                    println!(
                        "tail call to native procedure: {} from {}",
                        callee,
                        (*fp).callee
                    );
                }
                let caller = (*fp).caller;
                let return_pc = (*fp).return_pc;

                let mut to = (*fp).args.as_mut_ptr().add((*fp).argument_count());

                let argc = vm.tail_rands.len();

                for i in 0..argc {
                    to = to.sub(1);
                    to.write(vm.tail_rands[i]);
                }

                vm.tail_rands.clear();

                sp = to;
                sp = sp.cast::<CallFrame>().sub(1).cast();
                fp = sp.cast();

                (*fp).return_pc = return_pc;
                (*fp).caller = caller;
                (*fp).callee = callee;
                (*fp).argc = Value::encode_int32(argc as _);

                continue 'tail_call;
            }
        } else {
            unreachable!()
        }

        'interp: loop {
            let opcode = pc.read();
            pc = pc.add(1);

            match opcode {
                OP_ENTER => {
                    let t2 = (*fp).code_block.code_block();
                    let t2 = t2.num_vars;
                    let mut t2 = -(t2 as i16);
                    let t0 = Value::encode_undefined_value();
                    if t2 != 0 {
                        loop {
                            fp.cast::<Value>().offset(t2 as isize).write(t0);
                            t2 += 1;
                            if t2 >= 0 {
                                break;
                            }
                        }
                    }
                }

                OP_MOV => {
                    let op = OpMov::read(pc);
                    pc = pc.add(size_of::<OpMov>());
                    let src = get!(op.src);
                    set!(op.dst(), src);
                }

                OP_RETURN => {
                    let op = OpReturn::read(pc);

                    let src = get!(op.src);

                    let return_pc = (*fp).return_pc;
                    let caller = (*fp).caller;

                    // native code invoked this procedure, return to native code
                    if return_pc.is_null() {
                        return Ok(src);
                    } else {
                        // Invoked from VM code, return to VM code
                        fp = caller.cast::<CallFrame>();
                        pc = return_pc;

                        // read destination register
                        let call_inst = OpCall::read(pc);
                        pc = pc.add(size_of::<OpCall>());
                        let dst = call_inst.dst();
                        set!(dst, src);
                        continue 'interp;
                    }
                }

                OP_CALL => {
                    let op = OpCall::read(pc);
                    let callee = get!(op.callee());
                    let mut argument_start = op.argv() as isize;
                    argument_start = -argument_start;
                    let argc = op.argc();

                    let new_fp = fp
                        .cast::<Value>()
                        .offset(argument_start)
                        .cast::<CallFrame>();
                    (*new_fp).argc = Value::encode_int32(argc as _);
                    (*new_fp).callee = callee;
                    (*new_fp).caller = fp.cast();
                    (*new_fp).return_pc = pc;
                    sp = new_fp.cast();

                    if callee.is_vm_procedure() {
                        let cb = callee.procedure().code;
                        (*new_fp).code_block = cb.into();
                        pc = cb.start_ip();
                        fp = new_fp;
                        continue 'interp;
                    } else {
                        fp = new_fp;
                        pc = pc.add(size_of::<OpCall>());
                        continue 'tail_call;
                    }
                }

                OP_TAIL_CALL => {
                    let op = OpTailCall::read(pc);

                    let callee = get!(op.callee());
                    let caller = (*fp).caller;
                    let return_pc = (*fp).return_pc;

                    // arguments to `callee` begin here:
                    let mut argument_start = op.argv() as i16 as isize - 5;
                    argument_start = -argument_start;

                    // start of the previous frame which is: `fp + fp->argc`
                    let mut to = (*fp).args.as_mut_ptr().add((*fp).argument_count());
                    // start of arguments
                    let mut from = fp.cast::<Value>().offset(argument_start);
                    let argc = op.argc();
                    from = from.add(argc as _);
                    // copy arguments
                    for _ in 0..argc {
                        to = to.sub(1);
                        from = from.sub(1);
                        to.write(from.read());
                    }

                    sp = to;
                    // push new frame pointer
                    sp = sp.cast::<CallFrame>().sub(1).cast();
                    fp = sp.cast();
                    (*fp).return_pc = return_pc;
                    (*fp).caller = caller;
                    (*fp).callee = callee;
                    (*fp).argc = Value::encode_int32(argc as _);
                    println!(
                        "tail-call {}, {:?} fp={:p} sp={:p}",
                        callee,
                        (*fp).arguments(),
                        fp,
                        sp
                    );
                    if callee.is_vm_procedure() {
                        let cb = callee.procedure().code;
                        (*fp).code_block = cb.into();
                        pc = cb.start_ip();
                        continue 'interp;
                    } else {
                        continue 'tail_call;
                    }
                }

                OP_GLOBAL_REF => {
                    let op = OpGlobalRef::read(pc);
                    pc = pc.add(size_of::<OpGlobalRef>());
                    let dst = op.dst();
                    let code_block = (*fp).code_block().code_block();
                    let constant = code_block.literals.vector_ref(op.constant() as _);
                    if constant.is_xtype(Type::GLOC) {
                        set!(dst, constant.gloc().value);
                    } else {
                        let (value, gloc) =
                            scm_identifier_global_ref(constant.identifier()).unwrap();

                        set!(dst, value);
                        vm.mutator()
                            .write_barrier((*fp).code_block().code_block().literals.vector());
                        code_block
                            .literals
                            .vector_set(op.constant() as _, gloc.into());
                    }
                }

                OP_GLOBAL_SET => {
                    let op = OpGlobalSet::read(pc);
                    pc = pc.add(size_of::<OpGlobalSet>());
                    let src = get!(op.src);
                    let code_block = (*fp).code_block().code_block();
                    let constant = code_block.literals.vector_ref(op.constant() as _);
                    if constant.is_xtype(Type::GLOC) {
                        vm.mutator().write_barrier(constant.gloc());
                        constant.gloc().value = src;
                    } else {
                        let (_, mut gloc) =
                            scm_identifier_global_ref(constant.identifier()).unwrap();
                        vm.mutator().write_barrier(gloc);
                        gloc.value = src;
                        vm.mutator()
                            .write_barrier((*fp).code_block().code_block().literals.vector());
                        code_block
                            .literals
                            .vector_set(op.constant() as _, gloc.into());
                    }
                }

                OP_DEFINE => {
                    let def = OpDefine::read(pc);
                    pc = pc.add(size_of::<OpDefine>());

                    let src = get!(def.value());
                    let code_block = (*fp).code_block().code_block();
                    let name = code_block.literals.vector_ref(def.constant() as _);

                    let id = scm_outermost_identifier(name.identifier());
                    let module = id.module.module();
                    let name = id.name.symbol();

                    scm_make_binding(module, name, src, 0).unwrap();
                }

                OP_ASSERT_ARG_COUNT => {
                    let _op = OpAssertArgCount::read(pc);
                    pc = pc.add(size_of::<OpAssertArgCount>());
                }

                OP_ASSERT_MIN_ARG_COUNT => {
                    let _op = OpAssertMinArgCount::read(pc);
                    pc = pc.add(size_of::<OpAssertMinArgCount>());
                }

                OP_MOVI => {
                    let op = OpMovi::read(pc);
                    pc = pc.add(size_of::<OpMovi>());

                    set!(op.dst(), Value::encode_int32(op.imm()));
                }

                OP_MAKE_CLOSURE => {
                    let op = OpMakeClosure::read(pc);
                    pc = pc.add(size_of::<OpMakeClosure>());

                    let src = (*fp)
                        .code_block()
                        .code_block()
                        .literals
                        .vector_ref(op.src() as _)
                        .code_block();

                    let proc = make_closed_procedure(vm.mutator(), src, op.argc as _);
                    set!(op.dest(), proc.into());
                }

                OP_CLOSURE_CAPTURE => {
                    let op = OpClosureCapture::read(pc);
                    pc = pc.add(size_of::<OpClosureCapture>());

                    let src = get!(op.src());
                    let ix = op.index();
                    let proc = get!(op.dst());

                    let mut proc = proc.procedure();
                    vm.mutator().write_barrier(proc);
                    proc.captures.as_mut_ptr().add(ix as _).write(src);
                }

                OP_BOX => {
                    let op = OpBox::read(pc);
                    pc = pc.add(size_of::<OpBox>());

                    let src = get!(op.src());
                    let dst = op.dst();

                    let boxed = make_box(vm.mutator(), src);
                    set!(dst, boxed.into());
                }

                OP_BOX_REF => {
                    let op = OpBoxRef::read(pc);
                    pc = pc.add(size_of::<OpBoxRef>());

                    let src = get!(op.src());
                    let dst = op.dst();

                    let unboxed = src.box_ref();
                    set!(dst, unboxed);
                }

                OP_BOX_SET => {
                    let op = OpBoxSet::read(pc);
                    pc = pc.add(size_of::<OpBoxSet>());

                    let src = get!(op.value());
                    let dst = get!(op.box_());

                    vm.mutator().write_barrier(dst.get_object());
                    dst.box_set(src);
                }

                OP_CLOSURE_REF => {
                    let op = OpClosureRef::read(pc);
                    pc = pc.add(size_of::<OpClosureRef>());

                    let dst = op.dst();
                    let src = op.index();

                    let capture = (*fp).callee().procedure().captures.as_ptr().add(src as _);
                    set!(dst, capture.read());
                }

                OP_CLOSURE_SET => {
                    let op = OpClosureSet::read(pc);
                    pc = pc.add(size_of::<OpClosureSet>());

                    let src = get!(op.src());
                    let dst = op.index();

                    let capture = (*fp)
                        .callee()
                        .procedure()
                        .captures
                        .as_mut_ptr()
                        .add(dst as _);
                    vm.mutator().write_barrier((*fp).callee().procedure());
                    capture.write(src);
                }

                OP_CLOSURE_REF_BOX => {
                    let op = OpClosureRefBox::read(pc);
                    pc = pc.add(size_of::<OpClosureRefBox>());

                    let src = op.src();
                    let dst = op.index();

                    let capture = (*fp).callee().procedure().captures.as_ptr().add(dst as _);
                    let boxed = capture.read();
                    vm.mutator().write_barrier(boxed.get_object());
                    boxed.box_set(get!(src));
                }

                OP_CLOSURE_REF_UNBOX => {
                    let op = OpClosureRefUnbox::read(pc);
                    pc = pc.add(size_of::<OpClosureRefUnbox>());

                    let dst = op.dst();
                    let src = op.index();

                    let capture = (*fp).callee().procedure().captures.as_ptr().add(src as _);
                    let boxed = capture.read();
                    set!(dst, boxed.box_ref());
                }

                OP_BRANCH => {
                    let op = OpBranch::read(pc);
                    pc = pc.add(size_of::<OpBranch>());

                    let offset = op.offset() as isize;
                    pc = pc.offset(offset);
                }

                OP_BRANCH_IF => {
                    let op = OpBranchIf::read(pc);
                    pc = pc.add(size_of::<OpBranchIf>());

                    let cond = get!(op.src());
                    let offset = op.offset() as isize;

                    if !cond.is_false() {
                        pc = pc.offset(offset);
                    }
                }

                OP_BRANCH_IF_NOT => {
                    let op = OpBranchIfNot::read(pc);
                    pc = pc.add(size_of::<OpBranchIfNot>());

                    let cond = get!(op.src());
                    let offset = op.offset() as isize;

                    if cond.is_false() {
                        pc = pc.offset(offset);
                    }
                }

                OP_COLLECT_REST => {
                    let op = OpCollectRest::read(pc);
                    pc = pc.add(size_of::<OpCollectRest>());

                    let rest = (*fp).args.as_ptr().add(op.arg() as _);
                    let mut list = Value::encode_null_value();

                    for i in (0..(*fp).argc.get_int32() as usize - op.arg() as usize).rev() {
                        let e = rest.add(i as _).read();
                        list = scm_cons(vm.thread, e, list);
                    }

                    set!(op.dest(), list);
                }

                _ => unreachable!("{}", opcode),
            }
        }
    }
}

pub unsafe fn vm_entry(
    vm: &mut VM,
    callee: Value,
    args: &[Value],
    module: Option<Handle<Module>>,
) -> Result<Value, Value> {
    vm.prev_top_call_frame = vm.top_call_frame;
    vm.prev_top_entry_frame = vm.top_entry_frame;
    let saved_module = vm.module;
    let saved_entry = vm.next_entry();
    let sp = vm.sp;

    vm.module = module;

    let cb = AssertUnwindSafe(|| {
        let mut sp = vm.sp;

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
        vm.top_call_frame = cfr;
        vm.sp = sp;
        vm_eval(vm)
    });

    let result = std::panic::catch_unwind(|| cb());

    vm.top_call_frame = vm.prev_top_call_frame;
    vm.top_entry_frame = vm.prev_top_entry_frame;
    vm.module = saved_module;
    vm.entry = saved_entry;
    vm.sp = sp;

    match result {
        Ok(val) => val,
        Err(err) => std::panic::resume_unwind(err),
    }
}
