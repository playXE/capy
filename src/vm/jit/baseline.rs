//! # Baseline JIT
//!
//! Directly compiles bytecode to B3 IR, and then to machine code.

use std::{ptr::null_mut};

use macroassembler::jit::gpr_info::{ARGUMENT_GPR0, ARGUMENT_GPR1};

use crate::{
    op::disassembly,
    runtime::{
        error::wrong_contract,
        fun::scm_make_subr,
        list::scm_reverse,
        module::{scm_capy_module, scm_define},
        object::ScmResult,
        symbol::Intern,
        value::Value,
    },
    vm::{callframe::CallFrame, scm_vm, VMType},
};

pub mod stack2ssa;
pub mod thunks;

extern "C" fn jit_compile(cfr: &mut CallFrame) -> ScmResult {
    b3::macroassembler::jit::init_executable_allocator_with(jit_allocator::JitAllocatorOptions {
        use_dual_mapping: true,
        ..Default::default()
    });
    let proc = cfr.argument(0);

    if !proc.is_vm_procedure() {
        return wrong_contract::<()>("jit-compile", "vm-procedure?", 0, 1, cfr.arguments()).into();
    }
    let args = cfr.argument(1);
    let mut ops = b3::Options::default();
    ops.air_force_irc_allocator = true;
    //ops.dump_air_at_each_phase = true;
    //ops.dump_b3_at_each_phase = true;
    //ops.dump_b3_reduce_strength = true;
    ops.opt_level = b3::OptLevel::O2;
    let mut out = termcolor::StandardStream::stdout(termcolor::ColorChoice::Always);
    disassembly(proc.procedure().code, &mut out).unwrap();

    let mut procedure = b3::Procedure::new(ops);

    let entry = procedure.add_block(1.0);
    let mut builder = b3::BasicBlockBuilder::new(&mut procedure, entry);
    let vm_value = builder.argument(b3::Reg::new_gpr(ARGUMENT_GPR0), b3::Type::Int64);
    let cfr_value = builder.argument(b3::Reg::new_gpr(ARGUMENT_GPR1), b3::Type::Int64);

    let mut stack2ssa = stack2ssa::Stack2SSA::new(
        &mut procedure,
        proc.procedure().code,
        vm_value,
        cfr_value,
        entry,
    );

    stack2ssa.generate();

    println!("{}", procedure.display());

    let code = b3::compile(procedure);
    println!("{}", code.disassembly(),);
    let vm = scm_vm();
    let mut list = scm_reverse(scm_vm().mutator(), args);
    let mut sp = vm.sp;
    let saved_sp = vm.sp;
    vm.sp = sp;
    unsafe {
       
        let mut argc = 0;
        while list.is_pair() {
            sp = sp.sub(1);
            sp.write(list.car());
            list = list.cdr();
            argc += 1;
        }

        sp = sp.cast::<CallFrame>().sub(1).cast();
        sp.cast::<CallFrame>().write(CallFrame {
            caller: null_mut(),
            return_pc: null_mut(),
            code_block: proc.procedure().code.into(),
            argc: Value::encode_int32(argc),
            callee: proc,
            args: [],
        });

        vm.prev_top_call_frame = vm.top_call_frame;
        vm.prev_top_entry_frame = vm.top_entry_frame;

        vm.top_call_frame = sp.cast();
        vm.top_entry_frame = sp.cast();
        println!(
            "call {:p} {:?} {:p} {:p} {}",
            code.entrypoint(0),
            (*vm.top_call_frame).arguments(),
            vm.top_call_frame,
            cfr,
            cfr as *const CallFrame as isize - vm.top_call_frame as *mut CallFrame as isize
        );
        let f: extern "C" fn(&mut VMType, *mut CallFrame) -> ScmResult =
            std::mem::transmute(code.entrypoint(0));

        let res = f(vm, sp.cast());
        let vcfr = vm.top_call_frame;
        vm.top_call_frame = vm.prev_top_call_frame;
        vm.top_entry_frame = vm.prev_top_entry_frame;
        vm.sp = saved_sp;

        if res.is_ok() {
            println!("OK: {:?}", res.value())
        } else if res.is_err() {
            println!("ERR: {:?}", res.value())
        } else if res.tag == ScmResult::JIT_ERR {
            println!("JIT ERR: {:?}", res.value())
        } else if res.is_tail() {
            let _arg_start = vcfr.add(1).cast::<Value>().add((*vcfr).argument_count());

            println!("TAIL-CALL requested {} {:?}", (*vcfr).callee(), (*vcfr).arguments());
        }
    }

    ScmResult::ok(Value::encode_undefined_value())
}

pub(crate) fn init_baseline() {
    let module = scm_capy_module().module();

    let subr = scm_make_subr("jit-compile", jit_compile, 2, 2);
    scm_define(module, "jit-compile".intern().into(), subr).unwrap();
}
