use std::mem::transmute;

use capy::{vm::{options::VMOptions, scm_init, thread::Thread}, interpreter::{llint::LLIntGenerator, entry_record::vm_entry_record}};
use macroassembler::{jit::gpr_info::{CS1, ARGUMENT_GPR0}, assembler::{abstract_macro_assembler::AbsoluteAddress, link_buffer::LinkBuffer}};

extern "C-unwind" fn callback(thread: &mut Thread) {
    let entry_frame = thread.interpreter().top_entry_frame;
    unsafe {
        println!("entry frame: {:p}", entry_frame);
        let record = vm_entry_record(entry_frame);

        println!("prev_top_entry_frame: {:p}", (*record).prev_top_entry_frame);
        println!("prev_top_call_frame:  {:p}",  (*record).prev_top_call_frame);
    }
    println!("Inside callback...");
}

fn main() {
    let opts = match VMOptions::parse() {
        Ok(opts) => opts,
        Err(err) => {
            eprintln!("Error: {}", err);
            std::process::exit(1);
        }
    };

    let mut mmtk = mmtk::MMTKBuilder::new();
    mmtk.set_option("plan", opts.gc_plan.as_ref());
    mmtk.set_option(
        "gc_trigger",
        &format!(
            "DynamicHeapSize:{},{}",
            opts.gc_min_heap_size, opts.gc_max_heap_size
        ),
    );
    mmtk.set_option("threads", "4");

    let _vm = scm_init(mmtk.build(), opts.gc_plan);

    let ptr = 0 as *mut u8;
    unsafe {
        *ptr = 42;
    }

    /*let thread = Thread::current();
    let mut llint = LLIntGenerator::new();

    llint.do_vm_entry(|llint| {
        llint.mov(CS1, ARGUMENT_GPR0);
        llint.call_op(Some(AbsoluteAddress::new(callback as _)));
    });

    let mut link_buffer = LinkBuffer::from_macro_assembler(&mut llint).unwrap();

    let mut out = String::new();
    let code = link_buffer.finalize_with_disassembly(true, "llint", &mut out).unwrap();

    println!("{}", out);

    let cb: extern "C" fn(usize, &mut Thread, usize) = unsafe { transmute(code.start()) };

    cb(0, thread, 0);*/

    let fp = 1000;
    let sp = 900;

    let slot_0 = sp + 1;
    let converted_slot_1 = slot_0 - fp;
    println!("slot_0: {}, {}", slot_0, -converted_slot_1);



}
