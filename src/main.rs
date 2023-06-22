use capy::{
    
    compile::{make_cenv, pass1::pass1, r7rs_to_value, bytecompiler::ByteCompiler},
    list::{scm_cons, scm_reverse},
    module::scm_user_module,
    scm_dolist,
    value::Value, op::{Opcode, disassembly}, fun::make_procedure, vm::{interpreter::_vm_entry_trampoline, scm_vm},
};
use r7rs_parser::{expr::NoIntern, parser::Parser};
use rsgc::{
    prelude::HeapArguments,
    thread::{main_thread, Thread},
};

fn main() {
    env_logger::init();
    let _ = main_thread(HeapArguments::from_env(), |heap| {
        heap.add_core_root_set();

        capy::init();
        capy::vm::scm_init_vm();
        let t = Thread::current();

        let source = std::fs::read_to_string("test.scm").unwrap();
        let mut i = NoIntern;
        let mut parser = Parser::new(&mut i, &source, false);

        let module = scm_user_module().module();

        let mut bc = ByteCompiler::new(t);

        match bc.compile_toplevel(t, "test.scm", module, &mut parser)
        {
            Ok(proc) => {
                let proc = proc.procedure();
                let mut out = termcolor::StandardStream::stdout(termcolor::ColorChoice::Always);
                disassembly(proc.code, &mut out).unwrap();
            }

            Err(e) => {
                println!("error: {:?}", e);
            }
        }

        Ok(())
    });
}
