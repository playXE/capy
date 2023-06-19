use capy::{
    comp::ast::r7rs_to_value,
    compile::{make_cenv, pass1::pass1},
    list::{scm_cons, scm_reverse},
    module::scm_user_module,
    scm_dolist,
    value::Value,
};
use r7rs_parser::expr::NoIntern;
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
        let cenv = make_cenv(scm_user_module().module(), Value::encode_null_value());

        let file = std::fs::read_to_string("test.scm").unwrap();
        let mut i = NoIntern;
        let mut parser = r7rs_parser::parser::Parser::new(&mut i, &file, false);
        let mut sexp = Value::encode_null_value();
        let t = Thread::current();
        while !parser.finished() {
            let expr = parser.parse(true).unwrap();
            let expr = r7rs_to_value(t, &expr);

            sexp = scm_cons(t, expr, sexp);
        }

        sexp = scm_reverse(t, sexp);
        let mut out = termcolor::StandardStream::stdout(termcolor::ColorChoice::Always);
        scm_dolist!(sexp, sexp, {
            match pass1(sexp, cenv) {
                Ok(expr) => {
                    expr.pretty_print(&mut out).unwrap();
                    println!();
                }
                Err(e) => {
                    println!("Err: {:?}", e);
                }
            }
        });

        Ok(())
    });
}
