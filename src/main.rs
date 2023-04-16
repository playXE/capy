use std::vec;

use capy::{
    compiler::desugar::{expr_to_value, make_begin2, desugar},
    runtime::{value::ScmValue, Runtime, fmt::pretty},
};
use r7rs_parser::{expr::NoIntern, parser::Parser};
use rsgc::{prelude::HeapArguments, thread::main_thread};
fn main() {
    let _ = main_thread(HeapArguments::from_env(), move |_| {
        let mut nointern = NoIntern;

        let src = std::fs::read_to_string("test.scm").unwrap();
        let mut parser = Parser::new(&mut nointern, &src, false);

        let rt = Runtime::new();

        let cx = rt.context();

        let mut exprs = vec![];

        while !parser.finished() {
            let expr = parser.parse(true).unwrap();
            exprs.push(expr_to_value(cx, &expr));
        }

        let list = cx.make_list(&exprs);
        let toplevel = make_begin2(cx, list);
        
        let (desugared, defs) = match desugar(cx, toplevel) {
            Ok(val) => val,
            Err(err) => {
                eprintln!("{}", err.strvalue());
                return Ok(());
            }
        };

        let mut out = termcolor::Ansi::new(std::io::stdout());
        

        pretty(desugared, &mut out).unwrap();
        println!();
        pretty(defs, &mut out).unwrap();
        println!();
        Ok(())
    });
}
