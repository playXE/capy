//! Bootstrap compiler. Takes input files as arguments and outputs single bytecode file.

use std::collections::HashMap;

use capy::compiler::expand::define_syntax;
use capy::compiler::sexpr::r7rs_expr_to_sexpr;
use capy::compiler::sexpr::Sexpr;
use capy::compiler::tree_il::*;
use capy::compiler::Cenv;
use capy::compiler::P;
use capy::compiler::{compile, compile_bytecode::compile_bytecode};
use capy::runtime::symbol::scm_intern;
use capy::vm::scm_init;
use r7rs_parser::expr::*;
use r7rs_parser::parser::*;
fn main() {
    env_logger::init();
    let mut builder = mmtk::MMTKBuilder::new();
    builder.set_option("plan", "Immix");
    builder.set_option("gc_trigger", "FixedHeapSize:128m");
    builder.set_option("threads", "1");

    let _ = scm_init(builder.build(), capy::vm::options::GCPlan::Immix);

    let args = std::env::args().skip(1).collect::<Vec<_>>();

    if args.len() == 0 {
        eprintln!("Usage: capy-boot <input files>");
        std::process::exit(1);
    }

    let mut interner = NoIntern;

    let mut toplevel_seq = vec![];
    let env = define_syntax();
    let mut source_info = HashMap::new();
    for filename in args.iter() {
        let src = match std::fs::read_to_string(filename) {
            Ok(src) => src,
            Err(err) => {
                eprintln!("Error reading {}: {}", filename, err);
                std::process::exit(1);
            }
        };
        println!("Compiling {}...", filename);
        let mut parser = Parser::new(&mut interner, &src, false);

        loop {
            if parser.finished() {
                break;
            }
            let result = parser.parse(true);
            match result {
                Ok(expr) => {
                    let interner = NoIntern;
                    let sexpr = r7rs_expr_to_sexpr(
                        &interner,
                        scm_intern(filename),
                        &expr,
                        &mut source_info,
                    );
                    let cenv = Cenv {
                        frames: Sexpr::Null,
                        syntax_env: env.clone(),
                        source_loc: &source_info,
                    };
                    let iform = match compile(&sexpr, &cenv, true, true) {
                        Ok(iform) => iform,
                        Err(err) => {
                            eprintln!("Error compiling {}: {}", filename, err);
                            std::process::exit(1);
                        }
                    };
                    toplevel_seq.push(iform);
                }

                Err(err) => {
                    eprintln!("Error parsing {}: {}", filename, err);
                    std::process::exit(1);
                }
            }
        }
    }

    let toplevel_lambda = P(Lambda {
        src: None,
        name: Some("<toplevel>".to_string()),
        reqargs: 0,
        optarg: false,
        lvars: vec![],
        body: P(IForm::Seq(Seq {
            src: None,
            forms: toplevel_seq,
        })),
        bound_lvars: Default::default(),
        flag: LambdaFlag::None,
        calls: vec![],
        free_lvars: vec![],
        defs: vec![],
        lifted_var: LiftedVar::Candidate,
    });

    let lam = P(IForm::Lambda(toplevel_lambda));

    let mut bcode = vec![];
    //let mut out = termcolor::StandardStream::stderr(termcolor::ColorChoice::Never);
    //lam.pretty_print::<true>(&mut out).unwrap();
    //println!();
    compile_bytecode(lam, &mut bcode);

    match std::fs::write("boot.capy", bcode) {
        Ok(_) => {}
        Err(err) => {
            eprintln!("Error writing boot.capy: {}", err);
            std::process::exit(1);
        }
    }
}
