#![allow(unused_imports)]
extern crate capy;

use capy::{
    bytecode::{image::load_image_from_memory, opcodes::disassemble},
    bytecodeassembler::fasl::FASLPrinter,
    compiler::{
        compile,
        compile_bytecode::compile_bytecode,
        expand::{define_syntax, pass1},
        pass2::pass2,
        sexpr::{r7rs_expr_to_sexpr, Sexpr},
        tree_il::{IForm, Lambda, LambdaFlag, Seq},
        Cenv, P,
    },
    interpreter::scm_call_n,
    runtime::{
        gsubr::{scm_define_subr, Subr},
        object::{scm_car, scm_cdr, scm_set_car, scm_set_cdr, TypeId, *},
        value::Value,
    },
    utils::pretty_hex::{self, pretty_hex, PrettyHex},
    vm::{scm_init, scm_virtual_machine, thread::Thread}, gc::objstorage::ObjStorage,
};
use r7rs_parser::expr::NoIntern;

fn main() {
    env_logger::init();
    let mut builder = mmtk::MMTKBuilder::new();
    builder.set_option("plan", "Immix");
    builder.set_option("gc_trigger", "DynamicHeapSize:32m,128m");
    builder.set_option("threads", "6");
    builder.set_option(
        "nursery",
        &format!("Fixed:{}", &(4 * 1024 * 1024).to_string()),
    );
    //builder.set_option("stress_factor", &(128 * 1024).to_string());
    let _ = scm_init(builder.build());

    scm_define_subr(
        "print",
        0,
        0,
        1,
        Subr::F1({
            extern "C-unwind" fn print(_: &mut Thread, rest: &mut Value) -> Value {
                println!("{}", rest);
                Value::encode_null_value()
            }

            print
        }),
    );

    let mut interner = NoIntern;
    let src = std::fs::read_to_string("test.scm").unwrap();
    let mut parser = r7rs_parser::parser::Parser::new(&mut interner, &src, false);

    let syntax_env = define_syntax();

    let mut toplevel_seq = vec![];

    while !parser.finished() {
        let expr = parser.parse(true).unwrap();

        let _cenv = Cenv {
            frames: Sexpr::Null,
            syntax_env: syntax_env.clone(),
        };
        let interner = NoIntern;
        let expr = r7rs_expr_to_sexpr(&interner, &expr);

        let mut out = termcolor::StandardStream::stdout(termcolor::ColorChoice::Always);

        let iform = compile(
            &expr,
            &Cenv {
                frames: Sexpr::Null,
                syntax_env: syntax_env.clone(),
            },
            true,
            true,
        )
        .unwrap();

        iform.pretty_print::<true>(&mut out).unwrap();
        println!();

        toplevel_seq.push(iform);
    }

    let lam = P(Lambda {
        name: None,
        reqargs: 0,
        optarg: false,
        lvars: vec![],
        body: P(IForm::Seq(Seq {
            forms: toplevel_seq,
        })),
        flag: LambdaFlag::None,
        calls: vec![],
        free_lvars: vec![],
        bound_lvars: Default::default(),
        defs: vec![],
        lifted_var: capy::compiler::tree_il::LiftedVar::Candidate,
    });

    let mut bcode = vec![];
    compile_bytecode(P(IForm::Lambda(lam)), &mut bcode);
    println!("{}", pretty_hex(&bcode));

    let image = load_image_from_memory(&bcode, None).unwrap_or_else(|_| unreachable!());
    image.disassemble();
    mmtk::memory_manager::handle_user_collection_request(&scm_virtual_machine().mmtk, Thread::current().to_mmtk());
    let res = scm_call_n(Thread::current(), image.entry_program, &[]);
    
    match res {
        Ok(v) => println!("Result: {}", v),
        Err(e) => println!("{}", e),
    }
}

