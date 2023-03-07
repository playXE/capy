use capy::{
    compiler::{env::{make_environment, environment_get}, expr_to_value, *},
    jit::cranelift::JIT,
    pp::pretty_print,
    r5rs::{initialize_r5rs_environment, interaction_environment},
    structure::struct_ref,
    util::arraylist::ArrayList,
    value::Value,
    vm::intern,
};
use r7rs_parser::{expr::NoIntern, parser::Parser};
use rsgc::prelude::*;
use termcolor::StandardStream;

fn main() {
    env_logger::init();
    let args = HeapArguments::from_env();
    //args.heuristics = GCHeuristic::Static;
    let _ = rsgc::prelude::main_thread(args, |heap| {
        heap.add_core_root_set();
        let vm = capy::vm::vm();

        let src = std::fs::read_to_string("test.scm").unwrap();

        let mut nointern = NoIntern;
        let mut parser = Parser::new(&mut nointern, &src, false);

        let mut expr = vec![];

        while !parser.finished() {
            let e = parser.parse(true).unwrap();
            expr.push(e);
        }

        let mut exprs2 = ArrayList::with_capacity(vm.mutator(), expr.len());

        for e in expr {
            exprs2.push(vm.mutator(), expr_to_value(&nointern, &e));
        }

        let mut st = StandardStream::stdout(termcolor::ColorChoice::Always);
        let body = Value::make_list(vm.mutator(), &exprs2);

        let begin = make_begin2(body);

        let mut defs = Value::make_null();
        let desugared = match desugar(begin, &mut defs) {
            Ok(e) => e,
            Err(e) => {
                println!("pass1 error: {:?}", struct_ref(e, 0));
                return Ok(());
            }
        };
        //pretty_print(desugared, &mut st).unwrap();
        //print!("\n");

        let cps = redex::redex_transform(cps::t_c(desugared, intern("|%toplevel-cont")), &mut 2);

        let toplevel_code = Value::make_list(
            vm.mutator(),
            &[
                intern("lambda"),
                Value::make_list(Thread::current(), &[intern("|%toplevel-cont")]),
                cps,
            ],
        );

        println!(">after cps conversion:");
        pretty_print(toplevel_code, &mut st).unwrap();
        print!("\n");

        //println!(">after assignment conversion:");
        let uncovered = uncover_assigned(toplevel_code);
        let assigned = convert_assignments(uncovered);

        //pretty_print(assigned, &mut st).unwrap();
        //print!("\n");

        let m = meaning::meaning(assigned, Value::make_null(), true);

        let (toplevel, lambdas) = lambda_lifting::lift_lambdas(m.cdddr().cdr(), true);
        m.cdddr().set_pair_cdr(toplevel);

        let mut jit = JIT::default();
        let env = interaction_environment();

        let cstart = std::time::Instant::now();
        let code = jit.compile(env, defs, lambdas, m);
        println!(">compiled in {:.4}ms", cstart.elapsed().as_micros() as f64 / 1000.0);
        let start = std::time::Instant::now();
        let val = vm.apply(code, &[]);

        match val {
            Ok(v) => {
                println!(
                    ">result (in {:.4}ms):",
                    start.elapsed().as_micros() as f64 / 1000.0
                );
                pretty_print(v, &mut st).unwrap();
                print!("\n");
            }
            Err(e) => {

                println!("{}", struct_ref(e, 0));
            }
        }

        Ok(())
    });
}
