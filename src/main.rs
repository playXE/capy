use capy::{
    compiler::{expr_to_value, *, env::make_environment},
    util::arraylist::ArrayList,
    value::Value, pp::pretty_print, structure::struct_ref, vm::intern, jit::cranelift::JIT, r4rs::initialize_r4rs_environment,
};
use r7rs_parser::{expr::NoIntern, parser::Parser};
use rsgc::{prelude::*, heap::GCHeuristic};
use termcolor::StandardStream;

fn main() {
    env_logger::init();
    let mut args = HeapArguments::from_env();
    args.heuristics = GCHeuristic::Static;
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

        let cps = cps::t_c(desugared, intern("|%toplevel-cont"));

        let toplevel_code = Value::make_list(vm.mutator(),
            &[
                intern("lambda"),
                Value::make_list(Thread::current(), &[intern("|%toplevel-cont")]),
                cps]);

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
        let env = make_environment(intern("r4rs"));
        initialize_r4rs_environment(env);

        let code = jit.compile(env, defs, lambdas, m);
        let start = std::time::Instant::now();
        let val = vm.apply(code, &[]);

        match val {
            Ok(v) => {
                println!(">result (in {:.4}ms):", start.elapsed().as_micros() as f64 / 1000.0);
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
