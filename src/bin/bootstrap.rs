use capy::{
    compiler::{expr_to_value, *},
    structure::struct_ref,
    util::arraylist::ArrayList,
    value::Value,
    vm::intern, fasl::FaslWriter, pp::pretty_print,
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

        let slib_path = std::env::var("SCHEME_LIBRARY_PATH").unwrap_or_else(|_| {
            std::env::current_dir()
                .unwrap()
                .join("lib")
                .to_str()
                .unwrap()
                .to_string()
        });

        let src = std::fs::read_to_string(&format!("{}/init.scm", slib_path)).unwrap();

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

        let cps = redex::redex_transform(cps::t_c(desugared, intern("|%toplevel-cont")), &mut 0);

        let mut st = StandardStream::stdout(termcolor::ColorChoice::Always);

        pretty_print(cps, &mut st).unwrap();
        println!("");

        let toplevel_code = Value::make_list(
            vm.mutator(),
            &[
                intern("lambda"),
                Value::make_list(Thread::current(), &[intern("|%toplevel-cont")]),
                cps,
            ],
        );


        
        let uncovered = uncover_assigned(toplevel_code);
        let assigned = convert_assignments(uncovered);


        let m = meaning::meaning(assigned, Value::make_null(), true);
    
        let (toplevel, lambdas) = lambda_lifting::lift_lambdas(m.cdddr().cdr(), true);
        m.cdddr().set_pair_cdr(toplevel);

        let mut file = std::fs::File::create(&format!("{}/init.fasl", slib_path)).unwrap();

        let mut writer = FaslWriter::new(&mut file);

        writer.start(defs, m, lambdas).unwrap();

        println!("init.fasl written");

        /*println!(
            ">compiled in {:.4}ms",
            cstart.elapsed().as_micros() as f64 / 1000.0
        );
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
        }*/

        Ok(())
    });
}
