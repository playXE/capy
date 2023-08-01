#![allow(unused_imports)]
use fscheme::{
    compiler::{
        compile,
        expand::{define_syntax, pass1},
        pass2::pass2,
        sexpr::{r7rs_expr_to_sexpr, Sexpr},
        Cenv,
    },
    gc_frame,
    runtime::{
        object::{scm_car, scm_cdr, scm_set_car, scm_set_cdr},
        value::Value,
    },
    vm::{scm_init, scm_virtual_machine, thread::Thread},
};
use r7rs_parser::expr::NoIntern;

fn make_tree(thread: &mut Thread, depth: i32) -> Value {
    thread.safepoint();
    if depth == 0 {
        return thread
            .make_cons(Value::encode_null_value(), Value::encode_null_value())
            .into();
    } else {
        let pair: Value = thread
            .make_cons(Value::encode_null_value(), Value::encode_null_value())
            .into();
        gc_frame!(thread.stackchain() => pair = pair);
        let car = make_tree(thread, depth - 1);
        scm_set_car(*pair, thread, car);
        let cdr = make_tree(thread, depth - 1);
        scm_set_cdr(*pair, thread, cdr);

        //scm_set_car(pair, thread, *car);
        //scm_set_cdr(pair, thread, *cdr);

        return *pair;
    }
}

fn check_tree(tree: Value) -> usize {
    let left = scm_car(tree);
    let right = scm_cdr(tree);

    if left.is_null() {
        return 1;
    }

    return 1 + check_tree(left) + check_tree(right);
}

fn main() {
    env_logger::init();
    let mut builder = mmtk::MMTKBuilder::new();
    builder.set_option("plan", "GenImmix");
    builder.set_option("gc_trigger", "DynamicHeapSize:128m,4g");
    builder.set_option("threads", "4");

    let _ = scm_init(builder.build());


    let depth = 21;
    let min_depth = 4;
    let max_depth = depth;

    let stretch_depth = depth + 1;

    {
        println!("started");
        println!(
            "stretch tree of depth {}\tcheck: {}",
            stretch_depth,
            check_tree(make_tree(Thread::current(), stretch_depth))
        );

        let long_lasting_tree = make_tree(Thread::current(), max_depth);
        let thread = Thread::current();
        gc_frame!(thread.stackchain() => long_lasting_tree = long_lasting_tree);

        let mut d = min_depth;
        mmtk::memory_manager::handle_user_collection_request(&scm_virtual_machine().mmtk, thread.to_mmtk());
        while d <= max_depth {
            let iterations = 1 << (max_depth - d + min_depth);

            let mut check = 0;

            for _ in 1..=iterations {
                let tree_node = make_tree(Thread::current(), d);
                check += check_tree(tree_node);
            }

            println!("{}\t trees of depth {}\tcheck: {}", iterations, d, check);

            d += 2;
        }

        println!(
            "long lived tree of depth {}\tcheck: {}",
            max_depth,
            check_tree(*long_lasting_tree)
        );
    }
}
 
 /* 
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
    let mut interner = NoIntern;
    let src = std::fs::read_to_string("test.scm").unwrap();
    let mut parser = r7rs_parser::parser::Parser::new(&mut interner, &src, false);

    let syntax_env = define_syntax();

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
        )
        .unwrap();

        iform.pretty_print::<true>(&mut out).unwrap();
        println!();

        drop(iform);
    }
}
*/