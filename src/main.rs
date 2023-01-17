use capyscheme::prelude::*;

fn main() {
    env_logger::init();
    let res = scm_main_thread(|ctx| {
        let x = ctx.eval_path("test.scm", false);
        match x {
            Ok(val) => {
                println!("{}", val.to_string(false));
            }
            Err(exc) => {
                println!("{}", exc.to_string(false));
            }
        }
    });

    match res {
        ScmThreadResult::Ok(_) => {}
        ScmThreadResult::UncapturedContinuation(cont) => {
            println!("Uncaptured continuation: {:p}", cont);
        }
        ScmThreadResult::Panic(_) => {
            eprintln!("uncaptured panic");
        }

        ScmThreadResult::UnpacturedException(_) => {
            eprintln!("uncaptured exception");
        }
    }
}
