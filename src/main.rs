use capyscheme::{prelude::*, runtime::error::wrong_contract, data::structure::StructInstance, utilities::bigint::BigInt};

fn main() {
    env_logger::init();
    let res = scm_main_thread(|ctx| {
        /*let x = ctx.eval_path("test.scm", false);
        match x {
            Ok(val) => {
                println!("{}", val.to_string(false));
            }
            Err(exc) => {
                println!("{}", exc.to_string(false));
            }
        }*/
        let bigint = BigInt::from_u64(ctx.mutator(), 42);
        println!("{:?}", bigint.uwords());
        println!("{}", bigint.to_string(&BigInt::DEC_BASE));
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
