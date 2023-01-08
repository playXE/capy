use capyscheme::prelude::*;

fn main() {
    env_logger::init();
    scm_main_thread(|ctx| {
        let x = ctx.eval_path("test.scm", false);
        match x {
            Ok(val) => {
                println!("{}", val.to_string(false));
            }
            Err(exc) => {
                println!("{}", exc.inline_description());
            }
        }
    });
}
