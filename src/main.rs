use fscheme::{
    compiler::{
        expand::{define_syntax, pass1},
        sexpr::{r7rs_expr_to_sexpr, Sexpr},
        Cenv,
    },
    vm::scm_init,
};
use r7rs_parser::expr::NoIntern;

fn main() {
    let mut builder = mmtk::MMTKBuilder::new();
    builder.set_option("plan", "GenImmix");
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

        expr.pretty_print(&mut out).unwrap();
        println!();

        let iform = pass1(&expr, &_cenv).unwrap();

        iform.pretty_print(&mut out).unwrap();
        println!();
    }
}
