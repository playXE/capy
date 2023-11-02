use r5rscomp::expander::Cenv;
use r5rscomp::expander::global_env;
use r5rscomp::reader::SourceProvider;
use r5rscomp::reader::SymbolInterner;
use r5rscomp::sexpr::*;
use r5rscomp::expander::*;
use r5rscomp::tree_il::*;
use r7rs_parser::expr::NoIntern;
fn main() {
    let interner = SymbolInterner::new();
    let global = global_env(&interner);

    let denotation_of_define = global.lookup::<true>(&interner.intern("define")).special();
    let denotation_of_begin = global.lookup::<true>(&interner.intern("begin")).special();

    let cenv = Cenv {
        denotation_of_begin: Some(denotation_of_begin),
        denotation_of_define: Some(denotation_of_define),
        sources: SourceProvider::new(),
        expr_name: None,
        env: global.clone(),
        global_env: global,
        interner: interner.clone()
    };

    let source = std::fs::read_to_string("test.scm").unwrap();

    let mut i = NoIntern;
    let mut parser = r7rs_parser::parser::Parser::new(&mut i, &source, false);

    let mut exprs = Sexpr::Null;

    while !parser.finished() {
        let e = match parser.parse(true) {
            Ok(e) => e,
            Err(e) => {
                println!("Error: {:?}", e);
                return;
            }
        };
        
        exprs = cons(from_r7rs_parser(&e, &interner), exprs);
    }

    exprs = exprs.reverse();

    let mut out = termcolor::StandardStream::stdout(termcolor::ColorChoice::Always);


    let mut nodes = Vec::new();

    while let Some((expr, rest)) = exprs.pair() {
        let node = match expand(&expr, &cenv) {
            Ok(node) => node,
            Err(e) => {
                match e {
                    ScmError::UnexpectedType(expr, err) => {
                        eprint!("Error: {} {:?}:", err, expr.list_length());
                        expr.pretty_print(&mut out).unwrap();
                        eprintln!();
                    }

                    _ => {
                        eprintln!("Error: {:?}", e);
                    }
                }
                return;
            }
        };
        
        nodes.push(node);
        exprs = rest.clone();
    }

    let seq = make_seq(nodes);

    seq.pretty_print(&mut out).unwrap();

}