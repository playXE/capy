use r7rs_parser::{expr::NoIntern, parser::ParseError};

use crate::{prelude::{*, eval_error::EvalError}, data::exception::{Exception, SourcePosition}, compiler::{Compiler, r7rs_to_value}, utilities::arraylist::ArrayList};

pub(crate) fn core_library(rt: &mut Runtime) {
    let manager = library_manager();

    let base = manager.scheme_module.get_handle_of::<Library>();
    let thread = Thread::current();
    let name = Str::new(thread, "<loader>");
    let loader = Procedure {
        kind: ProcedureKind::Primitive(name, Implementation::Apply(compile_and_eval_first), None),
        id: Procedure::new_id(),
        module: base
    };

    let loader = thread.allocate(loader);
    rt.loader = Value::new(loader);

    let name = Str::new(thread, "load");
    let sname = rt.symbol_table().intern("load");
    let load = Procedure {
        kind: ProcedureKind::Primitive(name, Implementation::Apply(load), None),
        id: Procedure::new_id(),
        module: base
    };

    let load = thread.allocate(load);
    manager.define_const(base, sname, Value::new(load));
}


pub fn compile_and_eval_first(ctx: &mut Context, args: &Arguments) -> (Handle<Procedure>, ArrayList<Value>) {
    if args.len() != 3 {
        let ls = Value::make_list_slice(ctx, args, Value::nil());
        let exc = Exception::argument_count(ctx, None, 3, 3, ls, SourcePosition::unknown());
        ctx.error(exc);
    }

    let form = args[0];
    let source_dir = args[1];
    let library = args[2];

    let source_dir = if source_dir.is_string() {
        Some(source_dir.get_string())
    } else {
        None 
    };


    let library = if library.is_handle_of::<Library>() {
        library.get_handle_of::<Library>()
    } else {
        library_manager().user_module.get_handle_of::<Library>()
    };

    let code = Compiler::build(ctx, form, library, source_dir);

    let named = ClosureType::Named(Str::new(ctx.mutator(), "<loader>"));
    let proc = Procedure {
        kind: ProcedureKind::Closure(named, Value::nil(), ctx.runtime().empty_array, code),
        id: Procedure::new_id(),
        module: library 
    };
    let args = ArrayList::new(ctx.mutator());
    (ctx.mutator().allocate(proc), args)
}

pub fn load(ctx: &mut Context, args: &Arguments) -> (Handle<Procedure>, ArrayList<Value>) {
    if args.len() != 1 || args.len() != 2 {
        let ls = Value::make_list_slice(ctx, args, Value::nil());
        let exc = Exception::argument_count(ctx, None, 1, 2, ls, SourcePosition::unknown());
        ctx.error(exc);
    }

    let path = args.first().unwrap().to_string(false);

    let file_manager = ctx.runtime().file_manager.lock(true);
    let current = std::env::current_dir();
    let cur = current.as_ref().unwrap().to_str();
    let filename = file_manager.file_path(&path,cur )
        .or_else(|| {
            file_manager.library_file_path(&path, cur)
        }).unwrap_or(path);

    let library = if args.len() == 2 {
        if args[1].is_handle_of::<Library>() {
            args[1].get_handle_of::<Library>()
        } else {
            library_manager().user_module.get_handle_of::<Library>()
        }
    } else {
        library_manager().user_module.get_handle_of::<Library>()
    };

    drop(file_manager);

    let source = match std::fs::read_to_string(&filename) {
        Ok(source) => source,
        Err(_)=> {
            let exc = Exception::eval(ctx, EvalError::LibraryNotFound, &[args[0]], SourcePosition::unknown());
            ctx.error(exc);
        }
    };
    let mut i = NoIntern;
    let mut parser = r7rs_parser::parser::Parser::new(&mut i, &source, false);
    let mut exprs = Value::nil();

    while !parser.finished() {
        match parser.parse(true) {
            Ok(expr) => {
                let val = r7rs_to_value(ctx, u32::MAX, &expr);

                exprs = ctx.make_pair(val, exprs);
            }
            Err(err) => {
                match err {
                    ParseError::Lexical(pos, lexical) => {
                        let exc = Exception::lexical(ctx, lexical, &[], SourcePosition {
                            source_id: u32::MAX,
                            position: pos 
                        });

                        ctx.error(exc);
                    }

                    ParseError::Syntax(pos, syntax) => {
                        let exc = Exception::syntax(ctx, syntax, &[], SourcePosition {
                            source_id: u32::MAX,
                            position: pos 
                        });

                        ctx.error(exc);
                    }
                }
            }
        }
    }

    let loader = ctx.runtime().loader.get_handle_of::<Procedure>();
    let mut args = ArrayList::new(ctx.mutator());
    let dir = std::path::Path::new(&filename).parent().unwrap().to_str().unwrap();
    let dir = Str::new(ctx.mutator(), dir);
    args.push(ctx.mutator(), exprs);
    args.push(ctx.mutator(), Value::new(dir));
    args.push(ctx.mutator(), Value::new(library));

    (loader, args)
}

pub fn define()