use crate::value::Value;
use pretty::{BoxAllocator, DocAllocator, DocBuilder};
use termcolor::{Color, ColorSpec, WriteColor};



pub fn pretty_print(exp: Value, out: impl WriteColor) -> std::io::Result<()> {
    let allocator = BoxAllocator;

    internal::pp(exp, &allocator).1.render_colored(100, out)?;

    Ok(())
}

mod internal {
    use super::*;
    fn pp_call<'a, D>(expr: Value, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        let head = expr.car();
        let mut tail = expr.cdr();

        let mut v = vec![];
        //v.push(pp(head, allocator));
        while tail.pairp() {
            v.push(pp(tail.car(), allocator));
            tail = tail.cdr();
        }
        if !tail.nullp() {
            v.push(
                allocator
                    .text("#<invalid call> ")
                    .append(pp(tail, allocator)),
            );
        }

        pp(head, allocator)
            .append(allocator.hardline())
            .nest(4)
            .append(
                allocator
                    .intersperse(v.into_iter(), allocator.hardline())
                    .nest(4),
            )
            //.group()
            .parens()
    }

    fn pp_app<'a, D>(expr: Value, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        let head = expr.cdr().car();
        let mut tail = expr.cdr().cdr();

        let mut v = vec![];
        //v.push(pp(head, allocator));
        while tail.pairp() {
            v.push(pp(tail.car(), allocator));
            tail = tail.cdr();
        }
        if !tail.nullp() {
            v.push(
                allocator
                    .text("#<invalid call> ")
                    .append(pp(tail, allocator)),
            );
        }

        allocator
            .text("#%app")
            .annotate(ColorSpec::new().set_fg(Some(Color::Cyan)).clone())
            .append(allocator.space())
            .append(pp(head, allocator))
            .append(allocator.hardline())
            .nest(4)
            .append(
                allocator
                    .intersperse(v.into_iter(), allocator.hardline())
                    .nest(4),
            )
            //.group()
            .parens()
    }
    

    fn pp_define_args<'a, D>(
        name: Value,
        args: Value,
        allocator: &'a D,
    ) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        let mut v = vec![];
        v.push(pp(name, allocator));
        let mut args = args;
        while args.pairp() {
            v.push(pp(args.car(), allocator));
            args = args.cdr();
        }
        if !args.nullp() {
            v.push(allocator.text(".").append(pp(args, allocator)));
        }
        allocator
            .intersperse(v.into_iter(), allocator.space())
            .parens()
    }

    fn pp_set_then<'a, D>(exp: Value, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        allocator
            .text("set-then!")
            .annotate(ColorSpec::new().set_fg(Some(Color::Magenta)).clone())
            .append(allocator.space())
            .append(pp(exp.cadr(), allocator))
            .append(allocator.space())
            .append(pp(exp.caddr(), allocator))
            .append(allocator.line().nest(4))
            .append(pp(exp.cadddr(), allocator))
            .parens()
    }

    fn pp_set<'a, D>(exp: Value, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        allocator
            .text("set!")
            .annotate(ColorSpec::new().set_fg(Some(Color::Magenta)).clone())
            .append(allocator.space())
            .append(pp(exp.cadr(), allocator))
            .append(allocator.space())
            .append(pp(exp.caddr(), allocator))
            .parens()
    }

    fn pp_define<'a, D>(exp: Value, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        if exp.nullp() {
            allocator.text("define").parens()
        } else if exp.cddr().nullp() {
            allocator
                .text("define")
                .annotate(ColorSpec::new().set_fg(Some(Color::Magenta)).clone())
                .append(allocator.space())
                .append(pp(exp.cadr(), allocator))
                .parens()
        } else if exp.cadr().pairp() {
            let pattern = exp.cadr();
            let f = pattern.car();
            let args = pattern.cdr();
            let body = exp.cddr();

            allocator
                .text("define")
                .annotate(ColorSpec::new().set_fg(Some(Color::Magenta)).clone())
                .append(allocator.space())
                .append(pp_define_args(f, args, allocator))
                .append(allocator.line())
                .nest(4)
                .append(pp_body(body, allocator))
                .parens()
        } else {
            let name = exp.cadr();
            let exp = exp.caddr();

            allocator
                .text("define")
                .annotate(ColorSpec::new().set_fg(Some(Color::Magenta)).clone())
                .append(allocator.space())
                .append(allocator.text(name.to_string()))
                .append(allocator.space())
                .append(pp(exp, allocator))
                .parens()
        }
    }

    fn pp_body<'a, D>(body: Value, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        /*if body.nullp() {
            allocator.nil()
        } else if body.cdr().nullp() {
            pp(body.car(), allocator)
        } else {
            pp(body.car(), allocator)
                .append(allocator.line())
                .nest(4)
                .append(pp_body(body.cdr(), allocator))

        }*/

        let mut v = vec![];
        let mut body = body;
        while body.pairp() {
            v.push(pp(body.car(), allocator));
            body = body.cdr();
        }

        allocator
            .intersperse(v.into_iter(), allocator.hardline())
            .nest(4)
    }

    fn pp_lambda<'a, D>(exp: Value, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        let args = exp.cadr();
        let body = exp.cddr();

        allocator
            .text("lambda")
            .annotate(ColorSpec::new().set_fg(Some(Color::Magenta)).clone())
            .append(allocator.space())
            .append(pp(args, allocator))
            .append(allocator.line())
            .nest(4)
            .append(pp_body(body, allocator))
            .parens()
    }

    fn pp_if<'a, D>(exp: Value, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        if let Some(l) = exp.proper_list_length() {
            if l == 3 {
                let test = pp(exp.cadr(), allocator);
                let conseq = pp(exp.caddr(), allocator);

                allocator
                    .text("if")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Magenta)).clone())
                    .append(allocator.space().nest(4))
                    .append(test)
                    .append(allocator.line().nest(4))
                    .append(conseq)
                    .parens()
            } else if l == 4 {
                let test = pp(exp.cadr(), allocator);
                let conseq = pp(exp.caddr(), allocator);
                let alt = pp(exp.cadddr(), allocator);

                allocator
                    .text("if")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Magenta)).clone())
                    .append(allocator.space())
                    .append(test)
                    .append(allocator.line().nest(4))
                    .append(conseq)
                    .append(allocator.line().nest(4))
                    .append(alt)
                    .parens()
            } else {
                allocator.text("if").parens()
            }
        } else {
            allocator.text("if").parens()
        }
    }

    fn pp_begin<'a, D>(exp: Value, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        let body = exp.cdr();
        allocator
            .text("begin")
            .annotate(ColorSpec::new().set_fg(Some(Color::Magenta)).clone())
            .append(allocator.hardline())
            .nest(4)
            .append(pp_body(body, allocator))
            //.nest(4)
            .group()
            .parens()
    }
    pub fn pp<'a, D>(exp: Value, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        if exp.vectorp() {
            let v = allocator.intersperse(
                exp.vector_as_slice().iter().map(|v| pp(*v, allocator)),
                allocator.space(),
            );
            allocator.text("#(").append(v).append(allocator.text(")"))
        } else if exp.byte_vectorp() {
            let v = allocator.intersperse(
                exp.byte_vector_as_slice()
                    .iter()
                    .map(|v| allocator.text(format!("{}", v))),
                allocator.space(),
            );
            allocator.text("#u8(").append(v).append(allocator.text(")"))
        } else if exp.symbolp() {
            let exp = exp.strsym();
            let color = if exp.starts_with("#%") {
                Color::Cyan
            } else {
                Color::Yellow
            };
    
            allocator
                .text(exp)
                .annotate(ColorSpec::new().set_fg(Some(color)).clone())
        } else if exp.pairp() {
            if exp.car().symbolp() {
                let sym = exp.car().strsym();
                match sym {
                    "define" => pp_define(exp, allocator),
                    "lambda" => pp_lambda(exp, allocator),
                    "if" => pp_if(exp, allocator),
                    "begin" => pp_begin(exp, allocator),
                    "set!" => pp_set(exp, allocator),
                    "set-then!" => pp_set_then(exp, allocator),
                    "quote" => {
                        let exp = exp.cadr();
                        allocator
                            .text("'")
                            .append(pp(exp, allocator))
                            .annotate(ColorSpec::new().set_fg(Some(Color::Blue)).clone())
                    }
                    "#%app" => pp_app(exp, allocator),
                    _ => pp_list(exp, allocator),
                }
            } else {
                pp_call(exp, allocator)
            }
        } else {
            allocator.text(format!("{}", exp))
        }
    }

    fn pp_list<'a, D>(exp: Value, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        let head = exp.car();
        let mut tail = exp.cdr();

        let mut v = vec![];
        v.push(pp(head, allocator));
        while tail.pairp() {
            v.push(pp(tail.car(), allocator));
            tail = tail.cdr();
        }
        if !tail.nullp() {
            v.push(
                allocator
                    .text(".")
                    .append(allocator.space())
                    .append(pp(tail, allocator)),
            );
        }
        allocator
            .intersperse(v.into_iter(), allocator.space())
            .parens()
    }
}
