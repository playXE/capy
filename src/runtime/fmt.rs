//! Formatting for Scheme values.

use super::value::ScmValue;
use pretty::{BoxAllocator, DocAllocator, DocBuilder};
use termcolor::{ColorSpec, WriteColor};

pub fn pretty(expr: ScmValue, out: &mut dyn WriteColor) -> Result<(), std::io::Error> {
    let allocator = BoxAllocator;
    {
        pretty_inner(expr, &allocator).1.render_colored(70, out)?;
    }
    Ok(())
}

fn pretty_inner<'b, D>(val: ScmValue, allocator: &'b D) -> DocBuilder<'b, D, ColorSpec>
where
    D: DocAllocator<'b, ColorSpec>,
    D::Doc: Clone,
{
    if val.is_int32() {
        allocator.text(val.get_int32().to_string())
    } else if val.is_double() {
        allocator.text(val.get_double().to_string())
    } else if val.is_undefined() {
        allocator.text("#<undefined>")
    } else if val.is_null() {
        allocator.text("()")
    } else if val.is_bool() {
        if val.is_true() {
            allocator.text("#t")
        } else {
            allocator.text("#f")
        }
    } else if val.is_eof() {
        allocator.text("#<eof>")
    } else if val.is_symbol() {
        allocator.text(val.get_symbol().to_string())
    } else if val.is_string() {
        allocator.text(format!("{:?}", val.get_string().to_string()))
    } else if val.is_pair() {
        let mut doc = allocator.nil();
        let mut val = val;
        while val.is_pair() {
            let car = val.car();
            let cdr = val.cdr();
            if cdr.is_pair() {
                doc = doc.append(pretty_inner(car, allocator).append(allocator.space()));
            } else {
                doc = doc.append(pretty_inner(car, allocator));
            }
            val = cdr;
        }
        if !val.is_null() {
            doc = doc
                .append(allocator.space())
                .append(pretty_inner(val, allocator));
        }
        allocator.text("(").append(doc).append(allocator.text(")"))
    } else if val.is_vector() {
        let mut doc = allocator.nil();
        for (i, val) in val.get_vector().iter().enumerate() {
            if i > 0 {
                doc = doc.append(allocator.space());
            }
            doc = doc.append(pretty_inner(*val, allocator));
        }

        allocator.text("#(").append(doc).append(allocator.text(")"))
    } else if val.is_bytevector() {
        let mut doc = allocator.nil();
        for (i, val) in val.get_bytevector().iter().enumerate() {
            if i > 0 {
                doc = doc.append(allocator.space());
            }
            doc = doc.append(allocator.text(val.to_string()));
        }

        allocator
            .text("#u8(")
            .append(doc)
            .append(allocator.text(")"))
    } else {
        allocator.text("#<unknown>")
    }
}

impl std::fmt::Display for ScmValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let allocator = BoxAllocator;
        {

            pretty_inner(*self, &allocator).1.render_fmt(70, f)?;
        }
        Ok(())
    }
}