//! Open-coding primitive procedures

use crate::runtime::object::scm_symbol_str;

use super::{tree_il::IForm, P};

pub const TRANSPARENT_PRIMITIVE_NAMES: &[&str] = &[
    "vector",
    "cons",
    "pair?",
    "number?",
    "vector?",
    "string?",
    "symbol?",
    "char?",
    "boolean?",
    "procedure?",
    "null?",
    "list?",
    "eq?",
    "eqv?",
    "equal?",
    "not",
];

pub const INTERESTING_PRIMTIIVE_NAMES: &[&str] = &[
    "call-with-values",
    "call-with-current-continuation",
    "call/cc",
    "dynamic-wind",
    "eq?",
    "eqv?",
    "equal?",
    "memv",
    "memq",
    "=",
    ">",
    "<",
    "<=",
    ">=",
    "zero?",
    "positive?",
    "negative?",
    "+",
    "*",
    "-",
    "--",
    "/",
    "1-",
    "1+",
    "quotient",
    "remainder",
    "modulu",
    "exact->inexact",
    "expt",
    "ash",
    "logand",
    "logior",
    "logxor",
    "lognot",
    "logtest",
    "logbit?",
    "sqrt",
    "abs",
    "floor",
    "ceiling",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "not",
    "pair?",
    "null?",
    "list?",
    "symbol?",
    "vector?",
    "string?",
    "struct?",
    "number?",
    "char?",
    "undefined?",
    "undefined",
    "eof-object?",
    "bytevector?",
    "symbol->string",
    "string->symbol",
    "procedure?",
    "complex?",
    "real?",
    "rational?",
    "integer?",
    "inf?",
    "nan?",
    "exact?",
    "ineact?",
    "even?",
    "odd?",
    "exact-integer?",
    "char<?",
    "char<=?",
    "char>=?",
    "char>?",
    "integer->char",
    "char->integer",
    "number->string",
    "string->number",
    "acons",
    "cons",
    "cons*",
    "list",
    "vector",
    "car",
    "cdr",
    ".car:pair",
    ".cdr:pair",
    "set-car!",
    "set-cdr!",
    "make-box",
    "box-ref",
    "box-set!",
    "caar",
    "cadr",
    "cdar",
    "cddr",
    "caaar",
    "caadr",
    "cadar",
    "caddr",
    "cdaar",
    "cdadr",
    "cddar",
    "cdddr",
    "caaaar",
    "caaadr",
    "caadar",
    "caaddr",
    "cadaar",
    "cadadr",
    "caddar",
    "cadddr",
    "cdaaar",
    "cdaadr",
    "cdadar",
    "cdaddr",
    "cddadr",
    "cdddar",
    "cddddr",
    "length",
    "make-vector",
    "vector-length",
    "vector-ref",
    "vector-set!",
    "throw",
    "error",
    "scm-error",
    "string-length",
    "string-ref",
    "string-set!",
    "make-struct/simple",
    "struct-vtable",
    "struct-ref",
    "struct-set!",
    "bytevector-length",
    "bytevector-u8-ref",
    "bytevector-u8-set!",
    "bytevector-s8-ref",
    "bytevector-s8-set!",
    "bytevector-u16-ref",
    "bytevector-u16-set!",
    "bytevector-s16-ref",
    "bytevector-s16-set!",
    "bytevector-u32-ref",
    "bytevector-u32-set!",
    "bytevector-s32-ref",
    "bytevector-s32-set!",
    "bytevector-u64-ref",
    "bytevector-u64-set!",
    "bytevector-s64-ref",
    "bytevector-s64-set!",
    "bytevector-ieee-single-ref",
    "bytevector-ieee-single-set!",
    "bytevector-ieee-double-ref",
    "bytevector-ieee-double-set!",
];

pub const PRIMITIVE_ACCESSORS: &[&str] = &[
    "vector-ref",
    "car",
    "cdr",
    "memv",
    "memq",
    "struct-ref",
    "string-ref",
    "bytevector-u8-ref",
    "bytevector-s8-ref",
    "bytevector-u16-ref",
    "bytevector-s16-ref",
    "bytevector-u32-ref",
    "bytevector-s32-ref",
    "bytevector-u64-ref",
    "bytevector-s64-ref",
    "bytevector-ieee-single-ref",
    "bytevector-ieee-double-ref",
];

pub fn resolve_primitives(mut exp: P<IForm>) -> P<IForm> {
    match &mut *exp {
        IForm::LRef(_) => exp,
        IForm::GRef(gref) => {
            let name = gref.name.unwrap_id();
            let str = scm_symbol_str(name);

            if let Some(name) = INTERESTING_PRIMTIIVE_NAMES.iter().find(|&&n| n == str) {
                P(IForm::PrimRef(*name))
            } else {
                exp
            }
        }

        IForm::Call(call) => {
            call.proc = resolve_primitives(call.proc.clone());
            for arg in &mut call.args {
                *arg = resolve_primitives(arg.clone());
            }

            if let IForm::PrimRef(name) = &*call.proc {
                return P(IForm::PrimCall(call.src, *name, call.args.clone()));
            }

            exp
        }
        IForm::Seq(seq) => {
            for exp in seq.forms.iter_mut() {
                *exp = resolve_primitives(exp.clone());
            }

            exp
        }

        IForm::GSet(gset) => {
            gset.value = resolve_primitives(gset.value.clone());
            exp
        }

        IForm::LSet(lset) => {
            lset.value = resolve_primitives(lset.value.clone());
            exp
        }

        IForm::Define(def) => {
            def.value = resolve_primitives(def.value.clone());
            exp
        }

        IForm::If(cond) => {
            cond.cond = resolve_primitives(cond.cond.clone());
            cond.consequent = resolve_primitives(cond.consequent.clone());
            cond.alternative = resolve_primitives(cond.alternative.clone());
            exp
        }

        IForm::Label(label) => {
            label.body = resolve_primitives(label.body.clone());
            exp
        }

        IForm::Let(var) => {
            var.inits.iter_mut().for_each(|init| {
                *init = resolve_primitives(init.clone());
            });
            var.body = resolve_primitives(var.body.clone());
            exp
        }

        IForm::Lambda(lam) => {
            lam.body = resolve_primitives(lam.body.clone());
            exp
        }

        IForm::Fix(fix) => {
            for rhs in fix.rhs.iter_mut() {
                rhs.body = resolve_primitives(rhs.body.clone());
            }
            fix.body = resolve_primitives(fix.body.clone());
            exp
        }

        _ => exp,
    }
}
