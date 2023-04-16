//! Compiler from Scheme to CPS Intermediate Language.
//! 
//! Code is converted from: 
//! ```scheme
//! (define (square n) (* n n))
//! (display (square 5))
//! ```
//! 
//! Into the following CPS IL:
//! ```
//! (define square #<void>)
//! 
//! (set! square (lambda (k n) (k (* n n))))
//! 
//! (square (lambda (x) (display x)) 5)
//! ```

pub mod il;
pub mod anf;
pub mod desugar;

use std::collections::HashSet;

use once_cell::sync::Lazy;

pub const PRIMITIVE_LIST: Lazy<HashSet<&'static str>> = Lazy::new(|| {
    HashSet::from_iter([
        "+",
        "-",
        "*",
        "/",
        "abs",
        "acos",
        "angle",
        "append",
        "asin",
        "assoc",
        "assq",
        "assv",
        "atan",
        "boolean?",
        "call-with-current-continuation",
        "call/cc",
        "car",
        "cdr",
        "ceiling",
        "char->integer",
        "char-alphabetic?",
        "char-ci<=?",
        "char-ci<?",
        "char-ci=?",
        "char-ci>=?",
        "char-ci>?",
        "char-downcase",
        "char-lower-case?",
        "char-numeric?",
        "char-ready?",
        "char-upcase",
        "char-upper-case?",
        "char-whitespace?",
        "char<=?",
        "char<?",
        "char=?",
        "char>=?",
        "char>?",
        "char?",
        "close-input-port",
        "close-output-port",
        "complex?",
        "cons",
        "cos",
        "current-input-port",
        "current-output-port",
        "display",
        "eof-object?",
        "eq?",
        "equal?",
        "eqv?",
        ">",
        ">=",
        "=",
        "even?",
        "<",
        "<=",
        "length",
        "list",
        "list?",
        "log",
        "magnitude",
        "make-bytevector",
        "make-vector",
        "vector",
        "vector?",
        "bytevector?",
        "vector-length",
        "vector-ref",
        "vector-set!",
        "bytevector-length",
        "bytevector-u8-ref",
        "bytevector-u8-set!",
        "bytevector->list",
        "list->bytevector",
        "vector->list",
        "list->vector",

    ])
});