//! Constant-folding table
//!
//!
//! This file defines mapping from procedure name to constant folding function.

use std::collections::HashMap;

use once_cell::sync::Lazy;

use super::sexpr::Sexpr;

macro_rules! table {
    ($table_name: ident => $($name: literal, ($($predicate: ident),*) => ($args: ident) $b: block)*) => {
        pub fn $table_name() -> Vec<FoldingEntry> {
            vec![$(FoldingEntry {
                name: $name,
                predicates: &[$(Sexpr::$predicate),*],
                fold: {
                    fn f($args: &[Sexpr]) -> Option<Sexpr> $b
                    f
                }

            }),*]
        }
    };
}

table! {
    minimal_constant_folding_table =>
        "fixnum?", (always) => (args) {
            if args[0].is_fixnum() {
                Some(Sexpr::Boolean(true))
            } else {
                Some(Sexpr::Boolean(false))
            }
        }
        "flonum?", (always) => (args) {
            if args[0].is_number() {
                Some(Sexpr::Boolean(true))
            } else {
                Some(Sexpr::Boolean(false))
            }
        }

        "number?", (always) => (args) {
            if args[0].is_number() {
                Some(Sexpr::Boolean(true))
            } else {
                Some(Sexpr::Boolean(false))
            }
        }

        "char?", (always) => (args) {
            Some(Sexpr::Boolean(matches!(args[0], Sexpr::Char(_))))
        }
        "string?", (always) => (args) {
            Some(Sexpr::Boolean(matches!(args[0], Sexpr::String(_))))
        }

        "symbol?", (always) => (args) {
            Some(Sexpr::Boolean(matches!(args[0], Sexpr::Symbol(_))))
        }
        "char->integer", (is_char) => (args) {
            let Sexpr::Char(c) = args[0] else {
                return None;
            };

            Some(Sexpr::Fixnum(c as i32))
        }

        "integer->char", (is_charcode) => (args) {
            let Sexpr::Fixnum(ix) = args[0] else{
                return None;
            };

            Some(Sexpr::Char(char::from_u32(ix as u32).unwrap()))
        }

        ".car:pair", (is_pair) => (args) {
            Some(args[0].car())
        }

        ".cdr:pair", (is_pair) => (args) {
            Some(args[0].cdr())
        }

}

table! {
    extended_constant_folding_table =>
    "zero?", (is_number) => (args) {
        match args[0] {
            Sexpr::Fixnum(x) => Some(Sexpr::Boolean(x == 0)),
            Sexpr::Flonum(x) => Some(Sexpr::Boolean(x == 0.0)),
            _ => None
        }
    }

    "+", (is_number, is_number) => (args) {
        match (&args[0], &args[1]) {
            (Sexpr::Fixnum(x), Sexpr::Fixnum(y)) => {
                if let Some(res) = x.checked_add(*y) {
                    Some(Sexpr::Fixnum(res))
                } else {
                    None
                }
            }

            (Sexpr::Flonum(x), Sexpr::Flonum(y)) => {
                Some(Sexpr::Flonum(*x + *y))
            }

            (Sexpr::Fixnum(x), Sexpr::Flonum(y)) => {
                Some(Sexpr::Flonum(*x as f64 + y))
            }

            (Sexpr::Flonum(x), Sexpr::Fixnum(y)) => {
                Some(Sexpr::Flonum(x + *y as f64))
            }

            _ => None
        }
    }

    "-", (is_number, is_number) => (args) {
        match (&args[0], &args[1]) {
            (Sexpr::Fixnum(x), Sexpr::Fixnum(y)) => {
                if let Some(res) = x.checked_sub(*y) {
                    Some(Sexpr::Fixnum(res))
                } else {
                    None
                }
            }

            (Sexpr::Flonum(x), Sexpr::Flonum(y)) => {
                Some(Sexpr::Flonum(*x - *y))
            }

            (Sexpr::Fixnum(x), Sexpr::Flonum(y)) => {
                Some(Sexpr::Flonum(*x as f64 - y))
            }

            (Sexpr::Flonum(x), Sexpr::Fixnum(y)) => {
                Some(Sexpr::Flonum(x - *y as f64))
            }

            _ => None
        }
    }

    "quotient", (is_number, is_number) => (args) {
        match (&args[0], &args[1]) {
            (Sexpr::Fixnum(x), Sexpr::Fixnum(y)) => {
                if let Some(res) = x.checked_div(*y) {
                    Some(Sexpr::Fixnum(res))
                } else {
                    None
                }
            }

            (Sexpr::Flonum(x), Sexpr::Flonum(y)) => {
                Some(Sexpr::Flonum(*x / *y))
            }

            (Sexpr::Fixnum(x), Sexpr::Flonum(y)) => {
                Some(Sexpr::Flonum(*x as f64 / y))
            }

            (Sexpr::Flonum(x), Sexpr::Fixnum(y)) => {
                Some(Sexpr::Flonum(x / *y as f64))
            }

            _ => None
        }
    }
    "*", (is_number, is_number) => (args) {
        match (&args[0], &args[1]) {
            (Sexpr::Fixnum(x), Sexpr::Fixnum(y)) => {
                if let Some(res) = x.checked_mul(*y) {
                    Some(Sexpr::Fixnum(res))
                } else {
                    None
                }
            }

            (Sexpr::Flonum(x), Sexpr::Flonum(y)) => {
                Some(Sexpr::Flonum(*x * *y))
            }

            (Sexpr::Fixnum(x), Sexpr::Flonum(y)) => {
                Some(Sexpr::Flonum(*x as f64 * y))
            }

            (Sexpr::Flonum(x), Sexpr::Fixnum(y)) => {
                Some(Sexpr::Flonum(x * *y as f64))
            }

            _ => None
        }
    }
}

pub fn usual_constant_folding_table() -> Vec<FoldingEntry> {
    minimal_constant_folding_table()
        .into_iter()
        .chain(extended_constant_folding_table().into_iter())
        .collect()
}

pub struct FoldingEntry {
    pub name: &'static str,
    pub predicates: &'static [fn(&Sexpr) -> bool],
    pub fold: fn(&[Sexpr]) -> Option<Sexpr>,
}

impl Sexpr {
    pub fn is_fixnum(&self) -> bool {
        match self {
            Self::Fixnum(_) => true,
            _ => false,
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Self::Fixnum(_) => true,
            Self::Flonum(_) => true,
            _ => false,
        }
    }

    pub fn is_exact(&self) -> bool {
        match self {
            Self::Fixnum(_) => true,
            _ => false,
        }
    }

    pub fn always(&self) -> bool {
        true
    }

    pub fn is_char(&self) -> bool {
        matches!(self, Self::Char(_))
    }

    pub fn is_charcode(&self) -> bool {
        match self {
            Self::Fixnum(x) => char::from_u32(*x as u32).is_some(),
            _ => false,
        }
    }
}

pub static USUAL_CONSTANT_FOLDING_TABLE: Lazy<HashMap<&'static str, FoldingEntry>> = Lazy::new(|| {
    let mut table = HashMap::new();
    for entry in usual_constant_folding_table() {
        table.insert(entry.name, entry);
    }
    table
});