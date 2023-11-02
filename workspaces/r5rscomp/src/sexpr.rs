//! S-expression definition.
//!
//! Not only includes expressions but simulates interned/uninterned symbols to be
//! used in the macro expansion.

use std::hash::Hash;
use crate::env::Environment;
use crate::rc::Rc;
use crate::rc::Weak;
use crate::reader::SymbolInterner;

use num::BigInt;
use num::BigRational;
use num::Complex;
use num::Rational32;

#[derive(Clone)]
pub enum Symbol {
    Interned(String),
    Uninterned(String),
    Generated(Rc<Symbol>, Weak<Environment>),
}

impl Symbol {
    pub fn root_ref(&self) -> &Symbol {
        match self {
            Self::Interned(_) => self,
            Self::Uninterned(_) => self,
            Self::Generated(s, _) => s.root_ref(),
        }
    }

    pub fn root(this: &Rc<Symbol>) -> &Rc<Symbol> {
        match &**this {
            Self::Interned(_) => this,
            Self::Uninterned(_) => this,
            Self::Generated(s, _) => Self::root(s),
        }
    }

    pub fn lexical(&self) -> Option<(Rc<Symbol>, Rc<Environment>)> {
        match self {
            Self::Interned(_) => None,
            Self::Uninterned(_) => None,
            Self::Generated(s, env) => Some((s.clone(), env.upgrade().unwrap())),
        }
    
    }
}

impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        match self {
            Self::Interned(s) => s.as_str(),
            Self::Uninterned(s) => s.as_str(),
            Self::Generated(s, _) => (**s).as_ref(),
        }
    }
}

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Interned(s) => write!(f, "{:?}", s),
            Self::Uninterned(s) => write!(f, "{:?}", s),
            Self::Generated(s, _) => write!(f, "{:?}!", s),
        }
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Interned(s) => write!(f, "{}", s),
            Self::Uninterned(s) => write!(f, "{}", s),
            Self::Generated(s, _) => write!(f, "{}", s),
        }
    }

}

impl std::ops::Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self as *const _ == other as *const _
    }
}

impl Eq for Symbol {}

impl Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self as *const _ as usize)
    }
}

#[derive(Debug, Clone)]
pub enum Sexpr {
    Null,
    Undefined,
    Unspecified,
    Boolean(bool),
    Fixnum(i32),
    Flonum(f64),
    Character(char),
    String(Rc<String>),
    Symbol(Rc<Symbol>),
    Pair(Rc<(Sexpr, Sexpr)>),
    Vector(Rc<Vec<Sexpr>>),
    Bytevector(Rc<Vec<u8>>),
    Complex(Rc<Complex<f64>>),
    BigRational(Rc<BigRational>),
    Rational32(Rc<Rational32>),
    BigInt(Rc<BigInt>),
}

impl Sexpr {
    pub fn is_null(&self) -> bool {
        match self {
            Self::Null => true,
            _ => false,
        }
    }

    pub fn is_undefined(&self) -> bool {
        match self {
            Self::Undefined => true,
            _ => false,
        }
    }

    pub fn is_unspecified(&self) -> bool {
        match self {
            Self::Unspecified => true,
            _ => false,
        }
    }

    pub fn is_boolean(&self) -> bool {
        match self {
            Self::Boolean(_) => true,
            _ => false,
        }
    }

    pub fn is_fixnum(&self) -> bool {
        match self {
            Self::Fixnum(_) => true,
            _ => false,
        }
    }

    pub fn is_flonum(&self) -> bool {
        match self {
            Self::Flonum(_) => true,
            _ => false,
        }
    }

    pub fn is_character(&self) -> bool {
        match self {
            Self::Character(_) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Self::String(_) => true,
            _ => false,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self {
            Self::Symbol(_) => true,
            _ => false,
        }
    }

    pub fn symbol(&self) -> Option<&Rc<Symbol>> {
        match self {
            Self::Symbol(s) => Some(s),
            _ => None,
        }
    }

    pub fn is_pair(&self) -> bool {
        match self {
            Self::Pair(_) => true,
            _ => false,
        }
    }

    pub fn pair(&self) -> Option<&(Sexpr, Sexpr)> {
        match self {
            Self::Pair(p) => Some(p),
            _ => None,
        }
    }

    pub fn is_vector(&self) -> bool {
        match self {
            Self::Vector(_) => true,
            _ => false,
        }
    }

    pub fn is_bytevector(&self) -> bool {
        match self {
            Self::Bytevector(_) => true,
            _ => false,
        }
    }

    pub fn is_complex(&self) -> bool {
        match self {
            Self::Complex(_) => true,
            _ => false,
        }
    }

    pub fn is_bigrational(&self) -> bool {
        match self {
            Self::BigRational(_) => true,
            _ => false,
        }
    }

    pub fn is_rational32(&self) -> bool {
        match self {
            Self::Rational32(_) => true,
            _ => false,
        }
    }

    pub fn is_bigint(&self) -> bool {
        match self {
            Self::BigInt(_) => true,
            _ => false,
        }
    }

    pub fn is_proper_list(&self) -> bool {
        match self {
            Self::Null => true,
            Self::Pair(p) => p.1.is_proper_list(),
            _ => false,
        }
    }

    pub fn is_improper_list(&self) -> bool {
        match self {
            Self::Pair(p) => !p.1.is_proper_list(),
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            Self::Null => true,
            Self::Pair(p) => p.1.is_list(),
            _ => false,
        }
    }

    pub fn list_length(&self) -> Option<usize> {
        match self {
            Self::Null => Some(0),
            Self::Pair(p) => p.1.list_length().map(|n| n + 1),
            _ => None,
        }
    }
    

    pub fn is_vector_like(&self) -> bool {
        match self {
            Self::Vector(_) => true,
            Self::Bytevector(_) => true,
            _ => false,
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Self::Fixnum(_) => true,
            Self::Flonum(_) => true,
            Self::Complex(_) => true,
            Self::BigRational(_) => true,
            Self::Rational32(_) => true,
            Self::BigInt(_) => true,
            _ => false,
        }
    }

    pub fn is_exact_integer(&self) -> bool {
        match self {
            Self::Fixnum(_) => true,
            Self::BigInt(_) => true,
            _ => false,
        }
    }

    pub fn is_exact_rational(&self) -> bool {
        match self {
            Self::Fixnum(_) => true,
            Self::BigInt(_) => true,
            Self::BigRational(_) => true,
            Self::Rational32(_) => true,
            _ => false,
        }
    }

    pub fn is_real(&self) -> bool {
        match self {
            Self::Fixnum(_) => true,
            Self::BigInt(_) => true,
            Self::BigRational(_) => true,
            _ => false,
        }
    }

    pub fn car(&self) -> &Sexpr {
        match self {
            Self::Pair(p) => &p.0,
            _ => panic!("car called on non-pair"),
        }
    }

    pub fn cdr(&self) -> &Sexpr {
        match self {
            Self::Pair(p) => &p.1,
            _ => panic!("cdr called on non-pair"),
        }
    }

    pub fn cadr(&self) -> &Sexpr {
        match self {
            Self::Pair(p) => p.1.car(),
            _ => panic!("cadr called on non-pair"),
        }
    }

    pub fn cddr(&self) -> &Sexpr {
        match self {
            Self::Pair(p) => p.1.cdr(),
            _ => panic!("cddr called on non-pair"),
        }
    }

    pub fn cdar(&self) -> &Sexpr {
        match self {
            Self::Pair(p) => p.0.cdr(),
            _ => panic!("cdar called on non-pair"),
        }
    }

    pub fn caar(&self) -> &Sexpr {
        match self {
            Self::Pair(p) => p.0.car(),
            _ => panic!("caar called on non-pair"),
        }
    }

    pub fn caddr(&self) -> &Sexpr {
        match self {
            Self::Pair(p) => p.1.cadr(),
            _ => panic!("caddr called on non-pair"),
        }
    }

    pub fn cdddr(&self) -> &Sexpr {
        match self {
            Self::Pair(p) => p.1.cddr(),
            _ => panic!("cdddr called on non-pair"),
        }
    }

    pub fn equal(&self, other: &Self) -> bool {
        if self == other {
            return true;
        }
        match (self, other) {
            (Sexpr::Flonum(x), Sexpr::Flonum(y)) => x == y,
            (Sexpr::BigInt(x), Sexpr::BigInt(y)) => x == y,
            (Sexpr::BigRational(x), Sexpr::BigRational(y)) => x == y,
            (Sexpr::Rational32(x), Sexpr::Rational32(y)) => x == y,
            (Sexpr::Bytevector(x), Sexpr::Bytevector(y)) => x == y,
            (Sexpr::Vector(x), Sexpr::Vector(y)) => {
                if x.len() != y.len() {
                    return false;
                }

                for (x, y) in x.iter().zip(y.iter()) {
                    if !x.equal(y) {
                        return false;
                    }
                }

                true
            }

            (Sexpr::String(x), Sexpr::String(y)) => x == y,
            (Sexpr::Complex(x), Sexpr::Complex(y)) => x == y,
            (Sexpr::Pair(x), Sexpr::Pair(y)) => {
                if !x.0.equal(&y.0) {
                    return false;
                }

                if !x.1.equal(&y.1) {
                    return false;
                }

                true
            }

            _ => false
        }
    }

    pub fn make_list(exprs: &[Sexpr]) -> Self {
        let mut list = Self::Null;
        for expr in exprs.iter().rev() {
            list = Self::Pair(Rc::new((expr.clone(), list)));
        }
        list
    }

    pub fn make_list_star(exprs: &[Sexpr], rest: Sexpr) -> Self {
        let mut list = rest;
        for expr in exprs.iter().rev() {
            list = Self::Pair(Rc::new((expr.clone(), list)));
        }
        list
    }

    pub fn assq(&self, key: &Sexpr) -> Option<&Sexpr> {
        match self {
            Self::Pair(p) => {
                if &p.0 == key {
                    Some(&p.1)
                } else {
                    p.1.assq(key)
                }
            }

            _ => None,
        }
    }

    pub fn reverse(&self) -> Self {
        let mut list = Self::Null;
        let mut rest = self;

        while let Self::Pair(p) = rest {
            list = Self::Pair(Rc::new((p.0.clone(), list)));
            rest = &p.1;
        }

        list
    }

    pub fn append(&self, other: &Self) -> Self {
        match self {
            Self::Null => other.clone(),
            Self::Pair(p) => Self::Pair(Rc::new((p.0.clone(), p.1.append(other)))),
            _ => panic!("append called on non-list"),
        }
    }
}

impl PartialEq for Sexpr {
    /// Compares Sexpr by reference. For full comparison use [`Sexpr::equal`]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Sexpr::Null, Sexpr::Null) => true,
            (Sexpr::Undefined, Sexpr::Undefined) => true,
            (Sexpr::Unspecified, Sexpr::Unspecified) => true,
            (Sexpr::Fixnum(x), Sexpr::Fixnum(y)) => x == y,
            (Sexpr::Flonum(x), Sexpr::Flonum(y)) => x.to_bits() == y.to_bits(),
            (Sexpr::Character(c), Sexpr::Character(c2)) => c == c2,
            (Sexpr::String(s1), Sexpr::String(s2)) => Rc::ptr_eq(s1, s2),
            (Sexpr::Symbol(s1), Sexpr::Symbol(s2)) => Rc::ptr_eq(s1, s2),
            (Sexpr::Pair(x), Sexpr::Pair(y)) => Rc::ptr_eq(x, y),
            (Sexpr::Vector(x), Sexpr::Vector(y)) => Rc::ptr_eq(x, y),
            (Sexpr::Bytevector(x), Sexpr::Bytevector(y)) => Rc::ptr_eq(x, y),
            (Sexpr::Complex(x), Sexpr::Complex(y)) => Rc::ptr_eq(x, y),
            (Sexpr::BigRational(x), Sexpr::BigRational(y)) => Rc::ptr_eq(x, y),
            (Sexpr::Rational32(x), Sexpr::Rational32(y)) => Rc::ptr_eq(x, y),
            (Sexpr::BigInt(x), Sexpr::BigInt(y)) => Rc::ptr_eq(x, y),
            _ => false,
        }
    }
}

impl Eq for Sexpr {}

impl Hash for Sexpr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Sexpr::Null => {
                state.write_u8(0);
            }

            Sexpr::Undefined => {
                state.write_u8(1);
            }

            Sexpr::Unspecified => {
                state.write_u8(2);
            }

            Sexpr::Boolean(b) => {
                state.write_u8(3);
                state.write_u8(*b as u8);
            }

            Sexpr::Fixnum(n) => {
                state.write_u8(4);
                state.write_i32(*n);
            }

            Sexpr::Flonum(f) => {
                state.write_u8(5);
                state.write_u64(f.to_bits());
            }

            Sexpr::Character(c) => {
                state.write_u8(6);
                state.write_u32(*c as u32);
            }

            Sexpr::String(s) => {
                state.write_u8(7);
                state.write_usize(Rc::as_ptr(s) as usize);
            }

            Sexpr::Symbol(s) => {
                state.write_u8(8);
                state.write_usize(Rc::as_ptr(s) as usize);
            }

            Sexpr::Pair(p) => {
                state.write_u8(9);
                state.write_usize(Rc::as_ptr(p) as usize);
            }

            Sexpr::Vector(v) => {
                state.write_u8(10);
                state.write_usize(Rc::as_ptr(v) as usize);
            }

            Sexpr::Bytevector(b) => {
                state.write_u8(11);
                state.write_usize(Rc::as_ptr(b) as usize);
            }

            Sexpr::Complex(c) => {
                state.write_u8(12);
                state.write_usize(Rc::as_ptr(c) as usize);
            }

            Sexpr::BigRational(r) => {
                state.write_u8(13);
                state.write_usize(Rc::as_ptr(r) as usize);
            }

            Sexpr::Rational32(r) => {
                state.write_u8(14);
                state.write_usize(Rc::as_ptr(r) as usize);
            }

            Sexpr::BigInt(i) => {
                state.write_u8(15);
                state.write_usize(Rc::as_ptr(i) as usize);
            }
        }
    }
}

use pretty::BoxAllocator;
use pretty::{DocAllocator, DocBuilder};
use r7rs_parser::expr::Expr;
use r7rs_parser::expr::NoIntern;
use termcolor::ColorSpec;
use termcolor::WriteColor;

impl Sexpr {
    pub fn pretty<'a, D>(&self, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        match self {
            Self::Null => allocator.text("()"),

            Self::Undefined => allocator.text("#<undefined>"),

            Self::Unspecified => allocator.text("#<unspecified>"),

            Self::Boolean(b) => {
                if *b {
                    allocator.text("#t")
                } else {
                    allocator.text("#f")
                }
            }

            Self::Fixnum(n) => allocator.text(n.to_string()),

            Self::Flonum(f) => allocator.text(f.to_string()),

            Self::Character(c) => allocator.text(format!("#\\{}", c)),

            Self::String(s) => allocator.text(format!("{:?}", s)),

            Self::Symbol(s) => allocator.text(format!("{:?}", s)),

            Self::Pair(p) => {
                let mut doc = allocator.nil();
                let mut p = Some(&**p);

                while let Some((car, cdr)) = p {
                    doc = doc.append(car.pretty(allocator));
                    if let Self::Pair(cdr) = &*cdr {
                        doc = doc.append(allocator.text(" "));
                        p = Some(cdr);
                    } else if cdr.is_null() {
                        break;
                    } else {
                        doc = doc.append(allocator.text(" . "));
                        doc = doc.append(cdr.pretty(allocator));
                        break;
                    }
                }

                doc.parens().group().align()
            }

            Self::Vector(v) => {
                let elems =
                    allocator.intersperse(v.iter().map(|x| x.pretty(allocator)), allocator.space());

                let mut doc = allocator.text("#");
                doc = doc.append(elems.parens());

                doc.group().align()
            }

            Self::Bytevector(b) => {
                let elems = allocator.intersperse(
                    b.iter().map(|x| allocator.text(x.to_string())),
                    allocator.space(),
                );
                let mut doc = allocator.text("#u8");
                doc = doc.append(elems.parens());
                doc
            }

            Self::Complex(c) => allocator.text(format!("{}", c)),

            Self::BigRational(r) => allocator.text(format!("{}", r)),

            Self::Rational32(r) => allocator.text(format!("{}", r)),

            Self::BigInt(i) => allocator.text(format!("{}", i)),
        }
    }

    pub fn pretty_print(&self, out: impl WriteColor) -> std::io::Result<()> {
        let allocator = BoxAllocator;

        self.pretty(&allocator).1.render_colored(70, out)?;

        Ok(())
    }
}



#[derive(Debug, Clone)]
pub enum ScmError {
    MismatchedRepetitionPatterns(Sexpr),
    UnexpectedType(Sexpr, &'static str),
    NoMatchingPattern(Sexpr, Sexpr),
    DuplicateBinding(Sexpr),
    Custom(Sexpr, String),
}

pub fn cons(car: Sexpr, cdr: Sexpr) -> Sexpr {
    Sexpr::Pair(Rc::new((car, cdr)))
}


pub fn from_r7rs_parser(e: &Expr<NoIntern>, interner: &Rc<SymbolInterner>) -> Sexpr {
    match e {
        Expr::BigInt(n) => Sexpr::BigInt(Rc::new(n.clone())),
        Expr::BigRational(n) => Sexpr::BigRational(Rc::new(n.clone())),
        Expr::Bool(b) => Sexpr::Boolean(*b),
        Expr::Fixnum(n) => Sexpr::Fixnum(*n),
        Expr::ByteVector(b) => Sexpr::Bytevector(Rc::new(b.to_vec())),
        Expr::Char(c) => Sexpr::Character(*c),
        Expr::Complex(r) => Sexpr::Complex(Rc::new(r.clone())),
        Expr::Float(r) => Sexpr::Flonum(*r),
        Expr::GrowableVector(v) | Expr::ImmutableVector(v) => {
            let mut elems = Vec::new();
            for e in v.iter() {
                elems.push(from_r7rs_parser(e, interner));
            }
            Sexpr::Vector(Rc::new(elems))
        }

        Expr::Null => Sexpr::Null,
        Expr::Rational(r) => Sexpr::Rational32(Rc::new(r.clone())),
        Expr::Pair(car, cdr) => cons(from_r7rs_parser(car, interner), from_r7rs_parser(cdr, interner)),
        Expr::Symbol(s) => Sexpr::Symbol(interner.intern(s)),
        Expr::Syntax(_, expr) => from_r7rs_parser(expr, interner),
        Expr::Str(s) => Sexpr::String(Rc::new(s.clone())),
    }
}

impl std::fmt::Display for Sexpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut vec = Vec::new();
        let mut out = termcolor::NoColor::new(&mut vec);
        match self.pretty_print(&mut out) {
            Ok(_) => {
                f.write_str(std::str::from_utf8(&vec).unwrap())
            },
            Err(_) => Err(std::fmt::Error),
        }
    }
}