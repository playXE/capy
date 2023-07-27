use crate::runtime::{object::scm_symbol_str, symbol::scm_intern, value::Value};

use super::{
    tree_il::{IForm, LVar},
    Cenv, SyntaxEnv, P,
};

#[derive(Debug, Clone)]
pub enum Sexpr {
    Null,
    Undefined,
    Symbol(Value),
    Gensym(P<String>),
    Fixnum(i32),
    Char(char),
    Flonum(f64),
    Boolean(bool),
    String(P<String>),
    Pair(P<(Sexpr, Sexpr)>),
    Vector(P<Vec<Sexpr>>),
    Bytevector(P<Vec<u8>>),
    Identifier(P<Identifier>),

    SyntaxRules(P<SyntaxRules>),
    SyntaxPattern(P<SyntaxPattern>),
    PVRef(PVRef),
    Special(fn(Sexpr, &Cenv) -> Result<P<IForm>, String>),
    LVar(P<LVar>),
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: Sexpr,
    pub frames: Sexpr,
    pub env: P<SyntaxEnv>,
}

#[derive(Debug, Clone)]
pub struct SyntaxRules {
    pub name: Sexpr,
    pub max_num_pvars: u32,
    pub env: Sexpr,
    pub syntax_env: P<SyntaxEnv>,
    pub rules: Vec<SyntaxRuleBranch>,
}

#[derive(Debug, Clone)]
pub struct SyntaxRuleBranch {
    pub pattern: Sexpr,
    pub template: Sexpr,
    pub num_pvars: u32,
    pub max_level: u32,
}

#[derive(Debug, Clone)]
pub struct SyntaxPattern {
    pub pattern: Sexpr,
    pub vars: Sexpr,
    pub level: i16,
    pub num_following_items: i16,
}

#[derive(Debug, Clone)]
pub struct PVRef {
    pub level: i16,
    pub count: i16,
}

/// Compares on stack values by value, on heap values by pointer equality.
pub fn sexp_eq(x: &Sexpr, y: &Sexpr) -> bool {
    match (&x, &y) {
        (Sexpr::Null, Sexpr::Null) => true,
        (Sexpr::Undefined, Sexpr::Undefined) => true,
        (Sexpr::Symbol(x), Sexpr::Symbol(y)) => x == y,
        (Sexpr::Fixnum(x), Sexpr::Fixnum(y)) => x == y,
        (Sexpr::Char(x), Sexpr::Char(y)) => x == y,
        (Sexpr::Flonum(x), Sexpr::Flonum(y)) => x == y,
        (Sexpr::Boolean(x), Sexpr::Boolean(y)) => x == y,
        (Sexpr::String(x), Sexpr::String(y)) => x.as_ptr() == y.as_ptr(),
        (Sexpr::Pair(x), Sexpr::Pair(y)) => x.as_ptr() == y.as_ptr(),
        (Sexpr::Vector(x), Sexpr::Vector(y)) => x.as_ptr() == y.as_ptr(),
        (Sexpr::Bytevector(x), Sexpr::Bytevector(y)) => x.as_ptr() == y.as_ptr(),
        _ => false,
    }
}

pub fn sexp_equal(x: &Sexpr, y: &Sexpr) -> bool {
    match (&x, &y) {
        (Sexpr::Null, Sexpr::Null) => true,
        (Sexpr::Undefined, Sexpr::Undefined) => true,
        (Sexpr::Symbol(x), Sexpr::Symbol(y)) => x == y,
        (Sexpr::Fixnum(x), Sexpr::Fixnum(y)) => x == y,
        (Sexpr::Char(x), Sexpr::Char(y)) => x == y,
        (Sexpr::Flonum(x), Sexpr::Flonum(y)) => x == y,
        (Sexpr::Boolean(x), Sexpr::Boolean(y)) => x == y,
        (Sexpr::String(x), Sexpr::String(y)) => &**x == &**y,
        (Sexpr::Pair(x), Sexpr::Pair(y)) => sexp_equal(&x.0, &y.0) && sexp_equal(&x.1, &y.1),
        (Sexpr::Vector(x), Sexpr::Vector(y)) => {
            if x.len() != y.len() {
                return false;
            }
            for (x, y) in x.iter().zip(y.iter()) {
                if !sexp_equal(x, y) {
                    return false;
                }
            }
            true
        }
        (Sexpr::Bytevector(x), Sexpr::Bytevector(y)) => {
            if x.len() != y.len() {
                return false;
            }
            for (x, y) in x.iter().zip(y.iter()) {
                if x != y {
                    return false;
                }
            }
            true
        }
        _ => false,
    }
}

pub fn sexp_cons(car: Sexpr, cdr: Sexpr) -> Sexpr {
    Sexpr::Pair(P::new((car, cdr)))
}

pub fn sexp_acons(caar: Sexpr, cdar: Sexpr, cdr: Sexpr) -> Sexpr {
    sexp_cons(sexp_cons(caar, cdar), cdr)
}

impl Sexpr {
    pub fn cons(car: Sexpr, cdr: Sexpr) -> Self {
        sexp_cons(car, cdr)
    }

    pub fn car(&self) -> Sexpr {
        match &self {
            Sexpr::Pair(x) => x.0.clone(),
            _ => panic!("car: not a pair"),
        }
    }

    pub fn cdr(&self) -> Sexpr {
        match &self {
            Sexpr::Pair(x) => x.1.clone(),
            _ => panic!("cdr: not a pair"),
        }
    }

    pub fn set_car(&mut self, car: Sexpr) {
        match self {
            Sexpr::Pair(x) => x.0 = car,
            _ => panic!("set-car!: not a pair"),
        }
    }

    pub fn set_cdr(&mut self, cdr: Sexpr) {
        match self {
            Sexpr::Pair(x) => x.1 = cdr,
            _ => panic!("set-cdr!: not a pair"),
        }
    }

    pub fn caar(&self) -> Sexpr {
        self.car().car()
    }

    pub fn cadr(&self) -> Sexpr {
        self.cdr().car()
    }

    pub fn cdar(&self) -> Sexpr {
        self.car().cdr()
    }

    pub fn cddr(&self) -> Sexpr {
        self.cdr().cdr()
    }

    pub fn caddr(&self) -> Sexpr {
        self.cdr().cdr().car()
    }

    pub fn cdddr(&self) -> Sexpr {
        self.cdr().cdr().cdr()
    }

    pub fn cddar(&self) -> Sexpr {
        self.cdr().cdr().car()
    }

    pub fn cdaar(&self) -> Sexpr {
        self.cdr().car().car()
    }

    pub fn caaar(&self) -> Sexpr {
        self.car().car().car()
    }

    pub fn caadr(&self) -> Sexpr {
        self.car().cdr().car()
    }

    pub fn cadddr(&self) -> Sexpr {
        self.cdr().cdr().cdr().car()
    }

    pub fn null() -> Sexpr {
        Sexpr::Null
    }

    pub fn list(exprs: &[Sexpr]) -> Sexpr {
        let mut res = Sexpr::null();
        for expr in exprs.iter().rev() {
            res = Sexpr::cons(expr.clone(), res);
        }
        res
    }

    pub fn list_star(exprs: &[Sexpr]) -> Sexpr {
        let mut res = exprs.last().cloned().unwrap_or(Sexpr::null());
        for expr in exprs.iter().rev().skip(1) {
            res = Sexpr::cons(expr.clone(), res);
        }
        res
    }

    pub fn list_from_iter<I: DoubleEndedIterator<Item = Sexpr>>(iter: I) -> Sexpr {
        let mut res = Sexpr::null();
        for expr in iter.rev() {
            res = Sexpr::cons(expr, res);
        }
        res
    }

    pub fn list_to_vec(&self) -> Vec<Sexpr> {
        let mut res = vec![];
        let mut list = self.clone();
        while let Sexpr::Pair(x) = list {
            res.push(x.0.clone());
            list = x.1.clone();
        }
        res
    }

    pub fn list_for_each(&self, mut f: impl FnMut(&Sexpr)) {
        let mut list = self.clone();
        while let Sexpr::Pair(x) = list {
            f(&x.0);
            list = x.1.clone();
        }
    }

    pub fn is_null(&self) -> bool {
        match self {
            Sexpr::Null => true,
            _ => false,
        }
    }

    pub fn is_pair(&self) -> bool {
        match self {
            Sexpr::Pair(_) => true,
            _ => false,
        }
    }

    pub fn list_map(&self, mut f: impl FnMut(&Sexpr) -> Sexpr) -> Sexpr {
        if let Sexpr::Null = self {
            return Sexpr::Boolean(false);
        }

        let mut result = Self::null();
        let mut tail = Self::null();
        let mut list = self.clone();
        while let Sexpr::Pair(x) = list {
            let value = f(&x.0);
            let pair = Sexpr::cons(value, Sexpr::null());
            if tail.is_null() {
                result = pair.clone();
            } else {
                tail.set_cdr(pair.clone());
            }
            tail = pair;
            list = x.1.clone();
        }

        result
    }

    pub fn list_map2(mut f: impl FnMut(&Sexpr, &Sexpr) -> Sexpr, ls1: Sexpr, ls2: Sexpr) -> Sexpr {
        if ls1.is_null() || ls2.is_null() {
            return Sexpr::Boolean(false);
        }

        let mut result = Self::null();
        let mut tail = Self::null();
        let mut list1 = ls1;
        let mut list2 = ls2;
        while let (Sexpr::Pair(x), Sexpr::Pair(y)) = (&list1, &list2) {
            let value = f(&x.0, &y.0);
            let pair = Sexpr::cons(value, Sexpr::null());
            if tail.is_null() {
                result = pair.clone();
            } else {
                tail.set_cdr(pair.clone());
            }
            tail = pair;
            list1 = x.1.clone();
            list2 = y.1.clone();
        }

        result
    }

    pub fn assq(&self, key: &Sexpr) -> Sexpr {
        if let Sexpr::Pair(x) = self {
            if let Sexpr::Pair(kv) = &x.0 {
                if sexp_eq(&kv.0, key) {
                    return x.0.clone();
                }
            }
            return x.1.assq(key);
        }
        Sexpr::Boolean(false)
    }

    pub fn assq_set(self, key: &Sexpr, val: Sexpr) -> Sexpr {
        let alist = self.clone();

        let entry = alist.assq(key);

        if let Sexpr::Pair(mut x) = entry {
            x.1 = val;
            alist
        } else {
            sexp_acons(key.clone(), val, alist)
        }
    }

    pub fn append(ls2: Sexpr, ls1: Sexpr) -> Sexpr {
        if ls1.is_null() {
            return ls2;
        }

        let mut result = Self::null();
        let mut tail = Self::null();
        let mut list = ls1;
        while let Sexpr::Pair(x) = list {
            let pair = Sexpr::cons(x.0.clone(), Sexpr::null());
            if tail.is_null() {
                result = pair.clone();
            } else {
                tail.set_cdr(pair.clone());
            }
            tail = pair;
            list = x.1.clone();
        }

        tail.set_cdr(ls2);

        result
    }

    pub fn copy_alist(this: &Sexpr) -> Sexpr {
        this.list_map(|x| sexp_cons(x.car(), x.cdr()))
    }

    pub fn is_list(&self) -> bool {
        let mut list = self.clone();
        while let Sexpr::Pair(x) = list {
            list = x.1.clone();
        }
        list.is_null()
    }

    pub fn list_length(&self) -> Option<usize> {
        let mut list = self.clone();
        let mut len = 0;
        while let Sexpr::Pair(x) = list {
            len += 1;
            list = x.1.clone();
        }
        if list.is_null() {
            Some(len)
        } else {
            None
        }
    }

    pub fn memq(&self, obj: &Sexpr) -> Sexpr {
        let mut list = self.clone();

        while !list.is_null() {
            if sexp_eq(&list.car(), obj) {
                return list;
            }

            list = list.cdr();
        }

        Sexpr::Boolean(false)
    }

    pub fn last_pair(&self) -> Sexpr {
        if !self.is_pair() {
            unreachable!("last_pair: not a pair");
        }

        let mut slow = self.clone();
        let mut l = slow.clone();
        loop {
            if !l.cdr().is_pair() {
                return l;
            }

            l = l.cdr();

            if !l.cdr().is_pair() {
                return l;
            }

            l = l.cdr();
            slow = slow.cdr();

            if sexp_eq(&l, &slow) {
                panic!("last_pair: circular list");
            }
        }
    }

    pub fn list_reverse(&self) -> Sexpr {
        let mut list = self.clone();
        let mut res = Sexpr::null();
        while let Sexpr::Pair(x) = list {
            res = Sexpr::cons(x.0.clone(), res);
            list = x.1.clone();
        }
        res
    }
}

use pretty::{BoxAllocator, DocAllocator, DocBuilder};
use r7rs_parser::expr::{Expr, Interner};
use termcolor::{Color, ColorSpec, WriteColor};

impl Sexpr {
    pub fn pretty<'a, D>(&self, allocator: &'a D) -> DocBuilder<'a, D, ColorSpec>
    where
        D: DocAllocator<'a, ColorSpec>,
        D::Doc: Clone,
    {
        match self {
            Self::Gensym(x) => allocator.text(format!("#<gensym {}>", x)),
            Self::LVar(lvar) => allocator
                .text("#<lvar")
                .append(allocator.space())
                .append(lvar.name.pretty(allocator))
                .append(format!(".{:p}>", lvar.as_ptr())),
            Self::Special(_) => allocator.text("#<special>"),
            Self::Identifier(id) => allocator
                .text("#<identifier")
                .append(allocator.space())
                .append(id.name.pretty(allocator))
                .append(format!(".{:p}>", id.as_ptr()))
                .annotate(ColorSpec::new().set_fg(Some(Color::Red)).clone()),
            Self::Symbol(name) => allocator.text(scm_symbol_str(name.get_object())),

            Self::Pair(pair) => {
                let mut docs = vec![];

                let mut pair = pair.clone();
                loop {
                    docs.push(pair.0.pretty(allocator));
                    match &pair.1 {
                        Self::Pair(x) => pair = x.clone(),
                        Self::Null => break,
                        _ => {
                            docs.push(allocator.text("."));
                            docs.push(pair.1.pretty(allocator));
                            break;
                        }
                    }
                }

                allocator
                    .intersperse(docs, allocator.softline())
                    .group()
                    .parens()
            }

            Self::Vector(vec) => allocator.text("#").append(
                allocator
                    .intersperse(
                        vec.iter().map(|x| x.pretty(allocator)),
                        allocator.softline(),
                    )
                    .group()
                    .parens(),
            ),

            Self::Boolean(true) => allocator.text("#t"),
            Self::Boolean(false) => allocator.text("#f"),
            Self::Fixnum(x) => allocator.text(format!("{}", x)),
            Self::Flonum(x) => allocator.text(format!("{}", x)),
            Self::Char(x) => allocator.text(format!("#\\{}", x)),
            Self::String(x) => allocator.text(format!("{:?}", x)),
            Self::Null => allocator.text("()"),
            Self::Undefined => allocator.text("#<undefined>"),
            Self::Bytevector(x) => allocator.text(format!("#u8({:?})", x)),
            Self::SyntaxPattern(x) => allocator.text(format!("#<syntax-pattern {:?}>", x)),
            Self::SyntaxRules(x) => allocator.text(format!("#<syntax-rules {:?}>", x)),
            Self::PVRef(x) => allocator.text(format!("#<pv-ref {:?}>", x)),
        }
    }

    pub fn pretty_print(&self, w: &mut dyn WriteColor) -> std::io::Result<()> {
        let allocator = BoxAllocator;
        let doc = self.pretty(&allocator);
        doc.render(80, w)
    }
}

impl std::fmt::Display for Sexpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let allocator = BoxAllocator;
        let doc = self.pretty(&allocator);
        let mut writer_str = std::io::BufWriter::new(Vec::new());
        let mut writer = termcolor::NoColor::new(&mut writer_str);
        match doc.render(80, &mut writer) {
            Ok(_) => {
                let s = String::from_utf8(writer_str.into_inner().unwrap()).unwrap();
                write!(f, "{}", s)
            }
            Err(_) => Err(std::fmt::Error),
        }
    }
}

pub fn r7rs_expr_to_sexpr<I: Interner>(interner: &I, expr: &Expr<I>) -> Sexpr {
    match expr {
        Expr::Fixnum(x) => Sexpr::Fixnum(*x),
        Expr::Bool(x) => Sexpr::Boolean(*x),
        Expr::Char(x) => Sexpr::Char(*x),
        Expr::Str(x) => Sexpr::String(P::new(x.clone())),
        Expr::ByteVector(x) => Sexpr::Bytevector(P::new(x.to_vec())),
        Expr::Pair(x, y) => {
            let x = r7rs_expr_to_sexpr(interner, &x);
            let y = r7rs_expr_to_sexpr(interner, &y);

            Sexpr::cons(x, y)
        }

        Expr::ImmutableVector(vec) | Expr::GrowableVector(vec) => Sexpr::Vector(P(vec
            .iter()
            .map(|x| r7rs_expr_to_sexpr(interner, &x))
            .collect())),

        Expr::Syntax(_, expr) => r7rs_expr_to_sexpr(interner, &expr),
        Expr::Null => Sexpr::Null,
        Expr::Symbol(x) => Sexpr::Symbol(scm_intern(interner.description(x))),

        _ => todo!(),
    }
}

#[macro_export]
macro_rules! sexp_append1 {
    ($start: expr, $last: expr, $obj: expr) => {
        if $start.is_null() {
            *$start = $crate::compiler::sexpr::sexp_cons(
                $obj.clone(),
                $crate::compiler::sexpr::Sexpr::Null,
            );
            *$last = $start.clone();
        } else {
            $last.set_cdr($crate::compiler::sexpr::sexp_cons(
                $obj.clone(),
                $crate::compiler::sexpr::Sexpr::Null,
            ));
            *$last = $last.cdr();
        }
    };
}

#[macro_export]
macro_rules! sexp_append {
    ($start: expr, $last: expr, $obj: expr) => {{
        let list = $obj.clone();

        if $start.is_null() {
            *$start = list.clone();
            if !list.is_null() {
                *$last = list.last_pair();
            }
        } else {
            $last.set_cdr(list);
            *$last = $last.last_pair();
        }
    }};
}

pub fn sexp_append2(list: Sexpr, obj: Sexpr) -> Sexpr {
    if list.is_null() {
        return obj;
    }

    let mut cp;
    let mut start = Sexpr::Null;
    let mut last = Sexpr::Null;

    cp = list.clone();

    while cp.is_pair() {
        sexp_append1!(&mut start, &mut last, cp.car());
        cp = cp.cdr();
    }

    last.set_cdr(obj);

    start
}

pub fn sexp_reverse2x(list: Sexpr, tail: Sexpr) -> Sexpr {
    if !list.is_pair() {
        return tail;
    }

    let mut result = tail.clone();
    let mut first = list.clone();
    let mut next;

    while first.is_pair() {
        next = first.cdr();
        first.set_cdr(result);
        result = first;
        first = next;
    }

    result
}

pub fn sexp_reversex(list: Sexpr) -> Sexpr {
    sexp_reverse2x(list, Sexpr::Null)
}
