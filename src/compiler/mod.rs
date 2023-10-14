pub mod expand;
pub mod p;
#[macro_use]
pub mod sexpr;
pub mod assignment_elimination;
pub mod compile_bytecode;
pub mod constfold;
pub mod fix_letrec;
pub mod loops;
pub mod pass2;
pub mod pass2p2;
pub mod pass3;
pub mod primitives;
pub mod synrules;
pub mod tree_il;

use std::{collections::HashMap, rc::Rc, cell::RefCell};

pub use p::P;

use crate::runtime::value::Value;

use self::{
    primitives::resolve_primitives,
    sexpr::{
        sexp_acons, sexp_cons, sexp_eq, EqSexpr, Identifier, Sexpr, SourceInfo, SourceLoc,
        SyntaxRules,
    },
    tree_il::IForm,
};

pub fn make_identifier(name: Sexpr, senv: P<SyntaxEnv>, env: Sexpr) -> P<Identifier> {
    P::new(Identifier {
        name,
        env: senv,
        frames: sexp_cons(Sexpr::Boolean(false), env),
    })
}

pub fn outermost_identifier(mut id: P<Identifier>) -> P<Identifier> {
    while let Sexpr::Identifier(x) = &id.name {
        id = x.clone();
    }

    id
}

pub fn unwrap_identifier(id: P<Identifier>) -> Value {
    let Sexpr::Symbol(name) = outermost_identifier(id).name else {
        panic!("unwrap_identifier: not a symbol")
    };

    name
}

pub fn identifier_to_symbol(id: Sexpr) -> Value {
    match id {
        Sexpr::Identifier(id) => unwrap_identifier(id),
        Sexpr::Symbol(id) => id,
        _ => unreachable!("identifier_to_symbol: not an identifier"),
    }
}

pub fn wrap_identifier(id: P<Identifier>) -> P<Identifier> {
    make_identifier(
        Sexpr::Identifier(id.clone()),
        id.env.clone(),
        id.frames.clone(),
    )
}

fn get_binding_frame(var: Sexpr, env: Sexpr) -> Sexpr {
    let mut frame = env;

    while frame.is_pair() {
        if !frame.car().is_pair() {
            frame = frame.cdr();
            continue;
        }

        let mut fp = frame.cdar();

        while fp.is_pair() {
            if sexp_eq(&fp.caar(), &var) {
                return frame.clone();
            }

            fp = fp.cdr();
        }

        frame = frame.cdr();
    }

    Sexpr::Null
}

pub fn identifier_env(mut id: P<Identifier>) -> Sexpr {
    if let Sexpr::Boolean(false) = id.frames.car() {
        let f = get_binding_frame(id.name.clone(), id.frames.cdr());
        id.frames.set_cdr(f);
        id.frames.set_car(Sexpr::Boolean(true));
    }

    id.frames.cdr()
}

#[derive(Clone)]
pub struct SyntaxEnv {
    pub env: HashMap<Value, P<Denotation>>,
    pub denotation_of_define: Option<P<Denotation>>,
    pub denotation_of_begin: Option<P<Denotation>>,
}

#[derive(Clone)]
pub enum Denotation {
    Macro(P<SyntaxRules>),
    Special(fn(Sexpr, &Cenv) -> Result<P<IForm>, String>),
}

#[derive(Clone)]
pub struct Cenv {
    pub syntax_env: P<SyntaxEnv>,
    pub frames: Sexpr,
    pub source_loc: Rc<RefCell<SourceInfo>>,
    pub expr_name: Sexpr,
}

impl Cenv {
    pub fn maybe_source(&self, sexpr: &Sexpr) -> Option<SourceLoc> {
        if let Some(loc) = self.source_loc.borrow().get(&EqSexpr(sexpr.clone())) {
            Some(loc.clone())
        } else {
            None
        }
    }

    fn lookup_int(&self, name: Sexpr) -> Sexpr {
        let mut y = name.clone();

        let mut frames = self.frames.clone();

        loop {
            while frames.is_pair() {
                let fp1 = frames.clone();

                let mut vls = fp1.cdar();

                while vls.is_pair() {
                    let vp = vls.car();

                    if sexp_eq(&vp.car(), &y) {
                        return vp.cdr();
                    }

                    vls = vls.cdr();
                }

                frames = frames.cdr();
            }

            if let Sexpr::Identifier(ident) = y.clone() {
                let inner = ident.name.clone();
                if let Sexpr::Identifier(ref ident) = inner {
                    frames = ident.frames.clone();
                }

                y = inner;
            } else {
                break;
            }
        }

        if let Sexpr::Symbol(_) = name {
            Sexpr::Identifier(make_identifier(
                name.clone(),
                self.syntax_env.clone(),
                self.frames.clone(),
            ))
        } else {
            return name;
        }
    }

    pub fn lookup(&self, name: Sexpr) -> Sexpr {
        self.lookup_int(name)
    }

    pub fn extend(&self, frame: Sexpr, typ: Sexpr) -> Self {
        Self {
            syntax_env: self.syntax_env.clone(),
            frames: sexp_acons(typ, frame, self.frames.clone()),
            source_loc: self.source_loc.clone(),
            expr_name: self.expr_name.clone(),
        }
    }

    pub fn is_toplevel(&self) -> bool {
        self.frames.is_null()
    }
}

pub fn is_free_identifier_eq(id1: Sexpr, id2: Sexpr) -> bool {
    match (id1, id2) {
        (Sexpr::Identifier(id1), Sexpr::Identifier(id2)) => {
            if id1.as_ptr() == id2.as_ptr() {
                return true;
            }
            let b1 = Cenv {
                syntax_env: id1.env.clone(),
                frames: identifier_env(id1.clone()),
                source_loc: Rc::new(RefCell::new(SourceInfo::new())),
                expr_name: Sexpr::Null
            }
            .lookup_int(id1.name.clone());
            let b2 = Cenv {
                syntax_env: id2.env.clone(),
                frames: identifier_env(id2.clone()),
                source_loc: Rc::new(RefCell::new(SourceInfo::new())),
                expr_name: Sexpr::Null
            }
            .lookup_int(id2.name.clone());

            match (&b1, &b2) {
                (Sexpr::Identifier(_), Sexpr::Identifier(_)) => {
                    unwrap_identifier(id1) == unwrap_identifier(id2)
                }

                _ => sexp_eq(&b1, &b2),
            }
        }
        _ => false,
    }
}

pub fn er_compare(a: Sexpr, b: Sexpr, env: P<SyntaxEnv>, frames: Sexpr) -> bool {
    if matches!(a, Sexpr::Identifier(_) | Sexpr::Symbol(_))
        && matches!(b, Sexpr::Identifier(_) | Sexpr::Symbol(_))
    {
        let a1 = Cenv {
            syntax_env: env.clone(),
            frames: frames.clone(),
            expr_name: Sexpr::Null,
            source_loc: Rc::new(RefCell::new(SourceInfo::new())),
        }
        .lookup(a);
        let b1 = Cenv {
            syntax_env: env.clone(),
            frames: frames.clone(),
            source_loc: Rc::new(RefCell::new(SourceInfo::new())),
            expr_name: Sexpr::Null
        }
        .lookup(b);

        if sexp_eq(&a1, &b1) {
            return true;
        }

        match (a1, b1) {
            (Sexpr::Identifier(a1), Sexpr::Identifier(b1)) => {
                is_free_identifier_eq(Sexpr::Identifier(a1), Sexpr::Identifier(b1))
            }

            _ => false,
        }
    } else {
        false
    }
}

/// Given a symbol that may be an R5RS-mangled or R6RS-mangled
/// identifier, returns the unmangled identifier.  Should be used
/// only for documentation purposes, not for quoted constants.
///
/// R6RS/ERR5RS variables have one of these forms:
///     \x0;ID~HEX~HEX     where HEX is a sequence of hexadecimal digits
///     \x1;ID

pub fn unmangled(id: &str) -> &str {
    const GUID_PREFIX: char = 0 as char;
    const FREE_PREFIX: char = 1 as char;
    const SEPARATOR: char = '~';

    // code that extracts `ID` from the mangled identifier

    let mut id = id;

    if id.starts_with(GUID_PREFIX) {
        id = &id[2..];
        if let Some(i) = id.find(SEPARATOR) {
            id = &id[..i];
        }
    } else if id.starts_with(FREE_PREFIX) {
        id = &id[2..];
    }

    id
}

pub fn compile(
    sexpr: &Sexpr,
    cenv: &Cenv,
    recover_loops: bool,
    opt: bool,
) -> Result<P<IForm>, String> {
    let expanded = expand::pass1(sexpr, cenv)?;
    let fixed = fix_letrec::pass_fix_letrec(expanded);
    let immut = assignment_elimination::assignment_elimination(fixed);
    let immut = resolve_primitives(immut);
    if opt {
        let opt = pass2::pass2(immut, recover_loops)?;

        Ok(opt)
    } else {
        Ok(immut)
    }
}
