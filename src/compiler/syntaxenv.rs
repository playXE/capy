use std::sync::atomic::{AtomicUsize, Ordering};

use once_cell::sync::Lazy;
use rsgc::thread::Thread;

use crate::{
    bool::equal,
    value::{Parameter, Value},
    vm::{intern, parameterize},
};

pub static LAMBDA: Lazy<Value> = Lazy::new(|| intern("lambda "));
pub static SET: Lazy<Value> = Lazy::new(|| intern("set! "));

/*
(define standard-syntactic-environment
 (let ((hash-percent-datum (string->symbol "#%datum"))
       (hash-percent-top   (string->symbol "#%top"))
       (hash-percent-app   (string->symbol "#%app")))
  `((quote         . (special quote))
    (lambda        . (special lambda))
    (if            . (special if))
    (set!          . (special set!))
    (begin         . (special begin))
    (define        . (special define))
    (define-inline . (special define-inline))
    (define-syntax . (special define-syntax))
    (let-syntax    . (special let-syntax))
    (letrec-syntax . (special letrec-syntax))
    (syntax-rules  . (special syntax-rules))

    ;; MzScheme specific
    (,hash-percent-datum       . (special ,hash-percent-datum))
    (,hash-percent-top         . (special ,hash-percent-top))
    (,hash-percent-app         . (special ,hash-percent-app))
    )))
*/
pub fn standard_syntactic_environment_() -> Value {
    let hash_percent_datum = intern("#%datum");
    let hash_percent_top = intern("#%top");
    let hash_percent_app = intern("#%app");

    let special = intern("special");
    let quote = intern("quote");
    let lambda = intern("lambda");
    let if_ = intern("if");
    let set = intern("set!");
    let begin = intern("begin");
    let define = intern("define");
    let define_inline = intern("define-inline");
    let define_syntax = intern("define-syntax");
    let let_syntax = intern("let-syntax");
    let letrec_syntax = intern("letrec-syntax");
    let syntax_rules = intern("syntax-rules");

    let mut env = Value::make_null();
    env = Value::make_cons(
        Value::make_cons(quote, Value::make_cons(special, quote)),
        env,
    );
    env = Value::make_cons(
        Value::make_cons(lambda, Value::make_cons(special, lambda)),
        env,
    );
    env = Value::make_cons(Value::make_cons(if_, Value::make_cons(special, if_)), env);
    env = Value::make_cons(Value::make_cons(set, Value::make_cons(special, set)), env);
    env = Value::make_cons(
        Value::make_cons(begin, Value::make_cons(special, begin)),
        env,
    );
    env = Value::make_cons(
        Value::make_cons(define, Value::make_cons(special, define)),
        env,
    );
    env = Value::make_cons(
        Value::make_cons(define_inline, Value::make_cons(special, define_inline)),
        env,
    );
    env = Value::make_cons(
        Value::make_cons(define_syntax, Value::make_cons(special, define_syntax)),
        env,
    );
    env = Value::make_cons(
        Value::make_cons(let_syntax, Value::make_cons(special, let_syntax)),
        env,
    );
    env = Value::make_cons(
        Value::make_cons(letrec_syntax, Value::make_cons(special, letrec_syntax)),
        env,
    );
    env = Value::make_cons(
        Value::make_cons(syntax_rules, Value::make_cons(special, syntax_rules)),
        env,
    );
    env = Value::make_cons(
        Value::make_cons(
            hash_percent_datum,
            Value::make_cons(special, hash_percent_datum),
        ),
        env,
    );
    env = Value::make_cons(
        Value::make_cons(
            hash_percent_top,
            Value::make_cons(special, hash_percent_top),
        ),
        env,
    );
    env = Value::make_cons(
        Value::make_cons(
            hash_percent_app,
            Value::make_cons(special, hash_percent_app),
        ),
        env,
    );

    env
}

static STANDARD_SYNTACTIC_ENVIRONMENT: Lazy<Value> = Lazy::new(standard_syntactic_environment_);

pub fn make_basic_syntactic_environment() -> Value {
    Value::make_cons(
        Value::make_cons(
            LAMBDA.clone(),
            Value::assq(intern("lambda"), *STANDARD_SYNTACTIC_ENVIRONMENT).cdr(),
        ),
        Value::make_cons(
            Value::make_cons(
                SET.clone(),
                Value::assq(intern("set!"), *STANDARD_SYNTACTIC_ENVIRONMENT).cdr(),
            ),
            Value::copy_alist(Thread::current(), STANDARD_SYNTACTIC_ENVIRONMENT.clone()),
        ),
    )
}

pub fn make_minimal_syntactic_environment() -> Value {
    Value::make_cons(
        Value::make_cons(
            LAMBDA.clone(),
            Value::assq(intern("lambda"), *STANDARD_SYNTACTIC_ENVIRONMENT).cdr(),
        ),
        Value::make_cons(
            Value::make_cons(
                SET.clone(),
                Value::assq(intern("set!"), *STANDARD_SYNTACTIC_ENVIRONMENT).cdr(),
            ),
            Value::make_null(),
        ),
    )
}

pub static USUAL_SYNTACTIC_ENVIRONMENT: Lazy<Value> = Lazy::new(make_basic_syntactic_environment);

pub fn the_usual_syntactic_environment(copy: bool) -> Value {
    if copy {
        Value::copy_alist(Thread::current(), USUAL_SYNTACTIC_ENVIRONMENT.clone())
    } else {
        USUAL_SYNTACTIC_ENVIRONMENT.clone()
    }
}

pub fn syntactic_environment_names(syntaxenv: Value) -> Value {
    fn f(e: Value, n: Value) -> Value {
        if e.nullp() {
            n
        } else {
            let name = e.caar();
            if name == *LAMBDA || name == *SET {
                f(e.cdr(), n)
            } else {
                f(e.cdr(), Value::make_cons(name, n))
            }
        }
    }

    f(syntaxenv, Value::make_null())
}

pub fn syntactic_lookup(env: Value, id: Value) -> Value {
    let entry = Value::assq(id, env);
    if entry.pairp() {
        entry.cdr()
    } else {
        make_identifier_denotation(id)
    }
}

pub static IDENTIFIER: Lazy<Value> = Lazy::new(|| intern("identifier"));

pub fn make_identifier_denotation(id: Value) -> Value {
    Value::make_list(
        Thread::current(),
        &[
            *IDENTIFIER,
            id,
            Value::make_null(),
            Value::make_null(),
            Value::make_null(),
        ],
    )
}

pub static DENOTATION_OF_QUOTE: Lazy<Value> =
    Lazy::new(|| syntactic_lookup(*STANDARD_SYNTACTIC_ENVIRONMENT, intern("quote")));

pub fn denotation_of_quote() -> Value {
    DENOTATION_OF_QUOTE.clone()
}

pub static DENOTATION_OF_LAMBDA: Lazy<Value> =
    Lazy::new(|| syntactic_lookup(*STANDARD_SYNTACTIC_ENVIRONMENT, intern("lambda")));

pub fn denotation_of_lambda() -> Value {
    DENOTATION_OF_LAMBDA.clone()
}

pub static DENOTATION_OF_IF: Lazy<Value> =
    Lazy::new(|| syntactic_lookup(*STANDARD_SYNTACTIC_ENVIRONMENT, intern("if")));

pub fn denotation_of_if() -> Value {
    DENOTATION_OF_IF.clone()
}

pub static DENOTATION_OF_SET: Lazy<Value> =
    Lazy::new(|| syntactic_lookup(*STANDARD_SYNTACTIC_ENVIRONMENT, intern("set!")));

pub fn denotation_of_set() -> Value {
    DENOTATION_OF_SET.clone()
}

pub static DENOTATION_OF_BEGIN: Lazy<Value> =
    Lazy::new(|| syntactic_lookup(*STANDARD_SYNTACTIC_ENVIRONMENT, intern("begin")));

pub fn denotation_of_begin() -> Value {
    DENOTATION_OF_BEGIN.clone()
}

pub static DENOTATION_OF_DEFINE: Lazy<Value> =
    Lazy::new(|| syntactic_lookup(*STANDARD_SYNTACTIC_ENVIRONMENT, intern("define")));

pub fn denotation_of_define() -> Value {
    DENOTATION_OF_DEFINE.clone()
}

pub static DENOTATION_OF_DEFINE_INLINE: Lazy<Value> =
    Lazy::new(|| syntactic_lookup(*STANDARD_SYNTACTIC_ENVIRONMENT, intern("define-inline")));

pub fn denotation_of_define_inline() -> Value {
    DENOTATION_OF_DEFINE_INLINE.clone()
}

pub static DENOTATION_OF_DEFINE_SYNTAX: Lazy<Value> =
    Lazy::new(|| syntactic_lookup(*STANDARD_SYNTACTIC_ENVIRONMENT, intern("define-syntax")));

pub fn denotation_of_define_syntax() -> Value {
    DENOTATION_OF_DEFINE_SYNTAX.clone()
}

pub static DENOTATION_OF_LET_SYNTAX: Lazy<Value> =
    Lazy::new(|| syntactic_lookup(*STANDARD_SYNTACTIC_ENVIRONMENT, intern("let-syntax")));

pub fn denotation_of_let_syntax() -> Value {
    DENOTATION_OF_LET_SYNTAX.clone()
}

pub static DENOTATION_OF_LETREC_SYNTAX: Lazy<Value> =
    Lazy::new(|| syntactic_lookup(*STANDARD_SYNTACTIC_ENVIRONMENT, intern("letrec-syntax")));

pub fn denotation_of_letrec_syntax() -> Value {
    DENOTATION_OF_LETREC_SYNTAX.clone()
}

pub static DENOTATION_OF_SYNTAX_RULES: Lazy<Value> =
    Lazy::new(|| syntactic_lookup(*STANDARD_SYNTACTIC_ENVIRONMENT, intern("syntax-rules")));

pub fn denotation_of_syntax_rules() -> Value {
    DENOTATION_OF_SYNTAX_RULES.clone()
}

pub static DENOTATION_OF_APP: Lazy<Value> =
    Lazy::new(|| syntactic_lookup(*STANDARD_SYNTACTIC_ENVIRONMENT, intern("#%app")));

pub fn denotation_of_app() -> Value {
    DENOTATION_OF_APP.clone()
}

pub static DENOTATION_OF_DATUM: Lazy<Value> =
    Lazy::new(|| syntactic_lookup(*STANDARD_SYNTACTIC_ENVIRONMENT, intern("#%datum")));

pub fn denotation_of_datum() -> Value {
    DENOTATION_OF_DATUM.clone()
}

pub static DENOTATION_OF_TOP: Lazy<Value> =
    Lazy::new(|| syntactic_lookup(*STANDARD_SYNTACTIC_ENVIRONMENT, intern("#%top")));

pub fn denotation_of_top() -> Value {
    DENOTATION_OF_TOP.clone()
}

pub static DENOTATION_OF_TRANSFORMER: Lazy<Value> =
    Lazy::new(|| syntactic_lookup(*STANDARD_SYNTACTIC_ENVIRONMENT, intern("transformer")));

pub fn denotation_of_transformer() -> Value {
    DENOTATION_OF_TRANSFORMER.clone()
}

pub static DENOTATION_OF_ELLIPSIS: Lazy<Value> =
    Lazy::new(|| syntactic_lookup(*STANDARD_SYNTACTIC_ENVIRONMENT, intern("...")));

pub fn denotation_of_ellipsis() -> Value {
    DENOTATION_OF_ELLIPSIS.clone()
}

pub fn syntactic_alias(env: Value, alist: Value, env2: Value) -> Value {
    syntactic_divert(
        env,
        Value::list_map(Thread::current(), alist, |name_pair| {
            let old_name = name_pair.car();
            let new_name = name_pair.cdr();
            Value::cons(
                Thread::current(),
                new_name,
                syntactic_lookup(env2, old_name),
            )
        }),
    )
}

pub fn syntactic_divert(env1: Value, env2: Value) -> Value {
    Value::list_append(Thread::current(), env2, env1)
}

/// Given a syntactic environment and an alist returned by rename-vars,
/// extends the environment by binding the old identifiers to the fresh
/// identifiers.
/// For compiler, it also binds the fresh identifiers to their denotations.
/// This is ok so long as the fresh identifiers are not legal Scheme
/// identifiers.
pub fn syntactic_rename(env: Value, alist: Value) -> Value {
    if alist.nullp() {
        env
    } else {
        let old = alist.caar();
        let new = alist.cdar();

        let denotation = make_identifier_denotation(new);

        syntactic_rename(
            Value::make_cons(
                Value::make_cons(old, denotation),
                Value::make_cons(Value::make_cons(new, denotation), env),
            ),
            alist.cdr(),
        )
    }
}

/*pub fn make_rename_procedure() -> Value {
    static RENAMING_COUNTER: AtomicUsize = AtomicUsize::new(0);
    let suffix = RENAMING_COUNTER.fetch_add(1, Ordering::Relaxed).to_string();
    let suffix = Str::new(Thread::current(), &suffix);
    let proc = Vm::make_closed_procedure(
        "#%rename",
        |_vm, args, vars, _| {
            let suffix = vars[0].str();

            let sym = args[0];

            if sym.symbolp() {
                let s = sym.symbol_str();

                if s.len() > 0 && s.chars().next().unwrap() == '.' {
                    Trampoline::Return(intern(format!("{}{}", s, suffix)))
                } else {
                    Trampoline::Return(intern(format!(".{}{}", s, suffix)))
                }
            } else {
                eprintln!("illegal use of rename procedure: {}", sym);
                Trampoline::Return(sym)
            }
        },
        1,
        1,
        &[suffix],
    );

    proc
}*/

pub static RENAMING_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub fn make_rename_procedure() -> Box<dyn FnMut(Value) -> Value> {
    

    let suffix = format!("|{}", RENAMING_COUNTER.fetch_add(1, Ordering::Relaxed));

    Box::new(move |name: Value| {
        if name.symbolp() {
            let s = name.strsym();
            if s.len() > 0 && s.chars().next().unwrap() == '.' {
                intern(format!("{}{}", s, suffix))
            } else {
                intern(format!(".{}{}", s, suffix))
            }
        } else {
            eprintln!("illegal use of rename procedure: {}", name);
            name
        }
    })
}

pub fn m_strip_noncircular<'a>(x: Value) -> Value {
    /// This routine unmangles compiler R5RS naming
    fn original_symbol<'a>(x: Value) -> &'a str {
        fn loop_<'a>(sym: Value, s: &'a str, i: usize, n: usize) -> &'a str {
            if i == n {
                sym.strsym()
            } else if s.chars().nth(i).unwrap() == '|' {
                &s[1..i]
            } else {
                loop_(sym, s, i + 1, n)
            }
        }

        let s = x.strsym();

        if s.len() > 0 && s.chars().next().unwrap() == '.' {
            loop_(x, s, 1, s.len())
        } else {
            s
        }
    }

    if x.symbolp() {
        return intern(original_symbol(x));
    } else if x.pairp() {
        let a = m_strip_noncircular(x.car());
        let b = m_strip_noncircular(x.cdr());

        if a == x.car() && b == x.cdr() {
            return x;
        } else {
            return Value::make_cons(a, b);
        }
    } else if x.vectorp() {
        let v = x.vector_to_list();
        let v2 = Value::list_map(Thread::current(), v, |x| m_strip_noncircular(x));

        if equal(v, v2) {
            return x;
        } else {
            return v2.list_to_vector();
        }
    } else {
        return x;
    }
}

pub fn m_strip(x: Value) -> Value {
    // TODO: Does alexpander produce circular lists?
    // If so, we need to handle them here.
    m_strip_noncircular(x)
}

/// Given a list of identifiers, or a formal parameter "list",
/// returns an alist that associates each identifier with a fresh identifier.
pub fn rename_vars(original_vars: Value) -> Result<Value, Value> {
    let mut rename = make_rename_procedure();

    fn rec(
        vars: Value,
        newvars: Value,
        rename: &mut dyn FnMut(Value) -> Value,
    ) -> Result<Value, Value> {
        if vars.nullp() {
            Ok(Value::list_reverse(Thread::current(), newvars))
        } else if vars.pairp() {
            let var = vars.car();
            if var.symbolp() {
                rec(
                    vars.cdr(),
                    Value::make_cons(Value::make_cons(var, rename(var)), newvars),
                    rename,
                )
            } else {
                super::error(&format!("illegal variable name: {}", var))
            }
        } else if vars.symbolp() {
            rec(Value::make_cons(vars, Value::null()), newvars, rename)
        } else {
            super::error(&format!("illegal variable name: {}", vars))
        }
    }

    rec(original_vars, Value::null(), &mut rename)
}


/// Given a <formals> and an alist returned by rename_vars that contains
/// a new name for each formal identifier in <formals>, renames the
/// formal identifiers.

pub fn rename_formals(formals: Value, alist: Value) -> Value {
    if formals.nullp() {
        Value::make_null()
    } else if formals.pairp() {
        let a = Value::assq(formals.car(), alist);

        Value::make_cons(a.cdr(), rename_formals(formals.cdr(), alist))
    } else {
        Value::assq(formals, alist).cdr()
    }
}

pub fn is_special_denotation(denotation: Value) -> bool {
    denotation.car() == intern("special")
}

pub fn is_macro_denotation(denotation: Value) -> bool {
    denotation.car() == intern("macro")
}

pub fn is_inline_denotation(denotation: Value) -> bool {
    denotation.car() == intern("inline")
}

pub fn is_identifier_denotation(denotation: Value) -> bool {
    denotation.car() == intern("identifier")
}

pub fn make_macro_denotation(rules: Value, env: Value) -> Value {
    Value::make_list(
        Thread::current(),
        &[
            intern("macro"),
            rules,
            env,
            Value::make_false(),
            Value::make_false(),
        ],
    )
}

pub fn make_macro_denotation_optimized(rules: Value, env: Value, h: Value, v: Value) -> Value {
    Value::make_list(Thread::current(), &[intern("macro"), rules, env, h, v])
}

pub fn make_inline_denotation(id: Value, rules: Value, env: Value) -> Value {
    Value::make_list(Thread::current(), &[intern("inline"), rules, env, id])
}

pub fn r#macro_rules(m: Value) -> Value {
    m.cadr()
}

pub fn macro_env(m: Value) -> Value {
    m.caddr()
}

pub fn macro_hash(m: Value) -> Value {
    m.cadddr()
}

pub fn macro_rules_table(d: Value) -> Value {
    d.cdddr().cdr().car()
}

pub fn inline_rules(d: Value) -> Value {
    r#macro_rules(d)
}

pub fn inline_env(d: Value) -> Value {
    macro_env(d)
}

pub fn inline_name(d: Value) -> Value {
    d.cadddr()
}

pub fn identifier_name(d: Value) -> Value {
    d.cadr()
}

pub fn identifier_r_entry(d: Value) -> Value {
    d.cdr()
}

pub fn is_same_denotation(d1: Value, d2: Value) -> bool {
    if d1 == d2 {
        return true;
    }

    if is_identifier_denotation(d1) && is_identifier_denotation(d2) {
        return identifier_name(d1) == identifier_name(d2);
    }

    false
}

pub static GLOBAL_SYNTACTIC_ENVIRONMENT: Lazy<Value> = Lazy::new(|| {
    let param = Parameter::new(Thread::current(), Value::make_null(), Value::make_false());
    param
});

pub fn set_global_syntactic_environment(env: Value) {
    GLOBAL_SYNTACTIC_ENVIRONMENT.parameter_value().set_pair_cdr(env);
}

pub fn global_syntactic_environment() -> Value {
    *GLOBAL_SYNTACTIC_ENVIRONMENT.parameter_value()
}

pub fn syntactic_bind_globally(id: Value, denotation: Value) {
    if is_identifier_denotation(denotation) && id == identifier_name(denotation) {
        fn remove_bindings_for_id(bindings: Value, id: Value) -> Value {
            if bindings.nullp() {
                Value::make_null()
            } else if bindings.caar() == id {
                remove_bindings_for_id(bindings.cdr(), id)
            } else {
                Value::make_cons(bindings.car(), remove_bindings_for_id(bindings.cdr(), id))
            }
        }

        set_global_syntactic_environment(remove_bindings_for_id(
            global_syntactic_environment().cdr(),
            id,
        ))
    } else {
        let x = Value::assq(id, global_syntactic_environment());

        if !x.falsep() {
            x.set_pair_cdr(denotation);
        } else {
            set_global_syntactic_environment(Value::make_cons(
                Value::make_cons(id, denotation),
                global_syntactic_environment().cdr(),
            ))
        }
    }
}

pub fn syntactic_extend(env: Value, ids: Value, denotation: Value) -> Value {
    syntactic_divert(
        env,
        Value::list_map2(Thread::current(), ids, denotation, |id, denotation| {
            Value::make_cons(id, denotation)
        }),
    )
}

pub fn syntactic_assign(env: Value, id: Value, denotation: Value) {
    let entry = Value::assq(id, env);

    if entry.pairp() {
        entry.set_pair_cdr(denotation);
    } else {
        unreachable!("Bug in syntactic_assign")
    }
}

pub fn syntactic_environment_set(syntaxenv: Value, id: Value, macr: impl FnOnce() -> Result<Value, Value>) -> Result<(), Value> {
    parameterize(&[(*GLOBAL_SYNTACTIC_ENVIRONMENT, syntaxenv)], || -> Result<(), Value> {
        syntactic_bind_globally(id, macr()?);
        Ok(())
    })
}

pub fn syntactic_environment_remove(syntaxenv: Value, id: Value) -> Result<(), Value> {
    parameterize(&[(*GLOBAL_SYNTACTIC_ENVIRONMENT, syntaxenv)], || {
        syntactic_bind_globally(id, make_identifier_denotation(id));
        Ok(())
    })
}

pub enum SynLookup {
    NotFound,
    Found(Box<dyn FnOnce() -> Result<Value, Value>>)
}

/// Returns a closure that returns the denotation of the identifier `id` in the syntactic environment `syntaxenv`.
pub fn syntactic_environment_get(syntaxenv: Value, id: Value) -> SynLookup {
    let x = syntactic_lookup(syntaxenv, id);

    if is_identifier_denotation(x) {
        SynLookup::NotFound
    } else {
        SynLookup::Found(Box::new(move || Ok(x)))
    }
}

pub fn usual_syntax(id: Value) -> Result<SynLookup, Value> {
    let p = syntactic_environment_get(*USUAL_SYNTACTIC_ENVIRONMENT, id);
    match p {
        SynLookup::NotFound => super::error(&format!("usual-syntax: unknown macro: {}", id)),
        SynLookup::Found(f) => Ok(SynLookup::Found(f)),
    }
}

pub fn copy_alist(alist: Value) -> Value {
    Value::list_map(Thread::current(), alist, |x| {
        Value::make_cons(x.car(), x.cdr())
    })
}

pub fn syntactic_copy(env: Value) -> Value {
    copy_alist(env)
}