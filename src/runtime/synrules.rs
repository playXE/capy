#![allow(dead_code, unused_variables)]
//! # R5RS Macro
//! Keeping hygienic reference
//!
//!  - symbols which a template inserts into the expanded form are
//!    converted to identifiers at the macro definition time, encapsulating
//!   the defining environment of the macro.   So it doesn't interfere
//!   with the macro call environment.
//!
//!  - literal symbols provided to the syntax-rules are also converted
//!    to identifiers encapsulating the defining environment, and the
//!    environment information is used when comparing with the symbols
//!     in the macro call.
//!
//!
//!  - symbols in the macro call is treated as they are.  Since the result
//!   of macro expansion is immediately compiled in the macro call
//!   environment, those symbols can refer proper bindings.
//!
//!-------------------------------------------------------------------
//! pattern language compiler
//!   - recognize repeatable subpatterns and replace it to SyntaxPattern node.
//!   - convert free symbols in the template into identifiers
//!   - convert pattern variables into LREF object.
//!
//!

use crate::{
    gc::objstorage::ObjHandle, gc_protect, runtime::object::scm_cadr, scm_append1,
    vm::thread::Thread,
};

use super::{
    error::scm_error,
    list::{scm_acons, scm_assq, scm_cons, scm_length},
    object::{scm_car, scm_cdr, ScmCellHeader},
    symbol::scm_intern,
    value::Value,
};

#[repr(C)]
pub struct PVRef {
    pub(crate) header: ScmCellHeader,
    pub level: i16,
    pub count: i16,
}

const PVREF_LEVEL_MAX: u16 = 0xff;
const PVREF_COUNT_MAX: u32 = 0xff;

pub struct PatternContext {
    name: ObjHandle,
    form: ObjHandle,
    literals: ObjHandle,
    pvars: ObjHandle,
    renames: ObjHandle,
    ellipsis: ObjHandle,
    pvcnt: u32,
    maxlev: u32,
    env: ObjHandle,
    synenv: ObjHandle,
}

#[derive(Clone)]
pub struct MatchVar {
    branch: ObjHandle,
    sprout: ObjHandle,
    root: ObjHandle,
}

#[repr(C)]
pub struct SyntaxRules {
    pub(crate) header: ScmCellHeader,
    pub name: Value,
    pub max_num_pvars: u32,
    pub env: Value,
    pub syntax_env: Value,
    pub num_rules: u32,
    pub rules: [SyntaxRuleBranch; 0],
}

impl std::ops::Deref for SyntaxRules {
    type Target = [SyntaxRuleBranch];
    fn deref(&self) -> &Self::Target {
        unsafe { std::slice::from_raw_parts(self.rules.as_ptr(), self.num_rules as usize) }
    }
}

impl std::ops::DerefMut for SyntaxRules {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { std::slice::from_raw_parts_mut(self.rules.as_mut_ptr(), self.num_rules as usize) }
    }
}

impl std::ops::Index<usize> for SyntaxRules {
    type Output = SyntaxRuleBranch;
    fn index(&self, index: usize) -> &Self::Output {
        debug_assert!(index < self.num_rules as usize);
        unsafe { self.rules.as_ptr().add(index).as_ref().unwrap() }
    }
}

impl std::ops::IndexMut<usize> for SyntaxRules {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        debug_assert!(index < self.num_rules as usize);
        unsafe { self.rules.as_mut_ptr().add(index).as_mut().unwrap() }
    }
}

#[repr(C)]
pub struct SyntaxRuleBranch {
    pub pattern: Value,
    pub template: Value,
    pub num_pvars: u32,
    pub max_level: u32,
}

#[repr(C)]
pub struct SyntaxPattern {
    pub(crate) header: ScmCellHeader,
    pub pattern: Value,
    pub vars: Value,
    pub level: i16,
    pub num_following_items: i16,
}

impl PatternContext {
    fn add_pvar(&mut self, thread: &mut Thread, mut pat: Value, pvar: Value) -> Value {
        if pat.cast_as::<SyntaxPattern>().level > PVREF_LEVEL_MAX as i16 {
            scm_error(
                scm_intern("syntax-rules"),
                "syntax-rules",
                "pattern levels too deeply nested in the macro definition",
                scm_cons(self.name.get(), Value::encode_null_value()),
                Value::encode_null_value(),
            );
        }

        if self.pvcnt > PVREF_COUNT_MAX {
            scm_error(
                scm_intern("syntax-rules"),
                "syntax-rules",
                "too many pattern variables in the macro definition",
                scm_cons(self.name.get(), Value::encode_null_value()),
                Value::encode_null_value(),
            );
        }

        let mut pvref =
            thread.make_pvref(pat.cast_as::<SyntaxPattern>().level as _, self.pvcnt as _);
        self.pvcnt += 1;
        self.pvars
            .set(gc_protect!(thread => pvref, pat => scm_acons(pvar, pvref, self.pvars.get())));
        pat.cast_as::<SyntaxPattern>().vars = scm_cons(pvref, pat.cast_as::<SyntaxPattern>().vars);

        pvref
    }

    fn pvar_to_pvref(&mut self, pat: Value, pvar: Value) -> Value {
        let q = scm_assq(pvar, self.pvars.get());
        if !q.is_pair() {
            return pvar;
        }

        let pvref = scm_cdr(q);

        if pvref.cast_as::<PVRef>().level > pat.cast_as::<SyntaxPattern>().level as i16 {
            scm_error(
                scm_intern("syntax-rules"),
                "syntax-rules",
                "pattern variable used out of context",
                scm_cons(self.name.get(), Value::encode_null_value()),
                Value::encode_null_value(),
            );
        }

        pvref
    }

    fn is_ellipsis(&self, val: Value) -> bool {
        if self.ellipsis.get().is_false() {
            return false; // inside (... template)
        } else if self.ellipsis.get().is_true() {
            /*return er_compare(
                scm_virtual_machine().inherent_symbol(crate::vm::InherentSymbols::Ellipsis),
                val,
                self.synenv.get(),
                self.env.get()
            );*/
            true
        } else {
            self.ellipsis.get() == val
        }
    }

    fn ellipsis_following(&self, pat: Value) -> bool {
        scm_cdr(pat).is_pair() && self.is_ellipsis(scm_car(scm_cdr(pat)))
    }

    fn bad_ellipsis(&self) {
        scm_error(
            scm_intern("syntax-rules"),
            "syntax-rules",
            "ellipsis not allowed in this context",
            scm_cons(self.name.get(), Value::encode_null_value()),
            Value::encode_null_value(),
        );
    }

    fn check_literals(thread: &mut Thread, literals: Value) -> Value {
        let mut h = Value::encode_null_value();
        let mut t = Value::encode_null_value();

        if scm_length(literals).is_none() {
            scm_error(
                scm_intern("syntax-rules"),
                "syntax-rules",
                "syntax-rules: bad literal list",
                literals,
                Value::encode_null_value(),
            );
        }

        let mut lp = literals;

        while lp.is_pair() {
            let mut lit = scm_car(lp);

            if lit.is_symbol() || lit.is_identifier() {
                gc_protect!(thread => h, t, lp, lit => scm_append1!(&mut h, &mut t, lit));
            } else {
                scm_error(
                    scm_intern("syntax-rules"),
                    "syntax-rules",
                    "syntax-rules: bad literal list",
                    literals,
                    Value::encode_null_value(),
                );
            }
        }

        h
    }

    /// Compile a pattern or a template.
    /// In the pattern, we replace variables to identifiers.
    /// We also recognize pattern variables, and replace them for PVARs in
    /// the pattern, and for PVREFs in the template.
    /// When encounters a repeatable subpattern, replace it with
    /// SyntaxPattern node.
    fn compile_rule1(
        &mut self,
        thread: &mut Thread,
        mut form: Value,
        mut spat: Value,
        patternp: bool,
    ) -> Value {
        if form.is_pair() {
            let h = Value::encode_null_value();
            let t = Value::encode_null_value();

            let mut ellipsis_seen = false;

            if scm_cdr(form).is_bool() && self.is_ellipsis(scm_car(form)) {
                if patternp {
                    scm_error(
                        scm_intern("syntax-rules"),
                        "syntax-rules",
                        "syntax-rules: <ellipsis> can't appear at the beginning of list/vectors",
                        spat,
                        form,
                    );
                }

                let mut save_elli = self.ellipsis.get();
                self.ellipsis.set(Value::encode_bool_value(false));
                let r = gc_protect!(thread => save_elli => self.compile_rule1(thread, scm_cadr(form), spat, patternp));
                self.ellipsis.set(save_elli);
                return r;
            }

            let mut pp = form;

            while pp.is_pair() {
                if self.ellipsis_following(pp) {
                    if patternp && ellipsis_seen {
                        self.bad_ellipsis();
                    }

                    ellipsis_seen = true;
                    let mut base = scm_car(pp);
                    pp = scm_cdr(pp);

                    let mut num_trailing = 0;
                    let mut ellipsis_nesting = 1;

                    if patternp {
                        let mut trailing = scm_cdr(pp);

                        while trailing.is_pair() {
                            num_trailing += 1;
                            trailing = scm_cdr(trailing);
                        }
                    } else {
                        while self.ellipsis_following(pp) {
                            ellipsis_nesting += 1;
                            if !scm_cdr(pp).is_pair() {
                                break;
                            }

                            pp = scm_cdr(pp);
                        }
                    }

                    if self.maxlev < spat.cast_as::<SyntaxPattern>().level as u32 + ellipsis_nesting
                    {
                        self.maxlev =
                            spat.cast_as::<SyntaxPattern>().level as u32 + ellipsis_nesting;
                    }

                    let mut outermost = gc_protect!(thread => form, spat, base, pp => thread.make_syntax_pattern(Value::encode_null_value(),Value::encode_null_value(), spat.cast_as::<SyntaxPattern>().level + 1, num_trailing));

                    let mut outer = outermost;

                    for i in 1..ellipsis_nesting {
                        let inner = gc_protect!(thread => form, spat, base, pp, outer, outermost => thread.make_syntax_pattern(Value::encode_null_value(),Value::encode_null_value(), spat.cast_as::<SyntaxPattern>().level + i as i16 + 1, 0));
                        outer.cast_as::<SyntaxPattern>().pattern = inner;
                    }
                }
            }
        }

        form
    }
}
