#![allow(dead_code)]
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

use std::{
    mem::{offset_of, size_of},
    ops::{Index, IndexMut},
};

use crate::{
    compaux::{scm_identifier_env, scm_make_identifier, scm_unwrap_syntax, scm_wrap_identifier},
    compile::{
        cenv_exp_name, cenv_frames, cenv_module, cenv_set_exp_name, env_lookup_int, make_cenv,
    },
    runtime::cmp::scm_equal,
    runtime::fun::scm_make_closed_native_procedure,
    runtime::list::{
        list_to_vector, scm_acons, scm_append2, scm_assq, scm_cons, scm_is_list, scm_last_pair,
        scm_length, scm_memq, scm_reversex, vector_to_list,
    },
    runtime::module::scm_identifier_to_bound_gloc,
    runtime::object::{Identifier, Macro, Module, ObjectHeader, ScmResult, Type},
    runtime::{error::scm_raise_proc, string::make_string},
    runtime::{
        fun::{SCM_PRIM_ERI_MACRO, SCM_PRIM_ER_MACRO},
        value::Value,
    },
    runtime::{symbol::make_symbol, value::scm_box},
    scm_append, scm_append1, scm_for_each,
    vm::callframe::CallFrame,
};
use once_cell::sync::{Lazy, OnceCell};
use rsgc::{
    heap::{heap, root_processor::SimpleRoot},
    prelude::{Allocation, Handle, Object},
    system::array::Array,
    thread::Thread,
};

use super::{
    fun::scm_make_subr,
    list::{scm_assoc_ref, scm_list},
    module::{scm_capy_module, scm_define, scm_internal_module},
    symbol::Intern,
    vector::scm_vector_copy,
};

// no need to trace these symbols, they are registered in the global symbol table
// that is already traced.
pub static ELLIPSIS: Lazy<Value> = Lazy::new(|| make_symbol("...", true));
pub static UNDERBAR: Lazy<Value> = Lazy::new(|| make_symbol("_", true));

#[repr(C)]
pub struct SyntaxRules {
    pub(crate) header: ObjectHeader,
    pub name: Value,
    pub num_rules: u32,
    pub num_init_rules: u32,
    pub max_num_pvars: u32,
    pub module: Handle<Module>,
    pub env: Value,
    rules: [SyntaxRuleBranch; 0],
}

pub struct SyntaxRuleBranch {
    pub pattern: Value,
    pub template: Value,
    pub num_pvars: u32,
    pub max_level: u32,
}

unsafe impl Object for SyntaxRules {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.name.trace(visitor);
        self.module.trace(visitor);
        self.env.trace(visitor);
    }

    fn trace_range(&self, from: usize, to: usize, visitor: &mut dyn rsgc::prelude::Visitor) {
        if from >= self.num_init_rules as usize {
            return;
        }
        for i in from..to {
            unsafe {
                assert!(i < self.num_init_rules as usize, "Index out of bounds");
                let branch = self.rules.get_unchecked(i);

                branch.pattern.trace(visitor);
                branch.template.trace(visitor);
            }
        }
    }
}

impl Index<usize> for SyntaxRules {
    type Output = SyntaxRuleBranch;

    fn index(&self, index: usize) -> &Self::Output {
        assert!(index < self.num_rules as usize, "Index out of bounds");

        unsafe { self.rules.get_unchecked(index) }
    }
}

impl IndexMut<usize> for SyntaxRules {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        assert!(index < self.num_rules as usize, "Index out of bounds");

        unsafe { self.rules.get_unchecked_mut(index) }
    }
}

unsafe impl Allocation for SyntaxRules {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = size_of::<SyntaxRuleBranch>();
    const VARSIZE_NO_HEAP_PTRS: bool = false;
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(SyntaxRules, num_rules);
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(SyntaxRules, num_init_rules);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(SyntaxRules, rules);
}

#[repr(C)]
struct SyntaxPattern {
    header: ObjectHeader,
    pattern: Value,
    vars: Value,
    level: i16,
    num_following_items: i16,
}

unsafe impl Object for SyntaxPattern {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.pattern.trace(visitor);
        self.vars.trace(visitor);
    }
}
unsafe impl Allocation for SyntaxPattern {}

#[repr(C)]
struct PVRef {
    header: ObjectHeader,
    level: i16,
    count: i16,
}

unsafe impl Object for PVRef {}
unsafe impl Allocation for PVRef {}

impl Value {
    fn is_pvref(self) -> bool {
        self.is_xtype(Type::Pvref)
    }

    fn pvref(self) -> Handle<PVRef> {
        debug_assert!(self.is_pvref());

        unsafe { std::mem::transmute(self) }
    }

    fn is_syntax_pattern(self) -> bool {
        self.is_xtype(Type::Synpattern)
    }

    fn syntax_pattern(self) -> Handle<SyntaxPattern> {
        debug_assert!(self.is_syntax_pattern());

        unsafe { std::mem::transmute(self) }
    }
}

fn make_syntax_pattern(t: &mut Thread, level: i16, num_following: i16) -> Handle<SyntaxPattern> {
    t.allocate(SyntaxPattern {
        header: ObjectHeader::new(Type::Synpattern),
        pattern: Value::encode_null_value(),
        vars: Value::encode_null_value(),
        level,
        num_following_items: num_following,
    })
}

fn make_pvref(t: &mut Thread, level: i16, count: i16) -> Handle<PVRef> {
    t.allocate(PVRef {
        header: ObjectHeader::new(Type::Pvref),
        level,
        count,
    })
}

struct PatternContext {
    name: Value,
    form: Value,
    module: Handle<Module>,
    literals: Value,
    pvars: Value,
    renames: Value,
    ellipsis: Value,
    pvcnt: u32,
    maxlev: u32,
    env: Value,
}

const PVREF_LEVEL_MAX: u16 = 0xff;
const PVREF_COUNT_MAX: u32 = 0xff;

impl PatternContext {
    fn add_pvar(
        &mut self,
        t: &mut Thread,
        mut pat: Handle<SyntaxPattern>,
        pvar: Value,
    ) -> Result<Value, Value> {
        if pat.level > PVREF_LEVEL_MAX as i16 {
            return Err(make_string(
                t,
                &format!(
                    "Pattern levels too deeply nested in the macro definition of {:?}",
                    self.name
                ),
            )
            .into());
        }

        if self.pvcnt > PVREF_COUNT_MAX {
            return Err(make_string(
                t,
                &format!(
                    "Too many pattern variables in the macro definition of {:?}",
                    self.name
                ),
            )
            .into());
        }

        let pvref = make_pvref(t, pat.level, self.pvcnt as _);
        self.pvcnt += 1;
        self.pvars = scm_acons(t, pvar, pvref.into(), self.pvars);
        t.write_barrier(pat);
        pat.vars = scm_cons(t, pvref.into(), pat.vars);

        Ok(pvref.into())
    }

    fn pvar_to_pvref(
        &mut self,
        t: &mut Thread,
        pat: Handle<SyntaxPattern>,
        pvar: Value,
    ) -> Result<Value, Value> {
        let q = scm_assq(pvar, self.pvars);

        if !q.is_pair() {
            return Ok(pvar);
        }

        let pvref = q.cdr();

        if pvref.pvref().level > pat.level {
            return Err(make_string(
                t,
                &format!(
                    "{:?}: Pattern variable {:?} is used in wrong level: {:?}",
                    self.name, pvar, self.form
                ),
            )
            .into());
        }

        Ok(pvref)
    }

    fn is_ellipsis(&self, obj: Value) -> bool {
        if self.ellipsis.is_false() {
            return false; // inside (... template)
        } else if self.ellipsis.is_true() {
            return scm_er_compare(*ELLIPSIS, obj, self.module, self.env);
        } else {
            self.ellipsis == obj
        }
    }

    fn ellipsis_following(&self, pat: Value) -> bool {
        pat.cdr().is_pair() && self.is_ellipsis(pat.cadr())
    }

    fn bad_ellipsis(&self) -> Value {
        make_string(
            Thread::current(),
            &format!(
                "Bad ellipsis usage in macro definition of {:?}: {:?}",
                self.name, self.form
            ),
        )
        .into()
    }

    fn check_literals(thr: &mut Thread, literals: Value) -> Result<Value, Value> {
        let mut h = Value::encode_null_value();
        let mut t = Value::encode_null_value();

        if !scm_is_list(literals) {
            return Err(make_string(
                thr,
                &format!("Bad literal list in macro definition : {:?}", literals),
            )
            .into());
        }

        scm_for_each!(lp, literals, {
            let lit = lp.car();
            if lit.is_symbol() || lit.is_wrapped_identifier() {
                scm_append1!(thr, &mut h, &mut t, lit);
            } else {
                return Err(make_string(
                    thr,
                    &format!("Bad literal in macro definition: {:?}", literals),
                )
                .into());
            }
        });

        Ok(h)
    }

    /* Compile a pattern or a template.
       In the pattern, we replace variables to identifiers.
       We also recognize pattern variables, and replace them for PVARs in
       the pattern, and for PVREFs in the template.
       When encounters a repeatable subpattern, replace it with
       SyntaxPattern node.
    */
    fn compile_rule1(
        &mut self,
        thread: &mut Thread,
        form: Value,
        mut spat: Handle<SyntaxPattern>,
        patternp: bool,
    ) -> Result<Value, Value> {
        if form.is_pair() {
            let mut h = Value::encode_null_value();
            let mut t = h;

            let mut ellipsis_seen = false;

            if form.cdr().is_pair() && self.is_ellipsis(form.car()) {
                if patternp {
                    return Err(make_string(thread, &format!("in definition of macro {:?}: <ellipsis> can't appear at the beginning of list/vectors: {:?}", self.name, form)
                    ).into());
                }

                let save_elli = self.ellipsis;
                self.ellipsis = Value::encode_bool_value(false);

                let r = self.compile_rule1(thread, form.cadr(), spat, false)?;
                self.ellipsis = save_elli;
                return Ok(r);
            }
            let mut pp;
            scm_for_each!(declared pp, form, {
                if self.ellipsis_following(pp) {

                    if patternp && ellipsis_seen {
                        return Err(make_string(thread,
                            &format!("in definition of macro {:?}: Ellipses are not allowed to appear within the same list/vector more than once in a pattern: {:?}", self.name, form)
                        ).into())
                    }

                    ellipsis_seen = true;

                    let base = pp.car();

                    pp = pp.cdr();


                    let mut num_trailing = 0;
                    let mut ellipsis_nesting = 1;

                    if patternp {
                        let mut trailing = pp.cdr();

                        while trailing.is_pair() {
                            num_trailing += 1;
                            trailing = trailing.cdr();
                        }
                    } else {
                        while self.ellipsis_following(pp) {
                            ellipsis_nesting += 1;
                            if !pp.cdr().is_pair() {
                                break;
                            }

                            pp = pp.cdr();
                        }
                    }

                    if self.maxlev < spat.level as u32 + ellipsis_nesting {
                        self.maxlev  = spat.level as u32 + ellipsis_nesting + 1;
                    }

                    let mut outermost = make_syntax_pattern(thread, spat.level + 1, num_trailing);

                    let mut outer = outermost;

                    for i in 1..ellipsis_nesting {
                        let inner = make_syntax_pattern(thread, spat.level + i as i16 + 1, 0);
                        thread.write_barrier(outer);
                        outer.pattern = inner.into();
                        outer = inner;
                    }

                    thread.write_barrier(outer);
                    outer.pattern = self.compile_rule1(thread, base, outer, patternp)?;
                    thread.write_barrier(outermost);
                    outermost.vars = outer.vars;

                    template_append1(thread, &mut h, &mut t, outermost.into());

                    if !patternp {
                        let mut vp;

                        if outermost.vars.is_null() {
                            return Err(
                                make_string(thread, &format!("in definition of macro {:?}: a template contains repetition of constant form: {:?}", self.name, form)).into()
                            );
                        }

                        scm_for_each!(declared vp, outermost.vars, {
                            if vp.car().pvref().level >= outermost.level {
                                break;
                            }
                        });

                        if vp.is_null() {
                            return Err(
                                make_string(thread, &format!("in definition of macro {:?}: template's ellipsis nesting is deeper than pattern: {:?}", self.name, form)).into()
                            );
                        }
                    }

                    thread.write_barrier(spat);
                    spat.vars = scm_append2(thread, spat.vars, outermost.vars);

                } else {

                    let r = self.compile_rule1(thread, pp.car(), spat, patternp)?;
                    //println!("append {:?} head={:?}", r, h);
                    template_append1(thread, &mut h, &mut t, r);
                }
            });

            if !pp.is_null() {
                let r = self.compile_rule1(thread, pp, spat, patternp)?;

                template_append(thread, &mut h, &mut t, r);
            }

            return Ok(h);
        } else if form.is_vector() {
            let ls = vector_to_list(thread, form.vector());
            let r = self.compile_rule1(thread, ls, spat, patternp)?;
            return Ok(list_to_vector(thread, r).into());
        }

        if form.is_symbol() || form.is_wrapped_identifier() {
            if self.is_ellipsis(form) {
                return Err(self.bad_ellipsis());
            }

            if !scm_memq(form, self.literals).is_false() {
                if patternp {
                    let m = self.module;
                    let e = self.env;
                    return Ok(rename_variable(form, &mut self.renames, m, e));
                } else {
                    return Ok(form);
                }
            }

            if patternp && scm_er_compare(form, make_symbol("_", true), self.module, self.env) {
                return Ok(make_symbol("_", true));
            }

            if patternp {
                return self.add_pvar(thread, spat, form);
            } else {
                let pvref = self.pvar_to_pvref(thread, spat, form)?;

                if pvref == form {
                    return Ok(form);
                } else {
                    thread.write_barrier(spat);
                    spat.vars = scm_cons(thread, pvref, spat.vars);
                    return Ok(pvref);
                }
            }
        }

        Ok(form)
    }
}

fn rename_variable(var: Value, id_alist: &mut Value, module: Handle<Module>, env: Value) -> Value {
    let p = scm_assq(var, *id_alist);
    if p.is_pair() {
        //println!("rename_variable assq {:?} {:?}", var, p.cdr());
        return p.cdr();
    }

    let id = if var.is_symbol() {
        scm_make_identifier(var, Some(module), env)
    } else {
        scm_wrap_identifier(var.identifier())
    };

    *id_alist = scm_acons(Thread::current(), var, id.into(), *id_alist);
    //println!("rename_variable {:?} {:?}", var, Value::encode_object_value(id));
    id.into()
}

fn template_append1(t: &mut Thread, head: &mut Value, tail: &mut Value, elt: Value) {
    if head.is_null() {
        *head = scm_cons(t, elt, Value::encode_null_value());
        *tail = *head;
        return;
    }

    let p = scm_cons(t, elt, Value::encode_null_value());
    t.write_barrier(tail.pair());
    tail.pair().cdr = p;
    *tail = p;
}

fn template_append(t: &mut Thread, head: &mut Value, tail: &mut Value, rest: Value) {
    if head.is_null() {
        *head = rest;
        *tail = if rest.is_pair() {
            scm_last_pair(rest)
        } else {
            rest
        };

        return;
    }

    t.write_barrier(tail.pair());
    tail.pair().cdr = rest;
    if rest.is_pair() {
        *tail = scm_last_pair(rest);
    }
}

pub fn scm_er_compare(a: Value, b: Value, module: Handle<Module>, frames: Value) -> bool {
    if (a.is_symbol() || a.is_wrapped_identifier()) && (b.is_symbol() || b.is_wrapped_identifier())
    {
        let a1 = env_lookup_int(a, module, frames);
        let b1 = env_lookup_int(b, module, frames);

        if a1 == b1 {
            return true;
        }

        if !a1.is_wrapped_identifier() || !b1.is_wrapped_identifier() {
            return false;
        }

        is_free_identifier_eq(a1.identifier(), b1.identifier())
    } else {
        false
    }
}

pub fn is_free_identifier_eq(id1: Handle<Identifier>, id2: Handle<Identifier>) -> bool {
    if id1.as_ptr() == id2.as_ptr() {
        return true;
    }

    let b1 = env_lookup_int(id1.into(), id1.module.module(), scm_identifier_env(id1));
    let b2 = env_lookup_int(id2.into(), id2.module.module(), scm_identifier_env(id2));

    if b1.is_wrapped_identifier() && b2.is_wrapped_identifier() {
        let g1 = scm_identifier_to_bound_gloc(id1);
        let g2 = scm_identifier_to_bound_gloc(id2);

        if let (Some(g1), Some(g2)) = (g1, g2) {
            return g1.as_ptr() == g2.as_ptr();
        } else {
            return scm_unwrap_syntax(id1.into(), false) == scm_unwrap_syntax(id2.into(), false);
        }
    } else {
        b1 == b2
    }
}

fn compile_rules(
    name: Value,
    ellipsis: Value,
    literals: Value,
    rules: Value,
    module: Handle<Module>,
    env: Value,
) -> Result<Value, Value> {
    let t = Thread::current();

    let num_rules = scm_length(rules)
        .ok_or_else(|| make_string(t, &format!("Bad syntax-rules form",)).into())?;

    if scm_length(literals).is_none() {
        return Err(make_string(
            t,
            &format!("Bad syntax-rules form: bad literals: {}", literals),
        )
        .into());
    }
    let mut ctx = PatternContext {
        name,
        ellipsis,
        literals: PatternContext::check_literals(Thread::current(), literals)?,
        module,
        env,
        renames: Value::encode_null_value(),
        maxlev: 0,
        form: Value::encode_null_value(),
        pvars: Value::encode_null_value(),
        pvcnt: 0,
    };

    if !ellipsis.is_false() {
        scm_for_each!(cp, ctx.literals, {
            if ctx.is_ellipsis(cp.car()) {
                ctx.ellipsis = Value::encode_bool_value(false);
                break;
            }
        });
    }

    let mut sr = t.allocate_varsize::<SyntaxRules>(num_rules as _);

    unsafe {
        let sr = sr.assume_init_mut();
        sr.header = ObjectHeader::new(Type::Synrules);
        sr.name = name;
        sr.num_rules = num_rules as _;
        sr.max_num_pvars = 0;
        sr.module = module;
        sr.env = env;

        let mut rp = rules;

        for i in 0..num_rules {
            let rule = rp.car();

            if scm_length(rule) != Some(2) {
                return Err(
                    make_string(t, &format!("Bad syntax-rules form: bad rule: {}", rule)).into(),
                );
            }

            let mut pat = make_syntax_pattern(t, 0, 0);
            let mut tmpl = make_syntax_pattern(t, 0, 0);
            ctx.pvars = Value::encode_null_value();
            ctx.pvcnt = 0;
            ctx.maxlev = 0;

            // println!("rule {:?}", rule);

            ctx.form = rule.car();

            if !ctx.form.is_pair() {
                return Err(make_string(t, "Bad syntax-rules form: bad pattern").into());
            }
            t.write_barrier(pat);

            pat.pattern = ctx.compile_rule1(t, ctx.form.cdr(), pat, true)?;
            //println!("1) Compiled {:?} to {:?}",ctx.form.cdr(), pat.pattern);
            ctx.form = rule.cadr();
            t.write_barrier(tmpl);
            tmpl.pattern = ctx.compile_rule1(t, ctx.form, tmpl, false)?;
            let rule = sr.rules.as_mut_ptr().add(i).as_mut().unwrap();
            rule.pattern = pat.pattern;
            rule.template = tmpl.pattern;
            rule.num_pvars = ctx.pvcnt as _;
            rule.max_level = ctx.maxlev as _;
            sr.num_init_rules += 1;
            if ctx.pvcnt > sr.max_num_pvars {
                sr.max_num_pvars = ctx.pvcnt;
            }

            rp = rp.cdr();
        }
    }

    unsafe { Ok(sr.assume_init().into()) }
}

pub fn scm_compile_syntax_rules(
    mut name: Value,
    _src: Value,
    ellipsis: Value,
    literals: Value,
    rules: Value,
    module: Handle<Module>,
    env: Value,
) -> Result<Value, Value> {
    if name.is_wrapped_identifier() {
        name = name.identifier().name;
    }

    let sr = compile_rules(name, ellipsis, literals, rules, module, env)?;

    let t = Thread::current();
    let subr = scm_make_closed_native_procedure(t, name, synrule_transform, 2, 2, &[sr]);

    Ok(scm_make_macro(t, subr.into(), name).into())
}

#[derive(Debug)]
struct MatchVar {
    branch: Value,
    sprout: Value,
    root: Value,
}

unsafe impl Object for MatchVar {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.branch.trace(visitor);
        self.sprout.trace(visitor);
        self.root.trace(visitor);
    }
}

unsafe impl Allocation for MatchVar {}

fn get_pvref_value(pvref: Value, mvec: &[MatchVar], indices: &[i32], exlev: &mut i32) -> Value {
    let level = pvref.pvref().level;
    let count = pvref.pvref().count;

    let mut tree = mvec[count as usize].root;

    for i in 1..=level {
        for _ in 0..indices[i as usize] {
            if !tree.is_pair() {
                *exlev = i as i32;
                return Value::encode_undefined_value();
            }

            tree = tree.cdr();
        }

        if !tree.is_pair() {
            *exlev = i as i32;
            return Value::encode_undefined_value();
        }

        tree = tree.car();
    }

    tree
}

fn grow_branch(t: &mut Thread, rec: &mut MatchVar, level: i32) {
    if level <= 1 {
        return;
    }

    if rec.root.is_null() {
        rec.sprout = scm_cons(t, Value::encode_null_value(), Value::encode_null_value());
        rec.root = rec.sprout;

        if level == 2 {
            return;
        }
    }

    let mut trunc = rec.root;
    let mut i = 1;
    while i < level - 1 {
        scm_for_each!(declared trunc, trunc, {
            if trunc.cdr().is_null() {
                break;
            }
        });

        if trunc.car().is_null() {
            i += 1;
            while i < level - 1 {
                t.write_barrier(trunc.pair());
                trunc.pair().car =
                    scm_cons(t, Value::encode_null_value(), Value::encode_null_value());

                trunc = trunc.car();
                i += 1;
            }

            rec.sprout = scm_cons(t, Value::encode_null_value(), Value::encode_null_value());
            trunc.pair().car = rec.sprout;
            return;
        }

        trunc = trunc.car();
        i += 1;
    }

    scm_for_each!(declared trunc, trunc, {
        if trunc.cdr().is_null() {
            rec.sprout = scm_cons(t, Value::encode_null_value(), Value::encode_null_value());
            t.write_barrier(trunc.pair());
            trunc.pair().cdr = rec.sprout;
            break;
        }
    });
}

fn enter_subpattern(t: &mut Thread, subpat: Handle<SyntaxPattern>, mvec: &mut [MatchVar]) {
    scm_for_each!(pp, subpat.vars, {
        let pvref = pp.car();
        let count = pvref.pvref().count;
        grow_branch(t, &mut mvec[count as usize], subpat.level as _);
    });
}

fn exit_subpattern(t: &mut Thread, subpat: Handle<SyntaxPattern>, mvec: &mut [MatchVar]) {
    scm_for_each!(pp, subpat.vars, {
        let pvref = pp.car();
        let count = pvref.pvref().count;

        if pvref.pvref().level == subpat.level {
            if subpat.level == 1 {
                mvec[count as usize].root = scm_reversex(t, mvec[count as usize].branch);
            } else {
                t.write_barrier(mvec[count as usize].sprout.pair());
                mvec[count as usize].sprout.pair().car =
                    scm_reversex(t, mvec[count as usize].branch);
                mvec[count as usize].branch = Value::encode_null_value();
            }
        }
    });
}

fn match_insert(t: &mut Thread, pvref: Value, matched: Value, mvec: &mut [MatchVar]) {
    let count = pvref.pvref().count;
    if pvref.pvref().level == 0 {
        //println!("matched {:?}", matched);
        mvec[count as usize].root = matched;
    } else {
        //println!("matched {:?}", matched);
        mvec[count as usize].branch = scm_cons(t, matched, mvec[count as usize].branch);
    }
}

fn match_subpattern(
    t: &mut Thread,
    mut form: Value,
    pat: Handle<SyntaxPattern>,
    rest: Value,
    module: Handle<Module>,
    env: Value,
    mvec: &mut [MatchVar],
) -> bool {
    let mut limit = 0;

    let mut p = form;

    while p.is_pair() {
        p = p.cdr();
        limit += 1;
    }

    limit -= pat.num_following_items;

    enter_subpattern(t, pat, mvec);

    while limit > 0 {
        if !match_synrule(t, form.car(), pat.pattern, module, env, mvec) {
            return false;
        }

        form = form.cdr();
        limit -= 1;
    }

    exit_subpattern(t, pat, mvec);

    match_synrule(t, form, rest, module, env, mvec)
}

fn match_synrule(
    t: &mut Thread,
    mut form: Value,
    mut pattern: Value,
    module: Handle<Module>,
    env: Value,
    mvec: &mut [MatchVar],
) -> bool {
    if pattern.is_pvref() {
        match_insert(t, pattern, form, mvec);
        return true;
    }

    if pattern == *UNDERBAR {
        return true; // unconditional match
    }

    if pattern.is_wrapped_identifier() {
        return scm_er_compare(pattern, form, module, env);
    }

    if pattern.is_syntax_pattern() {
        return match_subpattern(
            t,
            form,
            pattern.syntax_pattern(),
            Value::encode_null_value(),
            module,
            env,
            mvec,
        );
    }

    if pattern.is_pair() {
        while pattern.is_pair() {
            let elt = pattern.car();

            if elt.is_syntax_pattern() {
                return match_subpattern(
                    t,
                    form,
                    elt.syntax_pattern(),
                    pattern.cdr(),
                    module,
                    env,
                    mvec,
                );
            } else if !form.is_pair() {
                return false;
            } else {
                if !match_synrule(t, form.car(), elt, module, env, mvec) {
                    return false;
                }

                pattern = pattern.cdr();
                form = form.cdr();
            }
        }

        if !pattern.is_null() {
            let x = match_synrule(t, form, pattern, module, env, mvec);

            return x;
        } else {
            return form.is_null();
        }
    }

    if pattern.is_vector() {
        if !form.is_vector() {
            return false;
        }

        let plen = pattern.vector_len();
        let mut elli = form.vector_len();

        let flen = elli;
        let mut has_elli = false;

        if plen == 0 {
            return flen == 0;
        }
        for i in 0..plen {
            if pattern.vector_ref(i).is_syntax_pattern() {
                has_elli = true;
                elli = i;
                break;
            }
        }

        if (!has_elli && plen != flen) || (has_elli && plen.wrapping_sub(1) > flen) {
            return false;
        }

        if elli < flen {
            let pat = pattern.vector_ref(elli).syntax_pattern();
            let prest = scm_list(Thread::current(), &pattern.vector()[elli + 1..plen]);
            let frest = scm_list(Thread::current(), &form.vector()[elli..flen]);

            return match_subpattern(t, frest, pat, prest, module, env, mvec);
        } else {
            return true;
        }
    }

    scm_equal(pattern, form)
}

fn alloc_mvec(num_pvars: usize) -> Handle<Array<MatchVar>> {
    Array::new(Thread::current(), num_pvars, |_, _| MatchVar {
        root: Value::encode_null_value(),
        branch: Value::encode_null_value(),
        sprout: Value::encode_null_value(),
    })
}

fn init_mvec(mvec: &mut [MatchVar]) {
    for mv in mvec {
        mv.root = Value::encode_null_value();
        mv.branch = Value::encode_null_value();
        mv.sprout = Value::encode_null_value();
    }
}

fn realize_template_rec(
    thread: &mut Thread,
    sr: Handle<SyntaxRules>,
    mut template: Value,
    mvec: &[MatchVar],
    level: i32,
    indices: &mut [i32],
    id_alist: &mut Value,
    exlev: &mut i32,
) -> Value {
    if template.is_pair() {
        let mut h = Value::encode_null_value();
        let mut t = Value::encode_null_value();

        while template.is_pair() {
            let e = template.car();
            if e.is_syntax_pattern() {
                let r = realize_template_rec(thread, sr, e, mvec, level, indices, id_alist, exlev);

                if r.is_undefined() {
                    return r;
                }

                template_append(thread, &mut h, &mut t, r);
            } else {
                let r = realize_template_rec(thread, sr, e, mvec, level, indices, id_alist, exlev);

                if r.is_undefined() {
                    return r;
                }
                template_append1(thread, &mut h, &mut t, r);
            }

            template = template.cdr();
        }

        if !template.is_null() {
            let r =
                realize_template_rec(thread, sr, template, mvec, level, indices, id_alist, exlev);
            if r.is_undefined() {
                return r;
            }

            if h.is_null() {
                return r;
            }

            template_append(thread, &mut h, &mut t, r);
        }

        return h;
    }

    if template.is_pvref() {
        let v = get_pvref_value(template, mvec, indices, exlev);

        return v;
    }

    if template.is_syntax_pattern() {
        let pat = template.syntax_pattern();
        let mut h = Value::encode_null_value();
        let mut t = Value::encode_null_value();

        indices[level as usize + 1] = 0;

        loop {
            let r = realize_template_rec(
                thread,
                sr,
                pat.pattern,
                mvec,
                level + 1,
                indices,
                id_alist,
                exlev,
            );

            if r.is_undefined() {
                if *exlev < pat.level as i32 {
                    return r;
                } else {
                    return h;
                }
            }

            if pat.pattern.is_syntax_pattern() {
                scm_append!(thread, &mut h, &mut t, r);
            } else {
                scm_append1!(thread, &mut h, &mut t, r);
            }

            indices[level as usize + 1] += 1;
        }
    }

    if template.is_symbol() || template.is_wrapped_identifier() {
        return rename_variable(template, id_alist, sr.module, sr.env);
    }

    template
}
fn realize_template(
    thread: &mut Thread,
    sr: Handle<SyntaxRules>,
    branch: &SyntaxRuleBranch,
    mvec: &[MatchVar],
) -> Value {
    let mut index = vec![0i32; 100];

    let mut exlev = 0;
    let mut idlist = Value::encode_null_value();

    if branch.max_level > 100 {
        index = vec![0; branch.max_level as usize + 1];
    }

    realize_template_rec(
        thread,
        sr,
        branch.template,
        mvec,
        0,
        &mut index,
        &mut idlist,
        &mut exlev,
    )
}

pub fn synrule_expand(
    t: &mut Thread,
    form: Value,
    module: Handle<Module>,
    env: Value,
    sr: Handle<SyntaxRules>,
) -> Result<Value, Value> {
    let mut mvec = alloc_mvec(sr.max_num_pvars as _);

    for i in 0..sr.num_rules {
        t.write_barrier(mvec);
        init_mvec(&mut mvec);
        
        if match_synrule(
            t,
            form.cdr(),
            sr[i as usize].pattern,
            module,
            env,
            &mut mvec,
        ) {
            let expanded = realize_template(t, sr, &sr[i as usize], &mvec);

            return Ok(expanded);
        }
    }

    Err(make_string(
        t,
        &format!("{:?}: no matching syntax rule: {}", form.car(), form.cdr()),
    )
    .into())
}

extern "C" fn synrule_transform(cfr: &mut CallFrame) -> ScmResult {
    let form = cfr.argument(0);
    let cenv = cfr.argument(1);

    assert!(cenv.is_vector());

    let module = cenv_module(cenv);
    let frames = cenv_frames(cenv);

    let sr = cfr.callee.closed_native_procedure()[0];

    match synrule_expand(Thread::current(), form, module, frames, sr.syntax_rules()) {
        Ok(x) => ScmResult::ok(x),
        Err(x) => ScmResult::tail(scm_raise_proc(), &[x]),
    }
}

pub(crate) fn pattern_print(f: &mut std::fmt::Formatter, val: Value) -> std::fmt::Result {
    let pat = val.syntax_pattern();

    write!(
        f,
        "#<pattern {}{:?} {:?}{}>",
        pat.level,
        pat.vars,
        pat.pattern,
        if pat.num_following_items != 0 {
            "..."
        } else {
            ""
        }
    )
}

pub(crate) fn pvref_print(f: &mut std::fmt::Formatter, val: Value) -> std::fmt::Result {
    let pv = val.pvref();

    write!(f, "#<pvref {}.{}>", pv.level, pv.count)
}

pub fn scm_make_macro(t: &mut Thread, transformer: Value, name: Value) -> Handle<Macro> {
    t.allocate(Macro {
        header: ObjectHeader::new(Type::Macro),
        name,
        transformer,
    })
}

extern "C" fn free_identifier_eq(cfr: &mut CallFrame) -> ScmResult {
    let id1 = cfr.argument(0);
    let id2 = cfr.argument(1);
    if !id1.is_wrapped_identifier() && !id2.is_wrapped_identifier() {
        return ScmResult::ok(false);
    }

    if id1 == id2 {
        return ScmResult::ok(true);
    }

    let b1 = env_lookup_int(
        id1,
        id1.identifier().module.module(),
        scm_identifier_env(id1.identifier()),
    );
    let b2 = env_lookup_int(
        id2,
        id2.identifier().module.module(),
        scm_identifier_env(id2.identifier()),
    );

    if b1.is_wrapped_identifier() && b2.is_wrapped_identifier() {
        let g1 = scm_identifier_to_bound_gloc(id1.identifier());
        let g2 = scm_identifier_to_bound_gloc(id2.identifier());

        if let (Some(g1), Some(g2)) = (g1, g2) {
            return ScmResult::ok(g1.as_ptr() == g2.as_ptr());
        } else {
            return ScmResult::ok(scm_unwrap_syntax(id1, false) == scm_unwrap_syntax(id2, false));
        }
    }

    ScmResult::ok(b1 == b2)
}

fn er_rename(form: Value, dict: Value, module: Value, env: Value) -> (Value, Value) {
    if form.is_identifier() {
        let id = scm_assoc_ref(dict, form, |x, y| x == y, None);
        if !id.is_false() {
            (id.cdr(), dict)
        } else {
            let id = scm_make_identifier(form, Some(module.module()), env);
            (
                id.into(),
                scm_acons(Thread::current(), form, id.into(), dict),
            )
        }
    } else if form.is_pair() {
        let (a, dict) = er_rename(form.car(), dict, module, env);
        let (d, dict) = er_rename(form.cdr(), dict, module, env);

        if a == form.car() && d == form.cdr() {
            (form, dict)
        } else {
            (scm_cons(Thread::current(), a, d), dict)
        }
    } else if form.is_vector() {
        let t = Thread::current();
        let mut dict = dict;
        let mut vec = form;
        let mut copied = false;
        for i in 0..form.vector_len() {
            let (e, ndict) = er_rename(form.vector_ref(i), dict, module, env);
            if e == form.vector_ref(i) {
                continue;
            }
            if !copied {
                vec = scm_vector_copy(t, vec.vector()).into();
                copied = true;
            }
            t.write_barrier(form.vector());
            form.vector_set(i, e);
            dict = ndict;
        }

        (vec, dict)
    } else {
        println!("ayo");
        (form, dict)
    }
}

extern "C" fn er_rename_proc(cfr: &mut CallFrame) -> ScmResult {
    let form = cfr.argument(0);
    let dict = cfr.argument(1);
    let module = cfr.argument(2);
    let env = cfr.argument(3);

    let (form, dict) = er_rename(form, dict, module, env);

    ScmResult::ok(scm_cons(Thread::current(), form, dict))
}

pub fn make_er_transformer<const HAS_INJECT: bool>(xformer: Value, def_env: Value) -> Value {
    let def_module = cenv_module(def_env);
    let def_frames = cenv_frames(def_env);

    extern "C" fn expand<const HAS_INJECT: bool>(cfr: &mut CallFrame) -> ScmResult {
        let form = cfr.argument(0);
        let use_env = cfr.argument(1);

        let use_module = cenv_module(use_env);
        let use_frames = cenv_frames(use_env);

        let def_module = cfr.callee().closed_native_procedure()[0];
        let def_frames = cfr.callee().closed_native_procedure()[1];
        let xformer = cfr.callee().closed_native_procedure()[2];

        let dict = scm_box(Value::encode_null_value());

        let rename_proc: Value = scm_make_closed_native_procedure(
            Thread::current(),
            "%rename".intern().into(),
            {
                extern "C" fn rename(cfr: &mut CallFrame) -> ScmResult {
                    let sym = cfr.argument(0);
                    let clos = cfr.callee().closed_native_procedure();

                    let dict = clos[0];
                    let def_module = clos[1];
                    let def_frames = clos[2];

                    let (id, dict_) = er_rename(sym, dict.box_ref(), def_module, def_frames);
                    Thread::current().write_barrier(dict.r#box());
                    dict.box_set(dict_);

                    ScmResult::ok(id)
                }
                rename
            },
            1,
            1,
            &[dict, def_module, def_frames],
        )
        .into();

        let compare_proc: Value = scm_make_closed_native_procedure(
            Thread::current(),
            "%compare".intern().into(),
            {
                extern "C" fn compare(cfr: &mut CallFrame) -> ScmResult {
                    let a = cfr.argument(0);
                    let b = cfr.argument(1);

                    let use_module = cfr.callee().closed_native_procedure()[0];
                    let use_frames = cfr.callee().closed_native_procedure()[1];

                    ScmResult::ok(scm_er_compare(a, b, use_module.module(), use_frames))
                }
                compare
            },
            2,
            2,
            &[use_module.into(), use_frames],
        )
        .into();

        let inject = if HAS_INJECT {
            scm_make_closed_native_procedure(
                Thread::current(),
                "%inject".intern().into(),
                {
                    extern "C" fn inject(cfr: &mut CallFrame) -> ScmResult {
                        let sym = cfr.argument(0);
                        let clos = cfr.callee().closed_native_procedure();

                        let dict = clos[0];
                        let use_module = clos[1];
                        let use_frames = clos[2];

                        let (id, dict_) = er_rename(sym, dict.box_ref(), use_module, use_frames);
                        Thread::current().write_barrier(dict.r#box());
                        dict.box_set(dict_);

                        ScmResult::ok(id)
                    }
                    inject
                },
                1,
                1,
                &[dict, use_module.into(), use_frames],
            )
            .into()
        } else {
            Value::encode_bool_value(false)
        };

        if HAS_INJECT {
            ScmResult::tail(xformer, &[form, rename_proc, compare_proc, inject])
        } else {
            ScmResult::tail(xformer, &[form, rename_proc, compare_proc])
        }
    }

    let mut transformer = scm_make_closed_native_procedure(
        Thread::current(),
        "%er-transformer".intern().into(),
        expand::<HAS_INJECT>,
        2,
        2,
        &[def_module.into(), def_frames, xformer],
    );
    transformer.header.flags = if HAS_INJECT {
        SCM_PRIM_ERI_MACRO
    } else {
        SCM_PRIM_ER_MACRO
    } as u32;
    scm_make_macro(
        Thread::current(),
        transformer.into(),
        cenv_exp_name(def_env),
    )
    .into()
}

pub fn make_er_transformer_toplevel<const HAS_INJECT: bool>(
    xformer: Value,
    def_module: Value,
    _def_name: Value,
) -> Value {
    let cenv = make_cenv(def_module.module(), Value::encode_null_value());
    cenv_set_exp_name(cenv, _def_name);
    make_er_transformer::<HAS_INJECT>(xformer, cenv)
}

extern "C" fn make_er_transformer_toplevel_proc(cfr: &mut CallFrame) -> ScmResult {
    let xformer = cfr.argument(0);
    let def_module = cfr.argument(1);
    let def_name = cfr.argument(2);
    let has_inject = cfr.argument(3);

    let xformer = if !has_inject.is_false() {
        make_er_transformer_toplevel::<true>(xformer, def_module, def_name)
    } else {
        make_er_transformer_toplevel::<false>(xformer, def_module, def_name)
    };

    ScmResult::ok(xformer)
}

extern "C" fn make_er_transformer_proc(cfr: &mut CallFrame) -> ScmResult {
    let xformer = cfr.argument(0);
    let def_env = cfr.argument(1);
    let _def_package = cfr.argument(2);
    let has_inject = cfr.argument(3);
    let xformer = if !has_inject.is_false() {
        make_er_transformer::<true>(xformer, def_env)
    } else {
        make_er_transformer::<false>(xformer, def_env)
    };

    ScmResult::ok(xformer)
}

pub static MAKE_ER_TRANSFORMER_TOPLEVEL: OnceCell<Value> = OnceCell::new();

pub fn get_make_er_transformer_toplevel() -> Value {
    *MAKE_ER_TRANSFORMER_TOPLEVEL.get_or_init(|| {
        scm_make_closed_native_procedure(
            Thread::current(),
            "%make-er-transformer-toplevel.".intern().into(),
            make_er_transformer_toplevel_proc,
            4,
            4,
            &[],
        )
        .into()
    })
}

pub static MAKE_ER_TRANSFORMER: OnceCell<Value> = OnceCell::new();

pub fn get_make_er_transformer() -> Value {
    *MAKE_ER_TRANSFORMER.get_or_init(|| {
        scm_make_closed_native_procedure(
            Thread::current(),
            "%make-er-transformer.".intern().into(),
            make_er_transformer_proc,
            3,
            3,
            &[],
        )
        .into()
    })
}

extern "C" fn unwrap_syntax(cfr: &mut CallFrame) -> ScmResult {
    let val = cfr.argument(0);
    let immutable = cfr.argument(1).to_bool();

    ScmResult::ok(scm_unwrap_syntax(val, immutable))
}

extern "C" fn identifier_p(cfr: &mut CallFrame) -> ScmResult {
    let val = cfr.argument(0);

    ScmResult::ok(val.is_identifier())
}

pub(crate) fn init_macros() {
    let module = scm_capy_module().module();

    let subr = scm_make_subr("free-identifier=?", free_identifier_eq, 2, 2);
    scm_define(module, "free-identifier=?".intern(), subr).unwrap();

    let subr = scm_make_subr("unwrap-syntax", unwrap_syntax, 2, 2);
    scm_define(module, "unwrap-syntax".intern(), subr).unwrap();

    let subr = scm_make_subr("identifier?", identifier_p, 1, 1);
    scm_define(module, "identifier?".intern(), subr).unwrap();

    let module = scm_internal_module().module();
    let subr = scm_make_subr("er-rename", er_rename_proc, 4, 4);
    scm_define(module, "er-rename".intern(), subr).unwrap();

    heap::heap().add_root(SimpleRoot::new("macros", "mc", |proc| {
        MAKE_ER_TRANSFORMER.get().map(|x| x.trace(proc.visitor()));
        MAKE_ER_TRANSFORMER_TOPLEVEL
            .get()
            .map(|x| x.trace(proc.visitor()));
    }));
}
