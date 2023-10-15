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

use once_cell::sync::Lazy;

use crate::runtime::{symbol::scm_intern, value::Value};

use super::{er_compare, make_identifier, sexpr::*, wrap_identifier, SyntaxEnv, P};

pub static ELLIPSIS: Lazy<Value> = Lazy::new(|| scm_intern("..."));
pub static UNDERBAR: Lazy<Value> = Lazy::new(|| scm_intern("_"));

pub struct PatternContext {
    name: Sexpr,
    form: Sexpr,
    literals: Sexpr,
    pvars: Sexpr,
    renames: Sexpr,
    ellipsis: Sexpr,
    pvcnt: u32,
    maxlev: u32,
    env: Sexpr,
    synenv: P<SyntaxEnv>,
}

const PVREF_LEVEL_MAX: u16 = 0xff;
const PVREF_COUNT_MAX: u32 = 0xff;

impl PatternContext {
    fn add_pvar(&mut self, mut pat: P<SyntaxPattern>, pvar: Sexpr) -> Result<Sexpr, String> {
        if pat.level > PVREF_LEVEL_MAX as i16 {
            return Err(format!(
                "pattern levels too deeply nested in the macro definition of {}",
                self.name
            ));
        }

        if self.pvcnt > PVREF_COUNT_MAX {
            return Err(format!(
                "too many pattern variables in the macro definition of {}",
                self.name
            ));
        }

        let pvref = PVRef {
            level: pat.level as i16,
            count: self.pvcnt as i16,
        };

        self.pvcnt += 1;

        self.pvars = sexp_acons(
            pvar.clone(),
            Sexpr::PVRef(pvref.clone()),
            self.pvars.clone(),
        );
        pat.vars = sexp_cons(Sexpr::PVRef(pvref.clone()), pat.vars.clone());

        Ok(Sexpr::PVRef(pvref))
    }

    fn pvar_to_pvref(&mut self, pat: P<SyntaxPattern>, pvar: Sexpr) -> Result<Sexpr, String> {
        let q = self.pvars.assq(&pvar);

        if !q.is_pair() {
            return Ok(pvar);
        }

        let Sexpr::PVRef(pvref) = q.cdr() else {
            unreachable!("[internal]: pvar_to_pvref: not a PVRef")
        };

        if pvref.level > pat.level {
            return Err(format!(
                "pattern variable {} used out of context in the macro definition of {}",
                pvar, self.name
            ));
        }

        Ok(Sexpr::PVRef(pvref))
    }

    fn is_ellipsis(&self, obj: Sexpr) -> bool {
        if matches!(self.ellipsis, Sexpr::Boolean(false)) {
            return false; // inside (... template)
        } else if matches!(self.ellipsis, Sexpr::Boolean(true)) {
            return er_compare(
                Sexpr::Symbol(*ELLIPSIS),
                obj,
                self.synenv.clone(),
                self.env.clone(),
            );
        } else {
            sexp_eq(&self.ellipsis, &obj)
        }
    }

    fn ellipsis_following(&self, pat: &Sexpr) -> bool {
        pat.cdr().is_pair() && self.is_ellipsis(pat.cadr())
    }

    fn bad_ellipsis(&self) -> String {
        format!(
            "bad ellipsis usage in macro definition of {}: {}",
            self.name, self.form
        )
    }

    fn check_literals(literals: &Sexpr) -> Result<Sexpr, String> {
        let mut h = Sexpr::Null;
        let mut t = Sexpr::Null;

        if !literals.is_list() {
            return Err(format!(
                "bad literal list in macro definition: {}",
                literals
            ));
        }

        let mut lp = literals.clone();

        while lp.is_pair() {
            let lit = lp.car();
            if matches!(lit, Sexpr::Symbol(_) | Sexpr::Identifier(_)) {
                sexp_append1!(&mut h, &mut t, lit.clone());
            } else {
                return Err(format!("bad literal in macro definition: {}", lit));
            }

            lp = lp.cdr();
        }

        Ok(h)
    }
    /// Compile a pattern or a template.
    /// In the pattern, we replace variables to identifiers.
    /// We also recognize pattern variables, and replace them for PVARs in
    /// the pattern, and for PVREFs in the template.
    /// When encounters a repeatable subpattern, replace it with
    /// SyntaxPattern node.
    fn compile_rule1(
        &mut self,
        form: Sexpr,
        mut spat: P<SyntaxPattern>,
        patternp: bool,
    ) -> Result<Sexpr, String> {
        if form.is_pair() {
            let mut h = Sexpr::Null;
            let mut t = Sexpr::Null;

            let mut ellipsis_seen = false;

            if form.cdr().is_pair() && self.is_ellipsis(form.car()) {
                if patternp {
                    return Err(format!(
                        "in definition of macro {}: <ellipsis> can't appear at the beginning of list/vectors: {}", self.name, self.form
                    ));
                }

                let save_elli = self.ellipsis.clone();
                self.ellipsis = Sexpr::Boolean(false);

                let r = self.compile_rule1(form.cadr(), spat.clone(), false);
                self.ellipsis = save_elli;
                return r;
            }

            let mut pp;

            sexpr_for_each!(declared pp, form, {
                if self.ellipsis_following(&pp) {
                    if patternp && ellipsis_seen {
                        return Err(
                            format!(
                                "in definition of macro {}: <ellipsis> can't appear twice in a pattern: {}",
                                self.name, self.form)
                        );
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
                        while self.ellipsis_following(&pp) {
                            ellipsis_nesting += 1;
                            if !pp.cdr().is_pair() {
                                break;
                            }

                            pp = pp.cdr();
                        }
                    }

                    if self.maxlev < spat.level as u32 + ellipsis_nesting {
                        self.maxlev = spat.level as u32 + ellipsis_nesting + 1;
                    }

                    let mut outermost = P(SyntaxPattern {
                        pattern: Sexpr::Null,
                        vars: Sexpr::Null,
                        level: spat.level + 1,
                        num_following_items: num_trailing,
                    });

                    let mut outer = outermost.clone();

                    for i in 1..ellipsis_nesting {
                        let inner = P(SyntaxPattern {
                            pattern: Sexpr::Null,
                            vars: Sexpr::Null,
                            level: spat.level + i as i16 + 1,
                            num_following_items: 0,
                        });

                        outer.pattern = Sexpr::SyntaxPattern(inner.clone());
                        outer = inner;
                    }

                    outer.pattern = self.compile_rule1(base.clone(), outer.clone(), patternp)?;
                    outermost.vars = outer.vars.clone();

                    template_append1(&mut h, &mut t, Sexpr::SyntaxPattern(outermost.clone()));

                    if !patternp {
                        let mut vp;

                        if outermost.vars.is_null() {
                            return Err(
                                format!(
                                    "in definition of macro {}:  a template contains repetition of constant form: {}",
                                    self.name, self.form
                                )
                            );
                        }

                       // vp = outermost.vars.clone();

                        sexpr_for_each!(declared vp, outermost.vars.clone(), {
                            let Sexpr::PVRef(pvref) = vp.car().clone() else {
                                unreachable!("[internal]: compile_rule1: not a PVRef")
                            };

                            if pvref.level >= outermost.level {
                                break;
                            }
                        });

                        if vp.is_null() {
                            return Err(
                                format!("in definition of macro {}:  template's ellipsis nesting is deeper than pattern: {}", self.name, self.form)
                            );
                        }
                    }

                    spat.vars = sexp_append2(spat.vars.clone(), outermost.vars.clone());
                } else {
                    let r = self.compile_rule1(pp.car(), spat.clone(), patternp)?;

                    template_append1(&mut h, &mut t, r);
                }

                //pp = pp.cdr();
            });

            if !pp.is_null() {
                let r = self.compile_rule1(pp, spat.clone(), patternp)?;

                template_append(&mut h, &mut t, r);
            }
            return Ok(h);
        } else if matches!(form, Sexpr::Vector(_)) {
            let Sexpr::Vector(ref vec) = form else {
                unreachable!("[internal]: compile_rule1: not a vector")
            };

            let patternvec = vec
                .iter()
                .map(|exp| self.compile_rule1(exp.clone(), spat.clone(), patternp))
                .collect::<Result<Vec<_>, _>>()?;

            return Ok(Sexpr::Vector(P(patternvec)));
        }

        if matches!(form, Sexpr::Symbol(_) | Sexpr::Identifier(_)) {
            if self.is_ellipsis(form.clone()) {
                println!("WOOOW {}", form);
                return Err(self.bad_ellipsis());
            }

            if !matches!(self.literals.memq(&form), Sexpr::Boolean(false)) {
                if patternp {
                    return Ok(rename_variable(
                        form,
                        &mut self.renames,
                        self.synenv.clone(),
                        self.env.clone(),
                    ));
                } else {
                    return Ok(form);
                }
            }

            if patternp
                && er_compare(
                    form.clone(),
                    Sexpr::Symbol(*UNDERBAR),
                    self.synenv.clone(),
                    self.env.clone(),
                )
            {
                return Ok(Sexpr::Symbol(*UNDERBAR));
            }

            if patternp {
                return self.add_pvar(spat, form);
            } else {
                let pvref = self.pvar_to_pvref(spat.clone(), form.clone())?;

                if sexp_eq(&pvref, &form) {
                    return Ok(form);
                } else {
                    spat.vars = sexp_cons(pvref.clone(), spat.vars.clone());
                    return Ok(pvref);
                }
            }
        }

        Ok(form)
    }
}

fn template_append1(head: &mut Sexpr, tail: &mut Sexpr, elt: Sexpr) {
    if head.is_null() {
        *head = sexp_cons(elt, Sexpr::Null);
        *tail = head.clone();
        return;
    }

    let p = sexp_cons(elt, Sexpr::Null);
    tail.set_cdr(p.clone());
    *tail = p;
}

fn template_append(head: &mut Sexpr, tail: &mut Sexpr, rest: Sexpr) {
    if head.is_null() {
        *head = rest.clone();
        *tail = if rest.is_pair() {
            rest.last_pair()
        } else {
            rest
        };
        return;
    }

    tail.set_cdr(rest.clone());
    if rest.is_pair() {
        *tail = rest.last_pair();
    }
}

fn rename_variable(
    var: Sexpr,
    id_alist: &mut Sexpr,
    syntax_env: P<SyntaxEnv>,
    env: Sexpr,
) -> Sexpr {
    let p = id_alist.assq(&var);

    if p.is_pair() {
        return p.cdr();
    }

    let id = match var.clone() {
        Sexpr::Symbol(var) => make_identifier(Sexpr::Symbol(var), syntax_env, env),
        Sexpr::Identifier(id) => wrap_identifier(id),
        _ => unreachable!(),
    };

    *id_alist = sexp_acons(var.clone(), Sexpr::Identifier(id.clone()), id_alist.clone());

    Sexpr::Identifier(id)
}

fn compile_rules(
    name: Sexpr,
    ellipsis: Sexpr,
    literals: Sexpr,
    rules: Sexpr,
    syntax_env: P<SyntaxEnv>,
    env: Sexpr,
) -> Result<Sexpr, String> {
    let num_rules = rules
        .list_length()
        .ok_or_else(|| format!("bad syntax-rules: {}", rules))?;

    if literals.list_length().is_none() {
        return Err(format!("bad syntax-rules: {}", literals));
    }

    let mut ctx = PatternContext {
        name,
        ellipsis: ellipsis.clone(),
        literals: PatternContext::check_literals(&literals)?,
        synenv: syntax_env,
        env,
        renames: Sexpr::Null,
        maxlev: 0,
        form: Sexpr::Null,
        pvars: Sexpr::Null,
        pvcnt: 0,
    };

    if !matches!(ellipsis, Sexpr::Boolean(false)) {
        let mut cp = ctx.literals.clone();
        while cp.is_pair() {
            if ctx.is_ellipsis(cp.car()) {
                ctx.ellipsis = Sexpr::Boolean(true);
                break;
            }

            cp = cp.cdr();
        }
    }

    let mut sr = P(SyntaxRules {
        name: ctx.name.clone(),
        max_num_pvars: 0,
        syntax_env: ctx.synenv.clone(),
        env: ctx.env.clone(),
        rules: vec![],
    });

    let mut rp = rules.clone();

    for _ in 0..num_rules {
        let rule = rp.car();

        if rule.list_length() != Some(2) {
            return Err(format!("bad syntax-rules: {}", rules));
        }

        let mut pat = P(SyntaxPattern {
            pattern: Sexpr::Null,
            vars: Sexpr::Null,
            level: 0,
            num_following_items: 0,
        });

        let mut tmpl = P(SyntaxPattern {
            pattern: Sexpr::Null,
            vars: Sexpr::Null,
            level: 0,
            num_following_items: 0,
        });

        ctx.pvars = Sexpr::Null;
        ctx.pvcnt = 0;
        ctx.maxlev = 0;

        ctx.form = rule.car();

        pat.pattern = ctx.compile_rule1(ctx.form.cdr().clone(), pat.clone(), true)?;
        ctx.form = rule.cadr();
        tmpl.pattern = ctx.compile_rule1(ctx.form.clone(), tmpl.clone(), false)?;
        let rule = SyntaxRuleBranch {
            pattern: pat.pattern.clone(),
            template: tmpl.pattern.clone(),
            num_pvars: ctx.pvcnt as _,
            max_level: ctx.maxlev as _,
        };

        sr.rules.push(rule);
        if ctx.pvcnt > sr.max_num_pvars {
            sr.max_num_pvars = ctx.pvcnt;
        }

        rp = rp.cdr();
    }

    return Ok(Sexpr::SyntaxRules(sr));
}

pub fn compile_syntax_rules(
    name: Sexpr,
    ellipsis: Sexpr,
    literals: Sexpr,
    rules: Sexpr,
    syntax_env: P<SyntaxEnv>,
    env: Sexpr,
) -> Result<Sexpr, String> {
    let name = if let Sexpr::Identifier(id) = name {
        id.name.clone()
    } else {
        name
    };

    let sr = compile_rules(name, ellipsis, literals, rules, syntax_env, env)?;

    Ok(sr)
}

#[derive(Clone)]
struct MatchVar {
    branch: Sexpr,
    sprout: Sexpr,
    root: Sexpr,
}

fn get_pvref_value(pvref: &PVRef, mvec: &[MatchVar], indices: &[i32], exlev: &mut i32) -> Sexpr {
    let level = pvref.level;
    let count = pvref.count;

    let mut tree = mvec[count as usize].root.clone();
    for i in 1..=level {
        for _ in 0..indices[i as usize] {
            if !tree.is_pair() {
                *exlev = i as i32;
                return Sexpr::Undefined;
            }

            tree = tree.cdr();
        }

        if !tree.is_pair() {
            *exlev = i as i32;
            return Sexpr::Undefined;
        }

        tree = tree.car();
    }
    //println!("pvref level={},count={} is {}", level, count, tree);
    tree
}

fn grow_branch(rec: &mut MatchVar, level: i32) {
    if level <= 1 {
        return;
    }

    if rec.root.is_null() {
        rec.sprout = sexp_cons(Sexpr::Null, Sexpr::Null);
        rec.root = rec.sprout.clone();

        if level == 2 {
            return;
        }
    }

    let mut trunc = rec.root.clone();
    let mut i = 1;
    while i < level - 1 {
        sexpr_for_each!(declared trunc, trunc.clone(), {
            if trunc.cdr().is_null() {
                break;
            }
        });

        if trunc.car().is_null() {
            i += 1;
            while i < level - 1 {
                trunc.set_car(sexp_cons(Sexpr::Null, Sexpr::Null));
                trunc = trunc.car();
                i += 1;
            }

            rec.sprout = sexp_cons(Sexpr::Null, Sexpr::Null);
            trunc.set_car(rec.sprout.clone());
            return;
        }

        trunc = trunc.car();
        i += 1;
    }

    sexpr_for_each!(declared trunc, trunc.clone(), {
        if trunc.cdr().is_null() {
            rec.sprout = sexp_cons(Sexpr::Null, Sexpr::Null);
            trunc.set_cdr(rec.sprout.clone());
            break;
        }
    });
}

fn enter_subpattern(subpat: P<SyntaxPattern>, mvec: &mut [MatchVar]) {
    /*let mut pp = subpat.vars.clone();

    while pp.is_pair() {
        let Sexpr::PVRef(pvref) = pp.car() else {
            unreachable!("[internal]: enter_subpattern: not a PVRef")
        };

        grow_branch(&mut mvec[pvref.count as usize], subpat.level as i32);
        pp = pp.cdr();
    }*/
    sexpr_for_each!(pp, subpat.vars.clone(), {
        let Sexpr::PVRef(pvref) = pp.car() else {
            unreachable!("[internal]: enter_subpattern: not a PVRef")
        };
        let count = pvref.count;
        grow_branch(&mut mvec[count as usize], subpat.level as i32)
    });
}

fn exit_subpattern(subpat: P<SyntaxPattern>, mvec: &mut [MatchVar]) {
    sexpr_for_each!(pp, subpat.vars.clone(), {
        let Sexpr::PVRef(pvref) = pp.car() else {
            unreachable!("[internal]: exit_subpattern: not a PVRef")
        };
        let count = pvref.count;
        let level = pvref.level;

        if level == subpat.level {
            if subpat.level == 1 {
                mvec[count as usize].root = sexp_reversex(mvec[count as usize].branch.clone());
            } else {
                mvec[count as usize]
                    .sprout
                    .set_car(sexp_reversex(mvec[count as usize].branch.clone()));
                mvec[count as usize].branch = Sexpr::Null;
            }
        }
    });

    /*let mut pp = subpat.vars.clone();

    while pp.is_pair() {
        let Sexpr::PVRef(pvref) = pp.car() else {
            unreachable!("[internal]: exit_subpattern: not a PVRef")
        };

        if pvref.level == subpat.level {
            if subpat.level == 1 {
                mvec[pvref.count as usize].root =
                    sexp_reversex(mvec[pvref.count as usize].branch.clone());
            } else {
                mvec[pvref.count as usize].sprout =
                    sexp_reversex(mvec[pvref.count as usize].branch.clone());

                mvec[pvref.count as usize].branch = Sexpr::Null;
            }
        }
        pp = pp.cdr();
    }*/
}

fn match_insert(pvref: &PVRef, matched: Sexpr, mvec: &mut [MatchVar]) {
    if pvref.level == 0 {
        mvec[pvref.count as usize].root = matched;
    } else {
        mvec[pvref.count as usize].branch =
            sexp_cons(matched, mvec[pvref.count as usize].branch.clone());
    }
}

fn match_subpattern(
    mut form: Sexpr,
    pat: P<SyntaxPattern>,
    rest: Sexpr,
    syntax_env: P<SyntaxEnv>,
    env: Sexpr,
    mvec: &mut [MatchVar],
) -> bool {
    let mut limit = 0;
    let mut p = form.clone();
    while p.is_pair() {
        p = p.cdr();
        limit += 1;
    }
    limit -= pat.num_following_items;

    enter_subpattern(pat.clone(), mvec);

    while limit > 0 {
        if !match_synrule(
            form.car(),
            &pat.pattern,
            syntax_env.clone(),
            env.clone(),
            mvec,
        ) {
            return false;
        }

        form = form.cdr();
        limit -= 1;
    }

    exit_subpattern(pat.clone(), mvec);

    match_synrule(form, &rest, syntax_env.clone(), env.clone(), mvec)
}

fn match_synrule(
    mut form: Sexpr,
    pattern: &Sexpr,
    syntax_env: P<SyntaxEnv>,
    env: Sexpr,
    mvec: &mut [MatchVar],
) -> bool {
    match pattern {
        Sexpr::PVRef(pvref) => {
            match_insert(pvref, form, mvec);
            return true;
        }

        Sexpr::Symbol(x) if *x == *UNDERBAR => {
            return true;
        }

        Sexpr::Symbol(sym) => {
            let res = er_compare(
                form.clone(),
                Sexpr::Symbol(sym.clone()),
                syntax_env.clone(),
                env.clone(),
            );

            return res;
        }

        Sexpr::Identifier(id) => {
            let res = er_compare(
                form.clone(),
                Sexpr::Identifier(id.clone()),
                syntax_env.clone(),
                env.clone(),
            );

            return res;
        }

        Sexpr::SyntaxPattern(pat) => {
            return match_subpattern(
                form.clone(),
                pat.clone(),
                Sexpr::Null,
                syntax_env.clone(),
                env.clone(),
                mvec,
            )
        }

        Sexpr::Pair(_) => {
            let mut pattern = pattern.clone();
            while pattern.is_pair() {
                let elt = pattern.car();

                if let Sexpr::SyntaxPattern(ref pat) = elt {
                    return match_subpattern(
                        form.clone(),
                        pat.clone(),
                        pattern.cdr(),
                        syntax_env.clone(),
                        env.clone(),
                        mvec,
                    );
                } else if !form.is_pair() {
                    return false;
                } else {
                    if !match_synrule(form.car(), &elt, syntax_env.clone(), env.clone(), mvec) {
                        return false;
                    }

                    form = form.cdr();
                    pattern = pattern.cdr();
                }
            }

            if !pattern.is_null() {
                return match_synrule(form, &pattern, syntax_env.clone(), env.clone(), mvec);
            } else {
                return form.is_null();
            }
        }

        Sexpr::Vector(vector) => {
            let Sexpr::Vector(ref form) = form else {
                return false;
            };

            let plen = vector.len();
            let mut elli = form.len();

            let flen = elli;
            let mut has_elli = false;

            if plen == 0 {
                return flen == 0;
            }

            for i in 0..plen {
                if let Sexpr::SyntaxPattern(_) = vector[i] {
                    has_elli = true;
                    elli = i;
                    break;
                }
            }

            if (!has_elli && plen != flen) || (has_elli && plen.wrapping_sub(1) > flen) {
                return false;
            }

            if elli < flen {
                let Sexpr::SyntaxPattern(pat) = vector[elli].clone() else {
                    unreachable!("match_synrule: not a SyntaxPattern")
                };

                let prest = Sexpr::list(&vector[elli + 1..plen]);
                let frest = Sexpr::list(&form[elli..flen]);

                return match_subpattern(frest, pat, prest, syntax_env.clone(), env.clone(), mvec);
            } else {
                return true;
            }
        }

        _ => sexp_equal(&form, &pattern),
    }
}

fn alloc_mvec(num_pvars: usize) -> Vec<MatchVar> {
    vec![
        MatchVar {
            root: Sexpr::Null,
            branch: Sexpr::Null,
            sprout: Sexpr::Null,
        };
        num_pvars
    ]
}

fn realize_template_rec(
    sr: &P<SyntaxRules>,
    mut template: Sexpr,
    mvec: &[MatchVar],
    level: i32,
    indices: &mut [i32],
    id_alist: &mut Sexpr,
    exlev: &mut i32,
) -> Sexpr {
    if template.is_pair() {
        let mut h = Sexpr::Null;
        let mut t = Sexpr::null();
        while template.is_pair() {
            let e = template.car();

            if matches!(e, Sexpr::SyntaxPattern(_)) {
                let r = realize_template_rec(sr, e, mvec, level, indices, id_alist, exlev);

                if matches!(r, Sexpr::Undefined) {
                    return r;
                }

                template_append(&mut h, &mut t, r);
            } else {
                let r = realize_template_rec(sr, e, mvec, level, indices, id_alist, exlev);

                if matches!(r, Sexpr::Undefined) {
                    return r;
                }

                template_append1(&mut h, &mut t, r);
            }
            //println!("h={}, t={}", h, t);

            template = template.cdr();
        }

        if !template.is_null() {
            let r =
                realize_template_rec(sr, template.clone(), mvec, level, indices, id_alist, exlev);

            if matches!(r, Sexpr::Undefined) {
                return r;
            }

            if h.is_null() {
                return r;
            }

            template_append(&mut h, &mut t, r);
        }

        return h;
    }

    if let Sexpr::PVRef(ref pvref) = &template {
        let v = get_pvref_value(pvref, mvec, indices, exlev);
        return v;
    }

    if let Sexpr::SyntaxPattern(ref pat) = &template {
        let mut h = Sexpr::Null;
        let mut t = Sexpr::Null;

        indices[level as usize + 1] = 0;

        loop {
            let r = realize_template_rec(
                sr,
                pat.pattern.clone(),
                mvec,
                level + 1,
                indices,
                id_alist,
                exlev,
            );

            if matches!(r, Sexpr::Undefined) {
                if *exlev < pat.level as i32 {
                    return r;
                } else {
                    return h;
                }
            }

            if matches!(pat.pattern, Sexpr::SyntaxPattern(_)) {
                sexp_append!(&mut h, &mut t, r);
            } else {
                sexp_append1!(&mut h, &mut t, r);
            }

            indices[level as usize + 1] += 1;
        }
    }

    if matches!(template, Sexpr::Symbol(_) | Sexpr::Identifier(_)) {
        return rename_variable(template, id_alist, sr.syntax_env.clone(), sr.env.clone());
    }

    template
}

fn realize_template(sr: &P<SyntaxRules>, branch: &SyntaxRuleBranch, mvec: &[MatchVar]) -> Sexpr {
    let mut index = vec![0; 100];

    let mut exlev = 0;
    let mut idlist = Sexpr::Null;

    if branch.max_level > 100 {
        index = vec![0; branch.max_level as usize + 1];
    }

    realize_template_rec(
        sr,
        branch.template.clone(),
        mvec,
        0,
        &mut index,
        &mut idlist,
        &mut exlev,
    )
}

pub fn synrule_expand(
    form: Sexpr,
    syntax_env: P<SyntaxEnv>,
    env: Sexpr,
    sr: &P<SyntaxRules>,
) -> Result<Sexpr, String> {
    let mut mvec = alloc_mvec(sr.max_num_pvars as usize);
    for rule in sr.rules.iter() {
        mvec.fill(MatchVar {
            root: Sexpr::Null,
            branch: Sexpr::Null,
            sprout: Sexpr::Null,
        });

        if match_synrule(
            form.cdr().clone(),
            &rule.pattern,
            syntax_env.clone(),
            env.clone(),
            &mut mvec,
        ) {
            let r = realize_template(sr, rule, &mvec);
            return Ok(r);
        }
    }

    Err(format!("no matching syntax-rules for {}", form))
}
