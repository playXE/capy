//! For a detailed discussion, see "Fixing Letrec: A Faithful Yet
//! Efficient Implementation of Scheme's Recursive Binding Construct", by
//! Oscar Waddell, Dipanwita Sarkar, and R. Kent Dybvig, as well as
//! "Fixing Letrec (reloaded)", by Abdulaziz Ghuloum and R. Kent Dybvig.

use std::collections::{HashMap, HashSet};

use crate::compiler::P;

use super::{sexpr::Sexpr, tree_il::*};

fn analyze_lexicals(iform: &IForm) -> (HashSet<P<LVar>>, HashSet<P<LVar>>) {
    let mut referenced = HashSet::new();
    let mut assigned = HashSet::new();

    fn rec(iform: &IForm, referenced: &mut HashSet<P<LVar>>, assigned: &mut HashSet<P<LVar>>) {
        match iform {
            IForm::LRef(lref) => {
                referenced.insert(lref.lvar.clone());
            }

            IForm::LSet(lset) => {
                assigned.insert(lset.lvar.clone());
            }

            IForm::GSet(gset) => {
                rec(&gset.value, referenced, assigned);
            }

            IForm::Define(define) => {
                rec(&define.value, referenced, assigned);
            }

            IForm::Call(call) => {
                rec(&call.proc, referenced, assigned);
                for arg in &call.args {
                    rec(arg, referenced, assigned);
                }
            }

            IForm::PrimCall(_, _, args) => {
                for arg in args {
                    rec(arg, referenced, assigned);
                }
            }

            IForm::If(if_) => {
                rec(&if_.cond, referenced, assigned);
                rec(&if_.consequent, referenced, assigned);
                rec(&if_.alternative, referenced, assigned);
            }

            IForm::Label(label) => {
                rec(&label.body, referenced, assigned);
            }

            IForm::Seq(seq) => {
                for form in &seq.forms {
                    rec(form, referenced, assigned);
                }
            }

            IForm::Let(let_) => {
                for init in &let_.inits {
                    rec(&init, referenced, assigned);
                }

                rec(&let_.body, referenced, assigned);
            }

            IForm::Lambda(lambda) => {
                rec(&lambda.body, referenced, assigned);
            }

            IForm::LetValues(let_values) => {
                rec(&let_values.init, referenced, assigned);
                rec(&let_values.body, referenced, assigned);
            }

            _ => (),
        }
    }

    rec(iform, &mut referenced, &mut assigned);

    (referenced, assigned)
}

fn free_variables(
    expr: &P<IForm>,
    cache: &mut HashMap<P<IForm>, P<HashSet<P<LVar>>>>,
) -> P<HashSet<P<LVar>>> {
    if let Some(free) = cache.get(expr) {
        return free.clone();
    }

    fn rec(expr: &IForm, set: &mut HashSet<P<LVar>>) {
        match expr {
            IForm::Call(call) => {
                rec(&call.proc, set);
                for arg in &call.args {
                    rec(arg, set);
                }
            }

            IForm::PrimCall(_, _, args) => {
                for arg in args {
                    rec(arg, set);
                }
            }

            IForm::GSet(gset) => {
                rec(&gset.value, set);
            }

            IForm::Define(define) => {
                rec(&define.value, set);
            }

            IForm::If(if_) => {
                rec(&if_.cond, set);
                rec(&if_.consequent, set);
                rec(&if_.alternative, set);
            }

            IForm::Label(label) => {
                rec(&label.body, set);
            }

            IForm::Seq(seq) => {
                for form in &seq.forms {
                    rec(form, set);
                }
            }

            IForm::Let(let_) => {
                if let LetType::Let = let_.typ {
                    for init in &let_.inits {
                        rec(&init, set);
                    }
                    let mut set2 = HashSet::new();
                    rec(&let_.body, &mut set2);

                    for lvar in &let_.lvars {
                        set2.remove(lvar);
                    }

                    set.extend(set2);
                } else {
                    let mut set2 = HashSet::new();
                    for init in &let_.inits {
                        rec(&init, &mut set2);
                    }
                    rec(&let_.body, &mut set2);
                    for lvar in &let_.lvars {
                        set2.remove(lvar);
                    }
                    set.extend(set2);
                }
            }

            IForm::Lambda(lambda) => {
                let mut set2 = HashSet::new();
                rec(&lambda.body, &mut set2);
                for lvar in &lambda.lvars {
                    set2.remove(lvar);
                }
                set.extend(set2);
            }

            IForm::LetValues(let_values) => {
                rec(&let_values.init, set);
                rec(&let_values.body, set);

                for lvar in &let_values.lvars {
                    set.remove(lvar);
                }
            }

            _ => (),
        }
    }

    let mut set = HashSet::new();
    rec(expr, &mut set);

    let cached = P(set);
    cache.insert(expr.clone(), cached.clone());
    cached
}

pub struct Vertex {
    pub ix: VertexId,
    pub lhs: P<LVar>,
    pub rhs: P<IForm>,
    pub idx: usize,
    pub adjacency: Vec<VertexId>,
    pub number: isize,
    pub lowlink: isize,
    pub onstack: bool,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct VertexId(u32);

pub fn tarjan(graph: &mut Graph) -> Vec<Vec<VertexId>> {
    let mut index = 0;
    let mut sccs = vec![];
    let mut stack = vec![];
    for v in (0..graph.vertices.len()).map(|ix| VertexId(ix as _)) {
        if graph[v].number == -1 {
            strongconnect(graph, v, &mut index, &mut stack, &mut sccs);
        }
    }

    fn strongconnect(
        graph: &mut Graph,
        v: VertexId,
        index: &mut isize,
        stack: &mut Vec<VertexId>,
        sccs: &mut Vec<Vec<VertexId>>,
    ) {
        graph[v].number = *index;
        graph[v].lowlink = *index;
        *index += 1;
        stack.push(v);
        graph[v].onstack = true;

        for j in 0..graph[v].adjacency.len() {
            let w = graph[v].adjacency[j];
            if graph[w].number == -1 {
                // Successor w has not yet been visited; recurse on it
                strongconnect(graph, w, index, stack, sccs);
            } else if graph[w].onstack {
                // Successor w is in stack S and hence in the current SCC
                // If w is not on stack, then (v, w) is an edge pointing to an SCC already found and must be ignored
                // Note: The next line may look odd - but is correct.
                // It says w.index not w.lowlink; that is deliberate and from the original paper
                graph[v].lowlink = std::cmp::min(graph[v].lowlink, graph[w].number);
            }
        }

        if graph[v].lowlink == graph[v].number {
            // start a new strongly connected component
            let mut new_scc = Vec::new();

            while stack.len() > 0 && graph[stack[stack.len() - 1]].number >= graph[v].number {
                let w = stack.pop().unwrap();
                graph[w].onstack = false;
                new_scc.push(w);
            }

            // output the current strongly connected component
            sccs.push(new_scc);
        }
    }

    sccs
}

pub struct Graph {
    vertices: Vec<Vertex>,
}

impl Graph {
    pub fn new() -> Graph {
        Graph {
            vertices: Vec::new(),
        }
    }

    pub fn add_edge(&mut self, v: VertexId, w: VertexId) {
        self[v].adjacency.push(w);
    }

    pub fn add_empty_vertex(&mut self) -> VertexId {
        let ix = self.vertices.len();
        self.vertices.push(Vertex {
            idx: 0,
            ix: VertexId(ix as u32),
            lhs: P(LVar {
                arg: false,
                boxed: false,
                name: Sexpr::Null,
                initval: None,
                ref_count: 0,
                set_count: 0,
            }),
            rhs: P(IForm::It),
            adjacency: Vec::new(),
            number: -1,
            lowlink: -1,
            onstack: false,
        });
        VertexId(ix as u32)
    }

    pub fn add_vertex(&mut self, lhs: P<LVar>, rhs: P<IForm>) -> VertexId {
        let ix = self.vertices.len();
        self.vertices.push(Vertex {
            ix: VertexId(ix as u32),
            lhs,
            rhs,
            idx: 0,
            adjacency: Vec::new(),
            number: -1,
            lowlink: -1,
            onstack: false,
        });
        VertexId(ix as u32)
    }
    /// Robert Tarjan's algorithm for finding strongly connected
    /// components in a graph.
    pub fn tarjan_sccs(&mut self) -> Vec<Vec<VertexId>> {
        let mut index = 0;
        let mut stack = Vec::new();
        let mut sccs = Vec::new();

        fn strong_connect(
            graph: &mut Graph,
            v: VertexId,
            index: &mut isize,
            stack: &mut Vec<VertexId>,
            sccs: &mut Vec<Vec<VertexId>>,
        ) {
            graph[v].number = *index;
            graph[v].lowlink = *index;
            *index += 1;

            stack.push(v);
            graph[v].onstack = true;

            for j in 0..graph[v].adjacency.len() {
                let w = graph[v].adjacency[j];
                if graph[w].number == -1 {
                    strong_connect(graph, w, index, stack, sccs);
                    graph[v].lowlink = std::cmp::min(graph[v].lowlink, graph[w].lowlink);
                } else if graph[w].onstack {
                    graph[v].lowlink = std::cmp::min(graph[v].lowlink, graph[w].number);
                }
            }

            if graph[v].number == graph[v].lowlink {
                let mut scc = Vec::new();
                loop {
                    let w = stack.pop().unwrap();
                    graph[w].onstack = false;
                    scc.push(w);
                    if w == v {
                        break;
                    }
                }

                sccs.push(scc);
            }
        }

        for i in 0..self.vertices.len() {
            let v = VertexId(i as u32);
            if self[v].number == -1 {
                strong_connect(self, v, &mut index, &mut stack, &mut sccs);
            }
        }

        sccs
    }
}

impl std::ops::Index<VertexId> for Graph {
    type Output = Vertex;

    fn index(&self, ix: VertexId) -> &Vertex {
        &self.vertices[ix.0 as usize]
    }
}

impl std::ops::IndexMut<VertexId> for Graph {
    fn index_mut(&mut self, ix: VertexId) -> &mut Vertex {
        &mut self.vertices[ix.0 as usize]
    }
}

impl Graph {
    pub fn unassigned_procedure(&self, v: VertexId) -> bool {
        let v = &self[v];
        let unassigned = v.lhs.is_immutable();
        let proc = matches!(*v.rhs, IForm::Lambda(_));
        unassigned && proc
    }

    pub fn make_mutations(&self, vertices: &[VertexId], body: P<IForm>) -> P<IForm> {
        if vertices.is_empty() {
            return body;
        } else {
            let mut new_seq = Vec::new();
            for v in vertices {
                let mut var = self[*v].lhs.clone();
                if var.is_immutable() {
                    var.set_count += 1;
                }
                let init = self[*v].rhs.clone();
                let mutation = P(IForm::LSet(LSet {
                    lvar: var,
                    value: init,
                }));

                new_seq.push(mutation);
            }
            let src = body.src();
            new_seq.push(body);

            P(IForm::Seq(Seq {
                src,
                forms: new_seq,
            }))
        }
    }
}

#[allow(dead_code)]
struct FixPass {
    referenced: HashSet<P<LVar>>,
    assigned: HashSet<P<LVar>>,
    fv_cache: HashMap<P<IForm>, P<HashSet<P<LVar>>>>,
    graph: Graph,
}

fn make_fixes(graph: &Graph, vertices: &[VertexId], body: P<IForm>) -> P<IForm> {
    if vertices.is_empty() {
        body
    } else {
        let fix_lhs = vertices.iter().map(|v| graph[*v].lhs.clone()).collect();
        let fix_rhs = vertices
            .iter()
            .map(|v| {
                if let IForm::Lambda(lam) = &*graph[*v].rhs {
                    lam.clone()
                } else {
                    panic!("only lambdas go into <fix> node")
                }
            })
            .collect();
        let fix = P(IForm::Fix(Fix {
            src: body.src(),
            lhs: fix_lhs,
            rhs: fix_rhs,
            body,
        }));

        fix
    }
}

fn fix1(
    in_order: bool,
    pass: &mut FixPass,
    scc: &[VertexId],
    fixes: &mut Vec<VertexId>,
    body: P<IForm>,
) -> P<IForm> {
    let src = body.src();
    if scc.len() == 1 {
        let b = scc[0];
        let var = pass.graph[b].lhs.clone();
        let init = pass.graph[b].rhs.clone();
        if pass.graph.unassigned_procedure(b) {
            fixes.push(b);
            body
        } else if !free_variables(&init, &mut pass.fv_cache).contains(&var) {
            // If var is not free in init". Consumes the
            // fixes previously saved up.
            let new_fixes = make_fixes(&pass.graph, fixes, body);
            fixes.clear();
            let bind = P(IForm::Let(Let {
                src,
                typ: LetType::Let,
                lvars: vec![var.clone()],
                inits: vec![init],
                body: new_fixes,
            }));
            bind
        } else {
            // "Otherwise, we resort to assignment". Also
            // consumes any fixes saved up.
            let new_fixes = make_fixes(&pass.graph, fixes, body);
            fixes.clear();
            let mutations = pass.graph.make_mutations(&[b], new_fixes);
            let bind = P(IForm::Let(Let {
                src,
                typ: LetType::Let,
                lvars: vec![var.clone()],
                inits: vec![P(IForm::Const(Sexpr::Undefined))],
                body: mutations,
            }));

            bind
        }
    } else {
        // l - contains lambdas, c - complex expressions
        let (mut l, mut c): (Vec<VertexId>, Vec<VertexId>) = scc
            .to_vec()
            .into_iter()
            .partition(|v| pass.graph.unassigned_procedure(*v));
        if c.is_empty() {
            // <var_λ,init_λ> if init is a lambda expression and
            // var is unassigned.
            fixes.extend(l);
            body
        } else {
            if in_order {
                c.sort_by(|a, b| pass.graph[*a].idx.cmp(&pass.graph[*b].idx));
            }

            // <var_c,init_c> otherwise.
            let bind = P(IForm::Let(Let {
                src,
                typ: LetType::Let,
                lvars: c.iter().map(|v| pass.graph[*v].lhs.clone()).collect(),
                inits: vec![P(IForm::Const(Sexpr::Undefined)); c.len()],
                body: {
                    let mut fixes = fixes.clone();
                    fixes.append(&mut l);
                    let mutations = pass.graph.make_mutations(&c, body);
                    make_fixes(&pass.graph, &fixes, mutations)
                }
            }));

            bind
        }
    }
}

fn fixing(
    pass: &mut FixPass,
    sccs: &[Vec<VertexId>],
    body: P<IForm>,
    in_order: bool,
) -> (Vec<VertexId>, P<IForm>) {
    fn rec(
        pass: &mut FixPass,
        sccs: &[Vec<VertexId>],
        body: P<IForm>,
        in_order: bool,
        fixes: &mut Vec<VertexId>,
    ) -> P<IForm> {
        if sccs.is_empty() {
            return body;
        } else {
            let body = rec(pass, &sccs[1..], body, in_order, fixes);
            fix1(in_order, pass, &sccs[0], fixes, body)
        }
    }
    let mut fixes = Vec::new();
    let body = rec(pass, sccs, body, in_order, &mut fixes);
    (fixes, body)
}

fn rec_deps(vs: &[VertexId], pass: &mut FixPass) {
    for &v in vs.iter() {
        for &w in vs.iter() {
            pass.graph.add_edge(w, v);
        }
    }
}

fn rec_deps_in_order(vs: &[VertexId], pass: &mut FixPass) {
    let mut w = vs;
    while !w.is_empty() {
        if w.len() != 1 {
            let xj = w[1];
            let xi = w[0];
            if !pass.graph[xi].rhs.is_transparent() {
                pass.graph.add_edge(xj, xi);
            }
            w = &w[1..];
        } else {
            break;
        }
    }
}

fn fixing_letrec(
    pass: &mut FixPass,
    in_order: bool,
    lhs: &[P<LVar>],
    rhs: &[P<IForm>],
    body: P<IForm>,
) -> P<IForm> {
    let vertices = lhs
        .iter()
        .zip(rhs.iter())
        .enumerate()
        .map(|(ix, (lhs, rhs))| {
            let v = pass.graph.add_vertex(lhs.clone(), rhs.clone());
            pass.graph[v].idx = ix;
            v
        })
        .collect::<Vec<_>>();
    rec_deps(&vertices, pass);
    if in_order {
        rec_deps_in_order(&vertices, pass);
    }
    let sccs = pass.graph.tarjan_sccs();
    let (fixes, body) = fixing(pass, &sccs, body, in_order);
    make_fixes(&pass.graph, &fixes, body)
}

pub fn pass_fix_letrec(mut x: P<IForm>) -> P<IForm> {
    let fv_cache = HashMap::new();
    x.count_refs();
    let (referenced, assigned) = analyze_lexicals(&x);
    let mut pass = FixPass {
        referenced,
        assigned,
        fv_cache,
        graph: Graph::new(),
    };

    fn post_order(mut x: P<IForm>, pass: &mut FixPass) -> P<IForm> {
        let orig = x.clone();
        match &mut *x {
            IForm::Fix(fix) => {
                for rhs in &mut fix.rhs {
                    rhs.body = post_order(rhs.body.clone(), pass);
                }
                fix.body = post_order(fix.body.clone(), pass);
                x
            }

            IForm::Lambda(lam) => {
                lam.body = post_order(lam.body.clone(), pass);
                x
            }
            IForm::LSet(lset) => {
                if pass.referenced.contains(&lset.lvar) {
                    return x;
                } else {
                    // sets to unreferenced variables may be replaced
                    // by their expression for side effect
                    P(IForm::Seq(Seq {
                        src: orig.src(),
                        forms: vec![lset.value.clone(), P(IForm::Const(Sexpr::Undefined))],
                    }))
                }
            }

            IForm::Call(call) => {
                call.proc = post_order(call.proc.clone(), pass);
                for arg in &mut call.args {
                    *arg = post_order(arg.clone(), pass);
                }

                x
            }

            IForm::Define(define) => {
                define.value = post_order(define.value.clone(), pass);
                x
            }

            IForm::GSet(gset) => {
                gset.value = post_order(gset.value.clone(), pass);
                x
            }

            IForm::If(if_) => {
                if_.cond = post_order(if_.cond.clone(), pass);
                if_.consequent = post_order(if_.consequent.clone(), pass);
                if_.alternative = post_order(if_.alternative.clone(), pass);
                x
            }

            IForm::Label(label) => {
                label.body = post_order(label.body.clone(), pass);
                x
            }

            IForm::Seq(seq) => {
                for form in &mut seq.forms {
                    *form = post_order(form.clone(), pass);
                }
                x
            }

            IForm::Let(let_) => match let_.typ {
                LetType::Rec => {
                    let res =
                        fixing_letrec(pass, false, &let_.lvars, &let_.inits, let_.body.clone());
                    //assert!(matches!(*res, IForm::Fix(_)));
                    post_order(res, pass)
                }
                LetType::RecStar => {
                    let res =
                        fixing_letrec(pass, true, &let_.lvars, &let_.inits, let_.body.clone());
                    assert!(matches!(*res, IForm::Fix(_)));
                    post_order(res, pass)
                }

                LetType::Let => {
                    for init in &mut let_.inits {
                        *init = post_order(init.clone(), pass);
                    }
                    let_.body = post_order(let_.body.clone(), pass);
                    x
                }
            },

            IForm::LetValues(lv) => {
                lv.init = post_order(lv.init.clone(), pass);
                lv.body = post_order(lv.body.clone(), pass);
                x
            }

            IForm::PrimCall(_, _, args) => {
                for arg in args {
                    *arg = post_order(arg.clone(), pass);
                }
                x
            }
            IForm::Const(_)
            | IForm::It
            | IForm::GRef(_)
            | IForm::Goto(_)
            | IForm::LRef(_)
            | IForm::PrimRef(_) => x,
        }
    }

    post_order(x, &mut pass)
}
