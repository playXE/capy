//! Fixing letrec
//!
//! This module implements the algorithm described in the paper
//! "Fixing Letrec: A Faithful Yet Efficient Implementation of Scheme's Recursive Binding Construct".
//!
//! The main idea is to remove unecessary boxing when implementing `letrec`. Traditionally it is lowered as:
//! ```text
//!
//! (letrec ([x <expr1>] [y <expr2>] ...)] <body>)))
//!
//! ; after lowering
//! (let ([x (void)] [y (void)] ...)
//!     (set! x <expr1>)
//!     (set! y <expr2>)
//!     ...
//!     <body>)
//! ```
//!
//! Which is correct but has significant downside of requiring boxing for mutable variables. This pass tries to remove
//! all the boxing. Lambdas are placed in `fix` node and variables are placed inside `bind`. If variable is proved to be "complex"
//! it would become boxed, otherwise it is not boxed and is accessed without any overhead.
//!
//! Invoke [pass_letrec] to fix all letrecs.
//!
//! TODO: Add wrappers for possibly uninitialized variables so that at runtime error is thrown
//! if variable being accessed is undefined.
use std::{collections::{HashMap, HashSet}, borrow::Cow};

use crate::{
    rc::Rc,
    sexpr::{Sexpr, Symbol},
    tree_il::*,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct VertexId(u32);

pub struct Vertex {
    pub ix: VertexId,
    pub lhs: Rc<Variable>,
    pub rhs: Rc<TreeNode>,
    pub idx: usize,
    pub adjacency: Vec<VertexId>,
    pub number: isize,
    pub lowlink: isize,
    pub onstack: bool,
}

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
            // dummy variable
            lhs: make_variable(Rc::new(Symbol::Uninterned(Cow::Borrowed("")))),
            rhs: make_constant(Sexpr::Unspecified, None),
            adjacency: Vec::new(),
            number: -1,
            lowlink: -1,
            onstack: false,
        });
        VertexId(ix as u32)
    }

    pub fn add_vertex(&mut self, lhs: Rc<Variable>, rhs: Rc<TreeNode>) -> VertexId {
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

    pub fn unasigned_procedure(&self, v: VertexId) -> bool {
        let unasigned = !self[v].lhs.mutated;
        let proc = matches!(*self[v].rhs, TreeNode::Proc(_));

        unasigned && proc
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
/*
fn is_safe_rhs(x: &TreeNode) -> bool {
    match x {
        TreeNode::Constant(_) | TreeNode::Proc(_) | TreeNode::PrimRef(_) => true,
        _ => false,
    }
}

fn wrap_refs(rhs: &Rc<TreeNode>, valid: &mut Rc<Variable>, lhs: &[Rc<Variable>]) -> Rc<TreeNode> {
    if is_safe_rhs(rhs) {
        return rhs.clone();
    }

    fn pass(x: &Rc<TreeNode>, valid: &mut Rc<Variable>, lhs: &[Rc<Variable>]) -> Rc<TreeNode> {
        match &**x {
            TreeNode::RecStar(vars, body) => make_rec_star(
                vars.iter()
                    .map(|(var, init)| (var.clone(), pass(init, valid, lhs)))
                    .collect(),
                pass(body, valid, lhs),
            ),

            TreeNode::Rec(vars, body) => make_rec(
                vars.iter()
                    .map(|(var, init)| (var.clone(), pass(init, valid, lhs)))
                    .collect(),
                pass(body, valid, lhs),
            ),
            TreeNode::Bind(lhs, rhs, body) => make_bind(
                lhs.clone(),
                rhs.iter().map(|x| pass(x, valid, lhs)).collect(),
                pass(body, valid, lhs),
            ),

            TreeNode::Proc(proc) => Rc::new(TreeNode::Proc(make_proc(
                proc.name.clone(),
                proc.cases
                    .iter()
                    .map(|case| {
                        Rc::new(ProcCase {
                            info: case.info.clone(),
                            body: pass(&case.body, valid, lhs),
                        })
                    })
                    .collect(),
            ))),

            TreeNode::Seq(seq) => {
                let mut s = vec![];

                for x in seq {
                    s.push(pass(x, valid, lhs));
                }

                make_seq(s)
            }

            TreeNode::Test(cond, cons, alt) => make_test(
                pass(cond, valid, lhs),
                pass(cons, valid, lhs),
                pass(alt, valid, lhs),
            ),

            TreeNode::FunCall(operator, operands, source) => make_fun_call(
                pass(operator, valid, lhs),
                &operands
                    .iter()
                    .map(|x| pass(x, valid, lhs))
                    .collect::<Vec<_>>(),
                source.clone(),
            ),

            TreeNode::Constant(_) => x.clone(),
            TreeNode::Ref(name) | TreeNode::Mutate(name, _) => {
                match lhs.iter().find(|x| Rc::ptr_eq(x, &name)) {
                    Some(_) => {
                        valid.referenced = true;
                        let check = make_test(
                            make_constant(Sexpr::Boolean(true)),
                            make_fun_call(make_primref(Rc::new("void".to_string())), &[], None),
                            make_fun_call(
                                make_primref(Rc::new("undefined-variable".to_string())),
                                &[make_constant(Sexpr::Symbol(name.name.clone()))],
                                None,
                            ),
                        );

                        make_seq(vec![check, x.clone()])
                    }

                    _ => match &**x {
                        TreeNode::Mutate(name, expr) => {
                            make_mutate(name.clone(), pass(expr, valid, lhs))
                        }

                        _ => x.clone(),
                    },
                }
            }

            _ => x.clone(),
        }
    }

    pass(rhs, valid, lhs)
}*/

pub fn pass_letrec(x: &Rc<TreeNode>) -> Rc<TreeNode> {
    struct FixPass {
        graph: Graph,
        fv_cache: HashMap<Rc<TreeNode>, Rc<HashSet<Rc<Variable>>>>,
    }

    fn is_free_variable(state: &mut FixPass, var: &Rc<Variable>, x: &Rc<TreeNode>) -> bool {
        let fv = free_variables(x, &mut state.fv_cache);
        fv.contains(var)
    }

    fn make_fixes(graph: &Graph, vertices: &[VertexId], body: Rc<TreeNode>) -> Rc<TreeNode> {
        if vertices.is_empty() {
            body
        } else {
            make_fix(
                vertices.iter().map(|x| graph[*x].lhs.clone()).collect(),
                vertices
                    .iter()
                    .map(|x| graph[*x].rhs.proc().unwrap().clone())
                    .collect(),
                body.clone(),
                body.src().cloned(),
            )
        }
    }

    fn make_mutations(graph: &Graph, verticies: &[VertexId], body: Rc<TreeNode>) -> Rc<TreeNode> {
        let mut seq = vec![];

        for &v in verticies {
            let mut var = graph[v].lhs.clone();
            let init = graph[v].rhs.clone();

            if !var.mutated {
                var.mutated = true;
            }

            seq.push(make_mutate(var, init, body.src().cloned()));
        }
        seq.push(body.clone());
        make_seq(seq, body.src().cloned())
    }

    fn fix1(
        in_order: bool,
        state: &mut FixPass,
        scc: &[VertexId],
        fixes: &mut Vec<VertexId>,
        body: Rc<TreeNode>,
    ) -> Rc<TreeNode> {
        let _src = body.src().cloned();

        if scc.len() == 1 {
            let b = scc[0].clone();
            let var = state.graph[b].lhs.clone();
            let init = state.graph[b].rhs.clone();

            if state.graph.unasigned_procedure(b) {
                // "If init is a lambda, and var is unassigned"
                fixes.push(b);
                body
            } else if !is_free_variable(state, &var, &init) {
                // "If var is not free in init". Consumes the fixes previously
                // saved up
                let new_fixes = make_fixes(&state.graph, fixes, body);
                fixes.clear();
                make_bind(vec![var], vec![init], new_fixes, _src.clone())
            } else {
                // "Otherwise we resort to assignment". Also
                // consumes any fixes saved up
                let new_fixes = make_fixes(&state.graph, fixes, body);
                fixes.clear();
                make_bind(
                    vec![var],
                    vec![make_fun_call(
                        make_primref(Rc::new("void".to_string()), _src.clone()),
                        &[],
                        _src.clone(),
                    )],
                    make_mutations(&state.graph, &[b], new_fixes),
                    _src.clone(),
                )
            }
        } else {
            // an SCC with multiple bindings
            let (mut lambda_bindings, mut complex_bindings): (Vec<VertexId>, Vec<VertexId>) = scc
                .to_vec()
                .into_iter()
                .partition(|x| state.graph.unasigned_procedure(*x));
            if complex_bindings.is_empty() {
                // <var_λ,init_λ> if init is a lambda expression and
                // var is unassigned.
                fixes.append(&mut lambda_bindings);
                body
            } else {
                if in_order {
                    complex_bindings.sort_by(|a, b| state.graph[*a].idx.cmp(&state.graph[*b].idx));
                }
                // <var_c, init_c> otherwise
                make_bind(
                    complex_bindings
                        .iter()
                        .map(|x| state.graph[*x].lhs.clone())
                        .collect(),
                    (0..complex_bindings.len())
                        .map(|_| {
                            make_fun_call(
                                make_primref(Rc::new("void".to_string()), _src.clone()),
                                &[],
                                None,
                            )
                        })
                        .collect(),
                    {
                        let mut fixes = fixes.clone();
                        fixes.append(&mut lambda_bindings);
                        let mutations = make_mutations(&state.graph, &complex_bindings, body);
                        make_fixes(&state.graph, &fixes, mutations)
                    },
                    _src.clone(),
                )
            }
        }
    }
    fn fixing(
        state: &mut FixPass,
        sccs: &[Vec<VertexId>],
        body: Rc<TreeNode>,
        in_order: bool,
    ) -> (Vec<VertexId>, Rc<TreeNode>) {
        fn rec(
            state: &mut FixPass,
            sccs: &[Vec<VertexId>],
            body: Rc<TreeNode>,
            in_order: bool,
            fixes: &mut Vec<VertexId>,
        ) -> Rc<TreeNode> {
            if sccs.is_empty() {
                return body;
            } else {
                let body = rec(state, &sccs[1..], body, in_order, fixes);
                fix1(in_order, state, &sccs[0], fixes, body)
            }
        }
        let mut fixes = Vec::new();
        let body = rec(state, sccs, body, in_order, &mut fixes);
        (fixes, body)
    }

    fn rec_deps(
        state: &mut FixPass,
        vertices: &[VertexId],
        _vars: &[(Rc<Variable>, Rc<TreeNode>)],
    ) {
        for &v in vertices {
            for &w in vertices {
                let lhs = state.graph[v].lhs.clone();
                let rhs = state.graph[w].rhs.clone();
                if w != v && is_free_variable(state, &lhs, &rhs) {
                    state.graph.add_edge(w, v);
                }
            }
        }
    }

    fn rec_star_deps(
        state: &mut FixPass,
        vertices: &[VertexId],
        _vars: &[(Rc<Variable>, Rc<TreeNode>)],
    ) {
        fn rec(w: &[VertexId], state: &mut FixPass) {
            if w.is_empty() {
                return;
            }
            if !w[1..].is_empty() {
                let xj = w[1];
                let xi = w[0];
                if state.graph[xi].rhs.might_cause_side_effects() {
                    state.graph.add_edge(xj, xi);
                }

                rec(&w[1..], state);
            }
        }

        rec(vertices, state);
    }

    fn fixing_letrec_star(
        state: &mut FixPass,
        vars: &[(Rc<Variable>, Rc<TreeNode>)],
        body: Rc<TreeNode>,
    ) -> Rc<TreeNode> {
        let vertices = vars
            .iter()
            .enumerate()
            .map(|(ix, (lhs, rhs))| {
                let v = state.graph.add_vertex(lhs.clone(), rhs.clone());
                state.graph[v].idx = ix;
                v
            })
            .collect::<Vec<_>>();
        rec_deps(state, &vertices, vars);
        rec_star_deps(state, &vertices, vars);

        let sccs = state.graph.tarjan_sccs();
        let (fixes, body) = fixing(state, &sccs, body, true);
        make_fixes(&state.graph, &fixes, body)
    }

    fn fixing_letrec(
        state: &mut FixPass,
        vars: &[(Rc<Variable>, Rc<TreeNode>)],
        body: Rc<TreeNode>,
    ) -> Rc<TreeNode> {
        let vertices = vars
            .iter()
            .enumerate()
            .map(|(ix, (lhs, rhs))| {
                let v = state.graph.add_vertex(lhs.clone(), rhs.clone());
                state.graph[v].idx = ix;
                v
            })
            .collect::<Vec<_>>();
        rec_deps(state, &vertices, vars);

        let sccs = state.graph.tarjan_sccs();
        let (fixes, body) = fixing(state, &sccs, body, false);
        make_fixes(&state.graph, &fixes, body)
    }

    fn pass(x: &Rc<TreeNode>, state: &mut FixPass) -> Rc<TreeNode> {
        match &**x {
            TreeNode::Rec(vars, body, _) => pass(&fixing_letrec(state, vars, body.clone()), state),

            TreeNode::RecStar(vars, body, _) => {
                pass(&fixing_letrec_star(state, vars, body.clone()), state)
            }

            TreeNode::Fix(lhs, rhs, body, src) => make_fix(
                lhs.clone(),
                rhs.iter()
                    .map(|proc| {
                        pass(&Rc::new(TreeNode::Proc(proc.clone())), state)
                            .proc()
                            .unwrap()
                            .clone()
                    })
                    .collect(),
                pass(&body, state),
                src.clone(),
            ),

            TreeNode::Bind(lhs, rhs, body, src) => make_bind(
                lhs.clone(),
                rhs.iter().map(|x| pass(x, state)).collect(),
                pass(body, state),
                src.clone(),
            ),
            TreeNode::Seq(seq, src) => {
                make_seq(seq.iter().map(|x| pass(x, state)).collect(), src.clone())
            }
            TreeNode::Proc(proc) => {
                let mut cases = vec![];
                for case in proc.cases.iter() {
                    cases.push(Rc::new(ProcCase {
                        info: case.info.clone(),
                        body: pass(&case.body, state),
                    }));
                }
                let mut p = make_proc(proc.name.clone(), cases);
                p.source = proc.source.clone();

                Rc::new(TreeNode::Proc(p))
            }

            TreeNode::Mutate(var, expr, src) => {
                make_mutate(var.clone(), pass(expr, state), src.clone())
            }
            TreeNode::Test(test, cons, alt, src) => make_test(
                pass(test, state),
                pass(cons, state),
                pass(alt, state),
                src.clone(),
            ),
            TreeNode::FunCall(operator, operands, src) => make_fun_call(
                pass(operator, state),
                &operands.iter().map(|x| pass(x, state)).collect::<Vec<_>>(),
                src.clone(),
            ),

            TreeNode::Constant(_, _) | TreeNode::Ref(_, _) | TreeNode::PrimRef(_, _) => x.clone(),
            _ => unreachable!("unknown node at fixing-letrec"),
        }
    }

    let mut state = FixPass {
        fv_cache: HashMap::new(),
        graph: Graph::new(),
    };

    pass(x, &mut state)
}

fn free_variables(
    x: &Rc<TreeNode>,
    fv_cache: &mut HashMap<Rc<TreeNode>, Rc<HashSet<Rc<Variable>>>>,
) -> Rc<HashSet<Rc<Variable>>> {
    if let Some(free) = fv_cache.get(x) {
        return free.clone();
    }

    fn rec(x: &Rc<TreeNode>, set: &mut HashSet<Rc<Variable>>) {
        match &**x {
            TreeNode::FunCall(operator, operands, _) => {
                rec(operator, set);
                operands.iter().for_each(|operand| rec(operand, set));
            }

            TreeNode::Seq(seq, _) => seq.iter().for_each(|x| rec(x, set)),
            TreeNode::Test(test, cons, alt, _) => {
                rec(test, set);
                rec(cons, set);
                rec(alt, set);
            }

            TreeNode::TagBody(body, _) => rec(body, set),
            TreeNode::Bind(lhs, rhs, body, _) => {
                rhs.iter().for_each(|x| rec(x, set));
                let mut set2 = HashSet::new();
                rec(body, &mut set2);
                for x in lhs {
                    set2.remove(x);
                }

                set.extend(set2);
            }

            TreeNode::Rec(vars, body, _) | TreeNode::RecStar(vars, body, _) => {
                let mut set2 = HashSet::new();

                for (_, init) in vars {
                    rec(init, &mut set2);
                }

                rec(body, &mut set2);

                for (var, _) in vars {
                    set2.remove(var);
                }

                set.extend(set2);
            }

            TreeNode::Proc(proc) => {
                for case in proc.cases.iter() {
                    let mut set2 = HashSet::new();
                    rec(&case.body, &mut set2);
                    for var in case.info.formals.iter() {
                        set2.remove(var);
                    }

                    set.extend(set2);
                }
            }

            TreeNode::Ref(var, _) => {
                set.insert(var.clone());
            }
            TreeNode::Mutate(var, _, _) => {
                set.insert(var.clone());
            }
            TreeNode::Fix(lhs, rhs, body, _) => {
                let mut set2 = HashSet::new();

                for proc in rhs.iter() {
                    for case in proc.cases.iter() {
                        rec(&case.body, &mut set2);
                        for var in case.info.formals.iter() {
                            set2.remove(var);
                        }
                    }
                }

                rec(body, &mut set2);

                for var in lhs.iter() {
                    set2.remove(var);
                }

                set.extend(set2);
            }

            TreeNode::Constant(_, _) | TreeNode::PrimRef(_, _) | TreeNode::Goto(_, _) => {}
            _ => (),
        }
    }
    let mut set = HashSet::new();
    rec(x, &mut set);
    let set = Rc::new(set);
    fv_cache.insert(x.clone(), set.clone());
    set
}
