use std::collections::{HashMap, HashSet};

use crate::{
    env::Environment,
    rc::Rc,
    reader::SymbolInterner,
    sexpr::{ScmError, Sexpr, Symbol},
};
use once_cell::unsync::OnceCell;
/// A SyntaxRules object defines hygienic macros.
pub struct SyntaxRules {
    pub name: Option<Rc<Symbol>>,
    pub ellipsis: Rc<Symbol>,
    pub reserved: HashSet<Rc<Symbol>>,
    pub literals: HashSet<Rc<Symbol>>,
    pub patterns: Vec<Sexpr>,
    pub templates: Vec<Sexpr>,
    pub lexical_env: Rc<Environment>,
    pub interner: Rc<SymbolInterner>,
}

impl SyntaxRules {
    pub fn new(
        interner: Rc<SymbolInterner>,
        name: Option<Rc<Symbol>>,
        literals: HashSet<Rc<Symbol>>,
        ellipsis: Option<Rc<Symbol>>,
        patterns: Vec<Sexpr>,
        templates: Vec<Sexpr>,
        env: Rc<Environment>,
    ) -> Self {
        let ellipsis = ellipsis.unwrap_or_else(|| interner.intern("..."));
        Self {
            name,
            ellipsis,
            reserved: HashSet::new(),
            literals,
            patterns,
            templates,
            lexical_env: env,
            interner,
        }
    }

    pub fn expand(&self, input: &Sexpr) -> Result<Sexpr, ScmError> {
        for index in 0..self.patterns.len() {
            if let Some(mut matches) = self.match_pat(&self.patterns[index], input) {
                let expr = self.instantiate(&self.templates[index], &mut matches, 0)?;
                return Ok(expr);
            }
        }

        Err(ScmError::NoMatchingPattern(
            input.clone(),
            Sexpr::Symbol(
                self.name
                    .clone()
                    .unwrap_or_else(|| self.interner.intern("_")),
            ),
        ))
    }

    fn match_pat(&self, pattern: &Sexpr, input: &Sexpr) -> Option<Matches> {
        let mut matches = Matches::new(&self.variables(pattern));

        self.match_internal(pattern, input, &mut matches, 0)
            .then(|| matches)
    }

    fn match_internal(
        &self,
        pattern: &Sexpr,
        input: &Sexpr,
        matches: &mut Matches,
        depth: usize,
    ) -> bool {
        match pattern {
            Sexpr::Symbol(sym) => {
                if self.literals.contains(Symbol::root(sym)) {
                    if let Sexpr::Symbol(input_sym) = input {
                        return Symbol::root(sym) == Symbol::root(input_sym);
                    } else {
                        return false;
                    }
                } else {
                  
                    matches.put(sym.clone(), input.clone());
                    return true;
                }
            }

            Sexpr::Pair(_) => {
                match input {
                    Sexpr::Null | Sexpr::Pair(_) => (),
                    _ => return false,
                }

                let mut pat = pattern.clone();
                let mut inp = input.clone();

                while let Some((token, rest)) = pat.pair() {
  
                    if token
                        .symbol()
                        .map(|s| Symbol::root(s) == &self.ellipsis)
                        .unwrap_or(false)
                    {

                        // ignore ellipsis
                    } else {
                        if let Some((Sexpr::Symbol(_), tail)) = rest.pair().filter(|(s, _)| {
                            s.symbol()
                                .map(|s| Symbol::root(s) == &self.ellipsis)
                                .unwrap_or(false)
                        }) {
                            matches.register(&self.variables(token), depth + 1);

                            let mut max_match_count =
                                inp.list_length().unwrap() - tail.list_length().unwrap();

                            while let Some((_, cdr)) = inp.pair().filter(|(car, _)| {
                                max_match_count > 0
                                    && self.match_internal(token, car, matches, depth)
                            }) {
                                max_match_count -= 1;
                                inp = cdr.clone();
                            }
                        } else if let Some((_, cdr)) = inp
                            .pair()
                            .filter(|(car, _)| self.match_internal(token, car, matches, depth))
                        {
                            inp = cdr.clone();
                        } else {
                            return false;
                        }
                    }

                    pat = rest.clone();
                }

                self.match_internal(&pat, &inp, matches, depth)
            }

            Sexpr::Vector(pat_vector) => {
                let Sexpr::Vector(ref inp_vector) = input else {
                    return false;
                };

                let mut inp_index = 0;

                for pat_index in 0..pat_vector.len() {
                    let token = &pat_vector[pat_index];
                    if let Some(_) = token.symbol().filter(|s| Symbol::root(s) == &self.ellipsis) {
                        // ignore ellipsis
                    } else {
                        if pat_index < pat_vector.len() - 1
                            && pat_vector[pat_vector.len() - 1]
                                .symbol()
                                .filter(|s| Symbol::root(s) == &self.ellipsis)
                                .is_some()
                        {
                            matches.register(&self.variables(token), depth + 1);

                            let mut max_match_count =
                                (inp_vector.len() - inp_index) - (pat_vector.len() - pat_index - 2);

                            while inp_index < inp_vector.len()
                                && max_match_count > 0
                                && self.match_internal(
                                    token,
                                    &inp_vector[inp_index],
                                    matches,
                                    depth + 1,
                                )
                            {
                                max_match_count -= 1;
                                inp_index += 1;
                            }
                        } else if inp_index < inp_vector.len()
                            && self.match_internal(token, &inp_vector[inp_index], matches, depth)
                        {
                            inp_index += 1;
                        } else {
                            return false;
                        }
                    }
                }

                inp_index == inp_vector.len()
            }
            _ => pattern.equal(input),
        }
    }

    fn instantiate_raw(&self, template: &Sexpr, matches: &mut Matches) -> Sexpr {
        match template {
            Sexpr::Symbol(sym) => matches.get(sym.clone(), self.lexical_env.clone()),

            Sexpr::Pair(_) => {
                let mut res = vec![];

                let mut templ = template;

                while let Some((token, rest)) = templ.pair() {
                    res.push(self.instantiate_raw(token, matches));
                    templ = rest;
                }

                Sexpr::make_list_star(&res, self.instantiate_raw(templ, matches))
            }

            Sexpr::Vector(vector) => {
                let mut res = vec![];

                for i in 0..vector.len() {
                    res.push(self.instantiate_raw(&vector[i], matches));
                }

                Sexpr::Vector(Rc::new(res))
            }

            _ => template.clone(),
        }
    }

    fn variables(&self, pattern: &Sexpr) -> Vec<Rc<Symbol>> {
        let mut vars = HashSet::new();

        fn traverse(expr: &Sexpr, vars: &mut HashSet<Rc<Symbol>>, this: &SyntaxRules) {
            match expr {
                Sexpr::Symbol(sym) => {
                    if !this.literals.contains(sym) && !this.reserved.contains(Symbol::root(sym)) {
                        vars.insert(sym.clone());
                    }
                }

                Sexpr::Pair(pair) => {
                    traverse(&pair.0, vars, this);
                    traverse(&pair.1, vars, this);
                }

                Sexpr::Vector(vec) => {
                    for expr in vec.iter() {
                        traverse(expr, vars, this);
                    }
                }

                _ => (),
            }
        }

        traverse(pattern, &mut vars, self);

        vars.drain().collect()
    }

    fn instantiate(
        &self,
        template: &Sexpr,
        matches: &mut Matches,
        depth: usize,
    ) -> Result<Sexpr, ScmError> {
        match template {
            Sexpr::Symbol(sym) => Ok(matches.get(sym.clone(), self.lexical_env.clone())),
            Sexpr::Pair(x)
                if x.0
                    .symbol()
                    .filter(|x| Symbol::root(x) == &self.ellipsis)
                    .is_some() =>
            {
                let rest = &x.1;

                let Some((car, _)) = rest.pair() else {
                    return Err(ScmError::UnexpectedType(
                        rest.clone(),
                        "ellipsis expected pair",
                    ));
                };

                return Ok(self.instantiate_raw(car, matches));
            }

            Sexpr::Pair(_) => {
                let mut res = vec![];
                let mut templ = template;
                let mut repeater = None::<Sexpr>;

                while let Some((token, rest)) = templ.pair() {
                    match rest.pair() {
                        Some((Sexpr::Symbol(s), _)) if Symbol::root(s) == &self.ellipsis => {
                            if token
                                .symbol()
                                .filter(|s| Symbol::root(s) == &self.ellipsis)
                                .is_some()
                            {
                                return Err(ScmError::MismatchedRepetitionPatterns(Sexpr::Symbol(
                                    self.ellipsis.clone(),
                                )));
                            }

                            repeater = Some(token.clone());
                        }

                        _ if token
                            .symbol()
                            .filter(|s| Symbol::root(s) == &self.ellipsis)
                            .is_some() =>
                        {
                            let Some(ref repeater_template) = repeater else {
                                return Err(ScmError::MismatchedRepetitionPatterns(Sexpr::Symbol(
                                    self.ellipsis.clone(),
                                )));
                            };

                            matches.instantiate(repeater_template, self, depth + 1, &mut res)?;

                            repeater = None;
                        }

                        _ => {
                            res.push(self.instantiate(token, matches, depth)?);
                        }
                    }

                    templ = rest;
                }

                Ok(Sexpr::make_list_star(
                    &res,
                    self.instantiate(templ, matches, depth)?,
                ))
            }

            Sexpr::Vector(vector) => {
                let mut res = vec![];

                for i in 0..vector.len() {
                    if i < vector.len() - 1
                        && vector[i + 1]
                            .symbol()
                            .filter(|s| Symbol::root(s) == &self.ellipsis)
                            .is_some()
                    {
                        if vector[i].symbol().is_some() {
                            return Err(ScmError::MismatchedRepetitionPatterns(Sexpr::Symbol(
                                self.ellipsis.clone(),
                            )));
                        }
                    } else if vector[i]
                        .symbol()
                        .filter(|s| Symbol::root(s) == &self.ellipsis)
                        .is_some()
                    {
                        if i > 0 {
                            matches.instantiate(&vector[i - 1], self, depth + 1, &mut res)?;
                        } else {
                            return Err(ScmError::MismatchedRepetitionPatterns(Sexpr::Symbol(
                                self.ellipsis.clone(),
                            )));
                        }
                    } else {
                        res.push(self.instantiate(&vector[i], matches, depth)?);
                    }
                }

                Ok(Sexpr::Vector(Rc::new(res)))
            }

            _ => Ok(template.clone()),
        }
    }
}

/// Struct `Matches` represents the result of matching an input expression with a template.
/// Objects of class `Matches` contain a mapping from symbols to values in the input expression
/// as well as symbols to generated symbols. Generated symbols are needed to guarantee the
/// hygiene property of Scheme's macros.
struct Matches {
    generated_sym: HashMap<Rc<Symbol>, Rc<Symbol>>,
    matched_val: HashMap<Rc<Symbol>, Rc<MatchTree>>,
}

impl Matches {
    fn new(syms: &[Rc<Symbol>]) -> Self {
        Self {
            generated_sym: HashMap::new(),
            matched_val: syms
                .iter()
                .cloned()
                .map(|x| (x, Rc::new(MatchTree::new())))
                .collect(),
        }
    }

    fn get(&mut self, sym: Rc<Symbol>, lexical_env: Rc<Environment>) -> Sexpr {
        let Some(value) = self.matched_val.get_mut(&sym).and_then(|x| x.value()) else {
            if let Some(gensym) = self.generated_sym.get(&sym) {
                return Sexpr::Symbol(gensym.clone());
            } else {
                let gensym = Rc::new(Symbol::Generated(sym.clone(), Rc::downgrade(&lexical_env)));
                self.generated_sym.insert(sym.clone(), gensym.clone());
                return Sexpr::Symbol(gensym);
            }
        };

        value
    }

    fn put(&mut self, sym: Rc<Symbol>, expr: Sexpr) {
        self.matched_val.get_mut(&sym).unwrap().enter(expr);
    }

    fn register(&mut self, syms: &[Rc<Symbol>], depth: usize) {
        for sym in syms {
            self.matched_val.get_mut(sym).and_then(|x| {
                x.descend_at(depth);
                Some(x)
            });
        }
    }

    fn instantiate(
        &mut self,
        template: &Sexpr,
        rules: &SyntaxRules,
        depth: usize,
        exprs: &mut Vec<Sexpr>,
    ) -> Result<(), ScmError> {
        let syms = rules.variables(template);

        for _ in 0..self.num_children(&syms, depth)? {
            exprs.push(rules.instantiate(template, self, depth)?);

            for sym in syms.iter() {
                self.matched_val.get_mut(sym).iter_mut().for_each(|x| {
                    x.rotate_at(depth);
                });
            }
        }
        Ok(())
    }

    fn num_children(&self, syms: &[Rc<Symbol>], depth: usize) -> Result<usize, ScmError> {
        let mut res = 0;

        for sym in syms.iter() {
            if let Some(tree) = self.matched_val.get(sym) {
                let s = tree.num_children(depth);

                if s > 0 {
                    if res != 0 && res != s {
                        return Err(ScmError::MismatchedRepetitionPatterns(Sexpr::Symbol(
                            sym.clone(),
                        )));
                    }
                    res = s;
                }
            }
        }

        Ok(res)
    }
}

/// A match tree is a data structure that is used to construct the value matching a particular
/// pattern variable
struct MatchTree {
    root: Node,
    depth: usize,
    #[allow(dead_code)]
    complete: bool,
    /// lazily initialized
    pos: OnceCell<Vec<usize>>,
}

impl MatchTree {
    fn pos_mut(&mut self, x: usize) -> &mut usize {
        self.pos.get_or_init(|| vec![0; self.depth]);
        let vec = self.pos.get_mut().unwrap();
        if vec.len() <= x {
            vec.resize(x + 1, 0);
        }

        &mut vec[x]
    }

    fn pos(&self, x: usize) -> usize {
        let vec = self.pos.get_or_init(|| vec![0; self.depth]);
        if vec.len() <= x {
            0
        } else {
            vec[x]
        }
    }

    fn tail_node(&mut self, depth: usize) -> &Node {
        let mut res = &self.root;
        for _ in 0..depth {
            res = res.last_child();
        }

        res
    }

    fn enter(&mut self, expr: Sexpr) {
        self.tail_node(self.depth).append_child(Node::Leaf(expr));
    }

    fn current_node(&self, depth: usize) -> Option<Node> {
        let mut res = self.root.clone();

        for i in 0..depth {
            let Node::Parent(ref children) = res else {
                return None;
            };

            res = children[self.pos(i)].clone();
        }

        Some(res)
    }

    fn num_children(&self, depth: usize) -> usize {
        if depth <= self.depth {
            self.current_node(depth)
                .map_or(0, |node| node.num_children())
        } else {
            0
        }
    }

    fn descend_at(&mut self, depth: usize) {
        self.tail_node(depth - 1).append_child(Node::new());
        self.depth = self.depth.max(depth);
    }

    fn rotate_at(&mut self, depth: usize) {
        if depth <= self.depth {
            *self.pos_mut(depth) += 1;
            if self.pos(depth) >= self.current_node(depth).unwrap().num_children() {
                *self.pos_mut(depth) = 0;
            }
        }
    }

    fn new() -> Self {
        Self {
            root: Node::new(),
            depth: 0,
            complete: false,
            pos: OnceCell::new(),
        }
    }

    fn value(&mut self) -> Option<Sexpr> {
        let Some(Node::Parent(ref children)) = self.current_node(self.depth) else {
            return None;
        };

        let Node::Leaf(expr) = children[self.pos(self.depth)].clone() else {
            return None;
        };

        Some(expr)
    }
}

#[derive(Clone)]
enum Node {
    Leaf(Sexpr),
    Parent(Rc<Vec<Node>>),
}

impl Node {
    fn new() -> Self {
        Self::Parent(Rc::new(vec![]))
    }

    fn append_child(&self, child: Node) {
        match self {
            Self::Leaf(_) => panic!("Cannot append child to leaf node"),
            Self::Parent(children) => {
                let mut children = children.clone();
                children.push(child)
            }
        }
    }

    fn last_child(&self) -> &Node {
        match self {
            Self::Leaf(_) => panic!("Cannot get last child of leaf node"),
            Self::Parent(children) => children.last().map(|x| &*x).unwrap(),
        }
    }

    fn num_children(&self) -> usize {
        match self {
            Self::Leaf(_) => 0,
            Self::Parent(children) => children.len(),
        }
    }
}
