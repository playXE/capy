use std::cell::Cell;

use once_cell::sync::OnceCell;

use crate::{compiler::env::Env, prelude::*, utilities::arraylist::ArrayList};

#[allow(dead_code)]
pub struct SyntaxRules {
    name: Option<Handle<Symbol>>,
    ellipsis: Handle<Symbol>,
    reserved: HashMap<Handle<Symbol>, ()>,
    literals: HashMap<Handle<Symbol>, ()>,
    patterns: ArrayList<Value>,
    templates: ArrayList<Value>,
    lexical_env: Env,
}

impl Object for SyntaxRules {}
impl Allocation for SyntaxRules {}

impl SyntaxRules {
    pub fn expand(&self, _ctx: &mut Context, input: Value) -> Value {
        input
    }
}

pub struct Matches {}

pub struct MatchTree {
    root: Node,
    depth: usize,
    complete: Cell<bool>,
    pos: OnceCell<ArrayList<usize>>,
}

impl MatchTree {
    pub fn new(ctx: &mut Context) -> Self {
        let node = Node::Parent(ArrayList::new(ctx.mutator()));

        Self {
            root: node,
            depth: 0,
            complete: Cell::new(false),
            pos: OnceCell::new(),
        }
    }

    pub fn pos(&self) -> &[usize] {
        self.pos.get_or_init(|| {
            self.complete.set(true);
            let mut ls = ArrayList::new(Thread::current());
            for _ in 0..self.depth {
                ls.push(Thread::current(), 0);
            }
            ls
        })
    }

    pub fn value(&self) -> Option<Value> {
        let depth = self.depth;
        if let Some(Node::Parent(children)) = self.current_node(depth) {
            if let Node::Leaf(expr) = children[self.pos()[self.depth]] {
                return Some(expr);
            }
        }
        None
    }

    pub fn current_node(&self, depth: usize) -> Option<&Node> {
        let mut res = &self.root;

        for i in 0..depth {
            if let Node::Parent(ref children) = res {
                res = &children[self.pos()[i]]
            } else {
                return None;
            }
        }

        Some(res)
    }
}

impl Object for MatchTree {
    fn trace(&self, visitor: &mut dyn Visitor) {
        self.root.trace(visitor);
        if let Some(pos) = self.pos.get() {
            pos.trace(visitor);
        }
    }
}

pub enum Node {
    Leaf(Value),
    Parent(ArrayList<Node>),
}

impl Object for Node {
    fn trace(&self, visitor: &mut dyn Visitor) {
        match self {
            Node::Leaf(v) => v.trace(visitor),
            Node::Parent(nodes) => nodes.trace(visitor),
        }
    }
}

impl Allocation for Node {}
