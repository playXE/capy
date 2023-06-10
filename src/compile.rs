use rsgc::{prelude::{Handle, Object, Allocation}, system::arraylist::ArrayList};

use crate::value::Value;




#[derive(Clone, Copy)]
pub enum IForm {
    Const(Value),
    LRef(Handle<LRef>),
    LSet(Handle<LSet>),
    If(Handle<If>),
    GSet(Handle<GSet>),
    GRef(Handle<GRef>),
    Let(Handle<Let>),
    Receive(Handle<Receive>),
}

impl Object for IForm {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        match self {
            IForm::Const(v) => v.trace(visitor),
            IForm::LRef(l) => l.trace(visitor),
            IForm::LSet(l) => l.trace(visitor),
            IForm::GRef(id) => id.trace(visitor),
            IForm::GSet(gset) => {
                gset.trace(visitor);
            }
            IForm::Let(l) => l.trace(visitor),
            IForm::If(i) => i.trace(visitor),
            IForm::Receive(r) => r.trace(visitor),
            
        }
    }
}

impl Allocation for IForm {}

pub struct LVar {
    pub name: Value,
    pub initval: IForm,
    pub ref_count: usize,
    pub set_count: usize,
}

impl Object for LVar {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.initval.trace(visitor);
        self.name.trace(visitor);
    }
}

impl Allocation for LVar {}

pub struct If {
    pub origin: Value,
    pub cond: IForm,
    pub cons: IForm,
    pub alt: IForm,
}

impl Object for If {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.origin.trace(visitor);
        self.cond.trace(visitor);
        self.cons.trace(visitor);
        self.alt.trace(visitor);
    }
}

impl Allocation for If {}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum LetScope {
    Let,
    Rec,
}

pub struct Let {
    pub origin: Value,
    pub scope: LetScope,
    pub lvars: ArrayList<Handle<LVar>>,
    pub inits: ArrayList<IForm>,
    pub body: IForm,
    
}

impl Object for Let {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.origin.trace(visitor);
        self.lvars.trace(visitor);
        self.inits.trace(visitor);
        self.body.trace(visitor);
    }
}

impl Allocation for Let {}

pub struct LSet {
    pub lvar: Handle<LVar>,
    pub value: IForm,
}

impl Object for LSet {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.lvar.trace(visitor);
        self.value.trace(visitor);
    }
}

pub struct LRef { 
    pub lvar: Handle<LVar>,
}

impl Object for LRef {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.lvar.trace(visitor);
    }
}

impl Allocation for LRef {}
impl Allocation for LSet {}

pub struct GSet {
    pub id: Value,
    pub value: IForm,
}

impl Object for GSet {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.id.trace(visitor);
        self.value.trace(visitor);
    }
}

pub struct GRef {
    pub id: Value,
}

impl Object for GRef {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.id.trace(visitor);
    }
}

impl Allocation for GSet {}
impl Allocation for GRef {}


pub struct Receive {
    pub origin: Value,
    pub reqargs: usize,
    pub optarg: bool,
    pub lvars: ArrayList<Handle<LVar>>,
    pub expr: IForm,
    pub body: IForm,

}

impl Object for Receive {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.origin.trace(visitor);
        self.lvars.trace(visitor);
        self.expr.trace(visitor);
        self.body.trace(visitor);
    }
}

impl Allocation for Receive {}