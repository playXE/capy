use crate::prelude::*;

pub struct SpecialForm {
    pub kind: Form,
    pub original_name: Option<Handle<Str>>
}

pub enum Form {
    Primitive(FormCompiler),
    Macro(Handle<Procedure>)
}

impl Object for Form {
    fn trace(&self, visitor: &mut dyn Visitor) {
        match self {
            Form::Primitive(_) => {}
            Form::Macro(proc) => proc.trace(visitor)
        }
    }
}

impl SpecialForm {
    pub fn name(&self) -> String {
        if let Some(original_name) = self.original_name {
            match self.kind {
                Form::Primitive(_) => original_name.to_string(),
                Form::Macro(_) => format!("{}@{:p}", original_name, self)
            }
        } else {
            format!("{:p}", self)
        }
    }
}

impl Object for SpecialForm {
    fn trace(&self, visitor: &mut dyn Visitor) {
        self.original_name.trace(visitor);
        self.kind.trace(visitor);
    }
}

impl Allocation for SpecialForm {}

pub struct Macro(pub Handle<Procedure>);

impl Object for Macro {
    fn trace(&self, visitor: &mut dyn Visitor) {
        self.0.trace(visitor);
    }
}

impl Allocation for Macro {}