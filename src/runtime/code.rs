use once_cell::sync::OnceCell;
use rsgc::{
    system::{
        array::Array,
        object::{Allocation, Handle},
        traits::Object,
    },
    thread::Thread,
};

use crate::{
    prelude::{Arity, Value, Library},
    utilities::{arraylist::ArrayList, vec_to_gc},
};

pub struct Code {
    pub instructions: ArrayList<Ins>,
    pub constants: ArrayList<Value>,
    pub fragments: ArrayList<Handle<Code>>,
    pub arity: once_cell::sync::OnceCell<Handle<Array<Arity>>>,
    pub module: Handle<Library>,
}

impl Code {
    pub fn new(
        thread: &mut Thread,
        instructions: ArrayList<Ins>,
        constants: ArrayList<Value>,
        fragments: ArrayList<Handle<Code>>,
        module: Handle<Library>,
    ) -> Handle<Self> {
        thread.safepoint();
        let code = Code {
            instructions,
            constants,
            fragments,
            module,
            arity: OnceCell::new(),
        };
        thread.allocate(code)
    }

    pub fn arity(&self) -> &[Arity] {
        self.arity
            .get_or_init(|| {
                let thread = Thread::current();
                let mut arities = vec![];

                let mut i = 0;

                while i < self.instructions.len() {
                    match self.instructions[i] {
                        Ins::AssertArgCount(n) => {
                            arities.push(Arity::Exact(n as _));
                            return vec_to_gc(thread, arities);
                        }

                        Ins::AssertMinArgCount(n) => {
                            arities.push(Arity::AtLeast(n as _));
                            return vec_to_gc(thread, arities);
                        }

                        Ins::BranchIfArgMismatch(offset, n) => {
                            arities.push(Arity::Exact(n as _));
                            i = (i as isize + offset as isize) as usize;
                            break;
                        }

                        Ins::BranchIfMinArgMismatch(offset, n) => {
                            arities.push(Arity::AtLeast(n as _));
                            i = (i as isize + offset as isize) as usize;
                            break;
                        }

                        _ => i += 1,
                    }
                }

                if i >= self.instructions.len() {
                    arities.push(Arity::AtLeast(0));
                    return vec_to_gc(thread, arities);
                }

                while i < self.instructions.len() {
                    match self.instructions[i] {
                        Ins::BranchIfArgMismatch(offset, n) => {
                            arities.push(Arity::Exact(n as _));
                            i = (i as isize + offset as isize) as usize;
                        }
                        Ins::BranchIfMinArgMismatch(offset, n) => {
                            arities.push(Arity::AtLeast(n as _));
                            i = (i as isize + offset as isize) as usize;
                        }
                        _ => return vec_to_gc(thread, arities)
                    }
                }

            

                vec_to_gc(thread, arities)
            })
            .as_ref()
    }

}

impl Object for Code {
    fn trace(&self, visitor: &mut dyn rsgc::system::traits::Visitor) {
        self.instructions.trace(visitor);
        self.constants.trace(visitor);
        self.fragments.trace(visitor);

        if let Some(arity) = self.arity.get() {
            arity.trace(visitor);
        }
    }
}
impl Allocation for Code {}

#[derive(Clone, Copy, Debug)]
pub enum Ins {
    Pop,
    Dup,
    Swap,
    Alloc(u16),
    AllocBelow(u16),
    Reset(i32, u16),
    PushUndef,
    PushVoid,
    PushEof,
    PushNull,
    PushTrue,
    PushFalse,
    PushFixnum(i32),
    PushConstant(u32),
    PushProcedure(u32),

    Pack(u32),
    Flatpack(u32),
    Unpack(u32, bool),

    PushFragment(u16),
    MakeClosure(i16, u16, u16),
    MakeTaggedClosure(i16, u16, u16),

    MakeFrame,
    InjectFrame,

    Call(u16),
    TailCall(u16),
    Apply(u16),
    Return,
    AssertArgCount(u16),
    AssertMinArgCount(u16),
    CollectRest(u16),

    Compile,

    MakeSyntax(u16),
    MakePromise,
    MakeStream,
    Force,
    StoreInPromise,
    MakeLocalVariable(u16),
    MakeVariableArgument(u16),

    PushGlobal(u32),
    SetGlobal(u32),
    DefineGlobal(u32, bool),
    PushCaptured(u16),
    SetCaptured(u16),

    PushLocal(u16),
    SetLocal(u16),

    Branch(i32),
    BranchIf(i32),
    BranchIfNot(i32),
    KeepOrBranchIfNot(i32),
    BranchIfArgMismatch(i32, u16),
    BranchIfMinArgMismatch(i32, u16),

    Or(i32),
    And(i32),

    Eq,
    Eqv,
    Equal,
    IsPair,
    IsNull,
    IsUndef,
    List(u32),
    Cons,
    DeCons,
    DeConsKeyword,
    Car,
    Cdr,
    Vector(u32),
    ListToVector,
    VectorAppend(u32),
    IsVector,
    Not,

    FailIfNotNull,
    NoOp,
}

impl Object for Ins {}
impl Allocation for Ins {
    const NO_HEAP_PTRS: bool = true;
}
