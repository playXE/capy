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
    utilities::{arraylist::ArrayList, vec_to_gc}, compiler::Capture,
};

pub struct Code {
    pub instructions: ArrayList<Ins>,
    pub constants: ArrayList<Value>,
    pub fragments: ArrayList<Handle<Code>>,
    pub arity: once_cell::sync::OnceCell<Handle<Array<Arity>>>,
    pub module: Handle<Library>,
    pub captures: ArrayList<ArrayList<Capture>>,
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
            captures: ArrayList::new(thread)
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
        self.module.trace(visitor);
        self.captures.trace(visitor);
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

    MakeSyntax(i32),
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


impl Code {
    pub fn dump(&self) {
        println!("code at {:p}:", self);
        println!("constants: ");
        for (i, c) in self.constants.iter().enumerate() {
            println!(" {:04}: {}", i, c.to_string(false));
        }

        println!("instructions: ");
        for (i, ins) in self.instructions.iter().enumerate() {
            print!(" {:04}: ",i);
            match *ins {
                Ins::Pop => print!("pop"),
                Ins::Dup => print!("dup"),
                Ins::Swap => print!("swap"),
                Ins::Alloc(n) => print!("alloc {}", n),
                Ins::AllocBelow(n) => print!("alloc-below {}", n),
                Ins::Reset(n, i) => print!("reset {} {}", n, i),
                Ins::PushUndef => print!("push-undef"),
                Ins::PushVoid => print!("push-void"),
                Ins::PushEof => print!("push-eof"),
                Ins::PushNull => print!("push-null"),
                Ins::PushTrue => print!("push-true"),
                Ins::PushFalse => print!("push-false"),
                Ins::PushFixnum(n) => print!("push-fixnum {}", n),
                Ins::PushConstant(n) => print!("push-constant {} ; => {}", n, self.constants[n as usize].to_string(false)),
                Ins::PushGlobal(n) => print!("push-global {} ; => {}", n, self.constants[n as usize].to_string(false)),
                Ins::SetGlobal(n) => print!("set-global {} ; => {}", n, self.constants[n as usize].to_string(false)),
                Ins::PushLocal(n) => print!("push-local {}", n),
                Ins::PushProcedure(n) => print!("push-procedure {}", n),

                Ins::Pack(n) => print!("pack {}", n),
                Ins::Unpack(n, overflow) => print!("unpack {} {}", n, overflow),
                Ins::Flatpack(n) => print!("flatpack {}", n),

                Ins::PushFragment(n) => print!("push-fragment {}", n),
                Ins::MakeClosure(n, i, j) => print!("make-closure {} {} {}", n, i, j),
                Ins::MakeTaggedClosure(n, i, j) => print!("make-tagged-closure {} {} {}", n, i, j),
                
                Ins::MakeFrame => print!("make-frame"),
                Ins::InjectFrame => print!("inject-frame"),
                
                Ins::Call(n) => print!("call {}", n),
                Ins::TailCall(n) => print!("tail-call {}", n),
                Ins::Return => print!("return"),
                Ins::Apply(n) => print!("apply {}", n),
                Ins::AssertArgCount(n) => print!("assert-arg-count {}", n),
                Ins::AssertMinArgCount(n) => print!("assert-min-arg-count {}", n),
                Ins::CollectRest(n) => print!("collect-rest {}", n),
                
                Ins::Compile => print!("compile"),

                Ins::MakeSyntax(n) => print!("make-syntax {}", n),
                Ins::SetLocal(n) => print!("set-local {}", n),
                Ins::PushCaptured(n) => print!("push-captured {}", n),
                Ins::SetCaptured(n) => print!("set-captured {}", n),

                Ins::Branch(off) => {
                    let to = i as i32 + off;
                    print!("branch {} ; => {:04}", off, to)
                }

                Ins::BranchIf(off) => {
                    let to = i as i32 + off;
                    print!("branch-if {} ; => {:04}", off, to)
                }

                Ins::BranchIfNot(off) => {
                    let to = i as i32 + off;
                    print!("branch-if-not {} ; => {:04}", off, to)
                }

                Ins::KeepOrBranchIfNot(off) => {
                    let to = i as i32 + off;
                    print!("keep-or-branch-if-not {} ; => {:04}", off, to)
                }

                Ins::BranchIfArgMismatch(off, n) => {
                    let to = i as i32 + off;
                    print!("branch-if-arg-mismatch {} {} ; => {:04}", off, n, to)
                }

                _ => print!("{:?}", ins)
            }
            println!();
        }

    }
}