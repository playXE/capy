use std::time::{Duration, Instant};

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
    compiler::Capture,
    prelude::{Arity, Library, Value},
    utilities::{arraylist::ArrayList, vec_to_gc}, 
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
            captures: ArrayList::new(thread),
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
                        _ => return vec_to_gc(thread, arities),
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
#[repr(C, u8)]
pub enum Ins {
    Pop,        //=0,
    Dup,        //=1,
    Swap,       //=2,
    Alloc(u16), //=3,

    Reset(i32, u16),    //=5,
    PushUndef,          //=6,
    PushVoid,           //=7,
    PushEof,            //=8,
    PushNull,           //=9,
    PushTrue,           //=10,
    PushFalse,          //=11,
    PushFixnum(i32),    //=12,
    PushConstant(u32),  //=13,
    PushProcedure(u32), //=14,

    MakeFrame,   //=21,
    InjectFrame, //=22,

    Call(u16),              //=23,
    TailCall(u16),          //=24,
    Apply(u16),             //=25,
    Return,                 //=26,
    AssertArgCount(u16),    //=27,
    AssertMinArgCount(u16), //=28,
    CollectRest(u16),       //=29,

    MakeSyntax(i32),           //=31,
    MakePromise,               //=32,
    MakeStream,                //=33,
    Force,                     //=34,
    StoreInPromise,            //=35,
    MakeLocalVariable(u16),    //=36,
    MakeVariableArgument(u16), //=37,

    PushGlobal(u32),         //=38,
    SetGlobal(u32),          //=39,
    DefineGlobal(u32, bool), //=40,
    PushCaptured(u16),       //=41,
    SetCaptured(u16),        //=42,

    PushLocal(u16), //=43,
    SetLocal(u16),  //=44,

    Branch(i32),                      //=45,
    BranchIf(i32),                    //=46,
    BranchIfNot(i32),                 //=47,
    KeepOrBranchIfNot(i32),           //=48,
    BranchIfArgMismatch(i32, u16),    //=49,
    BranchIfMinArgMismatch(i32, u16), //=50,

    Add2,            //=70,
    Sub2,            //=71,
    Mul2,            //=72,
    Div2,            //=73,
    Greater2,        //=74,
    Less2,           //=75,
    GreaterEq2,      //=76,
    LessEq2,         //=77,
    Equal2,          //=78,
    Eqv2,            //=79,
    Eq2,             //=80,
    AllocBelow(u16), //=4,
    Or(i32),         //=51,
    And(i32),        //=52,

    Compile,           //=30,
    Eq,                //=53,
    Eqv,               //=54,
    Equal,             //=55,
    IsPair,            //=56,
    IsNull,            //=57,
    IsUndef,           //=58,
    List(u32),         //=59,
    Cons,              //=60,
    DeCons,            //=61,
    DeConsKeyword,     //=62,
    Car,               //=63,
    Cdr,               //=64,
    Vector(u32),       //=65,
    ListToVector,      //=66,
    VectorAppend(u32), //=67,
    IsVector,          //=68,
    Not,               //=69,

    Pack(u32),         //=15,
    Flatpack(u32),     //=16,
    Unpack(u32, bool), //=17,

    AssertStruct(u8, u32),
    CheckStruct(u32),
    StructRef(u32),
    StructSet(u32),
    MakeStruct(u32),

    StructRefI(u32, u16),
    StructSetI(u32, u16),

    CheckStructProperty(u32),
    StructPropertyAccessor(u32, bool),

    PushFragment(u16),                //=18,
    MakeClosure(i16, u16, u16),       //=19,
    MakeTaggedClosure(i16, u16, u16), //=20,

    FailIfNotNull, //=81,
    NoOp,          //=82,
    Last,          //=83,
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
            print!(" {:04}: ", i);
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
                Ins::PushConstant(n) => print!(
                    "push-constant {} ; => {}",
                    n,
                    self.constants[n as usize].to_string(false)
                ),
                Ins::PushGlobal(n) => print!(
                    "push-global {} ; => {}",
                    n,
                    self.constants[n as usize].to_string(false)
                ),
                Ins::SetGlobal(n) => print!(
                    "set-global {} ; => {}",
                    n,
                    self.constants[n as usize].to_string(false)
                ),
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

                _ => print!("{:?}", ins),
            }
            println!();
        }
    }
}

#[derive(Clone, Copy)]
pub struct InsProfile {
    discriminant: u8,
    total: u64,
    total_time: u128,
}

impl InsProfile {
    pub fn add(ins: Ins, t: Instant) {
        unsafe {
            let time = t.elapsed().as_nanos();
            let profile: &mut InsProfile =
                &mut PROFILES[std::mem::transmute::<_, u8>(std::mem::discriminant(&ins)) as usize];
            profile.total += 1;
            profile.total_time += time;
        }
    }
}

pub static mut PROFILES: [InsProfile; unsafe {
    std::mem::transmute::<_, u8>(std::mem::discriminant(&Ins::Last)) as usize
}] = [InsProfile {
    discriminant: 0,
    total: 0,
    total_time: 0,
}; unsafe { std::mem::transmute::<_, u8>(std::mem::discriminant(&Ins::Last)) as usize }];

pub const fn instruction_discriminant(ins: Ins) -> u8 {
    unsafe {
        let discr = std::mem::discriminant(&ins);
        std::mem::transmute::<_, u8>(discr)
    }
}

pub fn instruction_name(i: usize) -> &'static str {
    match i {
        0 => "pop",
        1 => "dup",
        2 => "swap",
        3 => "alloc",
        4 => "alloc-below",
        5 => "reset",
        6 => "push-undef",
        7 => "push-void",
        8 => "push-eof",
        9 => "push-null",
        10 => "push-true",
        11 => "push-false",
        12 => "push-fixnum",
        13 => "push-constant",
        14 => "push-procedure",
        15 => "pack",
        16 => "flatpack",
        17 => "unpack",
        18 => "push-fragment",
        19 => "make-closure",
        20 => "make-tagged-closure",
        21 => "make-frame",
        22 => "inject-frame",
        23 => "call",
        24 => "tail-call",
        25 => "apply",
        26 => "return",
        27 => "assert-arg-count",
        28 => "assert-min-arg-count",
        29 => "collect-rest",
        30 => "compile",
        31 => "make-syntax",
        32 => "make-promise",
        33 => "make-stream",
        34 => "force",
        35 => "store-in-promise",
        36 => "make-local-variable",
        37 => "make-variable-argument",

        38 => "push-global",
        39 => "set-global",
        40 => "define-global",
        41 => "push-captured",
        42 => "set-captured",
        43 => "push-local",
        44 => "set-local",

        45 => "branch",
        46 => "branch-if",
        47 => "branch-if-not",
        48 => "keep-or-branch-if-not",
        49 => "branch-if-arg-mismatch",
        50 => "branch-if-not-arg-mismatch",

        51 => "or",
        52 => "and",

        53 => "eq?",
        54 => "eqv?",
        55 => "equal?",

        56 => "pair?",
        57 => "null?",
        58 => "undef?",
        59 => "list",
        60 => "cons",
        61 => "decons",
        62 => "decons-kword",
        63 => "car",
        64 => "cdr",
        65 => "vector",
        66 => "list->vector",
        67 => "vector-append",
        68 => "vector?",
        69 => "not",

        70 => "add2",
        71 => "sub2",
        72 => "mul2",
        73 => "div2",
        74 => "greater2",
        75 => "less2",
        76 => "greater-equal2",
        77 => "less-equal2",
        78 => "equal2",
        79 => "eqv2",
        80 => "eq2",
        81 => "fail-if-not-null",
        82 => "no-op",
        83 => "last",
        _ => unreachable!(),
    }
}

pub fn dump_profiles() {
    println!("instruction profiles:");
    unsafe {
        for (i, profile) in PROFILES.iter_mut().enumerate() {
            profile.discriminant = i as _;
        }

        PROFILES.sort_by_key(|p| p.total);

        for profile in PROFILES.iter() {
            if profile.total > 0 {
                let avg = profile.total_time / profile.total as u128;
                println!(
                    " {:04}: {}\t\t {} executions, {} ns/op, total op time: {:.4}ms",
                    profile.discriminant,
                    instruction_name(profile.discriminant as _),
                    profile.total,
                    avg,
                    Duration::from_nanos(profile.total_time as _).as_micros() as f64 / 1000.0
                );
            }
        }
    }
}
