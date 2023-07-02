use parse_display::Display;
use rsgc::prelude::Handle;
use termcolor::{ColorSpec, WriteColor};

use crate::runtime::object::CodeBlock;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display)]
#[display(style = "snake_case")]
pub enum Opcode {
    NoOp,
    /// Regular `enter` opcode. Means that function is not yet JIT compiled.
    /// Will simply increment hotness counter until certain threshold is reached.
    /// After JIT compilation is requested changes to `EnterCompiling`.
    Enter,
    /// Current function is being compiled with JIT, no-op for interpreter.
    EnterCompiling,
    /// Function compiled, background thread patched `EnterCompiling` to this opcode.
    /// 
    /// Will enter JITed code.
    EnterJit,
    /// Function was blacklisted from JIT compilation.
    EnterBlacklisted,
    Pop,
    Popn,
    Dup,
    Swap,

    Alloc,
    AllocBelow,
    Reset,
    LdArg,
    SetArg,
    PushUndef,
    PushNull,
    PushTrue,
    PushFalse,
    PushInt32,
    PushDouble,
    PushConstant,
    PushProcedure,

    GlobalRef,
    GlobalSet,

    Pack,
    Flatpack,
    Unpack,

    MakeClosure,

    Call,
    TailCall,
    Return,
    Apply,

    AssertArgCount,
    AssertMinArgCount,
    NoMatchingArgCount,
    CollectRest,

    StackSet,
    StackGet,
    Box,
    /// Boxes stack slot at offset `n` and stores value from stack to it.
    StackBox,
    BoxRef,
    BoxSet,

    Define,

    ClosureRef,
    ClosureSet,

    Branch,
    BranchIf,
    BranchIfNot,

    KeepBranchIfNot,
    BranchIfArgMismatch,
    BranchIfMinArgMismatch,

    IsNumber,
    IsComplex,
    IsReal,
    IsRational,
    IsInteger,
    IsExactInteger,
    IsExactNonnegativeInteger,
    IsExactPositiveInteger,
    IsFixnum,
    IsInexactReal,
    IsFlonum,
    IsExact,
    IsInexact,
    IsPair,
    IsNull,
    IsUndef,
    IsList,
    IsVector,
    IsTuple,
    IsStructTypeProperty,
    IsStruct,
    IsStructType,
    //IsProperty,
    List,
    Cons,
    Car,
    Cdr,
    SetCar,
    SetCdr,
    Vector,
    VectorRef,
    VectorRefI,
    VectorSet,
    VectorSetI,
    Tuple,
    TupleRef,
    TupleSet,
    TupleRefI,
    TupleSetI,
    ListToVector,
    VectorAppend,
    StructPropPred,
    StructRef,
    StructSet,
    StructPred,

    Not,

    Count,
    Wide = 255,
}

pub fn disassembly(
    code: Handle<CodeBlock>,
    mut out: impl WriteColor,
) -> Result<(), std::io::Error> {
    let mut ip = 0;

    let cb = code;
    let code = cb.code();
    macro_rules! read2 {
        () => {{
            let [a, b]: [u8; 2] = code[ip..ip + 2].try_into().unwrap();
            ip += 2;
            u16::from_le_bytes([a, b])
        }};
    }

    macro_rules! read4 {
        () => {{
            let [a, b, c, d]: [u8; 4] = code[ip..ip + 4].try_into().unwrap();
            ip += 4;
            u32::from_le_bytes([a, b, c, d])
        }};
    }

    macro_rules! read8 {
        () => {{
            let [a, b, c, d, e, f, g, h]: [u8; 8] = code[ip..ip + 8].try_into().unwrap();
            ip += 8;
            u64::from_le_bytes([a, b, c, d, e, f, g, h])
        }};
    }
    if cb.literals.vector_len() != 0 {
        writeln!(out, "constants:")?;
        for i in 0..cb.literals.vector_len() {
            let val = cb.literals.vector_ref(i);
            writeln!(out, "  {}: '{:?}'", i, val)?;
        }
    }

    while ip < code.len() {
        let op = code[ip];

        if op >= Opcode::Count as u8 {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "invalid opcode",
            ));
        }

        let op = unsafe { std::mem::transmute::<u8, Opcode>(op) };
        write!(out, "{:04}: ", ip)?;
        out.set_color(&ColorSpec::new().set_fg(Some(termcolor::Color::Blue)))?;
        write!(out, "{}", op)?;
        out.reset()?;
        ip += 1;
        match op {
            Opcode::Alloc
            | Opcode::AllocBelow
            | Opcode::LdArg
            | Opcode::SetArg
            | Opcode::StackBox
            | Opcode::GlobalRef
            | Opcode::GlobalSet
            | Opcode::Pack
            | Opcode::Flatpack
            | Opcode::MakeClosure
            | Opcode::Call
            | Opcode::TailCall
            | Opcode::Apply
            | Opcode::AssertArgCount
            | Opcode::AssertMinArgCount
            | Opcode::CollectRest
            | Opcode::StackSet
            | Opcode::StackGet
            | Opcode::ClosureRef
            | Opcode::ClosureSet
            | Opcode::Vector
            | Opcode::List
            | Opcode::Popn
            | Opcode::VectorRef
            | Opcode::VectorSet
            | Opcode::Tuple
            | Opcode::TupleRef
            | Opcode::TupleSet => {
                let n = read2!();
                writeln!(out, " {}", n)?;
            }

            Opcode::Unpack => {
                let n = read2!() as u16;
                let overflow = code[ip] != 0;
                ip += 1;
                writeln!(out, " {}, ovf={}", n, overflow)?;
            }

            Opcode::Define => {
                let n = read2!() as u16;
                let c = code[ip] != 0;
                ip += 1;
                writeln!(out, " {}, constant={}", n, c)?;
            }

            Opcode::Branch => {
                let n = read4!() as i32;
                writeln!(out, " {}; => {}", n, ip as i32 + n)?;
            }

            Opcode::BranchIf | Opcode::BranchIfNot | Opcode::KeepBranchIfNot => {
                let n = read4!() as i32;

                writeln!(out, " {}; => {}", n, ip as i32 + n,)?;
            }

            Opcode::BranchIfArgMismatch | Opcode::BranchIfMinArgMismatch => {
                let argc = read2!();
                let n = read4!() as i32;

                writeln!(out, " {}, {}; => {}", argc, n, ip as i32 + n,)?;
            }

            Opcode::PushConstant => {
                let n = read2!();
                writeln!(out, " {}", n)?;
            }

            Opcode::PushInt32 => {
                let n = read4!();
                writeln!(out, " {}", n as i32)?;
            }

            Opcode::PushDouble => {
                let n = read8!();
                writeln!(out, " {}", f64::from_bits(n))?;
            }

            Opcode::PushProcedure => {
                let n = read2!();
                writeln!(out, " {}", n)?;
            }
            _ => {
                writeln!(out, "")?;
            }
        }
    }

    Ok(())
}
