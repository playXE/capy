//! Scheme Intermediate Language. 
//! 
//! Simple SSA IR.

use smallvec::SmallVec;

use crate::runtime::value::ScmValue;


#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, PartialOrd, Ord)]
pub enum Opcode {
    Nop,
    Identity,

    // Constants
    Int,
    Float,
    BigInt,
    String,
    Symbol,
    Char,
    Bool,
    Null,
    Void,
    Vector,
    Bytevector,

    // Arguments
    /// Get an argument from the current frame.
    Rand,
    /// Push an argument 
    PushRand,
    /// Get number of arguments.
    Randc,

    // Variables
    /// Get a variable from the current frame.
    Get,
    /// Set a variable in the current frame.
    Set,
    /// Get a variable from the environment.
    EnvGet,
    /// Set a variable in the environment.
    EnvSet,

    /// Get a global variable.
    GlobalGet,
    /// Set a global variable.
    GlobalSet,

    // Control flow
    /// Unconditional branch
    Branch,
    /// Conditional branch
    BranchIf,

    // Heap operations
    Alloc,

    Car,
    Cdr,
    SetCar,
    SetCdr,
    VectorRef,
    VectorSet,
    BytevectorRef,
    BytevectorSet,

    // Arithmetic
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    Abs,
    Floor,
    Ceiling,
    Truncate,
    Round,
    Max,
    Min,

    // Comparison
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
    Eqv,
    Equal,

    // Type predicates
    IsNull,
    IsPair,
    IsSymbol,
    IsString,
    IsChar,
    IsBool,
    IsVector,
    IsBytevector,
    IsProcedure,
    IsNumber,
    IsInteger,
    IsFloat,
    IsRational,
    IsReal,
    IsComplex,
    IsEq,
    IsEqv,

    // Calls
    /// A call to runtime procedure. Does not accept continuations.
    CCall,
    /// A tail call, it is used to invoke a continuation or a procedure.
    TailCall,

    /// Creates a closure.
    MakeClosure,
}   

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NodeId(pub u32);

#[derive(Clone, PartialEq, Eq)]
pub struct Node {
    pub id: NodeId,
    pub opcode: Opcode,
    pub args: SmallVec<[NodeId; 3]>,
    pub variant: NodeVariant,
}


#[derive(Clone, PartialEq, Eq)]
pub enum NodeVariant {
    None,
    Local(u16),
    Env(u16),
    Value(ScmValue),
    Function(u32),
}