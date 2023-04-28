pub enum Opcode {
    Nop,
    Ldc(u32),
    Ldi(i32),
    /// Load environment variable
    Lde(u32),
    /// Store environment variable
    Ste(u32),

    /// Load global variable
    /// 
    /// Throws:
    /// - `if gloc->value == void`
    Ldg(u32),
    /// Store to global variable
    Stg(u32),

    /// Load accumulator from stack
    Lda(i16),
    /// Store accumulator to stack
    Sta(i16),

    /// Decrement stack pointer
    Pop,
    /// Push accumulator to stack and increment stack pointer
    Push,

    /// tail_call <argc> <profile id>
    TailCall(u16, u32),
    /// call <argc> <profile id>
    Call(u16, u32),

    /// jump <offset>
    Jump(i32),

    /// jump_if_false <offset>
    JumpIfFalse(i32),

    /// jump_if_true <offset>
    JumpIfTrue(i32),

    /// make_closure <argc>
    MakeClosure(u16),

    // Arithmetic
    Add(u32),
    Sub(u32),
    Mul(u32),
    Div(u32),
    Rem(u32),
    Neg(u32),

    Sqrt(u32),
    Abs(u32),
    Floor(u32),
    Ceil(u32),
    Round(u32),
    Trunc(u32),

    // Bitwise
    And(u32),
    Or(u32),
    Xor(u32),
    Shl(u32),
    Shr(u32),
    UShr(u32),

    // Comparison
    Eq(u32),
    Eqv(u32),
    Equals(u32),
    Lt(u32),
    Lte(u32),
    Gt(u32),
    Gte(u32),
    Not,

    // Type
    IsNull,
    IsUndefined,
    IsBoolean,
    IsNumber,
    IsString,
    IsSymbol,
    IsPair,
    IsVector,
    IsNativeProcedure,
    IsPrimitiveProcedure,


}