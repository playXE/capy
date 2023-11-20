use super::{value::{TaggedValue, Word, Tagged}, cell::{CellReference, Vector}};


/// Bytecode storage for a function.
#[repr(C)]
pub struct CodeBlock {
    /// Points to constant vector. Always uncompressed for 
    /// performance reasons in LLInt. We do not want
    /// to always uncompress it when we need to fetch a constant.
    pub constant_pool: Tagged<CellReference<Vector>>,
    pub opt_delay_counter: Word,
    pub reopt_try_counter: Word,
    pub num_callee_locals: Word,
    pub num_vars: Word,
    pub num_parameters: Word,
    pub instructions: *const u8,
}
