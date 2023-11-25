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

impl super::cell::Cell for CodeBlock {
    const TAG: super::cell::CellTag = super::cell::CellTag::CODE_BLOCK;
}

impl std::ops::Deref for CellReference<CodeBlock> {
    type Target = CodeBlock;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.to_ptr().cast() }
    }
}

impl std::ops::DerefMut for CellReference<CodeBlock> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.to_ptr().cast() }
    }
}