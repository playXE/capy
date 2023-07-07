//! # Baseline JIT
//! 
//! Directly compiles bytecode to B3 IR, and then to machine code.

pub mod stack2ssa;
pub mod thunks;