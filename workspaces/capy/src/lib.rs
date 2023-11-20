#![allow(incomplete_features)]
#![feature(core_intrinsics, thread_local, ptr_metadata, offset_of, adt_const_params, generic_const_exprs)]
#![recursion_limit = "256"]

pub mod bytecode;
pub mod gc;
pub mod interpreter;
pub mod bytecompiler;
pub mod runtime;
pub mod sync;

#[cfg(target_pointer_width = "32")]
compile_error!("32-bit targets are not supported");

