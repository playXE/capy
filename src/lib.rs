#![feature(core_intrinsics, thread_local, ptr_metadata)]
#![recursion_limit = "256"]
pub mod runtime;
pub mod gc;
pub mod bytecode;
pub mod sync;
pub mod interpreter;

#[cfg(target_pointer_width="32")]
compile_error!("32-bit targets are not supported"); 