//! setjmp/longjmp implementation for Rust based on musl libc

use std::mem::size_of;

cfg_if::cfg_if! {
    if #[cfg(target_arch="x86_64")] {
        mod x86_64;
        use x86_64 as arch;
    } else if #[cfg(target_arch="aarch64")] {
        mod aarch64;
        use aarch64 as arch;
    /* } else if #[cfg(target_arch="arm")] {
        mod arm;
        use arm as arch;*/
    } else if #[cfg(all(target_pointer_width="32", target_arch="x86"))] {
        mod i386;
        use i386 as arch;
    } else if #[cfg(target_arch="riscv64")] {
        mod riscv64;
        use riscv64 as arch;
    } else {
        compile_error!("Unsupported architecture");
    }
}

#[allow(non_camel_case_types)]
type __jmp_buf = [u64; 15];

#[repr(C)]
#[derive(Default)]
pub struct JmpBuf {
    __jb: __jmp_buf,
    __fl: u64,
    __ss: [u64; 128 / size_of::<i64>()],
}

impl JmpBuf {
    pub const fn new() -> Self {
        Self {
            __jb: [0; 15],
            __fl: 0,
            __ss: [0; 128 / size_of::<i64>()],
        }
    }
}

pub type JumpBuf = JmpBuf;

pub use arch::*;
