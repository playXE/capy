#![feature(
    core_intrinsics,
    thread_local,
    arbitrary_self_types,
    const_refs_to_cell
)]

#[macro_export]
macro_rules! offsetof {
    ($obj: ty, $($field: ident).+) => {{
        #[allow(unused_unsafe)]
        unsafe {
            let addr = 0x4000 as *const $obj;
            &(*addr).$($field).* as *const _ as usize - 0x4000
        }
    }
    };
}

#[macro_export]
macro_rules! cassert {
    ($($rest:tt)*) => {
        #[cfg(feature="debug-assertions")]
        {
            assert!($($rest)*);
        }

        #[cfg(not(feature="no-assert"))]
        {
           
        }
    };
}

pub use once_cell::sync::Lazy;

#[macro_export]
macro_rules! define_proc {
    (extern $sname: expr, $name: ident  ($vm: ident, $k: ident, $args: ident) $mina: expr, $maxa: expr => $e: expr) => {
        pub fn $name($vm: &mut $crate::vm::Vm, $k: $crate::value::Value, $args: &[$crate::value::Value]) -> $crate::vm::Trampoline {
            $e
        }

        paste::paste! {
            pub static [<$name:upper _PROC>]: $crate::Lazy<$crate::value::Value> = $crate::Lazy::new(|| {
                $crate::vm::Vm::make_procedure($sname, $name, $mina, $maxa)
            });

            pub static [<$name:upper _NAME>]: $crate::Lazy<$crate::value::Value> = $crate::Lazy::new(|| {
                $crate::vm::intern($sname)
            });
        }
    };

    (extern $sname: expr, $name: ident  ($vm: ident, $args: ident) $mina: expr, $maxa: expr => $e: expr) => {
        pub fn $name($vm: &mut $crate::vm::Vm, _: $crate::value::Value, $args: &[$crate::value::Value]) -> $crate::vm::Trampoline {
            $e
        }

        paste::paste! {
            pub static [<$name:upper _PROC>]: $crate::Lazy<$crate::value::Value> = $crate::Lazy::new(|| {
                $crate::vm::Vm::make_procedure($sname, $name, $mina, $maxa)
            });

            pub static [<$name:upper _NAME>]: $crate::Lazy<$crate::value::Value> = $crate::Lazy::new(|| {
                $crate::vm::intern($sname)
            });
        }
    };
}

#[macro_use]
pub mod ports;
//pub mod module;
pub mod case;
pub mod jit;
pub mod r5rs;
pub mod value;
pub mod vm;
//pub mod simple_eval;
pub mod r#bool;
pub mod compiler;
pub mod error;
pub mod fun;
pub mod hash;
pub mod list;
pub mod number;
pub mod precomp;
pub mod string;
pub mod ports_v2;
pub mod structure;
pub mod util;
pub mod utils;
pub mod print;
pub mod pp;

pub use rsgc;
