
#![feature(const_refs_to_cell, core_intrinsics)]


#[macro_export]
macro_rules! scm_for_each {
    ($p: ident, $list: expr, $body: expr) => {
        $p = $list;
        #[allow(unreachable_code)]
        while $p.is_pair() {
            $body;
            $p = $p.cdr();
        }
    };
}

pub mod data;
pub mod io;
pub mod utilities;
pub mod compiler;
pub mod runtime;

pub mod prelude {
    pub use crate::data::{
        environment::*,
        collection::*,
        symbol::*,
        value::*,
        procedure::*,
        library::*
    };
    pub use crate::runtime::{
        *,
        context::*
    };
    pub use rsgc::{
        sync::{monitor::*, mutex::*},
        system::{
            array::*, collections::hashmap::*, object::*, string::*, traits::*, weak_reference::*,
        },
        thread::*,
    };
}


