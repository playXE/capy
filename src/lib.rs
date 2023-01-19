#![feature(
    const_refs_to_cell,
    core_intrinsics,
    cstr_from_bytes_until_nul,
    arbitrary_self_types,
    stmt_expr_attributes,
    c_variadic
)]

use prelude::Value;

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

pub mod compiler;
pub mod data;
pub mod io;
pub mod runtime;
pub mod utilities;

pub mod prelude {
    pub use super::ScmResult;
    pub use crate::data::{
        collection::*, environment::*, library::*, procedure::*, symbol::*, value::*,
    };
    pub use crate::runtime::{context::*, *};
    pub use rsgc::{
        sync::{monitor::*, mutex::*},
        system::{
            array::*, collections::hashmap::*, object::*, string::*, traits::*, weak_reference::*,
        },
        thread::*,
    };
}

pub type ScmResult<T = Value> = Result<T, Value>;
