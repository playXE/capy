use std::collections::HashMap;

use crate::runtime::object::ObjectHeader;
use libloading::Library;

use super::value::Value;
#[repr(C)]
pub struct Dynlib {
    header: ObjectHeader,
    lib: Option<Library>
}


struct DynamicLibs {
    dso_table: HashMap<Value, Dynlib>,
}