use std::ffi::OsStr;

use crate::{
    raise_exn,
    runtime::object::ObjectHeader,
    vm::{callframe::CallFrame, scm_vm},
};
use libloading::Library;
use rsgc::prelude::{Allocation, Handle, Object};

use super::{
    error::wrong_contract,
    foreign::scm_from_pointer,
    fun::scm_make_subr,
    module::{scm_define, scm_foreign_module},
    object::{ScmResult, Type},
    symbol::Intern,
    value::Value,
};
#[repr(C)]
pub struct Dynlib {
    header: ObjectHeader,
    lib: Option<Library>,
}

unsafe impl Object for Dynlib {}
unsafe impl Allocation for Dynlib {
    const FINALIZE: bool = true;
    const DESTRUCTIBLE: bool = true;
}

impl Drop for Dynlib {
    fn drop(&mut self) {
        let _ = self.lib.take();
    }
}

impl Value {
    pub fn is_dynlib(self) -> bool {
        self.is_xtype(Type::Dynlib)
    }

    pub fn dynlib(self) -> Handle<Dynlib> {
        debug_assert!(self.is_dynlib());
        unsafe { std::mem::transmute(self) }
    }
}

extern "C" fn dlopen(cfr: &mut CallFrame) -> ScmResult {
    let name = cfr.argument(0);

    if !name.is_string() {
        return wrong_contract::<()>("dlopen", "string?", 0, 1, cfr.arguments()).into();
    }

    let name = name.string().to_string();

    unsafe {
        let os_str = OsStr::new(&name);
        let lib = Library::new(os_str)
            .map_err(|err| raise_exn!((), Fail, &[], "dlopen: {}", err).unwrap_err())?;

        let dynlib = Dynlib {
            header: ObjectHeader::new(Type::Dynlib),
            lib: Some(lib),
        };

        ScmResult::ok(scm_vm().mutator().allocate(dynlib))
    }
}

extern "C" fn dlclose(cfr: &mut CallFrame) -> ScmResult {
    let lib = cfr.argument(0);

    if !lib.is_dynlib() {
        return wrong_contract::<()>("dlclose", "dynlib?", 0, 1, cfr.arguments()).into();
    }

    let lib = lib.dynlib().lib.take();

    if let Some(lib) = lib {
        drop(lib);
    }

    ScmResult::ok(Value::encode_undefined_value())
}

extern "C" fn dlsym(cfr: &mut CallFrame) -> ScmResult {
    let lib = cfr.argument(0);
    let name = cfr.argument(1);

    if !lib.is_dynlib() {
        return wrong_contract::<()>("dlsym", "dynlib?", 0, 2, cfr.arguments()).into();
    }

    if !name.is_string() {
        return wrong_contract::<()>("dlsym", "string?", 1, 2, cfr.arguments()).into();
    }
    let lib = lib.dynlib();
    let lib = lib.lib.as_ref().unwrap();
    let name = name.string().to_string();

    unsafe {
        let symbol = lib
            .get::<*mut ()>(name.as_bytes())
            .map_err(|err| raise_exn!((), Fail, &[], "dlsym: {}", err).unwrap_err())?;

        ScmResult::ok(scm_from_pointer(symbol.cast()))
    }
}

pub(crate) fn init_dload() {
    let module = scm_foreign_module().module();

    let subr = scm_make_subr("dlopen", dlopen, 1, 1);
    scm_define(module, "dlopen".intern(), subr).unwrap();

    let subr = scm_make_subr("dlclose", dlclose, 1, 1);
    scm_define(module, "dlclose".intern(), subr).unwrap();

    let subr = scm_make_subr("dlsym", dlsym, 2, 2);
    scm_define(module, "dlsym".intern(), subr).unwrap();
}
