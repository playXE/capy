use std::collections::hash_map::RandomState;
use std::intrinsics::unlikely;
use std::mem::MaybeUninit;

use crate::runtime::fun::scm_make_subr;
use crate::runtime::list::{scm_list, scm_member};
use crate::runtime::module::{scm_capy_module, scm_define, scm_internal_module};
use crate::runtime::string::make_string;
use crate::runtime::symbol::Intern;
use crate::vm::callframe::CallFrame;
use crate::vm::interpreter::scm_eval;
use crate::vm::scm_vm;
use crate::{raise_exn, scm_for_each};
use crate::{runtime::value::Value, vm::scm_current_module};
use rsgc::heap::heap::heap;
use rsgc::heap::root_processor::SimpleRoot;
use rsgc::prelude::{Handle, Object};
use rsgc::sync::mutex::{Condvar, RawMutex};
use rsgc::system::collections::hashmap::HashMap;
use rsgc::thread::Thread;

use super::error::wrong_contract;
use super::list::{assoc_delete_x, scm_acons, scm_assoc_ref, scm_assq, scm_cons, scm_is_list};
use super::module::scm_reqbase_module;
use super::object::{Module, ScmResult, GLOC};
use super::port::{
    port_input_pred, port_open_file, Port, SCM_PORT_BUFFER_MODE_BLOCK, SCM_PORT_DIRECTION_IN,
    SCM_PORT_FILE_OPTION_NONE,
};
use super::reader::Reader;

pub fn scm_vm_load(
    filename: &str,
    paths: Option<Value>,
    suffixes: Option<Value>,
    error_if_not_found: bool,
    environment: Value,
) -> Result<Value, Value> {
    let paths = paths.unwrap_or_else(|| loader().load_path.value);
    let suffixes = suffixes.unwrap_or_else(|| loader().load_suffixes_rec.value);

    let r = find_load_file(filename, paths, suffixes, error_if_not_found)?;

    if r.is_false() {
        return Ok(Value::encode_bool_value(false));
    }

    let path = r.car().strsym();
    let _remaining_paths = r.cdr();

    let vm = scm_vm();
    let port = Port::new(vm.mutator());

    port_open_file(
        port,
        make_string(vm.mutator(), path).into(),
        SCM_PORT_DIRECTION_IN,
        SCM_PORT_FILE_OPTION_NONE,
        SCM_PORT_BUFFER_MODE_BLOCK,
        Value::encode_bool_value(true),
    )?;

    scm_load_from_port(port, environment)
}

pub fn scm_load_from_port(port: Handle<Port>, environment: Value) -> Result<Value, Value> {
    if !port_input_pred(port) {
        return wrong_contract("load-from-port", "input-port?", 0, 1, &[port.into()]);
    }

    if !(environment.is_false() || environment.is_module()) {
        return wrong_contract(
            "load-from-port",
            "(and/c #f module?)",
            1,
            1,
            &[port.into(), environment],
        );
    }

    let prev_module = scm_current_module();

    port.lock.lock(true);
    let mut last = Value::encode_undefined_value();
    if environment.is_module() {
        scm_vm().module = Some(environment.module());
    }
    let res = (|| -> Result<Value, Value> {
        let mut reader = Reader::new(scm_vm(), port, false);

        let mut s;
        let note = HashMap::with_hasher_and_capacity(RandomState::new(), 16);
        let prev = scm_vm().current_notes.take();
        scm_vm().current_notes = Some(note);
        loop {
            s = reader.read(Some(note))?;
            if s.is_eof_object() {
                break;
            }

            last = scm_eval(s, Value::encode_bool_value(false), Some(note)).map_err(|err| {
                scm_vm().current_notes = prev;
                err 
            })?;
        }

        scm_vm().current_notes = prev;

        Ok(last)
    })();

    scm_vm().module = prev_module;

    port.lock.unlock();

    res
}

fn find_load_file(
    filename: &str,
    paths: Value,
    suffixes: Value,
    error_if_not_found: bool,
) -> Result<Value, Value> {
    fn file_ok(file: &str) -> bool {
        let path = std::path::Path::new(file);

        path.exists() && !path.is_dir()
    }

    fn try_suffixes(stem: &str, suffixes: Value) -> Option<String> {
        if file_ok(stem) {
            return Some(stem.to_owned());
        }
        scm_for_each!(entry, suffixes, {
            let suffix = entry.car();

            if unlikely(!suffix.is_string()) {
                return None;
            }

            let suffix = suffix.strsym();

            let mut full_path = String::new();

            full_path.push_str(stem);
            full_path.push_str(suffix);

            if file_ok(&full_path) {
                return Some(full_path);
            }
        });

        None
    }

    fn do_absolute(stem: &str, suffixes: Value, error_if_not_found: bool) -> Result<Value, Value> {
        if let Some(found) = try_suffixes(stem, suffixes) {
            return Ok(scm_list(
                Thread::current(),
                &[
                    make_string(Thread::current(), &found).into(),
                    Value::encode_null_value(),
                ],
            ));
        } else if error_if_not_found {
            raise_exn!(FailFilesystemExists, &[], "cannot find {} to load", stem)
        } else {
            Ok(Value::encode_bool_value(false))
        }
    }

    fn do_relative(
        filename: &str,
        error_if_not_found: bool,
        orig: Value,
        paths: Value,
        suffixes: Value,
    ) -> Result<Value, Value> {
        if paths.is_null() {
            if error_if_not_found {
                return raise_exn!(
                    FailFilesystemExists,
                    &[],
                    "cannot find {} in {}",
                    filename,
                    orig
                );
            } else {
                return Ok(Value::encode_bool_value(false));
            }
        }

        let path = std::path::Path::new(paths.car().strsym());

        if path.is_dir() {
            if let Some(found) = try_suffixes(
                &format!("{}/{}", path.to_str().unwrap(), filename),
                suffixes,
            ) {
                return Ok(scm_list(
                    Thread::current(),
                    &[make_string(Thread::current(), &found).into(), paths.cdr()],
                ));
            } else {
                do_relative(filename, error_if_not_found, orig, paths.cdr(), suffixes)
            }
        } else {
            do_relative(filename, error_if_not_found, orig, paths.cdr(), suffixes)
        }
    }

    if filename == "" {
        return raise_exn!(FailFilesystem, &[], "bad filename to load: {}", filename);
    }

    if filename.starts_with("~") {
        let normalized = match std::path::absolute(filename) {
            Ok(x) => x,
            Err(e) => {
                if let Some(errno) = e.raw_os_error() {
                    return raise_exn!(
                        FailFilesystemErrno,
                        &[Value::encode_int32(errno)],
                        "failed to get absolute path for {}: {}",
                        filename,
                        e
                    );
                } else {
                    return raise_exn!(
                        FailFilesystem,
                        &[],
                        "failed to get absolute path for {}: {}",
                        filename,
                        e
                    );
                }
            }
        };

        do_absolute(
            &normalized.display().to_string(),
            suffixes,
            error_if_not_found,
        )
    } else if filename.starts_with("/") {
        do_absolute(filename, suffixes, error_if_not_found)
    } else if filename.starts_with("./") {
        do_absolute(filename, suffixes, error_if_not_found)
    } else {
        do_relative(filename, error_if_not_found, paths, paths, suffixes)
    }
}

#[allow(dead_code)]
pub(crate) struct Loader {
    load_path: Handle<GLOC>,
    dynload_path: Handle<GLOC>,
    load_suffixes_rec: Handle<GLOC>,
    load_path_hooks_rec: Handle<GLOC>,
    path_mutex: RawMutex,

    provided: Value,
    providing: Value,
    waiting: Value,

    prov_mutex: RawMutex,
    prov_cv: Condvar,
}
unsafe impl Object for Loader {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.load_path.trace(visitor);
        self.dynload_path.trace(visitor);
        self.load_suffixes_rec.trace(visitor);
        self.load_path_hooks_rec.trace(visitor);
        self.provided.trace(visitor);
        self.providing.trace(visitor);
        self.waiting.trace(visitor);
    }
}

fn env_paths(var: &str) -> Value {
    let Ok(path) = std::env::var(var) else {
        return Value::encode_null_value();
    };

    let mut paths = Value::encode_null_value();

    for path in path.split(":") {
        paths = scm_cons(
            Thread::current(),
            make_string(Thread::current(), path).into(),
            paths,
        );
    }

    paths
}

static mut LOADER: MaybeUninit<Loader> = MaybeUninit::uninit();

const MANIFEST_PATH: &str = env!("CARGO_MANIFEST_DIR");

pub(crate) fn init_load() {
    let thr = Thread::current();
    let mut init_load_paths = Value::encode_null_value();
    let mut t = Value::encode_null_value();
    scm_append!(
        thr,
        &mut init_load_paths,
        &mut t,
        env_paths("CAPY_LOAD_PATH")
    );

    init_load_paths = scm_cons(
        thr,
        make_string(Thread::current(), &format!("{}/lib", MANIFEST_PATH)).into(),
        init_load_paths,
    );

    let mut init_dynload_paths = Value::encode_null_value();
    t = init_dynload_paths;
    scm_append!(
        thr,
        &mut init_dynload_paths,
        &mut t,
        env_paths("CAPY_DYNLOAD_PATH")
    );

    let init_load_suffixes = scm_list(
        thr,
        &[
            make_string(Thread::current(), ".scm").into(),
            make_string(Thread::current(), ".sld").into(),
        ],
    );

    unsafe {
        let module = scm_capy_module().module();

        let gloc = scm_define(module, "*load-paths*".intern(), init_load_paths)
            .unwrap()
            .gloc();

        LOADER.write(Loader {
            load_path: gloc,
            dynload_path: scm_define(module, "*dynload-paths*".intern(), init_dynload_paths)
                .unwrap()
                .gloc(),
            load_suffixes_rec: scm_define(module, "*load-suffixes*".intern(), init_load_suffixes)
                .unwrap()
                .gloc(),
            load_path_hooks_rec: scm_define(
                module,
                "*load-path-hooks*".intern(),
                Value::encode_null_value(),
            )
            .unwrap()
            .gloc(),
            path_mutex: RawMutex::INIT,
            provided: Value::encode_null_value(),
            providing: Value::encode_null_value(),
            waiting: Value::encode_null_value(),
            prov_mutex: RawMutex::INIT,
            prov_cv: Condvar::new(),
        });

        heap().add_root(SimpleRoot::new("loader", "ld", |proc| {
            loader().trace(proc.visitor());
        }));

        let subr = scm_make_subr("load", load_proc, 1, 5);
        scm_define(module, "load".intern(), subr).unwrap();

        let subr = scm_make_subr("provide", provide, 1, 1);
        scm_define(module, "provide".intern(), subr).unwrap();

        let module = scm_internal_module().module();

        let subr = scm_make_subr("%require", require_proc, 1, 1);
        scm_define(module, "%require".intern(), subr).unwrap();
    }
}

pub(crate) fn loader() -> &'static mut Loader {
    unsafe { &mut *LOADER.as_mut_ptr() }
}

extern "C" fn load_proc(cfr: &mut CallFrame) -> ScmResult {
    let filename = cfr.argument(0);

    if !filename.is_string() {
        return wrong_contract::<()>(
            "load",
            "string?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let filename = filename.strsym();

    let paths = if cfr.argument_count() > 1 {
        let ls = cfr.argument(1);

        if !scm_is_list(ls) {
            return wrong_contract::<()>(
                "load",
                "(listof string?)",
                1,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        scm_dolist!(e, ls, {
            if !e.is_string() {
                return wrong_contract::<()>(
                    "load",
                    "(listof string?)",
                    1,
                    cfr.argument_count() as i32,
                    cfr.arguments(),
                )
                .into();
            }
        });

        Some(cfr.argument(1))
    } else {
        None
    };

    let suffixes = if cfr.argument_count() > 2 {
        let ls = cfr.argument(2);

        if !scm_is_list(ls) {
            return wrong_contract::<()>(
                "load",
                "(listof string?)",
                2,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }

        scm_dolist!(e, ls, {
            if !e.is_string() {
                return wrong_contract::<()>(
                    "load",
                    "(listof string?)",
                    2,
                    cfr.argument_count() as i32,
                    cfr.arguments(),
                )
                .into();
            }
        });

        Some(cfr.argument(2))
    } else {
        None
    };

    let error_if_not_found = if cfr.argument_count() > 3 {
        if !cfr.argument(3).is_bool() {
            return wrong_contract::<()>(
                "load",
                "boolean?",
                3,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }
        cfr.argument(3)
    } else {
        Value::encode_bool_value(true)
    };

    let environment = if cfr.argument_count() > 4 {
        if !cfr.argument(4).is_module() {
            return wrong_contract::<()>(
                "load",
                "module?",
                4,
                cfr.argument_count() as i32,
                cfr.arguments(),
            )
            .into();
        }
        cfr.argument(4)
    } else {
        Value::encode_bool_value(false)
    };

    scm_vm_load(
        filename,
        paths,
        suffixes,
        error_if_not_found.to_bool(),
        environment,
    )
    .into()
}

extern "C" fn provide(cfr: &mut CallFrame) -> ScmResult {
    let feature = cfr.argument(0);

    if !feature.is_string() {
        return wrong_contract::<()>(
            "provide",
            "string?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let loader = loader();

    loader.prov_mutex.lock(true);

    if scm_member(loader.provided, feature, |x, y| x.strsym() == y.strsym()).is_false() {
        loader.provided = scm_cons(Thread::current(), feature, loader.provided);
    }

    loader.prov_cv.notify_all();
    loader.prov_mutex.unlock();

    Ok(feature).into()
}

pub fn scm_require(feature: Value, _flags: i32, _base_module: Handle<Module>) -> Result<(), Value> {
    if !feature.is_string() {
        return wrong_contract::<()>("require", "string?", 0, 1, &[feature]).into();
    }

    let loader = loader();

    loader.prov_mutex.lock(true);
    let mut provided;
    let vm = scm_vm();
    let mut circular = false;
    loop {
        provided = scm_member(loader.provided, feature, |x, y| x.strsym() == y.strsym());

        if !provided.is_false() {
            break;
        }

        let providing = scm_assoc_ref(
            loader.providing,
            feature,
            |x, y| x.strsym() == y.strsym(),
            None,
        );

        if providing.is_false() {
            break;
        }

        let mut p = providing;

        if p.cdr().get_int32() == vm.vmid {
            circular = true;
            break;
        }

        loop {
            let q = scm_assq(p.cdr(), loader.waiting);

            if q.is_false() {
                break;
            }

            p = scm_assoc_ref(
                loader.providing,
                q.cdr(),
                |x, y| x.strsym() == y.strsym(),
                None,
            );

            if p.cadr().get_int32() == vm.vmid {
                circular = true;
                break;
            }
        }

        if circular {
            break;
        }

        loader.waiting = scm_acons(
            Thread::current(),
            feature,
            Value::encode_int32(vm.vmid),
            loader.waiting,
        );
        loader.prov_cv.wait_raw(true, &loader.prov_mutex);
        loader.waiting =
            assoc_delete_x(Value::encode_int32(vm.vmid), loader.waiting, |x, y| x == y);
    }

    if !circular && provided.is_false() {
        loader.providing = scm_acons(
            Thread::current(),
            feature,
            Value::encode_int32(vm.vmid),
            loader.providing,
        );
    }

    loader.prov_mutex.unlock();

    let prev_mod = vm.module;

    if circular {
        return raise_exn!(
            Fail,
            &[],
            "require: circular import dependency detected: {}",
            feature.strsym()
        );
    }

    if !provided.is_false() {
        return Ok(());
    }

    vm.module = Some(scm_reqbase_module().module());
    let r = scm_vm_load(feature.strsym(), None, None, true, false.into());
    vm.module = prev_mod;

    match r {
        Ok(_) => {}
        Err(e) => {
            loader.prov_mutex.lock(true);
            loader.providing =
                assoc_delete_x(Value::encode_int32(vm.vmid), loader.providing, |x, y| {
                    x == y
                });
            loader.prov_cv.notify_all();
            loader.prov_mutex.unlock();

            return Err(e);
        }
    }

    loader.prov_mutex.lock(true);
    loader.providing = assoc_delete_x(Value::encode_int32(vm.vmid), loader.providing, |x, y| {
        x == y
    });
    loader.provided = scm_cons(Thread::current(), feature, loader.provided);

    loader.prov_cv.notify_all();
    loader.prov_mutex.unlock();

    Ok(())
}

extern "C" fn require_proc(cfr: &mut CallFrame) -> ScmResult {
    scm_require(cfr.argument(0), 0, scm_reqbase_module().module())
        .map(|_| Value::encode_bool_value(true))
        .into()
}
