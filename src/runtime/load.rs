use mmtk::{util::Address, vm::RootsWorkFactory};
use once_cell::sync::Lazy;

use crate::{
    gc::ObjEdge,
    gc_protect,
    runtime::object::scm_gloc_set,
    vm::{scm_virtual_machine, thread::Thread},
};

use super::{environment::environment_get_cell, symbol::scm_intern, value::Value};

/// The path for Scheme files.
pub static mut SCM_LOC_LOAD_PATH: Lazy<Value> = Lazy::new(|| {
    let env = scm_virtual_machine().interaction_environment;
    let load_path = environment_get_cell(env, scm_intern("%load-path")).unwrap();
    load_path
});

/// Extensions for source files.
pub static mut SCM_LOC_LOAD_EXTENSIONS: Lazy<Value> = Lazy::new(|| {
    let env = scm_virtual_machine().interaction_environment;
    let load_extensions = environment_get_cell(env, scm_intern("%load-extensions")).unwrap();
    load_extensions
});

/// The path for compiled files. Like `%load-path` but for compiled files.
pub static mut SCM_LOC_LOAD_COMPILED_PATH: Lazy<Value> = Lazy::new(|| {
    let env = scm_virtual_machine().interaction_environment;
    let load_compiled_path = environment_get_cell(env, scm_intern("%load-compiled-path")).unwrap();
    load_compiled_path
});

/// Extensions for compiled files. Like `%load-extensions` but for compiled files.
pub static mut SCM_LOC_LOAD_COMPILED_EXTENSIONS: Lazy<Value> = Lazy::new(|| {
    let env = scm_virtual_machine().interaction_environment;
    let load_compiled_extensions =
        environment_get_cell(env, scm_intern("%load-compiled-extensions")).unwrap();
    load_compiled_extensions
});

/// Whether we should try to auto-compile. If set to `#f`, auto-compilation is disabled
/// and we use interpreter for TreeIL.
pub static mut SCM_LOC_LOAD_SHOULD_AUTO_COMPILE: Lazy<Value> = Lazy::new(|| {
    let env = scm_virtual_machine().interaction_environment;
    let load_should_auto_compile =
        environment_get_cell(env, scm_intern("%load-should-auto-compile")).unwrap();
    load_should_auto_compile
});

/// Whether to treat all auto-compiled files as stale.
pub static mut SCM_LOC_LOAD_FRESH_AUTO_COMPILE: Lazy<Value> = Lazy::new(|| {
    let env = scm_virtual_machine().interaction_environment;
    let load_fresh_auto_compile =
        environment_get_cell(env, scm_intern("%fresh-auto-compile")).unwrap();
    load_fresh_auto_compile
});

/// The fallback path for auto-compilation.
pub static mut SCM_LOC_COMPILE_FALLBACK_PATH: Lazy<Value> = Lazy::new(|| {
    let env = scm_virtual_machine().interaction_environment;
    let compile_fallback_path =
        environment_get_cell(env, scm_intern("%compile-fallback-path")).unwrap();
    compile_fallback_path
});

pub(crate) fn visit_roots(factory: &mut impl RootsWorkFactory<ObjEdge>) {
    unsafe {
        let mut edges = vec![];
        if let Some(load_path) = Lazy::get_mut(&mut SCM_LOC_LOAD_PATH) {
            if load_path.is_object() {
                edges.push(ObjEdge::new(Address::from_ptr(load_path as *mut Value)));
            }
        }

        if let Some(load_ext) = Lazy::get_mut(&mut SCM_LOC_LOAD_EXTENSIONS) {
            if load_ext.is_object() {
                edges.push(ObjEdge::new(Address::from_ptr(load_ext as *mut Value)));
            }
        }

        if let Some(load_path) = Lazy::get_mut(&mut SCM_LOC_LOAD_COMPILED_PATH) {
            if load_path.is_object() {
                edges.push(ObjEdge::new(Address::from_ptr(load_path as *mut Value)));
            }
        }

        if let Some(load_ext) = Lazy::get_mut(&mut SCM_LOC_LOAD_COMPILED_EXTENSIONS) {
            if load_ext.is_object() {
                edges.push(ObjEdge::new(Address::from_ptr(load_ext as *mut Value)));
            }
        }

        if let Some(load_fresh_auto_compile) = Lazy::get_mut(&mut SCM_LOC_LOAD_FRESH_AUTO_COMPILE) {
            if load_fresh_auto_compile.is_object() {
                edges.push(ObjEdge::new(Address::from_ptr(
                    load_fresh_auto_compile as *mut Value,
                )));
            }
        }

        if let Some(load_should_auto_compile) = Lazy::get_mut(&mut SCM_LOC_LOAD_SHOULD_AUTO_COMPILE)
        {
            if load_should_auto_compile.is_object() {
                edges.push(ObjEdge::new(Address::from_ptr(
                    load_should_auto_compile as *mut Value,
                )));
            }
        }

        if let Some(compile_fallback_path) = Lazy::get_mut(&mut SCM_LOC_COMPILE_FALLBACK_PATH) {
            if compile_fallback_path.is_object() {
                edges.push(ObjEdge::new(Address::from_ptr(
                    compile_fallback_path as *mut Value,
                )));
            }
        }

        factory.create_process_edge_roots_work(edges);
    }
}

pub fn parse_path(path: &str) -> Vec<String> {
    let sep = if cfg!(windows) { ';' } else { ':' };

    path.split(sep).map(|s| s.to_string()).collect()
}

/// Parse `path`, which is expected to be a colon-separated
/// string, into a list and return the resulting list with
/// `base` (a list) spliced in place of the `...` path
/// component, if present, or else `base` is added to the end.
/// If `path` is `#f`, `base` is returned.
pub fn parse_path_with_ellipsis(path: &str, base: &[String]) -> Vec<String> {
    if path.is_empty() {
        return base.to_vec();
    }
    let lst = parse_path(path);

    let mut result = vec![];
    let mut i = 0;

    while i < lst.len() {
        if lst[i] == "..." {
            result.extend(base.iter().cloned());
            i += 1;
        } else {
            result.push(lst[i].clone());
            i += 1;
        }
    }

    result
}

fn path_to_value(thread: &mut Thread, path: &[String]) -> Value {
    let mut lst = Value::encode_null_value();

    for p in path.iter().rev() {
        let str = gc_protect!(thread => lst => thread.make_string::<false>(p));
        lst = thread.make_cons::<false>(str, lst);
    }

    lst
}

pub(crate) fn init_load() {
    let mut path = Vec::new();
    let mut cpath = Vec::new();
    let thread = Thread::current();

    unsafe {
        let ext = thread.make_string::<false>(".scm");
        let cons = thread.make_cons::<false>(ext, Value::encode_null_value());
        scm_gloc_set(*SCM_LOC_LOAD_EXTENSIONS, thread, cons);
        scm_gloc_set(
            *SCM_LOC_LOAD_COMPILED_PATH,
            thread,
            Value::encode_null_value(),
        );
        let ext = thread.make_string::<false>(".capy");
        let cons = thread.make_cons::<false>(ext, Value::encode_null_value());
        scm_gloc_set(*SCM_LOC_LOAD_COMPILED_EXTENSIONS, thread, cons);
        scm_gloc_set(
            *SCM_LOC_LOAD_SHOULD_AUTO_COMPILE,
            thread,
            Value::encode_bool_value(false),
        );
        scm_gloc_set(
            *SCM_LOC_LOAD_FRESH_AUTO_COMPILE,
            thread,
            Value::encode_bool_value(false),
        );
    }

    const FALLBACK_DIR: &'static str = concat!("capy/ccache/", env!("CARGO_PKG_VERSION"));
    {
        let cachedir = if let Ok(e) = std::env::var("XDG_CACHE_HOME") {
            Some(format!("{}/{}", e, FALLBACK_DIR))
        } else if let Ok(e) = std::env::var("HOME") {
            Some(format!("{}/.cache/{}", e, FALLBACK_DIR))
        } else if let Ok(e) = std::env::var("LOCALAPPDATA") {
            Some(format!("{}/{}", e, FALLBACK_DIR))
        } else if let Ok(e) = std::env::var("APPDATA") {
            Some(format!("{}/{}", e, FALLBACK_DIR))
        } else {
            None
        };
        if let Some(cachedir) = cachedir {
            let dir = thread.make_string::<false>(cachedir.as_str());
            unsafe {
                scm_gloc_set(*SCM_LOC_COMPILE_FALLBACK_PATH, thread, dir);
            }
        }
    }

    let env = std::env::var("CAPY_LOAD_PATH");

    if let Ok(var) = env {
        path = parse_path_with_ellipsis(&var, &path);
    }

    if let Ok(var) = std::env::var("CAPY_LOAD_COMPILED_PATH") {
        cpath = parse_path_with_ellipsis(&var, &cpath);
    }

    unsafe {
        let lpath = path_to_value(thread, &path);
        scm_gloc_set(*SCM_LOC_LOAD_PATH, thread, lpath);
        let cpath = path_to_value(thread, &cpath);
        scm_gloc_set(*SCM_LOC_LOAD_COMPILED_PATH, thread, cpath);
    }

    unsafe {
        let auto_compile = std::env::var("CAPY_AUTO_COMPILE").ok();

        match auto_compile.as_deref() {
            Some("0") => {
                scm_gloc_set(
                    *SCM_LOC_LOAD_SHOULD_AUTO_COMPILE,
                    thread,
                    Value::encode_bool_value(false),
                );
                scm_gloc_set(
                    *SCM_LOC_LOAD_FRESH_AUTO_COMPILE,
                    thread,
                    Value::encode_bool_value(false),
                );
            }

            Some("fresh") => {
                scm_gloc_set(
                    *SCM_LOC_LOAD_SHOULD_AUTO_COMPILE,
                    thread,
                    Value::encode_bool_value(true),
                );
                scm_gloc_set(
                    *SCM_LOC_LOAD_FRESH_AUTO_COMPILE,
                    thread,
                    Value::encode_bool_value(true),
                );
            }

            _ => {
                scm_gloc_set(
                    *SCM_LOC_LOAD_SHOULD_AUTO_COMPILE,
                    thread,
                    Value::encode_bool_value(true),
                );
                scm_gloc_set(
                    *SCM_LOC_LOAD_FRESH_AUTO_COMPILE,
                    thread,
                    Value::encode_bool_value(false),
                );
            }
        }
    }
}
