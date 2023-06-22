use std::intrinsics::unlikely;

use crate::compile::bytecompiler::ByteCompiler;
use crate::compile::{ref_count_lvars, r7rs_to_value};
use crate::list::scm_is_list;
use crate::object::Module;
use crate::scm_for_each;
use crate::string::make_string;
use crate::vm::interpreter::apply;
use crate::vm::scm_vm;
use crate::{module::scm_user_module, value::Value, vm::scm_current_module};
use once_cell::sync::Lazy;
use rsgc::heap::heap::heap;
use rsgc::heap::root_processor::SimpleRoot;
use rsgc::prelude::{Handle, Object};
use rsgc::sync::mutex::Mutex;
use rsgc::thread::Thread;

static LOAD_PATH: Lazy<Mutex<Value>> = Lazy::new(|| Mutex::new(Value::encode_null_value()));

pub(crate) fn init_load() {
    heap().add_root(SimpleRoot::new("load", "ld", |processor| unsafe {
        let val = LOAD_PATH.unsafe_get();

        val.trace(processor.visitor());
    }));
}

pub fn scm_vm_load(
    filename: &str,
    mut paths: Value,
    env: Option<Handle<Module>>,
) -> Result<Value, Value> {
    let module =
        env.unwrap_or_else(|| scm_current_module().unwrap_or_else(|| scm_user_module().module()));

    let t = Thread::current();

    if unlikely(!scm_is_list(paths) && !paths.is_false()) {
        return Err(make_string(
            t,
            &format!("load: expected list of paths, got: {:?}", paths),
        )
        .into());
    }

    if paths.is_false() {
        paths = *LOAD_PATH.lock(true);
    }

    let mut path_to_use = if std::path::Path::new(filename).exists() {
        Some(filename.to_string())
    } else {
        None
    };

    if path_to_use.is_none() {
        scm_for_each!(entry, paths, {
            let path = entry.car();

            if unlikely(!path.is_string()) {
                return Err(make_string(
                    t,
                    &format!("load: expected string path, got: {:?}", path),
                )
                .into());
            }

            let path = path.strsym();

            let mut full_path = String::new();

            full_path.push_str(path);
            full_path.push_str(filename);

            if std::path::Path::new(&full_path).exists() {

                full_path.clear();
                full_path.push_str(path);
                full_path.push_str("/");
                full_path.push_str(filename);

                if std::path::Path::new(&full_path).exists() {
                    continue;
                }
            }

            path_to_use = Some(full_path);
            break;
        });

        if path_to_use.is_none() {
            return Err(make_string(
                t,
                &format!("load: file not found: {}", filename),
            )
            .into());
        }
    }

    let path_to_use = path_to_use.unwrap();

    let source = std::fs::read_to_string(&path_to_use).map_err(|err| {
        make_string(
            t,
            &format!("load: error reading file {}: {}", path_to_use, err),
        ).into()
    })?;

    let mut i = r7rs_parser::expr::NoIntern;
    let mut p = r7rs_parser::parser::Parser::new(&mut i, &source, false);

   

    let cenv = crate::compile::make_cenv(module, Value::encode_null_value());

    let proc = ByteCompiler::compile_while(t, |t| {
        if p.finished() {
            return Ok(None)
        }

        match p.parse(true) {
            Ok(val) => {
                let val = r7rs_to_value(t, &val);
                let val = crate::compile::pass1::pass1(val, cenv)?;
                ref_count_lvars(val);
                Ok(Some(val))
            }

            Err(err) => {
                let err = format!("load: error parsing file {}: {}", path_to_use, err);

                Err(make_string(t, &err).into())
            }
        }
    })?; 
    let saved = scm_current_module();
    scm_vm().module = Some(module);
    let result = apply(proc, &[]);
    scm_vm().module = saved;

    result
}
