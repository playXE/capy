use crate::{
    raise_exn,
    runtime::value::scm_uint,
    vm::{callframe::CallFrame, scm_vm},
};

use super::{
    error::wrong_contract,
    fun::scm_make_subr,
    list::scm_cons,
    module::{scm_capy_module, scm_define, scm_scheme_module},
    object::ScmResult,
    string::make_string,
    symbol::Intern,
    value::Value,
    vector::make_values,
};

extern "C" fn file_exists_p(cfr: &mut CallFrame) -> ScmResult {
    let path = cfr.argument(0);

    if !path.is_string() {
        return wrong_contract::<()>("file-exists?", "string?", 0, 1, cfr.arguments()).into();
    }

    let result = std::path::Path::new(path.strsym()).try_exists();
    match result {
        Ok(b) => ScmResult::ok(b),
        Err(e) => {
            let Some(errno) = e.raw_os_error() else {
                return raise_exn!(
                    (),
                    FailFilesystemExists,
                    &[],
                    "failed to check if file exists: {}",
                    e
                )
                .into();
            };

            return raise_exn!(
                (),
                FailFilesystemErrno,
                &[Value::encode_int32(errno)],
                "failed to check if file exists: {}",
                e
            )
            .into();
        }
    }
}

extern "C" fn delete_file(cfr: &mut CallFrame) -> ScmResult {
    let path = cfr.argument(0);

    if !path.is_string() {
        return wrong_contract::<()>("delete-file", "string?", 0, 1, cfr.arguments()).into();
    }

    let result = std::fs::remove_file(path.strsym());
    match result {
        Ok(_) => ScmResult::ok(Value::encode_undefined_value()),
        Err(e) => {
            let Some(errno) = e.raw_os_error() else {
                return raise_exn!((), FailFilesystem, &[], "failed to delete file: {}", e).into();
            };

            return raise_exn!(
                (),
                FailFilesystemErrno,
                &[Value::encode_int32(errno)],
                "failed to delete file: {}",
                e
            )
            .into();
        }
    }
}
scm_symbol!(SYM_DIRECTORY, "directory");
scm_symbol!(SCM_FILE, "file");

extern "C" fn metadata_values<const SYMLINK: bool>(cfr: &mut CallFrame) -> ScmResult {
    let path = cfr.argument(0);

    if !path.is_string() {
        return wrong_contract::<()>(
            if SYMLINK {
                "symlink-metadata*"
            } else {
                "metadata*"
            },
            "string?",
            0,
            1,
            cfr.arguments(),
        )
        .into();
    }

    let result = if !SYMLINK {
        std::fs::metadata(path.strsym())
    } else {
        std::fs::symlink_metadata(path.strsym())
    };

    match result {
        Ok(meta) => {
            let typ = if meta.is_dir() {
                *SYM_DIRECTORY
            } else {
                *SCM_FILE
            };

            let is_symlink = Value::encode_bool_value(meta.is_symlink());

            let len = scm_uint(meta.len());

            let readonly = Value::encode_bool_value(meta.permissions().readonly());

            let mtime = scm_uint(
                meta.modified()
                    .unwrap()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_millis() as _,
            );
            let atime = scm_uint(
                meta.accessed()
                    .unwrap()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_millis() as _,
            );
            let ctime = scm_uint(
                meta.created()
                    .unwrap()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_millis() as _,
            );

            ScmResult::ok(make_values(
                scm_vm().mutator(),
                &[typ, is_symlink, len, readonly, mtime, atime, ctime],
            ))
        }
        Err(e) => {
            let Some(errno) = e.raw_os_error() else {
                return raise_exn!((), FailFilesystem, &[], "failed to get metadata: {}", e).into();
            };

            return raise_exn!(
                (),
                FailFilesystemErrno,
                &[Value::encode_int32(errno)],
                "failed to get metadata: {}",
                e
            )
            .into();
        }
    }
}

extern "C" fn rename(cfr: &mut CallFrame) -> ScmResult {
    let from = cfr.argument(0);
    let to = cfr.argument(1);

    if !from.is_string() {
        return wrong_contract::<()>("rename-file", "string?", 0, 2, cfr.arguments()).into();
    }

    if !to.is_string() {
        return wrong_contract::<()>("rename-file", "string?", 1, 2, cfr.arguments()).into();
    }

    let result = std::fs::rename(from.strsym(), to.strsym());
    match result {
        Ok(_) => ScmResult::ok(Value::encode_undefined_value()),
        Err(e) => {
            let Some(errno) = e.raw_os_error() else {
                return raise_exn!((), FailFilesystem, &[], "failed to rename file: {}", e).into();
            };

            return raise_exn!(
                (),
                FailFilesystemErrno,
                &[Value::encode_int32(errno)],
                "failed to rename file: {}",
                e
            )
            .into();
        }
    }
}
extern "C" fn delete_directory(cfr: &mut CallFrame) -> ScmResult {
    let path = cfr.argument(0);

    if !path.is_string() {
        return wrong_contract::<()>("delete-directory", "string?", 0, 1, cfr.arguments()).into();
    }

    let result = std::fs::remove_dir(path.strsym());
    match result {
        Ok(_) => ScmResult::ok(Value::encode_undefined_value()),
        Err(e) => {
            let Some(errno) = e.raw_os_error() else {
                return raise_exn!((), FailFilesystem, &[], "failed to delete directory: {}", e)
                    .into();
            };

            return raise_exn!(
                (),
                FailFilesystemErrno,
                &[Value::encode_int32(errno)],
                "failed to delete directory: {}",
                e
            )
            .into();
        }
    }
}

extern "C" fn read_directory(cfr: &mut CallFrame) -> ScmResult {
    let path = cfr.argument(0);

    if !path.is_string() {
        return wrong_contract::<()>("read-directory", "string?", 0, 1, cfr.arguments()).into();
    }

    let result = std::fs::read_dir(path.strsym());

    match result {
        Ok(dir) => {
            let mut list = Value::encode_null_value();

            for entry in dir {
                match entry {
                    Ok(entry) => {
                        let name = entry.file_name().into_string().map_err(|err| {
                            raise_exn!(
                                (),
                                FailFilesystem,
                                &[],
                                "failed to read directory: {:?}",
                                err
                            )
                            .unwrap_err()
                        })?;

                        let name = make_string(scm_vm().mutator(), &name);

                        list = scm_cons(scm_vm().mutator(), name.into(), list);
                    }

                    Err(err) => {
                        let Some(errno) = err.raw_os_error() else {
                            return raise_exn!(
                                (),
                                FailFilesystem,
                                &[],
                                "failed to read directory: {}",
                                err
                            )
                            .into();
                        };

                        return raise_exn!(
                            (),
                            FailFilesystemErrno,
                            &[Value::encode_int32(errno)],
                            "failed to read directory: {}",
                            err
                        )
                        .into();
                    }
                }
            }

            ScmResult::ok(list)
        }
        Err(e) => {
            let Some(errno) = e.raw_os_error() else {
                return raise_exn!((), FailFilesystem, &[], "failed to read directory: {}", e)
                    .into();
            };

            return raise_exn!(
                (),
                FailFilesystemErrno,
                &[Value::encode_int32(errno)],
                "failed to read directory: {}",
                e
            )
            .into();
        }
    }
}

extern "C" fn dirname(cfr: &mut CallFrame) -> ScmResult {
    let path = cfr.argument(0);

    if !path.is_string() {
        return wrong_contract::<()>("dirname", "string?", 0, 1, cfr.arguments()).into();
    }

    let path = path.strsym();
    let dirname = std::path::Path::new(path).parent().ok_or_else(|| {
        raise_exn!((), FailFilesystem, &[], "failed to get dirname of {}", path).unwrap_err()
    })?;

    let dirname = dirname.to_str().ok_or_else(|| {
        raise_exn!((), FailFilesystem, &[], "failed to get dirname of {}", path).unwrap_err()
    })?;

    let dirname = make_string(scm_vm().mutator(), dirname);

    ScmResult::ok(dirname)
}

extern "C" fn basename(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 1 {
        let path = cfr.argument(0);

        if !path.is_string() {
            return wrong_contract::<()>("basename", "string?", 0, 1, cfr.arguments()).into();
        }

        let path = path.strsym();
        let basename = std::path::Path::new(path).file_name().ok_or_else(|| {
            raise_exn!(
                (),
                FailFilesystem,
                &[],
                "failed to get basename of {}",
                path
            )
            .unwrap_err()
        })?;

        let basename = basename.to_str().ok_or_else(|| {
            raise_exn!(
                (),
                FailFilesystem,
                &[],
                "failed to get basename of {}",
                path
            )
            .unwrap_err()
        })?;

        let basename = make_string(scm_vm().mutator(), basename);

        ScmResult::ok(basename)
    } else {
        let path = cfr.argument(0);
        let suffix = cfr.argument(1);

        if !path.is_string() {
            return wrong_contract::<()>("basename", "string?", 0, 2, cfr.arguments()).into();
        }

        if !suffix.is_string() {
            return wrong_contract::<()>("basename", "string?", 1, 2, cfr.arguments()).into();
        }

        let path = path.strsym();
        let suffix = suffix.strsym();

        let basename = std::path::Path::new(path).file_name().ok_or_else(|| {
            raise_exn!(
                (),
                FailFilesystem,
                &[],
                "failed to get basename of {}",
                path
            )
            .unwrap_err()
        })?;

        let basename = basename.to_str().ok_or_else(|| {
            raise_exn!(
                (),
                FailFilesystem,
                &[],
                "failed to get basename of {}",
                path
            )
            .unwrap_err()
        })?;

        if basename.ends_with(suffix) {
            let basename = &basename[..basename.len() - suffix.len()];
            let basename = make_string(scm_vm().mutator(), basename);
            ScmResult::ok(basename)
        } else {
            ScmResult::ok(Value::encode_bool_value(false))
        }
    }
}

extern "C" fn canonicalize_path(cfr: &mut CallFrame) -> ScmResult {
    let path = cfr.argument(0);

    if !path.is_string() {
        return wrong_contract::<()>("canonicalize-path", "string?", 0, 1, cfr.arguments()).into();
    }

    let path = path.strsym();
    let path = std::path::Path::new(path);

    let path = match path.canonicalize() {
        Ok(path) => path,
        Err(e) => {
            let Some(errno) = e.raw_os_error() else {
                return raise_exn!(
                    (),
                    FailFilesystem,
                    &[],
                    "failed to canonicalize path: {}",
                    e
                )
                .into();
            };

            return raise_exn!(
                (),
                FailFilesystemErrno,
                &[Value::encode_int32(errno)],
                "failed to canonicalize path: {}",
                e
            )
            .into();
        }
    };

    let path = path.to_str().ok_or_else(|| {
        raise_exn!(
            (),
            FailFilesystem,
            &[],
            "failed to canonicalize path: {}",
            path.display()
        )
        .unwrap_err()
    })?;

    let path = make_string(scm_vm().mutator(), path);

    ScmResult::ok(path)
}

pub(crate) fn init_file() {
    let module = scm_scheme_module().module();

    let subr = scm_make_subr("file-exists?", file_exists_p, 1, 1);
    scm_define(module, "file-exists?".intern(), subr).unwrap();

    let subr = scm_make_subr("delete-file", delete_file, 1, 1);
    scm_define(module, "delete-file".intern(), subr).unwrap();

    let module = scm_capy_module().module();

    let subr = scm_make_subr("metadata*", metadata_values::<false>, 1, 1);
    scm_define(module, "metadata*".intern(), subr).unwrap();

    let subr = scm_make_subr("symlink-metadata*", metadata_values::<false>, 1, 1);
    scm_define(module, "symlink-metadata*".intern(), subr).unwrap();

    let subr = scm_make_subr("rename-file", rename, 2, 2);
    scm_define(module, "rename-file".intern(), subr).unwrap();

    let subr = scm_make_subr("delete-directory", delete_directory, 1, 1);
    scm_define(module, "delete-directory".intern(), subr).unwrap();

    let subr = scm_make_subr("read-directory", read_directory, 1, 1);
    scm_define(module, "read-directory".intern(), subr).unwrap();

    let subr = scm_make_subr("dirname", dirname, 1, 1);
    scm_define(module, "dirname".intern(), subr).unwrap();

    let subr = scm_make_subr("basename", basename, 1, 2);
    scm_define(module, "basename".intern(), subr).unwrap();

    let subr = scm_make_subr("canonicalize-path", canonicalize_path, 1, 1);
    scm_define(module, "canonicalize-path".intern(), subr).unwrap();
}
