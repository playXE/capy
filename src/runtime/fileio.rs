use crate::vm::thread::{safepoint_scope, Thread};

use super::{
    control::wrong_type_argument_violation,
    gsubr::{scm_define_subr, Subr},
    object::{scm_bytevector_as_slice_mut, scm_bytevector_length, scm_string_str, scm_vector_set},
    value::Value,
};
use chrono::{DateTime, Datelike, Timelike, Utc};

pub const OPEN_READ: i32 = 0x01;
pub const OPEN_WRITE: i32 = 0x02;
pub const OPEN_APPEND: i32 = 0x04;
pub const OPEN_CREATE: i32 = 0x08;
pub const OPEN_TRUNCATE: i32 = 0x10;
pub const OPEN_BINARY: i32 = 0x20;
pub const CREATE_MODE: i32 = 0o666;

pub const ACCESS_EXISTS: i32 = 0x01;
pub const ACCESS_READ: i32 = 0x02;
pub const ACCESS_WRITE: i32 = 0x04;
pub const ACCESS_EXECUTE: i32 = 0x08;

extern "C-unwind" fn unix_open(
    _: &mut Thread,
    filename: &mut Value,
    flags: &mut Value,
    mode: &mut Value,
) -> Value {
    let flags = flags.get_int32();
    let mode = mode.get_int32();

    let mut fa = 0;
    if (flags & OPEN_READ) != 0 {
        fa |= libc::O_RDONLY;
    }

    if (flags & OPEN_WRITE) != 0 {
        fa |= libc::O_WRONLY;
    }

    if (flags & OPEN_APPEND) != 0 {
        fa |= libc::O_APPEND;
    }

    if (flags & OPEN_CREATE) != 0 {
        fa |= libc::O_CREAT;
    }

    if (flags & OPEN_TRUNCATE) != 0 {
        fa |= libc::O_TRUNC;
    }

    assert!(filename.is_string(), "filename must be a string");

    let filename = scm_string_str(*filename);
    let cstr = std::ffi::CString::new(filename).unwrap();
    let fd = unsafe { libc::open(cstr.as_ptr(), fa, mode) };

    Value::encode_int32(fd)
}

extern "C-unwind" fn unix_close(_: &mut Thread, fd: &mut Value) -> Value {
    let fd = fd.get_int32();
    let ret = unsafe { libc::close(fd) };
    Value::encode_int32(ret)
}

extern "C-unwind" fn unix_read(
    _: &mut Thread,
    fd: &mut Value,
    buf: &mut Value,
    count: &mut Value,
) -> Value {
    let fd = fd.get_int32();
    let count = count.get_int32();
    if count < 0 {
        panic!("count must be positive");
    }
    if !buf.is_bytevector() {
        panic!("buf must be a bytevector");
    }
    if count > scm_bytevector_length(*buf) as i32 {
        panic!("count must be less than or equal to the length of buf");
    }

    let buf = scm_bytevector_as_slice_mut(*buf);
    safepoint_scope(|| {
        let ret = unsafe { libc::read(fd, buf.as_mut_ptr() as *mut libc::c_void, count as usize) };

        Value::encode_int32(ret as _)
    })
}

extern "C-unwind" fn unix_write(
    _: &mut Thread,
    fd: &mut Value,
    buf: &mut Value,
    count: &mut Value,
    offset: &mut Value,
) -> Value {
    let fd = fd.get_int32();
    let count = count.get_int32();
    if count < 0 {
        panic!("count must be positive");
    }
    if !buf.is_bytevector() {
        panic!("buf must be a bytevector");
    }
    if count > scm_bytevector_length(*buf) as i32 {
        panic!("count must be less than or equal to the length of buf");
    }

    let offset = if offset.is_undefined() {
        0
    } else {
        offset.get_int32()
    };

    let buf = scm_bytevector_as_slice_mut(*buf);
    let ret = safepoint_scope(|| unsafe {
        libc::write(
            fd,
            buf.as_mut_ptr().offset(offset as isize) as *mut libc::c_void,
            count as usize,
        )
    });
    Value::encode_int32(ret as _)
}

extern "C-unwind" fn unix_lseek(
    _: &mut Thread,
    fd: &mut Value,
    offset: &mut Value,
    whence: &mut Value,
) -> Value {
    let fd = fd.get_int32();
    let offset = offset.get_int32();
    let whence = whence.get_int32();

    let whence = match whence {
        0 => libc::SEEK_SET,
        1 => libc::SEEK_CUR,
        2 => libc::SEEK_END,
        _ => panic!("invalid whence"),
    };

    let ret = unsafe { libc::lseek(fd, offset as libc::off_t, whence) };

    Value::encode_int32(ret as _)
}

extern "C-unwind" fn pollinput(_: &mut Thread, fd: &mut Value) -> Value {
    let fd = fd.get_int32();
    let mut fds = libc::pollfd {
        fd,
        events: libc::POLLIN,
        revents: 0,
    };
    let ret = unsafe { libc::poll(&mut fds, 1, 0) };
    Value::encode_int32(ret)
}

extern "C-unwind" fn unlink(thread: &mut Thread, filename: &mut Value) -> Value {
    if !filename.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "unlink",
            0,
            "string",
            *filename,
            1,
            &[filename],
        )
    }
    let filename = scm_string_str(*filename);
    let cstr = std::ffi::CString::new(filename).unwrap();
    let ret = unsafe { libc::unlink(cstr.as_ptr()) };
    Value::encode_int32(ret)
}

extern "C-unwind" fn rename(thread: &mut Thread, old: &mut Value, new: &mut Value) -> Value {
    if !old.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "rename",
            0,
            "string",
            *old,
            1,
            &[old, new],
        )
    }
    if !new.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "rename",
            1,
            "string",
            *new,
            1,
            &[old, new],
        )
    }
    let old = scm_string_str(*old);
    let new = scm_string_str(*new);
    let old = std::ffi::CString::new(old).unwrap();
    let new = std::ffi::CString::new(new).unwrap();
    let ret = unsafe { libc::rename(old.as_ptr(), new.as_ptr()) };
    Value::encode_int32(ret)
}

// `(mtime <filename> <vector len=6>)`
extern "C-unwind" fn mtime(thread: &mut Thread, filename: &mut Value, vec: &mut Value) -> Value {
    if !filename.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "mtime",
            0,
            "string",
            *filename,
            1,
            &[filename, vec],
        )
    }

    if !vec.is_vector() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "mtime",
            1,
            "vector",
            *vec,
            1,
            &[filename, vec],
        )
    }

    let stats = match std::fs::metadata(scm_string_str(*filename)) {
        Ok(stats) => stats,
        Err(err) => {
            if let Some(code) = err.raw_os_error() {
                return Value::encode_int32(code);
            } else {
                return Value::encode_int32(-1);
            }
        }
    };

    match stats.modified() {
        Ok(time) => {
            let time: DateTime<Utc> = time.into();

            scm_vector_set(*vec, thread, 0, Value::encode_int32(time.year() as _));
            scm_vector_set(*vec, thread, 1, Value::encode_int32(time.month() as _));
            scm_vector_set(*vec, thread, 2, Value::encode_int32(time.day() as _));
            scm_vector_set(*vec, thread, 3, Value::encode_int32(time.hour() as _));
            scm_vector_set(*vec, thread, 4, Value::encode_int32(time.minute() as _));
            scm_vector_set(*vec, thread, 5, Value::encode_int32(time.second() as _));

            Value::encode_int32(0)
        }
        Err(_) => Value::encode_int32(-1),
    }
}

extern "C-unwind" fn file_exists_p(thread: &mut Thread, filename: &mut Value) -> Value {
    if !filename.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "file-exists?",
            0,
            "string",
            *filename,
            1,
            &[filename],
        )
    }

    let filename = scm_string_str(*filename);
    let res = std::path::Path::new(filename).exists();

    Value::encode_bool_value(res)
}

extern "C-unwind" fn relative_path_string_p(thread: &mut Thread, path: &mut Value) -> Value {
    if !path.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "relative-path-string?",
            0,
            "string",
            *path,
            1,
            &[path],
        )
    }

    let path = scm_string_str(*path);
    let res = std::path::Path::new(path).is_relative();

    Value::encode_bool_value(res)
}

extern "C-unwind" fn absolute_path_string_p(thread: &mut Thread, path: &mut Value) -> Value {
    if !path.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "absolute-path-string?",
            0,
            "string",
            *path,
            1,
            &[path],
        )
    }

    let path = scm_string_str(*path);
    let res = std::path::Path::new(path).is_absolute();

    Value::encode_bool_value(res)
}

extern "C-unwind" fn dirname(thread: &mut Thread, path: &mut Value) -> Value {
    if !path.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "dirname",
            0,
            "string",
            *path,
            1,
            &[path],
        )
    }

    let path = scm_string_str(*path);
    let path = std::path::Path::new(path);
    let path = path.parent().unwrap_or(std::path::Path::new(""));

    let path = path.to_str().unwrap();

    thread.make_string::<false>(path)
}

extern "C-unwind" fn mkdir(thread: &mut Thread, path: &mut Value) -> Value {
    if !path.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "mkdir",
            0,
            "string",
            *path,
            1,
            &[path],
        )
    }

    let path = scm_string_str(*path);
    let path = std::path::Path::new(path);

    let res = std::fs::create_dir_all(path);

    match res {
        Ok(_) => Value::encode_int32(0),
        Err(err) => {
            if let Some(code) = err.raw_os_error() {
                Value::encode_int32(code)
            } else {
                Value::encode_int32(-1)
            }
        }
    }
}

extern "C-unwind" fn canonicalize_path(thread: &mut Thread, path: &mut Value) -> Value {
    if !path.is_string() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "canonicalize-path",
            0,
            "string",
            *path,
            1,
            &[path],
        )
    }

    let path = scm_string_str(*path);
    let path = std::path::Path::new(path);

    let res = std::fs::canonicalize(path);

    match res {
        Ok(path) => {
            let path = path.to_str().unwrap();
            thread.make_string::<false>(path)
        }
        Err(err) => {
            if let Some(code) = err.raw_os_error() {
                Value::encode_int32(code)
            } else {
                Value::encode_int32(-1)
            }
        }
    }
}

pub(crate) fn init() {
    scm_define_subr(
        "canonicalize-path",
        1,
        0,
        0,
        Subr::F1(canonicalize_path),
    );
    scm_define_subr("dirname", 1, 0, 0, Subr::F1(dirname));
    scm_define_subr("mkdir", 1, 0, 0, Subr::F1(mkdir));
    scm_define_subr("unix-open", 3, 0, 0, Subr::F3(unix_open));
    scm_define_subr("unix-close", 1, 0, 0, Subr::F1(unix_close));
    scm_define_subr("unix-read", 3, 0, 0, Subr::F3(unix_read));
    scm_define_subr("unix-write", 3, 1, 0, Subr::F4(unix_write));
    scm_define_subr("unix-lseek", 3, 0, 0, Subr::F3(unix_lseek));
    scm_define_subr("pollinput", 1, 0, 0, Subr::F1(pollinput));
    scm_define_subr("unlink", 1, 0, 0, Subr::F1(unlink));
    scm_define_subr("rename", 2, 0, 0, Subr::F2(rename));
    scm_define_subr("mtime", 2, 0, 0, Subr::F2(mtime));
    scm_define_subr("file-exists?", 1, 0, 0, Subr::F1(file_exists_p));
    scm_define_subr(
        "relative-path-string?",
        1,
        0,
        0,
        Subr::F1(relative_path_string_p),
    );
    scm_define_subr(
        "absolute-path-string?",
        1,
        0,
        0,
        Subr::F1(absolute_path_string_p),
    );
}
