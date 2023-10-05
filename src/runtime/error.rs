use once_cell::sync::Lazy;

use crate::{
    gc_protect,
    interpreter::stackframe::{frame_dynamic_link, frame_virtual_return_address},
    vm::{scm_virtual_machine, thread::Thread, BOOT_CONTINUATION_CODE},
};

use super::{list::scm_cons, object::scm_vector_set, value::Value};

pub fn scm_error(_key: Value, subr: &str, message: &str, args: Value, _rest: Value) -> ! {
    // TODO: Actually invoke `throw` in boot library
    eprintln!("{}: {}: {}", subr, message, args);
    std::process::exit(1);
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum Exception {
    Exn,
    Fail,
    FailContract,
    FailContractArity,
    FailContractDivideByZero,
    FailContractNonFixnumResult,
    FailContractContinuation,
    FailContractVariable,
    FailRead,
    FailReadEof,
    FailReadNonChar,
    FailFilesystem,
    FailFilesystemExists,
    FailFilesystemVersion,
    FailFilesystemErrno,
    FailNetwork,
    FailNetworkErrno,
    FailOutOfMemory,
    FailUnsupported,
    FailUser,
    Break,
    BreakHangUp,
    BreakTerminate,
    Other,
}

#[allow(unused_variables)]
#[cold]
#[inline(never)]
#[doc(hidden)]
pub fn finish_exn_impl(
    id: Exception,
    c: usize,
    eargs: &[Value],
    msg: String,
    errno_val: Option<errno::Errno>,
    unsupported: bool,
) -> ! {
    let thread = Thread::current();

    let mut reargs = thread.make_vector::<false>(eargs.len() + 2, Value::encode_bool_value(false));

    for i in 0..eargs.len() {
        scm_vector_set(reargs, thread, i as _, eargs[i]);
    }

    let msg = gc_protect!(thread => reargs => thread.make_string::<false>(&msg));
    scm_vector_set(reargs, thread, 0, msg);
    let marks = unsafe { gc_protect!(thread => reargs => capture_stacktrace(thread)) };
    scm_vector_set(reargs, thread, 1, marks);
    println!("args: {}", reargs);
    todo!()
}

pub unsafe fn capture_stacktrace(thread: &mut Thread) -> Value {
    if scm_virtual_machine().initialized {
        let mut ip = thread.interpreter().ip;
        let mut ls = Value::encode_null_value();
        let mut fp = thread.interpreter().fp;
        let vm = scm_virtual_machine();
        while !fp.is_null() {
            if let Some((mut source, line, col)) = vm.images.debug_info(ip) {
                let mark = gc_protect!(thread => ls, source => thread.make_vector::<false>(3, Value::encode_bool_value(false)));
                scm_vector_set(mark, thread, 0, source);
                scm_vector_set(mark, thread, 1, Value::encode_int32(line as _));
                scm_vector_set(mark, thread, 2, Value::encode_int32(col as _));
                ls = scm_cons(mark, ls);
            } else if ip == BOOT_CONTINUATION_CODE.as_ptr() {
            } else {
                ls = scm_cons(Value::encode_bool_value(false), ls);
            }
            ip = frame_virtual_return_address(fp);
            fp = frame_dynamic_link(fp).cast();
            if fp == thread.interpreter().stack_top {
                break;
            }
        }

        ls
    } else {
        Value::encode_null_value()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ExnRec {
    pub args: usize,
    pub typ: Value,
    pub exptime: usize,
}

pub static EXN_TABLE: Lazy<[ExnRec; Exception::Other as usize]> = Lazy::new(|| {
    let table = [ExnRec {
        args: 0,
        typ: Value::encode_null_value(),
        exptime: 0,
    }; Exception::Other as usize];

    table
});

#[macro_export]
macro_rules! raise_exn {
    ($id:ident, $eargs:expr, $msg:literal $(,)? $($arg:expr),*) => {{
        //unsafe { std::hint::unreachable_unchecked(); }
        $crate::runtime::error::finish_exn_impl($crate::runtime::error::Exception::$id, std::hint::black_box($crate::runtime::error::EXN_TABLE[$crate::runtime::error::Exception::$id as usize].args), $eargs, std::hint::black_box(format!($msg, $($arg),*)), None, false)
    }};

    ($t: ty, $id:ident, $eargs:expr, $msg:literal $(,)? $($arg:expr),*) => {{
        if $crate::runtime::error::Exception::$id == $crate::runtime::error::Exception::FailRead {
            panic!($msg, $($arg),*);
        }
        std::hint::black_box($crate::runtime::error::finish_exn_impl::<$t>($crate::runtime::error::Exception::$id, $crate::runtime::error::EXN_TABLE[$crate::runtime::error::Exception::$id as usize].args, $eargs, format!($msg, $($arg),*), None, false))
    }};
}


