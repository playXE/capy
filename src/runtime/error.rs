#![allow(dead_code, unused_variables)]
use std::{any::Any, cmp::Ordering};

use once_cell::sync::Lazy;
use rsgc::{
    heap::{heap::heap, root_processor::SimpleRoot},
    prelude::{Handle, Object},
    system::arraylist::ArrayList,
    thread::Thread,
};

use crate::{
    runtime::{
        fun::scm_make_subr,
        module::scm_define,
        object::MAX_ARITY,
        structure::{force_struct_type_info, make_struct_names_from_array, make_struct_values},
        symbol::Intern,
    },
    vm::{callframe::CallFrame, scm_vm, stacktrace::get_stacktrace_str},
};

use super::{
    arith::scm_n_compare,
    fun::get_proc_name,
    module::scm_capy_module,
    object::{ScmResult, Type},
    port::{port_extract_string, port_open_bytevector, Port, SCM_PORT_DIRECTION_OUT},
    print::Printer,
    string::{do_format, make_string},
    structure::{
        make_simple_struct_instance_from_array, make_struct_instance_, STRUCT_EXPTIME,
        STRUCT_NO_MAKE_PREFIX, STRUCT_NO_SET,
    },
    value::Value,
};

#[macro_export]
macro_rules! raise_exn {
    ($id:ident, $eargs:expr, $msg:literal $(,)? $($arg:expr),*) => {
        $crate::runtime::error::finish_exn_impl($crate::runtime::error::Exception::$id, $crate::runtime::error::EXN_TABLE[$crate::runtime::error::Exception::$id as usize].args, $eargs, format!($msg, $($arg),*), None, false)
    };

    ($t: ty, $id:ident, $eargs:expr, $msg:literal $(,)? $($arg:expr),*) => {
        $crate::runtime::error::finish_exn_impl::<$t>($crate::runtime::error::Exception::$id, $crate::runtime::error::EXN_TABLE[$crate::runtime::error::Exception::$id as usize].args, $eargs, format!($msg, $($arg),*), None, false)
    };
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(usize)]
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

/// A record of an exception type.
#[derive(Clone, Copy)]
pub struct ExnRec {
    pub args: usize,
    pub typ: Value,
    pub names: Option<Handle<ArrayList<Value>>>,
    pub exptime: Value,
    pub super_pos: usize,
}

fn make_arg_lines_string(indent: &str, which: i32, argc: i32, args: &[Value]) -> String {
    let mut other;
    if argc == 0 || (argc == 1 && which == 0) {
        other = " [none]".to_owned();

        return other;
    }

    other = String::with_capacity(32);

    for i in 0..argc {
        if i != which {
            other.push('\n');
            other.push_str(indent);
            other.push_str(args[i as usize].to_string().as_str());
        }
    }

    other
}

fn wrong_contract_impl(
    name: &str,
    expected: &str,
    // negative means result
    which: i32,
    // negative means result
    mut argc: i32,
    args: &[Value],
) -> Result<(), Value> {
    let mut isgiven = "given";
    let mut kind = "argument";

    let o = if which < 0 {
        args[0]
    } else {
        args[which as usize]
    };
    let mut isres = false;

    if argc < 0 {
        argc = -argc;
        isgiven = "received";
        kind = "result";
        isres = true;
    }

    if which == -2 {
        isgiven = "received";
        kind = "result";
    }

    if argc == 0 {
        kind = "value";
    }

    let s = o.to_string();

    if which < 0 || argc <= 1 {
        raise_exn!(
            FailContract,
            &[],
            "{}: contract violation\n  expected: {}\n  {}: {}",
            name,
            expected,
            isgiven,
            s
        )
    } else {
        let other = make_arg_lines_string("   ", which, argc, args);
        raise_exn!(
            FailContract,
            &[],
            "{}: contract violation\n  expected: {}\n  {}: {}\n  {} position: {}\n  other {}...: {}",
            name,
            expected,
            isgiven,
            s,
            kind,
            which + 1,
            isres.then_some("results").unwrap_or("arguments"),
            other
        )
    }
}

pub const EXN_FIELDS: [&'static str; 2] = ["message", "continuation-marks"];
pub const EXN_FAIL_CONTRACT_VARIABLE_FIELDS: [&'static str; 1] = ["id"];
pub const EXN_FAIL_READ_FIELDS: [&'static str; 1] = ["srclocs"];
pub const EXN_FAIL_FILESYSTEM_ERRNO_FIELDS: [&'static str; 1] = ["errno"];
pub const EXN_FAIL_NETWORK_ERRNO_FIELDS: [&'static str; 1] = ["errno"];
pub const EXN_BREAK_FIELDS: [&'static str; 1] = ["continuation"];

pub const EXN_FLAGS: i32 = STRUCT_EXPTIME | STRUCT_NO_SET | STRUCT_NO_MAKE_PREFIX;

pub static EXN_TABLE: Lazy<[ExnRec; Exception::Other as usize]> = Lazy::new(|| {
    let mut exn_table = [ExnRec {
        args: 0,
        typ: Value::encode_null_value(),
        names: None,
        exptime: Value::encode_null_value(),
        super_pos: 0,
    }; Exception::Other as usize];

    exn_table[Exception::Fail as usize].args = 2;
    exn_table[Exception::FailContract as usize].args = 2;
    exn_table[Exception::FailContractArity as usize].args = 2;
    exn_table[Exception::FailContractDivideByZero as usize].args = 2;
    exn_table[Exception::FailContractNonFixnumResult as usize].args = 2;
    exn_table[Exception::FailContractContinuation as usize].args = 2;
    exn_table[Exception::FailContractVariable as usize].args = 3;
    exn_table[Exception::FailRead as usize].args = 3;
    exn_table[Exception::FailReadEof as usize].args = 3;
    exn_table[Exception::FailReadNonChar as usize].args = 3;
    exn_table[Exception::FailFilesystem as usize].args = 2;
    exn_table[Exception::FailFilesystemExists as usize].args = 2;
    exn_table[Exception::FailFilesystemVersion as usize].args = 2;
    exn_table[Exception::FailFilesystemErrno as usize].args = 3;
    exn_table[Exception::FailNetwork as usize].args = 2;
    exn_table[Exception::FailNetworkErrno as usize].args = 3;
    exn_table[Exception::FailOutOfMemory as usize].args = 2;
    exn_table[Exception::FailUnsupported as usize].args = 2;
    exn_table[Exception::FailUser as usize].args = 2;
    macro_rules! setup_struct {
        ($id: expr, $parent: expr, $name: expr, $args: expr, $props: expr, $guard: expr) => {{
            let args: &[&str] = $args;
            let tmpo = $crate::runtime::structure::make_struct_type_from_string(
                $name,
                $parent,
                args.len(),
                $props,
                $guard,
            )
            .unwrap();
            exn_table[$id as usize].typ = tmpo;

            let tmpop = $crate::runtime::structure::make_struct_names_from_array(
                $name,
                args.len() as _,
                $args,
                EXN_FLAGS,
            );
            exn_table[$id as usize].names = Some(Thread::current().allocate(tmpop));
        }};
    }

    macro_rules! exn_parent {
        ($id: expr) => {
            exn_table[$id as usize].typ
        };
    }

    setup_struct!(
        Exception::Exn,
        Value::encode_bool_value(false),
        "exn",
        &["message", "stack-trace"],
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    setup_struct!(
        Exception::Fail,
        exn_parent!(Exception::Exn),
        "exn:fail",
        &[],
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    setup_struct!(
        Exception::FailContract,
        exn_parent!(Exception::Fail),
        "exn:fail:contract",
        &[],
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    setup_struct!(
        Exception::FailContractArity,
        exn_parent!(Exception::FailContract),
        "exn:fail:contract:arity",
        &[],
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    setup_struct!(
        Exception::FailContractDivideByZero,
        exn_parent!(Exception::FailContract),
        "exn:fail:contract:divide-by-zero",
        &[],
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    setup_struct!(
        Exception::FailContractNonFixnumResult,
        exn_parent!(Exception::FailContract),
        "exn:fail:contract:non-fixnum-result",
        &[],
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    setup_struct!(
        Exception::FailContractContinuation,
        exn_parent!(Exception::FailContract),
        "exn:fail:contract:continuation",
        &[],
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    setup_struct!(
        Exception::FailContractVariable,
        exn_parent!(Exception::FailContract),
        "exn:fail:contract:variable",
        &EXN_FAIL_CONTRACT_VARIABLE_FIELDS,
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    setup_struct!(
        Exception::FailRead,
        exn_parent!(Exception::Fail),
        "exn:fail:read",
        &[],
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    setup_struct!(
        Exception::FailReadEof,
        exn_parent!(Exception::FailRead),
        "exn:fail:read:eof",
        &[],
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    setup_struct!(
        Exception::FailReadNonChar,
        exn_parent!(Exception::FailRead),
        "exn:fail:read:non-char",
        &[],
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    setup_struct!(
        Exception::FailFilesystem,
        exn_parent!(Exception::Fail),
        "exn:fail:filesystem",
        &[],
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    setup_struct!(
        Exception::FailFilesystemExists,
        exn_parent!(Exception::FailFilesystem),
        "exn:fail:filesystem:exists",
        &[],
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    setup_struct!(
        Exception::FailFilesystemVersion,
        exn_parent!(Exception::FailFilesystem),
        "exn:fail:filesystem:version",
        &[],
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    setup_struct!(
        Exception::FailFilesystemErrno,
        exn_parent!(Exception::FailFilesystem),
        "exn:fail:filesystem:errno",
        &["errno"],
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    setup_struct!(
        Exception::FailNetwork,
        exn_parent!(Exception::Fail),
        "exn:fail:network",
        &[],
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    setup_struct!(
        Exception::FailNetworkErrno,
        exn_parent!(Exception::FailNetwork),
        "exn:fail:network:errno",
        &["errno"],
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    setup_struct!(
        Exception::FailOutOfMemory,
        exn_parent!(Exception::Fail),
        "exn:fail:out-of-memory",
        &[],
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    setup_struct!(
        Exception::FailUnsupported,
        exn_parent!(Exception::Fail),
        "exn:fail:unsupported",
        &[],
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    setup_struct!(
        Exception::FailUser,
        exn_parent!(Exception::Fail),
        "exn:fail:user",
        &[],
        Value::encode_null_value(),
        Value::encode_null_value()
    );

    exn_table
});

#[doc(hidden)]
pub fn finish_exn_impl<T>(
    mut id: Exception,
    mut c: usize,
    eargs: &[Value],
    msg: String,
    errno_val: Option<errno::Errno>,
    unsupported: bool,
) -> Result<T, Value> {
    let mut reargs = ArrayList::from_init_with_capacity(
        Thread::current(),
        eargs.len() + 2,
        Value::encode_null_value(),
        eargs.len() + 2,
    );

    for i in 2..eargs.len() {
        reargs.set(Thread::current(), i, eargs[i]);
    }

    reargs[0] = make_string(Thread::current(), msg).into();
    reargs[1] = make_string(Thread::current(), &get_stacktrace_str(scm_vm())).into(); // cmark

    if let Some(errno) = errno_val {
        if id == Exception::FailFilesystem {
            id = Exception::FailFilesystemErrno;
            reargs[2] = Value::encode_int32(errno.0);
            c += 1;
        } else if id == Exception::FailNetwork {
            id = Exception::FailNetworkErrno;
            reargs[2] = Value::encode_int32(errno.0);
            c += 1;
        }
    } else if unsupported {
        if id == Exception::Fail {
            id = Exception::FailUnsupported;
        }
    }

    let instance = make_struct_instance_(EXN_TABLE[id as usize].typ, &reargs[0..c])?;

    Err(instance)
}

pub fn wrong_contract<T>(
    name: &str,
    expected: &str,
    which: i32,
    argc: i32,
    args: &[Value],
) -> Result<T, Value> {
    match wrong_contract_impl(name, expected, which, argc, args) {
        Ok(_) => unreachable!(),
        Err(e) => Err(e),
    }
}

pub fn make_args_string(s: &str, which: i32, mut argc: i32, args: &[Value]) -> String {
    let mut isres = "arguments";

    if argc < 0 {
        argc = -argc;
        isres = "results";
    }

    let mut other = String::with_capacity(32);

    other.push_str(&format!("{}{} were:", s, isres));
    if argc < 50 {
        for i in 0..argc {
            if i != which {
                other.push_str(&format!(" {}", args[i as usize]));
            }
        }
    } else {
        other.push_str(&format!("; given {} arguments total", argc));
    }

    other
}

fn wrong_type_impl(
    name: &str,
    expected: &str,
    which: i32,
    mut argc: i32,
    args: &[Value],
) -> Result<(), Value> {
    let mut isress = "argument";
    let mut isgiven = "given";

    let o = if which < 0 {
        args[0]
    } else {
        args[which as usize]
    };

    let isres = if argc < 0 {
        argc = -argc;
        isress = "result";
        isgiven = "received";
        true
    } else {
        false
    };

    if which == -2 {
        isress = "value";
        isgiven = "received";
    }

    let s = o.to_string();

    if which < 0 || argc == 1 {
        raise_exn!(
            FailContract,
            &[],
            "{}: expect{} {} of type <{}>; {}: {}",
            name,
            (which < 2).then_some("ed").unwrap_or("s"),
            isress,
            expected,
            isgiven,
            s
        )
    } else {
        let other = if which >= 0 && argc > 1 {
            make_args_string("other ", which, if isres { -argc } else { argc }, args)
        } else {
            "".to_owned()
        };

        raise_exn!(
            FailContract,
            &[],
            "{}: expects type <{}> as {} {}, given: {}{}",
            name,
            expected,
            which + 1,
            isress,
            s,
            other
        )
    }
}

pub fn wrong_type<T>(
    name: &str,
    expected: &str,
    which: i32,
    argc: i32,
    args: &[Value],
) -> Result<T, Value> {
    match wrong_type_impl(name, expected, which, argc, args) {
        Ok(_) => unreachable!(),
        Err(e) => Err(e),
    }
}

pub fn wrong_field_type<T>(c_name: Value, expected: &str, o: Value) -> Result<T, Value> {
    let a = [o];
    let s = c_name.to_string();

    wrong_type(&s, expected, -1, 0, &a)
}

pub fn wrong_field_contract(c_name: Value, expected: &str, o: Value) -> Result<(), Value> {
    let a = [o];
    let s = c_name.to_string();

    wrong_contract(&s, expected, -1, 0, &a)
}

pub fn arg_mismatch<T>(name: &str, msg: &str, o: Option<Value>) -> Result<T, Value> {
    let s = o.map(|o| o.to_string()).unwrap_or_else(|| "".to_owned());

    raise_exn!(FailContract, &[], "{}: {}{}", name, msg, s)
}

fn do_out_of_range(
    name: &str,
    typ: Option<&str>,
    which: &str,
    ending: bool,
    i: Value,
    s: Value,
    low_bound: Value,
    sstart: Value,
    slen: Value,
) -> Result<(), Value> {
    let typ = typ.unwrap_or("string");
    let vm = scm_vm();
    let cmp = scm_n_compare(slen, sstart);
    if cmp.is_some() && cmp != Some(Ordering::Less) {
        let mut small_end = false;
        if ending {
            let cmp1 = scm_n_compare(i, low_bound);
            let cmp2 = scm_n_compare(i, sstart);
            if
            /*bin_gte(vm, i, low_bound)?*/
            cmp1 != Some(Ordering::Less) && /*bin_lt(vm, i, sstart)?*/ cmp2 == Some(Ordering::Less)
            {
                small_end = true;
            }
        }

        let sstr = s.to_string();

        raise_exn!(
            FailContract,
            &[],
            "{}: {}index is {}\n  {}index: {}\n  {}{}{}{}]\n  {}: {}",
            name,
            which,
            if small_end {
                "smaller than starting index"
            } else {
                "out of range"
            },
            which,
            i.to_string(),
            if ending {
                "starting index: "
            } else {
                "valid range: ["
            },
            sstart.to_string(),
            if ending { "\n valid range: [0, " } else { ", " },
            slen,
            typ,
            sstr
        )
    } else {
        raise_exn!(
            FailContract,
            &[],
            "{}: {}index is out of range for empty {}\n  {}index: {}",
            name,
            which,
            typ,
            which,
            i.to_string()
        )
    }
}

pub fn out_of_range<T>(
    name: &str,
    typ: Option<&str>,
    which: &str,
    i: Value,
    s: Value,
    mut start: i32,
    mut len: i32,
) -> Result<T, Value> {
    if start < 0 {
        start = 0;
        len = len - 1;
    }

    do_out_of_range(
        name,
        typ,
        which,
        which.contains("ending "),
        i,
        s,
        Value::encode_int32(0),
        Value::encode_int32(start),
        Value::encode_int32(len),
    )
    .map(|_| unreachable!())
}

fn do_raise_range_error(who: &str, args: &[Value]) -> ScmResult {
    if !args[0].is_symbol() {
        return ScmResult::err(
            wrong_contract::<()>(who, "symbol?", 0, args.len() as _, args).unwrap_err(),
        );
    }

    if !args[1].is_string() {
        return ScmResult::err(
            wrong_contract::<()>(who, "string?", 1, args.len() as _, args).unwrap_err(),
        );
    }

    if !args[2].is_string() {
        return ScmResult::err(
            wrong_contract::<()>(who, "string?", 2, args.len() as _, args).unwrap_err(),
        );
    }

    if !args[3].is_int32() || args[3].get_type() != Type::BigNum {
        return ScmResult::err(
            wrong_contract::<()>(who, "exact-integer?", 3, args.len() as _, args).unwrap_err(),
        );
    }

    if !args[5].is_int32() || args[5].get_type() != Type::BigNum {
        return ScmResult::err(
            wrong_contract::<()>(who, "exact-integer?", 5, args.len() as _, args).unwrap_err(),
        );
    }

    if !args[6].is_int32() || args[6].get_type() != Type::BigNum {
        return ScmResult::err(
            wrong_contract::<()>(who, "exact-integer?", 6, args.len() as _, args).unwrap_err(),
        );
    }

    if args.len() > 7 {
        if !args[7].is_false() && !args[7].is_int32() && args[7].get_type() != Type::BigNum {
            return ScmResult::err(
                wrong_contract::<()>(who, "(or/c exact-integer? #f)", 7, args.len() as _, args)
                    .unwrap_err(),
            );
        }
    }

    let typ = args[1].strsym();
    let desc = args[2].strsym();

    match do_out_of_range(
        args[0].strsym(),
        Some(typ),
        desc,
        args.len() > 7 && args[7].is_true(),
        args[3],
        args[4],
        args[7],
        args[5],
        args[6],
    ) {
        Ok(_) => unreachable!(),
        Err(e) => ScmResult::err(e),
    }
}

extern "C" fn raise_range_error(cfr: &mut CallFrame) -> ScmResult {
    do_raise_range_error("raise-range-error", cfr.arguments())
}

const MAX_MISMATCH_EXTRAS: usize = 5;

pub fn contract_error<'a, T>(name: &str, msg: &str, args: &'a [&'a dyn Any]) -> Result<T, Value> {
    let mut strs: [Option<&&str>; MAX_MISMATCH_EXTRAS] = [None; MAX_MISMATCH_EXTRAS];
    let mut vs: [Option<Value>; MAX_MISMATCH_EXTRAS] = [None; MAX_MISMATCH_EXTRAS];
    let mut v_strs: [Option<String>; MAX_MISMATCH_EXTRAS] = [None, None, None, None, None];

    let mut cnt = 0;

    while cnt < MAX_MISMATCH_EXTRAS {
        if cnt >= args.len() {
            break;
        }
        let str = args[cnt].downcast_ref::<&str>().unwrap();
        strs[cnt] = Some(str);

        if args[cnt + 1].is::<Value>() {
            vs[cnt + 1] = Some(*args[cnt + 1].downcast_ref::<Value>().unwrap());
        } else if args[cnt + 1].is::<String>() {
            v_strs[cnt + 1] = Some(args[cnt + 1].downcast_ref::<String>().unwrap().to_string());
            vs[cnt + 1] = None;
        } else {
            v_strs[cnt + 1] = Some(args[cnt + 1].downcast_ref::<&str>().unwrap().to_string());
            vs[cnt + 1] = None;
        }
        cnt += 1;
    }

    for i in 0..cnt {
        if let Some(v) = vs[i] {
            let v_str = v.to_string();
            v_strs[i] = Some(v_str);
        }
    }

    let sep = ": ";

    let mut out = String::new();

    out.push_str(&name);
    out.push_str(&sep);
    out.push_str(&msg);

    for i in 0..cnt {
        out.push('\n');
        out.push_str(&strs[i].unwrap_or(&""));
        out.push_str(": ");
        out.push_str(&v_strs[i].clone().unwrap_or("".to_string()));
    }

    raise_exn!(FailContract, &[], "{}", out)
}

fn do_error(who: &str, mode: Exception, args: &[Value]) -> Result<(), Value> {
    let mut newargs: [Value; 2] = [Value::encode_null_value(); 2];

    if args[0].is_symbol() {
        if args.len() < 2 {
            let s = args[0].strsym();
            let __l = s.len();

            newargs[0] = make_string(Thread::current(), &format!("error: {}", s)).into()
        } else {
            let port = Port::new(Thread::current());
            port_open_bytevector(
                port,
                "do_error".intern().into(),
                SCM_PORT_DIRECTION_OUT,
                Value::encode_bool_value(false),
                Value::encode_bool_value(false),
            );
            if !args[1].is_string() {
                return wrong_contract(who, "string?", 1, args.len() as _, args);
            }

            do_format(who, port, None, 1, 2, args.len(), args)?;

            let s = port_extract_string(port).unwrap();
            newargs[0] = make_string(
                Thread::current(),
                &format!("{}: {}", args[0].strsym(), s.strsym()),
            )
            .into();
        }
    } else {
        if !args[0].is_string() {
            return wrong_contract(who, "(or/c string? symbol?)", 0, args.len() as _, args);
        }

        let port = Port::new(Thread::current());
        port_open_bytevector(
            port,
            "do_error".intern().into(),
            SCM_PORT_DIRECTION_OUT,
            Value::encode_bool_value(false),
            Value::encode_bool_value(false),
        );
        let mut printer = Printer::new(crate::vm::scm_vm(), port);
        printer.write(args[0])?;

        for i in 1..args.len() {
            printer.puts(" ")?;
            printer.write(args[i])?;
        }

        newargs[0] = port_extract_string(port).unwrap();
    }

    newargs[1] = Value::encode_undefined_value();
    let instance = make_struct_instance_(EXN_TABLE[mode as usize].typ, &newargs)?;

    Err(instance)
}

fn do_raise_type_error(name: &str, args: &[Value], mode: Exception) -> Result<(), Value> {
    if !args[0].is_symbol() {
        return wrong_contract(name, "symbol?", 0, args.len() as _, args);
    }

    if !args[1].is_string() {
        return wrong_contract(name, "string?", 1, args.len() as _, args);
    }

    let mut negate = false;

    let wrong: Option<fn(&str, &str, i32, i32, &[Value]) -> Result<(), Value>> = match mode {
        Exception::Exn => Some(wrong_type),
        Exception::Fail => Some(wrong_contract),
        Exception::FailContract => {
            negate = true;
            Some(wrong_contract)
        }
        _ => None,
    };

    if args.len() == 3 {
        let v = args[2];
        let s = args[1].to_string();

        return (wrong.unwrap())(args[0].strsym(), &s, if negate { -2 } else { -1 }, 0, &[v]);
    } else {
        if !(args[2].is_int32() && args[2].get_int32() >= 0) {
            return wrong_contract(name, "exact-nonnegative-integer?", 2, args.len() as _, args);
        }

        if args[2].is_int32() && args[2].get_int32() >= args.len() as i32 - 3 {
            return contract_error(
                name,
                if negate {
                    &"position index >= provided result count"
                } else {
                    &"position index >= provided argument count"
                },
                &[
                    &"position index",
                    &args[2],
                    if negate {
                        &"provided result count"
                    } else {
                        &"provided argument count"
                    },
                    &Value::encode_int32(args.len() as i32 - 3),
                ],
            );
        }

        let mut args_ = ArrayList::with_capacity(Thread::current(), args.len() - 3);

        for i in 3..args.len() {
            args_.push(Thread::current(), args[i]);
        }

        let s = args[1].to_string();

        return (wrong.unwrap())(
            args[0].strsym(),
            &s,
            args[2].get_int32(),
            if negate {
                3 - args.len() as i32
            } else {
                args.len() as i32 - 3
            },
            &args_,
        );
    }
}

fn make_arity_expect_string(
    name: Option<&str>,
    minc: i32,
    maxc: i32,
    args: &[Value],
    map_name: &str,
) -> String {
    let name = name.unwrap_or("#<procedure>");

    let prefix1_msg;
    let prefix2_msg;
    let suffix_msg;
    if map_name.len() != 0 {
        prefix1_msg = map_name;
        prefix2_msg = ": argument mismatch;\n the given procedure's expected number of arguments does not match\n the given number of lists\n  given procedure: ";
        suffix_msg = "";
    } else {
        prefix1_msg = "";
        prefix2_msg = "";
        suffix_msg =
            ": arity mismatch;\n the expected number of arguments does not match the given number";
    }

    let mut f = if maxc == 0 {
        format!(
            "{}{}{}{}\n  expected: 0\n given: {}",
            prefix1_msg,
            prefix2_msg,
            name,
            suffix_msg,
            args.len()
        )
    } else if maxc < 0 {
        format!(
            "{}{}{}{}\n  expected: at least {}\n given: {}",
            prefix1_msg,
            prefix2_msg,
            name,
            suffix_msg,
            minc,
            args.len()
        )
    } else if minc == maxc {
        format!(
            "{}{}{}{}\n  expected: {}\n given: {}",
            prefix1_msg,
            prefix2_msg,
            name,
            suffix_msg,
            minc,
            args.len()
        )
    } else {
        format!(
            "{}{}{}{}\n  expected: {} to {}\n given: {}",
            prefix1_msg,
            prefix2_msg,
            name,
            suffix_msg,
            minc,
            maxc,
            args.len()
        )
    };

    if args.len() < 50 {
        for i in 0..args.len() {
            if i == 0 {
                f.push_str("\n  arguments...:\n   ");
            } else {
                f.push_str("\n   ");
            }

            f.push_str(&format!("{}", args[i]));
        }
    }

    f
}

fn wrong_count_impl(
    name: &str,
    minc: i32,
    mut maxc: i32,
    _argc: i32,
    args: &[Value],
) -> Result<(), Value> {
    if maxc > MAX_ARITY as i32 {
        maxc = -1;
    }

    let s = make_arity_expect_string(Some(name), minc, maxc, args, "");

    raise_exn!(FailContractArity, &[], "{}", s)
}

pub fn wrong_count<T>(
    name: &str,
    minc: i32,
    maxc: i32,
    argc: i32,
    args: &[Value],
) -> Result<T, Value> {
    wrong_count_impl(name, minc, maxc, argc, args).map(|_| unreachable!())
}

extern "C" fn error_proc(cfr: &mut CallFrame) -> ScmResult {
    match do_error("error", Exception::Fail, cfr.arguments()) {
        Ok(_) => unreachable!(),
        Err(v) => ScmResult::err(v),
    }
}

extern "C" fn assert_unreachable(_: &mut CallFrame) -> ScmResult {
    ScmResult::err(
        contract_error::<()>("assert-unreachable", "unreachable code reached", &[]).unwrap_err(),
    )
}

extern "C" fn raise_user_error(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::err(do_error("raise-user-error", Exception::FailUser, cfr.arguments()).unwrap_err())
}

extern "C" fn raise_type_error(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::err(
        do_raise_type_error("raise-type-error", cfr.arguments(), Exception::Exn).unwrap_err(),
    )
}

extern "C" fn raise_argument_error(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::err(
        do_raise_type_error("raise-arguments-error", cfr.arguments(), Exception::Fail).unwrap_err(),
    )
}

extern "C" fn raise_result_error(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::err(
        do_raise_type_error(
            "raise-result-error",
            cfr.arguments(),
            Exception::FailContract,
        )
        .unwrap_err(),
    )
}

fn do_raise_arity_error<T>(who: &str, args: &[Value]) -> Result<T, Value> {
    if !args[0].is_symbol() && !args[0].is_procedure() {
        return wrong_contract::<T>(who, "(or/c symbol? procedure?)", 0, args.len() as _, args);
    }

    let minc = args[1];
    let maxc = args[2];

    if !minc.is_int32() || minc.get_int32() < 0 {
        return wrong_contract::<T>(who, "exact-nonnegative-integer?", 1, args.len() as _, args);
    }

    if !maxc.is_int32() || maxc.get_int32() < -1 {
        return wrong_contract::<T>(
            who,
            "(or/c exact-nonnegative-integer? -1)",
            2,
            args.len() as _,
            args,
        );
    }

    let name = if args[0].is_symbol() {
        args[0].strsym()
    } else {
        get_proc_name(args[0]).unwrap_or("<anonymous>>")
    };

    match wrong_count_impl(
        name,
        minc.get_int32(),
        maxc.get_int32(),
        args.len() as i32 - 3,
        &args[3..],
    ) {
        Ok(_) => unreachable!(),
        Err(v) => Err(v),
    }
}

extern "C" fn raise_arity_error(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::err(do_raise_arity_error::<()>("raise-arity-error", cfr.arguments()).unwrap_err())
}

fn wrong_return_arity_impl(
    where_: &str,
    expected: i32,
    got: i32,
    args: Option<&[Value]>,
    s: &str,
) -> Result<(), Value> {
    let v = if got == 0 || args.is_none() {
        "".to_string()
    } else {
        make_arg_lines_string("   ", -1, got, args.unwrap())
    };

    let s = format!("{}{}result arity mismatch;\n expected number of values not received\n expected: {}\n received: {} {}\n  values...:{}",
        where_,
        if where_.len() != 0 {
            ": "
        } else {
            ""
        },
        expected,
        got,
        s,
        v
    );

    raise_exn!(FailContractArity, &[], "{}", s)
}

extern "C" fn raise_result_arity_error(cfr: &mut CallFrame) -> ScmResult {
    let args = cfr.arguments();
    let where_ = if args[0].is_false() {
        ""
    } else if args[0].is_symbol() {
        args[0].strsym()
    } else {
        return ScmResult::err(
            wrong_contract::<()>(
                "raise-result-arity-error",
                "(or/c #f symbol?)",
                0,
                args.len() as _,
                args,
            )
            .unwrap_err(),
        );
    };

    let expected = if args[1].is_int32() {
        args[0].get_int32()
    } else if args[1].is_bignum() && !args[1].bignum().is_negative() {
        ((-1i32 as u32) >> 1) as i32
    } else {
        -1
    };

    if expected < 1 {
        return ScmResult::err(
            wrong_contract::<()>(
                "raise-result-arity-error",
                "exact-nonnegative-integer?",
                1,
                args.len() as _,
                args,
            )
            .unwrap_err(),
        );
    }

    let detail = if args[2].is_false() {
        ""
    } else if args[2].is_string() {
        args[2].strsym()
    } else {
        return ScmResult::err(
            wrong_contract::<()>(
                "raise-result-arity-error",
                "(or/c #f string?)",
                2,
                args.len() as _,
                args,
            )
            .unwrap_err(),
        );
    };

    ScmResult::err(
        wrong_return_arity_impl(
            where_,
            expected,
            args.len() as i32 - 3,
            Some(&args[2..]),
            detail,
        )
        .unwrap_err(),
    )
}

pub static SRCLOC: Lazy<Value> = Lazy::new(|| {
    let typ = crate::runtime::structure::make_struct_type_from_string(
        "srcloc",
        Value::encode_bool_value(false),
        4,
        false.into(),
        false.into(),
    )
    .unwrap();
    typ
});

pub fn make_srcloc(source: Value, line: i32, column: i32, position: i32) -> Value {
    make_simple_struct_instance_from_array(
        &[
            source,
            Value::encode_int32(line),
            Value::encode_int32(column),
            Value::encode_int32(position),
        ],
        *SRCLOC,
    )
}

pub(crate) fn init_error() {
    let capy = scm_capy_module().module();

    macro_rules! defproc {
        ($($name: ident, $lit: literal, $mina: expr, $maxa: expr)*) => {
            $(

                let subr = scm_make_subr($lit, $name, $mina, $maxa);
                scm_define(capy, $lit.intern().into(), subr).unwrap();
            )*
        };
    }

    defproc! {
        error_proc, "error", 1, MAX_ARITY
        raise_user_error, "raise-user-error", 1, MAX_ARITY
        raise_type_error, "raise-type-error", 3, MAX_ARITY
        raise_argument_error, "raise-argument-error", 3, MAX_ARITY
        raise_result_error, "raise-result-error", 3, MAX_ARITY
        raise_argument_error, "raise-arguments-error", 3, MAX_ARITY
        raise_range_error, "raise-range-error", 7, 8
        raise_arity_error, "raise-arity-error", 3, MAX_ARITY
        raise_result_arity_error, "raise-result-arity-error", 3, MAX_ARITY
        assert_unreachable, "assert-unreachable", 0, 0
    };
    let _ = *EXN_TABLE;
    heap().add_root(SimpleRoot::new("exn-table", "exns", |processor| {
        let visitor = processor.visitor();
        if let Some(table) = Lazy::get(&EXN_TABLE) {
            for rec in table.iter() {
                rec.exptime.trace(visitor);
                rec.names.trace(visitor);
                rec.typ.trace(visitor);
            }
        }

        if let Some(srcloc) = Lazy::get(&SRCLOC) {
            srcloc.trace(visitor);
        }
    }));
    for exn in EXN_TABLE.iter() {
        if let Some(mut names) = exn.names.filter(|x| x.len() > 0) {
            force_struct_type_info(exn.typ.struct_type());
            let values = make_struct_values(exn.typ, &mut names, EXN_FLAGS);

            for j in (0..names.len() - 1).rev() {
                let name = names[j];
                let value = values.values_ref(j);
                scm_define(capy, name.strsym().intern(), value).unwrap();
            }
        }
    }

    let typ = *SRCLOC;
    let mut names = make_struct_names_from_array(
        "srcloc",
        4,
        &["source", "line", "column", "position"],
        EXN_FLAGS,
    );
    let values = make_struct_values(typ, &mut names, EXN_FLAGS);

    for j in (0..names.len() - 1).rev() {
        let name = names[j];
        let value = values.values_ref(j);
        scm_define(capy, name.strsym().intern(), value).unwrap();
    }
}
