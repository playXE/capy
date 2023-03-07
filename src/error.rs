//! Error handling.
//!
//!
//! This module defines exception types and basic functions for raising and constructing exceptions.
//!
use std::any::Any;

use once_cell::sync::Lazy;
use rsgc::{system::arraylist::ArrayList, thread::Thread, prelude::{Handle, Object}, heap::{heap::heap, root_processor::SimpleRoot}};

use crate::{
    number::{bin_gte, bin_lt},
    ports_v2::*,
    print::*,
    string::do_format,
    structure::{make_struct_instance_, STRUCT_EXPTIME, STRUCT_NO_SET, STRUCT_NO_MAKE_PREFIX, force_struct_type_info, make_struct_values, make_simple_struct_instance_from_array},
    value::{Str, Type, Value, SCHEME_MAX_ARGS},
    vm::{Trampoline, intern}, compiler::env::environment_set,
};

#[macro_export]
macro_rules! raise_exn {
    ($id:ident, $eargs:expr, $msg:literal $(,)? $($arg:expr),*) => {
        $crate::error::finish_exn_impl($crate::error::Exception::$id, $crate::error::EXN_TABLE[$crate::error::Exception::$id as usize].args, $eargs, format!($msg, $($arg),*), None, false)
    };

    ($t: ty, $id:ident, $eargs:expr, $msg:literal $(,)? $($arg:expr),*) => {
        $crate::error::finish_exn_impl::<$t>($crate::error::Exception::$id, $crate::error::EXN_TABLE[$crate::error::Exception::$id as usize].args, $eargs, format!($msg, $($arg),*), None, false)
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
        Value::make_null(),
        eargs.len() + 2,
    );

    for i in 2..eargs.len() {
        reargs.set(Thread::current(), i, eargs[i]);
    }
    let vm = crate::vm::vm();
    reargs[0] = Str::new(vm.mutator(), msg);
    reargs[1] = Value::make_null(); // cmark

    if let Some(errno) = errno_val {
        if id == Exception::FailFilesystem {
            id = Exception::FailFilesystemErrno;
            reargs[2] = Value::make_int(errno.0);
            c += 1;
        } else if id == Exception::FailNetwork {
            id = Exception::FailNetworkErrno;
            reargs[2] = Value::make_int(errno.0);
            c += 1;
        }
    } else if unsupported {
        if id == Exception::Fail {
            id = Exception::FailUnsupported;
        }
    }

    let instance = make_struct_instance_(vm, EXN_TABLE[id as usize].typ, &reargs[0..c])?;

    Err(instance)
}

pub const EXN_FIELDS: [&'static str; 2] = ["message", "continuation-marks"];
pub const EXN_FAIL_CONTRACT_VARIABLE_FIELDS: [&'static str; 1] = ["id"];
pub const EXN_FAIL_READ_FIELDS: [&'static str; 1] = ["srclocs"];
pub const EXN_FAIL_FILESYSTEM_ERRNO_FIELDS: [&'static str; 1] = ["errno"];
pub const EXN_FAIL_NETWORK_ERRNO_FIELDS: [&'static str; 1] = ["errno"];
pub const EXN_BREAK_FIELDS: [&'static str; 1] = ["continuation"];

pub const EXN_FLAGS: i32 = STRUCT_EXPTIME | STRUCT_NO_SET | STRUCT_NO_MAKE_PREFIX;

pub fn get_exn_type(id: Exception) -> Value {
    EXN_TABLE[id as usize].typ
}

pub static EXN_TABLE: Lazy<[ExnRec; Exception::Other as usize]> = Lazy::new(|| {
    let mut exn_table = [ExnRec {
        args: 0,
        typ: Value::make_null(),
        names: None,
        exptime: Value::make_null(),
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
            let tmpo =
                $crate::structure::make_struct_type_from_string($crate::vm::vm(), $name, $parent, args.len(), $props, $guard)
                    .unwrap();
            exn_table[$id as usize].typ = tmpo;

            let tmpop = $crate::structure::make_struct_names_from_array(
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
        Value::make_false(),
        "exn",
        &["message", "stack-trace"],
        Value::null(),
        Value::null()
    );

    setup_struct!(
        Exception::Fail,
        exn_parent!(Exception::Exn),
        "exn:fail",
        &[],
        Value::null(),
        Value::null()
    );

    setup_struct!(
        Exception::FailContract,
        exn_parent!(Exception::Fail),
        "exn:fail:contract",
        &[],
        Value::null(),
        Value::null()
    );

    setup_struct!(
        Exception::FailContractArity,
        exn_parent!(Exception::FailContract),
        "exn:fail:contract:arity",
        &[],
        Value::null(),
        Value::null()
    );

    setup_struct!(
        Exception::FailContractDivideByZero,
        exn_parent!(Exception::FailContract),
        "exn:fail:contract:divide-by-zero",
        &[],
        Value::null(),
        Value::null()
    );

    setup_struct!(
        Exception::FailContractNonFixnumResult,
        exn_parent!(Exception::FailContract),
        "exn:fail:contract:non-fixnum-result",
        &[],
        Value::null(),
        Value::null()
    );

    setup_struct!(
        Exception::FailContractContinuation,
        exn_parent!(Exception::FailContract),
        "exn:fail:contract:continuation",
        &[],
        Value::null(),
        Value::null()
    );

    setup_struct!(
        Exception::FailContractVariable,
        exn_parent!(Exception::FailContract),
        "exn:fail:contract:variable",
        &EXN_FAIL_CONTRACT_VARIABLE_FIELDS,
        Value::null(),
        Value::null()
    );

    setup_struct!(
        Exception::FailRead,
        exn_parent!(Exception::Fail),
        "exn:fail:read",
        &[],
        Value::null(),
        Value::null()
    );

    setup_struct!(
        Exception::FailReadEof,
        exn_parent!(Exception::FailRead),
        "exn:fail:read:eof",
        &[],
        Value::null(),
        Value::null()
    );

    setup_struct!(
        Exception::FailReadNonChar,
        exn_parent!(Exception::FailRead),
        "exn:fail:read:non-char",
        &[],
        Value::null(),
        Value::null()
    );

    setup_struct!(
        Exception::FailFilesystem,
        exn_parent!(Exception::Fail),
        "exn:fail:filesystem",
        &[],
        Value::null(),
        Value::null()
    );

    setup_struct!(
        Exception::FailFilesystemExists,
        exn_parent!(Exception::FailFilesystem),
        "exn:fail:filesystem:exists",
        &[],
        Value::null(),
        Value::null()
    );

    setup_struct!(
        Exception::FailFilesystemVersion,
        exn_parent!(Exception::FailFilesystem),
        "exn:fail:filesystem:version",
        &[],
        Value::null(),
        Value::null()
    );

    setup_struct!(
        Exception::FailFilesystemErrno,
        exn_parent!(Exception::FailFilesystem),
        "exn:fail:filesystem:errno",
        &["errno"],
        Value::null(),
        Value::null()
    );

    setup_struct!(
        Exception::FailNetwork,
        exn_parent!(Exception::Fail),
        "exn:fail:network",
        &[],
        Value::null(),
        Value::null()
    );

    setup_struct!(
        Exception::FailNetworkErrno,
        exn_parent!(Exception::FailNetwork),
        "exn:fail:network:errno",
        &["errno"],
        Value::null(),
        Value::null()
    );

    setup_struct!(
        Exception::FailOutOfMemory,
        exn_parent!(Exception::Fail),
        "exn:fail:out-of-memory",
        &[],
        Value::null(),
        Value::null()
    );

    setup_struct!(
        Exception::FailUnsupported,
        exn_parent!(Exception::Fail),
        "exn:fail:unsupported",
        &[],
        Value::null(),
        Value::null()
    );

    setup_struct!(
        Exception::FailUser,
        exn_parent!(Exception::Fail),
        "exn:fail:user",
        &[],
        Value::null(),
        Value::null()
    );

    heap().add_root(SimpleRoot::new("exception-records", "exnrec", |processor| {
        for rec in EXN_TABLE.iter() {
            rec.typ.trace(processor.visitor());
            rec.names.trace(processor.visitor());
            rec.exptime.trace(processor.visitor());
        }
    }));

    exn_table
});



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
    let vm = crate::vm::vm();
    if !bin_lt(vm, slen, sstart)? {
        let mut small_end = false;
        if ending {
            if bin_gte(vm, i, low_bound)? && bin_lt(vm, i, sstart)? {
                small_end = true;
            }
        }

        let sstr = s.to_string();

        raise_exn!(
            FailContract,
            &[],
            "{}: {} index is {}\n  {}index: {}\n  {}{}{}{}]\n  {}: {}",
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
        Value::make_int(0),
        Value::make_int(start),
        Value::make_int(len),
    )
    .map(|_| unreachable!())
}

fn do_raise_range_error(who: &str, args: &[Value]) -> Trampoline {
    if !args[0].symbolp() {
        return wrong_contract::<()>(who, "symbol?", 0, args.len() as _, args).into();
    }

    if !args[1].strp() {
        return wrong_contract::<()>(who, "string?", 1, args.len() as _, args).into();
    }

    if !args[2].strp() {
        return wrong_contract::<()>(who, "string?", 2, args.len() as _, args).into();
    }

    if !args[3].intp() || args[3].get_type() != Type::Bignum {
        return wrong_contract::<()>(who, "exact-integer?", 3, args.len() as _, args).into();
    }

    if !args[5].intp() || args[5].get_type() != Type::Bignum {
        return wrong_contract::<()>(who, "exact-integer?", 5, args.len() as _, args).into();
    }

    if !args[6].intp() || args[6].get_type() != Type::Bignum {
        return wrong_contract::<()>(who, "exact-integer?", 6, args.len() as _, args).into();
    }

    if args.len() > 7 {
        if !args[7].falsep() && !args[7].intp() && args[7].get_type() != Type::Bignum {
            return wrong_contract::<()>(who, "(or/c exact-integer? #f)", 7, args.len() as _, args)
                .into();
        }
    }

    let typ = args[1].str();
    let desc = args[2].str();

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
        Err(e) => Trampoline::Throw(e),
    }
}

define_proc! {
    extern "raise-range-error", raise_range_error (_vm, args) 6, 7 => {
        do_raise_range_error("raise-range-error", args)
    }
}

const MAX_MISMATCH_EXTRAS: usize = 5;

pub fn contract_error<'a, T>(name: &str, msg: &str, args: &'a [&'a dyn Any]) -> Result<T, Value> {
    let mut strs: [Option<&&str>; MAX_MISMATCH_EXTRAS] = [None; MAX_MISMATCH_EXTRAS];
    let mut vs: [Option<Value>; MAX_MISMATCH_EXTRAS] = [None; MAX_MISMATCH_EXTRAS];
    let mut v_strs: [Option<String>; MAX_MISMATCH_EXTRAS] = [None, None, None, None, None];

    let mut cnt = 0;

    while cnt < MAX_MISMATCH_EXTRAS {
        if cnt < args.len() {
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
    let mut newargs: [Value; 2] = [Value::make_null(); 2];

    if args[0].symbolp() {
        if args.len() < 2 {
            let s = args[0].strsym();
            let __l = s.len();

            newargs[0] = Str::new(Thread::current(), format!("error: {}", s)).into();
        } else {
            let port = Port::new(Thread::current());
            port_open_bytevector(port, intern("do_error"), SCM_PORT_DIRECTION_OUT, Value::make_false(), Value::make_false());
            if !args[1].strp() {
                return wrong_contract(who, "string?", 1, args.len() as _, args);
            }

            do_format(who, port, None, 1, 2, args.len(), args)?;

            let s = port_extract_string(port).unwrap();
            newargs[0] = Str::new(
                Thread::current(),
                format!("{}: {}", args[0].strsym(), s.str()),
            )
            .into();
        }
    } else {
        if !args[0].strp() {
            return wrong_contract(who, "(or/c string? symbol?)", 0, args.len() as _, args);
        }

        let port = Port::new(Thread::current());
        port_open_bytevector(port, intern("do_error"), SCM_PORT_DIRECTION_OUT, Value::make_false(), Value::make_false());
        let mut printer = Printer::new(crate::vm::vm(), port);
        printer.write(args[0])?;
       

        for i in 1..args.len() {
            printer.puts(" ")?;
            printer.write(args[i])?;
        }

        newargs[0] = port_extract_string(port).unwrap();
    }

    newargs[1] = Value::make_null();

    let instance = make_struct_instance_(crate::vm::vm(), EXN_TABLE[mode as usize].typ, &newargs)?;

    Err(instance)
}



define_proc! {
    extern "error", error (_vm, args) 1, -1 => {
        match do_error("error", Exception::Fail, args) {
            Ok(_) => unreachable!(),
            Err(e) => Trampoline::Throw(e),
        }
    }
}

fn do_raise_type_error(name: &str, args: &[Value], mode: Exception) -> Result<(), Value> {
    if !args[0].symbolp() {
        return wrong_contract(name, "symbol?", 0, args.len() as _, args);
    }

    if !args[1].strp() {
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
        if !(args[2].intp() && args[2].int() >= 0) {
            return wrong_contract(name, "exact-nonnegative-integer?", 2, args.len() as _, args);
        }

        if args[2].intp() && args[2].int() >= args.len() as i32 - 3 {
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
                    &Value::make_int(args.len() as i32 - 3),
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
            args[2].int(),
            if negate {
                3 - args.len() as i32
            } else {
                args.len() as i32 - 3
            },
            &args_,
        );
    }
}

define_proc! {
    extern "raise-type-error", raise_type_error (_vm, args) 3, 4 => {
        match do_raise_type_error("raise-type-error", args, Exception::Exn) {
            Ok(_) => unreachable!(),
            Err(e) => Trampoline::Throw(e),
        }
    }
}

define_proc! {
    extern "raise-argument-error", raise_argument_error (_vm, args) 3, 4 => {
        match do_raise_type_error("raise-argument-error", args, Exception::Fail) {
            Ok(_) => unreachable!(),
            Err(e) => Trampoline::Throw(e),
        }
    }
}

define_proc! {
    extern "raise-result-error", raise_result_error (_vm, args) 3, 4 => {
        match do_raise_type_error("raise-result-error", args, Exception::FailContract) {
            Ok(_) => unreachable!(),
            Err(e) => Trampoline::Throw(e),
        }
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
    if maxc > SCHEME_MAX_ARGS {
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


pub static SRCLOC: Lazy<Value> = Lazy::new(|| {
    let typ = crate::structure::make_struct_type_from_string(crate::vm::vm(), "srcloc", Value::make_false(), 4, Value::make_false(), Value::make_false()).unwrap();
    typ
});

pub fn make_srcloc(source: Value, line: i32, column: i32, position: i32) -> Value {
    make_simple_struct_instance_from_array(crate::vm::vm(), &[source, Value::make_int(line), Value::make_int(column), Value::make_int(position)], *SRCLOC)
}

pub fn initialize_error(env: Value) {
    environment_set(env, *RAISE_ARGUMENT_ERROR_NAME, *RAISE_ARGUMENT_ERROR_PROC);
    environment_set(env, *RAISE_RESULT_ERROR_NAME, *RAISE_RESULT_ERROR_PROC);
    environment_set(env, *RAISE_TYPE_ERROR_NAME, *RAISE_TYPE_ERROR_PROC);
    environment_set(env, *ERROR_NAME, *ERROR_PROC);


    for exn in EXN_TABLE.iter() {
        if let Some(mut names) = exn.names.filter(|x| x.len() > 0) {

            force_struct_type_info(exn.typ.downcast_structuretype());
            let values = make_struct_values(exn.typ, &mut names, EXN_FLAGS);

            for j in (0..names.len() - 1).rev() {
                let name = names[j];
                let value = values.values_ref(j);
                environment_set(env, name, value);
            }
        } 
    }

    let typ = *SRCLOC;
    let mut names = crate::structure::make_struct_names_from_array("srcloc", 4, &["source", "line", "column", "position"], EXN_FLAGS);
    let values = crate::structure::make_struct_values(typ, &mut names, EXN_FLAGS);

    for j in (0..names.len() - 1).rev() {
        let name = names[j];
        let value = values.values_ref(j);
        environment_set(env, name, value);
    }

}

