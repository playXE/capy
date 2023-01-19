use rsgc::{
    prelude::{Handle, Object},
    system::string::Str,
};

use crate::{
    data::structure::StructInstance,
    prelude::{library_manager, Arguments, Context, Value},
    utilities::arraylist::ArrayList,
    ScmResult,
};
#[macro_export]
macro_rules! raise_exn {
    ($ctx: expr, $id: expr, $args: expr, $msg: expr) => {
        {
            let ctx: &mut $crate::runtime::Context = &mut $ctx;
            $crate::runtime::error::_raise_exn(ctx, $id, $args, format_args!($msg))
        }
    };
    ($ctx: expr, $id: expr, $args: expr, $msg: expr, $($fmt_arg: expr),*) => {
        {
            let ctx: &mut $crate::runtime::Context = &mut $ctx;
            $crate::runtime::error::_raise_exn(ctx, $id, $args, format_args!($msg, $($fmt_arg),*))
        }
    };
}

#[derive(Clone, Copy)]
pub struct ExnRec {
    args: usize,
    typ: Value,
    names: Option<Handle<ArrayList<Value>>>,
    count: u32,
    exptime: Value,
}

impl Object for ExnRec {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.typ.trace(visitor);
        self.names.trace(visitor);
        self.exptime.trace(visitor);
    }
}

pub const EXN_FIELDS: [&'static str; 2] = ["message", "continuation-marks"];
pub const EXN_FAIL_CONTRACT_VARIABLE_FIELDS: [&'static str; 1] = ["id"];
pub const EXN_FAIL_READ_FIELDS: [&'static str; 1] = ["srclocs"];
pub const EXN_FAIL_FILESYSTEM_ERRNO_FIELDS: [&'static str; 1] = ["errno"];
pub const EXN_FAIL_NETWORK_ERRNO_FIELDS: [&'static str; 1] = ["errno"];
pub const EXN_BREAK_FIELDS: [&'static str; 1] = ["continuation"];

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(C)]
pub enum Exn {
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
    FailBreak,
    FailBreakHangUp,
    FailBreakTerminate,
    FailLast,
}

use super::{libraries::structure::*, Runtime};

const EXN_FLAGS: i32 = SCHEME_STRUCT_EXPTIME
    | SCHEME_STRUCT_NO_SET
    | SCHEME_STRUCT_NO_MAKE_PREFIX
    | SCHEME_STRUCT_BUILTIN;

pub fn init_exn(ctx: &mut Context) -> [ExnRec; Exn::FailLast as usize] {
    let mut exn_table = [ExnRec {
        args: 0,
        typ: Value::nil(),
        names: None,
        count: 0,
        exptime: Value::nil(),
    }; Exn::FailLast as usize];

    exn_table[Exn::Fail as usize].args = 2;
    exn_table[Exn::FailContract as usize].args = 2;
    exn_table[Exn::FailContractArity as usize].args = 2;
    exn_table[Exn::FailContractDivideByZero as usize].args = 2;
    exn_table[Exn::FailContractNonFixnumResult as usize].args = 2;
    exn_table[Exn::FailContractContinuation as usize].args = 2;
    exn_table[Exn::FailContractVariable as usize].args = 3;
    exn_table[Exn::FailRead as usize].args = 3;
    exn_table[Exn::FailReadEof as usize].args = 3;
    exn_table[Exn::FailReadNonChar as usize].args = 3;
    exn_table[Exn::FailFilesystem as usize].args = 2;
    exn_table[Exn::FailFilesystemExists as usize].args = 2;
    exn_table[Exn::FailFilesystemVersion as usize].args = 2;
    exn_table[Exn::FailFilesystemErrno as usize].args = 3;
    exn_table[Exn::FailNetwork as usize].args = 2;
    exn_table[Exn::FailNetworkErrno as usize].args = 3;
    exn_table[Exn::FailOutOfMemory as usize].args = 2;
    exn_table[Exn::FailUnsupported as usize].args = 2;
    exn_table[Exn::FailUser as usize].args = 2;
    exn_table[Exn::FailBreak as usize].args = 2;
    exn_table[Exn::FailBreakHangUp as usize].args = 2;
    exn_table[Exn::FailBreakTerminate as usize].args = 2;

    macro_rules! setup_struct {
        ($id: expr, $parent: expr, $name: expr, $args: expr, $props: expr, $guard: expr) => {{
            let args: &[&str] = $args;
            let tmpo =
                make_struct_type_from_string(ctx, $name, $parent, args.len(), $props, $guard)
                    .unwrap();
            exn_table[$id as usize].typ = Value::new(tmpo);

            let tmpop = make_struct_names_from_array(
                ctx,
                $name,
                args.len() as _,
                $args,
                EXN_FLAGS,
                Some(&mut exn_table[$id as usize].count),
            );
            exn_table[$id as usize].names = Some(ctx.mutator().allocate(tmpop));
        }};
    }

    macro_rules! exn_parent {
        ($id: expr) => {
            exn_table[$id as usize].typ.get_handle_of()
        };
    }

    setup_struct!(
        Exn::Exn,
        None,
        "exn",
        &["message", "stack-trace"],
        Value::nil(),
        Value::nil()
    );
    setup_struct!(
        Exn::Fail,
        Some(exn_parent!(Exn::Exn)),
        "exn:fail",
        &[],
        Value::nil(),
        Value::nil()
    );
    setup_struct!(
        Exn::FailContract,
        Some(exn_parent!(Exn::Fail)),
        "exn:fail:contract",
        &[],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailContractArity,
        Some(exn_parent!(Exn::FailContract)),
        "exn:fail:contract:arity",
        &[],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailContractDivideByZero,
        Some(exn_parent!(Exn::FailContract)),
        "exn:fail:contract:divide-by-zero",
        &[],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailContractNonFixnumResult,
        Some(exn_parent!(Exn::FailContract)),
        "exn:fail:contract:non-fixnum-result",
        &[],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailContractContinuation,
        Some(exn_parent!(Exn::FailContract)),
        "exn:fail:contract:continuation",
        &[],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailContractVariable,
        Some(exn_parent!(Exn::FailContract)),
        "exn:fail:contract:variable",
        &["id"],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailRead,
        Some(exn_parent!(Exn::Fail)),
        "exn:fail:read",
        &["srclocs"],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailReadEof,
        Some(exn_parent!(Exn::FailRead)),
        "exn:fail:read:eof",
        &[],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailReadNonChar,
        Some(exn_parent!(Exn::FailRead)),
        "exn:fail:read:non-char",
        &[],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailFilesystem,
        Some(exn_parent!(Exn::Fail)),
        "exn:fail:filesystem",
        &[],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailFilesystemExists,
        Some(exn_parent!(Exn::FailFilesystem)),
        "exn:fail:filesystem:exists",
        &[],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailFilesystemVersion,
        Some(exn_parent!(Exn::FailFilesystem)),
        "exn:fail:filesystem:version",
        &[],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailFilesystemErrno,
        Some(exn_parent!(Exn::FailFilesystem)),
        "exn:fail:filesystem:errno",
        &["errno"],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailNetwork,
        Some(exn_parent!(Exn::Fail)),
        "exn:fail:network",
        &[],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailNetworkErrno,
        Some(exn_parent!(Exn::FailNetwork)),
        "exn:fail:network:errno",
        &["errno"],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailOutOfMemory,
        Some(exn_parent!(Exn::Fail)),
        "exn:fail:out-of-memory",
        &[],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailUnsupported,
        Some(exn_parent!(Exn::Fail)),
        "exn:fail:unsupported",
        &[],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailUser,
        Some(exn_parent!(Exn::Fail)),
        "exn:fail:user",
        &[],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailBreak,
        Some(exn_parent!(Exn::Exn)),
        "exn:break",
        &[],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailBreakHangUp,
        Some(exn_parent!(Exn::FailBreak)),
        "exn:break:hang-up",
        &[],
        Value::nil(),
        Value::nil()
    );

    setup_struct!(
        Exn::FailBreakTerminate,
        Some(exn_parent!(Exn::FailBreak)),
        "exn:break:terminate",
        &[],
        Value::nil(),
        Value::nil()
    );

    let env = library_manager().scheme_module.get_handle_of();

    for i in 0..Exn::FailLast as usize {
        if exn_table[i].count != 0 {
            let values;

            values = make_struct_values(
                ctx,
                exn_table[i].typ.get_handle_of(),
                &exn_table[i].names.unwrap(),
                exn_table[i].count as _,
                EXN_FLAGS,
            )
            .unwrap();

            let mut j = exn_table[i].count - 1;
            while j > 0 {
                j -= 1;
                let value = values[j as usize];
                let name = exn_table[i].names.unwrap()[j as usize];

                library_manager().define_const(env, name.get_symbol(), value, true);
            }
        }
    }

    exn_table
}

fn do_error(ctx: &mut Context, who: &str, mode: Exn, args: &Arguments) -> ScmResult<()> {
    let mut newargs: [Value; 2] = [Value::nil(); 2];

    if args[0].is_symbol() {
        if args.len() < 2 {
            newargs[0] = Str::new(
                ctx.mutator(),
                format!("error: {}", args[0].get_symbol().identifier()),
            )
            .into();
        } else {
        }
    } else {
    }

    newargs[1] = Value::nil();

    let structure =
        make_struct_instance(ctx, Runtime::get().exn(mode).typ.get_handle_of(), &newargs)?;

    do_raise(ctx, true, structure.into())
}

fn do_raise(ctx: &mut Context, need_debug: bool, arg: Value) -> ScmResult<()> {
    if need_debug {
        let trace = ctx.get_stack_trace(None);
        let mut trace_list = Value::nil();
        for i in (0..trace.len()).rev() {
            trace_list = ctx.make_pair(trace[i].into(), trace_list)
        }

        ctx.mutator().write_barrier(arg.get_handle());
        arg.get_handle_of::<StructInstance>()
            .field_set(1, trace_list);
    }
    Err(arg)
}

pub fn raise(ctx: &mut Context, arg: Value) -> ScmResult {
    do_raise(ctx, false, arg).map(|_| Value::nil())
}

pub fn error(ctx: &mut Context, args: &Arguments) -> ScmResult {
    do_error(ctx, "error", Exn::FailUser, args).map(|_| Value::nil())
}

pub fn make_arg_lines_string(
    ctx: &mut Context,
    indent: &str,
    which: i32,
    args: &Arguments,
) -> String {
    if args.len() == 0 || (args.len() == 1 && which == 0) {
        return " [none]".to_string();
    }

    let mut other = String::new();

    for i in 0..args.len() {
        if i != which as usize {
            other.push('\n');
            other.push_str(indent);
            other.push_str(&args[i].to_string(false));
        }
    }

    other
}

pub fn wrong_contract(
    mut ctx: &mut Context,
    name: &str,
    expected: &str,
    which: i32,
    args: &Arguments,
    res: bool,
) -> ScmResult {
    let mut isgiven = "given";
    let mut kind = "argument";

    let o = args[which.max(0) as usize];

    if res {
        isgiven = "received";
        kind = "result";
    }

    if which == -2 {
        isgiven = "received";
        kind = "result";
    }

    if args.len() == 0 {
        kind = "value";
    }

    let s = o.to_string(false);

    if which < 0 || args.len() <= 1 {
        raise_exn!(
            ctx,
            Exn::FailContract,
            &[],
            "{}: contract violation\n  expected: {}\n  {}:{}",
            name,
            expected,
            isgiven,
            s
        )
    } else {
        let other = make_arg_lines_string(ctx, "   ", which, args);
        raise_exn!(
            ctx,
            Exn::FailContract,
            &[],
            "{}: contract violation\n  expected: {}\n  {}: {}\n  {} position: {}\n  other {}...:{}",
            name,
            expected,
            isgiven,
            s,
            kind,
            which + 1,
            if !res { "arguments" } else { "results" },
            other
        )
    }
}

pub fn _raise_exn(
    ctx: &mut Context,
    id: Exn,
    args: &Arguments,
    fmt: std::fmt::Arguments,
) -> ScmResult {
    let c = if id == Exn::FailLast {
        3
    } else {
        ctx.runtime().exn(id).args
    };

    let mut eargs = [Value::nil(); 3];

    let mut j = 0;

    for i in 2..c {
        eargs[i] = args[j];
        j += 1;
    }

    let msg = std::fmt::format(fmt);

    finish_raise_exn(ctx, id, c, None, &mut eargs, false, &msg)
}

fn finish_raise_exn(
    ctx: &mut Context,
    mut id: Exn,
    mut c: usize,
    errno_val: Option<i32>,
    eargs: &mut [Value],
    unsupported: bool,
    msg: &str,
) -> ScmResult {
    eargs[0] = Str::new(ctx.mutator(), msg).into();
    eargs[1] = Value::nil();

    if let Some(err) = errno_val {
        if id == Exn::FailFilesystem {
            id = Exn::FailFilesystemErrno;
            eargs[2] = Value::new(err);
            c += 1;
        } else if id == Exn::FailNetwork {
            id = Exn::FailNetworkErrno;
            eargs[2] = Value::new(err);
            c += 1;
        }
    } else if unsupported {
        if id == Exn::Fail {
            id = Exn::FailUnsupported;
        }
    }

    let instance =
        make_struct_instance(ctx, Runtime::get().exn(id).typ.get_handle_of(), &eargs[..c])?;
    do_raise(ctx, true, instance.into()).map(|_| Value::nil())
}

pub fn wrong_type(
    mut ctx: &mut Context,
    name: &str,
    expected: &str,
    which: i32,
    args: &Arguments,
    result: bool,
) -> ScmResult {
    let mut isress = "argument";
    let mut isgiven = "given";
    let mut res = false;

    let o = args[which.max(0) as usize];

    if result {
        isress = "result";
        isgiven = "received";
        res = true;
    }

    if which == -2 {
        isress = "value";
        isgiven = "received";
    }

    let s = o.to_string(false);

    if which < 0 || args.len() == 1 {
        return raise_exn!(
            ctx,
            Exn::FailContract,
            &[],
            "{}: expect{} {} of type <{}>; \n{}: {}",
            name,
            if which < 0 { "ed" } else { "s" },
            isress,
            expected,
            isgiven,
            s
        );
    } else {
        let other = if which > 0 && args.len() > 0 {
            make_args_string("other", res, which, args)
        } else {
            "".to_string()
        };

        return raise_exn!(
            ctx,
            Exn::FailContract,
            &[],
            "{}: expects type <{}> as {} {}, \ngiven: {}{}",
            name,
            expected,
            which + 1,
            isress,
            s,
            other
        )
    }
}

pub fn make_args_string(
    s: &str,
    res: bool,
    which: i32,
    args: &Arguments,
) -> String {
    let mut other = String::new();
    let mut isres = "arguments";

    if res {
        isres = "results";
    }

    other.push_str(&format!("; {}{} were:", s, isres));

    if args.len() < 50 {
        for i in 0..args.len() {
            if i != which as usize {
                other.push(' ');
                other.push_str(&args[i].to_string(false));
            }
        }
    } else {
        other.push_str(&format!("; given {} arguments total", args.len()));
    }

    other
}

pub fn arg_mismatch(mut ctx: &mut Context, name: &str, msg: &str, o: Option<Value>) -> ScmResult {
    let s = if let Some(o) = o {
        o.to_string(false)
    } else {
        "".to_string()
    };

    raise_exn!(
        ctx,
        Exn::FailContract,
        &[],
        "{}: {}{}", name, msg, s 
    )
}

fn do_out_of_range(mut ctx: &mut Context, 
    name: &str,
    typ: Option<&str>,
    which: &str,
    ending: i32,
    i: Value, 
    s: Value,
    low_bound: Value,
    sstart: Value,
    slen: Value
) -> ScmResult {
    let typ = typ.unwrap_or("string");

    todo!()
}