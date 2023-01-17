use rsgc::{
    prelude::{Handle, Object},
    system::string::Str,
};

use crate::{
    prelude::{library_manager, Arguments, Context, Value},
    utilities::arraylist::ArrayList,
};

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

use super::libraries::structure::*;

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

fn do_error(ctx: &mut Context, who: &str, mode: Exn, args: &Arguments) -> Value {
    let mut newargs: [Value; 2] = [Value::nil(); 2];

    if args[0].is_symbol() {
        if args.len() < 2 {
            newargs[0] = Str::new(
                ctx.mutator(),
                format!("error: {}", args[0].get_symbol().identifier()),
            ).into();
        } else {

        }
    }

    todo!()
}
