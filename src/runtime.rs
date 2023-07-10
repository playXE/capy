use self::{load::scm_require, module::scm_reqbase_module, string::make_string, structure::{is_struct_instance, struct_ref}, error::{EXN_TABLE, Exception}};
use crate::vm::interpreter::TRAMPOLINE_INSTALLED;
use rsgc::thread::Thread;

#[macro_use]
pub mod list;
#[macro_use]
pub mod vector;
pub mod arith;
pub mod arithfun;
pub mod base;
pub mod bigint;
pub mod cmp;
pub mod complex;
pub mod cont;
pub mod io;
pub mod date;
pub mod error;
pub mod fun;
pub mod load;
pub mod macros;
pub mod module;
pub mod number;
pub mod object;
pub mod port;
pub mod portfun;
pub mod print;
pub mod pure_nan;
pub mod reader;
pub mod string;
pub mod structure;
pub mod symbol;
pub mod tuple;
pub mod value;
pub mod values;

pub(crate) fn init() {
    base::init_base();
    number::init_number();
    fun::init();
    tuple::init_tuple();
    cmp::init_cmp();
    list::init_list();
    error::init_error();
    structure::initialize_struct();
    string::init_string();
    macros::init_macros();
    portfun::init_ports();
    arithfun::init_arith();
    cont::init_cont();
    crate::vm::stacktrace::init_stacktrace();
    date::init_date();
    // load necessary files
    let t = Thread::current();
    match scm_require(
        make_string(t, "capy").into(),
        0,
        scm_reqbase_module().module(),
    )
    {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Failed to load standard library;");
            eprint!("Error:");
            if is_struct_instance(EXN_TABLE[Exception::Exn as usize].typ, err) {
                let msg = struct_ref(err, 0);
                eprintln!("{}", msg);
            } else {
                eprintln!("{}", err);
            }

            std::process::exit(1);
        }
    }
    TRAMPOLINE_INSTALLED.store(true, std::sync::atomic::Ordering::Release);
    /*scm_require(
        make_string(t, "capy/list").into(),
        0,
        scm_reqbase_module().module(),
    )
    .expect("failed to load capy/list");
    // macros
    scm_require(
        make_string(t, "capy/macros").into(),
        0,
        scm_reqbase_module().module(),
    )
    .expect("failed to load capy/macros");
    // parameter
    scm_require(
        make_string(t, "capy/parameter").into(),
        0,
        scm_reqbase_module().module(),
    )
    .expect("failed to load capy/parameter");
    // dynwind
    scm_require(
        make_string(t, "capy/dynwind").into(),
        0,
        scm_reqbase_module().module(),
    )
    .expect("failed to load capy/dynwind");
    TRAMPOLINE_INSTALLED.store(true, std::sync::atomic::Ordering::Release);
    // quasiquote
    scm_require(
        make_string(t, "capy/quasiquote").into(),
        0,
        scm_reqbase_module().module(),
    )
    .expect("failed to load quasiquote");*/
}
