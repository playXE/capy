use self::{
    error::{Exception, EXN_TABLE},
    load::scm_require,
    module::scm_reqbase_module,
    string::make_string,
    structure::{is_struct_instance, struct_ref},
};
use crate::vm::interpreter::TRAMPOLINE_INSTALLED;
use rsgc::{thread::Thread, heap::heap::heap};

#[macro_use]
pub mod list;
#[macro_use]
pub mod vector;
pub mod arith;
pub mod arithfun;
pub mod base;
pub mod bigint;
pub mod dload;
pub mod bytevector;
pub mod cmp;
pub mod complex;
pub mod file;
pub mod cont;
pub mod date;
pub mod error;
pub mod foreign;
pub mod fun;
pub mod gc;
pub mod io;
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
pub mod sync;
pub mod tuple;
pub mod value;
pub mod values;
pub mod bitwise;

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
    file::init_file();
    portfun::init_ports();
    sync::init_sync();
    arithfun::init_arith();
    bitwise::init_bitwise();
    cont::init_cont();
    crate::vm::stacktrace::init_stacktrace();
    date::init_date();
    foreign::init_foreign();
    gc::init_gc();
    bytevector::init_bytevector();
    dload::init_dload();
    //crate::vm::jit::baseline::init_baseline();
    // load necessary files
    let t = Thread::current();
    let start = std::time::Instant::now();
   
    match scm_require(
        make_string(t, "capy").into(),
        0,
        scm_reqbase_module().module(),
    ) {
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
    println!("Loaded standard library in {:?}", start.elapsed());
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
