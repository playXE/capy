use once_cell::sync::OnceCell;
use rsgc::heap::heap;
use rsgc::heap::root_processor::SimpleRoot;
use rsgc::prelude::Object;
use rsgc::sync::mutex::Mutex;
use rsgc::thread::Thread;

use crate::raise_exn;
use crate::runtime::reader::Reader;
use crate::vm::callframe::CallFrame;
use crate::vm::scm_vm;

use super::fun::scm_make_subr;
use super::module::{scm_capy_module, scm_define};
use super::number::scm_nonnegative_exact_integer;
use super::port::*;
use super::string::make_string;
use super::symbol::Intern;
use super::vector::{make_bytevector, make_bytevector_from_slice};
use super::{error::*, object::*, value::*};

macro_rules! check_opened_port {
    ($port: expr, $name: expr, $which: expr, $args: expr) => {
        if !$port.opened {
            $port.lock.unlock();
            return wrong_contract::<()>(
                $name,
                "(not port-closed?)",
                $which,
                $args.len() as _,
                $args,
            )
            .into();
        }
    };
}

macro_rules! check_opened_input_port {
    ($port: expr, $name: expr, $which: expr, $args: expr) => {
        if !$port.opened || ($port.direction & SCM_PORT_DIRECTION_IN) == 0 {
            $port.lock.unlock();
            return wrong_contract::<()>(
                $name,
                "(and/c (not port-closed?) input-port?)",
                $which,
                $args.len() as _,
                $args,
            )
            .into();
        }
    };
}
#[allow(unused_macros)]
macro_rules! check_opened_output_port {
    ($port: expr, $name: expr, $which: expr, $args: expr) => {
        if !$port.opened || ($port.direction & SCM_PORT_DIRECTION_OUT) == 0 {
            $port.lock.unlock();
            return wrong_contract::<()>(
                $name,
                "(and/c (not port-closed?) output-port?)",
                $which,
                $args.len() as _,
                $args,
            )
            .into();
        }
    };
}

macro_rules! check_opened_input_textual_port {
    ($port: expr, $name: expr, $which: expr, $args: expr) => {
        if !$port.opened
            || ($port.direction & SCM_PORT_DIRECTION_IN) == 0
            || !port_textual_pred($port)
        {
            $port.lock.unlock();
            return wrong_contract::<()>(
                $name,
                "(and/c (not port-closed?) input-port? textual-port?)",
                $which,
                $args.len() as _,
                $args,
            )
            .into();
        }
    };
}

macro_rules! check_opened_input_binary_port {
    ($port: expr, $name: expr, $which: expr, $args: expr) => {
        if !$port.opened
            || ($port.direction & SCM_PORT_DIRECTION_IN) == 0
            || !port_binary_pred($port)
        {
            $port.lock.unlock();
            return wrong_contract::<()>(
                $name,
                "(and/c (not port-closed?) input-port? textual-port?)",
                $which,
                $args.len() as _,
                $args,
            )
            .into();
        }
    };
}

macro_rules! check_opened_output_binary_port {
    ($port: expr, $name: expr, $which: expr, $args: expr) => {
        if !$port.opened
            || ($port.direction & SCM_PORT_DIRECTION_OUT) == 0
            || !port_binary_pred($port)
        {
            $port.lock.unlock();
            return wrong_contract::<()>(
                $name,
                "(and/c (not port-closed?) output-port? binary-port?)",
                $which,
                $args.len() as _,
                $args,
            )
            .into();
        }
    };
}

macro_rules! check_opened_output_textual_port {
    ($port: expr, $name: expr, $which: expr, $args: expr) => {
        if !$port.opened
            || ($port.direction & SCM_PORT_DIRECTION_OUT) == 0
            || !port_textual_pred($port)
        {
            $port.lock.unlock();
            return wrong_contract::<()>(
                $name,
                "(and/c (not port-closed?) output-port? textual-port?)",
                $which,
                $args.len() as _,
                $args,
            )
            .into();
        }
    };
}

extern "C" fn set_port_current_line(cfr: &mut CallFrame) -> ScmResult {
    if !cfr.argument(0).is_port() || !cfr.argument(0).is_input_port() {
        return wrong_contract::<()>(
            "set-port-current-line!",
            "input-port?",
            0,
            2,
            cfr.arguments(),
        )
        .into();
    }

    if !cfr.argument(1).is_exact_integer() {
        return wrong_contract::<()>(
            "set-port-current-line!",
            "exact-integer?",
            1,
            2,
            cfr.arguments(),
        )
        .into();
    }

    let mut port = cfr.argument(0).port();

    if cfr.argument(1).is_int32() {
        port.lock.lock(true);
        port.line = cfr.argument(1).get_int32();
        port.lock.unlock();
        ScmResult::ok(Value::encode_undefined_value())
    } else {
        raise_exn!(
            (),
            Fail,
            &[],
            "set-port-current-line!: line number too large"
        )
        .into()
    }
}

extern "C" fn set_port_current_column(cfr: &mut CallFrame) -> ScmResult {
    if !cfr.argument(0).is_port() || !cfr.argument(0).is_input_port() {
        return wrong_contract::<()>(
            "set-port-current-column!",
            "input-port?",
            0,
            2,
            cfr.arguments(),
        )
        .into();
    }

    if !cfr.argument(1).is_exact_integer() {
        return wrong_contract::<()>(
            "set-port-current-column!",
            "exact-integer?",
            1,
            2,
            cfr.arguments(),
        )
        .into();
    }

    let mut port = cfr.argument(0).port();

    if cfr.argument(1).is_int32() {
        port.lock.lock(true);
        port.column = cfr.argument(1).get_int32();
        port.lock.unlock();
        ScmResult::ok(Value::encode_undefined_value())
    } else {
        raise_exn!(
            (),
            Fail,
            &[],
            "set-port-current-column!: column number too large"
        )
        .into()
    }
}

extern "C" fn port_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(cfr.argument(0).is_port())
}

extern "C" fn input_port_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(cfr.argument(0).is_input_port())
}

extern "C" fn output_port_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(cfr.argument(0).is_output_port())
}

extern "C" fn port_closed_p(cfr: &mut CallFrame) -> ScmResult {
    if !cfr.argument(0).is_port() {
        return wrong_contract::<()>("port-closed?", "port?", 0, 1, cfr.arguments()).into();
    }

    let port = cfr.argument(0).port();
    port.lock.lock(true);
    let res = port_open_pred(port);
    port.lock.unlock();

    ScmResult::ok(Value::encode_bool_value(!res))
}

extern "C" fn output_port_buffer_mode(cfr: &mut CallFrame) -> ScmResult {
    if !cfr.argument(0).is_port() {
        return wrong_contract::<()>("output-port-buffer-mode", "port?", 0, 1, cfr.arguments())
            .into();
    }

    let port = cfr.argument(0).port();

    port.lock.lock(true);
    let res = port_output_buffer_mode(port);
    port.lock.unlock();

    match res {
        SCM_PORT_BUFFER_MODE_NONE => ScmResult::ok("none".intern()),
        SCM_PORT_BUFFER_MODE_LINE => ScmResult::ok("line".intern()),
        SCM_PORT_BUFFER_MODE_BLOCK => ScmResult::ok("block".intern()),
        _ => unreachable!(),
    }
}

extern "C" fn flush_output_port(cfr: &mut CallFrame) -> ScmResult {
    if !cfr.argument(0).is_port() {
        return wrong_contract::<()>("flush-output-port", "port?", 0, 1, cfr.arguments()).into();
    }

    let port = cfr.argument(0).port();

    port.lock.lock(true);
    let res = port_flush_output(port);
    port.lock.unlock();

    res.into()
}

extern "C" fn close_port(cfr: &mut CallFrame) -> ScmResult {
    if !cfr.argument(0).is_port() {
        return wrong_contract::<()>("close-port", "port?", 0, 1, cfr.arguments()).into();
    }

    let port = cfr.argument(0).port();

    port.lock.lock(true);
    let res = port_close(port);
    port.lock.unlock();

    res.into()
}

extern "C" fn eof_object(_: &mut CallFrame) -> ScmResult {
    ScmResult::ok(Value::eof_object())
}

extern "C" fn eof_object_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(cfr.argument(0).is_eof_object())
}

static CURREN_INPUT_PORT: OnceCell<Mutex<Value>> = OnceCell::new();
static CURREN_OUTPUT_PORT: OnceCell<Mutex<Value>> = OnceCell::new();
static CURREN_ERROR_PORT: OnceCell<Mutex<Value>> = OnceCell::new();

fn init_std(fd: i32, name: &str, dir: u8) -> Result<Value, Value> {
    let t = Thread::current();
    let mut port = Port::new(t);

    port_open_std(
        port,
        fd,
        make_string(t, name).into(),
        dir,
        0,
        SCM_PORT_BUFFER_MODE_BLOCK,
        true.into(),
    );
    port.force_sync = true;
    port.mark = std_port_position(fd)?;
    Ok(port.into())
}

pub fn get_current_input_port() -> Result<Value, Value> {
    Ok(*CURREN_INPUT_PORT.get_or_try_init(|| -> Result<Mutex<Value>, Value> {
        let port = init_std(0, "/dev/stdin", SCM_PORT_DIRECTION_IN)?;
        Ok(Mutex::new(port))
    })?.lock(true))
}

pub fn get_current_output_port() -> Result<Value, Value> {
    Ok(*CURREN_OUTPUT_PORT.get_or_try_init(|| -> Result<Mutex<Value>, Value> {
        let port = init_std(1, "/dev/stdout", SCM_PORT_DIRECTION_OUT)?;
        Ok(Mutex::new(port))
    })?.lock(true))
}

pub fn get_current_error_port() -> Result<Value, Value> {
    Ok(*CURREN_ERROR_PORT.get_or_try_init(|| -> Result<Mutex<Value>, Value> {
        let port = init_std(2, "/dev/stderr", SCM_PORT_DIRECTION_OUT)?;
        Ok(Mutex::new(port))
    })?.lock(true))
}

extern "C" fn current_input_port(cfr: &mut CallFrame) -> ScmResult {
    let port = CURREN_INPUT_PORT.get_or_try_init(|| -> Result<Mutex<Value>, Value> {
        let port = init_std(0, "/dev/stdin", SCM_PORT_DIRECTION_IN)?;
        Ok(Mutex::new(port))
    })?;

    let mut port_locked = port.lock(true);
    if cfr.argument_count() > 0 {
        if !cfr.argument(0).is_input_port() {
            return wrong_contract::<()>(
                "current-input-port",
                "input-port?",
                0,
                1,
                cfr.arguments(),
            )
            .into();
        }

        *port_locked = cfr.argument(0);
    }

    ScmResult::ok(*port_locked)
}

extern "C" fn current_output_port(cfr: &mut CallFrame) -> ScmResult {
    let port = CURREN_OUTPUT_PORT.get_or_try_init(|| -> Result<Mutex<Value>, Value> {
        let port = init_std(1, "/dev/stdout", SCM_PORT_DIRECTION_OUT)?;
        Ok(Mutex::new(port))
    })?;

    let mut port_locked = port.lock(true);
    if cfr.argument_count() > 0 {
        if !cfr.argument(0).is_output_port() {
            return wrong_contract::<()>(
                "current-output-port",
                "output-port?",
                0,
                1,
                cfr.arguments(),
            )
            .into();
        }

        *port_locked = cfr.argument(0);
    }

    ScmResult::ok(*port_locked)
}

extern "C" fn current_error_port(cfr: &mut CallFrame) -> ScmResult {
    let port = CURREN_ERROR_PORT.get_or_try_init(|| -> Result<Mutex<Value>, Value> {
        let port = init_std(2, "/dev/stderr", SCM_PORT_DIRECTION_OUT)?;
        Ok(Mutex::new(port))
    })?;

    let mut port_locked = port.lock(true);
    if cfr.argument_count() > 0 {
        if !cfr.argument(0).is_output_port() {
            return wrong_contract::<()>(
                "current-error-port",
                "output-port?",
                0,
                1,
                cfr.arguments(),
            )
            .into();
        }

        *port_locked = cfr.argument(0);
    }

    ScmResult::ok(*port_locked)
}

extern "C" fn standard_input_port(_cfr: &mut CallFrame) -> ScmResult {
    let port = init_std(0, "/dev/stdin", SCM_PORT_DIRECTION_IN)?;
    ScmResult::ok(port)
}

extern "C" fn standard_output_port(_cfr: &mut CallFrame) -> ScmResult {
    let port = init_std(1, "/dev/stdout", SCM_PORT_DIRECTION_OUT)?;
    ScmResult::ok(port)
}

extern "C" fn standard_error_port(_cfr: &mut CallFrame) -> ScmResult {
    let port = init_std(2, "/dev/stderr", SCM_PORT_DIRECTION_OUT)?;
    ScmResult::ok(port)
}

static NATIVE_TRANSCODER: OnceCell<Value> = OnceCell::new();

extern "C" fn native_transcoder_descriptor(_: &mut CallFrame) -> ScmResult {
    ScmResult::ok(*NATIVE_TRANSCODER.get_or_init(|| {
        let t = Thread::current();
        make_bytevector_from_slice(
            t,
            &[
                SCM_PORT_CODEC_NATIVE,
                SCM_PORT_EOL_STYLE_NATIVE,
                SCM_PORT_ERROR_HANDLING_MODE_REPLACE,
            ],
        )
        .into()
    }))
}

extern "C" fn port_transcoder_descriptor(cfr: &mut CallFrame) -> ScmResult {
    if !cfr.argument(0).is_port() {
        return wrong_contract::<()>("port-transcoder-descriptor", "port?", 0, 1, cfr.arguments())
            .into();
    }

    ScmResult::ok(cfr.argument(0).port().transcoder)
}

extern "C" fn port_device_subtype(cfr: &mut CallFrame) -> ScmResult {
    if !cfr.argument(0).is_port() {
        return wrong_contract::<()>("port-device-subtype", "port?", 0, 1, cfr.arguments()).into();
    }

    match cfr.argument(0).port().subtype {
        SCM_PORT_SUBTYPE_NONE => ScmResult::ok("none".intern()),
        SCM_PORT_SUBTYPE_CHAR_SPECIAL => ScmResult::ok("char-special".intern()),
        SCM_PORT_SUBTYPE_FIFO => ScmResult::ok("fifo".intern()),
        _ => unreachable!(),
    }
}

extern "C" fn extract_accumulated_bytevector(cfr: &mut CallFrame) -> ScmResult {
    if !cfr.argument(0).is_port() {
        return wrong_contract::<()>(
            "extract-accumulated-bytevector",
            "port?",
            0,
            1,
            cfr.arguments(),
        )
        .into();
    }

    let port = cfr.argument(0).port();

    port.lock.lock(true);
    check_opened_port!(port, "extract-accumulated-bytevector", 0, cfr.arguments());

    if port_bytevector_pred(port) && port_output_pred(port) {
        let bv = port_extract_bytevector(port);
        port.lock.unlock();
        ScmResult::ok(bv)
    } else {
        port.lock.unlock();
        wrong_contract::<()>(
            "extract-accumulated-bytevector",
            "(and/c output-port? (or/c bytevector-port? string-port?))",
            0,
            1,
            cfr.arguments(),
        )
        .into()
    }
}

extern "C" fn get_accumulated_bytevector(cfr: &mut CallFrame) -> ScmResult {
    if !cfr.argument(0).is_port() {
        return wrong_contract::<()>("get-accumulated-bytevector", "port?", 0, 1, cfr.arguments())
            .into();
    }

    let port = cfr.argument(0).port();

    port.lock.lock(true);
    check_opened_port!(port, "get-accumulated-bytevector", 0, cfr.arguments());

    if port_bytevector_pred(port) && port_output_pred(port) {
        let bv = port_get_bytevector(port);
        port.lock.unlock();
        ScmResult::ok(bv)
    } else {
        port.lock.unlock();
        wrong_contract::<()>(
            "get-accumulated-bytevector",
            "(and/c output-port? (or/c bytevector-port? string-port?))",
            0,
            1,
            cfr.arguments(),
        )
        .into()
    }
}

extern "C" fn extract_accumulated_string(cfr: &mut CallFrame) -> ScmResult {
    if !cfr.argument(0).is_port() {
        return wrong_contract::<()>("extract-accumulated-string", "port?", 0, 1, cfr.arguments())
            .into();
    }

    let port = cfr.argument(0).port();

    port.lock.lock(true);
    check_opened_port!(port, "extract-accumulated-string", 0, cfr.arguments());

    if port_bytevector_pred(port) && port_output_pred(port) {
        let bv = port_extract_string(port);
        port.lock.unlock();
        bv.into()
    } else {
        port.lock.unlock();
        wrong_contract::<()>(
            "extract-accumulated-string",
            "(and/c output-port? (or/c bytevector-port? string-port?))",
            0,
            1,
            cfr.arguments(),
        )
        .into()
    }
}

extern "C" fn get_accumulated_string(cfr: &mut CallFrame) -> ScmResult {
    if !cfr.argument(0).is_port() {
        return wrong_contract::<()>("get-accumulated-string", "port?", 0, 1, cfr.arguments())
            .into();
    }

    let port = cfr.argument(0).port();

    port.lock.lock(true);
    check_opened_port!(port, "get-accumulated-string", 0, cfr.arguments());

    if port_bytevector_pred(port) && port_output_pred(port) {
        let bv = port_get_string(port);
        port.lock.unlock();
        bv.into()
    } else {
        port.lock.unlock();
        wrong_contract::<()>(
            "get-accumulated-string",
            "(and/c output-port? (or/c bytevector-port? string-port?))",
            0,
            1,
            cfr.arguments(),
        )
        .into()
    }
}

extern "C" fn make_string_output_port(_cfr: &mut CallFrame) -> ScmResult {
    let t = Thread::current();
    let transcoder = make_bytevector_from_slice(
        t,
        &[
            SCM_PORT_CODEC_UTF8,
            SCM_PORT_EOL_STYLE_NONE,
            SCM_PORT_ERROR_HANDLING_MODE_IGNORE,
        ],
    );

    let port = Port::new(t);
    port_open_bytevector(
        port,
        "string".intern().into(),
        SCM_PORT_DIRECTION_OUT,
        false.into(),
        transcoder.into(),
    );

    ScmResult::ok(port)
}

extern "C" fn make_string_input_port(cfr: &mut CallFrame) -> ScmResult {
    if !cfr.argument(0).is_string() {
        return wrong_contract::<()>("make-string-input-port", "string?", 0, 1, cfr.arguments())
            .into();
    }

    let t = Thread::current();
    let transcoder = make_bytevector_from_slice(
        t,
        &[
            SCM_PORT_CODEC_UTF8,
            SCM_PORT_EOL_STYLE_NONE,
            SCM_PORT_ERROR_HANDLING_MODE_IGNORE,
        ],
    );

    let bvector = make_bytevector_from_slice(t, cfr.argument(0).string().as_bytes());

    let port = Port::new(t);
    port_open_bytevector(
        port,
        "string".intern().into(),
        SCM_PORT_DIRECTION_IN,
        bvector.into(),
        transcoder.into(),
    );

    ScmResult::ok(port)
}

extern "C" fn open_port(cfr: &mut CallFrame) -> ScmResult {
    let typ;
    let args = cfr.arguments();
    let vm = scm_vm();
    if args[0].is_int32() {
        match args[1].get_int32() {
            1 => typ = SCM_PORT_TYPE_NAMED_FILE,
            2 => typ = SCM_PORT_TYPE_BYTEVECTOR,
            3 => typ = SCM_PORT_TYPE_CUSTOM,

            _ => {
                return wrong_contract::<()>("open-port", "(or/c 0 1 2)", 0, args.len() as _, args)
                    .into()
            }
        }
    } else {
        return wrong_contract::<()>("open-port", "smallint?", 0, args.len() as _, args).into();
    }

    let direction;

    if args[1].is_int32() {
        match args[2].get_int32() {
            1 => direction = SCM_PORT_DIRECTION_IN,
            2 => direction = SCM_PORT_DIRECTION_OUT,
            3 => direction = SCM_PORT_DIRECTION_BOTH,
            _ => {
                return wrong_contract::<()>("open-port", "(or/c 0 1 2)", 1, args.len() as _, args)
                    .into()
            }
        }
    } else {
        return wrong_contract::<()>("open-port", "smallint?", 1, args.len() as _, args).into();
    }

    let name = args[2];

    if !name.is_string() || !name.is_symbol() {
        return wrong_contract::<()>(
            "open-port",
            "(or/c string? symbol?)",
            2,
            args.len() as _,
            args,
        )
        .into();
    }

    match typ {
        SCM_PORT_TYPE_NAMED_FILE => {
            if name.is_string() {
                let file_options = if args[3].is_int32() {
                    args[3].get_int32() as u8
                } else if args[3].is_false() {
                    SCM_PORT_FILE_OPTION_NONE as u8
                } else {
                    return wrong_contract::<()>(
                        "open-port",
                        "(or/c smallint? #f)",
                        3,
                        args.len() as _,
                        args,
                    )
                    .into();
                };

                if (file_options
                    & !(SCM_PORT_FILE_OPTION_NONE
                        | SCM_PORT_FILE_OPTION_NO_CREATE
                        | SCM_PORT_FILE_OPTION_NO_FAIL
                        | SCM_PORT_FILE_OPTION_NO_FAIL))
                    != 0
                {
                    return wrong_contract::<()>(
                        "open-port",
                        "valid file options",
                        3,
                        args.len() as _,
                        args,
                    )
                    .into();
                }

                let buffer_mode = if args[4].is_int32() {
                    args[4].get_int32()
                } else if args[4].is_false() {
                    SCM_PORT_BUFFER_MODE_NONE as i32
                } else {
                    return wrong_contract::<()>(
                        "open-port",
                        "(or/c smallint? #f)",
                        4,
                        args.len() as _,
                        args,
                    )
                    .into();
                };

                let buffer_mode = match buffer_mode {
                    1 => SCM_PORT_BUFFER_MODE_NONE,
                    2 => SCM_PORT_BUFFER_MODE_LINE,
                    3 => SCM_PORT_BUFFER_MODE_BLOCK,
                    _ => {
                        return wrong_contract::<()>(
                            "open-port",
                            "(or/c 1 2 3)",
                            4,
                            args.len() as _,
                            args,
                        )
                        .into()
                    }
                };

                let transcoder =
                    if (args[5].is_true() || args[5].is_false()) || args[5].is_bytevector() {
                        args[5]
                    } else {
                        return wrong_contract::<()>(
                            "open-port",
                            "(or/c bytevector? #t #f)",
                            5,
                            args.len() as _,
                            args,
                        )
                        .into();
                    };

                let port = Port::new(vm.mutator());

                port_open_file(port, name, direction, file_options, buffer_mode, transcoder)?;

                ScmResult::ok(port)
            } else {
                return wrong_contract::<()>("open-port", "string?", 2, args.len() as _, args)
                    .into();
            }
        }

        SCM_PORT_TYPE_BYTEVECTOR => {
            if name.is_symbol() {
                let bytes;

                if args[3].is_bytevector() {
                    if direction == SCM_PORT_DIRECTION_IN {
                        bytes = args[3];
                    } else {
                        return wrong_contract::<()>(
                            "open-port",
                            "(or/c bytevector? #f)",
                            3,
                            args.len() as _,
                            args,
                        )
                        .into();
                    }
                } else if args[3].is_false() {
                    if direction == SCM_PORT_DIRECTION_OUT {
                        bytes = make_bytevector(vm.mutator(), 0).into()
                    } else {
                        return wrong_contract::<()>(
                            "open-port",
                            "(or/c bytevector? #f)",
                            3,
                            args.len() as _,
                            args,
                        )
                        .into();
                    }
                } else {
                    return wrong_contract::<()>(
                        "open-port",
                        "(or/c bytevector? #f)",
                        3,
                        args.len() as _,
                        args,
                    )
                    .into();
                }

                if !args[4].is_false() {
                    return wrong_contract::<()>("open-port", "#f", 4, args.len() as _, args)
                        .into();
                }

                let transcoder =
                    if (args[5].is_true() || args[5].is_false()) || args[5].is_bytevector() {
                        args[5]
                    } else {
                        return wrong_contract::<()>(
                            "open-port",
                            "(or/c bytevector? #t #f)",
                            5,
                            args.len() as _,
                            args,
                        )
                        .into();
                    };

                let port = Port::new(vm.mutator());

                port_open_bytevector(port, name, direction, bytes, transcoder);

                ScmResult::ok(port)
            } else {
                return wrong_contract::<()>("open-port", "symbol?", 2, args.len() as _, args)
                    .into();
            }
        }

        SCM_PORT_TYPE_CUSTOM => {
            if name.is_string() {
                let handlers = if args[3].is_vector() {
                    if args[3].vector_len() != 5 {
                        return wrong_contract::<()>(
                            "open-port",
                            "vector of length 5",
                            3,
                            args.len() as _,
                            args,
                        )
                        .into();
                    } else {
                        args[4]
                    }
                } else {
                    return wrong_contract::<()>("open-port", "vector?", 3, args.len() as _, args)
                        .into();
                };

                if !args[4].is_false() {
                    return wrong_contract::<()>("open-port", "#f", 4, args.len() as _, args)
                        .into();
                }

                let transcoder = if args[5].is_false() || args[5].is_true() {
                    args[5]
                } else {
                    return wrong_contract::<()>(
                        "open-port",
                        "(or/c #t #f)",
                        5,
                        args.len() as _,
                        args,
                    )
                    .into();
                };

                let port = Port::new(vm.mutator());

                port_make_custom_port(port, name, direction, handlers, transcoder);

                ScmResult::ok(port)
            } else {
                return wrong_contract::<()>("open-port", "string?", 2, args.len() as _, args)
                    .into();
            }
        }
        _ => todo!(),
    }
}

extern "C" fn make_file_input_port(cfr: &mut CallFrame) -> ScmResult {
    let args = cfr.arguments();
    let vm = scm_vm();
    if args[0].is_string() {
        let port = Port::new(vm.mutator());

        match port_open_file(
            port,
            args[0],
            SCM_PORT_DIRECTION_IN,
            0,
            SCM_PORT_BUFFER_MODE_BLOCK,
            true.into(),
        ) {
            Ok(_) => {}
            Err(e) => {
                return ScmResult::err(e);
            }
        }

        ScmResult::ok(port)
    } else {
        return wrong_contract::<()>("make-file-input-port", "string?", 0, args.len() as _, args)
            .into();
    }
}

extern "C" fn make_file_output_port(cfr: &mut CallFrame) -> ScmResult {
    let args = cfr.arguments();
    let vm = scm_vm();
    if args[0].is_string() {
        let port = Port::new(vm.mutator());

        match port_open_file(
            port,
            args[0],
            SCM_PORT_DIRECTION_OUT,
            SCM_PORT_FILE_OPTION_NO_FAIL,
            SCM_PORT_BUFFER_MODE_BLOCK,
            true.into(),
        ) {
            Ok(_) => {}
            Err(e) => {
                return ScmResult::err(e);
            }
        }

        ScmResult::ok(port)
    } else {
        return wrong_contract::<()>("make-file-output-port", "string?", 0, args.len() as _, args)
            .into();
    }
}

extern "C" fn make_temporary_file_port(cfr: &mut CallFrame) -> ScmResult {
    let args = cfr.arguments();

    if args[0].is_string() {
        if args[1].is_bytevector() || args[1].is_false() || args[1].is_true() {
            let transcoder = args[1];

            if transcoder.is_bytevector() && transcoder.bytevector_len() != 3 {
                return wrong_contract::<()>(
                    "make-temporary-file-port",
                    "bytevector of length 3",
                    1,
                    args.len() as _,
                    args,
                )
                .into();
            }

            let port = Port::new(scm_vm().mutator());

            port_open_temp_file(port, args[0], SCM_PORT_BUFFER_MODE_BLOCK, transcoder)?;

            ScmResult::ok(port)
        } else {
            return wrong_contract::<()>(
                "make-temporary-file-port",
                "(or/c bytevector? #f #t)",
                1,
                args.len() as _,
                args,
            )
            .into();
        }
    } else {
        return wrong_contract::<()>(
            "make-temporary-file-port",
            "string?",
            0,
            args.len() as _,
            args,
        )
        .into();
    }
}

extern "C" fn nonblock_byte_ready_p(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument(0).is_port() {
        let port = cfr.argument(0).port();

        port.lock.lock(true);
        check_opened_input_port!(port, "nonblock-byte-ready?", 0, cfr.arguments());
        let rv = port_nonblock_byte_ready(port);
        port.lock.unlock();

        rv.into()
    } else {
        return wrong_contract::<()>(
            "nonblock-byte-ready?",
            "input-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    }
}

extern "C" fn get_char(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument(0).is_port() {
        let port = cfr.argument(0).port();

        port.lock.lock(true);
        check_opened_input_port!(port, "get-char", 0, cfr.arguments());
        let rv = port_get_char(port);
        port.lock.unlock();

        rv.into()
    } else {
        return wrong_contract::<()>(
            "get-char",
            "input-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    }
}

extern "C" fn lookahead_char(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument(0).is_port() {
        let port = cfr.argument(0).port();

        port.lock.lock(true);
        check_opened_input_port!(port, "lookahead-char", 0, cfr.arguments());
        let rv = port_lookahead_char(port);
        port.lock.unlock();

        rv.into()
    } else {
        return wrong_contract::<()>(
            "lookahead-char",
            "input-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    }
}



extern "C" fn port_position_proc(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument(0).is_port() {
        let port = cfr.argument(0).port();

        port.lock.lock(true);

        check_opened_port!(port, "port-position", 0, cfr.arguments());

        if port_has_port_position_pred(port) {
            let rv = port_position(port);
            port.lock.unlock();

            ScmResult::ok(scm_int(rv?))
        } else {
            port.lock.unlock();

            return wrong_contract::<()>(
                "port-position",
                "port-has-port-position?",
                0,
                cfr.arguments().len() as _,
                cfr.arguments(),
            )
            .into();
        }
    } else {
        return wrong_contract::<()>(
            "port-position",
            "port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    }
}

extern "C" fn port_has_port_position(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument(0).is_port() {
        let port = cfr.argument(0).port();

        port.lock.lock(true);

        check_opened_port!(port, "port-has-port-position?", 0, cfr.arguments());

        let rv = port_has_port_position_pred(port);
        port.lock.unlock();

        ScmResult::ok(rv)
    } else {
        return wrong_contract::<()>(
            "port-has-port-position?",
            "port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    }
}

extern "C" fn port_has_set_port_position(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument(0).is_port() {
        let port = cfr.argument(0).port();

        port.lock.lock(true);

        check_opened_port!(port, "port-has-set-port-position?", 0, cfr.arguments());

        let rv = port_has_set_port_position_pred(port);
        port.lock.unlock();

        ScmResult::ok(rv)
    } else {
        return wrong_contract::<()>(
            "port-has-set-port-position?",
            "port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    }
}

extern "C" fn set_port_position(cfr: &mut CallFrame) -> ScmResult {
    if !cfr.argument(1).is_exact_integer() {
        return wrong_contract::<()>(
            "set-port-position!",
            "exact-integer?",
            1,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    }
    if cfr.argument(0).is_port() {
        let port = cfr.argument(0).port();
        let Some(pos) = cfr.argument(1).get_int64() else {
            return raise_exn!(
                (),
                FailFilesystem,
                &[],
                "set-port-position!: position too large: {}",
                cfr.argument(1)
            )
            .into();
        };

        port.lock.lock(true);

        check_opened_port!(port, "set-port-position", 0, cfr.arguments());

        if port_has_set_port_position_pred(port) {
            let rv = port_set_port_position(port, pos);
            port.lock.unlock();
            rv?;
            ScmResult::ok(Value::encode_undefined_value())
        } else {
            port.lock.unlock();

            return wrong_contract::<()>(
                "set-port-position",
                "port-has-set-port-position?",
                0,
                cfr.arguments().len() as _,
                cfr.arguments(),
            )
            .into();
        }
    } else {
        return wrong_contract::<()>(
            "set-port-position",
            "(and/c port? integer?)",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    }
}

extern "C" fn port_eof_p(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument(0).is_port() {
        let port = cfr.argument(0).port();

        port.lock.lock(true);

        check_opened_input_port!(port, "port-eof?", 0, cfr.arguments());

        let rv = port_eof(port);
        port.lock.unlock();

        rv.into()
    } else {
        return wrong_contract::<()>(
            "port-eof?",
            "port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    }
}

extern "C" fn get_u8(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument(0).is_port() {
        let port = cfr.argument(0).port();

        port.lock.lock(true);

        check_opened_input_port!(port, "get-u8", 0, cfr.arguments());

        let rv = port_get_u8(port);
        port.lock.unlock();

        rv.into()
    } else {
        return wrong_contract::<()>(
            "get-u8",
            "input-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    }
}

extern "C" fn lookahead_u8(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument(0).is_port() {
        let port = cfr.argument(0).port();

        port.lock.lock(true);

        check_opened_input_port!(port, "lookahead-u8", 0, cfr.arguments());

        let rv = port_lookahead_u8(port);
        port.lock.unlock();

        rv.into()
    } else {
        return wrong_contract::<()>(
            "lookahead-u8",
            "input-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    }
}

extern "C" fn get_byte(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument(0).is_port() {
        let port = cfr.argument(0).port();

        port.lock.lock(true);

        check_opened_input_port!(port, "get-byte", 0, cfr.arguments());

        let rv = port_get_byte(port);
        port.lock.unlock();

        rv.map(|x| Value::encode_int32(x)).into()
    } else {
        return wrong_contract::<()>(
            "get-byte",
            "input-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    }
}

extern "C" fn lookahead_byte(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument(0).is_port() {
        let port = cfr.argument(0).port();

        port.lock.lock(true);

        check_opened_input_port!(port, "lookahead-byte", 0, cfr.arguments());

        let rv = port_lookahead_byte(port);
        port.lock.unlock();

        rv.map(|x| Value::encode_int32(x)).into()
    } else {
        return wrong_contract::<()>(
            "lookahead-byte",
            "input-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    }
}

extern "C" fn get_bytevector_n(cfr: &mut CallFrame) -> ScmResult {
    let args = cfr.arguments();

    if args[0].is_port() {
        let count = if scm_nonnegative_exact_integer(args[1]) && args[1].is_int32() {
            args[1].get_int32()
        } else {
            return wrong_contract::<()>(
                "get-bytevector-n",
                "(and/c fixnum? nonnegative-exact-integer?)",
                1,
                args.len() as _,
                args,
            )
            .into();
        };

        let port = args[0].port();
        port.lock.lock(true);

        check_opened_input_binary_port!(port, "get-bytevector-n", 0, args);

        let mut bvector = make_bytevector(Thread::current(), count as usize);

        if count == 0 {
            port.lock.unlock();
            return ScmResult::ok(bvector);
        }

        let n = port_get_bytes(port, &mut bvector);

        match n {
            Ok(n) => {
                if n == 0 {
                    port.lock.unlock();
                    return ScmResult::ok(Value::eof_object());
                } else if n == count as i32 {
                    port.lock.unlock();
                    return ScmResult::ok(bvector);
                } else {
                    let bvector = make_bytevector_from_slice(Thread::current(), &bvector);
                    port.lock.unlock();
                    return ScmResult::ok(bvector);
                }
            }

            Err(e) => {
                port.lock.unlock();
                return ScmResult::err(e);
            }
        }
    } else {
        return wrong_contract::<()>(
            "get-bytevector-n",
            "input-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    }
}

extern "C" fn get_bytevector_n_destructing(cfr: &mut CallFrame) -> ScmResult {
    // port, bvector, start, count
    let args = cfr.arguments();

    let port = if args[0].is_port() {
        args[0].port()
    } else {
        return wrong_contract::<()>("get-bytevector-n!", "input-port?", 0, args.len() as _, args)
            .into();
    };

    let mut bvector = if args[1].is_bytevector() {
        args[1].bytevector()
    } else {
        return wrong_contract::<()>("get-bytevector-n!", "bytevector?", 1, args.len() as _, args)
            .into();
    };

    let start = if scm_nonnegative_exact_integer(args[2]) && args[2].is_int32() {
        args[2].get_int32() as usize
    } else {
        return wrong_contract::<()>(
            "get-bytevector-n!",
            "(and/c fixnum? nonnegative-exact-integer?)",
            2,
            args.len() as _,
            args,
        )
        .into();
    };

    let count = if scm_nonnegative_exact_integer(args[3]) && args[3].is_int32() {
        args[3].get_int32() as usize
    } else {
        return wrong_contract::<()>(
            "get-bytevector-n!",
            "(and/c fixnum? nonnegative-exact-integer?)",
            3,
            args.len() as _,
            args,
        )
        .into();
    };

    if count == 0 {
        return ScmResult::ok(Value::encode_int32(0));
    }

    port.lock.lock(true);

    if start + count < args[1].bytevector_len() {
        let n = port_get_bytes(port, &mut bvector[start..start + count]);

        match n {
            Ok(n) => {
                if n == 0 {
                    port.lock.unlock();
                    return ScmResult::ok(Value::eof_object());
                } else {
                    port.lock.unlock();
                    return ScmResult::ok(Value::encode_int32(n));
                }
            }

            Err(e) => {
                port.lock.unlock();
                return ScmResult::err(e);
            }
        }
    } else {
        port.lock.unlock();

        if start >= args[1].bytevector_len() {
            return out_of_range::<()>(
                "get-bytevector-n!",
                Some("bytevector"),
                "start",
                args[2],
                args[1],
                0,
                args[1].bytevector_len() as _,
            )
            .into();
        } else {
            return out_of_range::<()>(
                "get-bytevector-n!",
                Some("bytevector"),
                "count",
                args[3],
                args[1],
                0,
                args[1].bytevector_len() as _,
            )
            .into();
        }
    }
}

extern "C" fn get_bytevector_some(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument(0).is_port() {
        let port = cfr.argument(0).port();

        port.lock.lock(true);

        check_opened_input_binary_port!(port, "get-bytevector-some", 0, cfr.arguments());

        let c = port_lookahead_byte(port).or_else(|e| {
            port.lock.unlock();
            Err(e)
        })?;

        if c == libc::EOF {
            port.lock.unlock();
            return ScmResult::ok(Value::eof_object());
        }

        let n = port_buffered_byte_count(port);
        let mut bvector = make_bytevector(Thread::current(), n as usize);

        for i in 0..n as usize {
            bvector[i] = port_get_byte(port).map(|x| x as u8).or_else(|e| {
                port.lock.unlock();
                Err(e)
            })?;
        }

        port.lock.unlock();

        return ScmResult::ok(bvector);
    } else {
        return wrong_contract::<()>(
            "get-bytevector-some",
            "input-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    }
}

extern "C" fn get_bytevector_all(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument(0).is_port() {
        let port = cfr.argument(0).port();

        port.lock.lock(true);
        check_opened_input_binary_port!(port, "get-bytevector-all", 0, cfr.arguments());
        let output = Port::new(scm_vm().mutator());

        port_open_bytevector(
            port,
            "bytevector".intern().into(),
            SCM_PORT_DIRECTION_OUT,
            false.into(),
            false.into(),
        );

        {
            let mut buf = vec![0u8; SCM_PORT_BLOCK_BUFFER_SIZE];

            loop {
                let n = port_get_bytes(port, &mut buf).or_else(|e| {
                    port.lock.unlock();
                    port_discard_buffer(output);
                    Err(e)
                })?;

                if n == 0 {
                    if port_position(port).map_err(|e| {
                        port.lock.unlock();
                        port_discard_buffer(output);
                        e
                    })? == 0
                    {
                        port.lock.unlock();
                        port_discard_buffer(output);
                        return ScmResult::ok(Value::eof_object());
                    }

                    let bv = port_extract_bytevector(port);

                    port_discard_buffer(output);
                    port.lock.unlock();

                    return ScmResult::ok(bv);
                }

                port_put_bytes(output, &buf[..n as usize]).or_else(|e| {
                    port.lock.unlock();
                    port_discard_buffer(output);
                    Err(e)
                })?;
            }
        }
    } else {
        return wrong_contract::<()>(
            "get-bytevector-all",
            "input-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    }
}

extern "C" fn get_string_n(cfr: &mut CallFrame) -> ScmResult {
    let port = if cfr.argument(0).is_port() {
        cfr.argument(0).port()
    } else {
        return wrong_contract::<()>(
            "get-string-n",
            "input-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    let n = if scm_nonnegative_exact_integer(cfr.argument(1)) && cfr.argument(1).is_int32() {
        cfr.argument(1).get_int32() as usize
    } else {
        return wrong_contract::<()>(
            "get-string-n",
            "(and/c fixnum? nonnegative-exact-integer?)",
            1,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    port.lock.lock(true);

    check_opened_input_textual_port!(port, "get-string-n", 0, cfr.arguments());

    let mut s = String::with_capacity(n);

    for i in 0..n {
        let c = port_get_char(port).map_err(|e| {
            port.lock.unlock();
            e
        })?;

        if c.is_eof_object() {
            port.lock.unlock();
            if i == 0 {
                return ScmResult::ok(Value::eof_object());
            }

            return ScmResult::ok(make_string(Thread::current(), s));
        }

        s.push(c.get_char());
    }

    port.lock.unlock();

    return ScmResult::ok(make_string(Thread::current(), s));
}

extern "C" fn get_string_n_destructing(cfr: &mut CallFrame) -> ScmResult {
    // port, string, start, count

    let port = if cfr.argument(0).is_port() {
        cfr.argument(0).port()
    } else {
        return wrong_contract::<()>(
            "get-string-n/destructive",
            "input-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    let mut s = if cfr.argument(1).is_string() {
        cfr.argument(1).string()
    } else {
        return wrong_contract::<()>(
            "get-string-n/destructive",
            "string?",
            1,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    let start = if scm_nonnegative_exact_integer(cfr.argument(2)) && cfr.argument(2).is_int32() {
        cfr.argument(2).get_int32() as usize
    } else {
        return wrong_contract::<()>(
            "get-string-n/destructive",
            "(and/c fixnum? nonnegative-exact-integer?)",
            2,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    let count = if scm_nonnegative_exact_integer(cfr.argument(3)) && cfr.argument(3).is_int32() {
        cfr.argument(3).get_int32() as usize
    } else {
        return wrong_contract::<()>(
            "get-string-n/destructive",
            "(and/c fixnum? nonnegative-exact-integer?)",
            3,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    let length = s.chars().count();

    if start + count < length {
        for i in 0..count {
            let c = port_get_char(port).map_err(|e| {
                port.lock.unlock();
                e
            })?;

            if c.is_eof_object() {
                if i == 0 {
                    port.lock.unlock();
                    return ScmResult::ok(Value::eof_object());
                } else {
                    port.lock.unlock();
                    return ScmResult::ok(Value::encode_int32(i as _));
                }
            }

            let pos = s
                .char_indices()
                .nth(start + i)
                .map(|(pos, ch)| (pos..pos + ch.len_utf8()))
                .unwrap();
            s.replace_range(pos, &c.get_char().to_string());
        }

        port.lock.unlock();

        return ScmResult::ok(Value::encode_int32(count as _));
    } else {
        port.lock.unlock();
        let args = cfr.arguments();
        if start >= length {
            return out_of_range::<()>(
                "get-bytevector-n!",
                Some("bytevector"),
                "start",
                args[2],
                args[1],
                0,
                args[1].bytevector_len() as _,
            )
            .into();
        } else {
            return out_of_range::<()>(
                "get-bytevector-n!",
                Some("bytevector"),
                "count",
                args[3],
                args[1],
                0,
                args[1].bytevector_len() as _,
            )
            .into();
        }
    }
}

extern "C" fn get_string_all(cfr: &mut CallFrame) -> ScmResult {
    let port = if cfr.argument(0).is_port() {
        cfr.argument(0).port()
    } else {
        return wrong_contract::<()>(
            "get-string-all",
            "input-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    port.lock.lock(true);

    check_opened_input_textual_port!(port, "get-string-all", 0, cfr.arguments());

    let mut s = String::new();

    loop {
        let c = port_get_char(port).map_err(|e| {
            port.lock.unlock();
            e
        })?;

        if c.is_eof_object() {
            port.lock.unlock();
            if s.is_empty() {
                return ScmResult::ok(Value::eof_object());
            }

            return ScmResult::ok(make_string(Thread::current(), s));
        }

        s.push(c.get_char());
    }
}

extern "C" fn get_line(cfr: &mut CallFrame) -> ScmResult {
    let port = if cfr.argument(0).is_port() {
        cfr.argument(0).port()
    } else {
        return wrong_contract::<()>(
            "get-line",
            "input-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    port.lock.lock(true);

    check_opened_input_textual_port!(port, "get-line", 0, cfr.arguments());

    let mut s = String::new();

    loop {
        let c = port_get_char(port).map_err(|e| {
            port.lock.unlock();
            e
        })?;

        if c.is_eof_object() {
            port.lock.unlock();
            if s.is_empty() {
                return ScmResult::ok(Value::eof_object());
            }

            return ScmResult::ok(make_string(Thread::current(), s));
        }

        if c.get_char() == '\n' {
            port.lock.unlock();
            return ScmResult::ok(make_string(Thread::current(), s));
        }

        s.push(c.get_char());
    }
}

extern "C" fn put_u8(cfr: &mut CallFrame) -> ScmResult {
    let port = if cfr.argument(0).is_port() {
        cfr.argument(0).port()
    } else {
        return wrong_contract::<()>(
            "put-u8",
            "output-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    let byte = if scm_nonnegative_exact_integer(cfr.argument(1)) && cfr.argument(1).is_int32() {
        cfr.argument(1).get_int32() as u8
    } else {
        return wrong_contract::<()>(
            "put-u8",
            "(and/c fixnum? nonnegative-exact-integer?)",
            1,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    port.lock.lock(true);

    check_opened_output_binary_port!(port, "put-u8", 0, cfr.arguments());

    port_put_byte(port, byte).map_err(|e| {
        port.lock.unlock();
        e
    })?;

    port.lock.unlock();

    return ScmResult::ok(Value::encode_undefined_value());
}

extern "C" fn put_byte(cfr: &mut CallFrame) -> ScmResult {
    let port = if cfr.argument(0).is_port() {
        cfr.argument(0).port()
    } else {
        return wrong_contract::<()>(
            "put-byte",
            "output-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    let byte = if scm_nonnegative_exact_integer(cfr.argument(1)) && cfr.argument(1).is_int32() {
        cfr.argument(1).get_int32() as u8
    } else {
        return wrong_contract::<()>(
            "put-byte",
            "(and/c fixnum? nonnegative-exact-integer?)",
            1,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    port.lock.lock(true);

    check_opened_output_binary_port!(port, "put-byte", 0, cfr.arguments());

    port_put_byte(port, byte).map_err(|e| {
        port.lock.unlock();
        e
    })?;

    port.lock.unlock();

    return ScmResult::ok(Value::encode_undefined_value());
}

extern "C" fn put_bytevector(cfr: &mut CallFrame) -> ScmResult {
    // port, bvector, start, count
    let port = if cfr.argument(0).is_port() {
        cfr.argument(0).port()
    } else {
        return wrong_contract::<()>(
            "put-byte",
            "output-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    let bytes = if cfr.argument(1).is_bytevector() {
        cfr.argument(1).bytevector()
    } else {
        return wrong_contract::<()>(
            "put-byte",
            "bytevector?",
            1,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    let start = if scm_nonnegative_exact_integer(cfr.argument(2)) && cfr.argument(2).is_int32() {
        cfr.argument(2).get_int32() as usize
    } else {
        return wrong_contract::<()>(
            "put-byte",
            "(and/c fixnum? nonnegative-exact-integer?)",
            2,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    let count = if scm_nonnegative_exact_integer(cfr.argument(3)) && cfr.argument(3).is_int32() {
        cfr.argument(3).get_int32() as usize
    } else {
        return wrong_contract::<()>(
            "put-byte",
            "(and/c fixnum? nonnegative-exact-integer?)",
            3,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    if start + count > bytes.len() {
        return wrong_contract::<()>(
            "put-byte",
            "start + count <= (bytevector-length bvector)",
            2,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    }

    port.lock.lock(true);

    check_opened_output_binary_port!(port, "put-byte", 0, cfr.arguments());

    port_put_bytes(port, &bytes[start..start + count]).map_err(|e| {
        port.lock.unlock();
        e
    })?;
    if port.force_sync {
        port_flush_output(port).map_err(|e| {
            port.lock.unlock();
            e
        })?;
    }
    port.lock.unlock();

    return ScmResult::ok(Value::encode_undefined_value());
}

extern "C" fn put_char(cfr: &mut CallFrame) -> ScmResult {
    let port = if cfr.argument(0).is_port() {
        cfr.argument(0).port()
    } else {
        return wrong_contract::<()>(
            "put-char",
            "output-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    let c = if cfr.argument(1).is_char() {
        cfr.argument(1).get_char()
    } else {
        return wrong_contract::<()>(
            "put-char",
            "char?",
            1,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    port.lock.lock(true);

    check_opened_output_textual_port!(port, "put-char", 0, cfr.arguments());

    port_put_char(port, c).map_err(|e| {
        port.lock.unlock();
        e
    })?;
    if port.force_sync {
        port_flush_output(port).map_err(|e| {
            port.lock.unlock();
            e
        })?;
    }
    port.lock.unlock();

    return ScmResult::ok(Value::encode_undefined_value());
}

extern "C" fn put_string(cfr: &mut CallFrame) -> ScmResult {
    let port = if cfr.argument(0).is_port() {
        cfr.argument(0).port()
    } else {
        return wrong_contract::<()>(
            "put-string",
            "output-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    let s = if cfr.argument(1).is_string() {
        cfr.argument(1).string()
    } else {
        return wrong_contract::<()>(
            "put-string",
            "string?",
            1,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();
    };

    let start = if cfr.argument_count() > 2 {
        if scm_nonnegative_exact_integer(cfr.argument(2)) && cfr.argument(2).is_int32() {
            cfr.argument(2).get_int32() as usize
        } else {
            return wrong_contract::<()>(
                "put-string",
                "(and/c fixnum? nonnegative-exact-integer?)",
                2,
                cfr.arguments().len() as _,
                cfr.arguments(),
            )
            .into();
        }
    } else {
        0
    };

    let count = if cfr.argument_count() > 3 {
        if scm_nonnegative_exact_integer(cfr.argument(3)) && cfr.argument(3).is_int32() {
            cfr.argument(3).get_int32() as usize
        } else {
            return wrong_contract::<()>(
                "put-string",
                "(and/c fixnum? nonnegative-exact-integer?)",
                3,
                cfr.arguments().len() as _,
                cfr.arguments(),
            )
            .into();
        }
    } else {
        s.chars().count()
    };

    port.lock.lock(true);

    check_opened_output_textual_port!(port, "put-string", 0, cfr.arguments());
    let orig = s.chars().count();
    if start + count > orig {
        port.lock.unlock();
        return out_of_range::<()>(
            "put-string",
            Some("string"),
            "start + count",
            cfr.argument(3),
            cfr.argument(2),
            0,
            orig as _,
        )
        .into();
    }

    if start == 0 && count == orig {
        port_put_string(port, s).map_err(|e| {
            port.lock.unlock();
            e
        })?;
    } else {
        for c in s.chars().skip(start).take(count) {
            port_put_char(port, c).map_err(|e| {
                port.lock.unlock();
                e
            })?;
        }
    }
    
    if port.force_sync {
        port_flush_output(port).map_err(|e| {
            port.lock.unlock();
            e
        })?;
    }

    port.lock.unlock();

    return ScmResult::ok(Value::encode_undefined_value());
}


extern "C" fn read(cfr: &mut CallFrame) -> ScmResult {
    let port = if cfr.argument_count() > 0 {
        cfr.argument(0)
    } else {
        get_current_input_port()?
    };

    if !port.is_port() {
        return wrong_contract::<()>(
            "read",
            "input-port?",
            0,
            cfr.arguments().len() as _,
            cfr.arguments(),
        )
        .into();    
    }

    let port = port.port();
    port.lock.lock(true);

    check_opened_input_textual_port!(port, "read", 0, cfr.arguments());

    let mut reader = Reader::new(scm_vm(), port, false);

    reader.read().and_then(|val| {
        port.lock.unlock();
        Ok(val)
    }).map_err(|e| {
        port.lock.unlock();
        e
    }).into()
}


pub(crate) fn init_ports() {
    heap::heap().add_root(SimpleRoot::new("portfun", "ports", |proc| {
        let visitor = proc.visitor();

        CURREN_INPUT_PORT.get().and_then(|p| unsafe {
            p.unsafe_get().trace(visitor);
            Some(())
        });
        CURREN_OUTPUT_PORT.get().and_then(|p| unsafe {
            p.unsafe_get().trace(visitor);
            Some(())
        });
        CURREN_ERROR_PORT.get().and_then(|p| unsafe {
            p.unsafe_get().trace(visitor);
            Some(())
        });

        NATIVE_TRANSCODER.get().and_then(|p| {
            p.trace(visitor);
            Some(())
        });
    }));
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
        set_port_current_line, "set-port-current-line!", 2, 2
        set_port_current_column, "set-port-current-column!", 2, 2
        port_p, "port?", 1, 1
        input_port_p, "input-port?", 1, 1
        output_port_p, "output-port?", 1, 1
        port_closed_p, "port-closed?", 1, 1
        output_port_buffer_mode, "output-port-buffer-mode", 1, 1
        flush_output_port, "flush-output-port", 1, 1
        close_port, "close-port", 1, 1
        eof_object, "eof-object", 0, 0
        eof_object_p, "eof-object?", 1, 1
        current_input_port, "current-input-port", 0, 1
        current_output_port, "current-output-port", 0, 1
        current_error_port, "current-error-port", 0, 1
        standard_input_port, "standard-input-port", 0, 0
        standard_output_port, "standard-output-port", 0, 0
        standard_error_port, "standard-error-port", 0, 0
        native_transcoder_descriptor, "native-transcoder-descriptor", 0, 0
        port_transcoder_descriptor, "port-transcoder-descritor", 1, 1
        port_device_subtype, "port-device-subtype", 1, 1
        extract_accumulated_bytevector, "extract-accumulated-bytevector", 1, 1
        extract_accumulated_string, "extract-accumulated-string", 1, 1
        get_accumulated_bytevector, "get-accumulated-bytevector", 1, 1
        get_accumulated_string, "get-accumulated-string", 1, 1
        make_string_output_port, "make-string-output-port", 0, 1
        make_string_input_port, "make-string-input-port", 1, 1
        open_port, "open-port", 6, 6
        make_file_input_port, "make-file-input-port", 1, 1
        make_file_output_port, "make-file-output-port", 1, 1
        make_temporary_file_port, "make-temporary-file-port", 0, 0
        nonblock_byte_ready_p, "nonblock-byte-ready?", 1, 1
        get_char, "get-char", 1, 1
        lookahead_char, "lookahead-char", 1, 1
        port_position_proc, "port-position", 1, 1
        port_has_port_position, "port-has-position?", 1, 1
        port_has_set_port_position, "port-has-set-position?", 1, 1
        set_port_position, "port-set-position!", 2, 2
        port_eof_p, "port-eof?", 1, 1
        get_u8, "get-u8", 1, 1
        lookahead_u8, "lookahead-u8", 1, 1
        get_byte, "get-byte", 1, 1
        lookahead_byte, "lookahead-byte", 1, 1
        get_bytevector_n, "get-bytevector-n", 2, 2
        get_bytevector_n_destructing, "get-bytevector-n!", 4, 4
        get_bytevector_some, "get-bytevector-some", 1,1
        get_bytevector_all, "get-bytevector-all", 1, 1
        get_string_n, "get-string-n", 2, 2
        get_string_n_destructing, "get-string-n!", 2, 4
        get_string_all, "get-string-all", 1, 1
        get_line, "get-line", 1, 1
        put_u8, "put-u8", 2, 2
        put_byte, "put-byte", 2, 2
        put_bytevector, "put-bytevector", 4, 4
        put_char, "put-char", 2, 2
        put_string, "put-string", 2, 4
        read, "read", 0, 1

    }
}