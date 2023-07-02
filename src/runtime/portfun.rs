use once_cell::sync::OnceCell;
use rsgc::sync::mutex::Mutex;
use rsgc::thread::Thread;

use crate::raise_exn;
use crate::vm::callframe::CallFrame;
use crate::vm::scm_vm;

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

macro_rules! check_opened_output_port {
    ($port: expr, $name: expr, $which: expr, $args: expr) => {
        if !$port.opened || (port.direction & SCM_PORT_DIRECTION_OUT) == 0 {
            port.lock.unlock();
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
    if args[1].is_int32() {
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
