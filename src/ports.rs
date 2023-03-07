use once_cell::sync::{Lazy, OnceCell};
use rsgc::{sync::mutex::Mutex, thread::Thread};

use crate::{
    error::{wrong_contract, wrong_count, out_of_range},
    ports_v2::{
        *
    },
    raise_exn,
    value::Value,
    vm::{intern, Runtime, Trampoline},
};

define_proc! {
    extern "set-port-current-line!", set_port_current_line(_vm, args) 2, 2 => {
        if !args[0].portp() || !args[0].input_portp() {
            return wrong_contract::<()>("set-port-current-line!", "input-port?", 0, 2, args).into();
        }

        if !args[1].exact_integerp() {
            return wrong_contract::<()>("set-port-current-line!", "exact-integer?", 1, 2, args).into();
        }

        let mut port = args[0].downcast_port();

        if args[1].intp() {
            port.lock.lock(true);
            port.line = args[1].int();
            port.lock.unlock();
        } else {
            return raise_exn!(Value,
                FailContract,
                &[],
                "set-port-current-line!: line too large: {}",
                args[1]
            ).into();
        }

        crate::vm::Trampoline::Return(Value::make_void())
    }
}

define_proc! {
    extern "set-port-current-column!", set_port_current_column(_vm, args) 2, 2 => {
        if !args[0].portp() || !args[0].input_portp() {
            return wrong_contract::<()>("set-port-current-line!", "input-port?", 0, 2, args).into();
        }

        if !args[1].exact_integerp() {
            return wrong_contract::<()>("set-port-current-line!", "exact-integer?", 1, 2, args).into();
        }

        let mut port = args[0].downcast_port();

        if args[1].intp() {
            port.lock.lock(true);
            port.column = args[1].int();
            port.lock.unlock();
        } else {
            return raise_exn!(Value,
                FailContract,
                &[],
                "set-port-current-line!: line too large: {}",
                args[1]
            ).into()
        }

        crate::vm::Trampoline::Return(Value::make_void())
    }
}

define_proc! {
    extern "port?", is_port(_vm, args) 1, 1 => {
        crate::vm::Trampoline::Return(args[0].portp().into())
    }
}

define_proc! {
    extern "inpurt-port?", is_input_port(_vm, args) 1, 1 => {
        crate::vm::Trampoline::Return(args[0].input_portp().into())
    }
}

define_proc! {
    extern "output-port?", is_output_port(_vm, args) 1, 1 => {
        crate::vm::Trampoline::Return(args[0].output_portp().into())
    }
}

define_proc! {
    extern "port-closed?", is_port_closed(_vm, args) 1, 1 => {
        if !args[0].portp() {
            return wrong_contract::<()>("port-closed?", "port?", 0, 1, args).into();
        }

        let port = args[0].downcast_port();

        port.lock.lock(true);
        let res = port_open_pred(port);
        port.lock.unlock();

        crate::vm::Trampoline::Return((!res).into())
    }
}

define_proc! {
    extern "output-port-buffer-mode", output_port_buffer_mode(_vm, args) 1,1 => {
        if !args[0].portp() {
            return wrong_contract::<()>("output-port-buffer-mode", "port?", 0, 1, args).into();
        }

        let port = args[0].downcast_port();

        port.lock.lock(true);
        let res = port_output_buffer_mode(port);
        port.lock.unlock();
        match res {
            SCM_PORT_BUFFER_MODE_NONE => crate::vm::Trampoline::Return(intern("none")),
            SCM_PORT_BUFFER_MODE_BLOCK => crate::vm::Trampoline::Return(intern("block")),
            SCM_PORT_BUFFER_MODE_LINE => crate::vm::Trampoline::Return(intern("line")),
            _ => unreachable!()
        }
    }
}

define_proc! {
    extern "flush-output-port", flush_output_port(_vm, args) 1, 1 => {
        if !args[0].portp() {
            return wrong_contract::<()>("flush-output-port", "port?", 0, 1, args).into();
        }

        let port = args[0].downcast_port();

        port.lock.lock(true);
        let res = crate::ports_v2::port_flush_output(port);
        port.lock.unlock();

        res.map(|_| Value::make_void()).into()
    }
}

define_proc! {
    extern "close-port", close_port(_vm, args) 1, 1 => {
        if !args[0].portp() {
            return wrong_contract::<()>("close-port", "port?", 0, 1, args).into();
        }

        let port = args[0].downcast_port();

        port.lock.lock(true);
        let res = crate::ports_v2::port_close(port);
        port.lock.unlock();

        res.map(|_| Value::make_void()).into()
    }
}

define_proc! {
    extern "eof-object", eof_object(_vm, _args) 0, 0 => {
        crate::vm::Trampoline::Return(Value::make_eof())
    }
}

define_proc! {
    extern "eof-object?", is_eof_object(_vm, args) 1, 1 => {
        crate::vm::Trampoline::Return(args[0].eofp().into())
    }
}

static CURREN_INPUT_PORT: OnceCell<Mutex<Value>> = OnceCell::new();
static CURREN_OUTPUT_PORT: OnceCell<Mutex<Value>> = OnceCell::new();
static CURREN_ERROR_PORT: OnceCell<Mutex<Value>> = OnceCell::new();

fn init_std(fd: i32, name: &str, dir: u8) -> Result<Value, Value> {
    let mut port = Port::new(Thread::current());

    port_open_std(
        port,
        fd,
        Value::make_str(Thread::current(), name),
        dir,
        0,
        0,
        Value::make_false(),
    );
    port.mark = std_port_position(fd)?;
    unsafe { Ok(Value::encode_ptr(port.as_ptr())) }
}

define_proc! {
    extern "current-input-port", current_input_port(_vm, args) 0, 1 => {
        let port = CURREN_INPUT_PORT.get_or_try_init(|| -> Result<Mutex<Value>, Value> {
            let port = init_std(0, "/dev/stdin", SCM_PORT_DIRECTION_IN)?;
            Ok(Mutex::new(port))
        }).unwrap();
        let mut port_locked = port.lock(true);
        if args.len() > 0 {
            if !args[0].input_portp() {
                return wrong_contract::<()>("current-input-port", "input-port?", 0, 1, args).into();
            }

            *port_locked = args[0];
        }
        crate::vm::Trampoline::Return(*port_locked)
    }
}

define_proc! {
    extern "current-output-port", current_output_port(_vm, args) 0, 1 => {
        let port = CURREN_OUTPUT_PORT.get_or_try_init(|| -> Result<Mutex<Value>, Value> {
            let port = init_std(1, "/dev/stdout", SCM_PORT_DIRECTION_OUT)?;
            Ok(Mutex::new(port))
        }).unwrap();
        let mut port_locked = port.lock(true);
        if args.len() > 0 {
            if !args[0].output_portp() {
                return wrong_contract::<()>("current-output-port", "output-port?", 0, 1, args).into();
            }

            *port_locked = args[0];
        }
        crate::vm::Trampoline::Return(*port_locked)
    }
}

define_proc! {
    extern "current-error-port", current_error_port(_vm, args) 0, 1 => {
        let port = CURREN_ERROR_PORT.get_or_try_init(|| -> Result<Mutex<Value>, Value> {
            let port = init_std(2, "/dev/stderr", SCM_PORT_DIRECTION_OUT)?;
            Ok(Mutex::new(port))
        }).unwrap();
        let mut port_locked = port.lock(true);
        if args.len() > 0 {
            if !args[0].output_portp() {
                return wrong_contract::<()>("current-error-port", "output-port?", 0, 1, args).into();
            }

            *port_locked = args[0];
        }
        crate::vm::Trampoline::Return(*port_locked)
    }
}

define_proc! {
    extern "standard-input-port", standard_input_port(_vm, _args) 0, 0 => {
        let port = match init_std(0, "/dev/stdin", SCM_PORT_DIRECTION_OUT) {
            Ok(port) => port,
            Err(err) => return crate::vm::Trampoline::Throw(err)
        };
        crate::vm::Trampoline::Return(port)
    }
}

define_proc! {
    extern "standard-output-port", standard_output_port(_vm, _args) 0, 0 => {
        let port = match init_std(1, "/dev/stdout", SCM_PORT_DIRECTION_OUT) {
            Ok(port) => port,
            Err(err) => return crate::vm::Trampoline::Throw(err)
        };
        crate::vm::Trampoline::Return(port)
    }
}

define_proc! {
    extern "standard-error-port", standard_error_port(_vm, _args) 0, 0 => {
        let port = match init_std(2, "/dev/stderr", SCM_PORT_DIRECTION_OUT) {
            Ok(port) => port,
            Err(err) => return crate::vm::Trampoline::Throw(err)
        };
        crate::vm::Trampoline::Return(port)
    }
}

define_proc! {
    extern "native-transcoder-descriptor", native_transcoder_descriptor(_vm, _args) 0, 0 => {
        crate::vm::Trampoline::Return(Runtime::get().native_transcoder())
    }
}

define_proc! {
    extern "port-transcoder-descriptor", port_transcoder_descriptor(_vm, args) 1,1 => {
        if !args[0].portp() {
            return wrong_contract::<()>("port-transcoder-descriptor", "port?", 0, 1, args).into();
        }

        Trampoline::Return(args[0].downcast_port().transcoder)
    }
}

define_proc! {
    extern "port-device-subtype", port_device_subtype(_vm, args) 1,1 => {
        if !args[0].portp() {
            return wrong_contract::<()>("port-device-subtype", "port?", 0, 1, args).into();
        }

        Trampoline::Return(match args[0].downcast_port().subtype {
            SCM_PORT_SUBTYPE_NONE => intern("none"),
            SCM_PORT_SUBTYPE_CHAR_SPECIAL => intern("char-special"),
            SCM_PORT_SUBTYPE_FIFO => intern("fifo"),
            _ => unreachable!()
        })
    }
}

macro_rules! check_opened_port {
    ($port: expr, $name: expr, $which: expr, $args: expr) => {
        if !$port.opened {
            $port.lock.unlock();
            return wrong_contract::<()>($name, "(not port-closed?)", $which, $args.len() as _, $args)
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
        if !$port.opened || ($port.direction & SCM_PORT_DIRECTION_IN) == 0 || !port_textual_pred($port) {
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
        if !$port.opened || ($port.direction & SCM_PORT_DIRECTION_IN) == 0 || !port_binary_pred($port) {
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


macro_rules! check_opened_output_textual_port {
    ($port: expr, $name: expr, $which: expr, $args: expr) => {
        if !$port.opened || ($port.direction & SCM_PORT_DIRECTION_OUT) == 0 || !port_textual_pred($port) {
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

define_proc! {
    extern "extract-accumulated-bytevector", extract_accumulated_bytevector(_vm, args) 1,1 => {
        if !args[0].portp() {
            return wrong_contract::<()>("extract-accumulated-bytevector", "port?", 0, 1, args).into();
        }

        let port = args[0].downcast_port();

        port.lock.lock(true);

        check_opened_port!(port, "extract-accumulated-bytevector", 0, args);

        if port_bytevector_pred(port) && port_output_pred(port) {
            let bv = port_extract_bytevector(port);
            port.lock.unlock();
            return Trampoline::Return(bv);
        } else {
            port.lock.unlock();
            wrong_contract::<()>("extract-accumulated-bytevector", "(and/c output-port? (or/c bytevector-port? string-port?))", 0, 1, args).into()
        }

    }
}

define_proc! {
    extern "get-accumulated-bytevector", get_accumulated_bytevector(_vm, args) 1,1 => {
        if !args[0].portp() {
            return wrong_contract::<()>("get-accumulated-bytevector", "port?", 0, 1, args).into();
        }

        let port = args[0].downcast_port();

        port.lock.lock(true);

        check_opened_port!(port, "get-accumulated-bytevector", 0, args);

        if port_bytevector_pred(port) && port_output_pred(port) {
            let bv = port_get_bytevector(port);
            port.lock.unlock();
            return Trampoline::Return(bv);
        } else {
            port.lock.unlock();
            wrong_contract::<()>("get-accumulated-bytevector", "(and/c output-port? (or/c bytevector-port? string-port?))", 0, 1, args).into()
        }

    }
}

define_proc! {
    extern "extract-accumulated-string", extract_accumulated_string(_vm, args) 1,1 => {
        if !args[0].portp() {
            return wrong_contract::<()>("extract-accumulated-string", "port?", 0, 1, args).into();
        }

        let port = args[0].downcast_port();

        port.lock.lock(true);

        check_opened_port!(port, "extract-accumulated-string", 0, args);

        if port_bytevector_pred(port) && port_output_pred(port) {
            let bv = port_extract_string(port);
            port.lock.unlock();
            return bv.into();
        } else {
            port.lock.unlock();
            wrong_contract::<()>("extract-accumulated-string", "(and/c output-port? (or/c bytevector-port? string-port?))", 0, 1, args).into()
        }

    }
}

define_proc! {
    extern "get-accumulated-string", get_accumulated_string(_vm, args) 1,1 => {
        if !args[0].portp() {
            return wrong_contract::<()>("get-accumulated-string", "port?", 0, 1, args).into();
        }

        let port = args[0].downcast_port();

        port.lock.lock(true);

        check_opened_port!(port, "get-accumulated-string", 0, args);

        if port_bytevector_pred(port) && port_output_pred(port) {
            let bv = port_get_string(port);
            port.lock.unlock();
            return bv.into();
        } else {
            port.lock.unlock();
            wrong_contract::<()>("get-accumulated-string", "(and/c output-port? (or/c bytevector-port? string-port?))", 0, 1, args).into()
        }

    }
}

define_proc! {
    extern "make-string-output-port", make_string_output_port(vm, _args) 0, 0 => {
        let transcoder = Value::make_byte_vector(vm.mutator(), 3, 0);
        transcoder.byte_vector_set(0, SCM_PORT_CODEC_UTF8);
        transcoder.byte_vector_set(1, SCM_PORT_EOL_STYLE_NONE);
        transcoder.byte_vector_set(2, SCM_PORT_ERROR_HANDLING_MODE_IGNORE);

        let port = Port::new(vm.mutator());
        port_open_bytevector(port, intern("string"), SCM_PORT_DIRECTION_OUT, Value::make_false(), transcoder);

        Trampoline::Return(unsafe {
            Value::encode_ptr(port.as_ptr())
        })
    }
}

define_proc! {
    extern "make-string-input-port", make_string_input_port(vm, args) 1, 1 => {
        if !args[0].strp() {
            return wrong_contract::<()>("make-string-input-port", "string?", 0, 1, args).into();
        }

        let transcoder = Value::make_byte_vector(vm.mutator(), 3, 0);
        transcoder.byte_vector_set(0, SCM_PORT_CODEC_UTF8);
        transcoder.byte_vector_set(1, SCM_PORT_EOL_STYLE_NONE);
        transcoder.byte_vector_set(2, SCM_PORT_ERROR_HANDLING_MODE_IGNORE);

        let bvector = Value::make_byte_vector_from(vm.mutator(), args[0].str());

        let port = Port::new(vm.mutator());
        port_open_bytevector(port, intern("string"), SCM_PORT_DIRECTION_IN, bvector, transcoder);

        Trampoline::Return(unsafe {
            Value::encode_ptr(port.as_ptr())
        })
    }
}

define_proc! {
    extern "open-port", open_port(vm, args) 5, 5 => {
        let typ;

        if args[1].intp() {
            match args[1].int() {
                1 => typ = SCM_PORT_TYPE_NAMED_FILE,
                2 => typ = SCM_PORT_TYPE_BYTEVECTOR,
                3 => typ = SCM_PORT_TYPE_CUSTOM,

                _ => return wrong_contract::<()>("open-port", "(or/c 0 1 2)", 0, args.len() as _, args).into(),
            }
        } else {
            return wrong_contract::<()>("open-port", "smallint?", 0, args.len() as _, args).into();
        }

        let direction;

        if args[1].intp() {
            match args[2].int() {
                1 => direction = SCM_PORT_DIRECTION_IN,
                2 => direction = SCM_PORT_DIRECTION_OUT,
                3 => direction = SCM_PORT_DIRECTION_BOTH,
                _ => return wrong_contract::<()>("open-port", "(or/c 0 1 2)", 1, args.len() as _, args).into(),
            }
        } else {
            return wrong_contract::<()>("open-port", "smallint?", 1, args.len() as _, args).into();
        }

        let name = args[2];

        if !name.strp() || !name.symbolp() {
            return wrong_contract::<()>("open-port", "(or/c string? symbol?)", 2, args.len() as _, args).into();
        }

        match typ {
            SCM_PORT_TYPE_NAMED_FILE => {
                if name.strp() {
                    let file_options = if args[3].intp() {
                        args[3].int() as u8
                    } else if args[3].falsep() {
                        SCM_PORT_FILE_OPTION_NONE as u8 
                    } else {
                        return wrong_contract::<()>("open-port", "(or/c smallint? #f)", 3, args.len() as _, args).into();
                    };

                    if (file_options & !(SCM_PORT_FILE_OPTION_NONE | SCM_PORT_FILE_OPTION_NO_CREATE | SCM_PORT_FILE_OPTION_NO_FAIL | SCM_PORT_FILE_OPTION_NO_FAIL)) != 0 {
                        return wrong_contract::<()>("open-port", "valid file options", 3, args.len() as _, args).into();
                    }

                    let buffer_mode = if args[4].intp() {
                        args[4].int()
                    } else if args[4].falsep() {
                        SCM_PORT_BUFFER_MODE_NONE as i32
                    } else {
                        return wrong_contract::<()>("open-port", "(or/c smallint? #f)", 4, args.len() as _, args).into();
                    };

                    let buffer_mode = match buffer_mode {
                        1 => SCM_PORT_BUFFER_MODE_NONE,
                        2 => SCM_PORT_BUFFER_MODE_LINE,
                        3 => SCM_PORT_BUFFER_MODE_BLOCK,
                        _ => return wrong_contract::<()>("open-port", "(or/c 1 2 3)", 4, args.len() as _, args).into(),
                    };

                    let transcoder = if (args[5].truep() || args[5].falsep()) || args[5].byte_vectorp() {
                        args[5]
                    } else {
                        return wrong_contract::<()>("open-port", "(or/c bytevector? #t #f)", 5, args.len() as _, args).into();
                    };

                    let port = Port::new(vm.mutator());

                    port_open_file(port, name, direction, file_options, buffer_mode, transcoder)?;

                    Trampoline::Return(unsafe {
                        Value::encode_ptr(port.as_ptr())
                    })
                } else {
                    return wrong_contract::<()>("open-port", "string?", 2, args.len() as _, args).into();
                }
            }

                SCM_PORT_TYPE_BYTEVECTOR => {
                    if name.symbolp() {
                        let bytes;

                        if args[3].byte_vectorp() {
                            if direction == SCM_PORT_DIRECTION_IN {
                                bytes = args[3];
                            } else {
                                return wrong_contract::<()>("open-port", "(or/c bytevector? #f)", 3, args.len() as _, args).into();
                            }
                        } else if args[3].falsep() {
                            if direction == SCM_PORT_DIRECTION_OUT {
                                bytes = Value::make_byte_vector(vm.mutator(), 0, 0);
                            } else {
                                return wrong_contract::<()>("open-port", "(or/c bytevector? #f)", 3, args.len() as _, args).into();
                            }
                        } else {
                            return wrong_contract::<()>("open-port", "(or/c bytevector? #f)", 3, args.len() as _, args).into();
                        }

                        if !args[4].falsep() {
                            return wrong_contract::<()>("open-port", "#f", 4, args.len() as _, args).into();
                        }

                        let transcoder = if (args[5].truep() || args[5].falsep()) || args[5].byte_vectorp() {
                            args[5]
                        } else {
                            return wrong_contract::<()>("open-port", "(or/c bytevector? #t #f)", 5, args.len() as _, args).into();
                        };

                        let port = Port::new(vm.mutator());

                        port_open_bytevector(port, name, direction, bytes, transcoder);

                        Trampoline::Return(unsafe {
                            Value::encode_ptr(port.as_ptr())
                        })
                    } else {
                        return wrong_contract::<()>("open-port", "symbol?", 2, args.len() as _, args).into();
                    }
                }

                SCM_PORT_TYPE_CUSTOM => {
                    if name.strp() {
                        let handlers = if args[3].vectorp() {
                            if args[3].vector_len() != 5 {
                                return wrong_contract::<()>("open-port", "vector of length 5", 3, args.len() as _, args).into();
                            } else {
                                args[4]
                            }
                        } else {
                            return wrong_contract::<()>("open-port", "vector?", 3, args.len() as _, args).into();
                        };

                        if !args[4].falsep() {
                            return wrong_contract::<()>("open-port", "#f", 4, args.len() as _, args).into();
                        }

                        let transcoder = if args[5].falsep() || args[5].truep() {
                            args[5]
                        } else {
                            return wrong_contract::<()>("open-port", "(or/c #t #f)", 5, args.len() as _, args).into();
                        };


                        let port = Port::new(vm.mutator());

                        port_make_custom_port(port, name, direction, handlers, transcoder);

                        Trampoline::Return(unsafe {
                            Value::encode_ptr(port.as_ptr())
                        })
                    } else {
                        return wrong_contract::<()>("open-port", "string?", 2, args.len() as _, args).into();
                    }
                }
                _ => todo!()

        }
    }
}

define_proc! {
    extern "make-file-input-port", make_file_input_port(vm, args) 1, 1 => {
        if args[0].strp() {
            let port = Port::new(vm.mutator());

            match port_open_file(port, args[0], SCM_PORT_DIRECTION_IN, 0, SCM_PORT_BUFFER_MODE_BLOCK, Value::make_true()) {
                Ok(_) => {}
                Err(e) => {
                    return Trampoline::Throw(e);
                }
            }

            Trampoline::Return(unsafe {
                Value::encode_ptr(port.as_ptr())
            })
        } else {
            return wrong_contract::<()>("make-file-input-port", "string?", 0, args.len() as _, args).into();
        }
    }
}

define_proc! {
    extern "make-file-output-port", make_file_output_port(vm, args) 1, 1 => {
        if args[0].strp() {
            let port = Port::new(vm.mutator());

            port_open_file(port, args[0], SCM_PORT_DIRECTION_OUT, SCM_PORT_FILE_OPTION_NO_FAIL, SCM_PORT_BUFFER_MODE_BLOCK, Value::make_true())?;

            Trampoline::Return(unsafe {
                Value::encode_ptr(port.as_ptr())
            })
        } else {
            return wrong_contract::<()>("make-file-output-port", "string?", 0, args.len() as _, args).into();
        }
    }
}

define_proc! {
    extern "make-temporary-file-port", make_temporary_file_port(vm, args) 2, 2 => {
        if args[0].strp() {
            if args[1].byte_vectorp() || args[1].falsep() || args[1].truep() {
                let transcoder = args[1];

                if transcoder.byte_vectorp() && transcoder.byte_vector_len() != 3 {
                    return wrong_contract::<()>("make-temporary-file-port", "bytevector of length 3", 1, args.len() as _, args).into();
                }

                let port = Port::new(vm.mutator());

                port_open_temp_file(port, args[0], SCM_PORT_BUFFER_MODE_BLOCK, transcoder)?;

                Trampoline::Return(unsafe {
                    Value::encode_ptr(port.as_ptr())
                })
            } else {
                return wrong_contract::<()>("make-temporary-file-port", "(or/c bytevector? #t #f)", 1, args.len() as _, args).into();
            }
        } else {
            return wrong_contract::<()>("make-temporary-file-port", "string?", 0, args.len() as _, args).into();
        }
    }
}

define_proc! {
    extern "nonblock-byte-ready?", nonblock_byte_ready(_vm, args) 1, 1 => {
        if args[0].portp() {
            let port = args[0].downcast_port();

            port.lock.lock(true);

            check_opened_input_port!(port, "nonblock-byte-ready?", 0, args);

            let rv = port_nonblock_byte_ready(port);
            port.lock.unlock();
            rv.map::<Value, _>(|x| x.into()).into()
        } else {
            return wrong_contract::<()>("nonblock-byte-ready?", "input-port?", 0, args.len() as _, args).into();
        }
    }
}

define_proc! {
    extern "get-char", get_char(_vm, args) 1, 1 => {
        if args[0].portp() {
            let port = args[0].downcast_port();

            port.lock.lock(true);

            check_opened_input_textual_port!(port, "get-char", 0, args);

            let rv = port_get_char(port);
            port.lock.unlock();
            rv.map::<Value, _>(|x| x.into()).into()
        } else {
            return wrong_contract::<()>("get-char", "input-port?", 0, args.len() as _, args).into();
        }
    }
}

define_proc! {
    extern "lookahead-char", lookahead_char(_vm, args) 1, 1 => {
        if args[0].portp() {
            let port = args[0].downcast_port();

            port.lock.lock(true);

            check_opened_input_textual_port!(port, "lookahead-char", 0, args);

            let rv = port_lookahead_char(port);
            port.lock.unlock();
            rv.map(|x| x).into()
        } else {
            return wrong_contract::<()>("lookahead-char", "input-port?", 0, args.len() as _, args).into();
        }
    }
}

define_proc! {
    extern "port-has-port-position?", is_port_has_port_position(_vm, args) 1, 1 => {
        if args[0].portp() {
            let port = args[0].downcast_port();

            port.lock.lock(true);

            check_opened_port!(port, "port-has-port-position?", 0, args);

            let rv = port_has_port_position_pred(port);
            port.lock.unlock();
            Trampoline::Return(rv.into())
        } else {
            return wrong_contract::<()>("port-has-port-position?", "port?", 0, args.len() as _, args).into();
        }
    }
}

define_proc! {
    extern "port-position", get_port_position(_vm, args) 1, 1 => {
        if args[0].portp() {
            let port = args[0].downcast_port();
            port.lock.lock(true);

            check_opened_port!(port, "port-position", 0, args);

            if port_has_port_position_pred(port) {
                let rv = port_position(port);
                port.lock.unlock();
                rv.map(|x| Value::make_int(x as i32)).into()
            } else {
                port.lock.unlock();
                return wrong_contract::<()>("port-position", "port-has-port-position?", 0, args.len() as _, args).into();
            }

        } else {
            return wrong_contract::<()>("port-position", "port?", 0, args.len() as _, args).into();
        }
    }
}

define_proc! {
    extern "port-has-set-port-position?", is_port_has_set_port_position(_vm, args) 1, 1 => {
        if args[0].portp() {
            let port = args[0].downcast_port();

            port.lock.lock(true);

            check_opened_port!(port, "port-has-set-port-position?", 0, args);

            let rv = port_has_set_port_position_pred(port);
            port.lock.unlock();
            Trampoline::Return(rv.into())
        } else {
            return wrong_contract::<()>("port-has-set-port-position?", "port?", 0, args.len() as _, args).into();
        }
    }
}

define_proc! {
    extern "port-set-position!", set_port_position(_vm, args) 2, 2 => {
        if args[0].portp() {
            let port = args[0].downcast_port();
            port.lock.lock(true);

            check_opened_port!(port, "port-set-position!", 0, args);

            if port_has_set_port_position_pred(port) {
                if args[1].intp() {
                    let pos = args[1].int() as i64;
                    let rv = port_set_port_position(port, pos);
                    port.lock.unlock();
                    rv.map(|_| Value::make_void()).into()
                } else {
                    port.lock.unlock();
                    return wrong_contract::<()>("port-set-position!", "smallint?", 1, args.len() as _, args).into();
                }
            } else {
                port.lock.unlock();
                return wrong_contract::<()>("port-set-position!", "port-has-set-port-position?", 0, args.len() as _, args).into();
            }

        } else {
            return wrong_contract::<()>("port-set-position!", "port?", 0, args.len() as _, args).into();
        }
    }
}

define_proc! {
    extern "port-eof?", is_port_eof(_vm, args) 1, 1 => {
        if args[0].portp() {
            let port = args[0].downcast_port();

            port.lock.lock(true);

            check_opened_input_port!(port, "port-eof?", 0, args);

            let rv = port_eof(port);
            port.lock.unlock();
            rv.into()
        } else {
            return wrong_contract::<()>("port-eof?", "port?", 0, args.len() as _, args).into();
        }
    }
}

define_proc! {
    extern "get-u8", get_u8(_vm, args) 1, 1 => {
        if args[0].portp() {
            let port = args[0].downcast_port();

            port.lock.lock(true);

            check_opened_input_port!(port, "get-u8", 0, args);

            let rv = port_get_u8(port);
            port.lock.unlock();
            rv.map::<Value, _>(|x| x.into()).into()
        } else {
            return wrong_contract::<()>("get-u8", "input-port?", 0, args.len() as _, args).into();
        }
    }
}

define_proc! {
    extern "lookahead-u8", lookahead_u8(_vm, args) 1, 1 => {
        if args[0].portp() {
            let port = args[0].downcast_port();

            port.lock.lock(true);

            check_opened_input_port!(port, "lookahead-u8", 0, args);

            let rv = port_lookahead_u8(port);
            port.lock.unlock();
            rv.map(|x| x).into()
        } else {
            return wrong_contract::<()>("lookahead-u8", "input-port?", 0, args.len() as _, args).into();
        }
    }
}

define_proc! {
    extern "get-byte", get_byte(_vm, args) 1, 1 => {
        if args[0].portp() {
            let port = args[0].downcast_port();

            port.lock.lock(true);

            check_opened_input_port!(port, "get-byte", 0, args);

            let rv = port_get_u8(port);
            port.lock.unlock();
            rv.map::<Value, _>(|x| x.into()).into()
        } else {
            return wrong_contract::<()>("get-byte", "input-port?", 0, args.len() as _, args).into();
        }
    }
}

define_proc! {
    extern "lookahead-byte", lookahead_byte(_vm, args) 1, 1 => {
        if args[0].portp() {
            let port = args[0].downcast_port();

            port.lock.lock(true);

            check_opened_input_port!(port, "lookahead-byte", 0, args);

            let rv = port_lookahead_u8(port);
            port.lock.unlock();
            rv.map(|x| x).into()
        } else {
            return wrong_contract::<()>("lookahead-byte", "input-port?", 0, args.len() as _, args).into();
        }
    }
}

define_proc! {
    extern "get-bytevector-n", get_bytevector_n(vm, args) 2, 2 => {
        if args[0].portp() {
            let port = args[0].downcast_port();

            port.lock.lock(true);

            check_opened_input_binary_port!(port, "get-bytevector-n", 0, args);

            let count = if args[1].is_nonnegative_exact_smallint() {
                args[1].int() as usize 
            } else {
                port.lock.unlock();
                return wrong_contract::<()>("get-bytevector-n", "nonnegative-exact-integer?", 1, args.len() as _, args).into();
            };
            
            let bvector = Value::make_byte_vector(vm.mutator(), count as _, 0);

            if count == 0 {
                port.lock.unlock();
                return Trampoline::Return(bvector);
            }

            let n = port_get_bytes(port, bvector.byte_vector_as_slice_mut());

            match n {
                Ok(n) => {
                    if n == 0 {
                        port.lock.unlock();
                        return Trampoline::Return(Value::make_eof())
                    } else if n == count as i32 {
                        port.lock.unlock();
                        return Trampoline::Return(bvector);
                    } else {
                        let bvector2 = Value::make_byte_vector(vm.mutator(), n as _, 0);
                        bvector2.byte_vector_as_slice_mut().copy_from_slice(&bvector.byte_vector_as_slice()[..n as usize]);

                        port.lock.unlock();
                        return Trampoline::Return(bvector2);
                    }
                },
                Err(e) => {
                    port.lock.unlock();
                    return Trampoline::Throw(e);
                }
            }
        } else {
            return wrong_contract::<()>("get-bytevector-n", "input-port?", 0, args.len() as _, args).into();
        }
    }
}

define_proc! {
    extern "get-bytevector-n!", get_bytevector_n_destructing(_vm, args) 4, 4 => {
        if args[0].portp() {

            let port = args[0].downcast_port();

            port.lock.lock(true);

            check_opened_input_binary_port!(port, "get-bytevector-n!", 0, args);
            
            if args[1].byte_vectorp() {
                let start = if args[2].is_nonnegative_exact_smallint() {
                    args[2].int() as usize
                } else {
                    port.lock.unlock();
                    return wrong_contract::<()>("get-bytevector-n!", "nonnegative-exact-integer?", 2, args.len() as _, args).into();
                };
                
                let count = if args[3].is_nonnegative_exact_smallint() {
                    args[3].int() as usize
                } else {
                    port.lock.unlock();
                    return wrong_contract::<()>("get-bytevector-n!", "nonnegative-exact-integer?", 3, args.len() as _, args).into();
                };

                if count == 0 {
                    port.lock.unlock();
                    return Trampoline::Return(Value::make_int(0));
                }
                if start + count <= args[1].byte_vector_len()  {
                    
                    let n = port_get_bytes(port, &mut args[1].byte_vector_as_slice_mut()[start..]);

                    match n {
                        Ok(n) => if n == 0 {
                            port.lock.unlock();
                            return Trampoline::Return(Value::make_eof());
                        } else {
                            port.lock.unlock();
                            return Trampoline::Return(Value::make_int(n));
                        },
                        Err(e) => {
                            port.lock.unlock();
                            return Trampoline::Throw(e);
                        }
                    }
                } else {
                    port.lock.unlock();
                    
                    if start >= args[1].byte_vector_len() {
                        return out_of_range::<()>("get-bytevector-n!", Some("bytevector"), "start", args[2], args[1], 0, args[1].byte_vector_len() as _).into();
                    } else {
                        return out_of_range::<()>("get-bytevector-n!", Some("bytevector"), "count", args[3], args[1], 0, args[1].byte_vector_len() as _).into();
                    }
                }
            } else {
                port.lock.unlock();
                return wrong_contract::<()>("get-bytevector-n!", "bytevector?", 1, args.len() as _, args).into();
            }

        } else {
            return wrong_contract::<()>("get-bytevector-n!", "input-port?", 0, args.len() as _, args).into();
        }
    }
}

define_proc! {
    extern "get-bytevector-some", get_bytevector_some(vm, args) 1, 1 => {
        if args[0].portp() {
            let port = args[0].downcast_port();

            port.lock.lock(true);

            check_opened_input_binary_port!(port, "get-bytevector-some", 0, args);

            let c = port_lookahead_byte(port).or_else(|e| {
                port.lock.unlock();
                Err(e)
            })?;

            if c == libc::EOF {
                port.lock.unlock();
                return Trampoline::Return(Value::make_eof());
            }

            let n = port_buffered_byte_count(port);

            let bvector = Value::make_byte_vector(vm.mutator(), n as _, 0);

            for i in 0..n {
                bvector.byte_vector_set(i as _, port_get_byte(port).map(|x| x as u8).or_else(|e| {
                    port.lock.unlock();
                    Err(e)
                })?);
            }

            port.lock.unlock();
            return Trampoline::Return(bvector);
        } else {
            return wrong_contract::<()>("get-bytevector-some", "input-port?", 0, args.len() as _, args).into();
        }
    }
}

define_proc! {
    extern "get-bytevector-all", get_bytevector_all(vm, args) 1, 1 => {
        if args[0].portp() {
            let port = args[0].downcast_port();

            port.lock.lock(true);

            check_opened_input_binary_port!(port, "get-bytevector-all", 0, args);

            let output = Port::new(vm.mutator());

            port_open_bytevector(port, intern("bytevector"), SCM_PORT_DIRECTION_OUT, Value::make_false(), Value::make_false());

            {
                let mut buf = [0u8; SCM_PORT_BLOCK_BUFFER_SIZE];

                loop {
                    let n = port_get_bytes(port, &mut buf).map_err(|e| {
                        port_discard_buffer(output);
                        e 
                    })?;

                    if n == 0 {
                        if port_position(port).map_err(|e| {
                            port_discard_buffer(output);
                            e
                        })? == 0 {
                            port_discard_buffer(output);
                            port.lock.unlock();
                            return Trampoline::Return(Value::make_eof());
                        }

                        let bv = port_extract_bytevector(port);

                        port_discard_buffer(output);

                        port.lock.unlock();

                        return Trampoline::Return(bv);
                    }

                    port_put_bytes(output, &buf[..n as usize]).map_err(|e| {
                        port_discard_buffer(output);
                        e
                    })?;
                }
            }
        } else {
            return wrong_contract::<()>("get-bytevector-all", "input-port?", 0, args.len() as _, args).into();
        }
    }
}