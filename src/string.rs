use rsgc::prelude::Handle;

use crate::{
    compiler::env::environment_set,
    error::{contract_error, make_args_string, wrong_contract},
    ports_v2::{
        port_extract_string, port_open_bytevector, port_puts, Port, SCM_PORT_DIRECTION_OUT,
    },
    raise_exn,
    value::Value,
    vm::intern,
};

pub fn do_format(
    who: &str,
    port: Handle<Port>,
    format: Option<&str>,
    fpos: usize,
    offset: usize,
    argc: usize,
    args: &[Value],
) -> Result<(), Value> {
    let format = if format.is_none() {
        if !args[fpos].strp() {
            return wrong_contract(who, "string?", fpos as _, args.len() as _, args);
        }

        args[fpos].str()
    } else {
        format.unwrap()
    };
    let bytes = format.as_bytes();
    let end = bytes.len() - 1;
    let mut end_ok = false;
    /*let mut char_err = 0;
    let mut num_err = 0;*/
    let mut i = 0;
    let mut used = offset;
    while i < end {
        if bytes[i] == b'~' {
            i += 1;
            if (bytes[i] as char).is_ascii_whitespace() {
            } else {
                match bytes[i] {
                    b'~' => {
                        if i == end {
                            end_ok = true;
                        }
                    }
                    b'%' | b'n' | b'N' => {}
                    b'a' | b'A' | b's' | b'S' | b'v' | b'V' | b'e' | b'E' => {
                        used += 1;
                    }
                    b'.' => match bytes[i + 1] {
                        b'a' | b'A' | b's' | b'S' | b'v' | b'V' => {}
                        _ => {
                            return contract_error(
                                who,
                                "ill-formed pattern string",
                                &[
                                    &"explanation",
                                    &"tag '~.' not followed by `a', `s', or `V'",
                                    &"pattern string",
                                    &args[fpos],
                                ],
                            );
                        }
                    },
                    _ => {
                        let v = format!("tag `~{}' not allowed", bytes[i] as char);
                        return contract_error(
                            who,
                            "ill-formed pattern string",
                            &[&"explanation", &v, &"pattern string", &args[fpos]],
                        );
                    }
                }
            }
        } else {
            i += 1;
        }
    }

    if bytes[end] == b'~' && !end_ok {
        return contract_error(
            who,
            "ill-formed pattern string",
            &[
                &"explanation",
                &"cannot end with `~'",
                &"pattern string",
                &args[fpos],
            ],
        );
    }

    if used != argc {
        let args = make_args_string("", -1, args.len() as _, args);
        if used > argc {
            return raise_exn!(
                FailContract,
                &[],
                "{}: format string requires {} arguments, given {} {}",
                who,
                used - offset,
                argc - offset,
                args
            );
        } else {
            return raise_exn!(
                FailContract,
                &[],
                "{}: format string requires {} arguments, given {} {}",
                who,
                used - offset,
                argc - offset,
                args
            );
        }
    }

    let mut used = offset;
    let mut i = 0;
    let mut start = 0;

    while i < format.len() {
        if bytes[i] == b'~' {
            if start < i {
                /*port.put_string(std::str::from_utf8(&bytes[start..i]).unwrap())
                .map_err(map_io_error)?;*/
                port_puts(port, std::str::from_utf8(&bytes[start..i]).unwrap())?;
            }
            i += 1;

            if (bytes[i] as char).is_ascii_whitespace() {
                loop {
                    if bytes[i] == b'\n' || bytes[i] == b'\r' {
                        if bytes[i] == b'\r' && bytes[i + 1] == b'\n' {
                            i += 1;
                        }

                        i += 1;

                        while ((bytes[i] as char).is_ascii_whitespace() && bytes[i] != b'\n')
                            || bytes[i] != b'\r'
                        {
                            i += 1;
                        }
                        break;
                    } else {
                        i += 1;
                    }

                    if !(bytes[i] as char).is_ascii_whitespace() {
                        break;
                    }
                }
                i -= 1;
            } else {
                match bytes[i] {
                    b'~' => {
                        port_puts(port, "~")?;
                    }

                    b'%' | b'n' | b'N' => {
                        port_puts(port, "\n")?;
                        //port.put_string("\n").map_err(map_io_error)?;
                    }

                    b'c' | b'C' | b'a' | b'A' => {
                        port_puts(port, &format!("{}", args[used]))?;
                        //port.display(args[used]).map_err(map_io_error)?;
                        used += 1;
                    }
                    b's' | b'S' => {
                        port_puts(port, &format!("{}", args[used]))?;
                        used += 1;
                    }

                    b'v' | b'P' => {
                        port_puts(port, &format!("{}", args[used]))?;
                        used += 1;
                    }

                    _ => {}
                }
            }

            start = i + 1;
        }

        i += 1;
    }
    if start < i {
        port_puts(port, std::str::from_utf8(&bytes[start..i]).unwrap())?;
    }

    Ok(())
}

define_proc! {
    extern "format", format(_vm, args) 0, -1 => {
        let port = Port::new(_vm.mutator);
        port_open_bytevector(port, intern("format"), SCM_PORT_DIRECTION_OUT, Value::make_false(), Value::make_false());

        match do_format("format", port, None, 0, 1, args.len() as _, args) {
            Ok(_) => crate::vm::Trampoline::Return(match port_extract_string(port) {
                Ok(s) => s,
                Err(e) => return crate::vm::Trampoline::Throw(e),
            }) ,
            Err(e) => crate::vm::Trampoline::Throw(e),
        }
    }
}

pub fn initialize_string(env: Value) {
    environment_set(env, *FORMAT_NAME, *FORMAT_PROC);
}
