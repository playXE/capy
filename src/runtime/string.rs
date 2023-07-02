use rsgc::{prelude::Handle, thread::Thread};

use crate::{runtime::object::{ObjectHeader, Str, Type}, raise_exn, vm::{scm_vm, callframe::CallFrame}};

use super::{port::{Port, port_puts, port_open_bytevector, SCM_PORT_DIRECTION_OUT, port_extract_string}, value::Value, error::{wrong_contract, contract_error, make_args_string}, print::Printer, object::{ScmResult, MAX_ARITY}, symbol::Intern, module::{scm_scheme_module, scm_define}, fun::scm_make_subr};

pub fn make_string(thread: &mut Thread, s: impl AsRef<str>) -> Handle<Str> {
    let mut str = thread.allocate_varsize::<Str>(s.as_ref().len());
    let orig = s.as_ref();
    unsafe {
        let s = str.assume_init_mut();
        s.object = ObjectHeader::new(Type::Str);

        s.data
            .as_mut_ptr()
            .copy_from_nonoverlapping(orig.as_ptr(), orig.len());
        str.assume_init()
    }
}
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
        if !args[fpos].is_string() {
            return wrong_contract(who, "string?", fpos as _, args.len() as _, args);
        }

        args[fpos].strsym()
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

    let mut printer = Printer::new(scm_vm(), port);

    while i < format.len() {
        if bytes[i] == b'~' {
            if start < i {
                /*port.put_string(std::str::from_utf8(&bytes[start..i]).unwrap())
                .map_err(map_io_error)?;*/
                printer.puts(std::str::from_utf8(&bytes[start..i]).unwrap())?;
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
                        printer.puts("~")?;
                    }

                    b'%' | b'n' | b'N' => {
                        printer.puts("\n")?;
                        //port.put_string("\n").map_err(map_io_error)?;
                    }

                    b'c' | b'C' | b'a' | b'A' => {
                        printer.escape = false;
                        
                        printer.write(args[used])?;
                        //port.display(args[used]).map_err(map_io_error)?;
                        used += 1;
                    }
                    b's' | b'S' => {
                        printer.escape = true;
                        printer.write(args[used])?;
                        //port_puts(port, &format!("{}", args[used]))?;
                        used += 1;
                    }

                    b'v' | b'P' => {
                        printer.escape = true;
                        printer.write(args[used])?;
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

extern "C" fn format(cfr: &mut CallFrame) -> ScmResult {
    let vm = scm_vm();
    let port = Port::new(vm.mutator());
    port_open_bytevector(port, "format".intern().into(), SCM_PORT_DIRECTION_OUT, false.into(), false.into());

    match do_format("format", port, None, 0, 1, cfr.argument_count(), cfr.arguments()) {
        Ok(_) => match port_extract_string(port) {
            Ok(s) => ScmResult::ok(s),
            Err(e) => ScmResult::err(e),
        }
        Err(e) => ScmResult::err(e),
    }
}

pub(crate) fn init_string() {
    let module = scm_scheme_module().module();

    let subr = scm_make_subr("format", format, 1, MAX_ARITY);
    scm_define(module, "format".intern(), subr).unwrap();
}