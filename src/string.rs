use crate::{
    error::{contract_error, make_args_string, wrong_contract},
    ports::{map_io_error, TextOutputPort},
    raise_exn,
    value::Value,
};

pub fn do_format<T: TextOutputPort>(
    who: &str,
    port: &mut T,
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
    println!("done");
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
                "{}: format string requires {} arguments, given {}{}",
                who,
                used - offset,
                argc - offset,
                args
            );
        } else {
            return raise_exn!(
                FailContract,
                &[],
                "{}: format string requires {} arguments, given {}{}",
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
                port.put_string(std::str::from_utf8(&bytes[start..i]).unwrap())
                    .map_err(map_io_error)?;
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
                        port.put_string("~").map_err(map_io_error)?;
                    }

                    b'%' | b'n' | b'N' => {
                        port.put_string("\n").map_err(map_io_error)?;
                    }

                    b'c' | b'C' | b'a' | b'A' => {
                        port.display(args[used]).map_err(map_io_error)?;
                        used += 1;
                    }
                    b's' | b'S' => {
                        port.write(args[used]).map_err(map_io_error)?;
                        used += 1;
                    }

                    b'v' | b'P' => {
                        port.display(args[used]).map_err(map_io_error)?;
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
        port.put_string(std::str::from_utf8(&bytes[start..i]).unwrap())
            .map_err(map_io_error)?;
    }

    Ok(())
}
