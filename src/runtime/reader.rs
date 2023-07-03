

use once_cell::sync::Lazy;

use rsgc::prelude::Handle;

use crate::{vm::VM, raise_exn};

use super::{port::{Port, port_regular_file_pred, port_has_port_position_pred, port_has_set_port_position_pred, port_position, port_set_port_position, port_nonblock_byte_ready, port_get_byte, port_get_char, port_lookahead_char}, value::Value, error::{make_srcloc}, list::{scm_cons, scm_list}};

pub struct Reader<'a> {
    vm: &'a mut VM,
    first_line: i32,
    parsing_line_from: i32,
    parsing_line_to: i32,
    ungetbuf: Value,
    ungetbuf_valid: bool,
    port: Handle<Port>,
    file: bool,
    foldcase: bool,
}

const CHAR_MAP_SYMBOL: u8 = 0x01;
const CHAR_MAP_INITIAL: u8 = 0x02;
const CHAR_MAP_DELIMITER: u8 = 0x04;

static CHAR_MAP: Lazy<[u8; 100]> = Lazy::new(|| {
    let mut map = [0; 100];

    for i in 1..map.len() as u8 {
        map[i as usize] =
            if (i as char).is_alphanumeric() || ".!?*+-/:<=>$%&@^_~".contains(i as char) {
                CHAR_MAP_SYMBOL
            } else {
                0
            };

        map[i as usize] = if (i as char).is_alphabetic() || "!?*/:<=>$%&^_~".contains(i as char) {
            CHAR_MAP_INITIAL
        } else {
            0
        };

        map[i as usize] = if "()[]\";#".contains(i as char) {
            CHAR_MAP_DELIMITER
        } else {
            0
        };
    }

    map
});

fn delimited(c: char) -> bool {
    if c.is_whitespace() {
        return true;
    }

    if c as u32 > 127 {
        return false;
    }

    (CHAR_MAP[c as usize] & CHAR_MAP_DELIMITER) != 0
}



impl<'a> Reader<'a> {
    pub fn new(vm: &'a mut VM, input: Handle<Port>, foldcase: bool) -> Self {
        Self {
            file: port_regular_file_pred(input),
            vm,
            first_line: 0,
            ungetbuf: Value::eof_object(),
            ungetbuf_valid: false,
            port: input,
            parsing_line_from: 0,
            parsing_line_to: 0,
            foldcase,
        }
    }

    pub fn done(self) -> Result<(), Value> {
        if self.ungetbuf_valid && !self.ungetbuf.is_eof_object() {
            if port_has_port_position_pred(self.port) && port_has_set_port_position_pred(self.port) {
                let n = self.ungetbuf.get_char();
                let position = port_position(self.port)?;

                if position >= 0 {
                    port_set_port_position(self.port, position - n.len_utf8() as i64)?;
                }
            }
        }
        Ok(())
    }


    pub fn lexical_error<T>(&mut self, msg: &str) -> Result<T, Value> {
        self.port.lock.lock(true);
        let c = self.port.column;
        let pos = if port_has_port_position_pred(self.port) {
            port_position(self.port)?
        } else {
            -1
        };
        
        while port_nonblock_byte_ready(self.port).map_err(|e| {
            self.port.lock.unlock();
            e 
        })? {
            if port_get_byte(self.port).map_err(|e| {
                self.port.lock.unlock();
                e
            })? == libc::EOF {
                break;
            }

            continue;
        }

        let mut message = msg.to_string();
        message.push_str(&format!("\n ... {}:", self.port.name));
        if self.parsing_line_from == self.parsing_line_to {
            message.push_str(&format!("{}", self.parsing_line_from));
        } else {
            message.push_str(&format!("{}-{}", self.parsing_line_from, self.parsing_line_to));
        }

        raise_exn!(FailRead, &[make_srcloc(self.port.name, self.parsing_line_from, c, pos as _)], "{}", message)
    }

    fn get_char(&mut self) -> Result<Option<char>, Value> {
        if self.ungetbuf_valid {
            self.ungetbuf_valid = false;
            if self.ungetbuf.is_eof_object() {
                return Ok(None)
            }

            Ok(Some(self.ungetbuf.get_char()))
        } else {
            let ch = port_get_char(self.port)?;
            self.ungetbuf = ch;
            if ch.is_eof_object() {
                Ok(None)
            } else {
                Ok(Some(ch.get_char()))
            }
        }
    }

    fn unget_char(&mut self) {
        self.ungetbuf_valid = true;
    }

    fn lookahead_char(&mut self) -> Result<Option<char>, Value> {
        if self.ungetbuf_valid {
            if self.ungetbuf.is_eof_object() {
                return Ok(None)
            }

            Ok(Some(self.ungetbuf.get_char()))
        } else {
            let ch = port_lookahead_char(self.port)?;
            if ch.is_eof_object() {
                Ok(None)
            } else {
                Ok(Some(ch.get_char()))
            }
        }
    }

    fn cons(&mut self, e1: Value, e2: Value) -> Value {
        scm_cons(self.vm.mutator(), e1, e2)
    }

    fn list2(&mut self, e1: Value, e2: Value) -> Value {
        scm_list(self.vm.mutator(), &[e1, e2])
    }

    fn reverse_list(&mut self, mut lst: Value, tail: Value) -> Value {
        let mut r = tail;
        while lst.is_pair() {
            r = self.cons(lst.car(), r);
            lst = lst.cdr();
        }

        r 
    }

    fn skip_line(&mut self) -> Result<Value, Value> {
        while let Some(_c) = self.get_char()? {
            
        }

        Ok(Value::eof_object())
    }

    fn skip_srfi30(&mut self) -> Result<Value, Value> {
        let mut c1;
        let mut c2;

        let mut nest = 0;

        'seek_c1: loop {
            c1 = self.get_char()?;

            'seek_c2: loop {
                c2 = self.get_char()?;

                if c2.is_none() {
                    return self.lexical_error("unexpected end of file while reading comments");
                }

                if c1 == Some('|') && c2 == Some('#') {
                    if nest == 0{
                        todo!()
                    }
                    nest -= 1;
                    continue 'seek_c1;
                }

                if c1 == Some('#') && c2 == Some('|') {
                    nest += 1;
                    continue 'seek_c1;
                }

                c1 = c2;

                if c1 == Some('|') || c1 == Some('#') {
                    continue 'seek_c2;
                }

                continue 'seek_c1;
            }
        }
    }

    
}

