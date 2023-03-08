use once_cell::sync::Lazy;
use rsgc::prelude::Handle;

use crate::{
    error::make_srcloc,
    ports_v2::{
        cnvt_ucs4_to_utf8, port_get_byte, port_get_char, port_has_port_position_pred,
        port_has_set_port_position_pred, port_lookahead_char, port_nonblock_byte_ready,
        port_position, port_regular_file_pred, port_set_port_position, Port,
    },
    raise_exn,
    value::Value,
    vm::Vm,
};

pub struct Reader<'a> {
    first_line: i32,
    parsing_line_from: i32,
    parsing_line_to: i32,
    ungetbuf: Value,
    ungetbuf_valid: bool,
    graph_ref: i32,
    vm: &'a mut Vm,
    file: bool,
    fold_case: bool,
    m_in: Handle<Port>,
}

static CHAR_MAP: Lazy<[u8; 128]> = Lazy::new(|| {
    let mut map = [0u8; 128];

    for i in 1..128 {
        map[i] = if (i as u8 as char).is_alphanumeric()
            || ".!?*+-/:<=>$%&@^_~".contains(i as u8 as char)
        {
            0x01
        } else {
            0
        };

        map[i] |= if (i as u8 as char).is_alphabetic() || "!?*/:<=>$%&^_~".contains(i as u8 as char)
        {
            0x02
        } else {
            0
        };

        map[i] |= if (i as u8 as char).is_ascii_digit() || "()[]\";#".contains(i as u8 as char) {
            0x04
        } else {
            0
        };
    }

    map
});

fn cnvt_hex_char_to_int(c: i32) -> i32 {
    if c >= '0' as i32 && c <= '9' as i32 {
        c - '0' as i32
    } else if c >= 'a' as i32 && c <= 'f' as i32 {
        c - 'a' as i32 + 10
    } else if c >= 'A' as i32 && c <= 'F' as i32 {
        c - 'A' as i32 + 10
    } else {
        -1
    }
}

fn delimited(c: char) -> bool {
    if c.is_whitespace() {
        true
    } else if c as u32 > 127 {
        false
    } else {
        CHAR_MAP[c as usize] & 0x04 != 0
    }
}

impl<'a> Reader<'a> {
    pub fn new(vm: &'a mut Vm, port: Handle<Port>, fold_case: bool) -> Self {
        Self {
            first_line: 0,
            parsing_line_from: 0,
            parsing_line_to: 0,
            ungetbuf: Value::make_eof(),
            ungetbuf_valid: false,
            graph_ref: 0,

            vm,
            file: port_regular_file_pred(port),
            fold_case,
            m_in: port,
        }
    }

    pub fn destroy(self) -> Result<(), Value> {
        if self.ungetbuf_valid && !self.ungetbuf.eofp() {
            if port_has_port_position_pred(self.m_in) && port_has_set_port_position_pred(self.m_in)
            {
                let n = self.ungetbuf.char_val().len_utf8();
                let position = port_position(self.m_in)? - n as i64;

                if position >= 0 {
                    port_set_port_position(self.m_in, position)?;
                }
            }
        }

        Ok(())
    }

    pub fn lexical_error<T>(&mut self, msg: &str) -> Result<T, Value> {
        self.m_in.lock.lock(true);

        let col = self.m_in.column;
        let pos = port_position(self.m_in)?;

        while port_nonblock_byte_ready(self.m_in)? {
            if port_get_byte(self.m_in)? == libc::EOF {
                break;
            }
            continue;
        }

        let srcloc = make_srcloc(self.m_in.name, self.parsing_line_from, col, pos as _);

        self.m_in.lock.unlock();

        raise_exn!(T, FailRead, &[srcloc], "lexical error: {}", msg)
    }

    pub fn get_char(&mut self) -> Result<Option<char>, Value> {
        if self.ungetbuf_valid {
            self.ungetbuf_valid = false;

            if self.ungetbuf.eofp() {
                return Ok(None);
            }

            return Ok(Some(*self.ungetbuf.char_val()));
        } else {
            let c = port_get_char(self.m_in)?;

            self.ungetbuf = c;

            if c.eofp() {
                return Ok(None);
            }

            return Ok(Some(*c.char_val()));
        }
    }

    pub fn unget_char(&mut self) {
        self.ungetbuf_valid = true;
    }

    pub fn lookahead_char(&mut self) -> Result<Option<char>, Value> {
        if self.ungetbuf_valid {
            if self.ungetbuf.eofp() {
                return Ok(None);
            }

            return Ok(Some(*self.ungetbuf.char_val()));
        } else {
            let c = port_lookahead_char(self.m_in)?;

            if c.eofp() {
                return Ok(None);
            }

            return Ok(Some(*c.char_val()));
        }
    }

    pub fn make_cons(&mut self, car: Value, cdr: Value) -> Value {
        Value::make_cons(car, cdr)
    }

    pub fn list2(&mut self, car: Value, cdr: Value) -> Value {
        Value::make_cons(car, Value::make_cons(cdr, Value::make_null()))
    }

    pub fn reverse_list(&mut self, mut lst: Value, tail: Value) -> Value {
        let mut r = tail;

        while lst.pairp() {
            r = Value::make_cons(lst.car(), r);
            lst = lst.cdr();
        }

        r
    }

    pub fn skip_line(&mut self) -> Result<Value, Value> {
        loop {
            let c = self.get_char()?;

            if c.is_none() {
                return Ok(Value::make_eof());
            }

            let c = c.unwrap();

            if c == '\r' || c == '\n' {
                return self.read_token();
            }
        }
    }

    pub fn skip_srfi30(&mut self) -> Result<Value, Value> {
        let mut c1;
        let mut c2;
        let mut nest = 0;

        'skip_c1: loop {
            c1 = self.get_char()?;

            'skip_c2: loop {
                c2 = self.get_char()?;

                if c2.is_none() {
                    return self.lexical_error("unexpected EOF while reading comments");
                }

                if c1 == Some('|') && c2 == Some('#') {
                    if nest == 0 {
                        return self.read_token();
                    } else {
                        nest -= 1;
                        continue 'skip_c1;
                    }
                }

                if c1 == Some('#') && c2 == Some('|') {
                    nest += 1;
                    continue 'skip_c1;
                }

                c1 = c2;

                if c1 == Some('|') || c1 == Some('#') {
                    continue 'skip_c2;
                }

                continue 'skip_c1;
            }
        }
    }

    pub fn read_thing(&mut self, limit: usize) -> Result<String, Value> {
        let mut i = 0;
        let mut res = String::new();
        while i < limit {
            let c = self.lookahead_char()?;

            if c.is_none() {
                return Ok(res);
            }

            if delimited(c.unwrap()) {
                return Ok(res);
            }

            self.get_char()?;
            let c = c.unwrap();
            res.push(c);
        }

        self.lexical_error(&format!(
            "token buffer overflow while reading identifier, {}...",
            res
        ))
    }

    pub fn read_hex_scalar_value(&mut self) -> Result<i32, Value> {
        let mut ucs4 = 0;

        let c = self.get_char()?;

        if c.is_none() {
            return self.lexical_error("unexpected EOF while reading hex scalar value");
        }

        let c = c.unwrap();

        if delimited(c) {
            return self.lexical_error(&format!(
                "expected hex digit, but got {}, while reading hex scalar value",
                c
            ));
        }

        self.unget_char();

        loop {
            let c = self.get_char()?;

            if c.is_none() || delimited(c.unwrap()) {
                self.unget_char();
                return Ok(ucs4);
            }

            let n = cnvt_hex_char_to_int(c.unwrap() as i32);

            if n < 0 {
                return self.lexical_error(&format!(
                    "expected hex digit, but got {}, while reading hex scalar value",
                    c.unwrap()
                ));
            }

            ucs4 = (ucs4 << 4) + n;

            if ucs4 > 0x10FFFF {
                return self.lexical_error(&format!("hex scalar value out of range: #x{:X}", ucs4));
            }
        }
    }

    pub fn read_bytevector(&mut self) -> Result<Value, Value> {
        let buf = self.read_thing(buf)?;

        let c = self.get_char()?;

        if c == Some('(') {
            let line_begin = self.m_in.line;

            let lst = self.read_list()?;

            if buf == "u8" {
                let n = lst.proper_list_length().ok_or_else(|| {
                    self.lexical_error(&format!(
                        "expected proper list for bytevector but got: {}",
                        lst
                    ))
                })?;
                let bvector = Value::make_byte_vector(self.vm.mutator(), n as _, 0);

                for i in 0..n {
                    let datum = lst.car();

                    if !datum.intp() {
                        return self.lexical_error(&format!(
                            "expected integer for bytevector element but got: {}",
                            datum
                        ));
                    }

                    let datum = datum.int();

                    bvector.byte_vector_set(i, datum as _);

                    lst = lst.cdr();
                }

                return Ok(bvector);
            }
        }

        self.lexical_error(&format!(
            "invalid lexical syntax: #{}{}",
            buf,
            if c.is_none() {
                "#<eof>".to_string()
            } else {
                c.unwrap().to_string()
            }
        ))  
    }

    pub fn read_number(&mut self) -> Result<Value, Value> {
        let s = self.read_thing(4096)?;
    }

    pub fn read_list(&mut self) -> Result<Value, Value> {
        todo!()
    }

    pub fn read_token(&mut self) -> Result<Value, Value> {
        todo!()
    }
}
