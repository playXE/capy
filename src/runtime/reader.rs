use std::cmp::Ordering;

use once_cell::sync::Lazy;

use rsgc::{prelude::Handle, system::collections::hashmap::HashMap};

use crate::{
    raise_exn,
    vm::{inherent_symbols, InherentSymbol, VM},
};

use super::{
    arith::parse_number,
    error::make_srcloc,
    list::{scm_cons, scm_length, scm_list, scm_reverse, scm_reverse2x},
    port::{
        port_get_byte, port_get_char, port_has_port_position_pred, port_has_set_port_position_pred,
        port_lookahead_char, port_nonblock_byte_ready, port_position, port_regular_file_pred,
        port_set_port_position, Port, SCM_PORT_UCS4_LF,
    },
    string::make_string,
    symbol::{make_symbol, Intern},
    value::Value,
    vector::make_vector,
};

pub const EOF_CH: char = '\0';
pub const EOL_CH: char = '\n';
pub const RET_CH: char = '\r';
pub const TAB_CH: char = '\t';
pub const SPACE_CH: char = ' ';
pub const ZERO_CH: char = '0';
pub const B_CH: char = 'b';
pub const D_CH: char = 'd';
pub const O_CH: char = 'o';
pub const LA_CH: char = 'a';
pub const UA_CH: char = 'A';
pub const LZ_CH: char = 'z';
pub const UZ_CH: char = 'Z';
pub const DQ_CH: char = '"';
pub const BQ_CH: char = '`';
pub const Q_CH: char = '\'';
pub const DOT_CH: char = '.';
pub const COMMA_CH: char = ',';
pub const AT_CH: char = '@';
pub const HASH_CH: char = '#';
pub const SEMI_CH: char = ';';
pub const BANG_CH: char = '!';
pub const MINUS_CH: char = '-';
pub const PLUS_CH: char = '+';
pub const LE_CH: char = 'e';
pub const UE_CH: char = 'E';
pub const LI_CH: char = 'i';
pub const UI_CH: char = 'I';
pub const BS_CH: char = '\\';
pub const SLASH_CH: char = '/';
pub const BAR_CH: char = '|';
pub const X_CH: char = 'x';
pub const T_CH: char = 't';
pub const R_CH: char = 'r';
pub const N_CH: char = 'n';
pub const UN_CH: char = 'N';
pub const V_CH: char = 'v';
pub const U_CH: char = 'u';
pub const G_CH: char = 'g';
pub const F_CH: char = 'f';
pub const EIGHT_CH: char = '8';
pub const LPAREN_CH: char = '(';
pub const RPAREN_CH: char = ')';
pub const DIGITS: &'static str = "0123456789";

pub const LHEX_DIGITS: &'static str = "abcdef";

pub const UHEX_DIGITS: &'static str = "ABCDEF";

pub const INITIALS: &'static str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";

pub fn is_space(c: char) -> bool {
    c.is_whitespace()
}

pub fn is_letter(c: char) -> bool {
    c.is_alphabetic()
}

pub fn is_digit(c: char) -> bool {
    c.is_digit(10)
}

pub fn is_digit_for_radix(c: char, radix: u32) -> bool {
    c.is_digit(radix)
}

/// `is_initial` returns true if `ch` is an initial identifier character.
#[inline]
pub fn is_initial(ch: char) -> bool {
    ch.is_alphabetic() || is_special_initial(ch)
}

/// `is_special_initial` returns true if `ch` is a special subsequent for identifiers.
#[inline]
pub fn is_special_initial(ch: char) -> bool {
    "!$%&*/:<=>?^_~".contains(ch)
}

/// `is_subsequent` returns true if `ch` is a subsequent identifier character.
#[inline]
pub fn is_subsequent(ch: char) -> bool {
    is_initial(ch) || ch.is_digit(10) || is_special_subsequent(ch)
}

/// `is_explicit_sign` returns true if ch is a plus (+) or minus (-) sign.
#[inline]
pub fn is_explicit_sign(ch: char) -> bool {
    ch == '+' || ch == '-'
}

/// `is_special_subsequent` returns true if `ch` is a special subsequent identifier character.
#[inline]
pub fn is_special_subsequent(ch: char) -> bool {
    is_explicit_sign(ch) || ch == '.' || ch == '@'
}

/// `is_dot_subsequent` returns true if `ch` is a dot subsequent for identifiers.
#[inline]
pub fn is_dot_subsequent(ch: char) -> bool {
    is_sign_subsequent(ch) || ch == '.'
}

/// `is_sign_subsequent` returns true if `ch` is a sign subsequent for identifiers.
#[inline]
pub fn is_sign_subsequent(ch: char) -> bool {
    is_initial(ch) || is_explicit_sign(ch) || ch == '@'
}

pub fn digit_val(c: char) -> isize {
    if DIGITS.contains(c) {
        return c as isize - ZERO_CH as isize;
    } else if LHEX_DIGITS.contains(c) {
        c as isize - LA_CH as isize + 10
    } else if UHEX_DIGITS.contains(c) {
        c as isize - UA_CH as isize + 10
    } else {
        16
    }
}

#[allow(dead_code)]
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
    notes: Option<Handle<HashMap<Value, Value>>>,
}
/// Compare `str` with `b`. `b` is zero-terminated.
fn strcmp(str: &[u8], b: &[u8]) -> Ordering {
    unsafe {
        match libc::strcmp(str.as_ptr() as *const i8, b.as_ptr() as *const i8) {
            x if x < 0 => Ordering::Less,
            x if x > 0 => Ordering::Greater,
            _ => Ordering::Equal,
        }
    }
}

const CHAR_MAP_SYMBOL: u8 = 0x01;
const CHAR_MAP_INITIAL: u8 = 0x02;
const CHAR_MAP_DELIMITER: u8 = 0x04;

fn hex_char_to_int(c: char) -> u32 {
    if c >= '0' && c <= '9' {
        c as u32 - '0' as u32
    } else if c >= 'a' && c <= 'f' {
        c as u32 - 'a' as u32 + 10
    } else if c >= 'A' && c <= 'F' {
        c as u32 - 'A' as u32 + 10
    } else {
        u32::MAX
    }
}

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

    "()[]\";#".contains(c)
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
            notes: None,
        }
    }

    pub fn put_note(&mut self, key: Value, value: Value) {
        self.notes.unwrap().put(key, value);
    }

    pub fn done(self) -> Result<(), Value> {
        if self.ungetbuf_valid && !self.ungetbuf.is_eof_object() {
            if port_has_port_position_pred(self.port) && port_has_set_port_position_pred(self.port)
            {
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
        //self.port.lock.lock(true);
        let c = self.port.column;
        let pos = if port_has_port_position_pred(self.port) {
            port_position(self.port)?
        } else {
            -1
        };

        while port_nonblock_byte_ready(self.port).map_err(|e| {
            //self.port.lock.unlock();
            e
        })? {
            if port_get_byte(self.port).map_err(|e| {
                //self.port.lock.unlock();
                e
            })? == libc::EOF
            {
                break;
            }

            continue;
        }

        let mut message = msg.to_string();
        message.push_str(&format!("\n ... {}:", self.port.name));
        if self.parsing_line_from == self.parsing_line_to {
            message.push_str(&format!("{}:{}", self.parsing_line_from, c));
        } else {
            message.push_str(&format!(
                "{}-{}",
                self.parsing_line_from, self.parsing_line_to
            ));
        }

        raise_exn!(
            FailRead,
            &[make_srcloc(
                self.port.name,
                self.parsing_line_from,
                c,
                pos as _
            )],
            "{}",
            message
        )
    }

    fn get_char(&mut self) -> Result<Option<char>, Value> {
        if self.ungetbuf_valid {
            self.ungetbuf_valid = false;
            if self.ungetbuf.is_eof_object() {
                return Ok(None);
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
                return Ok(None);
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

    fn skip_line(&mut self) -> Result<Value, Value> {
        while let Some(c) = self.get_char()? {
            if c == '\n' {
                return self.read_token();
            }

            if c == '\r' {
                return self.read_token();
            }
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
                    if nest == 0 {
                        return self.read_token();
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

    fn read_thing(&mut self, buf: &mut [u8]) -> Result<(), Value> {
        let mut i = 0;
        while i + 4 < buf.len() {
            let c = self.lookahead_char()?;

            if let Some(c) = c {
                if delimited(c) {
                    buf[i] = 0;
                    return Ok(());
                }

                self.get_char()?;
                if c < 128 as char {
                    buf[i] = c as u8;
                    i += 1;
                } else {
                    i += c.encode_utf8(&mut buf[i..]).len();
                }
            } else {
                buf[i] = 0;
                return Ok(());
            }
        }

        raise_exn!(
            FailOutOfMemory,
            &[],
            "token buffer overflow while reading identifier at {}:{}",
            self.port.name,
            self.port.line
        )
    }

    fn read_number(&mut self) -> Result<Value, Value> {
        let mut buf = [0u8; 4096];

        self.read_thing(&mut buf)?;

        let obj = parse_number(self.vm, &buf, 0, 0)?;
        if !obj.is_false() {
            return Ok(obj);
        }

        if buf[1] == 0 && buf[0] == b'.' {
            return Ok(inherent_symbols()[InherentSymbol::Dot]);
        }

        if buf[1] == 0 && buf[0] == b'+' {
            return Ok(make_symbol("+", true));
        } else if buf[1] == 0 && buf[0] == b'-' {
            return Ok(make_symbol("-", true));
        } else if &buf[..4] == b"...\0" {
            return Ok(make_symbol("...", true));
        } else if buf[0] == b'-' && buf[1] == b'>' {
            let mut i = 0;
            let mut c;

            while buf[i] != 0 {
                c = buf[i];
                i += 1;
                if c > 127 {
                    continue;
                }

                if (CHAR_MAP[c as usize] & CHAR_MAP_SYMBOL) != 0 {
                    continue;
                }

                return self.lexical_error("invalid identifier");
            }

            return Ok(make_symbol(std::str::from_utf8(&buf[..i]).unwrap(), true));
        }

        Ok(make_symbol(std::str::from_utf8(&buf).unwrap(), true))
    }

    fn read_prefixed_number(
        &mut self,
        exactness: u8,
        radix: u8,
        swap: bool,
    ) -> Result<Value, Value> {
        let mut buf = [0u8; 4096];
        self.read_thing(&mut buf)?;

        let obj = parse_number(self.vm, &buf, exactness, radix)?;

        if !obj.is_false() {
            return Ok(obj);
        }

        if exactness != 0 {
            if radix > 32 {
                if swap {
                    return self.lexical_error(&format!(
                        "invalid lexical syntax #{}#{}{}",
                        radix as char,
                        exactness as char,
                        std::str::from_utf8(&buf).unwrap()
                    ));
                } else {
                    return self.lexical_error(&format!(
                        "invalid lexical syntax #{}#{}{}",
                        exactness as char,
                        radix as char,
                        std::str::from_utf8(&buf).unwrap()
                    ));
                }
            }
        }

        self.lexical_error(&format!(
            "invalid lexical syntax #{}{}",
            radix as char,
            std::str::from_utf8(&buf).unwrap()
        ))
    }

    fn read_exactness(&mut self, radix: u8) -> Result<u8, Value> {
        if self.get_char()? != Some('#') {
            self.unget_char();

            return Ok(10);
        }

        match self.get_char()? {
            Some('e') => Ok(b'e'),
            Some('i') => Ok(b'i'),
            Some('E') => Ok(b'E'),
            Some('I') => Ok(b'I'),
            None => self.lexical_error("unexpected end of file while reading number"),
            Some(c) => {
                self.lexical_error(&format!("invalid lexical syntax #{}{}", c, radix as char))
            }
        }
    }

    fn read_radix(&mut self, exactness: u8) -> Result<u8, Value> {
        if self.get_char()? != Some('#') {
            self.unget_char();

            return Ok(10);
        }
        let c = self.get_char()?;
        match c {
            Some('b') | Some('B') => Ok(b'b'),
            Some('o') | Some('O') => Ok(b'o'),
            Some('d') | Some('D') => Ok(b'd'),
            Some('x') | Some('X') => Ok(b'x'),
            None => self.lexical_error("unexpected end of file while reading number"),
            Some(c) => self.lexical_error(&format!(
                "invalid lexical syntax #{}{}",
                c, exactness as char
            )),
        }
    }

    fn ensure_ucs4(&mut self, c: u32) -> Result<u32, Value> {
        if c as u32 > 0x10ffff {
            self.lexical_error("invalid character literal")
        } else if c as u32 >= 0xd800 && (c as u32) <= 0xdfff {
            self.lexical_error("invalid character literal")
        } else {
            Ok(c)
        }
    }

    fn read_hex_scalar_value(&mut self) -> Result<u32, Value> {
        let mut ucs4 = 0;

        let Some(c) = self.get_char()? else {
            return self.lexical_error("unexpected end of file while reading hex scalar value");
        };

        if delimited(c) {
            return self.lexical_error(&format!("expected hex digit, but got {}", c));
        }

        self.unget_char();

        loop {
            let Some(c) = self.get_char()? else {
                self.unget_char();
                return self.ensure_ucs4(ucs4);
            };

            if delimited(c) {
                self.unget_char();
                return self.ensure_ucs4(ucs4);
            }

            let n = hex_char_to_int(c);
            if n == u32::MAX {
                return self.lexical_error(&format!("expected hex digit, but got {}", c));
            }

            ucs4 = (ucs4 << 4) + n;
            if ucs4 > 0x10ffff {
                return self.lexical_error("invalid character literal");
            }
        }
    }

    fn read_char(&mut self) -> Result<Value, Value> {
        const CHAR_NAME: &[(&'static str, u32)] = &[
            ("nul\0", 0x0000),
            ("null\0", 0x0000),
            ("alarm\0", 0x0007),
            ("backspace\0", 0x0008),
            ("tab\0", 0x0009),
            ("linefeed\0", 0x000A),
            ("newline\0", 0x000A),
            ("vtab\0", 0x000B),
            ("page\0", 0x000C),
            ("return\0", 0x000D),
            ("esc\0", 0x001B),
            ("escape\0", 0x001B),
            ("space\0", 0x0020),
            ("delete\0", 0x007F),
        ];

        let mut c = self.get_char()?;
        if c == Some('x') {
            c = self.lookahead_char()?;
            if c.is_none() {
                return Ok(Value::encode_char('x'));
            }
            let c = c.unwrap();
            if delimited(c) {
                return Ok(Value::encode_char('x'));
            } else {
                return Ok(Value::encode_char(
                    char::from_u32(self.read_hex_scalar_value()?).unwrap(),
                ));
            }
        }

        self.unget_char();

        let mut buf = [0u8; 32];

        if c == Some('(') {
            c = self.get_char()?;

            if c.is_none() {
                return Ok(Value::encode_char('('));
            }

            if delimited(c.unwrap()) {
                return Ok(Value::encode_char('('));
            }

            self.read_thing(&mut buf)?;
            return self.lexical_error(&format!(
                "invalid character literal #\\({}",
                std::str::from_utf8(&buf).unwrap()
            ));
        }

        self.unget_char();

        self.read_thing(&mut buf)?;

        if buf[0] == 0 {
            c = self.get_char()?;

            if c.is_none() {
                return self
                    .lexical_error("unexpected end-of-file while reading character literal");
            }

            return Ok(Value::encode_char(c.unwrap()));
        }

        if buf[1] == 0 {
            return Ok(Value::encode_char(buf[0] as char));
        }

        let mut foldcase = [0u8; 32];
        let mut i = 0;

        if self.foldcase {
            while buf[i] != 0 {
                foldcase[i] = if char::is_ascii(&(buf[i] as char)) {
                    (buf[i] as char).to_ascii_lowercase() as u8
                } else {
                    buf[i]
                };
                i += 1;
            }
        }

        foldcase[i] = 0;

        for i in 0..CHAR_NAME.len() {
            if strcmp(&foldcase, b"null\0") == Ordering::Equal {
                continue;
            }

            if strcmp(&foldcase, b"escape\0") == Ordering::Equal {
                continue;
            }

            if strcmp(&buf, CHAR_NAME[i].0.as_bytes()) == Ordering::Equal {
                return Ok(Value::encode_char(char::from_u32(CHAR_NAME[i].1).unwrap()));
            }

            if self.foldcase && strcmp(&foldcase, CHAR_NAME[i].0.as_bytes()) == Ordering::Equal {
                return Ok(Value::encode_char(char::from_u32(CHAR_NAME[i].1).unwrap()));
            }
        }

        if let Some(c) = char::from_u32(u32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]])) {
            return Ok(Value::encode_char(c));
        } else {
            return self.lexical_error(&format!(
                "invalid character literal #\\{}",
                std::str::from_utf8(&buf).unwrap()
            ));
        }
    }

    fn read_escape_sequence(&mut self) -> Result<u32, Value> {
        match self.get_char()? {
            Some(c) => match c {
                'x' => {
                    let c = self.get_char()?;
                    if c.is_none() {
                        return self
                            .lexical_error("unexpected end-of-file while reading escape sequence");
                    }

                    self.unget_char();
                    let c = self.read_hex_scalar_value()?;
                    if self.get_char()? != Some(';') {
                        return self
                            .lexical_error("inline hex escape missing terminating semi-colon");
                    }
                    return Ok(c);
                }

                'a' => {
                    return Ok(0x0007);
                }

                'b' => {
                    return Ok(0x0008);
                }

                't' => {
                    return Ok(0x0009);
                }

                'n' => {
                    return Ok(0x000A);
                }

                'v' => {
                    return Ok(0x000B);
                }

                'f' => {
                    return Ok(0x000C);
                }

                'r' => {
                    return Ok(0x000D);
                }

                '"' => {
                    return Ok(0x0022);
                }

                '\\' => {
                    return Ok(0x005C);
                }
                '|' => {
                    return Ok(0x007c);
                }

                _ => {
                    return self.lexical_error(&format!("unknown escape sequence #\\{}", c));
                }
            },

            None => self.lexical_error("unexpected end-of-file while reading escape sequence"),
        }
    }

    pub fn read_string(&mut self) -> Result<Value, Value> {
        let mut buf = vec![0; 2048];

        let mut i = 0;

        loop {
            if buf.len() <= i + 4 {
                buf.resize(buf.len() * 2, 0);
            }

            let Some(mut c) = self.get_char()? else {
                return self.lexical_error("unexpected end-of-file while reading string literal");
            };

            match c {
                '\r' => {
                    if let Some(nc) = self.get_char()? {
                        c = nc;

                        if c != '\n' {
                            self.unget_char();
                        }
                    } else {
                        self.unget_char();
                    }
                }

                '\n' => {
                    buf[i] = SCM_PORT_UCS4_LF as u8;
                    i += 1;
                    continue;
                }

                _ => (),
            }

            if c == '"' {
                buf[i] = 0;

                return Ok(
                    make_string(self.vm.mutator(), std::str::from_utf8(&buf[..i]).unwrap()).into(),
                );
            }

            if c == '\\' {
                c = self.get_char()?.ok_or_else(|| {
                    self.lexical_error::<()>("unexpected end-of-file while reading string literal")
                        .unwrap_err()
                })?;
                if c.is_whitespace() {
                    loop {
                        if let None = self.get_char()?.filter(|x| x.is_whitespace()) {
                            break;
                        }
                    }

                    match c {
                        '\r' => {
                            if let Some(nc) = self.get_char()? {
                                c = nc;

                                if c != '\n' {
                                    self.unget_char();
                                }
                            } else {
                                self.unget_char();
                            }
                        }

                        '\n' => loop {
                            if let None = self.get_char()?.filter(|x| x.is_whitespace()) {
                                break;
                            }
                        },

                        _ => {}
                    }
                }

                match c {
                    '\r' => {
                        c = self.get_char()?.ok_or_else(|| {
                            self.lexical_error::<()>(
                                "unexpected end-of-file while reading string literal",
                            )
                            .unwrap_err()
                        })?;

                        if c != '\n' {
                            self.unget_char();
                        }
                    }

                    '\n' => {
                        loop {
                            if let None = self.get_char()?.filter(|x| x.is_whitespace()) {
                                break;
                            }
                        }

                        self.unget_char();
                        continue;
                    }

                    _ => (),
                }

                self.unget_char();
                c = char::from_u32(self.read_escape_sequence()?).ok_or_else(|| {
                    self.lexical_error::<()>("invalid character in string literal")
                        .unwrap_err()
                })?;

                let len_utf8 = c.len_utf8();
                buf[i..i + len_utf8].copy_from_slice(c.encode_utf8(&mut [0; 4]).as_bytes());
                i += len_utf8;

                continue;
            }

            if c.is_ascii() {
                buf[i] = c as u8;
                i += 1;
            } else {
                let utf8_len = c.len_utf8();
                buf[i..i + utf8_len].copy_from_slice(c.encode_utf8(&mut [0; 4]).as_bytes());
                i += utf8_len;
            }
        }
    }

    pub fn read_quoted_symbol(&mut self) -> Result<Value, Value> {
        let mut buf = [0u8; 256];

        let mut i = 0;

        while i + 4 < buf.len() {
            let mut c = self.get_char()?.ok_or_else(|| {
                self.lexical_error::<()>("unexpected end-of-file while reading quoted symbol")
                    .unwrap_err()
            })?;

            if c == '|' {
                buf[i] = 0;
                return Ok(make_symbol(std::str::from_utf8(&buf[..i]).unwrap(), true).into());
            }

            if c == '\\' {
                if self.lookahead_char()? != Some('\\') {
                    c = char::from_u32(self.read_escape_sequence()?).unwrap();
                } else {
                    buf[i] = c as u8;
                    i += 1;
                    c = self.get_char()?.ok_or_else(|| {
                        self.lexical_error::<()>(
                            "unexpected end-of-file while reading quoted symbol",
                        )
                        .unwrap_err()
                    })?;
                }
            }

            if c.is_ascii() {
                buf[i] = c as u8;
                i += 1;
            } else {
                let utf8_len = c.len_utf8();
                buf[i..i + utf8_len].copy_from_slice(c.encode_utf8(&mut [0; 4]).as_bytes());
                i += utf8_len;
            }
        }

        self.lexical_error("token buffer overflow while reading quoted symbol")
    }

    pub fn read_kw_symbol(&mut self) -> Result<Value, Value> {
        let mut buf = [0u8; 256];
        buf[0] = '#' as u8;
        buf[1] = ':' as u8;
        let mut i = 2;
        while i + 4 < buf.len() {
            let mut c = match self.lookahead_char()? {
                Some(c) => c,
                None => {
                    buf[i] = 0;
                    return Ok(make_symbol(std::str::from_utf8(&buf[..i]).unwrap(), true).into());
                }
            };

            if delimited(c) {
                buf[i] = 0;
                return Ok(make_symbol(std::str::from_utf8(&buf[..i]).unwrap(), true).into());
            }

            self.get_char()?;

            if c == '\\' {
                c = self.get_char()?.ok_or_else(|| {
                    self.lexical_error::<()>("unexpected end-of-file while reading symbol")
                        .unwrap_err()
                })?;

                if c == 'x' {
                    self.unget_char();
                    c = char::from_u32(self.read_escape_sequence()?).unwrap();
                    i += c.encode_utf8(&mut buf[i..]).len();
                    continue;
                } else {
                    return self.lexical_error("invalid character in symbol");
                }
            }

            if c as u8 > 127 {
                self.ensure_ucs4(c as u32)?;

                if i == 0 {
                    if is_initial(c) {
                        i += c.encode_utf8(&mut buf[i..]).len();
                        continue;
                    }
                } else {
                    if is_subsequent(c) {
                        i += c.encode_utf8(&mut buf[i..]).len();
                        continue;
                    }
                }

                return self.lexical_error("invalid character in symbol");
            }

            if self.foldcase && c.is_ascii() {
                c = c.to_ascii_lowercase();
            }

            if is_subsequent(c) {
                i += c.encode_utf8(&mut buf[i..]).len();
                continue;
            }

            return self.lexical_error(&format!("invalid character '{}' in symbol", c));
        }

        self.lexical_error("token buffer overflow while reading symbol")
    }

    pub fn read_symbol(&mut self) -> Result<Value, Value> {
        let mut buf = [0u8; 256];

        let mut i = 0;
        while i + 4 < buf.len() {
            let mut c = match self.lookahead_char()? {
                Some(c) => c,
                None => {
                    buf[i] = 0;
                    return Ok(make_symbol(std::str::from_utf8(&buf[..i]).unwrap(), true).into());
                }
            };

            if delimited(c) {
                buf[i] = 0;

                return Ok(make_symbol(std::str::from_utf8(&buf[..i]).unwrap(), true).into());
            }

            self.get_char()?;

            if c == '\\' {
                c = self.get_char()?.ok_or_else(|| {
                    self.lexical_error::<()>("unexpected end-of-file while reading symbol")
                        .unwrap_err()
                })?;

                if c == 'x' {
                    self.unget_char();
                    c = char::from_u32(self.read_escape_sequence()?).unwrap();
                    i += c.encode_utf8(&mut buf[i..]).len();
                    continue;
                } else {
                    return self.lexical_error("invalid character in symbol");
                }
            }

            if c as u8 > 127 {
                self.ensure_ucs4(c as u32)?;

                if i == 0 {
                    if is_initial(c) {
                        i += c.encode_utf8(&mut buf[i..]).len();
                        continue;
                    }
                } else {
                    if is_subsequent(c) {
                        i += c.encode_utf8(&mut buf[i..]).len();
                        continue;
                    }
                }

                return self.lexical_error("invalid character in symbol");
            }

            if self.foldcase && c.is_ascii() {
                c = c.to_ascii_lowercase();
            }

            if is_subsequent(c) {
                i += c.encode_utf8(&mut buf[i..]).len();
                continue;
            }

            return self.lexical_error(&format!("invalid character '{}' in symbol", c));
        }

        self.lexical_error("token buffer overflow while reading symbol")
    }

    fn read_list(&mut self, bracketed: bool, vector: bool) -> Result<Value, Value> {
        let mut lst = Value::encode_null_value();

        let line_begin = self.port.line;
        let mut column_begin = self.port.column - 1;
        if column_begin < 1 {
            column_begin = 1;
        }
        let pos = self.port.mark;
        loop {
            let token = self.read_token()?;
            if token.is_eof_object() {
                break;
            }

            if token == inherent_symbols()[InherentSymbol::RParen] {
                if bracketed {
                    self.parsing_line_from = line_begin;
                    self.parsing_line_to = self.port.line;
                    return self.lexical_error("bracketed list terminated by parenthesis");
                }

                lst = scm_reverse(self.vm.mutator(), lst);
                if self.notes.is_some() {
                    self.put_note(
                        lst,
                        make_srcloc(self.port.name, line_begin, column_begin, pos as _),
                    );
                }
                return Ok(lst);
            }

            if token == inherent_symbols()[InherentSymbol::RBrack] {
                if !bracketed {
                    self.parsing_line_from = line_begin;
                    self.parsing_line_to = self.port.line;
                    return self.lexical_error("parenthesized list terminated by bracket");
                }

                lst = scm_reverse(self.vm.mutator(), lst);
                if self.notes.is_some() {
                    self.put_note(
                        lst,
                        make_srcloc(self.port.name, line_begin, column_begin, pos as _),
                    );
                }
                return Ok(lst);
            }

            if token == inherent_symbols()[InherentSymbol::LParen] {
                let elem = self.read_list(false, false)?;
                lst = scm_cons(self.vm.mutator(), elem, lst);
                continue;
            }

            if token == inherent_symbols()[InherentSymbol::LBrack] {
                let elem = self.read_list(true, false)?;
                lst = scm_cons(self.vm.mutator(), elem, lst);
                continue;
            }

            if token == inherent_symbols()[InherentSymbol::Dot] {
                if vector {
                    return self.lexical_error("misplaced '.' while reading vector");
                }

                if lst.is_null() {
                    self.parsing_line_from = line_begin;
                    self.parsing_line_to = self.port.line;

                    return self.lexical_error("misplaced '.' while reading list");
                }

                let rest = self.read_expr()?;
                if rest == inherent_symbols()[InherentSymbol::Dot] {
                    return self.lexical_error("multiple '.' in list");
                }

                let token = self.read_token()?;

                if token == inherent_symbols()[InherentSymbol::RParen] {
                    if bracketed {
                        self.parsing_line_from = line_begin;
                        self.parsing_line_to = self.port.line;
                        return self.lexical_error("bracketed list terminated by parenthesis");
                    }

                    lst = scm_reverse2x(self.vm.mutator(), lst, rest);
                    if self.notes.is_some() {
                        self.put_note(
                            lst,
                            make_srcloc(self.port.name, line_begin, column_begin, pos as _),
                        );
                    }
                    return Ok(lst);
                }

                if token == inherent_symbols()[InherentSymbol::RBrack] {
                    if !bracketed {
                        self.parsing_line_from = line_begin;
                        self.parsing_line_to = self.port.line;
                        return self.lexical_error("parenthesized list terminated by bracket");
                    }

                    lst = scm_reverse2x(self.vm.mutator(), lst, rest);
                    if self.notes.is_some() {
                        self.put_note(
                            lst,
                            make_srcloc(self.port.name, line_begin, column_begin, pos as _),
                        );
                    }
                    return Ok(lst);
                }

                self.parsing_line_from = line_begin;
                self.parsing_line_to = self.port.line;

                if token.is_eof_object() {
                    return self.lexical_error("unexpected end-of-file after '.'");
                }

                return self.lexical_error("more than one item following '.' while reading list");
            }

            if token.is_pair() {
                if self.notes.is_some() {
                    self.put_note(
                        token,
                        make_srcloc(self.port.name, line_begin, column_begin, pos as _),
                    );
                }
            }

            lst = scm_cons(self.vm.mutator(), token, lst);
        }

        self.parsing_line_from = line_begin;
        self.parsing_line_to = self.port.line;

        self.lexical_error("unexpected end-of-file while reading list")
    }

    fn read_bytevector(&mut self) -> Result<Value, Value> {
        todo!()
    }

    pub fn read_token(&mut self) -> Result<Value, Value> {
        'top: loop {
            let Some(mut c) = self.get_char()? else {
                return Ok(Value::eof_object());
            };
            self.parsing_line_from = self.port.line;
            self.parsing_line_to = self.port.line;

            if c.is_whitespace() {
                continue 'top;
            }

            if c.is_ascii_digit() {
                self.unget_char();
                return self.read_number();
            }

            match c {
                ';' => return self.skip_line(),

                '"' => return self.read_string(),
                '|' => return self.read_quoted_symbol(),
                '(' => return Ok(inherent_symbols()[InherentSymbol::LParen]),
                ')' => return Ok(inherent_symbols()[InherentSymbol::RParen]),
                '[' => return Ok(inherent_symbols()[InherentSymbol::LBrack]),
                ']' => return Ok(inherent_symbols()[InherentSymbol::RBrack]),
                '\'' => {
                    let obj = self.read_expr()?;
                    return Ok(scm_list(
                        self.vm.mutator(),
                        &[inherent_symbols()[InherentSymbol::Quote], obj],
                    ));
                }

                '`' => {
                    let obj = self.read_expr()?;
                    return Ok(scm_list(
                        self.vm.mutator(),
                        &[inherent_symbols()[InherentSymbol::Quasiquote], obj],
                    ));
                }

                '+' | '.' => {
                    self.unget_char();
                    return self.read_number();
                }

                '-' => {
                    self.unget_char();
                    return self.read_number();
                }

                '#' => {
                    c = self.get_char()?.ok_or_else(|| {
                        self.lexical_error::<()>("unexpected end-of-file after #")
                            .unwrap_err()
                    })?;

                    match c {
                        '!' => {
                            let desc = self.read_symbol()?;

                            if desc.strsym() == "fold-case" {
                                self.foldcase = true;
                            } else if desc.strsym() == "no-fold-case" {
                                self.foldcase = false;
                            } else {
                                return self.lexical_error("unknown directive after #!");
                            }

                            continue 'top;
                        }

                        'f' => {
                            let c2 = self.get_char()?;

                            if c2.filter(|&c| delimited(c)).is_some() || c2.is_none() {
                                self.unget_char();
                                return Ok(Value::encode_bool_value(false));
                            }

                            return self.lexical_error("invalid character after #f");
                        }

                        't' => {
                            let c2 = self.get_char()?;

                            if c2.filter(|&c| delimited(c)).is_some() || c2.is_none() {
                                self.unget_char();
                                return Ok(Value::encode_bool_value(true));
                            }

                            return self.lexical_error("invalid character after #t");
                        }

                        '(' => {
                            let mut list = self.read_list(false, true)?;

                            let mut vec = make_vector(self.vm.mutator(), scm_length(list).unwrap());

                            let mut i = 0;

                            while list.is_pair() {
                                self.vm.mutator().write_barrier(vec);
                                vec[i] = list.car();
                                list = list.cdr();
                                i += 1;
                            }

                            if !list.is_null() {
                                return self.lexical_error("improper list in vector literal");
                            }

                            return Ok(vec.into());
                        }

                        '|' => {
                            return self.skip_srfi30();
                        }
                        '\\' => {
                            return self.read_char();
                        }

                        ';' => {
                            if self.read_expr()? == inherent_symbols()[InherentSymbol::Dot] {
                                return self
                                    .lexical_error("misplaced dot('.') immediately after '#;'");
                            }

                            continue 'top;
                        }

                        'B' | 'b' => {
                            let exactness = self.read_exactness(b'b')?;
                            return self.read_prefixed_number(exactness, b'b', true);
                        }
                        'd' | 'D' => {
                            let exactness = self.read_exactness(b'd')?;
                            return self.read_prefixed_number(exactness, b'd', false);
                        }

                        'x' | 'X' => {
                            let exactness = self.read_exactness(b'x')?;
                            return self.read_prefixed_number(exactness, b'x', false);
                        }

                        'i' | 'I' => {
                            let radix = self.read_radix(c as u8);
                            return self.read_prefixed_number(c as _, radix?, false);
                        }

                        'e' | 'E' => {
                            let radix = self.read_radix(c as u8);
                            return self.read_prefixed_number(c as _, radix?, false);
                        }

                        '\'' => {
                            let expr = self.read_expr()?;
                            return Ok(scm_list(
                                self.vm.mutator(),
                                &[inherent_symbols()[InherentSymbol::Syntax], expr],
                            ));
                        }

                        '`' => {
                            let expr = self.read_expr()?;
                            return Ok(scm_list(
                                self.vm.mutator(),
                                &[inherent_symbols()[InherentSymbol::Quasisyntax], expr],
                            ));
                        }

                        ',' => {
                            let c2 = self.get_char()?.ok_or_else(|| {
                                self.lexical_error::<()>("unexpected end-of-file after #")
                                    .unwrap_err()
                            })?;

                            if c2 == '@' {
                                let expr = self.read_expr()?;
                                return Ok(scm_list(
                                    self.vm.mutator(),
                                    &[inherent_symbols()[InherentSymbol::UnsyntaxSplicing], expr],
                                ));
                            } else {
                                self.unget_char();
                                let expr = self.read_expr()?;
                                return Ok(scm_list(
                                    self.vm.mutator(),
                                    &[inherent_symbols()[InherentSymbol::Unsyntax], expr],
                                ));
                            }
                        }

                        ':' => {
                            let sym = self.read_kw_symbol()?;

                            return Ok(sym);
                        }

                        _ => {
                            if c == 'u' {
                                self.unget_char();

                                return self.read_bytevector();
                            }
                        }
                    }
                    return self.lexical_error("unknown directive after #");
                }

                ',' => {
                    c = self.get_char()?.ok_or_else(|| {
                        self.lexical_error::<()>("unexpected end-of-file after #")
                            .unwrap_err()
                    })?;

                    if c == '@' {
                        let expr = self.read_expr()?;
                        return Ok(scm_list(
                            self.vm.mutator(),
                            &[inherent_symbols()[InherentSymbol::UnquoteSplicing], expr],
                        ));
                    } else {
                        self.unget_char();
                        let expr = self.read_expr()?;
                        return Ok(scm_list(
                            self.vm.mutator(),
                            &[inherent_symbols()[InherentSymbol::Unquote], expr],
                        ));
                    }
                }

                _ => {
                    self.unget_char();
                    return self.read_symbol();
                }
            }
        }
    }

    pub fn read_expr(&mut self) -> Result<Value, Value> {
        let token = self.read_token()?;
        if token == inherent_symbols()[InherentSymbol::RParen] {
            self.lexical_error("unexpected ')'")
        } else if token == inherent_symbols()[InherentSymbol::RBrack] {
            self.lexical_error("unexpected ']'")
        } else if token == inherent_symbols()[InherentSymbol::LParen] {
            self.read_list(false, false)
        } else if token == inherent_symbols()[InherentSymbol::LBrack] {
            self.read_list(true, false)
        } else {
            Ok(token)
        }
    }

    pub fn read(&mut self, note: Option<Handle<HashMap<Value, Value>>>) -> Result<Value, Value> {
        self.notes = note;

        if self.notes.is_some() {
            self.put_note(".&SOURCE_PATH".intern().into(), self.port.name);
        }

        let obj = self.read_expr()?;

        if obj == inherent_symbols()[InherentSymbol::Dot] {
            return self.lexical_error("misplaced dot('.') in expression");
        }

        self.parsing_line_from = self.first_line;
        self.parsing_line_to = self.port.line;

        Ok(obj)
    }
}
