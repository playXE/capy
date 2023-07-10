#![allow(dead_code)]
use std::collections::hash_map::RandomState;

use r7rs_parser::lexer::scanner::is_digit;
use rsgc::prelude::Handle;
use rsgc::system::collections::hashmap::HashMap;

use crate::compaux::scm_outermost_identifier;


use super::bigint::BigInt;
use super::fun::SCM_PRIM_TYPE_PARAMETER;
use super::port::*;
use super::{object::*, value::*};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum EscapeMode {
    String,
    Symbol,
}

/// Formats a Scheme value as a string.
#[allow(dead_code)]
pub struct Printer<'a> {
    vm: &'a mut crate::vm::VM,
    port: Handle<Port>,
    column_limit: i32,
    shared_tag: i32,
    radix: i32,
    pub escape: bool,
    pub unwrap: bool,
    flush: bool,
    r6rs: bool,
}

const WRITE_STRING_ESCAPE_CODES: [u8; 9] = [7, 8, 9, 10, 11, 12, 13, 92, 0];
const WRITE_STRING_ESCAPE_NAMES: [&str; 8] = ["a", "b", "t", "n", "v", "f", "r", "\\"];

impl<'a> Printer<'a> {  
    pub fn new(vm: &'a mut crate::vm::VM, port: Handle<Port>) -> Self {
        Self {
            vm,
            port,
            column_limit: 0,
            shared_tag: 0,
            radix: 10,
            escape: false,
            unwrap: false,
            flush: false,
            r6rs: false,
        }
    }

    pub fn set_column_limit(&mut self, limit: i32) {
        self.column_limit = limit;
    }

    pub fn flush(&mut self) -> Result<(), Value> {
        port_flush_output(self.port)
    }

    pub fn symbol_needs_bar(&self, s: &str) -> bool {
        let s = s.as_bytes();

        if s[0] == 0 || is_digit(s[0] as char) {
            return true;
        }

        match s[0] {
            b'@' => return true,
            b'+' => {
                if s[1] == 0 {
                    return false;
                }

                if s[1] == b'`' && self.unwrap {
                    return false;
                }
                return true;
            }

            b'-' => {
                if s[1] == 0 {
                    return false;
                }

                if s[1] == b'`' && self.unwrap {
                    return false;
                }

                if s[1] != b'>' {
                    return true;
                }
            }
            b'.' => {
                if s[1] != b'.' {
                    return true;
                }

                if s[2] != b'.' {
                    return true;
                }

                if s[3] == 0 {
                    return false;
                }

                if s[3] == b'`' && self.unwrap {
                    return true;
                }
            }

            _ => (),
        }

        let mut i = 0;
        while i < s.len() && s[i] != 0 {
            let c = s[i];

            if c < 128 && (c as char).is_alphanumeric() && !"!$%&/:*<=>?^_~+-.@".contains(c as char)
            {
                return true;
            }

            i = i + utf8_byte_count(c);
        }

        false
    }

    pub fn write_string(&mut self, utf8: &[u8]) -> Result<(), Value> {
        let mut i = 0;
        let mut ucs4 = 0;

        while i < utf8.len() {
            if utf8[i] < 128 {
                let c = utf8[i];
                if c == b'"' {
                    port_put_byte(self.port, b'\\')?;
                    port_put_byte(self.port, c)?;
                } else {
                    let p = WRITE_STRING_ESCAPE_CODES.iter().position(|&x| x == c);

                    if let Some(p) = p {
                        port_put_byte(self.port, b'\\')?;
                        port_put_byte(self.port, WRITE_STRING_ESCAPE_NAMES[p].as_bytes()[0])?;
                    } else {
                        if c < 32 || c == 127 {
                            let s = format!("\\x{:x}", c);
                            port_puts(self.port, &s)?;
                        } else {
                            port_put_byte(self.port, c)?;
                        }
                    }
                }

                i += 1;
                continue;
            } else {
                let bytes = cnvt_utf8_to_ucs4(&utf8[i..], &mut ucs4);

                if bytes < 0 {
                    panic!("invalid UTF-8: {:?}", utf8);
                }

                self.write_ucs4(ucs4)?;

                i += bytes as usize;
            }
        }

        Ok(())
    }

    fn write_pretty_symbol(&mut self, utf8: &[u8]) -> Result<(), Value> {
        let quote = self.symbol_needs_bar(unsafe { std::str::from_utf8_unchecked(utf8) });
        if quote {
            port_put_byte(self.port, b'|')?;
        }

      
        let mut i = 0;

        while i < utf8.len() {
            if utf8[i] < 128 {
                let c = utf8[i];
                if c == b'|' {
                    port_put_byte(self.port, b'\\')?;
                    port_put_byte(self.port, c)?;
                } else if c < 32 || c == 127 {
                    port_puts(self.port, &format!("\\x{:x}", c))?;
                } else {
                    port_put_byte(self.port, c)?;
                }

                i += 1;
            } else {
                port_put_byte(self.port, utf8[i])?;
                i += 1;
            }
        }

        Ok(())
    }

    pub fn write_ucs4(&mut self, ucs4: u32) -> Result<(), Value> {
        let mut utf8 = [0; 4];
        let bytes = cnvt_ucs4_to_utf8(ucs4, &mut utf8);

        for i in 0..bytes {
            port_put_byte(self.port, utf8[i])?;
        }

        Ok(())
    }

    pub fn ucs4(&mut self, ucs4: u32) -> Result<(), Value> {
        self.write_ucs4(ucs4)
    }

    pub fn byte(&mut self, b: u8) -> Result<(), Value> {
        port_put_byte(self.port, b)
    }

    pub fn puts(&mut self, s: &str) -> Result<(), Value> {
        port_puts(self.port, s)
    }

    pub fn write_abbreviated(&mut self, obj: Value) -> Result<bool, Value> {
        if obj.is_symbol() {
            match obj.strsym() {
                "quote" => {
                    self.puts("'")?;
                    Ok(true)
                }
                "quasiquote" => {
                    self.puts("`")?;
                    Ok(true)
                }
                "unquote" => {
                    self.puts(",")?;
                    Ok(true)
                }
                "unquote-splicing" => {
                    self.puts(",@")?;
                    Ok(true)
                }

                _ => Ok(false),
            }
        } else {
            Ok(false)
        }
    }

    pub fn scan(&mut self, mut ht: Handle<HashMap<Value, Value>>, obj: Value) {
        let value = ht.get(&obj);

        if let Some(val) = value {
            if val.is_true() {
                return;
            }

            if val.is_false() {
                ht.put(self.vm.mutator(), obj, Value::encode_bool_value(true));
                return;
            }
        }

        if obj.is_pair() {
            ht.put(self.vm.mutator(), obj, Value::encode_bool_value(false));
            self.scan(ht, obj.car());
            self.scan(ht, obj.cdr());
            return;
        }

        if obj.is_vector() {
            ht.put(self.vm.mutator(), obj, Value::encode_bool_value(false));
            let len = obj.vector_len();
            for i in 0..len {
                self.scan(ht, obj.vector_ref(i));
            }
            return;
        }

        if obj.is_struct() {
            ht.put(self.vm.mutator(), obj, Value::encode_bool_value(false));
            let s = obj.structure();
            let len = s.slots.len();
            for i in 0..len {
                self.scan(ht, s.slots[i]);
            }
            return;
        }
    }

    pub fn write(&mut self, obj: Value) -> Result<(), Value> {
        let ht = HashMap::with_hasher_and_capacity(RandomState::new(), 4);
        self.scan(ht, obj);
        self._write(Some(ht), obj)
    }

    pub fn _write(
        &mut self,
        ht: Option<Handle<HashMap<Value, Value>>>,
        mut obj: Value,
    ) -> Result<(), Value> {
        /*if let Some(mut ht) = ht {
            let value = ht.get(&obj);

            if let Some(value) = value {
                if value.is_int32() {
                    port_puts(self.port, &format!("#{}", value.get_int32()))?;
                    return Ok(());
                }

                if value.is_false() {
                    port_puts(self.port, &format!("#{}#", self.shared_tag))?;
                    self.shared_tag += 1;
                    ht.put(
                        self.vm.mutator(),
                        obj,
                        Value::encode_int32(self.shared_tag - 1),
                    );
                    return Ok(());
                }
            }
        }*/

        if obj.is_pair() {
            let abbreviated =
                obj.cdr().is_pair() && obj.cddr().is_null() && self.write_abbreviated(obj.car())?;

            if abbreviated {
                obj = obj.cdr();
            } else {
                port_put_byte(self.port, b'(')?;
            }

            let mut head = true;

            let mut e = obj;

            while !e.is_null() {
                if head {
                    head = false;
                } else {
                    port_put_byte(self.port, b' ')?;
                }

                if e.is_pair() {
                    if let Some(ht) = ht {
                        if let Some(_) = ht.get(&e.cdr()).filter(|x| x.is_int32() || x.is_true()) {
                            self._write(Some(ht), e.car())?;
                            port_puts(self.port, " . ")?;
                            self._write(Some(ht), e.cdr())?;
                            break;
                        }
                    }

                    if false && e.car().is_symbol() && e.car().strsym() == "unquote" {
                        if e.cdr().is_pair() && e.cddr().is_null() {
                            port_puts(self.port, ". ,")?;
                            self._write(ht, e.cadr())?;
                            break;
                        }
                    }

                    self._write(ht, e.car())?;

                    if self.column_limit != 0 && self.port.column > self.column_limit {
                        port_puts(self.port, " ...)")?;
                        return Ok(());
                    }
                    e = e.cdr();
                } else {
                    port_puts(self.port, ". ")?;
                    self._write(ht, e)?;
                    break;
                }
            }

            if !abbreviated {
                port_put_byte(self.port, b')')?;
            }

            return Ok(());
        } else if obj.is_int32() {
            self.puts(obj.get_int32().to_string().as_str())
        } else if obj.is_true() {
            self.puts("#t")
        } else if obj.is_false() {
            self.puts("#f")
        } else if obj.is_null() {
            self.puts("()")
        } else if obj.is_undefined() {
            self.puts("#<undefined>")
        } else if obj.is_eof_object() {
            self.puts("#<eof>")
        } else if obj.is_char() {
            self.puts(obj.get_char().to_string().as_str())
        } else if obj.is_string() {
            if self.escape {
                self.puts("\"")?;
                self.write_string(obj.string().as_bytes())?;
                self.puts("\"")
            } else {
                self.puts(&*obj.string())
            }
        } else if obj.is_port() {
            let port = obj.port();

            if port.direction == SCM_PORT_DIRECTION_BOTH {
                if port_textual_pred(port) {
                    self.puts("#<textual-port>")
                } else {
                    self.puts("#<binary-port>")
                }
            } else if port.direction == SCM_PORT_DIRECTION_IN {
                if port_textual_pred(port) {
                    self.puts("#<input-port>")
                } else {
                    self.puts("#<binary-input-port>")
                }
            } else {
                if port_textual_pred(port) {
                    self.puts("#<output-port>")
                } else {
                    self.puts("#<binary-output-port>")
                }
            }
        } else if obj.is_symbol() {
            self.puts(obj.strsym())
        } else if obj.is_vector() {
            self.puts("#(")?;

            let len = obj.vector_len();

            for i in 0..len {
                if i != 0 {
                    self.puts(" ")?;
                }

                self._write(ht, obj.vector_ref(i))?;
            }

            self.puts(")")
        } else if obj.is_bytevector() {
            self.puts("#u8(")?;

            let len = obj.bytevector_len();

            for i in 0..len {
                if i != 0 {
                    self.puts(" ")?;
                }

                self.puts(obj.bytevector_ref(i).to_string().as_str())?;
            }

            self.puts(")")
        } else if obj.is_values() {
            self.puts("#<values>")?;
            Ok(())
        } else if obj.is_procedure() {
            match obj.get_type() {
                Type::Procedure => {
                    let name = obj.procedure().code.name;
                    self.puts("#<procedure ")?;
                    self._write(ht, name)?;
                    self.puts(">")
                }

                _ => {
                    let name = obj.native_procedure().name;
                    if (obj.object_header().flags & SCM_PRIM_TYPE_PARAMETER as u32) != 0 {
                        self.puts(&format!("#<parameter {}>", name))
                    } else {
                        self.puts(&format!("#<procedure {}>", name))
                    }
                }
            }
        } else if obj.is_wrapped_identifier() {
            let obj = scm_outermost_identifier(obj.identifier());
            self.puts(&format!("#<identifier "))?;
            self._write(ht, obj.module.module().name)?;
            self.puts("@")?;
            self._write(ht, obj.name)?;
            self.puts(&format!(".{:p}", obj.as_ptr()))?;
            self.puts(">")
        } else if obj.is_double() {
            self.puts(obj.get_double().to_string().as_str())
        } else if obj.is_struct() {
            self.puts("#<")?;
            self._write(ht, obj.structure().type_.name)?;
            self.puts(">")
        } else if obj.is_bignum() {
            self.puts(&obj.bignum().to_string(&BigInt::DEC_BASE))
        } else if obj.is_box() {
            self.puts("#<box ")?;
            self._write(ht, obj.box_ref())?;
            self.puts(">")
        
        } else {
            self.puts(&format!("#<unknown {:?}@{:x}>", obj.get_type(), obj.get_raw()))
        }
    }
}
