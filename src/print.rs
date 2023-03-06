//! Implementation of Scheme values printer.

use std::collections::hash_map::RandomState;

use rsgc::{prelude::Handle, system::collections::hashmap::HashMap};

use crate::{ports_v2::*, value::{Value, Type}};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum EscapeMode {
    String,
    Symbol,
}

/// Formats a Scheme value as a string.
#[allow(dead_code)]
pub struct Printer<'a> {
    vm: &'a mut crate::vm::Vm,
    port: Handle<Port>,
    column_limit: i32,
    shared_tag: i32,
    radix: i32,
    escape: bool,
    unwrap: bool,
    flush: bool,
    r6rs: bool,
}

const WRITE_STRING_ESCAPE_CODES: [u8; 9] = [7, 8, 9, 10, 11, 12, 13, 92, 0];
const WRITE_STRING_ESCAPE_NAMES: [&str; 8] = ["a", "b", "t", "n", "v", "f", "r", "\\"];

impl<'a> Printer<'a> {
    
    pub fn new(vm: &'a mut crate::vm::Vm, port: Handle<Port>) -> Self {
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

    pub fn symbol_need_bar(&self, _s: &str) -> bool {
        /*if s.is_empty() || s.starts_with(|c: char| c.is_digit(10)) {
            return true;
        }
        let chars = s.chars();
        match chars.next().unwrap() {
            '@' => true,
            '+' => {
                if chars.next().is_none() {
                    return true;
                }
                let x = chars.next().unwrap();
                if x == '`' && self.unwrap {
                    return false;
                }
                return true;
            }
            '-' => {
                if chars.next().is_none() {
                    return true;
                }
                let x = chars.next().unwrap();
                if x == '`' && self.unwrap {
                    return false;
                }
                if x != '>' {
                    return true;
                }
            }

            '.' => {
                if chars.next() != Some('.') {
                    return true;
                }

                if chars.next() != Some('.') {
                    return true;
                }

                if chars.next().is_none() {
                    return false;
                }

                if chars.next() == Some('`') && self.unwrap {
                    return true;
                }

                return true;
            }
            _ => ()
        }
        */
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
        if obj.symbolp() {
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
            if val.truep() {
                return;
            }

            if val.falsep() {
                ht.put(self.vm.mutator(), obj, Value::make_true());
                return;
            }
        } 

        if obj.pairp() {
            ht.put(self.vm.mutator(), obj, Value::make_false());
            self.scan(ht, obj.car());
            self.scan(ht, obj.cdr());
            return;
        }

        if obj.vectorp() {
            ht.put(self.vm.mutator(), obj, Value::make_false());
            let len = obj.vector_len();
            for i in 0..len {
                self.scan(ht, obj.vector_ref(i));
            }
            return;
        }
    }

    pub fn write(&mut self, obj: Value) -> Result<(), Value> {
        let ht = HashMap::with_hasher_and_capacity(RandomState::new(), 4);
        self.scan(ht, obj);
        self._write(Some(ht), obj)
    }

    pub fn _write(&mut self, ht: Option<Handle<HashMap<Value, Value>>>, mut obj: Value) -> Result<(), Value> {
        if let Some(mut ht) = ht {
            let value = ht.get(&obj);

            if let Some(value) = value {
                if value.intp() {
                    port_puts(self.port, &format!("#{}", value.int()))?;
                    return Ok(());
                }

                if value.truep() {
                    port_puts(self.port, &format!("#{}#", self.shared_tag))?;
                    self.shared_tag += 1;
                    ht.put(self.vm.mutator(), obj, Value::make_int(self.shared_tag - 1));
                    return Ok(());
                }
            }
        }

        if obj.pairp() {
            let abbreviated = obj.cdr().pairp() && obj.cddr().nullp() && self.write_abbreviated(obj.car())?;

            if abbreviated {
                obj = obj.cdr();
            } else {
                port_put_byte(self.port, b'(')?;
            }

            let mut head = true;

            let mut e = obj;

            while !e.nullp() {
                if head {
                    head = false;
                } else {
                    port_put_byte(self.port, b' ')?;
                }

                if e.pairp() {
                    if let Some(ht) = ht {
                        if let Some(_) = ht.get(&e.cdr()).filter(|x| x.intp() || x.truep()) {
                            self._write(Some(ht), e.car())?;
                            port_puts(self.port, " . ")?;
                            self._write(Some(ht), e.cdr())?;
                            break;
                        }
                    }

                    if e.car().symbolp() && e.car().strsym() == "unquote" {
                        if e.cdr().pairp() && e.cddr().nullp() {
                            port_puts(self.port, ". ,")?;
                            self._write(ht, e.cadr())?;
                            break;
                        }
                    }

                    self._write(ht, e.car())?;

                    if self.column_limit != 0  && self.port.column > self.column_limit {
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
        } else if obj.intp() {
            self.puts(obj.int().to_string().as_str())
        } else if obj.truep() {
            self.puts("#t")
        } else if obj.falsep() {
            self.puts("#f")
        } else if obj.nullp() {
            self.puts("()")
        } else if obj.voidp() {
            self.puts("#<void>")
        } else if obj.eofp() {
            self.puts("#<eof>")
        } else if obj.charp() {
            self.puts(obj.char_val().to_string().as_str())
        } else if obj.strp() {
            self.write_string(obj.str().as_bytes())
        } else if obj.symbolp() {
            self.puts(obj.strsym())
        } else if obj.vectorp() {
            self.puts("#(")?;

            let len = obj.vector_len();

            for i in 0..len {
                if i != 0 {
                    self.puts(" ")?;
                }

                self._write(ht, obj.vector_ref(i))?;
            }

            self.puts(")")
        } else if obj.byte_vectorp() {
            self.puts("#u8(")?;

            let len = obj.byte_vector_len();

            for i in 0..len {
                if i != 0 {
                    self.puts(" ")?;
                }

                self.puts(obj.byte_vector_ref(i).to_string().as_str())?;
            }

            self.puts(")")
        } else if obj.valuesp() {
            self.puts("#<values>")?;
            Ok(())
        } else if obj.procedurep() {
            match obj.get_type() {
                Type::PrimitiveProcedure => {
                    let name = obj.downcast_primitive_proc().name;
                    self.puts(&format!("#<procedure {}>", name.symbol_str()))
                }
                Type::ClosedPrimitiveProcedure => {
                    let name = obj.downcast_closed_primitive_proc().name;
                    self.puts(&format!("#<procedure {}>", name.symbol_str()))
                }
                Type::NativeProcedure => {
                    return self.puts("#<procedure>");
                }

                Type::ReturnCont => self.puts("#<return-continuation>"),
                Type::Parameter => self.puts("#<parameter>"),
                _ => unreachable!(),
            }
        } else if obj.doublep() {
            self.puts(obj.double_val().to_string().as_str())
         }else if obj.structp() {
            self.puts("#<")?;
            self._write(ht, obj.struct_stype().name)?;
            self.puts(">")
        } else if obj.parameterp() {
            self.puts(&format!("#<parameter {:p}>", obj.downcast_parameter()))
        } else {
            self.puts(&format!("#<unknown {:x}:{:?}>", obj.raw(), obj.get_type()))
        }
    }
}
