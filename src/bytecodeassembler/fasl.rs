use std::collections::HashMap;

use crate::{
    compiler::sexpr::Sexpr,
    runtime::object::{scm_symbol_str, TypeId},
};

pub struct FASLPrinter<'a, W: std::io::Write> {
    lites: HashMap<Sexpr, usize>,
    stack: Vec<Sexpr>,
    writer: &'a mut W,
}
pub const TAG_LOOKUP: u8 = 253;
pub const TAG_PLIST: u8 = 254;
pub const TAG_DLIST: u8 = 255;

#[allow(dead_code)]
impl<'a, W: std::io::Write> FASLPrinter<'a, W> {
    fn emit_u8(&mut self, value: u8) -> std::io::Result<()> {
        self.writer.write_all(&[value])
    }

    fn emit_u16(&mut self, value: u16) -> std::io::Result<()> {
        self.writer.write_all(&value.to_le_bytes())
    }

    fn emit_u32(&mut self, value: u32) -> std::io::Result<()> {
        self.writer.write_all(&value.to_le_bytes())
    }

    fn emit_u64(&mut self, value: u64) -> std::io::Result<()> {
        self.writer.write_all(&value.to_le_bytes())
    }

    fn write_bytes(&mut self, bytes: &[u8]) -> std::io::Result<()> {
        self.writer.write_all(bytes)
    }

    fn push(&mut self, sexpr: Sexpr) {
        self.stack.push(sexpr);
    }

    fn pop(&mut self) -> Option<Sexpr> {
        self.stack.pop()
    }

    fn scan(&mut self, mut obj: Sexpr) {
        loop {
            match obj {
                Sexpr::Null => return,
                Sexpr::Symbol(_) | Sexpr::String(_) => {
                    let n = self.lites.len();
                    self.lites.insert(obj.clone(), n);
                    return;
                }

                Sexpr::Pair(ref pair) => {
                    self.scan(pair.0.clone());
                    obj = pair.1.clone();
                    continue;
                }

                Sexpr::Vector(ref vector) => {
                    if vector.len() == 0 {
                        return;
                    }

                    for elt in vector.iter() {
                        self.scan(elt.clone());
                    }

                    continue;
                }

                _ => break,
            }
        }
    }

    fn put_list(&mut self, mut obj: Sexpr) -> std::io::Result<()> {
        let mut count = 0;

        while obj.is_pair() {
            self.push(obj.car());
            obj = obj.cdr();
            count += 1;
        }

        if obj.is_null() {
            self.emit_u8(TAG_PLIST)?;
            self.emit_u32(count as u32)?;
        } else {
            self.emit_u8(TAG_DLIST)?;
            self.emit_u32(count as u32)?;
            self.put_datum(obj)?;
        }

        while count != 0 {
            let obj = self.pop().unwrap();
            self.put_datum(obj)?;
            count -= 1;
        }

        Ok(())
    }

    fn put_datum(&mut self, obj: Sexpr) -> std::io::Result<()> {
        match obj {
            Sexpr::Null => {
                self.emit_u8(TypeId::Null as _)?;
            }

            Sexpr::Undefined => {
                self.emit_u8(TypeId::Undefined as _)?;
            }

            Sexpr::Boolean(x) => {
                if x {
                    self.emit_u8(TypeId::True as _)?;
                } else {
                    self.emit_u8(TypeId::False as _)?;
                }
            }

            Sexpr::Symbol(_) | Sexpr::String(_) => {
                let id = *self.lites.get(&obj).unwrap();
                self.emit_u8(TAG_LOOKUP)?;
                self.emit_u32(id as u32)?;
            }

            Sexpr::Fixnum(fix) => {
                self.emit_u8(TypeId::Int32 as _)?;
                self.emit_u32(fix as u32)?;
            }

            Sexpr::Flonum(flo) => {
                self.emit_u8(TypeId::Double as _)?;
                self.emit_u64(flo.to_bits())?;
            }

            Sexpr::Char(ch) => {
                self.emit_u8(TypeId::Char as _)?;
                self.emit_u32(ch as u32)?;
            }

            Sexpr::Program(program) => {
                self.emit_u8(TypeId::Program as _)?;
                self.emit_u32(program.0)?;
            }

            Sexpr::Global(global) => {
                let id = *self.lites.get(&Sexpr::Symbol(global)).unwrap();
                self.emit_u8(TypeId::GLOC as _)?;
                self.emit_u32(id as u32)?;
            }

            Sexpr::Bytevector(bvec) => {
                self.emit_u8(TypeId::Bytevector as _)?;
                self.emit_u32(bvec.len() as u32)?;
                self.write_bytes(&bvec)?;
            }

            Sexpr::Vector(vec) => {
                self.emit_u8(TypeId::Vector as _)?;
                self.emit_u32(vec.len() as u32)?;
                for elt in vec.iter() {
                    self.put_datum(elt.clone())?;
                }
            }

            Sexpr::Pair(_) => {
                self.put_list(obj)?;
            }

            _ => (),
        }

        Ok(())
    }

    fn put_lites(&mut self) -> std::io::Result<()> {
        let lites = self
            .lites
            .iter()
            .map(|(x, y)| (x.clone(), y.clone()))
            .collect::<Vec<_>>();

        self.emit_u32(lites.len() as _)?;

        for i in 0..lites.len() {
            let symbol = lites[i].0.clone();

            match symbol {
                Sexpr::Symbol(sym) => {
                    self.emit_u8(TypeId::Symbol as _)?;
                    self.emit_u32(scm_symbol_str(sym).len() as _)?;
                    self.write_bytes(scm_symbol_str(sym).as_bytes())?;
                }
                Sexpr::String(str) => {
                    self.emit_u8(TypeId::String as _)?;
                    self.emit_u32(str.len() as _)?;
                    self.write_bytes(str.as_bytes())?;
                }

                _ => unreachable!(),
            }
        }

        Ok(())
    }

    pub fn put(&mut self, obj: Sexpr) -> std::io::Result<()> {
        self.scan(obj.clone());
        self.put_lites()?;
        self.put_datum(obj)?;
        Ok(())
    }

    pub fn new(writer: &'a mut W) -> Self {
        Self {
            writer,
            stack: Vec::new(),
            lites: HashMap::new(),
        }
    }
}
