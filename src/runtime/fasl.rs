#![allow(dead_code)]
use super::{object::TypeId, symbol::scm_intern, value::Value};
use crate::gc_frame;
use crate::runtime::object::{scm_set_car, scm_set_cdr, scm_vector_set};
use crate::vm::scm_virtual_machine;
use crate::vm::thread::Thread;
use std::io::Read;
use std::{collections::HashMap, io::Cursor};

pub const TAG_LOOKUP: u8 = 253;
pub const TAG_PLIST: u8 = 254;
pub const TAG_DLIST: u8 = 255;

pub struct FASLReader<'a, const IMMORTAL: bool, R: std::io::Read + AsRef<[u8]>> {
    buffer: &'a mut Cursor<R>,
    lites: HashMap<u32, Value>,
    pub programs: Vec<Value>,
    code: Option<&'a [u8]>,
}

impl<'a, const IMMORTAL: bool, R: std::io::Read + AsRef<[u8]>> FASLReader<'a, IMMORTAL, R> {
    pub fn new(buffer: &'a mut Cursor<R>, code: Option<&'a [u8]>) -> Self {
        Self {
            buffer,
            lites: HashMap::new(),
            programs: Vec::new(),
            code,
        }
    }

    fn read_u32(&mut self) -> std::io::Result<u32> {
        let mut buf = [0u8; 4];
        self.buffer.read_exact(&mut buf)?;
        Ok(u32::from_le_bytes(buf))
    }

    fn read_u8(&mut self) -> std::io::Result<u8> {
        let mut buf = [0u8; 1];
        self.buffer.read_exact(&mut buf)?;
        Ok(u8::from_le_bytes(buf))
    }

    fn read_u16(&mut self) -> std::io::Result<u16> {
        let mut buf = [0u8; 2];
        self.buffer.read_exact(&mut buf)?;
        Ok(u16::from_le_bytes(buf))
    }

    fn read_u64(&mut self) -> std::io::Result<u64> {
        let mut buf = [0u8; 8];
        self.buffer.read_exact(&mut buf)?;
        Ok(u64::from_le_bytes(buf))
    }

    pub fn get_lites(&mut self) -> std::io::Result<()> {
        let count = self.read_u32()?;
        self.lites = HashMap::with_capacity(count as _);
        let mut buf = vec![0u8; 256];
        println!("count: {}", count);
        for _ in 0..count {
            let tag = self.read_u8()?;
            let uid = self.read_u32()?;
            let size = self.read_u32()?;
            if size > buf.len() as u32 {
                buf.resize(size as _, 0);
            }

            self.buffer.read_exact(&mut buf[..size as usize])?;

            let str = std::str::from_utf8(&buf[..size as usize]).map_err(|_| {
                std::io::Error::new(std::io::ErrorKind::InvalidData, "invalid utf8")
            })?;
            match tag {
                x if x == TypeId::Symbol as u8 => {
                    println!("symbol: {}", str);
                    self.lites.insert(uid, scm_intern(str));
                }

                x if x == TypeId::String as u8 => {
                    println!("string: {}", str);
                    // allocate string in immortal space, it cannot be garbage collected.
                    let str = Thread::current().make_string::<true>(str);
                    self.lites.insert(uid, str);
                }

                _ => {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "invalid tag",
                    ))
                }
            }
        }

        Ok(())
    }

    pub fn get_datum(&mut self) -> std::io::Result<Value> {
        let tag = self.read_u8()?;
        
        match tag {
            TAG_LOOKUP => {
                let uid = self.read_u32()?;
                return Ok(self.lites.get(&uid).unwrap().clone());
            }

            x if x == TypeId::Undefined as u8 => {
                return Ok(Value::encode_undefined_value());
            }

            x if x == TypeId::Null as u8 => {
                return Ok(Value::encode_null_value());
            }

            x if x == TypeId::True as u8 => {
                return Ok(Value::encode_bool_value(true));
            }

            x if x == TypeId::False as u8 => {
                return Ok(Value::encode_bool_value(false));
            }

            x if x == TypeId::Int32 as u8 => {
                let val = self.read_u32()?;
                return Ok(Value::encode_int32(val as _));
            }

            x if x == TypeId::Double as u8 => {
                let val = self.read_u64()?;
                return Ok(Value::encode_f64_value(f64::from_bits(val)));
            }

            x if x == TypeId::Char as u8 => {
                let val = self.read_u32()?;
                return Ok(Value::encode_char(char::from_u32(val).ok_or_else(
                    || std::io::Error::new(std::io::ErrorKind::InvalidData, "invalid char"),
                )?));
            }

            x if x == TypeId::Program as u8 => {
                let offset = self.read_u32()?;
                if let Some(code) = self.code {
                    let vcode = code[offset as usize..].as_ptr();
                    let program = Thread::current().make_program::<IMMORTAL>(vcode, 0);
                    self.programs.push(program);
                    return Ok(program);
                } else {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "no code provided",
                    ));
                }
            }

            x if x == TypeId::GLOC as u8 => {
                let uid = self.read_u32()?;

                let sym = self.lites.get(&uid).unwrap().clone();
                return Ok(scm_virtual_machine().get_cell(sym));
            }

            x if x == TypeId::Bytevector as u8 => {
                let len = self.read_u32()?;
                let mut buf = vec![0u8; len as usize];
                self.buffer.read_exact(&mut buf)?;

                return Ok(Thread::current().make_bytevector_from_slice::<IMMORTAL>(&buf));
            }

            x if x == TypeId::Vector as u8 => {
                
                let len = self.read_u32()?;
                
                let t = Thread::current();
                let v = t.make_vector::<IMMORTAL>(len as _, Value::encode_null_value());
               
                gc_frame!(t.stackchain() => v = v);
                for i in 0..len {
                    let datum = self.get_datum()?;
                    scm_vector_set(*v, t, i, datum);
                }
                
                return Ok(*v);
            }

            TAG_PLIST => {
                let count = self.read_u32()?;
                let lst = Value::encode_null_value();
                let t = Thread::current();
                gc_frame!(t.stackchain() => lst = lst);

                for _ in 0..count {
                    let pair = t.make_cons::<IMMORTAL>(
                        Value::encode_null_value(),
                        Value::encode_null_value(),
                    );
                    let car = self.get_datum()?;
                    scm_set_car(pair, t, car);
                    scm_set_cdr(pair, t, *lst);
                    *lst = pair;
                }

                return Ok(*lst);
            }

            TAG_DLIST => {
                let count = self.read_u32()?;
                let lst = self.get_datum()?;
                let t = Thread::current();
                gc_frame!(t.stackchain() => lst = lst);

                for _ in 0..count {
                    let pair = t.make_cons::<IMMORTAL>(
                        Value::encode_null_value(),
                        Value::encode_null_value(),
                    );
                    let car = self.get_datum()?;
                    scm_set_car(pair, t, car);
                    scm_set_cdr(pair, t, *lst);
                    *lst = pair;
                }

                return Ok(*lst);
            }

            _ => todo!(),
        }
    }
}
