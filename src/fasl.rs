//! Encodes and decodes FASL files.
//!
//! # FASL
//!
//! FASL is a binary format for serializing Lisp objects. It is used by the compiler to save compiled code to disk.

use std::{io::Write, collections::hash_map::RandomState};

use rsgc::{thread::Thread, prelude::Handle, system::collections::hashmap::HashMap};

use crate::{value::*, vm::intern, compiler::lambda_lifting::LiftedLambda};

const FASL_MAGIC: &[u8] = b"CAPY FASL";

#[derive(Debug)]
pub enum FaslError {
    CannotSerialize(Value),
    InvalidMagic,
    InvalidType(u8),
    InvalidStringIndex(usize),
    InvalidSymbolIndex(usize),
    Io(std::io::Error),
}

impl From<std::io::Error> for FaslError {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}


pub struct FaslWriter<'a, W: Write> {
    w: &'a mut W,

    symbols: Vec<Value>,
    strings: Vec<Value>,
}

impl<'a, W: Write> FaslWriter<'a, W> {
    pub fn new(w: &'a mut W) -> Self {
        Self {
            w,
            symbols: vec![],
            strings: vec![],
        }
    }

    pub fn intern_string(&mut self, v: Value) -> Result<usize, FaslError> {
        for (i, s) in self.strings.iter().enumerate() {
            if s.strsym() == v.strsym() {
                return Ok(i);
            }
        }
        self.strings.push(v);

        Ok(self.symbols.len() - 1)
    }

    pub fn intern_symbol(&mut self, v: Value) -> Result<usize, FaslError> {
        for (i, s) in self.symbols.iter().enumerate() {
            if s == &v {
                return Ok(i);
            }
        }
        self.symbols.push(v);

        Ok(self.symbols.len() - 1)
    }

    pub fn put_u8(&mut self, v: u8) -> Result<(), FaslError> {
        self.w.write_all(&[v])?;
        Ok(())
    }

    pub fn put_u16(&mut self, v: u16) -> Result<(), FaslError> {
        self.w.write_all(&v.to_le_bytes())?;
        Ok(())
    }

    pub fn put_u32(&mut self, v: u32) -> Result<(), FaslError> {
        self.w.write_all(&v.to_le_bytes())?;
        Ok(())
    }

    pub fn put_u64(&mut self, v: u64) -> Result<(), FaslError> {
        self.w.write_all(&v.to_le_bytes())?;
        Ok(())
    }

    pub fn intern(&mut self, v: Value) -> Result<(), FaslError> {
        if v.symbolp() {
            self.intern_symbol(v)?;
        } else if v.strp() {
            self.intern_string(v)?;
        } else if v.pairp() {
            self.intern(v.car())?;
            self.intern(v.cdr())?;
        } else if v.vectorp() {
            for i in 0..v.vector_len() {
                self.intern(v.vector_ref(i))?;
            }
        }

        Ok(())
    }

    pub fn write(&mut self, v: Value) -> Result<(), FaslError> {
        if v.truep() {
            self.put_u8(Type::True as _)
        } else if v.falsep() {
            self.put_u8(Type::False as _)
        } else if v.undefp() {
            self.put_u8(Type::Undef as _)
        } else if v.eofp() {
            self.put_u8(Type::Eof as _)
        } else if v.voidp() {
            self.put_u8(Type::Void as _)
        } else if v.nullp() {
            self.put_u8(Type::Null as _)
        } else if v.intp() {
            self.put_u8(Type::Integer as _)?;
            self.put_u32(v.int() as u32)
        } else if v.doublep() {
            self.put_u8(Type::Double as _)?;
            self.put_u64(v.double().to_bits())
        } else if v.pairp() {
            self.put_u8(Type::Pair as _)?;
            self.write(v.car())?;
            self.write(v.cdr())
        } else if v.symbolp() {
            self.put_u8(Type::Symbol as _)?;
            let ix = self.intern_symbol(v);
            self.put_u32(ix? as u32)
        } else if v.strp() {
            self.put_u8(Type::Str as _)?;
            let ix = self.intern_string(v);
            self.put_u32(ix? as u32)
        } else if v.vectorp() {
            self.put_u8(Type::Vector as _)?;
            self.put_u32(v.vector_len() as u32)?;
            for i in 0..v.vector_len() {
                self.write(v.vector_ref(i))?;
            }
            Ok(())
        } else if v.byte_vectorp() {
            self.put_u8(Type::ByteVector as _)?;
            self.put_u32(v.byte_vector_len() as u32)?;
            self.w.write_all(v.byte_vector_as_slice())?;
            Ok(())
        } else if v.charp() {
            self.put_u8(Type::Char as _)?;
            self.put_u32(v.char() as u32)
        } else {
            Err(FaslError::CannotSerialize(v))
        }
    }

    pub fn start(&mut self, mut defs: Value, toplevel: Value, lambdas: Handle<HashMap<i32, LiftedLambda>>) -> Result<(), FaslError> {
        self.w.write_all(FASL_MAGIC)?;
        self.intern(toplevel)?;
        self.intern(defs)?;

        for (_, v) in lambdas.iter() {
            self.intern(v.body)?;
            self.intern(v.arity)?;
            self.intern(v.bound)?;
            self.intern(v.free)?;
        }

        self.put_u32(self.symbols.len() as u32)?;
        for i in 0..self.symbols.len() {
            self.put_u32(self.symbols[i].strsym().len() as u32)?;
            self.w.write_all(self.symbols[i].strsym().as_bytes())?;
        }

        self.put_u32(self.strings.len() as u32)?;
        for i in 0..self.strings.len() {
            self.put_u32(self.strings[i].strsym().len() as u32)?;
            self.w.write_all(self.strings[i].strsym().as_bytes())?;
        }

        let defs_c = defs.list_length();
        self.put_u32(defs_c as u32)?;
        for _ in 0..defs_c {
            self.write(defs.car())?;
            defs = defs.cdr();
        }


        self.put_u32(lambdas.len() as u32)?;
        for (id, v) in lambdas.iter() {
            self.put_u32(*id as u32)?;
            self.write(v.body)?;
            self.write(v.arity)?;
            self.write(v.bound)?;
            self.write(v.free)?;
        }

        self.write(toplevel)?;
        Ok(())
    }
}

pub struct FaslReader<R: std::io::Read> {
    r: R,
    symbols: Vec<Value>,
    strings: Vec<Value>,
}

impl<R: std::io::Read> FaslReader<R> {
    pub fn new(r: R) -> Self {
        Self {
            r,
            symbols: vec![],
            strings: vec![],
        }
    }

    pub fn get_u8(&mut self) -> Result<u8, FaslError> {
        let mut buf = [0u8; 1];
        self.r.read_exact(&mut buf)?;
        Ok(buf[0])
    }

    pub fn get_u16(&mut self) -> Result<u16, FaslError> {
        let mut buf = [0u8; 2];
        self.r.read_exact(&mut buf)?;
        Ok(u16::from_le_bytes(buf))
    }

    pub fn get_u32(&mut self) -> Result<u32, FaslError> {
        let mut buf = [0u8; 4];
        self.r.read_exact(&mut buf)?;
        Ok(u32::from_le_bytes(buf))
    }

    pub fn get_u64(&mut self) -> Result<u64, FaslError> {
        let mut buf = [0u8; 8];
        self.r.read_exact(&mut buf)?;
        Ok(u64::from_le_bytes(buf))
    }

    pub fn get_symbol(&mut self, ix: usize) -> Result<Value, FaslError> {
        if ix >= self.symbols.len() {
            Err(FaslError::InvalidSymbolIndex(ix))
        } else {
            Ok(self.symbols[ix])
        }
    }

    pub fn get_string(&mut self, ix: usize) -> Result<Value, FaslError> {
        if ix >= self.strings.len() {
            Err(FaslError::InvalidStringIndex(ix))
        } else {
            Ok(self.strings[ix])
        }
    }

    pub fn read(&mut self) -> Result<Value, FaslError> {
        let t = self.get_u8()?;
        let t = Type::from_u8(t).ok_or(FaslError::InvalidType(t))?;

        match t {
            Type::True => Ok(Value::make_true()),
            Type::False => Ok(Value::make_false()),
            Type::Undef => Ok(Value::make_undef()),
            Type::Eof => Ok(Value::make_eof()),
            Type::Void => Ok(Value::make_void()),
            Type::Null => Ok(Value::make_null()),
            Type::Integer => Ok(Value::make_int(self.get_u32()? as i32)),
            Type::Double => Ok(Value::make_double(
                Thread::current(),
                f64::from_bits(self.get_u64()?),
            )),
            Type::Pair => {
                let car = self.read()?;
                let cdr = self.read()?;
                Ok(Value::make_cons(car, cdr))
            }
            Type::Symbol => {
                let ix = self.get_u32()? as usize;
                self.get_symbol(ix)
            }
            Type::Str => {
                let ix = self.get_u32()? as usize;
                self.get_string(ix)
            }
            Type::Vector => {
                let len = self.get_u32()? as usize;
                let v = Value::make_vector(Thread::current(), len as _, Value::make_null());
                for i in 0..len {
                    v.vector_set(i, self.read()?);
                }
                Ok(v)
            }
            Type::ByteVector => {
                let len = self.get_u32()?;
                let v = Value::make_byte_vector(Thread::current(), len, 0);
                self.r.read_exact(v.byte_vector_as_slice_mut())?;
                Ok(v)
            }
            Type::Char => Ok(Value::make_char(
                Thread::current(),
                char::from_u32(self.get_u32()?).unwrap(),
            )),

            _ => Err(FaslError::InvalidType(t as u8)),
        }
    }

    pub fn start(&mut self) -> Result<(Value, Value, Handle<HashMap<i32, LiftedLambda>>), FaslError> {
        let mut buf = [0u8; FASL_MAGIC.len()];
        self.r.read_exact(&mut buf)?;
        if buf != FASL_MAGIC {
            return Err(FaslError::InvalidMagic);
        }

        let mut buf = Vec::with_capacity(256);
        let n_symbols = self.get_u32()? as usize;
        unsafe {
            for _ in 0..n_symbols {
                let len = self.get_u32()? as usize;
                buf.resize(len, 0);
                self.r.read_exact(&mut buf)?;

                self.symbols
                    .push(intern(std::str::from_utf8_unchecked(&buf)));
                buf.clear();
            }

            let n_strings = self.get_u32()? as usize;
            for _ in 0..n_strings {
                let len = self.get_u32()? as usize;
                buf.resize(len, 0);
                self.r.read_exact(&mut buf)?;
                self.strings.push(Value::make_string(
                    Thread::current(),
                    std::str::from_utf8_unchecked(&buf),
                ));
                buf.clear();
            }
        }

        let n_defs = self.get_u32()? as usize;
        let mut defs = Value::make_null();
        for _ in 0..n_defs {
            defs = Value::make_cons(self.read()?, defs);
        }

        

        let n_lambdas = self.get_u32()? as usize;

        let mut lambdas = HashMap::with_hasher_and_capacity(RandomState::new(), n_lambdas as _);

        for _ in 0..n_lambdas {
            let id = self.get_u32()? as i32;
            let body = self.read()?;
            let arity = self.read()?;
            let bound = self.read()?;
            let free = self.read()?;

            lambdas.put(Thread::current(), 
                id,
                LiftedLambda {
                    id,
                    body,
                    arity,
                    bound,
                    free,
                },
            );
        }



        Ok((self.read()?, defs, lambdas))
    }
}

