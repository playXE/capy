use std::{
    collections::hash_map::RandomState,
    fs::File,
    io::{self, Read, Write},
};

use r7rs_parser::{expr::NoIntern, parser::Parser};
use rsgc::{prelude::Handle, system::collections::hashmap::HashMap, thread::Thread};

use crate::{
    raise_exn,
    value::{Hdr, Str, Type, Value},
    vm::{intern, Vm},
};

pub trait TextInputPort {
    fn read_to_string(&mut self, out: &mut String) -> io::Result<usize>;
    fn set_parsed(&mut self, obj: Value);
    fn parsed(&self) -> Value;

    fn read(&mut self, vm: &mut Vm) -> Result<Value, ReadError> {
        if self.parsed().voidp() {
            let mut s = String::new();

            match self.read_to_string(&mut s) {
                Ok(_) => {}
                Err(err) => return Err(ReadError::Io(err)),
            }
            let mut nointern = NoIntern;

            let mut parser = Parser::new(&mut nointern, &s, false);

            let mut exprs = vec![];

            while !parser.finished() {
                exprs.push(match parser.parse(false) {
                    Ok(expr) => expr,
                    Err(err) => return Err(ReadError::Parse(err)),
                });
            }

            let mut ls = Value::make_null();

            for expr in exprs.into_iter().rev() {
                let val = read_sexp(vm, &nointern, &*expr);
                ls = Value::make_pair(vm.mutator(), val, ls);
            }

            self.set_parsed(ls);
        }

        if self.parsed().nullp() {
            return Ok(Value::make_eof());
        } else {
            let car = *self.parsed().pair_car();
            self.set_parsed(*self.parsed().pair_cdr());
            Ok(car)
        }
    }
}

// todo: save source information
pub fn read_sexp<I: r7rs_parser::expr::Interner>(
    vm: &mut Vm,
    i: &I,
    expr: &r7rs_parser::expr::Expr<I>,
) -> Value {
    use r7rs_parser::expr::Expr;
    match expr {
        Expr::Bool(b) => {
            if *b {
                Value::make_true()
            } else {
                Value::make_false()
            }
        }

        Expr::Null => Value::make_null(),
        Expr::Char(c) => vm.make_char(*c),
        Expr::Fixnum(x) => Value::make_int(*x),
        Expr::Float(x) => Value::make_double(vm.mutator(), *x),
        Expr::Symbol(x) => intern(i.description(x)),
        Expr::Str(x) => Value::make_string(vm.mutator(), x),
        Expr::Syntax(_, expr) => read_sexp(vm, i, expr),
        Expr::ByteVector(x) => {
            let bvec = Value::make_byte_vector(vm.mutator(), x.len() as _, 0);

            bvec.byte_vector_as_slice_mut().copy_from_slice(x);
            return bvec;
        }

        Expr::GrowableVector(x) => {
            let vec = Value::make_vector(vm.mutator(), x.len() as _, Value::make_undef());

            for (k, e) in x.iter().enumerate() {
                vec.vector_as_slice_mut()[k] = read_sexp(vm, i, e);
            }

            return vec;
        }

        Expr::ImmutableVector(x) => {
            let vec = Value::make_vector(vm.mutator(), x.len() as _, Value::make_undef());

            for (k, e) in x.iter().enumerate() {
                vec.vector_as_slice_mut()[k] = read_sexp(vm, i, e);
            }

            return vec;
        }

        Expr::Pair(x, y) => {
            let car = read_sexp(vm, i, x);
            let cdr = read_sexp(vm, i, y);

            Value::cons(vm.mutator(), car, cdr)
        }

        _ => todo!(),
    }
}

#[derive(Debug)]
pub enum ReadError {
    Io(io::Error),
    Parse(r7rs_parser::parser::ParseError),
}

#[repr(C)]
pub struct FileInputPort {
    pub(crate) hdr: Hdr,
    pub(crate) file: Option<File>,
    pub parsed: Value,
}

impl FileInputPort {
    pub fn new(file: File) -> Self {
        Self {
            hdr: Hdr::new(Type::FileInputPort),
            file: Some(file),
            parsed: Value::make_void(),
        }
    }

    pub fn close(&mut self) -> bool {
        self.file.take().is_some()
    }

    pub fn is_closed(&self) -> bool {
        self.file.is_none()
    }
}

impl TextInputPort for FileInputPort {
    fn read_to_string(&mut self, out: &mut String) -> io::Result<usize> {
        self.file.as_mut().unwrap().read_to_string(out)
    }

    fn set_parsed(&mut self, obj: Value) {
        self.parsed = obj;
    }

    fn parsed(&self) -> Value {
        self.parsed
    }
}

#[repr(C)]
pub struct StringInputPort {
    pub(crate) hdr: Hdr,
    pub(crate) str: Handle<Str>,
    pub(crate) idx: usize,
    pub parsed: Value,
}

impl StringInputPort {
    pub fn new(vm: &mut Vm, s: impl AsRef<str>) -> Self {
        let str = Value::make_string(vm.mutator(), s.as_ref());
        Self {
            hdr: Hdr::new(Type::StringInputPort),
            str: str.downcast_str(),
            idx: 0,
            parsed: Value::make_void(),
        }
    }

    pub fn read_char(&mut self) -> Option<char> {
        let mut chars = self.str.str().chars();
        let ret = chars.nth(self.idx);
        self.idx += 1;

        ret
    }
}

impl TextInputPort for StringInputPort {
    fn read_to_string(&mut self, out: &mut String) -> io::Result<usize> {
        out.push_str(&self.str.str()[self.idx..]);
        Ok(self.str.str().len() - self.idx)
    }

    fn set_parsed(&mut self, obj: Value) {
        self.parsed = obj;
    }

    fn parsed(&self) -> Value {
        self.parsed
    }
}

#[repr(C)]
pub struct BinaryFileInputPort {
    pub(crate) hdr: Hdr,
    pub(crate) file: Option<File>,
}

impl BinaryFileInputPort {
    pub fn new(file: File) -> Self {
        Self {
            hdr: Hdr::new(Type::BinaryFileInputPort),
            file: Some(file),
        }
    }

    pub fn close(&mut self) -> bool {
        self.file.take().is_some()
    }

    pub fn is_closed(&self) -> bool {
        self.file.is_none()
    }

    pub fn read_byte(&mut self) -> io::Result<u8> {
        let mut buf = [0u8; 1];
        self.file.as_mut().unwrap().read_exact(&mut buf)?;
        Ok(buf[0])
    }

    pub fn read_bytes(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.file.as_mut().unwrap().read(buf)
    }

    pub fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        self.file.as_mut().unwrap().read_to_end(buf)
    }
}

pub trait TextOutputPort {
    fn put_string(&mut self, s: &str) -> io::Result<()>;

    fn write(&mut self, obj: Value) -> io::Result<()> {
        let mut shared_id = 1;
        let mut seen = HashMap::<u64, Value>::with_hasher_and_capacity(RandomState::new(), 4);

        self.scan(obj, &mut seen);
        self.display_one(obj, &mut seen, &mut shared_id)
    }

    fn display(&mut self, obj: Value) -> io::Result<()> {
        let mut shared_id = 1;
        let mut seen = HashMap::<u64, Value>::with_hasher_and_capacity(RandomState::new(), 4);

        self.scan(obj, &mut seen);
        self.display_one(obj, &mut seen, &mut shared_id)
    }

    fn display_one(
        &mut self,
        obj: Value,
        seen: &mut Handle<HashMap<u64, Value>>,
        shared_id: &mut i32,
    ) -> io::Result<()> {
        let seen_state = seen.get(&obj.raw()).copied().unwrap_or(Value::make_false());
        let t = Thread::current();
        if seen_state.truep() {
            seen.put(t, obj.raw(), Value::make_int(*shared_id));
            self.put_string(&format!("#{}=", shared_id))?;
            *shared_id += 1;
        } else if seen_state.intp() {
            println!("{}", seen_state.raw() == Value::make_false().raw());
            return self.put_string(&format!("#{:x}#", seen_state.int()));
        }

        if obj.intp() {
            self.put_string(&obj.int().to_string())
        } else if obj.truep() {
            self.put_string("#t")
        } else if obj.falsep() {
            self.put_string("#f")
        } else if obj.voidp() {
            self.put_string("#void")
        } else if obj.nullp() {
            self.put_string("()")
        } else if obj.eofp() {
            self.put_string("#eof")
        } else if obj.undefp() {
            self.put_string("#undef")
        } else if obj.pairp() {
            self.display_pair(obj, seen, shared_id)
        } else if obj.vectorp() {
            self.display_vector(obj, seen, shared_id)
        } else if obj.strp() {
            self.put_string(obj.str())
        } else if obj.symbolp() {
            self.put_string(obj.symbol_str())
        } else if obj.charp() {
            self.put_string(&obj.char().to_string())
        } else if obj.byte_vectorp() {
            self.put_string("#u8(")?;
            for i in 0..obj.byte_vector_len() {
                self.put_string(&format!("{}", obj.byte_vector_at(i)))?;
                if i != obj.byte_vector_len() - 1 {
                    self.put_string(" ")?;
                }
            }

            self.put_string(")")
        } else if obj.identifierp() {
            let mut id = obj;

            while id.identifierp() {
                id = *id.identifier_name();
            }

            self.display_one(id, seen, shared_id)
        } else if obj.portp() {
            match obj.get_type() {
                Type::StringInputPort => self.put_string("#<string-input-port>"),
                Type::StringOutputPort => self.put_string("#<string-output-port>"),
                Type::BinaryFileInputPort => self.put_string("#<binary-file-input-port>"),
                Type::BinaryFileOutputPort => self.put_string("#<binary-file-output-port>"),
                Type::FileInputPort => self.put_string("#<file-input-port>"),
                Type::FileOutputPort => self.put_string("#<file-output-port>"),
                Type::StdInputPort => self.put_string("#<stdin>"),
                Type::StdOutputPort => self.put_string("#<stdout>"),
                Type::StdErrorPort => self.put_string("#<stderr>"),
                Type::ByteVectorInputPort => self.put_string("#<bytevector-input-port>"),
                Type::ByteVectorOutputPort => self.put_string("#<bytevector-output-port>"),
                Type::BinaryByteVectorInputPort => {
                    self.put_string("#<binary-bytevector-input-port>")
                }
                Type::BinaryByteVectorOutputPort => {
                    self.put_string("#<binary-bytevector-output-port>")
                }
                _ => unreachable!(),
            }
        } else if obj.structp() {
            self.put_string("#<")?;
            self.display_one(obj.struct_stype().name, seen, shared_id)?;
            self.put_string(">")
        } else if obj.structuretypep() {
            self.put_string("#<struct-type ")?;
            self.display_one(*obj.structuretype_name(), seen, shared_id)?;
            self.put_string(">")
        } else if obj.structurepropertyp() {
            self.put_string("#<struct-property ")?;
            self.display_one(*obj.structureproperty_name(), seen, shared_id)?;
            self.put_string(">")
        } else if obj.procedurep() {
            match obj.get_type() {
                Type::PrimitiveProcedure => {
                    let name = obj.downcast_primitive_proc().name;
                    self.put_string(&format!("#<procedure {}>", name.symbol_str()))
                }
                Type::ClosedPrimitiveProcedure => {
                    let name = obj.downcast_closed_primitive_proc().name;
                    self.put_string(&format!("#<procedure {}>", name.symbol_str()))
                }
                Type::NativeProcedure => {
                    return self.put_string("#<procedure>");
                }

                Type::ReturnCont => self.put_string("#<return-continuation>"),
                _ => unreachable!(),
            }
        } else if obj.doublep() {
            self.put_string(&obj.double().to_string())
        } else if obj.macrop() {
            self.put_string(&format!("#<macro {}>", obj.macro_name().symbol_str()))
        } else if obj.valuesp() {
            self.display_values(obj, seen, shared_id)
        } else {
            self.put_string(&format!("#<unknown {:?}:{:x}>", obj.get_type(), obj.raw()))
        }
    }

    fn display_vector(
        &mut self,
        v: Value,
        seen: &mut Handle<HashMap<u64, Value>>,
        shared_id: &mut i32,
    ) -> io::Result<()> {
        self.put_string("#(")?;

        for i in 0..v.vector_len() {
            self.display_one(v.vector_ref(i), seen, shared_id)?;
            if i != v.vector_len() - 1 {
                self.put_string(" ")?;
            }
        }

        self.put_string(")")?;
        Ok(())
    }

    fn display_values(
        &mut self,
        v: Value,
        seen: &mut Handle<HashMap<u64, Value>>,
        shared_id: &mut i32,
    ) -> io::Result<()> {
        self.put_string("#values(")?;

        for i in 0..v.values_len() {
            self.display_one(v.values_ref(i), seen, shared_id)?;
            if i != v.values_len() - 1 {
                self.put_string(" ")?;
            }
        }

        self.put_string(")")?;
        Ok(())
    }

    fn display_pair(
        &mut self,
        p: Value,
        seen: &mut Handle<HashMap<u64, Value>>,
        shared_id: &mut i32,
    ) -> io::Result<()> {
        self.put_string("(")?;

        self.display_one(p.car(), seen, shared_id)?;

        let mut obj = p.cdr();
        loop {
            let seen_state = seen.get(&obj.raw()).copied().unwrap_or(Value::make_false());

            if obj.pairp() && seen_state.falsep() {
                self.put_string(" ")?;
                self.display_one(obj.car(), seen, shared_id)?;
                obj = obj.cdr();
            } else if obj.nullp() {
                self.put_string(")")?;
                return Ok(());
            } else {
                self.put_string(" . ")?;
                self.display_one(obj, seen, shared_id)?;
                self.put_string(")")?;
                return Ok(());
            }
        }
    }

    fn scan(&mut self, mut obj: Value, seen: &mut Handle<HashMap<u64, Value>>) {
        loop {
            if obj.pairp() {
                let val = seen.get(&obj.raw()).copied().unwrap_or(Value::make_void());

                if val.falsep() {
                    seen.put(Thread::current(), obj.raw(), Value::make_true());
                    return;
                } else if val.truep() {
                    return;
                } else {
                    seen.put(Thread::current(), obj.raw(), Value::make_false());
                }

                self.scan(obj.car(), seen);
                obj = obj.cdr();
                continue;
            } else if obj.vectorp() {
                let val = seen.get(&obj.raw()).copied().unwrap_or(Value::make_void());

                if val.falsep() {
                    seen.put(Thread::current(), obj.raw(), Value::make_true());
                    return;
                } else if val.truep() {
                    return;
                } else {
                    seen.put(Thread::current(), obj.raw(), Value::make_true());
                }

                for e in obj.vector_as_slice() {
                    self.scan(*e, seen);
                }

                break;
            } else {
                break;
            }
        }
    }

    fn format(&mut self, fmt: &str, args: &[Value]) -> Result<(), FormatError> {
        let mut chars = fmt.chars();

        let mut i = 0;

        while let Some(c) = chars.next() {
            if c == '~' {
                if let Some(mut c) = chars.next() {
                    while c.is_whitespace() {
                        match chars.next() {
                            Some(x) => c = x,
                            None => return Ok(()),
                        }
                    }

                    if (c == 'a' || c == 'A') || (c == 'e' || c == 'E') || (c == 'd' || c == 'D') {
                        if i < args.len() {
                            self.display(args[i])?;
                            i += 1;
                        } else {
                            return Err(FormatError::NotEnoughArguments(i, args.len()));
                        }
                    } else if c == 'x' || c == 'X' {
                        if i < args.len() {
                            if args[i].exact_integerp() {
                                write_exact(self, args[i], 16)?;
                                i += 1;
                            } else {
                                return Err(FormatError::ExactExpected(i));
                            }
                        } else {
                            return Err(FormatError::NotEnoughArguments(i, args.len()));
                        }
                    } else if c == 'b' || c == 'B' {
                        if i < args.len() {
                            if args[i].exact_integerp() {
                                write_exact(self, args[i], 2)?;
                                i += 1;
                            } else {
                                return Err(FormatError::ExactExpected(i));
                            }
                        } else {
                            return Err(FormatError::NotEnoughArguments(i, args.len()));
                        }
                    } else if c == 'o' || c == 'O' {
                        if i < args.len() {
                            if args[i].exact_integerp() {
                                write_exact(self, args[i], 8)?;
                                i += 1;
                            } else {
                                return Err(FormatError::ExactExpected(i));
                            }
                        } else {
                            return Err(FormatError::NotEnoughArguments(i, args.len()));
                        }
                    } else if c == 's' || c == 'S' {
                        if i < args.len() {
                            self.write(args[i])?;
                            i += 1;
                        } else {
                            return Err(FormatError::NotEnoughArguments(i, args.len()));
                        }
                    } else if c == '~' {
                        self.put_string("~")?;
                    } else {
                        return Err(FormatError::Unknown(c));
                    }
                } else {
                    break;
                }
            } else {
                self.put_string(&c.to_string())?;
            }
        }

        Ok(())
    }
}

fn write_exact<W: TextOutputPort + ?Sized>(port: &mut W, val: Value, base: u32) -> io::Result<()> {
    assert!(val.exact_integerp());

    if val.intp() {
        match base {
            2 => port.put_string(&format!("{:b}", val.int()))?,
            8 => port.put_string(&format!("{:o}", val.int()))?,
            10 => port.put_string(&format!("{}", val.int()))?,
            16 => port.put_string(&format!("{:x}", val.int()))?,
            _ => unreachable!(),
        }
    } else {
        todo!()
    }

    Ok(())
}

#[derive(Debug)]
pub enum FormatError {
    NotEnoughArguments(usize, usize),
    Unknown(char),
    ExactExpected(usize),
    Io(io::Error),
}

impl From<io::Error> for FormatError {
    fn from(e: io::Error) -> Self {
        FormatError::Io(e)
    }
}

#[repr(C)]
pub struct FileOutputPort {
    pub(crate) hdr: Hdr,
    pub(crate) file: Option<File>,
}

impl FileOutputPort {
    pub fn new(file: File) -> Self {
        Self {
            hdr: Hdr::new(Type::FileOutputPort),
            file: Some(file),
        }
    }

    pub fn close(&mut self) -> bool {
        self.file.take().is_some()
    }

    pub fn closed(&self) -> bool {
        self.file.is_none()
    }
}

impl TextOutputPort for FileOutputPort {
    fn put_string(&mut self, s: &str) -> io::Result<()> {
        if let Some(file) = &mut self.file {
            file.write_all(s.as_bytes())?;
        }

        Ok(())
    }
}

#[repr(C)]
pub struct StdOutputPort {
    pub(crate) hdr: Hdr,
}

impl StdOutputPort {
    pub fn new() -> Self {
        Self {
            hdr: Hdr::new(Type::StdOutputPort),
        }
    }
}

impl TextOutputPort for StdOutputPort {
    fn put_string(&mut self, s: &str) -> io::Result<()> {
        print!("{}", s);
        Ok(())
    }
}

#[repr(C)]
pub struct StdErrorPort {
    pub(crate) hdr: Hdr,
}

impl StdErrorPort {
    pub fn new() -> Self {
        Self {
            hdr: Hdr::new(Type::StdErrorPort),
        }
    }
}

impl TextOutputPort for StdErrorPort {
    fn put_string(&mut self, s: &str) -> io::Result<()> {
        eprint!("{}", s);
        Ok(())
    }
}

#[repr(C)]
pub struct StringOutputPort {
    pub(crate) hdr: Hdr,
    pub(crate) string: Handle<Str>,
}

impl StringOutputPort {
    pub fn new() -> Self {
        Self {
            hdr: Hdr::new(Type::StringOutputPort),
            string: Str::new(Thread::current(), "").downcast_str(),
        }
    }

    pub fn string(&self) -> Handle<Str> {
        self.string.clone()
    }
}

impl TextOutputPort for StringOutputPort {
    fn put_string(&mut self, s: &str) -> io::Result<()> {
        self.string.str_mut().push_str(s);
        Ok(())
    }
}

#[repr(C)]
pub struct ByteVectorInputPort {
    pub(crate) hdr: Hdr,
    pub(crate) bytes: Value,
    pub(crate) pos: usize,
    pub parsed: Value,
}

impl ByteVectorInputPort {
    pub fn new(bytes: Value) -> Self {
        Self {
            hdr: Hdr::new(Type::ByteVectorInputPort),
            bytes,
            pos: 0,
            parsed: Value::make_void(),
        }
    }

    pub fn byte_vector(&self) -> Value {
        self.bytes
    }

    pub fn position(&self) -> usize {
        self.pos
    }

    pub fn set_position(&mut self, pos: usize) {
        self.pos = pos;
    }

    pub fn eof(&self) -> bool {
        self.pos >= self.bytes.byte_vector_len()
    }

    pub fn read_u8(&mut self) -> Option<u8> {
        if self.eof() {
            None
        } else {
            let byte = self.bytes.byte_vector_ref(self.pos);
            self.pos += 1;
            Some(byte)
        }
    }

    pub fn read_u16(&mut self) -> Option<u16> {
        if self.eof() || self.pos + 1 >= self.bytes.byte_vector_len() {
            None
        } else {
            let byte1 = self.bytes.byte_vector_ref(self.pos) as u16;
            let byte2 = self.bytes.byte_vector_ref(self.pos + 1) as u16;
            self.pos += 2;
            Some(byte1 | (byte2 << 8))
        }
    }

    pub fn read_u32(&mut self) -> Option<u32> {
        if self.eof() || self.pos + 3 >= self.bytes.byte_vector_len() {
            None
        } else {
            let byte1 = self.bytes.byte_vector_ref(self.pos) as u32;
            let byte2 = self.bytes.byte_vector_ref(self.pos + 1) as u32;
            let byte3 = self.bytes.byte_vector_ref(self.pos + 2) as u32;
            let byte4 = self.bytes.byte_vector_ref(self.pos + 3) as u32;
            self.pos += 4;
            Some(byte1 | (byte2 << 8) | (byte3 << 16) | (byte4 << 24))
        }
    }

    pub fn read_u64(&mut self) -> Option<u64> {
        if self.eof() || self.pos + 7 >= self.bytes.byte_vector_len() {
            None
        } else {
            let byte1 = self.bytes.byte_vector_ref(self.pos) as u64;
            let byte2 = self.bytes.byte_vector_ref(self.pos + 1) as u64;
            let byte3 = self.bytes.byte_vector_ref(self.pos + 2) as u64;
            let byte4 = self.bytes.byte_vector_ref(self.pos + 3) as u64;
            let byte5 = self.bytes.byte_vector_ref(self.pos + 4) as u64;
            let byte6 = self.bytes.byte_vector_ref(self.pos + 5) as u64;
            let byte7 = self.bytes.byte_vector_ref(self.pos + 6) as u64;
            let byte8 = self.bytes.byte_vector_ref(self.pos + 7) as u64;
            self.pos += 8;
            Some(
                byte1
                    | (byte2 << 8)
                    | (byte3 << 16)
                    | (byte4 << 24)
                    | (byte5 << 32)
                    | (byte6 << 40)
                    | (byte7 << 48)
                    | (byte8 << 56),
            )
        }
    }
}

impl TextInputPort for ByteVectorInputPort {
    fn read_to_string(&mut self, out: &mut String) -> io::Result<usize> {
        let bytes = self.bytes.byte_vector_as_slice();
        let len = bytes.len() - self.pos;
        out.push_str(&String::from_utf8_lossy(&bytes[self.pos..]));
        self.pos = bytes.len();
        Ok(len)
    }

    fn parsed(&self) -> Value {
        self.parsed
    }

    fn set_parsed(&mut self, obj: Value) {
        self.parsed = obj;
    }
}

#[macro_export]
macro_rules! scm_format {
    ($fmt: expr, $($val: expr),*) => {
        {
            use $crate::ports::TextOutputPort;
            let mut t = $crate::rsgc::prelude::Thread::current();
            let mut port = $crate::ports::StringOutputPort::new();
            let mut ls = $crate::util::arraylist::ArrayList::with_capacity(t, 1);
            $(
                ls.push(t, $val);
            )*
            port.format(&$fmt, &ls).unwrap();

            unsafe { $crate::value::Value::encode_ptr(port.string().as_ptr()) }
        }
    };

    ($fmt: expr) => {
        {
            use $crate::ports::TextOutputPort;
            let mut t = $crate::rsgc::prelude::Thread::current();
            let mut port = $crate::rsgc::prelude::StringOutputPort::new();
            port.format(&$fmt, &[]).unwrap();

            unsafe { $crate::value::Value::encode_ptr(port.string.as_ptr()) }
        }
    }
}

pub fn map_io_error(err: io::Error) -> Value {
    let err: Result<(), Value> = raise_exn!(FailFilesystem, &[], "I/O error: {}", err);

    err.unwrap_err()
}
