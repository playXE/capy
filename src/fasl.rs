//! # FASL - Fast Loading
//!  
//! Implementation of FASL for serializating Scheme objects to binary files.

use std::collections::hash_map::RandomState;

use rsgc::{
    prelude::Handle,
    system::{
        arraylist::ArrayList,
        collections::hashmap::{Entry, HashMap},
    },
};

use crate::{
    runtime::{
        object::Type,
        port::{port_put_byte, Port},
        value::Value,
    },
    vm::VM,
};

pub const FASL_EOF: u8 = 0;
pub const FASL_TAG_LOOKUP: u8 = 1;
pub const FASL_TAG_INT32: u8 = 2;
pub const FASL_TAG_PLIST: u8 = 3;
pub const FASL_TAG_DLIST: u8 = 4;
pub const FASL_TAG_VECTOR: u8 = 5;
pub const FASL_TAG_RATIONAL: u8 = 6;
pub const FASL_TAG_COMPLEX: u8 = 7;
pub const FASL_TAG_FLONUM: u8 = 8;
pub const FASL_TAG_BIGNUM: u8 = 9;
pub const FASL_TAG_BVECTOR: u8 = 10;
pub const FASL_TAG_CHAR: u8 = 11;
pub const FASL_TAG_NIL: u8 = 12;
pub const FASL_TAG_T: u8 = 13;
pub const FASL_TAG_F: u8 = 14;
pub const FASL_TAG_SYMBOL: u8 = 15;
pub const FASL_TAG_IDENTIFIER: u8 = 16;
pub const FASL_TAG_UNINTERNED_SYMBOL: u8 = 17;
pub const FASL_TAG_SYNTAX_RULES: u8 = 18;
pub const FASL_TAG_SYNTAX_PATTERN: u8 = 19;
pub const FASL_TAG_PVREF: u8 = 20;
pub const FASL_TAG_STRING: u8 = 21;

pub struct FASLPrinter<'a> {
    pub vm: &'a mut VM,
    pub port: Handle<Port>,
    pub lites: Handle<HashMap<Value, Value>>,
    pub stack: ArrayList<Value>,
    pub bad: Value,
}

impl<'a> FASLPrinter<'a> {
    pub fn new(port: Handle<Port>, vm: &'a mut VM) -> Self {
        Self {
            port,
            lites: HashMap::with_hasher_and_capacity(RandomState::new(), 4),
            stack: ArrayList::with_capacity(vm.mutator(), 4),
            bad: Value::encode_bool_value(false),
            vm,
        }
    }

    pub fn emit_u8(&mut self, byte: u8) -> Result<(), Value> {
        port_put_byte(self.port, byte)
    }

    pub fn emit_u32(&mut self, mut n: u32) -> Result<(), Value> {
        for _ in 0..5 {
            let code = n & 0x7f;
            n = n >> 7;
            if n == 0 {
                self.emit_u8((code | 0x80) as u8)?;
                return Ok(());
            } else {
                self.emit_u8(code as u8)?;
            }
        }

        Ok(())
    }

    pub fn emit_u64(&mut self, mut n: u64) -> Result<(), Value> {
        for _ in 0..8 {
            self.emit_u8((n & 0xff) as u8)?;
            n = n >> 8;
        }

        Ok(())
    }

    pub fn emit_bytes(&mut self, bytes: &[u8]) -> Result<(), Value> {
        for byte in bytes {
            self.emit_u8(*byte)?;
        }

        Ok(())
    }

    pub fn push(&mut self, val: Value) {
        self.stack.push(self.vm.mutator(), val);
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    pub fn scan(&mut self, mut obj: Value) {
        loop {
            if obj.is_null() {
                return;
            }

            if obj.is_symbol() || obj.is_string() || obj.is_wrapped_identifier() {
                let size = self.lites.len();
                match self.lites.entry(obj) {
                    Entry::Occupied(_) => return,
                    Entry::Vacant(entry) => {
                        entry.insert(Value::encode_int32(size as _));
                        return;
                    }
                }
            }

            if obj.is_pair() {
                self.scan(obj.car());
                obj = obj.cdr();
                continue;
            }

            if obj.is_vector() {
                let vector = obj.vector();

                if vector.len() == 0 {
                    return;
                }

                for i in 0..vector.len() {
                    self.scan(vector[i]);
                }

                return;
            }

            if obj.is_struct() {
                let struct_ = obj.structure();
                for i in 0..struct_.slots.len() {
                    self.scan(struct_.slots[i]);
                }

                return;
            }

            if obj.is_struct_type() {
                let struct_type = obj.struct_type();
                self.scan(struct_type.name);
                self.scan(struct_type.accessor);
                self.scan(struct_type.mutator);
                self.scan(struct_type.guard);
                self.scan(struct_type.uninit_val);
                for i in 0..struct_type.props.len() {
                    self.scan(struct_type.props[i]);
                }

                for i in 0..struct_type.parent_types.len() {
                    self.scan(struct_type.parent_types[i]);
                }

                return;
            }

            if obj.is_tuple() {
                let tuple = obj.tuple();
                for i in 0..tuple.len() {
                    self.scan(tuple[i]);
                }

                return;
            }

            if obj.is_values() {
                let values = obj.values();
                for i in 0..values.len() {
                    self.scan(values[i]);
                }

                return;
            }

            if obj.is_box() {
                obj = obj.box_ref();
                continue;
            }

            if obj.is_native_procedure() {
                self.bad = obj;
                return;
            }

            return;
        }
    }

    fn put_list(&mut self, mut obj: Value) -> Result<(), Value> {
        let mut count = 0;
        while obj.is_pair() {
            self.push(obj.car());
            obj = obj.cdr();

            count += 1;
        }
        self.emit_u8(Type::Pair as _)?;
        self.emit_u8(obj.is_null() as u8)?;
        if !obj.is_null() {
            self.push(obj);
        }

        while count > 0 {
            obj = self.pop().unwrap();
            self.put_datum(obj)?;
            count -= 1;
        }

        Ok(())
    }

    fn put_datum(&mut self, obj: Value) -> Result<(), Value> {
        match obj.get_type() {
            Type::Null => self.emit_u8(Type::Null as _),
            Type::True => self.emit_u8(Type::True as _),
            Type::False => self.emit_u8(Type::False as _),
            Type::Int32 => {
                self.emit_u8(Type::Int32 as _)?;
                self.emit_u32(obj.get_int32() as _)
            }
            Type::Char => {
                self.emit_u8(Type::Char as _)?;
                self.emit_u32(obj.get_char() as _)
            }

            Type::Double => {
                self.emit_u8(Type::Double as _)?;
                self.emit_u64(obj.get_double().to_bits())
            }
            Type::BigNum => {
                self.emit_u8(Type::BigNum as _)?;
                let bignum = obj.bignum();
                self.emit_u32(bignum.uwords().len() as _)?;
                self.emit_u8(bignum.is_negative() as _)?;
                for &uword in bignum.uwords().iter() {
                    self.emit_u32(uword)?;
                }

                Ok(())
            }

            Type::Box => {
                self.emit_u8(Type::Box as _)?;
                self.put_datum(obj.box_ref())
            }

            Type::Symbol | Type::Str | Type::Identifier => {
                let id = self.lites.get(&obj).unwrap().get_int32();
                self.emit_u8(FASL_TAG_LOOKUP)?;
                self.emit_u32(id as _)
            }

            Type::Pair => self.put_list(obj),

            Type::Vector => {
                let vector = obj.vector();
                self.emit_u8(Type::Vector as _)?;
                self.emit_u32(vector.len() as _)?;
                for i in 0..vector.len() {
                    self.put_datum(vector[i])?;
                }

                Ok(())
            }

            Type::Bytevector => {
                let bytevector = obj.bytevector();
                self.emit_u8(Type::Bytevector as _)?;
                self.emit_u32(bytevector.len() as _)?;
                for i in 0..bytevector.len() {
                    self.emit_u8(bytevector[i])?;
                }

                Ok(())
            }

            Type::Undefined => self.emit_u8(Type::Undefined as _),

            _ => todo!(),
        }
    }

    fn put_lites(&mut self) -> Result<(), Value> {
        let lites = self.lites.iter().map(|(&x, &y)| (x, y)).collect::<Vec<_>>();
        for (k, i) in lites {
            let i = i.get_int32();
            if k.is_symbol() {
                if !k.symbol().interned {
                    self.emit_u8(FASL_TAG_UNINTERNED_SYMBOL)?;
                } else {
                    self.emit_u8(FASL_TAG_SYMBOL)?;
                }
                self.emit_u32(i as _)?;
                let name = k.strsym();
                self.emit_u32(name.len() as _)?;
                self.emit_bytes(name.as_bytes())?;
                continue;
            } else if k.is_string() {
                self.emit_u8(FASL_TAG_STRING)?;
                self.emit_u32(i as _)?;
                let name = k.strsym();
                self.emit_u32(name.len() as _)?;
                self.emit_bytes(name.as_bytes())?;
            } else {
                todo!()
            }
        }

        Ok(())
    }

    pub fn put(&mut self, obj: Value) -> Result<(), Value> {
        self.scan(obj);
        if !self.bad.is_false() {
            return Err(self.bad);
        }
        self.put_lites()?;
        self.put_datum(obj)
    }
}
