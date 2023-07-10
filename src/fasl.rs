//! # FASL - Fast Loading
//!  
//! Implementation of FASL for serializating Scheme objects to binary files.

use rsgc::{prelude::Handle, system::{collections::hashmap::HashMap, arraylist::ArrayList}};

use crate::{runtime::{port::{Port, port_put_byte}, value::Value}, vm::VM};


pub const FASL_EOF: u8 = 0;
pub const FASL_TAG_LOOKUP: u8 = 1;
pub const FASL_TAG_INT32: u8 = 2;
pub const FASL_TAG_PLIST: u8 = 3;
pub const FASL_TAG_DLISST: u8 = 4;
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

pub struct FASLPrinter<'a> {
    pub vm: &'a mut VM,
    pub port: Handle<Port>,
    pub lites: Handle<HashMap<Value, Value>>,
    pub stack: ArrayList<Value>,
    pub bad: Value
}

impl<'a> FASLPrinter<'a> {
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
}

