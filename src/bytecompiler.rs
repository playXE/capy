#![allow(dead_code, unused_imports)]
use crate::{
    bytecode::{
        disassembler::disassemble,
        encode::Encode,
        opcodes::*,
        virtual_register::{
            virtual_register_for_argument, virtual_register_for_local, VirtualRegister,
        },
    },
    compaux::scm_unwrap_identifier,
    compile::{IForm, LVar, Lambda, LetScope},
    op::{disassembly, Opcode},
    runtime::fun::make_procedure,
    runtime::object::{CodeBlock, ObjectHeader, Type, MAX_ARITY},
    runtime::value::Value,
    runtime::vector::make_vector_from_slice,
};
use rsgc::{
    prelude::Handle,
    system::{array::Array, arraylist::ArrayList},
    thread::Thread,
};
use std::{collections::HashMap, fmt::Display, ptr::null};

use self::register::RegisterAllocator;

pub mod bytecodegenerator;
pub mod register;
pub mod register_id;
