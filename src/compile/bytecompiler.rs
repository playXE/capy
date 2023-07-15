use rsgc::{
    prelude::Handle,
    system::{array::Array, arraylist::ArrayList},
    thread::Thread,
};
use std::{collections::HashMap, ptr::null};

use crate::{
    compaux::scm_unwrap_identifier,
    compile::LetScope,
    op::{disassembly, Opcode},
    runtime::fun::make_procedure,
    runtime::object::{CodeBlock, ObjectHeader, Type, MAX_ARITY},
    runtime::value::Value,
    runtime::vector::make_vector_from_slice,
};

use super::{AsmOperand, IForm, LVar, Lambda};

struct BindingGroup {
    bindings: HashMap<usize, usize>,
    parent: Option<Box<BindingGroup>>,
}

impl BindingGroup {
    pub fn lookup(&self, lvar: Handle<LVar>) -> Option<usize> {
        if let Some(&ix) = self.bindings.get(&(lvar.as_ptr() as usize)) {
            Some(ix)
        } else if let Some(ref parent) = self.parent {
            parent.lookup(lvar)
        } else {
            None
        }
    }
}

struct CaptureGroup {
    captures: Vec<usize>,
}

impl CaptureGroup {
    pub fn capture(&mut self, lvar: Handle<LVar>) -> usize {
        if let Some(ix) = self
            .captures
            .iter()
            .position(|&x| x == lvar.as_ptr() as usize)
        {
            ix
        } else {
            let ix = self.captures.len();
            self.captures.push(lvar.as_ptr() as usize);
            ix
        }
    }
}

pub struct ByteCompiler {
    parent: Option<*mut ByteCompiler>,
    group: Box<BindingGroup>,
    captures: CaptureGroup,
    num_locals: usize,
    max_locals: usize,
    code: Vec<u8>,
    literals: ArrayList<Value>,
    fragments: ArrayList<Handle<CodeBlock>>,
    pub note: Option<Handle<rsgc::system::collections::hashmap::HashMap<Value, Value>>>,
    pub ranges: Option<ArrayList<((u32, u32), Value, Value)>>,
}

impl ByteCompiler {
    pub fn new(thread: &mut Thread) -> Self {
        Self {
            note: None,
            captures: CaptureGroup {
                captures: Vec::new(),
            },
            code: Vec::new(),
            literals: ArrayList::with_capacity(thread, 16),
            fragments: ArrayList::with_capacity(thread, 4),
            num_locals: 0,
            max_locals: 0,
            parent: None,
            ranges: Some(ArrayList::with_capacity(thread, 4)),
            group: Box::new(BindingGroup {
                bindings: HashMap::new(),
                parent: None,
            }),
        }
    }

    pub fn next_local_index(&mut self) -> usize {
        self.num_locals += 1;
        if self.num_locals > self.max_locals {
            self.max_locals = self.num_locals;
        }

        self.num_locals - 1
    }

    pub fn add_constant(&mut self, thread: &mut Thread, constant: Value) -> usize {
        assert!(!constant.is_empty());
        if let Some(ix) = self.literals.iter().position(|&value| value == constant) {
            ix
        } else {
            let ix = self.literals.len();
            self.literals.push(thread, constant);
            ix
        }
    }

    pub fn emit_load(&mut self, thread: &mut Thread, constant: Value) {
        if constant.is_empty() {
            panic!("wtf??");
        }
        if constant.is_int32() {
            self.code.push(Opcode::PushInt32 as _);
            self.code
                .extend_from_slice(&constant.get_int32().to_le_bytes());
        } else if constant.is_double() {
            self.code.push(Opcode::PushDouble as _);
            self.code
                .extend_from_slice(&constant.get_double().to_bits().to_le_bytes());
        } else if constant.is_undefined() {
            self.code.push(Opcode::PushUndef as _);
        } else if constant.is_null() {
            self.code.push(Opcode::PushNull as _);
        } else if constant.is_true() {
            self.code.push(Opcode::PushTrue as _);
        } else if constant.is_false() {
            self.code.push(Opcode::PushFalse as _);
        } else {
            let literal;

            if let Some(index) = self.literals.iter().position(|&value| value == constant) {
                literal = index;
            } else {
                literal = self.literals.len();
                self.literals.push(thread, constant);
            }

            self.emit_simple(Opcode::PushConstant);
            self.code.extend_from_slice(&(literal as u32).to_le_bytes());
        }
    }

    pub fn emit_call(&mut self, argc: u16) {
        self.emit_simple(Opcode::Call);
        self.code.extend_from_slice(&argc.to_le_bytes());
    }

    pub fn emit_tail_call(&mut self, argc: u16) {
        self.emit_simple(Opcode::TailCall);
        self.code.extend_from_slice(&argc.to_le_bytes());
    }

    pub fn emit_simple(&mut self, opcode: Opcode) {
        self.code.extend_from_slice(&(opcode as u8).to_le_bytes());
    }

    pub fn emit_ldarg(&mut self, arg: u16) {
        self.code.push(Opcode::LdArg as _);
        self.code.extend_from_slice(&arg.to_le_bytes());
    }

    pub fn jmp(&mut self) -> impl FnOnce(&mut Self) {
        let p = self.code.len();
        self.code.push(Opcode::Branch as u8);
        self.code.extend_from_slice(&(0u32).to_le_bytes());

        move |ctx| {
            let offset = ctx.code.len() as isize - p as isize;
            ctx.code[p + 1..p + 5].copy_from_slice(&(offset as i32).to_le_bytes());
        }
    }

    pub fn cjmp(&mut self, cond: bool) -> impl FnOnce(&mut Self) {
        let p = self.code.len();
        self.code.push(Opcode::Branch as u8);
        self.code.extend_from_slice(&(0u32).to_le_bytes());
        move |ctx| {
            ctx.code[p + 1..p + 5].copy_from_slice(&(5i32).to_le_bytes());
            ctx.code[p] = if cond {
                Opcode::BranchIf
            } else {
                Opcode::BranchIfNot
            } as u8;
        }
    }

    pub fn finalize(&mut self, thread: &mut Thread) -> Handle<CodeBlock> {
        let fragments = Array::new(thread, self.fragments.len(), |_, x| self.fragments[x]);
        let literals = make_vector_from_slice(thread, &self.literals);

        let code = CodeBlock {
            mcode: null(),
            header: ObjectHeader::new(Type::CodeBlock),
            name: Value::encode_undefined_value(),
            literals: Value::encode_object_value(literals),
            fragments,
            num_vars: 0,
            code_len: self.code.len() as _,
            mina: 0,
            stack_size: 0,
            maxa: 0,
            ranges: self.ranges.take(),
            code: [],
        };

        let code = unsafe {
            let mut ptr = thread.allocate_varsize::<CodeBlock>(self.code.len());

            let uninit = ptr.assume_init_mut();
            *uninit = code;

            std::ptr::copy_nonoverlapping(
                self.code.as_ptr(),
                uninit.code.as_mut_ptr(),
                self.code.len(),
            );

            ptr.assume_init()
        };
        if !true {
            disassembly(
                code,
                termcolor::StandardStream::stdout(termcolor::ColorChoice::Never),
            )
            .unwrap();
        }
        code
    }

    pub fn bind(&mut self, lvar: Handle<LVar>, ix: usize) {
        self.group.bindings.insert(lvar.as_ptr() as usize, ix);
    }

    pub fn compile_lambda(&mut self, thread: &mut Thread, lam: &Lambda) {
        let mut closure_compiler = ByteCompiler {
            note: self.note,
            captures: CaptureGroup {
                captures: Vec::new(),
            },
            ranges: Some(ArrayList::with_capacity(thread, 4)),
            code: Vec::new(),
            literals: ArrayList::with_capacity(thread, 16),
            fragments: ArrayList::with_capacity(thread, 4),
            num_locals: 0,
            max_locals: 0,
            parent: Some(self),
            group: Box::new(BindingGroup {
                bindings: HashMap::new(),
                parent: None,
            }),
        };
        let assert_argcount = lam.lvars.len() - lam.optarg as usize;
        closure_compiler.emit_simple(Opcode::Enter);
        if lam.optarg {
            closure_compiler.emit_simple(Opcode::AssertMinArgCount);
            closure_compiler
                .code
                .extend_from_slice(&(assert_argcount as u16).to_le_bytes());
        } else {
            closure_compiler.emit_simple(Opcode::AssertArgCount);
            closure_compiler
                .code
                .extend_from_slice(&(assert_argcount as u16).to_le_bytes());
        }

        for ix in 0..lam.lvars.len() - lam.optarg as usize {
            closure_compiler.emit_ldarg(ix as _);
            let lvar = lam.lvars[ix];

            if !lvar.is_immutable() {
                closure_compiler.emit_simple(Opcode::Box);
            }

            let ix = closure_compiler.next_local_index();
            closure_compiler.bind(lvar, ix);
        }

        if lam.optarg {
            let lvar = lam.lvars[lam.lvars.len() - 1];
            closure_compiler.emit_simple(Opcode::CollectRest);
            closure_compiler
                .code
                .extend_from_slice(&(lam.lvars.len() as u16 - 1).to_le_bytes());
            let ix = closure_compiler.next_local_index();

            closure_compiler.bind(lvar, ix);
        }

        closure_compiler.compile_body(thread, lam.body, lam.lvars.len());
        let mut code = closure_compiler.finalize(thread);
        code.name = if lam.name.is_wrapped_identifier() {
            scm_unwrap_identifier(lam.name.identifier()).into()
        } else if lam.name.is_symbol() {
            lam.name
        } else {
            Value::encode_null_value()
        };
        code.mina = lam.lvars.len() as _;
        if lam.optarg {
            code.maxa = MAX_ARITY;
        } else {
            code.maxa = lam.lvars.len() as _;
        }
        if closure_compiler.captures.captures.is_empty() {
            let proc = make_procedure(thread, code);
            self.emit_load(thread, proc.into());
        } else {
            let captures = std::mem::take(&mut closure_compiler.captures.captures);
            for capture in captures.iter().copied().rev() {
                let lvar = unsafe { std::mem::transmute::<_, Handle<LVar>>(capture) };
                self.resolve_local(lvar, false);
            }

            self.emit_load(thread, code.into());
            self.emit_simple(Opcode::MakeClosure);
            self.code
                .extend_from_slice(&(captures.len() as u16).to_le_bytes());
        }
    }

    pub fn emit_lref(&mut self, ix: u16) {
        self.code.push(Opcode::StackGet as _);
        self.code.extend_from_slice(&ix.to_le_bytes());
    }

    pub fn emit_closure_ref(&mut self, ix: u16) {
        self.code.push(Opcode::ClosureRef as _);
        self.code.extend_from_slice(&ix.to_le_bytes());
    }

    pub fn emit_u16(&mut self, ix: u16) {
        self.code.extend_from_slice(&ix.to_le_bytes());
    }

    pub fn emit_lset(&mut self, ix: u16) {
        self.code.push(Opcode::StackSet as _);
        self.code.extend_from_slice(&ix.to_le_bytes());
    }

    pub fn emit_closure_set(&mut self, ix: u16) {
        self.code.push(Opcode::ClosureSet as _);
        self.code.extend_from_slice(&ix.to_le_bytes());
    }

    fn resolve_local(&mut self, lvar: Handle<LVar>, unbox: bool) {
        let this = self as *mut Self;

        let mut current = Some(this);

        while let Some(cc) = current {
            let cc = unsafe { &mut *cc };
            if let Some(ix) = cc.group.lookup(lvar) {
                if cc as *mut Self == this {
                    self.emit_lref(ix as _);
                    if !lvar.is_immutable() && unbox {
                        self.emit_simple(Opcode::BoxRef);
                    }
                    return;
                } else {
                    let ix = unsafe { (*this).captures.capture(lvar) };

                    if !lvar.is_immutable() && unbox {
                        self.emit_simple(Opcode::ClosureRefUnbox);
                        self.code.extend_from_slice(&(ix as u16).to_le_bytes());
                    } else {
                        self.emit_closure_ref(ix as _);
                    }

                    return;
                }
            }

            current = cc.parent;
        }

        panic!("unresolved local variable: {:?}", lvar.name);
    }

    fn set_local(&mut self, lvar: Handle<LVar>, boxed: bool) {
        let this = self as *mut Self;

        let mut current = Some(this);

        while let Some(cc) = current {
            let cc = unsafe { &mut *cc };
            if let Some(ix) = cc.group.lookup(lvar) {
                if cc as *mut Self == this {
                    if lvar.is_immutable() || !boxed {
                        self.emit_lset(ix as _);
                    } else {
                        self.emit_lref(ix as _);
                        self.emit_simple(Opcode::BoxSet);
                    }
                    return;
                } else {
                    let ix = unsafe { (*this).captures.capture(lvar) };
                    if !boxed {
                        self.emit_closure_set(ix as _);
                    } else {
                        self.emit_closure_ref(ix as _);
                        self.emit_simple(Opcode::BoxSet);
                    }
                    return;
                }
            }

            current = cc.parent;
        }
    }

    pub fn compile_iform(&mut self, thread: &mut Thread, iform: Handle<IForm>, tail: bool) -> bool {
        let start = self.code.len();
        let mut origin = Value::encode_undefined_value();
        let exit = match &*iform {
            IForm::Const(x) => {
                self.emit_load(thread, *x);
                false
            }

            IForm::Define(def) => {
                origin = def.origin;
                self.compile_iform(thread, def.value, false);
                self.emit_simple(Opcode::Define);
                assert!(!def.name.is_empty());
                let id = self.add_constant(thread, def.name) as u16;
                self.code.extend_from_slice(&id.to_le_bytes());
                self.code.push(0);
                false
            }

            IForm::Call(x) => {
                origin = x.origin;
                for arg in x.args.iter().rev() {
                    self.compile_iform(thread, *arg, false);
                }

                self.compile_iform(thread, x.proc, false);

                if tail {
                    self.emit_tail_call(x.args.len() as _);
                    true
                } else {
                    self.emit_call(x.args.len() as _);
                    false
                }
            }

            IForm::Seq(x) => {
                origin = x.origin;
                let mut exit = false;
                for i in 0..x.body.len() {
                    if i > 0 {
                        self.emit_simple(Opcode::Pop);
                    }

                    exit = self.compile_iform(thread, x.body[i], tail && (i == x.body.len() - 1));
                }

                exit
            }
            IForm::Lambda(lam) => {
                origin = lam.origin;
                self.compile_lambda(thread, lam);
                false
            }
            IForm::GRef(x) => {
                origin = x.origin;
                let c = self.add_constant(thread, x.id);
                self.emit_simple(Opcode::GlobalRef);
                self.code.extend_from_slice(&(c as u16).to_le_bytes());
                false
            }

            IForm::GSet(x) => {
                origin = x.origin;
                self.compile_iform(thread, x.value, false);
                let c = self.add_constant(thread, x.id);
                self.emit_simple(Opcode::GlobalSet);
                self.code.extend_from_slice(&(c as u16).to_le_bytes());

                self.emit_simple(Opcode::PushUndef);
                false
            }
            IForm::LRef(x) => {
                origin = x.origin;
                self.resolve_local(x.lvar, true);
                false
            }

            IForm::LSet(x) => {
                origin = x.origin;
                self.compile_iform(thread, x.value, false);
                self.set_local(x.lvar, true);
                self.emit_simple(Opcode::PushUndef);
                false
            }

            IForm::Let(var) => {
                origin = var.origin;
                let mut group = Box::new(BindingGroup {
                    parent: None,
                    bindings: HashMap::new(),
                });

                let init_locals = self.num_locals;

                let exit = match var.scope {
                    LetScope::Let => {
                        for (i, &lvar) in var.lvars.iter().enumerate() {
                            let index = self.next_local_index();
                            group.bindings.insert(lvar.as_ptr() as usize, index);

                            let init = var.inits[i];
                            self.compile_iform(thread, init, false);

                            if lvar.is_immutable() {
                                self.emit_lset(index as _);
                            } else {
                                self.emit_simple(Opcode::StackBox);
                                self.code.extend_from_slice(&(index as u16).to_le_bytes());
                            }
                        }
                        std::mem::swap(&mut self.group, &mut group);
                        self.group.parent = Some(group);

                        self.compile_iform(thread, var.body, tail)
                    }

                    LetScope::Rec => {
                        for (_, &lvar) in var.lvars.iter().enumerate() {
                            let index = self.next_local_index();
                            group.bindings.insert(lvar.as_ptr() as usize, index);

                            if !lvar.is_immutable() {
                                self.emit_simple(Opcode::PushUndef);
                                self.emit_simple(Opcode::StackBox);
                                self.code.extend_from_slice(&(index as u16).to_le_bytes());
                            } else {
                                self.emit_simple(Opcode::PushUndef);
                                self.emit_lset(index as _);
                            }
                        }

                        std::mem::swap(&mut self.group, &mut group);
                        self.group.parent = Some(group);

                        for (i, &lvar) in var.lvars.iter().enumerate() {
                            let init = var.inits[i];
                            self.compile_iform(thread, init, false);

                            let index = self.group.bindings[&(lvar.as_ptr() as usize)];

                            if lvar.is_immutable() {
                                self.emit_lset(index as _);
                            } else {
                                self.emit_lref(index as _);
                                self.emit_simple(Opcode::BoxSet);
                            }
                        }

                        self.compile_iform(thread, var.body, tail)
                    }
                };

                self.group = self.group.parent.take().unwrap();
                self.num_locals = init_locals;
                exit
            }

            IForm::Asm(op) => {
                for arg in op.args.iter() {
                    self.compile_iform(thread, *arg, false);
                }

                self.emit_simple(op.op);

                if let Some(operands) = op.operands.as_ref() {
                    for operand in operands.iter() {
                        match operand {
                            AsmOperand::I16(x) => {
                                self.code.extend_from_slice(&x.to_le_bytes());
                            }
                            AsmOperand::I32(x) => {
                                self.code.extend_from_slice(&x.to_le_bytes());
                            }
                            AsmOperand::I8(x) => {
                                self.code.push(*x as u8);
                            }

                            AsmOperand::Constant(x) => {
                                let ix = self.add_constant(thread, *x);
                                self.code.extend_from_slice(&(ix as u16).to_le_bytes());
                            }
                        }
                    }
                }

                if !op.pushes && !op.exits {
                    self.emit_simple(Opcode::PushUndef);
                }

                op.exits
            }

            IForm::If(cond) => {
                origin = cond.origin;
                self.compile_iform(thread, cond.cond, false);

                let cons = cond.cons;
                let alt = cond.alt;

                let else_jump_ip = self.code.len();

                self.emit_simple(Opcode::NoOp5);
                for _ in 0..4 {
                    self.code.push(0);
                }

                if self.compile_iform(thread, alt, tail) {
                    self.code[else_jump_ip] = Opcode::BranchIf as u8;
                    let diff = (self.code.len() - else_jump_ip) as i32 - 5;
                    let n = diff.to_le_bytes();
                    self.code[else_jump_ip + 1] = n[0];
                    self.code[else_jump_ip + 2] = n[1];
                    self.code[else_jump_ip + 3] = n[2];
                    self.code[else_jump_ip + 4] = n[3];
                    self.compile_iform(thread, cons, true)
                } else {
                    let exit_jump_ip = self.code.len();

                    self.emit_simple(Opcode::NoOp5);
                    for _ in 0..4 {
                        self.code.push(0);
                    }
                    self.code[else_jump_ip] = Opcode::BranchIf as u8;
                    let diff = (self.code.len() - else_jump_ip) as i32 - 5;
                    let n = diff.to_le_bytes();
                    self.code[else_jump_ip + 1] = n[0];
                    self.code[else_jump_ip + 2] = n[1];
                    self.code[else_jump_ip + 3] = n[2];
                    self.code[else_jump_ip + 4] = n[3];

                    if self.compile_iform(thread, cons, tail) {
                        self.code[exit_jump_ip] = Opcode::Return as u8;
                        true
                    } else {
                        self.code[exit_jump_ip] = Opcode::Branch as u8;
                        let diff = (self.code.len() - exit_jump_ip) as i32 - 5;
                        let n = diff.to_le_bytes();
                        self.code[exit_jump_ip + 1] = n[0];
                        self.code[exit_jump_ip + 2] = n[1];
                        self.code[exit_jump_ip + 3] = n[2];
                        self.code[exit_jump_ip + 4] = n[3];

                        false
                    }
                }
            }
            _ => todo!(),
        };

        let end = self.code.len();
        if let (Some(ref mut ranges), Some(note)) = (self.ranges.as_mut(), self.note) {
            let note = note
                .get(&origin)
                .copied()
                .unwrap_or(Value::encode_null_value());
            if !note.is_null() {
                ranges.push(thread, ((start as _, end as _), note, origin));
            }
        }
        exit
    }

    pub fn compile_body(&mut self, thread: &mut Thread, expr: Handle<IForm>, argc: usize) {
        let patch_alloc = self.code.len();
        self.emit_simple(Opcode::NoOp3);
        self.code.push(0);
        self.code.push(0);

        if !self.compile_iform(thread, expr, true) {
            self.emit_simple(Opcode::Return);
        }

        if self.max_locals > argc {
            let diff = self.max_locals - argc;
            self.code[patch_alloc] = Opcode::Alloc as _;
            let n = (diff as u16).to_le_bytes();
            self.code[patch_alloc + 1] = n[0];
            self.code[patch_alloc + 2] = n[1];
        }
    }
}
