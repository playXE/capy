use std::{collections::HashMap, marker::PhantomData, mem::MaybeUninit, ptr::null};

use rsgc::{
    prelude::Handle,
    system::{array::Array, arraylist::ArrayList},
    thread::Thread,
    utils::round_up,
};

use crate::{
    bytecode::{
        disassembler::disassemble,
        encode::Encode,
        encode::*,
        opcodes::*,
        virtual_register::{
            virtual_register_for_argument, virtual_register_for_local, VirtualRegister,
        },
    },
    compaux::scm_unwrap_identifier,
    compile::{Call, Define, GRef, GSet, IForm, If, LRef, LSet, LVar, Lambda, Let, LetScope, Seq},
    runtime::{
        fun::make_procedure,
        object::{CodeBlock, ObjectHeader, Type, MAX_ARITY},
        value::Value,
        vector::make_vector_from_slice,
    },
    vm::callframe::CallFrame,
};

use super::register_id::{RegisterID, RegisterRef};

struct BindingGroup {
    bindings: HashMap<usize, VarOffset>,
    parent: Option<Box<BindingGroup>>,
}

impl BindingGroup {
    pub fn lookup(&self, lvar: Handle<LVar>) -> Option<VarOffset> {
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
#[derive(Copy, Clone)]
pub enum Var {
    Local(RegisterRef),
    Argument(VirtualRegister),
    Captured(usize),
}

#[derive(Copy, Clone)]
pub enum VarOffset {
    Local(RegisterRef),
    Argument(VirtualRegister),
}

impl Var {
    pub fn local(&self) -> Option<RegisterRef> {
        match self {
            Var::Local(r) => Some(*r),
            _ => None,
        }
    }
}

pub struct CallArguments {
    argv: Vec<RegisterRef>,
    padding: usize,
}

impl CallArguments {
    pub fn argument(&self, i: usize) -> RegisterRef {
        self.argv[i]
    }

    pub fn stack_offset(&self, generator: &mut BytecodeGenerator) -> usize {
        if self.argv.len() == 0 {
            let tmp = generator.new_temporary();
            return (-tmp.index() as isize) as usize + CallFrame::HEADER_SIZE_IN_REGISTERS;
        }
        (-self.argv[0].index() as isize) as usize + CallFrame::HEADER_SIZE_IN_REGISTERS
    }

    pub fn argument_count(&self) -> usize {
        self.argv.len()
    }

    pub fn new(generator: &mut BytecodeGenerator, args: Option<&[Handle<IForm>]>) -> Self {
        let mut argc = 0;

        if let Some(args) = args {
            for _ in args.iter() {
                argc += 1;
            }
        }

        let mut argv = Vec::<MaybeUninit<RegisterRef>>::with_capacity(argc);
        unsafe {
            argv.set_len(argc);
        }

        for i in (0..argc).rev() {
            let mut arg = generator.new_temporary();
            arg.inc();
            argv[i] = MaybeUninit::new(arg);
            /*unsafe {
                assert!(
                    i == argv.len() - 1
                        || argv[i].assume_init_ref().index()
                            == argv[i + 1].assume_init_ref().index() - 1,
                    "{} {}",
                    i, argv[i].assume_init_ref().virtual_register(),
                );
            }*/
        }

        Self {
            argv: unsafe { std::mem::transmute(argv) },
            padding: 0,
        }
    }
}

impl Drop for CallArguments {
    fn drop(&mut self) {
        for arg in self.argv.iter_mut() {
            arg.dec();
        }
    }
}

pub struct BytecodeGenerator {
    parent: Option<*mut BytecodeGenerator>,
    constants: ArrayList<Value>,
    code: Vec<u8>,
    callee_locals: Vec<RegisterID>,
    num_callee_locals: usize,
    num_vars: usize,
    ignored_result: RegisterID,
    scope: BindingGroup,
    captures: CaptureGroup,
    fragments: ArrayList<Handle<CodeBlock>>,
}

impl BytecodeGenerator {
    pub fn new() -> Self {
        let thr = Thread::current();
        Self {
            parent: None,
            constants: ArrayList::with_capacity(thr, 4),
            code: Vec::with_capacity(32),
            callee_locals: Vec::with_capacity(32),
            num_callee_locals: 0,
            num_vars: 0,
            ignored_result: RegisterID::new(VirtualRegister(0)),
            scope: BindingGroup {
                bindings: HashMap::new(),
                parent: None,
            },

            captures: CaptureGroup { captures: vec![] },
            fragments: ArrayList::with_capacity(thr, 4),
        }
    }

    pub fn add_constant(&mut self, c: Value) -> u32 {
        for (i, x) in self.constants.iter().enumerate() {
            if x == &c {
                return i as u32;
            }
        }

        self.constants.push(Thread::current(), c);
        self.constants.len() as u32 - 1
    }

    pub fn write<T: Encode>(&mut self, value: T) {
        value.write(self);
    }

    pub fn write_u8(&mut self, value: u8) {
        self.code.push(value)
    }

    pub fn write_u16(&mut self, value: u16) {
        self.code.extend_from_slice(&value.to_le_bytes())
    }

    pub fn write_u32(&mut self, value: u32) {
        self.code.extend_from_slice(&value.to_le_bytes())
    }

    pub fn write_i32(&mut self, value: i32) {
        self.code.extend_from_slice(&value.to_le_bytes())
    }

    pub fn write_u64(&mut self, value: u64) {
        self.code.extend_from_slice(&value.to_le_bytes())
    }

    pub fn new_register(&mut self) -> RegisterRef {
        let reg = virtual_register_for_local(self.callee_locals.len() as i16);
        self.callee_locals.push(RegisterID::new(reg));

        let mut num_callee_locals = self.callee_locals.len().max(self.num_callee_locals);
        num_callee_locals = round_up(num_callee_locals, 2, 0);
        self.num_callee_locals = num_callee_locals;

        RegisterRef::new(self.callee_locals.last_mut().unwrap() as *mut RegisterID)
    }

    pub fn reclaim_free_registers(&mut self) {
       while !self.callee_locals.is_empty() && self.callee_locals.last().unwrap().refcnt() == 0 {
            self.callee_locals.pop();
       }
    }

    pub fn new_temporary(&mut self) -> RegisterRef {
        self.reclaim_free_registers();
        let mut result = self.new_register();
        
        result.set_temporary(true);
        result
    }

    pub fn add_var(&mut self) -> RegisterRef {
        self.reclaim_free_registers();
        self.num_vars += 1;
        let mut result = self.new_register();
        assert_eq!(
            result.virtual_register().to_local(),
            self.num_vars as i16 - 1
        );
        result.inc(); // we never free this slot
        result
    }

    pub fn ignored_result(&self) -> RegisterRef {
        RegisterRef::new(&self.ignored_result as *const RegisterID as *mut RegisterID)
    }

    /// Returns a place to write intermediate values of an operation
    /// which reuses dst if it is safe to do so.
    pub fn temp_destination(&mut self, dst: Option<RegisterRef>) -> RegisterRef {
        dst.filter(|&x| x != self.ignored_result() && x.is_temporary())
            .unwrap_or_else(|| self.new_temporary())
    }

    /// Returns the place to write the final output of an operation.
    pub fn final_destination(
        &mut self,
        original_dst: Option<RegisterRef>,
        temp_dst: Option<RegisterRef>,
    ) -> RegisterRef {
        if original_dst
            .filter(|&x| x != self.ignored_result())
            .is_some()
        {
            return original_dst.unwrap();
        }

        if temp_dst.filter(|x| x.is_temporary()).is_some() {
            return temp_dst.unwrap();
        }

        self.new_temporary()
    }

    pub fn push_lexical_scope(&mut self, lvars: &[Handle<LVar>]) {
        let mut group = BindingGroup {
            bindings: HashMap::new(),
            parent: None,
        };

        for lvar in lvars {
            let reg = self.add_var();
            group
                .bindings
                .insert(lvar.as_ptr() as usize, VarOffset::Local(reg));
        }

        let prev = std::mem::replace(&mut self.scope, group);
        self.scope.parent = Some(Box::new(prev));
    }

    pub fn pop_lexical_scope(&mut self) {
        for (_, reg) in self.scope.bindings.iter_mut() {
            match reg {
                VarOffset::Local(reg) => reg.dec(),
                _ => (),
            }
        }
        let prev = self.scope.parent.take().expect("No parent scope");
        self.scope = *prev;
    }

    pub fn mov(&mut self, dst: Option<RegisterRef>, src: RegisterRef) -> Option<RegisterRef> {
        if dst.is_none() {
            return Some(src);
        }
        let dst = dst.unwrap();
        if dst == self.ignored_result() {
            None
        } else {
            if dst != src {
                OpMov::new(dst.virtual_register(), src.virtual_register()).write(self);
                Some(dst)
            } else {
                Some(src)
            }
        }
    }



    pub fn variable(&mut self, lvar: Handle<LVar>) -> Var {
        let this: *mut BytecodeGenerator = self as *mut Self;

        let mut current = Some(this);

        while let Some(cc) = current {
            let cc = unsafe { &mut *cc };

            if let Some(ix) = cc.scope.lookup(lvar) {
                if cc as *mut Self == this {
                    match ix {
                        VarOffset::Local(reg) => return Var::Local(reg),
                        VarOffset::Argument(reg) => return Var::Argument(reg),
                    }
                } else {
                    let ix = unsafe { (*this).captures.capture(lvar) };
                    return Var::Captured(ix);
                }
            }

            current = cc.parent;
        }

        panic!("variable '{}' not found", lvar.name)
    }

    pub fn emit_lref(&mut self, lref: &LRef, dst: Option<RegisterRef>) -> Option<RegisterRef> {
        let var = self.variable(lref.lvar);

        match var {
            Var::Local(ix) => {
                if dst == Some(self.ignored_result()) {
                    return None;
                }

                if lref.lvar.is_immutable() {
                    return self.mov(dst, ix);
                } else {
                    let dst = self.final_destination(dst, None);
                    OpBoxRef::new(dst.virtual_register(), ix.virtual_register()).write(self);
                    return Some(dst);
                }
            }

            Var::Captured(ix) => {
                let final_dest = self.final_destination(dst, None);

                if lref.lvar.is_immutable() {
                    OpClosureRef::new(final_dest.virtual_register(), ix as _).write(self);
                } else {
                    OpClosureRefUnbox::new(final_dest.virtual_register(), ix as _).write(self);
                }

                return Some(final_dest);
            }

            Var::Argument(reg) => {
                let dst = self.final_destination(dst, None);
                if dst == self.ignored_result() {
                    return None;
                }

                OpMov::new(dst.virtual_register(), reg).write(self);

                return Some(dst);
            }
        }
    }

    pub fn emit_lset(&mut self, lset: &LSet, dst: Option<RegisterRef>) -> Option<RegisterRef> {
        let var = self.variable(lset.lvar);

        let (_, result) = self.emit_iform(&lset.value, false, dst);

        let result = result.unwrap();

        match var {
            Var::Local(ix) => {
                if lset.lvar.is_immutable() {
                    return self.mov(Some(ix), result);
                } else {
                    OpBoxSet::new(ix.virtual_register(), result.virtual_register()).write(self);
                    return Some(result);
                }
            }

            Var::Captured(ix) => {
                if lset.lvar.is_immutable() {
                    OpClosureSet::new(result.virtual_register(), ix as _).write(self);
                } else {
                    OpClosureRefBox::new(result.virtual_register(), ix as _).write(self);
                }

                return Some(result);
            }

            Var::Argument(reg) => {
                OpMov::new(reg, result.virtual_register()).write(self);
                return Some(result);
            }
        }
    }

    pub fn emit_call(
        &mut self,
        call: &Call,
        tail: bool,
        dst: Option<RegisterRef>,
    ) -> (bool, Option<RegisterRef>) {
        /*let return_value = self.final_destination(dst, None);

        let temp_func = self.temp_destination(dst);
        let (_, function) = self.emit_iform(&call.proc, false, Some(temp_func));
        let function = function.unwrap();

        let arguments = CallArguments::new(self, Some(&call.args));
        let mut i = 0;
        for arg in call.args.iter() {
            
            let (_, _arg) = self.emit_iform(arg, false, Some(arguments.argument(i)));
            i += 1;
        }
        let argv = arguments.stack_offset(self);

        for _ in 0..CallFrame::HEADER_SIZE_IN_REGISTERS {
            self.new_temporary();
        }

        if tail {
            OpTailCall::new(
                function.virtual_register(),
                argv as _,
                arguments.argument_count() as _,
            )
            .write(self);
            (true, None)
        } else {
            OpCall::new(
                return_value.virtual_register(),
                function.virtual_register(),
                argv as _,
                arguments.argument_count() as _,
            )
            .write(self);
            (false, Some(return_value))
        }*/

        let mut func = self.temp_destination(dst);
        func.inc();
        self.emit_iform(&call.proc, false, Some(func));

        let arguments = CallArguments::new(self, Some(&call.args));

        let mut i = 0;
        for arg in call.args.iter() {
            
            let (_, _arg) = self.emit_iform(arg, false, Some(arguments.argument(i)));
            i += 1;
        }
        let argv = arguments.stack_offset(self);

        for _ in 0..CallFrame::HEADER_SIZE_IN_REGISTERS {
            self.new_temporary();
        }
        func.dec();
        if tail {
            OpTailCall::new(
                func.virtual_register(),
                argv as _,
                arguments.argument_count() as _,
            )
            .write(self);
            (true, None)
        } else {
            let return_value = self.final_destination(dst, None);
            OpCall::new(
                return_value.virtual_register(),
                func.virtual_register(),
                argv as _,
                arguments.argument_count() as _,
            )
            .write(self);
            (false, Some(return_value))
        }

    }

    fn emit_let(
        &mut self,
        var: &Let,
        tail: bool,
        dst: Option<RegisterRef>,
    ) -> (bool, Option<RegisterRef>) {
        self.push_lexical_scope(&var.lvars);

        match var.scope {
            LetScope::Let => {
                for (lvar, init) in var.lvars.iter().zip(var.inits.iter()) {
                    let loc = self.variable(*lvar).local().expect("must be local");
                    let (_, _) = self.emit_iform(init, false, Some(loc));
                }
            }

            LetScope::Rec => {
                let c = VirtualRegister::new_constant(
                    self.add_constant(Value::encode_undefined_value()) as _,
                );
                for lvar in var.lvars.iter() {
                    let loc = self.variable(*lvar).local().expect("must be local");
                    if !lvar.is_immutable() {
                        OpBox::new(loc.virtual_register(), c).write(self);
                    } else {
                        OpMov::new(loc.virtual_register(), c).write(self);
                    }
                }

                for (lvar, init) in var.lvars.iter().zip(var.inits.iter()) {
                    let loc = self.variable(*lvar).local().expect("must be local");
                    if lvar.is_immutable() {
                        let (_, _) = self.emit_iform(init, false, Some(loc));
                    } else {
                        let temp = self.temp_destination(None);
                        let (_, _) = self.emit_iform(init, false, Some(temp));
                        OpBoxSet::new(loc.virtual_register(), temp.virtual_register()).write(self);
                    }
                }
            }
        }

        let (exit, res) = self.emit_iform(&var.body, tail, dst);

        self.pop_lexical_scope();
        let res = res.and_then(|src| {
            self.mov(dst, src)
        });

        (exit, res)
    }

    pub fn emit_const(&mut self, c: Value, dst: Option<RegisterRef>) -> Option<RegisterRef> {
        let dest = self.final_destination(dst, None);

        if c.is_int32() {
            OpMovi::new(dest.virtual_register(), c.get_int32()).write(self);
        } else {
            let ix = self.add_constant(c);
            OpMov::new(
                dest.virtual_register(),
                VirtualRegister::new_constant(ix as _),
            )
            .write(self);
        }

        Some(dest)
    }

    pub fn emit_define(&mut self, def: &Define, dst: Option<RegisterRef>) -> RegisterRef {
        let dest = self.temp_destination(dst);

        let (_, _) = self.emit_iform(&def.value, false, Some(dest));

        let name = self.add_constant(def.name);

        OpDefine::new(dest.virtual_register(), name as _).write(self);
        OpMov::new(
            dest.virtual_register(),
            VirtualRegister::new_constant(name as _),
        )
        .write(self);
        dest
    }

    pub fn emit_gref(&mut self, gref: &GRef, dst: Option<RegisterRef>) -> Option<RegisterRef> {
        let dest = self.final_destination(dst, None);

        let ix = self.add_constant(gref.id);

        OpGlobalRef::new(dest.virtual_register(), ix as _).write(self);
        Some(dest)
    }

    pub fn emit_gset(&mut self, gset: &GSet, dst: Option<RegisterRef>) -> Option<RegisterRef> {
        let src = self.temp_destination(dst);
        let (_, _) = self.emit_iform(&gset.value, false, Some(src));

        let ix = self.add_constant(gset.id);

        OpGlobalSet::new(src.virtual_register(), ix as _).write(self);
        Some(src)
    }

    pub fn emit_seq(
        &mut self,
        seq: &Seq,
        tail: bool,
        dst: Option<RegisterRef>,
    ) -> (bool, Option<RegisterRef>) {
        let dst = self.final_destination(dst, None);
        let mut exit = false;
        for (i, iform) in seq.body.iter().enumerate() {
            let tail = tail && i == seq.body.len() - 1;
            let (ex, _) = self.emit_iform(iform, tail, Some(dst));
            exit |= ex;
        }

        (exit, Some(dst))
    }

    pub fn bind(&mut self, lvar: Handle<LVar>, ix: VarOffset) {
        self.scope.bindings.insert(lvar.as_ptr() as usize, ix);
    }

    pub fn emit_lambda(
        &mut self,
        lambda: &Lambda,
        dst: Option<RegisterRef>,
    ) -> (bool, Option<RegisterRef>) {
        let mut closure_compiler = BytecodeGenerator::new();
        closure_compiler.parent = Some(self);

        let assert_argcount = lambda.lvars.len() - lambda.optarg as usize;

        closure_compiler.write(OpEnter::new());

        if lambda.optarg {
            closure_compiler.write(OpAssertMinArgCount::new(assert_argcount as _));
        } else {
            closure_compiler.write(OpAssertArgCount::new(assert_argcount as _));
        }

        for ix in 0..lambda.lvars.len() - lambda.optarg as usize {
            let lvar = lambda.lvars[ix];

            if lvar.is_immutable() {
                closure_compiler.bind(
                    lvar,
                    VarOffset::Argument(virtual_register_for_argument(ix as _)),
                );
            } else {
                let var = closure_compiler.add_var();
                closure_compiler.bind(lvar, VarOffset::Local(var));
                closure_compiler.write(OpBox::new(
                    var.virtual_register(),
                    virtual_register_for_argument(ix as _),
                ));
            }
        }

        if lambda.optarg {
            let lvar = lambda.lvars[lambda.lvars.len() - 1];
            let var = closure_compiler.add_var();
            closure_compiler.write(OpCollectRest::new(
                var.virtual_register(),
                lambda.lvars.len() as u16 - 1,
            ));

            closure_compiler.bind(lvar, VarOffset::Local(var));
        }

        closure_compiler.compile_body(&lambda.body);

        let mut code = closure_compiler.finalize();

        code.name = if lambda.name.is_wrapped_identifier() {
            scm_unwrap_identifier(lambda.name.identifier()).into()
        } else if lambda.name.is_symbol() {
            lambda.name
        } else {
            Value::encode_null_value()
        };

        code.mina = lambda.lvars.len() as _;
        if lambda.optarg {
            code.maxa = MAX_ARITY;
        } else {
            code.maxa = lambda.lvars.len() as _;
        }

        let dest = if closure_compiler.captures.captures.is_empty() {
            let proc = make_procedure(Thread::current(), code);
            let proc = self.add_constant(proc.into());
            let dest = self.final_destination(dst, None);
            OpMov::new(
                dest.virtual_register(),
                VirtualRegister::new_constant(proc as _),
            )
            .write(self);
            dest
        } else {
            let mut dest = self.final_destination(dst, None);
            dest.inc();
            let proc = self.add_constant(code.into());
            OpMakeClosure::new(
                dest.virtual_register(),
                proc as _,
                closure_compiler.captures.captures.len() as _,
            )
            .write(self);

            for (i, &capture) in closure_compiler.captures.captures.iter().enumerate() {
                let lvar = unsafe { std::mem::transmute::<_, Handle<LVar>>(capture) };
                let var = self.variable(lvar);
                match var {
                    Var::Local(local) => {
                        OpClosureCapture::new(
                            dest.virtual_register(),
                            i as _,
                            local.virtual_register(),
                        )
                        .write(self);
                    }
                    Var::Argument(arg) => {
                        OpClosureCapture::new(dest.virtual_register(), i as _, arg).write(self);
                    }

                    Var::Captured(arg) => {
                        let src = self.temp_destination(None);
                        OpClosureRef::new(src.virtual_register(), arg as _).write(self);
                        OpClosureCapture::new(
                            dest.virtual_register(),
                            i as _,
                            src.virtual_register(),
                        )
                        .write(self);
                    }
                }
            }

            dest.dec();
            dest
        };

        (false, Some(dest))
    }

    pub fn offset_from(&self, ip: usize) -> isize {
        self.code.len() as isize - ip as isize
    }

    pub fn emit_if(
        &mut self,
        cond: &If,
        tail: bool,
        dst: Option<RegisterRef>,
    ) -> (bool, Option<RegisterRef>) {
        let consequent = cond.cons;
        let alternative = cond.alt;
        let test = cond.cond;
        
 
        let cond = self.emit_iform(&test, false, None).1.unwrap();

        let else_jump_ip = self.code.len();
        for _ in 0..7 {
            self.write(OpNop::new());
        }

        let (exit, _src) = self.emit_iform(&alternative, tail, None);
        
        if exit {
            self.code[else_jump_ip] = OP_BRANCH_IF;
            
            let reg = cond.virtual_register().0.to_le_bytes();
            self.code[else_jump_ip + 1] = reg[0];
            self.code[else_jump_ip + 2] = reg[1];
            let off = (self.offset_from(else_jump_ip) as i32 - 7).to_le_bytes();
            self.code[else_jump_ip + 3] = off[0];
            self.code[else_jump_ip + 4] = off[1];
            self.code[else_jump_ip + 5] = off[2];
            self.code[else_jump_ip + 6] = off[3];

            let (exit, src) = self.emit_iform(&consequent, tail, None);
            
            if !exit {
                let src = if !src.is_none() {
                    let _c = self.add_constant(Value::encode_undefined_value());
                    todo!()
                } else {
                    self.mov(dst, src.unwrap())
                };
                return (exit, src);
            }
            return (exit, src);
        }

        let exit_jump_ip = self.code.len();

        for _ in 0..7 {
            self.write(OpNop::new());
        }

        self.code[else_jump_ip] = OP_BRANCH_IF;
        let reg = cond.virtual_register().0.to_le_bytes();
        self.code[else_jump_ip + 1] = reg[0];
        self.code[else_jump_ip + 2] = reg[1];
        let off = (self.offset_from(else_jump_ip) as i32 - 7).to_le_bytes();
        self.code[else_jump_ip + 3] = off[0];
        self.code[else_jump_ip + 4] = off[1];
        self.code[else_jump_ip + 5] = off[2];
        self.code[else_jump_ip + 6] = off[3];

        let (exit, src) = self.emit_iform(&consequent, tail, None);
        if exit {
            self.code[exit_jump_ip] = OP_RETURN;
            let reg = src.unwrap().virtual_register().0.to_le_bytes();
            self.code[exit_jump_ip + 1] = reg[0];
            self.code[exit_jump_ip + 2] = reg[1];
            return (true, None);
        }
        let dst = self.mov(dst, src.unwrap());
        self.code[exit_jump_ip] = OP_BRANCH;
        let off = (self.offset_from(exit_jump_ip) as i32 - 7).to_le_bytes();
        self.code[exit_jump_ip + 1] = off[0];
        self.code[exit_jump_ip + 2] = off[1];
        self.code[exit_jump_ip + 3] = off[2];
        self.code[exit_jump_ip + 4] = off[3];
        
        (false, dst)
    }

    pub fn emit_iform(
        &mut self,
        iform: &IForm,
        tail: bool,
        dst: Option<RegisterRef>,
    ) -> (bool, Option<RegisterRef>) {
        match iform {
            IForm::Const(x) => (false, self.emit_const(*x, dst)),
            IForm::Define(x) => (false, Some(self.emit_define(x, dst))),
            IForm::LRef(x) => (false, self.emit_lref(x, dst)),
            IForm::LSet(x) => (false, self.emit_lset(x, dst)),
            IForm::Call(x) => self.emit_call(x, tail, dst),
            IForm::GRef(x) => (false, self.emit_gref(x, dst)),
            IForm::GSet(x) => (false, self.emit_gset(x, dst)),
            IForm::Let(x) => self.emit_let(x, tail, dst),
            IForm::Seq(x) => self.emit_seq(x, tail, dst),
            IForm::Lambda(x) => self.emit_lambda(x, dst),
            IForm::If(x) => self.emit_if(x, tail, dst),
            _ => todo!(),
        }
    }

    pub fn compile_body(&mut self, iform: &IForm) {
        let (exit, src) = self.emit_iform(iform, true, None);
        if !exit {
            let src = if src.is_none() {
                let c = self.add_constant(Value::encode_undefined_value());

                VirtualRegister::new_constant(c as _)
            } else {
                src.unwrap().virtual_register()
            };

            OpReturn::new(src).write(self);
        }
    }

    pub fn finalize(&mut self) -> Handle<CodeBlock> {
        let thread = Thread::current();
        let fragments = Array::new(thread, self.fragments.len(), |_, x| self.fragments[x]);
        let literals = make_vector_from_slice(thread, &self.constants);

        let code = CodeBlock {
            mcode: null(),
            header: ObjectHeader::new(Type::CodeBlock),
            name: Value::encode_undefined_value(),
            literals: Value::encode_object_value(literals),
            fragments,
            code_len: self.code.len() as _,
            num_vars: self.num_vars as _,
            mina: 0,
            stack_size: self.num_callee_locals as _,
            maxa: 0,
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
        if true {
            let mut out = String::new();
            disassemble(&code.code(), &mut out).unwrap();
            println!("disassembly for {}:\n{}", code.name, out);
        }
        code
    }
}
