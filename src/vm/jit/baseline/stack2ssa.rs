#![allow(unused_imports, dead_code, unused_variables)]
use std::{
    cell::Cell,
    collections::BTreeMap,
    mem::{offset_of, size_of},
    rc::Rc,
};

use b3::{
    jit::register_set::RegisterSetBuilder, variable::VariableId, BasicBlockBuilder, BlockId,
    Procedure, Reg, ValueId, ValueRep, ValueRepKind,
};
use macroassembler::{
    assembler::{
        abstract_macro_assembler::{AbsoluteAddress, Address, Label},
        RelationalCondition, ResultCondition, TargetMacroAssembler,
    },
    jit::{
        gpr_info::{
            ARGUMENT_GPR0, ARGUMENT_GPR1, ARGUMENT_GPR2, RETURN_VALUE_GPR, RETURN_VALUE_GPR2,
        },
        helpers::AssemblyHelpers,
    },
};
use rsgc::{prelude::Handle, thread::Thread};

use crate::{
    op::Opcode,
    runtime::{
        object::{CodeBlock, ObjectHeader, ScmResult, Type, GLOC},
        value::Value,
    },
    vm::{callframe::CallFrame, VMType},
    vm::{jit::baseline::thunks::*, scm_vm},
};

struct BasicBlock {
    low: usize,
    high: usize,
    max: usize,

    block: BlockId,
    stack: Vec<ValueId>,
    flags: u8,
}

pub struct Stack2SSA<'a> {
    pub procedure: &'a mut Procedure,
    locals: Vec<VariableId>,
    operand_stack: Vec<ValueId>,
    code: Handle<CodeBlock>,
    instr_index: usize,
    current_block: BasicBlock,
    ungenerated: Vec<BasicBlock>,
    blocks: BTreeMap<u32, (BlockId, Vec<ValueId>)>,
    end_of_basic_block: bool,
    fallthrough: bool,
    runoff: usize,
    vm: ValueId,
    cfr: ValueId,
    exit_label: Rc<Cell<Label>>,
    exit_block: BlockId,
    #[cfg(windows)]
    result_reg: ValueId,
}

impl<'a> Stack2SSA<'a> {
    pub fn new(
        procedure: &'a mut Procedure,
        code_block: Handle<CodeBlock>,
        vm: ValueId,
        cfr: ValueId,
        entry: BlockId,
    ) -> Self {
        Self {
            procedure,
            locals: vec![],
            operand_stack: vec![],
            code: code_block,
            instr_index: 0,
            current_block: BasicBlock {
                max: usize::MAX,
                low: 0,
                high: 0,
                block: entry,
                stack: vec![],
                flags: 0,
            },
            vm,
            ungenerated: vec![],
            blocks: BTreeMap::new(),
            end_of_basic_block: false,
            fallthrough: false,
            runoff: usize::MAX,
            cfr,
            exit_label: Rc::new(Cell::new(Label::unset())),
            exit_block: BlockId(0),
        }
    }

    fn print_val(&mut self, val: ValueId) {
        extern "C" fn print(x: Value) {
            println!("{:x} {:p}", x.get_raw(), scm_vm().sp);
        }

        let mut builder = BasicBlockBuilder::new(self.procedure, self.current_block.block);
        let print = builder.const64(print as _);
        builder.ccall(b3::Type::Void, print, &[val], b3::Effects::for_call());
    }

    pub fn generate(&mut self) {
        self.exit_block = self.procedure.add_block(1.0);
        self.generate_from(0);

        while let Some(mut block) = self.ungenerated.pop() {
            self.operand_stack.clear();
            self.operand_stack = std::mem::take(&mut block.stack);

            self.current_block = block;

            self.generate_from(self.current_block.low);
        }

        let exit = self.exit_label.clone();
        let mut builder = BasicBlockBuilder::new(self.procedure, self.exit_block);

        let patchpoint = builder.patchpoint(b3::Type::Void);
        let effects = builder.procedure.patchpoint_effects_mut(patchpoint);
        effects.exit_sideways = true;
        effects.terminal = true;

        builder.procedure.stackmap_set_generator(
            patchpoint,
            Rc::new(move |jit, params| {
                let label = jit.label();
                exit.set(label);
                params.emit_epilogue(jit);
            }),
        );
    }

    pub fn get_or_create_block(&mut self, offset: u32) -> &(BlockId, Vec<ValueId>) {
        self.end_of_basic_block = true;

        self.blocks.entry(offset).or_insert_with(|| {
            let dest_block = self.procedure.add_block(1.0);
            let mut builder = BasicBlockBuilder::new(self.procedure, dest_block);
            let mut phis = vec![];
            for _ in 0..self.operand_stack.len() {
                phis.push(builder.phi(b3::Type::Int64));
            }

            let bb = BasicBlock {
                low: offset as _,
                high: offset as _,
                max: usize::MAX,
                block: dest_block,
                stack: phis.clone(),
                flags: 0,
            };

            self.ungenerated.push(bb);
            (dest_block, phis)
        })
    }

    fn generate_from(&mut self, from: usize) {
        let mut index = from;

        self.end_of_basic_block = false;
        self.fallthrough = false;

        loop {
            self.current_block.high = from;
            self.instr_index = index;
            let opcode = self.code[index];
            index += 1;

            macro_rules! read2 {
                () => {{
                    let [a, b]: [u8; 2] = self.code.code()[index..index + 2].try_into().unwrap();
                    index += 2;
                    u16::from_le_bytes([a, b])
                }};
            }

            macro_rules! read4 {
                () => {{
                    let [a, b, c, d]: [u8; 4] =
                        self.code.code()[index..index + 4].try_into().unwrap();
                    index += 4;
                    u32::from_le_bytes([a, b, c, d])
                }};
            }

            macro_rules! read8 {
                () => {{
                    let [a, b, c, d, e, f, g, h]: [u8; 8] =
                        self.code.code()[index..index + 8].try_into().unwrap();
                    index += 8;
                    u64::from_le_bytes([a, b, c, d, e, f, g, h])
                }};
            }

            let op = unsafe { std::mem::transmute::<_, crate::op::Opcode>(opcode) };
            println!("{:?}", op);
            match op {
                Opcode::Alloc => {
                    let n = read2!();

                    for _ in 0..n {
                        let var = BasicBlockBuilder::new(self.procedure, self.current_block.block)
                            .const64(Value::encode_undefined_value().get_raw());
                        self.operand_stack.push(var);
                    }
                }

                Opcode::Enter
                | Opcode::EnterBlacklisted
                | Opcode::EnterCompiling
                | Opcode::EnterJit => {}

                Opcode::NoOp => {}
                Opcode::Pop => {
                    self.operand_stack.pop().unwrap();
                }

                Opcode::Popn => {
                    let n = read2!();
                    for _ in 0..n {
                        self.operand_stack.pop().unwrap();
                    }
                }

                Opcode::Dup => {
                    let top = self.operand_stack.last().unwrap();
                    self.operand_stack.push(*top);
                }

                Opcode::Swap => {
                    let a = self.operand_stack.pop().unwrap();
                    let b = self.operand_stack.pop().unwrap();
                    self.operand_stack.push(a);
                    self.operand_stack.push(b);
                }

                Opcode::AssertArgCount => {
                    let check_argcn = read2!();

                    let real_argc_off = offset_of!(CallFrame, argc) as i32;

                    let mut builder =
                        BasicBlockBuilder::new(self.procedure, self.current_block.block);

                    let real_argc =
                        builder.load(b3::Type::Int32, self.cfr, real_argc_off, None, None);
                    let check_argc = builder.const32(check_argcn as _);

                    let cmp = builder.binary(b3::Opcode::Equal, real_argc, check_argc);

                    let check_ok = builder.procedure.add_block(1.0);
                    let check_err = builder.procedure.add_block(1.0);

                    builder.branch(cmp, check_ok, (check_err, b3::Frequency::Rare));

                    builder.switch_to_block(check_err);
                    let patchpoint = builder.patchpoint(b3::Type::Void);
                    builder
                        .procedure
                        .stackmap_append_some_register(patchpoint, self.cfr);
                    let effects = builder.procedure.patchpoint_effects_mut(patchpoint);
                    effects.terminal = true;
                    effects.exit_sideways = true;
                    builder.procedure.stackmap_set_generator(
                        patchpoint,
                        Rc::new(move |jit, params| {
                            let reg = params[0].get_reg();

                            if reg.gpr() != ARGUMENT_GPR0 {
                                jit.mov(reg.gpr(), ARGUMENT_GPR0);
                            }
                            jit.call_op(Some(AbsoluteAddress::new(
                                baseline_assert_argcount_err as _,
                            )));
                            jit.mov(RETURN_VALUE_GPR, RETURN_VALUE_GPR2);
                            jit.mov(ScmResult::JIT_ERR as i32, RETURN_VALUE_GPR);
                            params.emit_epilogue(jit);
                        }),
                    );

                    builder.switch_to_block(check_ok);
                    self.current_block.block = check_ok;
                }

                Opcode::AssertMinArgCount => {
                    let check_argcn = read2!();

                    let real_argc_off = offset_of!(CallFrame, argc) as i32;

                    let mut builder =
                        BasicBlockBuilder::new(self.procedure, self.current_block.block);

                    let real_argc =
                        builder.load(b3::Type::Int32, self.cfr, real_argc_off, None, None);
                    let check_argc = builder.const32(check_argcn as _);

                    let cmp = builder.binary(b3::Opcode::LessThan, real_argc, check_argc);

                    let check_ok = builder.procedure.add_block(1.0);
                    let check_err = builder.procedure.add_block(1.0);

                    builder.branch(cmp, check_ok, (check_err, b3::Frequency::Rare));

                    builder.switch_to_block(check_err);
                    let patchpoint = builder.patchpoint(b3::Type::Void);
                    builder
                        .procedure
                        .stackmap_append_some_register(patchpoint, self.cfr);
                    let effects = builder.procedure.patchpoint_effects_mut(patchpoint);
                    effects.terminal = true;
                    effects.exit_sideways = true;
                    builder.procedure.stackmap_set_generator(
                        patchpoint,
                        Rc::new(move |jit, params| {
                            let reg = params[0].get_reg();

                            if reg.gpr() != ARGUMENT_GPR0 {
                                jit.mov(reg.gpr(), ARGUMENT_GPR0);
                            }
                            jit.call_op(Some(AbsoluteAddress::new(
                                baseline_assert_argcount_err as _,
                            )));
                            jit.mov(RETURN_VALUE_GPR, RETURN_VALUE_GPR2);
                            jit.mov(ScmResult::JIT_ERR as i32, RETURN_VALUE_GPR);
                            params.emit_epilogue(jit);
                        }),
                    );

                    builder.switch_to_block(check_ok);
                    self.current_block.block = check_ok;
                }

                Opcode::PushConstant | Opcode::PushProcedure => {
                    let c = read2!();
                    let literal = self.code.literals.vector_ref(c as _);
                    let val = BasicBlockBuilder::new(self.procedure, self.current_block.block)
                        .const64(literal.get_raw());
                    self.operand_stack.push(val);
                }

                Opcode::PushDouble => {
                    let v = read8!();
                    let val = BasicBlockBuilder::new(self.procedure, self.current_block.block)
                        .const64(Value::encode_f64_value(f64::from_bits(v)).get_raw());

                    self.operand_stack.push(val);
                }

                Opcode::PushFalse => {
                    let val = BasicBlockBuilder::new(self.procedure, self.current_block.block)
                        .const64(Value::encode_bool_value(false).get_raw());
                    self.operand_stack.push(val);
                }

                Opcode::PushTrue => {
                    let val = BasicBlockBuilder::new(self.procedure, self.current_block.block)
                        .const64(Value::encode_bool_value(true).get_raw());
                    self.operand_stack.push(val);
                }

                Opcode::PushUndef => {
                    let val = BasicBlockBuilder::new(self.procedure, self.current_block.block)
                        .const64(Value::encode_undefined_value().get_raw());
                    self.operand_stack.push(val);
                }

                Opcode::PushInt32 => {
                    let v = read4!();
                    let val = BasicBlockBuilder::new(self.procedure, self.current_block.block)
                        .const64(Value::encode_int32(v as _).get_raw());
                    self.operand_stack.push(val);
                }

                Opcode::Branch => {
                    let x = read4!() as i32;

                    let target = (index as i32 + x) as usize;

                    if target != index {
                        let (target, phis) = self.get_or_create_block(target as _).clone();

                        for (i, phi) in phis.iter().copied().enumerate() {
                            let val = self.operand_stack[i];
                            BasicBlockBuilder::new(self.procedure, self.current_block.block)
                                .upsilon(val, Some(phi));
                        }

                        BasicBlockBuilder::new(self.procedure, self.current_block.block)
                            .jump(Some(target));

                        self.end_of_basic_block = true;
                    }
                }

                Opcode::BranchIf => {
                    let x = read4!() as i32;
                    let val = self.operand_stack.pop().unwrap();

                    let target = (index as i32 + x) as usize;

                    let mut builder =
                        BasicBlockBuilder::new(self.procedure, self.current_block.block);

                    let false_const = builder.const64(Value::encode_bool_value(false).get_raw());
                    let cond = builder.binary(b3::Opcode::Equal, val, false_const);

                    let (target_if_true, phis) = self.get_or_create_block(target as _).clone();
                    let (target_if_false, phis_false) =
                        self.get_or_create_block(index as _).clone();
                    for (i, phi) in phis.iter().copied().enumerate() {
                        let val = self.operand_stack[i];
                        BasicBlockBuilder::new(self.procedure, self.current_block.block)
                            .upsilon(val, Some(phi));
                    }

                    for (i, phi) in phis_false.iter().copied().enumerate() {
                        let val = self.operand_stack[i];
                        BasicBlockBuilder::new(self.procedure, self.current_block.block)
                            .upsilon(val, Some(phi));
                    }

                    let mut builder =
                        BasicBlockBuilder::new(self.procedure, self.current_block.block);

                    builder.branch(
                        cond,
                        target_if_false,
                        (target_if_true, b3::Frequency::Normal),
                    );
                    self.end_of_basic_block = true;
                }

                Opcode::BranchIfNot => {
                    let x = read4!() as i32;
                    let val = self.operand_stack.pop().unwrap();

                    let target = (index as i32 + x) as usize;

                    let mut builder =
                        BasicBlockBuilder::new(self.procedure, self.current_block.block);

                    let true_const = builder.const64(Value::encode_bool_value(false).get_raw());
                    let cond = builder.binary(b3::Opcode::Equal, val, true_const);

                    let (target_if_true, phis) = self.get_or_create_block(target as _).clone();
                    let target_if_false = self.procedure.add_block(1.0);

                    for (i, phi) in phis.iter().copied().enumerate() {
                        let val = self.operand_stack[i];
                        BasicBlockBuilder::new(self.procedure, self.current_block.block)
                            .upsilon(val, Some(phi));
                    }

                    let mut builder =
                        BasicBlockBuilder::new(self.procedure, self.current_block.block);
                    builder.branch(
                        cond,
                        target_if_true,
                        (target_if_false, b3::Frequency::Normal),
                    );
                    self.end_of_basic_block = true;
                }

                Opcode::LdArg => {
                    let n = read2!();

                    //let arg = (*cfr).args.as_ptr().add(n as usize).read();

                    let arg_offset = size_of::<CallFrame>() + n as usize * size_of::<Value>();

                    let arg = BasicBlockBuilder::new(self.procedure, self.current_block.block)
                        .load(b3::Type::Int64, self.cfr, arg_offset as i32, None, None);

                    self.operand_stack.push(arg);
                }

                Opcode::StackSet => {
                    let var = read2!();
                    let val = self.operand_stack.pop().unwrap();
                    self.operand_stack[var as usize] = val;
                }

                Opcode::StackGet => {
                    let var = read2!();

                    let val = self.operand_stack[var as usize];
                    self.operand_stack.push(val);
                }

                Opcode::Define => {
                    let n = read2!();

                    index += 1;

                    let name = self.code.literals.vector_ref(n as usize);
                    let value = self.operand_stack.pop().unwrap();
                    let mut builder =
                        BasicBlockBuilder::new(self.procedure, self.current_block.block);

                    let thunk_addr = builder.const64(baseline_define as i64);
                    let name_constant = builder.const64(name.get_raw());

                    self.call_thunk_with_result(
                        baseline_define as *const u8,
                        &[name_constant, value],
                    );
                    self.operand_stack.push(name_constant);
                }

                Opcode::GlobalRef => {
                    let n = read2!();
                    let constant = self.code.literals.vector_ref(n as usize);

                    let value_offset = offset_of!(GLOC, value);

                    // GLOC is resolved, directly load the value from the GLOC
                    if constant.is_xtype(Type::GLOC) {
                        let mut builder =
                            BasicBlockBuilder::new(self.procedure, self.current_block.block);
                        let gloc = builder.const64(constant.get_raw());
                        let value =
                            builder.load(b3::Type::Int64, gloc, value_offset as i32, None, None);
                        self.operand_stack.push(value);
                    } else {
                        // GLOC was not resolved, this is *very* rare path, just invoke thunk that resolves the GLOC.
                        let mut builder =
                            BasicBlockBuilder::new(self.procedure, self.current_block.block);
                        let cb = builder.load(b3::Type::Int64, self.cfr, offset_of!(CallFrame, code_block) as _, None, None);
                        let ix = builder.const32(n as _);
                        let gloc =
                            self.call_thunk_with_result(baseline_resolve_global as _, &[cb, ix]);
                        let mut builder =
                            BasicBlockBuilder::new(self.procedure, self.current_block.block);
                        let value =
                            builder.load(b3::Type::Int64, gloc, value_offset as i32, None, None);
                        self.operand_stack.push(value);
                    }
                }

                Opcode::GlobalSet => {
                    let n = read2!();
                    let constant = self.code.literals.vector_ref(n as usize);

                    let value_offset = offset_of!(GLOC, value);

                    let value = self.operand_stack.pop().unwrap();

                    // GLOC is resolved, directly store the value to the GLOC
                    if constant.is_xtype(Type::GLOC) {
                        let mut builder =
                            BasicBlockBuilder::new(self.procedure, self.current_block.block);
                        let gloc = builder.const64(constant.get_raw());
                        builder.store(value, gloc, value_offset as i32, None, None);
                    } else {
                        // GLOC was not resolved, this is *very* rare path, just invoke thunk that resolves the GLOC.
                        let mut builder =
                            BasicBlockBuilder::new(self.procedure, self.current_block.block);
                        let cb = builder.const64(self.code.as_ptr() as _);
                        let ix = builder.const32(n as _);
                        let gloc =
                            self.call_thunk_with_result(baseline_resolve_global as _, &[cb, ix]);
                        let mut builder =
                            BasicBlockBuilder::new(self.procedure, self.current_block.block);
                        builder.store(value, gloc, value_offset as i32, None, None);
                    }
                }

                Opcode::ClosureRef => {
                    let n = read2!();
                    let offset = offset_of!(crate::runtime::object::Procedure, captures)
                        + n as usize * size_of::<Value>();
                    let callee_offset = offset_of!(CallFrame, callee);
                    let cfr = self.cfr;

                    let mut builder =
                        BasicBlockBuilder::new(self.procedure, self.current_block.block);

                    let closure =
                        builder.load(b3::Type::Int64, cfr, callee_offset as i32, None, None);
                    let capture = builder.load(b3::Type::Int64, closure, offset as i32, None, None);
                    self.operand_stack.push(capture);
                }

                Opcode::ClosureRefUnbox => {
                    let n = read2!();
                    let offset = offset_of!(crate::runtime::object::Procedure, captures)
                        + n as usize * size_of::<Value>();
                    let callee_offset = offset_of!(CallFrame, callee);
                    let box_offset = offset_of!(crate::runtime::object::Box, value);
                    let cfr = self.cfr;

                    let mut builder =
                        BasicBlockBuilder::new(self.procedure, self.current_block.block);

                    let closure =
                        builder.load(b3::Type::Int64, cfr, callee_offset as i32, None, None);
                    let capture = builder.load(b3::Type::Int64, closure, offset as i32, None, None);
                    let capture =
                        builder.load(b3::Type::Int64, capture, box_offset as i32, None, None);
                    self.operand_stack.push(capture);
                }

                Opcode::BoxRef => {
                    let offset = offset_of!(crate::runtime::object::Box, value);
                    let operand = self.operand_stack.pop().unwrap();
                    let mut builder =
                        BasicBlockBuilder::new(self.procedure, self.current_block.block);

                    let val = builder.load(b3::Type::Int64, operand, offset as i32, None, None);
                    self.operand_stack.push(val);
                }
                Opcode::BoxSet => {
                    let offset = offset_of!(crate::runtime::object::Box, value);
                    let box_ = self.operand_stack.pop().unwrap();
                    let value = self.operand_stack.pop().unwrap();

                    self.write_barrier::<false>(box_);
                    let mut builder =
                        BasicBlockBuilder::new(self.procedure, self.current_block.block);
                    builder.store(value, box_, offset as i32, None, None);
                }

                Opcode::Box => {
                    let value = self.operand_stack.pop().unwrap();
                    let mut builder =
                        BasicBlockBuilder::new(self.procedure, self.current_block.block);
                    let thunk = builder.const64(baseline_box as _);
                    let value =
                        builder.ccall(b3::Type::Int64, thunk, &[value], b3::Effects::for_call());
                    self.operand_stack.push(value);
                }

                Opcode::StackBox => {
                    let off = read2!();
                    let value = self.operand_stack[off as usize];

                    let mut builder =
                        BasicBlockBuilder::new(self.procedure, self.current_block.block);
                    let thunk = builder.const64(baseline_box as _);
                    let value =
                        builder.ccall(b3::Type::Int64, thunk, &[value], b3::Effects::for_call());

                    self.operand_stack[off as usize] = value;
                }

                Opcode::Return => {
                    let value = self.operand_stack.pop().unwrap();
                    let mut builder =
                        BasicBlockBuilder::new(self.procedure, self.current_block.block);

                    let patchpoint = builder.patchpoint(b3::Type::Void);
                    builder
                        .procedure
                        .patchpoint_effects_mut(patchpoint)
                        .terminal = true;
                    builder
                        .procedure
                        .patchpoint_effects_mut(patchpoint)
                        .exit_sideways = true;

                    builder.procedure.stackmap_append(
                        patchpoint,
                        value,
                        ValueRep::new(ValueRepKind::WarmAny),
                    );
                    let exit = self.exit_label.clone();
                    builder.procedure.stackmap_set_generator(
                        patchpoint,
                        Rc::new(|jit, params| {
                            let value = params[0];
                            if value.is_stack() {
                                jit.load64(
                                    Address::new(
                                        TargetMacroAssembler::FRAME_POINTER_REGISTER,
                                        value.offset_from_fp() as _,
                                    ),
                                    RETURN_VALUE_GPR2,
                                );
                            } else if value.is_reg() {
                                jit.mov(value.get_reg().gpr(), RETURN_VALUE_GPR2);
                            } else if value.is_constant() {
                                jit.mov(value.value(), RETURN_VALUE_GPR2);
                            }

                            jit.mov(ScmResult::OK as i32, RETURN_VALUE_GPR);
                            //params.emit_epilogue(jit);
                            let succs = params.successor_labels();
                            let br = jit.jump();

                            jit.add_link_task(Box::new(move |link_buffer| {
                                let succ = *succs[0].clone().borrow();
                                let loc = link_buffer.location_of(succ) as *mut u8;
                                link_buffer.link_jump(br, loc);
                            }));
                        }),
                    );
                    builder
                        .procedure
                        .add_successor(builder.block, self.exit_block);
                    builder
                        .procedure
                        .block_mut(self.exit_block)
                        .predecessor_list_mut()
                        .push(builder.block);
                    self.end_of_basic_block = true;
                }

                Opcode::Call => {
                    let argc = read2!();
                    let callee = self.operand_stack.pop().unwrap();
                    let args = self
                        .operand_stack
                        .split_off(self.operand_stack.len() - argc as usize);
                    let pc = &self.code[index] as *const u8 as i64;
                    let mut builder =
                        BasicBlockBuilder::new(self.procedure, self.current_block.block);

                    let mut sp = builder.load(
                        b3::Type::Int64,
                        self.vm,
                        offset_of!(VMType, sp) as i32,
                        None,
                        None,
                    );
                    // self.print_val(sp);
                    let mut builder =
                        BasicBlockBuilder::new(self.procedure, self.current_block.block);
                    let off = builder.const64(size_of::<Value>() as i64);
                    for (i, &arg) in args.iter().enumerate() {
                        builder.store(arg, sp, -8, None, None);
                        sp = builder.binary(b3::Opcode::Sub, sp, off);
                    }

                    let off = builder.const64(size_of::<CallFrame>() as _);
                    let sp = builder.binary(b3::Opcode::Sub, sp, off);

                    let pc = builder.const64(pc);
                    builder.store(pc, sp, offset_of!(CallFrame, return_pc) as i32, None, None);
                    builder.store(
                        self.cfr,
                        sp,
                        offset_of!(CallFrame, caller) as i32,
                        None,
                        None,
                    );
                    builder.store(callee, sp, offset_of!(CallFrame, callee) as i32, None, None);
                    let argc = builder.const64(Value::encode_int32(argc as _).get_raw());
                    builder.store(argc, sp, offset_of!(CallFrame, argc) as i32, None, None);

                    let is_vm_procedure = builder.procedure.add_block(1.0);
                    let do_thunk = builder.procedure.add_block(1.0);
                    let merge = builder.procedure.add_block(1.0);

                    self.branch_if_xtype(
                        callee,
                        Type::Procedure,
                        is_vm_procedure,
                        do_thunk,
                        b3::Frequency::Normal,
                    );
                    self.current_block.block = is_vm_procedure;
                    let mut builder = BasicBlockBuilder::new(self.procedure, is_vm_procedure);

                    let code_block = builder.load(
                        b3::Type::Int64,
                        callee,
                        offset_of!(crate::runtime::object::Procedure, code) as _,
                        None,
                        None,
                    );
                    let mcode = builder.load(
                        b3::Type::Int64,
                        code_block,
                        offset_of!(CodeBlock, mcode) as _,
                        None,
                        None,
                    );
                    let zero = builder.const64(0);

                    let fast_call = builder.procedure.add_block(1.0);
                    let cmp = builder.binary(b3::Opcode::NotEqual, mcode, zero);

                    // invoking JITed bytecode procedure is just regular `call` instruction
                    builder.branch(cmp, fast_call, (do_thunk, b3::Frequency::Normal));
                    self.current_block.block = fast_call;
                    let mut builder = BasicBlockBuilder::new(self.procedure, fast_call);
                    builder.switch_to_block(fast_call);
                    builder.store(
                        code_block,
                        sp,
                        offset_of!(CallFrame, code_block) as i32,
                        None,
                        None,
                    );

                    let patchpoint = builder.patchpoint(b3::Type::Int64);
                    builder
                        .procedure
                        .patchpoint_effects_mut(patchpoint)
                        .exit_sideways = true;
                    builder
                        .procedure
                        .patchpoint_effects_mut(patchpoint)
                        .control_dependent = true;

                        builder
                        .procedure
                        .stackmap_append(patchpoint, self.vm, ValueRep::reg(Reg::new_gpr(ARGUMENT_GPR1)));
                    builder
                        .procedure
                        .stackmap_append(patchpoint, self.vm, ValueRep::reg(Reg::new_gpr(ARGUMENT_GPR0)));
                    builder
                        .procedure
                        .stackmap_append_some_register(patchpoint, mcode);

                    builder.procedure.patchpoint_set_result_constraints(
                        patchpoint,
                        ValueRep::from_reg(Reg::new_gpr(RETURN_VALUE_GPR2)),
                    );

                    let mut set = RegisterSetBuilder::callee_saved_registers();
                    set.add(Reg::new_gpr(RETURN_VALUE_GPR), b3::Width::W64);
                    let set = RegisterSetBuilder::from_regs(&set);
                    builder.procedure.stackmap_clobber(patchpoint, &set);
                    let exit = self.exit_label.clone();
                    builder.procedure.stackmap_set_generator(
                        patchpoint,
                        Rc::new(move |jit, params| {
                            let _out = params[0];
                            let sp = params[1].get_reg().gpr();
                            let vm = params[2].get_reg().gpr();
                            let mcode = params[3].get_reg().gpr();
                            jit.store64(sp, Address::new(vm, offset_of!(VMType, sp) as _));
                            jit.store64(
                                sp,
                                Address::new(vm, offset_of!(VMType, top_call_frame) as _),
                            );

                            jit.mov(vm, ARGUMENT_GPR0);
                            jit.mov(sp, ARGUMENT_GPR1);
                            jit.call_op(Some(mcode));

                            let br_exit = jit.branch_test32(
                                ResultCondition::NonZero,
                                RETURN_VALUE_GPR,
                                RETURN_VALUE_GPR,
                            );
                            let exit = exit.clone();
                            jit.add_late_link_task(Box::new(move |link_buffer| {
                                let loc = link_buffer.location_of(exit.get());
                                link_buffer.link_jump(br_exit, loc);
                            }));
                        }),
                    );

                    builder.store(
                        self.cfr,
                        self.vm,
                        offset_of!(VMType, top_call_frame) as _,
                        None,
                        None,
                    );
                    builder.store(self.cfr, self.vm, offset_of!(VMType, sp) as _, None, None);
                    let upsilon_fast = builder.upsilon(patchpoint, None);
                    builder.jump(Some(merge));

                    builder.switch_to_block(do_thunk);
                    self.current_block.block = do_thunk;
                    let mut builder = BasicBlockBuilder::new(self.procedure, do_thunk);

                    let null = builder.const64(0);
                    builder.store(null, sp, offset_of!(CallFrame, return_pc) as i32, None, None);
                    let old = self.cfr;
                    self.cfr = sp;
                    let res = self.call_thunk_with_result(baseline_call as _, &[self.vm]);
                    self.cfr = old;
                    let mut builder = BasicBlockBuilder::new(self.procedure, do_thunk);
                    let upsilon_slow = builder.upsilon(res, None);
                    builder.jump(Some(merge));

                    builder.switch_to_block(merge);
                    self.current_block.block = merge;
                    let mut builder = BasicBlockBuilder::new(self.procedure, merge);
                    let phi = builder.phi(b3::Type::Int64);
                    builder.procedure.upsilon_set_phi(upsilon_fast, phi);
                    builder.procedure.upsilon_set_phi(upsilon_slow, phi);

                    self.operand_stack.push(phi);
                }

                Opcode::TailCall => {
                    let argc = read2!();
                    let callee = self.operand_stack.pop().unwrap();
                    let args = self
                        .operand_stack
                        .split_off(self.operand_stack.len() - argc as usize);
                    let pc = &self.code[index] as *const u8 as i64;
                    let mut builder =
                        BasicBlockBuilder::new(self.procedure, self.current_block.block);

                    let caller = builder.load(
                        b3::Type::Int64,
                        self.cfr,
                        offset_of!(CallFrame, caller) as _,
                        None,
                        None,
                    );
                    let return_pc = builder.load(
                        b3::Type::Int64,
                        self.cfr,
                        offset_of!(CallFrame, return_pc) as _,
                        None,
                        None,
                    );

                    let cargc = builder.load(
                        b3::Type::Int32,
                        self.cfr,
                        offset_of!(CallFrame, argc) as _,
                        None,
                        None,
                    );
                    let cargc = builder.zext32(cargc);
                    let shift = builder.const64(8);
                    let cfr_size = builder.const64(size_of::<CallFrame>() as i64);
                    let offset = builder.binary(b3::Opcode::Mul, cargc, shift);
                    let offset = builder.binary(b3::Opcode::Add, offset, cfr_size);
                    let mut cursor = builder.binary(b3::Opcode::Add, self.cfr, offset);

                    let off = builder.const64(8);
                    for &arg in args.iter() {
                        cursor = builder.binary(b3::Opcode::Sub, cursor, off);
                        builder.store(arg, cursor, 0, None, None);
                    }

                    let off = builder.const64(size_of::<CallFrame>() as i64);
                    cursor = builder.binary(b3::Opcode::Sub, cursor, off);

                    builder.store(
                        return_pc,
                        cursor,
                        offset_of!(CallFrame, return_pc) as _,
                        None,
                        None,
                    );
                    builder.store(
                        caller,
                        cursor,
                        offset_of!(CallFrame, caller) as _,
                        None,
                        None,
                    );
                    let argc_ = builder.const64(Value::encode_int32(argc as _).get_raw());
                    builder.store(argc_, cursor, offset_of!(CallFrame, argc) as _, None, None);
                    builder.store(
                        callee,
                        cursor,
                        offset_of!(CallFrame, callee) as _,
                        None,
                        None,
                    );

                    builder.store(
                        cursor,
                        self.vm,
                        offset_of!(VMType, top_call_frame) as _,
                        None,
                        None,
                    );
                    builder.store(cursor, self.vm, offset_of!(VMType, sp) as _, None, None);

                    let is_vm_procedure = builder.procedure.add_block(1.0);
                    let ret_tail = builder.procedure.add_block(1.0);

                    self.branch_if_xtype(
                        callee,
                        Type::Procedure,
                        is_vm_procedure,
                        ret_tail,
                        b3::Frequency::Normal,
                    );
                    self.current_block.block = is_vm_procedure;
                    let mut builder = BasicBlockBuilder::new(self.procedure, is_vm_procedure);

                    let code_block = builder.load(
                        b3::Type::Int64,
                        callee,
                        offset_of!(crate::runtime::object::Procedure, code) as _,
                        None,
                        None,
                    );
                    let mcode = builder.load(
                        b3::Type::Int64,
                        code_block,
                        offset_of!(CodeBlock, mcode) as _,
                        None,
                        None,
                    );
                    let zero = builder.const64(0);

                    let fast_call = builder.procedure.add_block(1.0);
                    let cmp = builder.binary(b3::Opcode::NotEqual, mcode, zero);
                    // invoking JITed bytecode procedure is just regular `call` instruction
                    builder.branch(cmp, fast_call, (ret_tail, b3::Frequency::Normal));
                    self.current_block.block = fast_call;
                    let mut builder = BasicBlockBuilder::new(self.procedure, fast_call);
                    builder.switch_to_block(fast_call);
                    builder.store(
                        code_block,
                        cursor,
                        offset_of!(CallFrame, code_block) as i32,
                        None,
                        None,
                    );

                    let patchpoint = builder.patchpoint(b3::Type::Void);

                    builder.procedure.stackmap_append(
                        patchpoint,
                        cursor,
                        ValueRep::reg(Reg::new_gpr(ARGUMENT_GPR1)),
                    );
                    builder.procedure.stackmap_append(
                        patchpoint,
                        self.vm,
                        ValueRep::reg(Reg::new_gpr(ARGUMENT_GPR0)),
                    );
                    builder.procedure.stackmap_append(
                        patchpoint,
                        mcode,
                        ValueRep::reg(Reg::new_gpr(ARGUMENT_GPR2)),
                    );

                    let effects = builder.procedure.patchpoint_effects_mut(patchpoint);
                    effects.terminal = true;
                    effects.exit_sideways = true;
                    effects.control_dependent = true;

                    builder.procedure.stackmap_set_generator(
                        patchpoint,
                        Rc::new(|jit, params| {
                            let _cfr = ARGUMENT_GPR1;
                            let _vm = ARGUMENT_GPR0;
                            let mcode = ARGUMENT_GPR2;

                            params.emit_epilogue_without_return(jit);

                            jit.far_jump(mcode);
                        }),
                    );

                    builder.switch_to_block(ret_tail);
                    self.current_block.block = ret_tail;

                    let mut builder = BasicBlockBuilder::new(self.procedure, ret_tail);

                    let patchpoint = builder.patchpoint(b3::Type::Void);
                    let effects = builder.procedure.patchpoint_effects_mut(patchpoint);
                    effects.terminal = true;
                    effects.exit_sideways = true;
                    effects.control_dependent = true;
                    let exit = self.exit_label.clone();
                    builder.procedure.stackmap_set_generator(
                        patchpoint,
                        Rc::new(move |jit, params| {
                            jit.mov(ScmResult::TAIL as i32, RETURN_VALUE_GPR);
                            jit.mov(Value::encode_undefined_value().get_raw(), RETURN_VALUE_GPR2);

                            let succs = params.successor_labels();
                            let br = jit.jump();

                            jit.add_link_task(Box::new(move |link_buffer| {
                                let succ = *succs[0].clone().borrow();
                                let loc = link_buffer.location_of(succ) as *mut u8;
                                link_buffer.link_jump(br, loc);
                            }));
                        }),
                    );

                    builder
                        .procedure
                        .add_successor(builder.block, self.exit_block);
                    builder
                        .procedure
                        .block_mut(self.exit_block)
                        .predecessor_list_mut()
                        .push(builder.block);
                }

                Opcode::NoOp3 => {
                    let _ = read2!();
                }
                _ => (),
            }

            if !self.end_of_basic_block && index == self.runoff {
                self.end_of_basic_block = true;
                self.fallthrough = true;
            }

            if index >= self.code.code_len as usize {
                self.end_of_basic_block = true;
                self.fallthrough = false;
            }

            if self.end_of_basic_block {
                if self.fallthrough {
                    let (block, phis) = self.get_or_create_block(index as _).clone();

                    for (i, phi) in phis.iter().copied().enumerate() {
                        let val = self.operand_stack[i];
                        BasicBlockBuilder::new(self.procedure, self.current_block.block)
                            .upsilon(val, Some(phi));
                    }

                    BasicBlockBuilder::new(self.procedure, self.current_block.block)
                        .jump(Some(block));
                }

                return;
            }
        }
    }

    pub fn is_pointer(&mut self, val: ValueId) -> ValueId {
        let mut builder = BasicBlockBuilder::new(self.procedure, self.current_block.block);
        let not_cell_mask = builder.const64(Value::NOT_CELL_MASK as _);
        let val = builder.binary(b3::Opcode::BitAnd, val, not_cell_mask);
        let zero = builder.const64(0);
        builder.binary(b3::Opcode::Equal, val, zero)
    }

    pub fn branch_if_xtype(
        &mut self,
        val: ValueId,
        xtype: Type,
        ok: BlockId,
        err: BlockId,
        frequency: b3::Frequency,
    ) {
        //self.print_val(val);
        let is_ptr = self.is_pointer(val);
        let mut builder = BasicBlockBuilder::new(self.procedure, self.current_block.block);
        let check_xtype = builder.procedure.add_block(1.0);
        builder.branch(is_ptr, check_xtype, (err, frequency));
        builder.switch_to_block(check_xtype);
        let tag = builder.load8z(val, offset_of!(ObjectHeader, typ) as i32, None, None);
        let xtype = builder.const32(xtype as _);
        let cmp = builder.binary(b3::Opcode::Equal, tag, xtype);
        builder.branch(cmp, ok, (err, frequency));
    }

    pub fn branch_if_int32(
        &mut self,
        val: ValueId,
        ok: BlockId,
        err: BlockId,
        frequency: b3::Frequency,
    ) {
        let mut builder = BasicBlockBuilder::new(self.procedure, self.current_block.block);

        let ntag = builder.const64(Value::NUMBER_TAG);
        let val = builder.binary(b3::Opcode::BitAnd, val, ntag);

        let eq = builder.binary(b3::Opcode::Equal, val, ntag);
        builder.branch(eq, ok, (err, frequency));
    }

    pub fn write_barrier<const CHECK_OBJ: bool>(&mut self, val: ValueId) {
        let cm_in_progress_off = Thread::cm_in_progress_offset();
        let thread_offset = offset_of!(VMType, thread);

        let mut builder = BasicBlockBuilder::new(self.procedure, self.current_block.block);
        let thread = builder.load(b3::Type::Int64, self.vm, thread_offset as i32, None, None);
        let cm_in_progress = builder.load8s(thread, cm_in_progress_off as i32, None, None);
        let zero = builder.const32(0);
        let cm_in_progress = builder.binary(b3::Opcode::NotEqual, cm_in_progress, zero);
        let do_wb = builder.procedure.add_block(1.0);
        let wb_done = builder.procedure.add_block(1.0);

        builder.branch(cm_in_progress, wb_done, (do_wb, b3::Frequency::Rare));

        self.current_block.block = do_wb;
        if CHECK_OBJ {
            let is_pointer = self.is_pointer(val);

            let mut builder = BasicBlockBuilder::new(self.procedure, self.current_block.block);

            let if_true = builder.procedure.add_block(1.0);

            builder.branch(is_pointer, if_true, (wb_done, b3::Frequency::Normal));

            builder.switch_to_block(if_true);

            let thunk = builder.const64(baseline_write_barrier as _);
            builder.ccall(
                b3::Type::Void,
                thunk,
                &[self.vm, val],
                b3::Effects::for_call(),
            );

            builder.jump(Some(wb_done));
        } else {
            let mut builder = BasicBlockBuilder::new(self.procedure, self.current_block.block);
            let thunk = builder.const64(baseline_write_barrier as _);
            builder.ccall(
                b3::Type::Void,
                thunk,
                &[self.vm, val],
                b3::Effects::for_call(),
            );
            builder.jump(Some(wb_done));
        }

        self.current_block.block = wb_done;
    }

    pub fn call_thunk_with_result(&mut self, addr: *const u8, args: &[b3::ValueId]) -> b3::ValueId {
        let mut builder = BasicBlockBuilder::new(self.procedure, self.current_block.block);
        builder.store(
            self.cfr,
            self.vm,
            offset_of!(VMType, top_call_frame) as i32,
            None,
            None,
        );
        builder.store(self.cfr, self.vm, offset_of!(VMType, sp) as i32, None, None);

        #[cfg(windows)]
        {
            todo!()
        }

        #[cfg(not(windows))]
        {
            let exit = self.exit_label.clone();
            let mut builder = BasicBlockBuilder::new(self.procedure, self.current_block.block);

            let patchpoint = builder.patchpoint(b3::Type::Int64);

            *builder.procedure.patchpoint_effects_mut(patchpoint) = b3::Effects::for_call();

            let argrep = match args {
                [_] => vec![ValueRep::reg(Reg::new_gpr(ARGUMENT_GPR0))],
                [_, _] => vec![
                    ValueRep::reg(Reg::new_gpr(ARGUMENT_GPR0)),
                    ValueRep::reg(Reg::new_gpr(ARGUMENT_GPR1)),
                ],

                [_, _, _] => vec![
                    ValueRep::reg(Reg::new_gpr(ARGUMENT_GPR0)),
                    ValueRep::reg(Reg::new_gpr(ARGUMENT_GPR1)),
                    ValueRep::reg(Reg::new_gpr(ARGUMENT_GPR2)),
                ],

                _ => todo!(),
            };

            for (i, arg) in args.iter().enumerate() {
                builder
                    .procedure
                    .stackmap_append(patchpoint, *arg, argrep[i]);
            }

            let mut set = RegisterSetBuilder::callee_saved_registers();
            set.add(Reg::new_gpr(RETURN_VALUE_GPR), b3::Width::W64);
            let set = RegisterSetBuilder::from_regs(&set);
            builder.procedure.stackmap_clobber(patchpoint, &set);
            builder.procedure.patchpoint_set_result_constraints(
                patchpoint,
                ValueRep::reg(Reg::new_gpr(RETURN_VALUE_GPR2)),
            );

            builder.procedure.stackmap_set_generator(
                patchpoint,
                Rc::new(move |jit, _params| {
                    // Arguments are in correct registers, return value tag is in RETURN_VALUE_GPR
                    // return value itself is in RETURN_VALUE_GPR2
                    jit.call_op(Some(AbsoluteAddress::new(addr)));
                    let br_exit =
                        jit.branch_test32(ResultCondition::NonZero, RETURN_VALUE_GPR, 0i32);
                    let exit = exit.clone();
                    jit.add_late_link_task(Box::new(move |link_buffer| {
                        let loc = link_buffer.location_of(exit.get());
                        link_buffer.link_jump(br_exit, loc);
                    }));
                }),
            );

            patchpoint
        }
    }
}
