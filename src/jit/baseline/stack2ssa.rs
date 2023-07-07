use std::{
    collections::BTreeMap,
    mem::{offset_of, size_of}, rc::Rc,
};

use b3::{
    jit::register_set::RegisterSetBuilder, variable::VariableId, BasicBlockBuilder, BlockId,
    Procedure, Reg, ValueId, ValueRep, ValueRepKind,
};
use macroassembler::{jit::gpr_info::{
    ARGUMENT_GPR0, ARGUMENT_GPR1, ARGUMENT_GPR2, RETURN_VALUE_GPR, RETURN_VALUE_GPR2,
}, assembler::{abstract_macro_assembler::AbsoluteAddress, RelationalCondition}};
use rsgc::prelude::Handle;

use crate::{
    jit::baseline::thunks::baseline_define,
    op::Opcode,
    runtime::{
        object::{CodeBlock, GLOC},
        value::Value,
    },
    vm::callframe::CallFrame,
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
    procedure: &'a mut Procedure,
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
    cfr: ValueId,
    #[cfg(windows)]
    result_reg: ValueId,
}

impl<'a> Stack2SSA<'a> {
    pub fn new(
        procedure: &'a mut Procedure,
        code_block: Handle<CodeBlock>,
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
            ungenerated: vec![],
            blocks: BTreeMap::new(),
            end_of_basic_block: false,
            fallthrough: false,
            runoff: usize::MAX,
            cfr,
        }
    }

    pub fn generate(&mut self) {
        self.generate_from(0);

        while let Some(mut block) = self.ungenerated.pop() {
            self.operand_stack.clear();
            self.operand_stack = std::mem::take(&mut block.stack);

            self.current_block = block;

            self.generate_from(self.current_block.low);
        }
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

            match op {
                Opcode::Alloc => {
                    let n = read2!();

                    for _ in 0..n {
                        let var = self.procedure.add_variable(b3::Type::Int64);
                        self.locals.push(var);
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

                Opcode::AssertArgCount => {}
                Opcode::AssertMinArgCount => {}
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

                    let arg_offset = offset_of!(CallFrame, args) + n as usize * size_of::<Value>();

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

                Opcode::Return => {
                    let val = self.operand_stack.pop().unwrap();
                    let mut builder =
                        BasicBlockBuilder::new(self.procedure, self.current_block.block);
                    builder.return_(Some(val));
                    self.end_of_basic_block = true;
                }

                Opcode::Define => {
                    let n = read2!();
                    let _constant = self.code.code()[index as usize];
                    index += 1;

                    let name = self.code.literals.vector_ref(n as usize);
                    let value = self.operand_stack.pop().unwrap();
                    let mut builder =
                        BasicBlockBuilder::new(self.procedure, self.current_block.block);

                    let thunk_addr = builder.const64(baseline_define as i64);
                    let name_constant = builder.const64(name.get_raw());

                    //let ccall = builder.ccall(thunk_addr, &[name_constant, value]);
                }
                _ => (),
            }

            if !self.end_of_basic_block && index == self.runoff {
                self.end_of_basic_block = true;
                self.fallthrough = true;
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

    pub fn call_thunk_with_result(
        &mut self,
        addr: *const u8,
        args: &[b3::ValueId],
    ) -> b3::ValueId {
        #[cfg(windows)]
        {
            todo!()
        }

        #[cfg(not(windows))]
        {
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

            builder.procedure.stackmap_set_generator(patchpoint, Rc::new(move |jit, _params| {
                // Arguments are in correct registers, return value tag is in RETURN_VALUE_GPR
                // return value itself is in RETURN_VALUE_GPR2
                jit.call_op(Some(AbsoluteAddress::new(addr)));
                let br_ok = jit.branch32(RelationalCondition::NotEqual, RETURN_VALUE_GPR, 2i32);
                jit.ret();
                br_ok.link(jit);
            }));
        }

        todo!()
    }
}
