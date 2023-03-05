use std::collections::hash_map::RandomState;

use crate::compiler::env::environment_get_cell;
use crate::compiler::get_arity_from_lambda;
use crate::compiler::lambda_lifting::LiftedLambda;
use crate::jit::inline::try_inline_application;
use crate::value::Boxed;
use crate::value::NativeProcedure;
use crate::value::Pair;
use crate::value::Value as ScmValue;
use crate::value::Vector;
use crate::value::SCHEME_MAX_ARGS;
use crate::vm::Vm;
use cranelift::codegen;
use cranelift::prelude::*;
use cranelift_frontend::FunctionBuilderContext;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::Linkage;
use cranelift_module::{DataContext, FuncId, Module};
use rsgc::prelude::Handle;
use rsgc::prelude::Object;
use rsgc::system::arraylist::ArrayList;
use rsgc::system::collections::hashmap::HashMap;
use rsgc::thread::Thread;

use super::thunks::make_closure;
use super::thunks;


#[allow(dead_code)]
pub struct JIT {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    module: JITModule,
    data_ctx: DataContext,
    functions: Functions,
}

impl Default for JIT {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        flag_builder.set("opt_level", "speed").unwrap();
        flag_builder.set("enable_alias_analysis", "true").unwrap();
        
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        builder.symbols(
            [
                ("listify", thunks::listify as *const u8),
                ("make_wrong_arity", thunks::make_wrong_arity as *const u8),
                ("box", thunks::make_boxed as *const u8),
                ("grow_tail_rands", thunks::grow_tail_rands as *const u8),
                ("make_closure", thunks::make_closure as *const u8),
                ("undefined_global", thunks::undefined_global as *const u8),
                (
                    "flush_ssb_and_do_wb",
                    thunks::flush_ssb_and_do_wb as *const u8,
                ),
            ]
            .into_iter(),
        );
        let mut module = JITModule::new(builder);

        let listify = {
            let mut sig = module.make_signature();
            sig.params.extend_from_slice(&[
                AbiParam::new(types::I64),
                AbiParam::new(types::I64),
                AbiParam::new(types::I64),
            ]);
            sig.returns.push(AbiParam::new(types::I64));
            module
                .declare_function("listify", Linkage::Import, &sig)
                .unwrap()
        };

        let make_wrong_arity = {
            let mut sig = module.make_signature();
            sig.params.extend_from_slice(&[
                AbiParam::new(types::I64),
                AbiParam::new(types::I64),
                AbiParam::new(types::I64),
                AbiParam::new(types::I64),
                AbiParam::new(types::I64),
                AbiParam::new(types::I64),
            ]);
            sig.returns.push(AbiParam::new(types::I64));
            module
                .declare_function("make_wrong_arity", Linkage::Import, &sig)
                .unwrap()
        };

        let make_boxed = {
            let mut sig = module.make_signature();
            sig.params
                .extend_from_slice(&[AbiParam::new(types::I64), AbiParam::new(types::I64)]);
            sig.returns.push(AbiParam::new(types::I64));
            module
                .declare_function("box", Linkage::Import, &sig)
                .unwrap()
        };

        let grow_tail_rands = {
            let mut sig = module.make_signature();
            sig.params
                .extend_from_slice(&[AbiParam::new(types::I64), AbiParam::new(types::I64)]);
            module
                .declare_function("grow_tail_rands", Linkage::Import, &sig)
                .unwrap()
        };

        let make_closure = {
            let mut sig = module.make_signature();
            sig.params.extend_from_slice(&[
                AbiParam::new(types::I64),
                AbiParam::new(types::I64),
                AbiParam::new(types::I64),
                AbiParam::new(types::I64),
                AbiParam::new(types::I64),
            ]);
            sig.returns.push(AbiParam::new(types::I64));
            module
                .declare_function("make_closure", Linkage::Import, &sig)
                .unwrap()
        };

        let undefined_global = {
            let mut sig = module.make_signature();
            sig.params
                .extend_from_slice(&[AbiParam::new(types::I64), AbiParam::new(types::I64)]);
            sig.returns.push(AbiParam::new(types::I64));
            module
                .declare_function("undefined_global", Linkage::Import, &sig)
                .unwrap()
        };

        let flush_ssb_and_do_wb = {
            let mut sig = module.make_signature();
            sig.params
                .extend_from_slice(&[AbiParam::new(types::I64), AbiParam::new(types::I64)]);

            module
                .declare_function("flush_ssb_and_do_wb", Linkage::Import, &sig)
                .unwrap()
        };

        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
            functions: Functions {
                list: listify,
                wrong_arity: make_wrong_arity,
                r#box: make_boxed,
                grow_tail_rands,
                make_closure,
                undefined_global,
                flush_ssb_and_do_wb,
                lifted: HashMap::with_hasher(RandomState::new()),
            },
        }
    }
}

fn make_toplevel_lambda(toplevel: ScmValue) -> LiftedLambda {
    /*let arity = ScmValue::make_cons(intern("="), ScmValue::make_int(1));
    let bound = ScmValue::make_cons(
        ScmValue::make_cons(intern("#%toplevel-cont"), ScmValue::make_int(0)),
        ScmValue::make_null(),
    );

    LiftedLambda {
        id: i32::MIN,
        arity,
        bound,
        free: ScmValue::make_null(),
        body: ScmValue::make_cons(body, ScmValue::make_null()),
    }*/

    let arity = toplevel.cadr();
    let bound = toplevel.caddr();
    let free = toplevel.cadddr();
    let body = toplevel.cdddr().cdr();

    LiftedLambda {
        id: i32::MIN,
        arity,
        bound,
        free,
        body,
    }
}

impl JIT {
    pub fn compile(
        &mut self,
        env: ScmValue,
        mut defs: ScmValue,
        lifted: Handle<HashMap<i32, LiftedLambda>>,
        toplevel: ScmValue,
    ) -> ScmValue {
        self.functions.lifted.clear();

        let scm_signature = {
            let mut sig = self.module.make_signature();

            sig.params.extend_from_slice(&[
                AbiParam::new(types::I64), // vm
                AbiParam::new(types::I64), // env
                AbiParam::new(types::I64), // argv
                AbiParam::new(types::I64), // argc
                AbiParam::new(types::I64), // tag
            ]);

            sig.returns.push(AbiParam::new(types::I64));

            sig
        };

        for (id, lambda) in lifted.iter() {
            let fid = self
                .module
                .declare_anonymous_function(&scm_signature)
                .unwrap();
            self.functions.lifted.put(
                Thread::current(),
                *id,
                LiftedEntry {
                    lambda: lambda.clone(),
                    id: fid,
                },
            );
        }

        let toplevel_id = self
            .module
            .declare_anonymous_function(&scm_signature)
            .unwrap();
        let toplevel_lambda = make_toplevel_lambda(toplevel);
        
        self.functions.lifted.put(
            Thread::current(),
            i32::MIN,
            LiftedEntry {
                lambda: toplevel_lambda,
                id: toplevel_id,
            },
        );
        // initialize definitions
        while !defs.nullp() {
            environment_get_cell(env, defs.car());
            defs = defs.cdr();
        }

        self.translate(env);

        unsafe {
            let code = self.module.get_finalized_function(toplevel_id);
            make_closure(crate::vm::vm(), std::mem::transmute(code), 0, 1, 1)
        }
    }

    fn translate(&mut self, env: ScmValue) {
        let ids = self
            .functions
            .lifted
            .iter()
            .map(|(id, _)| *id)
            .collect::<Vec<_>>();
        let scm_signature = {
            let mut sig = self.module.make_signature();

            sig.params.extend_from_slice(&[
                AbiParam::new(types::I64), // vm
                AbiParam::new(types::I64), // env
                AbiParam::new(types::I64), // argv
                AbiParam::new(types::I64), // argc
                AbiParam::new(types::I64), // tag
            ]);

            sig.returns.push(AbiParam::new(types::I64));

            sig
        };

        let mut compiled = vec![];
        for id in ids.iter().copied() {
            let entry = *self.functions.lifted.get(&id).unwrap();
            {
                self.ctx.func.signature = scm_signature.clone();
                let mut translator = FunctionTranslator::new(
                    self,
                    entry.lambda.arity,
                    entry.lambda.bound,
                    entry.lambda.body,
                    env,
                );

                translator.generate_prelude();

                translator.translate();
                
                translator.builder.finalize();
            }

            self.ctx.preopt(self.module.isa()).unwrap();

            let data = self.module
                .define_function(entry.id, &mut self.ctx)
                .unwrap();
            compiled.push((data.size, entry.id));
            self.module.clear_context(&mut self.ctx);
        }

        self.module.finalize_definitions().unwrap();
        /* 
        for (size, id) in compiled {
            let code = self.module.get_finalized_function(id);
            unsafe {
                println!("function {}:", id);
                let code = std::slice::from_raw_parts(code, size as usize);
                disassemble(code);
            }
        }*/
    }
}

#[derive(Clone, Copy)]
pub struct LiftedEntry {
    pub lambda: LiftedLambda,
    pub id: FuncId,
}

impl Object for LiftedEntry {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.lambda.trace(visitor);
    }
}

pub struct Functions {
    pub list: FuncId,
    pub wrong_arity: FuncId,
    pub r#box: FuncId,
    pub grow_tail_rands: FuncId,
    pub make_closure: FuncId,
    pub undefined_global: FuncId,
    pub flush_ssb_and_do_wb: FuncId,
    pub lifted: Handle<HashMap<i32, LiftedEntry>>,
}

pub struct FunctionTranslator<'a> {
    pub builder: FunctionBuilder<'a>,
    pub module: &'a mut JITModule,
    pub functions: &'a Functions,
    pub arity: ScmValue,
    pub bound: ScmValue,
    pub body: ScmValue,

    pub vm: Value,
    pub env: Value,
    pub cont: Value,
    pub argv: Value,
    pub argc: Value,
    pub tag: Value,
    pub slow_paths: Vec<Box<dyn FnOnce(&mut Self)>>,
    pub genv: ScmValue,
}

impl<'a> FunctionTranslator<'a> {

    pub fn heap_check(&mut self, val: Value, tag: crate::value::Type) -> Value {
        let is_smallint = self.check_smallint(val);
        let merge = self.builder.create_block();

        self.builder.append_block_param(merge, types::I64);

        let slow = self.builder.create_block();
        let false_ = self.builder.ins().iconst(types::I64, ScmValue::make_false().raw() as i64);
        self.builder.ins().brif(is_smallint, merge, &[false_], slow, &[]);
        
        {
            self.builder.switch_to_block(slow);
            let vtag = self.builder.ins().load(types::I64, MemFlags::new(), val, 0);
            let is_eq = self.builder.ins().icmp_imm(IntCC::Equal, vtag, tag as i64);
            let true_ = self.builder.ins().iconst(types::I64, ScmValue::make_true().raw() as i64);
            let res = self.builder.ins().select(is_eq, true_, false_);
            self.builder.ins().jump(merge, &[res]);
        }
        self.builder.switch_to_block(merge);
        self.builder.seal_block(merge);
        self.builder.block_params(merge)[0]
    }
    /// Is `val` in the range of `start..=end` types?
    pub fn heap_check_in_range(&mut self, val: Value, start: crate::value::Type, end: crate::value::Type) -> Value {
        let is_smallint = self.check_smallint(val);
        let merge = self.builder.create_block();

        self.builder.append_block_param(merge, types::I64);

        let slow = self.builder.create_block();
        let false_ = self.builder.ins().iconst(types::I64, ScmValue::make_false().raw() as i64);
        self.builder.ins().brif(is_smallint, merge, &[false_], slow, &[]);
        
        {
            self.builder.switch_to_block(slow);
            let vtag = self.builder.ins().load(types::I64, MemFlags::new(), val, 0);
            let is_greater = self.builder.ins().icmp_imm(IntCC::UnsignedGreaterThan, vtag, start as i64);
            let is_less = self.builder.ins().icmp_imm(IntCC::UnsignedLessThanOrEqual, vtag, end as i64);
            let is_in_range = self.builder.ins().band(is_greater, is_less);
            let true_ = self.builder.ins().iconst(types::I64, ScmValue::make_true().raw() as i64);
            let res = self.builder.ins().select(is_in_range, true_, false_);
            self.builder.ins().jump(merge, &[res]);
        }
        self.builder.switch_to_block(merge);
        self.builder.seal_block(merge);
        self.builder.block_params(merge)[0]
    }

    pub fn add_slow_path(&mut self, f: impl FnOnce(&mut Self) + 'static) {
        self.slow_paths.push(Box::new(f));
    }

    pub fn check_smallint(&mut self, val: Value) -> Value {
        let is_smallint = self.builder.ins().band_imm(val, 1);
        let is_smallint = self.builder.ins().icmp_imm(IntCC::NotEqual, is_smallint, 0);
        is_smallint
    }

    pub fn unbox_smallint(&mut self, val: Value) -> Value {
        let val = self.builder.ins().ushr_imm(val, 1);
        self.builder.ins().ireduce(types::I32, val)
    }

    pub fn make_smallint(&mut self, val: Value) -> Value {
        let val = self.builder.ins().uextend(types::I64, val);
        let shifted = self.builder.ins().ishl_imm(val, 1);
        self.builder.ins().bor_imm(shifted, 1)
    }

    pub fn new(
        jit: &'a mut JIT,
        arity: ScmValue,
        bound: ScmValue,
        body: ScmValue,
        genv: ScmValue,
    ) -> Self {
        let mut builder = FunctionBuilder::new(&mut jit.ctx.func, &mut jit.builder_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);

        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        let vm = builder.block_params(entry_block)[0];
        let env = builder.block_params(entry_block)[1];
        let argv = builder.block_params(entry_block)[2];
        let argc = builder.block_params(entry_block)[3];
        let tag = builder.block_params(entry_block)[4];
        let cont = builder.ins().load(types::I64, MemFlags::trusted(), argv, 0);

        Self {
            builder,
            module: &mut jit.module,
            functions: &jit.functions,
            arity,
            bound,
            body,
            vm,
            env,
            cont,
            argv,
            argc,
            tag,
            slow_paths: Vec::with_capacity(4),
            genv,
        }
    }

    pub fn generate_prelude(&mut self) {
        let check = self.arity.car().strsym();
        let n = self.arity.cdr().int();
        let cc = if check == "=" {
            IntCC::Equal
        } else {
            IntCC::UnsignedGreaterThanOrEqual
        };
    
        let test = self.builder.ins().icmp_imm(cc, self.argc, n as i64);

        let check_ok = self.builder.create_block();
        let check_fail = self.builder.create_block();
        self.builder.set_cold_block(check_fail);
        self.builder
            .ins()
            .brif(test, check_ok, &[], check_fail, &[]);

        self.slow_paths.push(Box::new(move |this| {
            // Invoke function that allocates `exn:fail:contract:arity` and then
            // returns error tag and the exception produced by the function.
            this.builder.switch_to_block(check_fail);
            let wrong_arity = this.functions.wrong_arity;
            let wrong_arity = this
                .module
                .declare_func_in_func(wrong_arity, &mut this.builder.func);
            let mina = this.builder.ins().iconst(types::I64, n as i64);

            let maxa = if check == ">=" {
                this.builder
                    .ins()
                    .iconst(types::I64, SCHEME_MAX_ARGS as i64)
            } else {
                this.builder.ins().iconst(types::I64, n as i64)
            };

            let body = this.builder.ins().iconst(types::I64, this.body.raw() as i64);
            let val = this
                .builder
                .ins()
                .call(wrong_arity, &[this.vm, this.argc, maxa, mina, this.argv, body]);
            let ret = this.builder.inst_results(val)[0];
            /* set error return tag */
            {
                let tag = this.builder.ins().iconst(types::I8, 1);
                this.builder
                    .ins()
                    .store(MemFlags::trusted(), tag, this.tag, 0);
            }
            this.builder.ins().return_(&[ret]);
        }));

        self.builder.switch_to_block(check_ok);
        self.builder.seal_block(check_ok);
        self.load_arguments(check == ">=", n as _);
    }

    fn load_arguments(&mut self, rest: bool, _n: usize) {
        let mut bound = self.bound;

        // continuation is not passed in the `argv` variable, special case.
        /*let ix = Variable::from_u32(0);
        self.builder.declare_var(ix, types::I64);
        self.builder.def_var(ix, self.cont);*/

        let mut i = 0;
        let mut j = 0;
        while !bound.nullp() {
            if rest && bound.cdr().nullp() {
                // Convert the rest of argument to a list.
                let l = self.functions.list;
                let l = self.module.declare_func_in_func(l, &mut self.builder.func);

                let rest_argv = self.builder.ins().iadd_imm(self.argv, i as i64 * 8);
                let tmp = self.builder.ins().iconst(types::I64, i as i64);
                let rest_argc = self.builder.ins().isub(self.argc, tmp);

                let list = self.builder.ins().call(l, &[self.vm, rest_argv, rest_argc]);
                let ret = self.builder.inst_results(list)[0];
                let ix = Variable::from_u32(i as _);
                self.builder.declare_var(ix, types::I64);
                self.builder.def_var(ix, ret);
            } else {
                let arg = self.arg_load(j as i32);
                let ix = Variable::from_u32(i as _);
                self.builder.declare_var(ix, types::I64);
                self.builder.def_var(ix, arg);
            }
            i += 1;
            j += 1;
            bound = bound.cdr();
        }

        let next = self.builder.create_block();
        self.builder.ins().jump(next, &[]);
        self.builder.seal_block(next);
        self.builder.switch_to_block(next);
    }

    fn arg_load(&mut self, i: i32) -> Value {
        let offset = i * 8;
        self.builder
            .ins()
            .load(types::I64, MemFlags::trusted(), self.argv, offset)
    }

    /// Generate code for a tail call.
    ///
    /// - `rator` is the function to call.
    /// - `rands` is the list of arguments.
    ///
    /// # Steps
    ///
    /// - Check if the tail call buffer is large enough to hold the arguments.
    /// - If not, allocate a new buffer.
    /// - Copy the arguments to the buffer.
    /// - Set the `tail_rator` field of the VM to the function to call.
    /// - Set the `tail_rands.len` field of the VM to the number of arguments.
    /// - Set the `tag` pointer to 2, indicating that the VM should do a tail call.
    pub fn gen_tail_call(&mut self, rator: Value, rands: &[Value]) {
        let tail_rands = self.builder.ins().load(
            types::I64,
            MemFlags::trusted(),
            self.vm,
            offsetof!(Vm, tail_rands) as i32,
        );

        let cap = self.builder.ins().load(
            types::I32,
            MemFlags::trusted(),
            tail_rands,
            ArrayList::<ScmValue>::cap_offset() as i32,
        );

        let check = self
            .builder
            .ins()
            .icmp_imm(IntCC::UnsignedLessThan, cap, rands.len() as i64);
        let fail = self.builder.create_block();
        let ok = self.builder.create_block();
        self.builder.ins().brif(check, fail, &[], ok, &[]);
        self.builder.switch_to_block(ok);
        let tail_rands = self.builder.ins().load(
            types::I64,
            MemFlags::trusted(),
            self.vm,
            offsetof!(Vm, tail_rands) as i32,
        );
        let tail_rands_buf = self
            .builder
            .ins()
            .iadd_imm(tail_rands, ArrayList::<ScmValue>::data_offset() as i64);
        for (i, rand) in rands.iter().enumerate() {
            let offset = i as i32 * 8;
            self.builder
                .ins()
                .store(MemFlags::trusted(), *rand, tail_rands_buf, offset);
        }

        let argc = self.builder.ins().iconst(types::I32, rands.len() as i64);
        self.builder.ins().store(
            MemFlags::trusted(),
            argc,
            tail_rands,
            ArrayList::<ScmValue>::len_offset() as i32,
        );

        let tag = self.builder.ins().iconst(types::I8, 2);
        self.builder
            .ins()
            .store(MemFlags::trusted(), tag, self.tag, 0);

        self.builder.ins().store(
            MemFlags::trusted(),
            rator,
            self.vm,
            offsetof!(Vm, tail_rator) as i32,
        );
        self.builder.ins().return_(&[rator]);

        let to_grow = rands.len();
        self.builder.set_cold_block(fail);
        self.slow_paths.push(Box::new(move |this| {
            this.builder.switch_to_block(fail);
            // grow tail_rands to fit `rands.len()` elements and jump back to application.
            let grow = this.functions.grow_tail_rands;
            let grow = this
                .module
                .declare_func_in_func(grow, &mut this.builder.func);
            let n = this.builder.ins().iconst(types::I64, to_grow as i64);
            this.builder.ins().call(grow, &[this.vm, n]);
            this.builder.ins().jump(ok, &[]);
        }));
    }

    fn compile_sequence(&mut self, seq: ScmValue, tail: bool) -> Value {
        if seq.nullp() {
            return self
                .builder
                .ins()
                .iconst(types::I64, ScmValue::make_void().raw() as i64);
        } else if seq.cdr().nullp() {
            self.compile(seq.car(), tail)
        } else {
            self.compile(seq.car(), false);
            self.compile_sequence(seq.cdr(), tail)
        }
    }

    fn compile_app(&mut self, exp: ScmValue, tail: bool) -> Value {
        assert!(
            tail,
            "#%app must be in tail position after CPS transformation"
        );

        let mut cranelift_values = vec![];

        let mut args = exp.cdr();
        while !args.nullp() {
            cranelift_values.push(self.compile(args.car(), false));
            args = args.cdr();
        }

        

        if try_inline_application(self, exp.car(), &cranelift_values) {
            
            return Value::from_u32(0);
        } else {
            let rator = self.compile(exp.car(), false);
            self.gen_tail_call(rator, &cranelift_values);
            rator
        }
    }

    fn compile_if(&mut self, exp: ScmValue, tail: bool) -> Value {
        let test = self.compile(exp.car(), false);

        let check =
            self.builder
                .ins()
                .icmp_imm(IntCC::NotEqual, test, ScmValue::make_false().raw() as i64);

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let next = (!tail).then(|| {
            let b = self.builder.create_block();
            self.builder.append_block_param(b, types::I64);
            b
        });

        self.builder
            .ins()
            .brif(check, then_block, &[], else_block, &[]);
        self.builder.switch_to_block(then_block);
        let then = self.compile(exp.cadr(), tail);

        if let Some(next) = next {
            self.builder.ins().jump(next, &[then]);
        }

        self.builder.switch_to_block(else_block);
        let else_ = self.compile(exp.caddr(), tail);
        if let Some(next) = next {
            self.builder.ins().jump(next, &[else_]);
        }

        if let Some(next) = next {
            self.builder.switch_to_block(next);
            self.builder.block_params(next)[0]
        } else {
            then
        }
    }

    fn compile_lambda(&mut self, exp: ScmValue, _tail: bool) -> Value {
        let id = exp.cadr().int();

        let entry = self.functions.lifted.get(&id).unwrap();

        let lam = entry.lambda;
        let id = entry.id;

        let (mina, maxa) = get_arity_from_lambda(lam.arity);

        let mina = self.builder.ins().iconst(types::I64, mina as i64);

        let maxa = self.builder.ins().iconst(types::I64, maxa as i64);

        let mut free = lam.free;

        let mut free_vars = vec![];

        while !free.nullp() {
            free_vars.push(self.compile(free.car(), false));
            free = free.cdr();
        }

        let lambda_ref = self.module.declare_func_in_func(id, &mut self.builder.func);
        let lambda_ref = self.builder.ins().func_addr(types::I64, lambda_ref);

        let make_closure = self
            .module
            .declare_func_in_func(self.functions.make_closure, &mut self.builder.func);

        let n = self
            .builder
            .ins()
            .iconst(types::I64, free_vars.len() as i64);

        let closure = self
            .builder
            .ins()
            .call(make_closure, &[self.vm, lambda_ref, n, mina, maxa]);

        let closure = self.builder.inst_results(closure)[0];

        let captures = self.builder.ins().load(
            types::I64,
            MemFlags::trusted(),
            closure,
            offsetof!(NativeProcedure, captures) as i32,
        );

        self.generate_write_barrier(captures);
        for (i, free_var) in free_vars.into_iter().enumerate() {
            let offset = i as i32 * 8 + Vector::data_offset() as i32;
            self.builder
                .ins()
                .store(MemFlags::trusted(), free_var, captures, offset);
        }

        closure
    }


    pub fn generate_global_reference(&mut self, var: ScmValue) -> Value {
        let cell = environment_get_cell(self.genv, var);

        let cell = self.builder.ins().iconst(types::I64, cell.raw() as i64);

        let val = self.builder.ins().load(
            types::I64,
            MemFlags::trusted(),
            cell,
            offsetof!(Pair, cdr) as i32,
        );

        let check_not_void =
            self.builder
                .ins()
                .icmp_imm(IntCC::NotEqual, val, ScmValue::make_void().raw() as i64);

        let ok = self.builder.create_block();
        let fail = self.builder.create_block();

        self.builder.append_block_param(ok, types::I64);
        self.builder.append_block_param(fail, types::I64);
        self.builder.set_cold_block(fail);
        self.builder
            .ins()
            .brif(check_not_void, ok, &[val], fail, &[cell]);
        self.builder.switch_to_block(ok);

        self.slow_paths.push(Box::new(move |this| {
            // create `exn:fail` and return it.
            this.builder.switch_to_block(fail);
            let cell = this.builder.block_params(fail)[0];
            let undef_global = this
                .module
                .declare_func_in_func(this.functions.undefined_global, &mut this.builder.func);
            let val = this.builder.ins().call(undef_global, &[this.vm, cell]);
            let exn = this.builder.inst_results(val)[0];

            let tag = this.builder.ins().iconst(types::I8, 1);
            this.builder
                .ins()
                .store(MemFlags::trusted(), tag, this.tag, 0);
            this.builder.ins().return_(&[exn]);
        }));

        val
    }

    pub fn generate_global_set(&mut self, var: ScmValue, val: Value) -> Value {
        let cell = environment_get_cell(self.genv, var);
        let cell = self.builder.ins().iconst(types::I64, cell.raw() as i64);
        self.generate_write_barrier(cell);
        self.builder
            .ins()
            .store(MemFlags::trusted(), val, cell, offsetof!(Pair, cdr) as i32);
        val
    }

    pub fn generate_write_barrier(&mut self, val: Value) {
        let check_is_int = self.builder.create_block();
        let check_heap_allocated = self.builder.create_block();
        let write_barrier_check = self.builder.create_block();
        let write_barrier_fast = self.builder.create_block();
        let done = self.builder.create_block();
        let write_barrier_slow = self.builder.create_block();
        let mutator_ref = self.builder.ins().load(
            types::I64,
            MemFlags::trusted(),
            self.vm,
            offsetof!(Vm, mutator) as i32,
        );
        self.builder.set_cold_block(write_barrier_slow);
        self.builder
            .append_block_param(write_barrier_fast, types::I64);
        self.builder
            .append_block_param(write_barrier_slow, types::I64);

        {
            // 0) Check if concurrent mark is in progress
            //     Write barrier can work without this check, but this check just removes unnecessary
            //     write barrier writes and makes the code faster.
            //
            let cm_in_progress = self.builder.ins().load(
                types::I8,
                MemFlags::trusted(),
                mutator_ref,
                Thread::cm_in_progress_offset() as i32,
            );
            let cm_in_progress = self
                .builder
                .ins()
                .icmp_imm(IntCC::NotEqual, cm_in_progress, 0);
            self.builder
                .ins()
                .brif(cm_in_progress, check_is_int, &[], done, &[]);
        }
        {
            self.builder.switch_to_block(check_is_int);
            // 1) check if the value is on-stack integer
            let band = self.builder.ins().band_imm(val, 1);
            let is_int = self.builder.ins().icmp_imm(IntCC::NotEqual, band, 0);
            self.builder
                .ins()
                .brif(is_int, done, &[], check_heap_allocated, &[]);
        }
        {
            self.builder.seal_block(check_is_int);
            // 2) check if the value is heap allocated
            self.builder.switch_to_block(check_heap_allocated);
            let ty = self
                .builder
                .ins()
                .load(types::I8, MemFlags::trusted(), val, 0);
            let is_heap_allocated = self.builder.ins().icmp_imm(
                IntCC::UnsignedGreaterThan,
                ty,
                crate::value::Type::Integer as i64,
            );
            self.builder
                .ins()
                .brif(is_heap_allocated, write_barrier_check, &[], done, &[]);
        }

        {
            self.builder.seal_block(check_heap_allocated);
            // 3) Check if LocalSSB queue is full
            self.builder.switch_to_block(write_barrier_check);

            let satb_buffer_index = self.builder.ins().load(
                types::I64,
                MemFlags::trusted(),
                mutator_ref,
                Thread::satb_index_offset() as i32,
            );
            let is_zero = self
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, satb_buffer_index, 0);
            self.builder.ins().brif(
                is_zero,
                write_barrier_slow,
                &[val],
                write_barrier_fast,
                &[satb_buffer_index],
            );
        }
        {
            self.builder.seal_block(write_barrier_check);
            // 4) Write barrier fast path
            // - Calculate new LocalSSB index
            // - Store the value to LocalSSB buffer at new index
            // - Update LocalSSB index to new index
            //
            // Note: we do not check if the value is marked or not. It is done in the slow path when buffer is full.
            // We also do not check if object is already in buffer, we offload this to the concurrent marker that will do this
            // work for us for free (Simply won't mark object multiple times).
            self.builder.switch_to_block(write_barrier_fast);
            let satb_buffer_index = self.builder.block_params(write_barrier_fast)[0];
            let new_index = self.builder.ins().iadd_imm(satb_buffer_index, -1);
            let satb_buffer = self.builder.ins().load(
                types::I64,
                MemFlags::trusted(),
                mutator_ref,
                Thread::satb_buffer_offset() as i32,
            );
            let offset = self.builder.ins().imul_imm(new_index, 8);
            let loc = self.builder.ins().iadd(satb_buffer, offset);
            let p = self.builder.ins().iconst(types::I64, 8);
            let val = self.builder.ins().isub(val, p); // get header
            self.builder.ins().store(MemFlags::trusted(), val, loc, 0);
            self.builder.ins().store(
                MemFlags::trusted(),
                new_index,
                mutator_ref,
                Thread::satb_index_offset() as i32,
            );
            self.builder.ins().jump(done, &[]);
        }
        self.builder.seal_block(write_barrier_fast);
        self.builder.switch_to_block(done);
        self.slow_paths.push(Box::new(move |this| {
            // 5) Write barrier slow path
            // - Invokes function to flush LocalSSB and do write barrier out-of-line
            this.builder.switch_to_block(write_barrier_slow);
            let cell = this.builder.block_params(write_barrier_slow)[0];
            let write_barrier = this
                .module
                .declare_func_in_func(this.functions.flush_ssb_and_do_wb, &mut this.builder.func);
            this.builder.ins().call(write_barrier, &[this.vm, cell]);
            this.builder.ins().jump(done, &[]);
        }));
    }

    pub fn generate_reference(&mut self, exp: ScmValue, tail: bool) -> Value {
        let n = exp.cadr();
        let _an = exp.caddr();
        let addr = exp.cadddr();

        let val = if addr.falsep() {
            self.generate_global_reference(n)
        } else {
            let kind = addr.car();
            let ix = addr.cdr();

            match kind.strsym() {
                "bound" => {
                    let var = Variable::from_u32(ix.int() as _);
                    self.builder.use_var(var)
                }
                "free" => {
                    let env = self.env;
                    let val_ptr = self
                        .builder
                        .ins()
                        .iadd_imm(env, Vector::data_offset() as i64 + (ix.int() as i64 * 8));

                    let val = self
                        .builder
                        .ins()
                        .load(types::I64, MemFlags::trusted(), val_ptr, 0);
                    val
                }
                _ => unreachable!(),
            }
        };

        if tail {
            unreachable!()
        }
        val
    }

    pub fn generate_set(&mut self, exp: ScmValue) -> Value {
        let var = exp.cadr();
        let val = self.compile(exp.caddr(), false);

        let n = var.cadr();
        let _an = var.caddr();
        let addr = var.cadddr();

        if addr.falsep() {
            self.generate_global_set(n, val);
        } else {
            let kind = addr.car();
            let ix = addr.cdr();

            match kind.strsym() {
                "bound" => {
                    let var = Variable::from_u32(ix.int() as _);
                    self.builder.def_var(var, val);
                }
                "free" => {
                    let env = self.env;
                    let val_ptr = self
                        .builder
                        .ins()
                        .iadd_imm(env, Vector::data_offset() as i64 + (ix.int() as i64 * 8));

                    self.builder
                        .ins()
                        .store(MemFlags::trusted(), val, val_ptr, 0);
                }
                _ => unreachable!(),
            }
        }

        val
    }

    pub fn generate_set_box(&mut self, exp: ScmValue) -> Value {
        let var = exp.cadr();
        let val = self.compile(exp.caddr(), false);

        let n = var.cadr();
        let _an = var.caddr();
        let addr = var.cadddr();

        if addr.falsep() {
            let global = self.generate_global_reference(n);
            self.generate_write_barrier(global);
            self.builder.ins().store(
                MemFlags::trusted(),
                val,
                global,
                Boxed::value_offset() as i32,
            );
        } else {
            let kind = addr.car();
            let ix = addr.cdr();

            match kind.strsym() {
                "bound" => {
                    let var = Variable::from_u32(ix.int() as _);
                    let var = self.builder.use_var(var);

                    self.generate_write_barrier(var);
                    self.builder.ins().store(
                        MemFlags::trusted(),
                        val,
                        var,
                        Boxed::value_offset() as i32,
                    );
                }
                "free" => {
                    let env = self.env;
                    let var_ptr = self
                        .builder
                        .ins()
                        .iadd_imm(env, Vector::data_offset() as i64 + (ix.int() as i64 * 8));

                    let var = self
                        .builder
                        .ins()
                        .load(types::I64, MemFlags::trusted(), var_ptr, 0);
                    self.generate_write_barrier(var);
                    self.builder.ins().store(
                        MemFlags::trusted(),
                        val,
                        var_ptr,
                        Boxed::value_offset() as i32,
                    );
                }
                _ => unreachable!(),
            }
        }

        val
    }

    pub fn generate_unbox(&mut self, exp: ScmValue, tail: bool) -> Value {
        let val = self.compile(exp.cadr(), false);
        let val = self.builder.ins().load(
            types::I64,
            MemFlags::trusted(),
            val,
            Boxed::value_offset() as i32,
        );

        if tail {
            unreachable!()
        }
        val
    }

    pub fn generate_box(&mut self, exp: ScmValue, tail: bool) -> Value {
        let val = self.compile(exp.cadr(), false);

        let box_func = self
            .module
            .declare_func_in_func(self.functions.r#box, &mut self.builder.func);
        let boxed = self.builder.ins().call(box_func, &[self.vm, val]);
        let boxed = self.builder.inst_results(boxed)[0];
        if tail {
            unreachable!()
        }
        boxed
    }

    fn compile(&mut self, exp: ScmValue, tail: bool) -> Value {
        match exp.car().strsym() {
            "#%app" => self.compile_app(exp.cdr(), tail),
            "if" => self.compile_if(exp.cdr(), tail),
            "begin" => self.compile_sequence(exp.cdr(), tail),
            "lambda" => self.compile_lambda(exp, tail),
            "quote" => {
                let val = exp.cadr();
                self.builder.ins().iconst(types::I64, val.raw() as i64)
            }

            "set!" => self.generate_set(exp),
            "#%set-box!" => self.generate_set_box(exp),
            "#%unbox" => self.generate_unbox(exp, tail),
            "#%box" => self.generate_box(exp, tail),
            "#%ref" => self.generate_reference(exp, tail),
            "#%toplevel-cont" => self.cont,
            _ => unreachable!(),
        }
    }

    pub fn translate(&mut self) {
        self.compile_sequence(self.body, true);

        for slow_path in std::mem::replace(&mut self.slow_paths, vec![]) {
            slow_path(self);
        }

        for slow_path in std::mem::replace(&mut self.slow_paths, vec![]) {
            slow_path(self);
        }

        for slow_path in std::mem::replace(&mut self.slow_paths, vec![]) {
            slow_path(self);
        }

        self.builder.seal_all_blocks();
    }
}


#[allow(dead_code)]
fn disassemble(mem: &[u8]) {
    use capstone::prelude::*;
    let cs = Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .syntax(arch::x86::ArchSyntax::Att)
        .detail(true)
        .build()
        .expect("Failed to create Capstone object");

    let insns = cs
        .disasm_all(mem, &mem[0] as *const u8 as u64)
        .expect("Failed to disassemble");

    for ins in insns.as_ref() {
        println!("{}", ins);
    }


}
