#![allow(dead_code)]
use std::{
    cell::Cell,
    fmt::Display,
    hash::Hash,
    intrinsics::{likely, unlikely},
    panic::{RefUnwindSafe, UnwindSafe},
    ptr::null_mut,
    sync::atomic::{AtomicI32, AtomicUsize},
};

use r7rs_parser::{
    expr::NoIntern,
    parser::{ParseError, Parser},
};
use rsgc::heap::root_processor::ThreadRootProcessor;

use crate::{
    compiler::{r7rs_to_value, Compiler},
    data::{
        exception::{Exception, SourcePosition},
        special_form::{Form, SpecialForm},
        structure::{StructInstance, StructProperty, StructType},
    },
    prelude::*,
    utilities::arraylist::ArrayList,
};

use super::{
    code::{Code, Ins},
    eval_error::EvalError,
};

/// Thread-local context for the Scheme runtime.
///
/// Holds VM state, mutator for allocation, and a pointer to the runtime.
pub struct Context {
    pub(crate) stack: Vec<Value>,
    pub(crate) sp: usize,
    pub(crate) registers: Registers,
    pub(crate) winders: Option<Handle<Winder>>,
    rt: &'static mut Runtime,
    limit_sp: usize,
    thread: &'static mut Thread,

    open_upvalues: Option<Handle<Upvalue>>,

    pub(crate) next: *mut Context,
    pub(crate) prev: *mut Context,
}

impl Context {
    pub(crate) fn new(
        rt: &'static mut Runtime,
        limit_sp: usize,
        thread: &'static mut Thread,
    ) -> &'static mut Self {
        let code = {
            let insns = ArrayList::new(thread);
            let consts = ArrayList::new(thread);
            let frags = ArrayList::new(thread);
            let module = library_manager().null_module;
            Code::new(thread, insns, consts, frags, module.get_handle_of())
        };

        let mut stack = Vec::with_capacity(512);
        stack.resize(512, Value::UNDEFINED);
        let this = Box::leak(Box::new(Self {
            registers: Registers::new(code, library_manager().null_module, None, 0, true),
            winders: None,
            rt,
            limit_sp,
            thread,
            open_upvalues: None,
            stack,
            sp: 0,
            next: null_mut(),
            prev: null_mut(),
        }));
        let ptr = this as *mut Self;
        this.rt.add_context(ptr);
        {
            CURRENT_CONTEXT.with(|cur| {
                cur.set(ptr);
            });
        }
        this
    }

    pub fn parse(&mut self, path: &str, fold_case: bool) -> Result<Value, Handle<Exception>> {
        let exprs = self.parse_exprs(path, fold_case)?;

        Ok(Value::make_list_arraylist(self, &exprs, Value::nil()))
    }

    pub fn parse_exprs(
        &mut self,
        path: &str,
        fold_case: bool,
    ) -> Result<ArrayList<Value>, Handle<Exception>> {
        let runtime = Runtime::get();
        let mut manager = runtime.source_manager.lock(true);
        let res = manager.read_source(path);

        match res {
            Ok((source_id, source)) => {
                let mut no_intern = NoIntern;
                let mut parser = Parser::new(&mut no_intern, source, fold_case);

                let mut exprs = ArrayList::new(self.mutator());

                while !parser.finished() {
                    let expr = parser.parse(true);

                    match expr {
                        Ok(expr) => {
                            let val = r7rs_to_value(self, source_id as _, &expr);
                            let mut i = NoIntern;
                            println!("compile {}", expr.to_string(&mut i, false));
                            exprs.push(self.mutator(), val);
                        }
                        Err(ParseError::Syntax(position, error)) => {
                            let exc = Exception::syntax(
                                self,
                                error,
                                &[],
                                SourcePosition {
                                    position,
                                    source_id: source_id as _,
                                },
                            );

                            return Err(exc);
                        }

                        Err(ParseError::Lexical(position, error)) => {
                            let exc = Exception::lexical(
                                self,
                                error,
                                &[],
                                SourcePosition {
                                    position,
                                    source_id: source_id as _,
                                },
                            );

                            return Err(exc);
                        }
                    }
                }

                return Ok(exprs);
            }
            Err(err) => {
                todo!("error: {:?}", err);
            }
        }
    }

    pub fn compile_and_eval(&mut self, expr: Value, library: Handle<Library>) -> ScmResult {
        self.try_catch(move |ctx| {
            let code = ctx.make_pair(expr, Value::nil());

            let code = Compiler::build(ctx, code, library, None)?;

            let proc = Procedure {
                kind: ProcedureKind::Closure(ClosureType::Anonymous, Value::nil(), None, code),
                id: Procedure::new_id(),
                module: library,
            };

            let proc = Value::new(ctx.mutator().allocate(proc));

            ctx.apply(proc, Value::nil())
        })
    }

    pub fn eval_path(&mut self, path: &str, fold_case: bool) -> ScmResult {
        let exprs = self.parse(path, fold_case)?;

        let source_dir = std::path::Path::new(path).parent().unwrap();

        let loader = self.runtime().loader;
        let s = Str::new(self.mutator(), source_dir.display().to_string());
        let lib = library_manager().user_module;
        let list = Value::make_list_slice(self, &[exprs, Value::new(s), lib], Value::nil());
        self.apply(loader, list)
    }

    pub fn eval_path_in(&mut self, path: &str, fold_case: bool, lib: Handle<Library>) -> ScmResult {
        let exprs = self.parse(path, fold_case)?;

        let source_dir = std::path::Path::new(path).parent().unwrap();

        let loader = self.runtime().loader;
        let s = Str::new(self.mutator(), source_dir.display().to_string());
        let lib = Value::new(lib);
        let list = Value::make_list_slice(self, &[exprs, Value::new(s), lib], Value::nil());
        self.apply(loader, list)
    }

    #[inline(always)]
    pub fn push(&mut self, v: impl Into<Value>) -> ScmResult<()> {
        let v = v.into();
        //if self.sp < self.stack.len() {
        unsafe {
            *self.stack.get_unchecked_mut(self.sp as usize) = v;
        }
        //} else {
        //  self.push_slow(v)?;
        //}
        self.sp += 1;
        Ok(())
    }

    #[inline(always)]
    pub fn pop(&mut self) -> Value {
        self.sp -= 1;
        unsafe { *self.stack.get_unchecked(self.sp as usize) }
    }

    #[inline(always)]
    pub fn popn(&mut self, n: usize) {
        self.sp -= n;
    }

    #[cold]
    fn push_slow(&mut self, _: Value) -> ScmResult<()> {
        let exc = Exception::eval(
            self,
            EvalError::StackOverflow,
            &[],
            SourcePosition::unknown(),
        );
        self.error(exc)
    }

    #[inline]
    pub fn top(&self) -> Value {
        unsafe { *self.stack.get_unchecked(self.sp as usize - 1) }
    }

    pub fn current() -> &'static mut Self {
        CURRENT_CONTEXT.with(|cur| unsafe { &mut *cur.get() })
    }

    pub fn mutator(&mut self) -> &mut Thread {
        self.thread
    }

    pub fn runtime(&mut self) -> &mut Runtime {
        self.rt
    }

    pub(crate) fn roots(&self, processor: &mut ThreadRootProcessor) {
        let visitor = processor.visitor();
        for val in self.stack.iter().take(self.sp) {
            val.trace(visitor);
        }
        self.registers.trace(visitor);
        self.open_upvalues.trace(visitor);
        if let Some(winders) = self.winders {
            winders.trace(visitor);
        }
    }

    pub fn make_pair(&mut self, car: Value, cdr: Value) -> Value {
        self.thread.safepoint();
        let pair = Pair { car, cdr };
        Value::new(self.thread.allocate(pair))
    }

    fn pop_as_list(&mut self, n: usize) -> Value {
        let mut res = Value::nil();

        let mut i = n;

        while i > 0 {
            let v = self.pop();
            res = self.make_pair(v, res);
            i -= 1;
        }

        res
    }

    /// Pushes the given list of arguments onto the stack and returns the number of arguments pushed
    /// onto the stack.
    fn push_arguments(&mut self, arglist: Value) -> ScmResult<usize> {
        let mut args = arglist;
        let mut n = 0;
        while args.is_pair() {
            let arg = args.car();
            self.push(arg)?;
            n += 1;
            args = args.cdr();
        }

        if !args.is_null() {
            let exc = Exception::eval(
                self,
                EvalError::MalformedArgumentList,
                &[arglist],
                SourcePosition::unknown(),
            );
            self.error(exc)?;
        }

        Ok(n)
    }

    pub fn try_catch<R>(
        &mut self,
        closure: impl FnOnce(&mut Self) -> ScmResult<R>,
    ) -> ScmResult<R> {
        closure(self)
    }

    pub fn try_finally<T, R>(
        &mut self,
        data: &mut T,
        closure: impl FnOnce(&mut Self, &mut T) -> ScmResult<R>,
        finally: impl FnOnce(&mut Self, &mut T) -> ScmResult<()>,
    ) -> ScmResult<R> {
        let result = closure(self, data);
        finally(self, data)?;

        result
    }

    pub fn make_arguments(&mut self, args: &[Value]) -> Value {
        Value::make_list_slice(self, args, Value::nil())
    }

    pub fn apply(&mut self, fun: Value, args: Value) -> ScmResult {
        self.try_catch(|ctx| unsafe {
            ctx.push(fun)?;

            let mut n = ctx.push_arguments(args)?; // throws

            let proc = ctx.invoke(&mut n, 1)?; // throws

            match proc.kind {
                ProcedureKind::Closure(_, _, ref captured, ref code) => {
                    ctx.execute_catch(*code, n, *captured) // throws
                }

                ProcedureKind::Transformer(ref rules) => {
                    let rules = *rules;

                    if n != 1 {
                        let exc = Exception::argument_count(
                            ctx,
                            None,
                            1,
                            1,
                            args,
                            SourcePosition::unknown(),
                        );
                        ctx.error(exc)?;
                    }
                    let arg = ctx.pop();
                    let res = rules.expand(ctx, arg);

                    ctx.pop();
                    Ok(res)
                }

                ProcedureKind::RawContinuation(ref cont) => {
                    let mut cont = *cont;
                    cont.argument = super::libraries::control_flow::make_values(ctx, args)?;

                    std::panic::resume_unwind(Box::new(cont));
                }
                // native function executed
                _ => Ok(ctx.pop()),
            }
        })
    }

    fn get_proc(&mut self, n: usize) -> Value {
        unsafe { *self.stack.get_unchecked(self.sp - n - 1) }
    }


    pub fn check_arity(&mut self, proc: Handle<Procedure>, n: usize) -> bool {
        let arity = proc.arity();

        for ar in arity.iter() {
            match ar {
                Arity::AtLeast(x) => {
                    if n >= *x {
                        return true;
                    }
                }
                Arity::Exact(x) => {
                    if n == *x {
                        return true;
                    }
                }
            }
        }

        false
    }

    fn invoke(&mut self, n: &mut usize, overhead: usize) -> ScmResult<Handle<Procedure>> {
        let p = unsafe { *self.stack.get_unchecked(self.sp - *n - 1) }; //[self.sp - *n - 1];

        if unlikely(!p.is_handle_of::<Procedure>()) {
            let exc = Exception::eval(
                self,
                EvalError::NonApplicativeValue,
                &[p],
                SourcePosition::unknown(),
            );
            self.error(exc)?;
        }

        let mut proc = p.get_handle_of::<Procedure>();
        if matches!(proc.kind, ProcedureKind::Closure(_, _, _, _)) {
            return Ok(proc);
        }
        let mut res = #[inline(always)]
        || -> ScmResult<Option<Handle<Procedure>>> {
            loop {
                if let ProcedureKind::Primitive(_, ref imp, _) = proc.kind {
                    match imp {
                        Implementation::Eval(eval) => {
                            let this = unsafe { &mut *(self as *const Self as *mut Self) };
                            let generated = eval(self, &this.stack[this.sp - *n..this.sp])?;
                            self.popn(*n + 1);

                            let proc = Procedure {
                                kind: ProcedureKind::Closure(
                                    ClosureType::Anonymous,
                                    Value::nil(),
                                    None,
                                    generated,
                                ),
                                id: Procedure::new_id(),
                                module: generated.module,
                            };

                            let proc = self.mutator().allocate(proc);
                            self.push(Value::new(proc))?;

                            *n = 0;
                            return Ok(Some(proc));
                        }
                        Implementation::Apply(apply) => {
                            let this = unsafe { &mut *(self as *const Self as *mut Self) };
                            let (next, args) = apply(self, &this.stack[this.sp - *n..this.sp])?;
                            self.popn(*n + 1);
                            self.push(Value::new(next))?;
                            for i in 0..args.len() {
                                self.push(args[i])?;
                            }
                            *n = args.len();
                            proc = next;
                            continue;
                        }
                        Implementation::Native0(exec) => {
                            if unlikely(*n != 0) {
                                let args = self.pop_as_list(*n as _);
                                let exc = Exception::argument_count(
                                    self,
                                    None,
                                    0,
                                    0,
                                    args,
                                    SourcePosition::unknown(),
                                );
                                self.error(exc)?;
                            }

                            self.popn(overhead);
                            let res = exec(self)?;
                            self.push(res)?;
                            *n = 0;
                            return Ok(Some(proc));
                        }

                        Implementation::Native1(exec) => {
                            if unlikely(*n != 1) {
                                let args = self.pop_as_list(*n as _);
                                let exc = Exception::argument_count(
                                    self,
                                    None,
                                    1,
                                    1,
                                    args,
                                    SourcePosition::unknown(),
                                );
                                self.error(exc)?;
                            }
                            let arg = self.pop();
                            self.popn(overhead);

                            let res = exec(self, arg)?;
                            self.push(res)?;
                            *n = 0;
                            return Ok(Some(proc));
                        }

                        Implementation::Native2(exec) => {
                            if unlikely(*n != 2) {
                                let args = self.pop_as_list(*n as _);
                                let exc = Exception::argument_count(
                                    self,
                                    None,
                                    2,
                                    2,
                                    args,
                                    SourcePosition::unknown(),
                                );
                                self.error(exc)?;
                            }
                            let arg2 = self.pop();
                            let arg1 = self.pop();
                            self.popn(overhead);
                            let res = exec(self, arg1, arg2)?;
                            self.push(res)?;
                            *n = 0;
                            return Ok(Some(proc));
                        }

                        Implementation::Native3(exec) => {
                            if unlikely(*n != 3) {
                                let args = self.pop_as_list(*n as _);
                                let exc = Exception::argument_count(
                                    self,
                                    None,
                                    3,
                                    3,
                                    args,
                                    SourcePosition::unknown(),
                                );
                                self.error(exc)?;
                            }
                            let arg3 = self.pop();
                            let arg2 = self.pop();
                            let arg1 = self.pop();
                            self.popn(overhead);

                            let res = exec(self, arg1, arg2, arg3)?;
                            self.push(res)?;
                            *n = 0;
                            return Ok(Some(proc));
                        }

                        Implementation::Native4(exec) => {
                            if unlikely(*n != 4) {
                                let args = self.pop_as_list(*n as _);
                                let exc = Exception::argument_count(
                                    self,
                                    None,
                                    4,
                                    4,
                                    args,
                                    SourcePosition::unknown(),
                                );
                                self.error(exc)?;
                            }
                            let arg4 = self.pop();
                            let arg3 = self.pop();
                            let arg2 = self.pop();
                            let arg1 = self.pop();
                            self.popn(overhead);

                            let res = exec(self, arg1, arg2, arg3, arg4)?;
                            self.push(res)?;
                            *n = 0;
                            return Ok(Some(proc));
                        }

                        Implementation::Native0O(exec) => {
                            if *n == 0 {
                                self.popn(overhead as _);
                                let res = exec(self, None)?;
                                self.push(res)?;
                            } else if *n == 1 {
                                self.popn(overhead);
                                let arg = self.pop();

                                let res = exec(self, Some(arg))?;
                                self.push(res)?;
                            } else {
                                let args = self.pop_as_list(*n as _);
                                let exc = Exception::argument_count(
                                    self,
                                    None,
                                    0,
                                    1,
                                    args,
                                    SourcePosition::unknown(),
                                );
                                self.error(exc)?;
                            }
                            return Ok(Some(proc));
                        }

                        Implementation::Native1O(exec) => {
                            if *n == 1 {
                                let arg = self.pop();
                                self.popn(overhead);
                                let res = exec(self, arg, None)?;
                                self.push(res)?;
                            } else if *n == 2 {
                                let arg2 = self.pop();
                                let arg1 = self.pop();
                                self.popn(overhead);
                                let res = exec(self, arg1, Some(arg2))?;
                                self.push(res)?;
                            } else {
                                let args = self.pop_as_list(*n as _);
                                let exc = Exception::argument_count(
                                    self,
                                    None,
                                    1,
                                    2,
                                    args,
                                    SourcePosition::unknown(),
                                );
                                self.error(exc)?;
                            }
                            return Ok(Some(proc));
                        }

                        Implementation::Native2O(exec) => {
                            if *n == 2 {
                                let arg2 = self.pop();
                                let arg1 = self.pop();
                                self.popn(overhead);
                                let res = exec(self, arg1, arg2, None)?;
                                self.push(res)?;
                            } else if *n == 3 {
                                let arg3 = self.pop();
                                let arg2 = self.pop();
                                let arg1 = self.pop();
                                self.popn(overhead);
                                let res = exec(self, arg1, arg2, Some(arg3))?;
                                self.push(res)?;
                            } else {
                                let args = self.pop_as_list(*n as _);
                                let exc = Exception::argument_count(
                                    self,
                                    None,
                                    2,
                                    3,
                                    args,
                                    SourcePosition::unknown(),
                                );
                                self.error(exc)?;
                            }
                            return Ok(Some(proc));
                        }

                        Implementation::Native3O(exec) => {
                            if *n == 3 {
                                let arg3 = self.pop();
                                let arg2 = self.pop();
                                let arg1 = self.pop();
                                self.popn(overhead);
                                let res = exec(self, arg1, arg2, arg3, None)?;
                                self.push(res)?;
                            } else if *n == 4 {
                                let arg4 = self.pop();
                                let arg3 = self.pop();
                                let arg2 = self.pop();
                                let arg1 = self.pop();
                                self.popn(overhead);
                                let res = exec(self, arg1, arg2, arg3, Some(arg4))?;
                                self.push(res)?;
                            } else {
                                let args = self.pop_as_list(*n as _);
                                let exc = Exception::argument_count(
                                    self,
                                    None,
                                    3,
                                    4,
                                    args,
                                    SourcePosition::unknown(),
                                );
                                self.error(exc)?;
                            }
                            return Ok(Some(proc));
                        }

                        Implementation::Native0OO(exec) => {
                            if *n == 0 {
                                self.popn(overhead as _);
                                let res = exec(self, None, None)?;
                                self.push(res)?;
                            } else if *n == 1 {
                                let arg = self.pop();
                                self.popn(overhead);
                                let res = exec(self, Some(arg), None)?;
                                self.push(res)?;
                            } else if *n == 2 {
                                let arg2 = self.pop();
                                let arg1 = self.pop();
                                self.popn(overhead);
                                let res = exec(self, Some(arg1), Some(arg2))?;
                                self.push(res)?;
                            } else {
                                let args = self.pop_as_list(*n as _);
                                let exc = Exception::argument_count(
                                    self,
                                    None,
                                    0,
                                    2,
                                    args,
                                    SourcePosition::unknown(),
                                );
                                self.error(exc)?;
                            }
                            return Ok(Some(proc));
                        }

                        Implementation::Native1OO(exec) => {
                            if *n == 1 {
                                let arg = self.pop();
                                self.popn(overhead);
                                let res = exec(self, arg, None, None)?;
                                self.push(res)?;
                            } else if *n == 2 {
                                let arg2 = self.pop();
                                let arg1 = self.pop();
                                self.popn(overhead);
                                let res = exec(self, arg1, Some(arg2), None)?;
                                self.push(res)?;
                            } else if *n == 3 {
                                let arg3 = self.pop();
                                let arg2 = self.pop();
                                let arg1 = self.pop();
                                self.popn(overhead);
                                let res = exec(self, arg1, Some(arg2), Some(arg3))?;
                                self.push(res)?;
                            } else {
                                let args = self.pop_as_list(*n as _);
                                let exc = Exception::argument_count(
                                    self,
                                    None,
                                    1,
                                    3,
                                    args,
                                    SourcePosition::unknown(),
                                );
                                self.error(exc)?;
                            }

                            return Ok(Some(proc));
                        }

                        Implementation::Native2OO(exec) => {
                            if *n == 2 {
                                let arg2 = self.pop();
                                let arg1 = self.pop();
                                self.popn(overhead);
                                let res = exec(self, arg1, arg2, None, None)?;
                                self.push(res)?;
                            } else if *n == 3 {
                                let arg3 = self.pop();
                                let arg2 = self.pop();
                                let arg1 = self.pop();
                                self.popn(overhead);
                                let res = exec(self, arg1, arg2, Some(arg3), None)?;
                                self.push(res)?;
                            } else if *n == 4 {
                                let arg4 = self.pop();
                                let arg3 = self.pop();
                                let arg2 = self.pop();
                                let arg1 = self.pop();
                                self.popn(overhead);
                                let res = exec(self, arg1, arg2, Some(arg3), Some(arg4))?;
                                self.push(res)?;
                            } else {
                                let args = self.pop_as_list(*n as _);
                                let exc = Exception::argument_count(
                                    self,
                                    None,
                                    2,
                                    4,
                                    args,
                                    SourcePosition::unknown(),
                                );
                                self.error(exc)?;
                            }

                            return Ok(Some(proc));
                        }

                        Implementation::Native3OO(exec) => {
                            if *n == 3 {
                                let arg3 = self.pop();
                                let arg2 = self.pop();
                                let arg1 = self.pop();
                                self.popn(overhead);
                                let res = exec(self, arg1, arg2, arg3, None, None)?;
                                self.push(res)?;
                            } else if *n == 4 {
                                let arg4 = self.pop();
                                let arg3 = self.pop();
                                let arg2 = self.pop();
                                let arg1 = self.pop();
                                self.popn(overhead);
                                let res = exec(self, arg1, arg2, arg3, Some(arg4), None)?;
                                self.push(res)?;
                            } else if *n == 5 {
                                let arg5 = self.pop();
                                let arg4 = self.pop();
                                let arg3 = self.pop();
                                let arg2 = self.pop();
                                let arg1 = self.pop();
                                self.popn(overhead);
                                let res = exec(self, arg1, arg2, arg3, Some(arg4), Some(arg5))?;
                                self.push(res)?;
                            } else {
                                let args = self.pop_as_list(*n as _);
                                let exc = Exception::argument_count(
                                    self,
                                    None,
                                    3,
                                    5,
                                    args,
                                    SourcePosition::unknown(),
                                );
                                self.error(exc)?;
                            }

                            return Ok(Some(proc));
                        }

                        Implementation::Native0R(exec) => {
                            let this = unsafe { &mut *(self as *const Self as *mut Self) };
                            let args = &self.stack[(self.sp - *n)..self.sp];
                            let res = exec(this, args)?;
                            self.popn(*n as usize + overhead as usize);
                            self.push(res)?;

                            return Ok(Some(proc));
                        }

                        Implementation::Native1R(exec) => {
                            let this = unsafe { &mut *(self as *const Self as *mut Self) };
                            if *n >= 1 {
                                let arg0 = self.stack[self.sp - *n];
                                let args = &self.stack[(self.sp - *n + 1)..self.sp];

                                let res = exec(this, arg0, args)?;
                                self.popn(*n as usize + overhead as usize);
                                self.push(res)?;
                            } else {
                                let args = self.pop_as_list(*n as _);
                                let exc = Exception::argument_count(
                                    self,
                                    None,
                                    1,
                                    0,
                                    args,
                                    SourcePosition::unknown(),
                                );
                                self.error(exc)?;
                            }

                            return Ok(Some(proc));
                        }

                        Implementation::Native2R(exec) => {
                            let this = unsafe { &mut *(self as *const Self as *mut Self) };
                            if *n >= 2 {
                                let arg0 = self.stack[self.sp - *n];
                                let arg1 = self.stack[self.sp - *n + 1];
                                let args = &self.stack[(self.sp - *n + 2)..self.sp];
                                let res = exec(this, arg0, arg1, args)?;
                                self.popn(*n as usize + overhead as usize);
                                self.push(res)?;
                            } else {
                                let args = self.pop_as_list(*n as _);
                                let exc = Exception::argument_count(
                                    self,
                                    None,
                                    2,
                                    0,
                                    args,
                                    SourcePosition::unknown(),
                                );
                                self.error(exc)?;
                            }

                            return Ok(Some(proc));
                        }

                        Implementation::Native3R(exec) => {
                            let this = unsafe { &mut *(self as *const Self as *mut Self) };
                            if *n >= 3 {
                                let arg0 = self.stack[self.sp - *n];
                                let arg1 = self.stack[self.sp - *n + 1];
                                let arg2 = self.stack[self.sp - *n + 2];
                                let args = &self.stack[(self.sp - *n + 3)..self.sp];

                                let res = exec(this, arg0, arg1, arg2, args)?;
                                self.popn(*n as usize + overhead as usize);
                                self.push(res)?;
                            } else {
                                let args = self.pop_as_list(*n as _);
                                let exc = Exception::argument_count(
                                    self,
                                    None,
                                    3,
                                    0,
                                    args,
                                    SourcePosition::unknown(),
                                );
                                self.error(exc)?;
                            }
                            return Ok(Some(proc));
                        }

                        Implementation::Native4R(exec) => {
                            let this = unsafe { &mut *(self as *const Self as *mut Self) };
                            if *n >= 3 {
                                let arg0 = self.stack[self.sp - *n];
                                let arg1 = self.stack[self.sp - *n + 1];
                                let arg2 = self.stack[self.sp - *n + 2];
                                let arg3 = self.stack[self.sp - *n + 3];
                                let args = &self.stack[(self.sp - *n + 4)..self.sp];

                                let res = exec(this, arg0, arg1, arg2, arg3, args)?;
                                self.popn(*n as usize + overhead as usize);
                                self.push(res)?;
                            } else {
                                let args = self.pop_as_list(*n as _);
                                let exc = Exception::argument_count(
                                    self,
                                    None,
                                    4,
                                    usize::MAX,
                                    args,
                                    SourcePosition::unknown(),
                                );
                                self.error(exc)?;
                            }
                            return Ok(Some(proc));
                        }
                    }
                } else {
                    return Ok(None);
                }
            }
        };

        let result = res();

        match result {
            Ok(exec) => {
                if let Some(proc) = exec {
                    return Ok(proc);
                } else {
                    match proc.kind {
                        ProcedureKind::RawContinuation(ref cont) => {
                            let mut cont = *cont;
                            let values = self.pop_as_list(*n);
                            cont.argument =
                                super::libraries::control_flow::make_values(self, values)?;
                            return Err(Value::new(cont));
                        }
                        _ => return Ok(proc),
                    }
                }
            }

            Err(e) => {
                if e.is_handle_of::<Exception>() {
                    e.get_handle_of::<Exception>().attach_ctx(self, Some(proc));
                    return Err(Value::new(e));
                } else {
                    return Err(e);
                }
            }
        }
    }

    fn restore_state(&mut self, state: &SavedState) {
        {
            self.stack.fill(Value::UNDEFINED);
            for i in 0..state.stack.len() {
                self.stack[i] = state.stack[i];
            }

            self.sp = state.sp;
            let after: *mut Value = &mut self.stack[self.sp];
            self.close_upvalues(after);
            self.registers = state.registers;
        }
    }

    pub fn save_state(&mut self) -> SavedState {
        let mut stack = ArrayList::new(self.mutator());
        for i in 0..self.sp - 2 {
            let val = self.stack[i];
            stack.push(self.mutator(), val);
        }

        let mut state = SavedState {
            stack,
            sp: self.sp - 2,
            registers: self.registers,
            winders: self.winders,
        };

        state.registers.ip -= 1;

        state
    }

    pub fn get_stack_trace(
        &mut self,
        current: Option<Handle<Procedure>>,
    ) -> ArrayList<Handle<Procedure>> {
        let mut stack_trace = ArrayList::new(self.mutator());

        if let Some(current) = current {
            stack_trace.push(self.mutator(), current);
        }

        let mut fp = self.registers.fp;

        while fp > 0 {
            if self.stack[fp - 1].is_handle_of::<Procedure>() {
                let proc = self.stack[fp - 1].get_handle_of::<Procedure>();
                stack_trace.push(self.mutator(), proc);

                if fp > 2 {
                    let newfp = self.stack[fp - 3];

                    if newfp.is_int32() && newfp.get_int32() < fp as i32 {
                        fp = newfp.get_int32() as usize;
                    } else {
                        return stack_trace;
                    }
                } else {
                    fp = 0;
                }
            } else {
                return stack_trace;
            }
        }

        stack_trace
    }

    pub fn get_call_trace_info(
        &mut self,
        current: Option<Handle<Procedure>>,
        cap: Option<usize>,
    ) -> Option<Vec<String>> {
        Some(
            self.get_call_trace(current, cap)?
                .iter()
                .map(|x| x.to_string(false))
                .collect(),
        )
    }

    
    pub fn get_call_trace(
        &mut self,
        current: Option<Handle<Procedure>>,
        cap: Option<usize>,
    ) -> Option<ArrayList<Value>> {
        let mut cap = cap.unwrap_or(20);

        if cap == 0 {
            return None;
        }

        let mut stack_trace = ArrayList::new(self.mutator());

        if let Some(current) = current {
            cap -= 1;
            let name = self.runtime().symbol_table().intern(current.name());
            let dotdotdot = self.runtime().symbol_table().intern("...");
            let ls = self.make_pair(Value::new(name), Value::new(dotdotdot));
            stack_trace.push(self.mutator(), ls);
        }

        let mut fp = self.registers.fp;

        while fp > 0 && cap > 0 {
            let proc = self.stack[fp - 1];

            if proc.is_handle_of::<Procedure>() {
                let proc = proc.get_handle_of::<Procedure>();
                let arities = proc.arity();

                let mut min = usize::MAX;
                let mut max = Some(0usize);

                for arity in arities.iter() {
                    match arity {
                        Arity::Exact(n) => {
                            if n < &min {
                                min = *n;
                            }
                            if let Some(_) = max.filter(|m| n > m) {
                                max = Some(*n);
                            }
                        }

                        Arity::AtLeast(n) => {
                            if n < &min {
                                min = *n;
                            }

                            max = None;
                        }
                    }
                }

                let mut call = Value::nil();

                if max.is_none() || max.unwrap() > min {
                    let dotdotdot = self.runtime().symbol_table().intern("...");
                    call = self.make_pair(Value::new(dotdotdot), call);
                }

                while min > 0 {
                    min -= 1;
                    let offset = fp + min as usize;
                    if offset > 0 && offset < self.sp {
                        call = self.make_pair(self.stack[offset], call);
                    }
                }

                cap -= 1;
                let name = self.runtime().symbol_table().intern(proc.name());
                let ls = self.make_pair(Value::new(name), call);
                stack_trace.push(self.mutator(), ls);

                if fp > 2 {
                    let newfp = self.stack[fp - 3];
                    if newfp.is_int32() && newfp.get_int32() < fp as i32 {
                        fp = newfp.get_int32() as usize;
                    } else {
                        return Some(stack_trace);
                    }
                } else {
                    fp = 0;
                }
            } else {
                return Some(stack_trace);
            }
        }

        Some(stack_trace)
    }

    fn capture_upvalue(&mut self, location: *mut Value) -> Handle<Upvalue> {
        let mut prev_upvalue = None;
        let mut upvalue = self.open_upvalues;

        while let Some(value) = upvalue {
            if value.stack_location() == location {
                return value;
            }
            upvalue = value.next();
            prev_upvalue = Some(value);
        }

        let created_upvalue = self.mutator().allocate(Upvalue {
            closed: false,
            state: UpvalueState { stack: location },
            next: upvalue,
        });

        if let Some(mut prev) = prev_upvalue {
            prev.next = Some(created_upvalue);
        } else {
            self.open_upvalues = Some(created_upvalue);
        }

        created_upvalue
    }

    fn close_upvalues(&mut self, last: *mut Value) {
        let mut upvalue = self.open_upvalues;

        while let Some(mut value) = upvalue {
            if value.stack_location() >= last {
                value.close();
                self.open_upvalues = value.next();
                upvalue = self.open_upvalues;
            } else {
                upvalue = value.next();
            }
        }
    }

    fn capture(&mut self, n: usize) -> Handle<Array<Handle<Upvalue>>> {
        let mut list = ArrayList::new(self.mutator());

        for i in 0..self.registers.code.captures[n].len() {
            let capture = self.registers.code.captures[n][i];

            if capture.local {
                let offset = self.registers.fp + capture.index as usize;
                let ptr = &mut self.stack[offset] as *mut Value;
                let upvalue = self.capture_upvalue(ptr);
                list.push(self.mutator(), upvalue);
            } else {
                let upvalue =
                    unsafe { self.registers.captured.unwrap_unchecked()[capture.index as usize] };
                list.push(self.mutator(), upvalue);
            }
        }

        Array::new(self.mutator(), list.len(), |_, ix| list[ix])
    }

    /*fn capture(&mut self, n: usize) -> Handle<Array<Handle<Upvalue>>> {
        let mut list = ArrayList::new(self.mutator());
        for i in 0..self.registers.code.captures[n].len() {
            let capture = self.registers.code.captures[n][i];

            if capture.local {
                let offset = self.registers.fp + capture.index as usize;
                let ptr = &mut self.stack[offset] as *mut Value;
                let start = self.stack.as_ptr();
                let end = unsafe { start.add(self.stack.len()) };
                let upvalue = self.mutator().allocate(Upvalue {
                    closed: AtomicBool::new(false),
                    next: AtomicPtr::new(null_mut()),
                    state: UnsafeCell::new(UpvalueState {
                        stack: (ptr, start as usize, end as usize),
                    }),
                });
                unsafe {
                    self.runtime().push_upvalues(upvalue);
                }
                list.push(self.mutator(), upvalue);
            } else {
                let upvalue = self.registers.captured[capture.index as usize];
                list.push(self.mutator(), upvalue);
            }
        }
        Array::new(self.thread, list.len(), |_, ix| list[ix])
    }*/

    pub(crate) unsafe fn execute_named(
        &mut self,
        code: Handle<Code>,
        name: Handle<Str>,
    ) -> ScmResult {
        let proc = self.mutator().allocate(Procedure {
            id: Procedure::new_id(),
            kind: ProcedureKind::Closure(ClosureType::Named(name), Value::UNDEFINED, None, code),
            module: code.module,
        });

        self.push(Value::new(proc))?;

        self.execute_catch(code, 0, None)
    }

    pub fn wind_up(
        &mut self,
        before: Handle<Procedure>,
        after: Handle<Procedure>,
        handlers: Option<Value>,
    ) {
        let winder = Winder::new(before, after, handlers, self.winders);
        self.winders = Some(self.mutator().allocate(winder));
    }

    pub fn wind_down(&mut self) -> Option<Handle<Winder>> {
        let res = self.winders?;

        self.winders = res.next;

        Some(res)
    }

    pub fn current_handlers(&self) -> Option<Value> {
        let mut winders = self.winders;

        while let Some(w) = winders.filter(|w| w.handlers.is_none()) {
            winders = w.next;
        }

        winders?.handlers
    }

    pub(crate) unsafe fn execute_unnamed(&mut self, code: Handle<Code>) -> ScmResult {
        let proc = self.mutator().allocate(Procedure {
            id: Procedure::new_id(),
            kind: ProcedureKind::Closure(ClosureType::Anonymous, Value::UNDEFINED, None, code),
            module: code.module,
        });

        self.push(Value::new(proc))?;
        self.execute_catch(code, 0, None)
    }

    pub(crate) unsafe fn execute_catch(
        &mut self,
        code: Handle<Code>,
        args: usize,
        captured: Option<Handle<Array<Handle<Upvalue>>>>,
    ) -> ScmResult {
        let saved_registers = self.registers;
        self.registers = Registers::new(
            code,
            Value::new(code.module),
            captured,
            self.sp - args,
            !saved_registers.is_initialized(),
        );

        let result = self.execute();

        match result {
            Ok(value) => {
                self.registers = saved_registers;
                Ok(value)
            }
            Err(err) => {
                self.registers = saved_registers;
                if err.is_handle_of::<Exception>() {
                    err.get_handle_of::<Exception>().attach_ctx(self, None);
                }
                return Err(err);
            }
        }
    }

    pub fn dump_stack(&self) {
        for i in 0..self.sp {
            println!("#{}: {}", i, self.stack[i].to_string(false));
        }
    }

    pub(crate) unsafe fn execute(&mut self) -> ScmResult {
        'interp: loop {
            //rsgc::heap::heap::heap().request_gc();
            let ip = self.registers.ip;
            self.registers.ip = ip + 1;
            #[cfg(feature = "code-profiling")]
            let start = { std::time::Instant::now() };

            let ins = *self.registers.code.instructions.get_unchecked(ip);
            debug_assert!(ip < self.registers.code.instructions.len());
            match ins {
                Ins::NoOp => continue,
                Ins::Pop => {
                    self.pop();
                }
                Ins::Dup => {
                    let top = self.top();
                    self.push(top)?;
                }
                Ins::PushVoid => {
                    self.push(Value::void())?;
                }
                Ins::PushUndef => {
                    self.push(Value::UNDEFINED)?;
                }
                Ins::PushConstant(c) => {
                    self.push(self.registers.code.constants[c as usize])?;
                }

                Ins::PushFixnum(n) => {
                    self.push(Value::new(n))?;
                }

                Ins::PushEof => {
                    self.push(Value::eof())?;
                }

                Ins::PushTrue => {
                    self.push(Value::new(true))?;
                }

                Ins::PushFalse => {
                    self.push(Value::new(false))?;
                }

                Ins::PushNull => {
                    self.push(Value::nil())?;
                }

                Ins::PushLocal(l) => {
                    let val = *self.stack.get_unchecked(self.registers.fp + l as usize); // [self.registers.fp + l as usize];
                                                                                         //println!("{:04} push-local {} = {}",ip, l, val.to_string(false));
                    self.push(val)?;
                }

                Ins::SetLocal(l) => {
                    let val = self.pop();
                    //println!("{:04} set-local {} = {}",ip, l, val.to_string(false));
                    *self.stack.get_unchecked_mut(self.registers.fp + l as usize) = val;
                    //self.stack[self.registers.fp + l as usize] = val;
                }

                Ins::PushCaptured(l) => {
                    let val = self.registers.captured.unwrap_unchecked()[l as usize].get();
                    self.push(val)?;
                }

                Ins::SetCaptured(l) => {
                    let val = self.pop();
                    self.thread
                        .write_barrier(self.registers.captured.unwrap_unchecked()[l as usize]);
                    self.registers.captured.unwrap_unchecked()[l as usize].set(val);
                }

                Ins::Add2 => {
                    let b = self.pop();
                    let a = self.pop();

                    if likely(a.is_int32() && b.is_int32()) {
                        self.push(a.get_int32().wrapping_add(b.get_int32()))?;
                    } else if likely(a.is_double() && b.is_double()) {
                        self.push(a.get_double() + b.get_double())?;
                    } else {
                        let x = super::libraries::math::plus(self, &[a, b])?;
                        self.push(x)?;
                    }
                }

                Ins::Sub2 => {
                    let b = self.pop();
                    let a = self.pop();
                    if likely(a.is_int32() && b.is_int32()) {
                        self.push(a.get_int32().wrapping_sub(b.get_int32()))?;
                    } else if likely(a.is_double() && b.is_double()) {
                        self.push(a.get_double() - b.get_double())?;
                    } else {
                        let x = super::libraries::math::minus(self, a, &[b])?;
                        self.push(x)?;
                    }
                }

                Ins::Mul2 => {
                    let b = self.pop();
                    let a = self.pop();
                    if likely(a.is_int32() && b.is_int32()) {
                        self.push(a.get_int32().wrapping_mul(b.get_int32()))?;
                    } else if likely(a.is_double() && b.is_double()) {
                        self.push(a.get_double() * b.get_double())?;
                    } else {
                        let x = super::libraries::math::multiply(self, &[a, b])?;
                        self.push(x)?;
                    }
                }

                Ins::Div2 => {
                    let b = self.pop();
                    let a = self.pop();
                    if likely(a.is_int32() && b.is_int32()) {
                        self.push(a.get_int32().wrapping_div(b.get_int32()))?;
                    } else if likely(a.is_double() && b.is_double()) {
                        self.push(a.get_double() / b.get_double())?;
                    } else {
                        let x = super::libraries::math::divide(self, &[a, b])?;
                        self.push(x)?;
                    }
                }

                Ins::Greater2 => {
                    let b = self.pop();
                    let a = self.pop();
                    if likely(a.is_int32() && b.is_int32()) {
                        self.push(Value::new(a.get_int32() > b.get_int32()))?;
                    } else if likely(a.is_double() && b.is_double()) {
                        self.push(Value::new(a.get_double() > b.get_double()))?;
                    } else {
                        let x = super::libraries::math::greater(self, a, b)?;
                        self.push(x)?;
                    }
                }

                Ins::GreaterEq2 => {
                    let b = self.pop();
                    let a = self.pop();
                    if likely(a.is_int32() && b.is_int32()) {
                        self.push(Value::new(a.get_int32() >= b.get_int32()))?;
                    } else if likely(a.is_double() && b.is_double()) {
                        self.push(Value::new(a.get_double() >= b.get_double()))?;
                    } else {
                        let x = super::libraries::math::greater_or_equal(self, a, b)?;
                        self.push(x)?;
                    }
                }

                Ins::Less2 => {
                    let b = self.pop();
                    let a = self.pop();
                    if likely(a.is_int32() && b.is_int32()) {
                        self.push(Value::new(a.get_int32() < b.get_int32()))?;
                    } else if likely(a.is_double() && b.is_double()) {
                        self.push(Value::new(a.get_double() < b.get_double()))?;
                    } else {
                        let x = super::libraries::math::less(self, a, b)?;
                        self.push(x)?;
                    }
                }

                Ins::LessEq2 => {
                    let b = self.pop();
                    let a = self.pop();
                    if likely(a.is_int32() && b.is_int32()) {
                        self.push(Value::new(a.get_int32() <= b.get_int32()))?;
                    } else if likely(a.is_double() && b.is_double()) {
                        self.push(Value::new(a.get_double() <= b.get_double()))?;
                    } else {
                        let x = super::libraries::math::less_or_equal(self, a, b)?;
                        self.push(x)?;
                    }
                }

                Ins::Equal2 => {
                    let b = self.pop();
                    let a = self.pop();
                    if likely(a.is_int32() && b.is_int32()) {
                        self.push(Value::new(a.get_int32() == b.get_int32()))?;
                    } else if likely(a.is_double() && b.is_double()) {
                        self.push(Value::new(a.get_double() == b.get_double()))?;
                    } else {
                        let x = super::libraries::core::equal(self, a, b)?;
                        self.push(x)?;
                    }
                }

                Ins::Eq2 => {
                    let b = self.pop();
                    let a = self.pop();
                    if likely(a.is_int32() && b.is_int32()) {
                        self.push(Value::new(a.get_int32() == b.get_int32()))?;
                    } else if likely(a.is_double() && b.is_double()) {
                        self.push(Value::new(a.get_double() == b.get_double()))?;
                    } else {
                        let x = super::libraries::core::eq(self, a, b)?;
                        self.push(x)?;
                    }
                }

                Ins::Eqv2 => {
                    let b = self.pop();
                    let a = self.pop();
                    if likely(a.is_int32() && b.is_int32()) {
                        self.push(Value::new(a.get_int32() == b.get_int32()))?;
                    } else if likely(a.is_double() && b.is_double()) {
                        self.push(Value::new(a.get_double() == b.get_double()))?;
                    } else {
                        let x = super::libraries::core::eqv(self, a, b)?;
                        self.push(x)?;
                    }
                }

                Ins::MakeVariableArgument(_) => {}
                Ins::MakeLocalVariable(n) => {
                    let val = self.pop();
                    self.stack[self.registers.fp + n as usize] = val;
                }

                Ins::PushGlobal(ix) => {
                    let mut v = *self.registers.code.constants.get_unchecked(ix as usize); //self.registers.code.constants[ix as usize];
                    if likely(v.is_handle_of::<Gloc>()) {
                        v = Gloc::get(self, v.get_handle_of::<Gloc>())?;
                    } else {
                        let mut gloc = None;
                        let id = v.get_handle_of::<Identifier>();
                        v = self.global_ref(id.module, id.name.get_symbol(), &mut gloc)?;

                        if let Some(gloc) = gloc {
                            let thread = Thread::current();
                            self.registers
                                .code
                                .constants
                                .set(thread, ix as _, Value::new(gloc));
                        }
                    }

                    self.push(v)?;
                }

                Ins::SetGlobal(ix) => {
                    let value = self.pop();

                    let gloc = self.registers.code.constants[ix as usize];
                    if likely(gloc.is_handle_of::<Gloc>()) {
                        self.thread.write_barrier(gloc.get_handle());
                        Gloc::set(self, gloc.get_handle_of::<Gloc>(), value)?;
                    } else {
                        let mut gloc_ = None;
                        let id = gloc.get_handle_of::<Identifier>();
                        self.global_set(id.module, id.name.get_symbol(), value, &mut gloc_)?;

                        if let Some(gloc) = gloc_ {
                            let thread = Thread::current();
                            self.registers
                                .code
                                .constants
                                .set(thread, ix as _, Value::new(gloc));
                        }
                    }
                }

                Ins::DefineGlobal(ix, constant) => {
                    let lm = library_manager();
                    let value = self.pop();
                    let identifier = self.registers.code.constants[ix as usize];

                    if !identifier.is_handle_of::<Identifier>() {
                        let name = identifier.get_handle_of::<Gloc>();

                        let exc = Exception::eval(
                            self,
                            EvalError::SymbolAlreadyExists,
                            &[Value::new(name.name())],
                            SourcePosition::unknown(),
                        );

                        return Err(exc.into());
                    }

                    let identifier = identifier.get_handle_of::<Identifier>();
                    let sym = identifier.name.get_symbol();
                    let library = identifier.module;
                    if constant {
                        lm.define_const(library, sym, value, false);
                    } else {
                        lm.define(library, sym, value, false);
                    }

                    self.push(Value::void())?;
                }

                Ins::MakeFrame => {
                    //println!("{:04}: make-frame", ip);
                    self.push(Value::encode_int32(self.registers.fp as i32))?;
                    self.push(Value::UNDEFINED)?;
                }

                Ins::InjectFrame => {
                    let top = self.top();
                    self.push(Value::encode_int32(self.registers.fp as i32))?;
                    self.push(Value::UNDEFINED)?;
                    self.push(top)?;
                }

                Ins::Alloc(n) => {
                    if self.sp + n as usize >= self.stack.len() {
                        let exc = Exception::eval(
                            self,
                            EvalError::StackOverflow,
                            &[],
                            SourcePosition::unknown(),
                        );
                        self.error(exc)?;
                    }

                    self.sp += n as usize;
                }

                Ins::AllocBelow(n) => {
                    let top = self.pop();

                    if self.sp + n as usize >= self.stack.len() {
                        let exc = Exception::eval(
                            self,
                            EvalError::StackOverflow,
                            &[],
                            SourcePosition::unknown(),
                        );
                        self.error(exc)?;
                    }

                    self.sp += n as usize;
                    self.push(top)?;
                }

                Ins::CollectRest(n) => {
                    let mut rest = Value::nil();

                    while self.sp > self.registers.fp + n as usize {
                        let val = self.pop();
                        rest = self.make_pair(val, rest);
                    }

                    self.push(rest)?;
                }

                Ins::AssertArgCount(n) => {
                    if unlikely(self.sp - n as usize != self.registers.fp) {
                        let ls = self.pop_as_list(self.sp - self.registers.fp);
                        let exc = Exception::argument_count(
                            self,
                            None,
                            n as _,
                            n as _,
                            ls,
                            SourcePosition::unknown(),
                        );
                        self.error(exc)?;
                    }
                }

                Ins::AssertMinArgCount(n) => {
                    if (self.sp - n as usize) < self.registers.fp {
                        let ls = self.pop_as_list(self.sp - self.registers.fp);
                        let exc = Exception::argument_count(
                            self,
                            None,
                            n as _,
                            usize::MAX,
                            ls,
                            SourcePosition::unknown(),
                        );
                        self.error(exc)?;
                    }
                }

                Ins::Call(n) => {
                    self.thread.safepoint();
                    *self.stack.get_unchecked_mut(self.sp - n as usize - 2) = //[self.sp - n as usize - 2] =
                        Value::encode_int32(self.registers.ip as _);
                    let mut m = n as usize;

                    let proc = self.get_proc(m);

                    if proc.is_handle_of::<Procedure>() {
                        let proc = proc.get_handle_of::<Procedure>();
                        if let ProcedureKind::Closure(_, _, ref newcaptured, ref newcode) =
                            proc.kind
                        {
                            #[cfg(feature = "code-profiling")]
                            {
                                InsProfile::add(ins, start);
                            }
                            self.registers.r#use(*newcode, *newcaptured, self.sp - m);
                            continue;
                        }
                    }

                    if let ProcedureKind::Closure(_, _, ref newcaptured, ref newcode) =
                        self.invoke(&mut m, 3)?.kind
                    {
                        self.registers.r#use(*newcode, *newcaptured, self.sp - m);
                    }
                }

                Ins::TailCall(m) => {
                    self.thread.safepoint();
                    let mut n = m as usize;
                    let after: *mut Value = &mut self.stack[self.registers.fp];
                    self.close_upvalues(after);

                    let proc = self.get_proc(n);

                    if proc.is_handle_of::<Procedure>() {
                        let proc = proc.get_handle_of::<Procedure>();
                        if let ProcedureKind::Closure(_, _, ref newcaptured, ref newcode) =
                            proc.kind
                        {
                            self.registers
                                .r#use(*newcode, *newcaptured, self.registers.fp);
                            for i in 0..=n {
                                self.stack[self.registers.fp - 1 + i] =
                                    self.stack[self.sp - n - 1 + i];
                            }

                            self.sp = self.registers.fp + n;
                            #[cfg(feature = "code-profiling")]
                            {
                                InsProfile::add(ins, start);
                            }
                            continue;
                        }
                    }

                    let proc = self.invoke(&mut n, 1)?;

                    match proc.kind {
                        ProcedureKind::Closure(_, _, ref newcaptured, ref newcode) => {
                            self.registers
                                .r#use(*newcode, *newcaptured, self.registers.fp);

                            for i in 0..=n {
                                self.stack[self.registers.fp - 1 + i] =
                                    self.stack[self.sp - n - 1 + i];
                            }

                            self.sp = self.registers.fp + n;
                        }
                        ProcedureKind::RawContinuation(_) => (),
                        _ => {
                            if self.registers.top_level() {
                                let res = self.pop();

                                self.sp = self.registers.initial_fp - 1;
                                #[cfg(feature = "code-profiling")]
                                {
                                    InsProfile::add(ins, start);
                                }
                                return Ok(res);
                            } else {
                                self.exit_frame();
                            }
                        }
                    }

                    #[cfg(feature = "code-profiling")]
                    {
                        InsProfile::add(ins, start);
                    }
                }

                Ins::Return => {
                    let after: *mut Value = &mut self.stack[self.registers.fp];
                    self.close_upvalues(after);
                    if self.registers.top_level() {
                        let res = self.pop();

                        self.sp = self.registers.initial_fp - 1;
                        #[cfg(feature = "code-profiling")]
                        {
                            InsProfile::add(ins, start);
                        }
                        return Ok(res);
                    } else {
                        self.exit_frame();
                    }
                }

                Ins::Branch(offset) => {
                    self.registers.ip = (self.registers.ip as i32 + offset - 1) as usize;
                }

                Ins::BranchIf(offset) => {
                    let val = self.pop();
                    if !val.is_false() {
                        self.registers.ip = (self.registers.ip as i32 + offset - 1) as usize;
                    }
                }

                Ins::BranchIfNot(offset) => {
                    let val = self.pop();
                    if val.is_false() {
                        self.registers.ip = (self.registers.ip as i32 + offset - 1) as usize;
                    }
                    self.thread.safepoint();
                }

                Ins::KeepOrBranchIfNot(offset) => {
                    let top = self.pop();
                    if top.is_false() {
                        self.registers.ip = (self.registers.ip as i32 + offset - 1) as usize;
                    } else {
                        self.push(top)?;
                    }
                    self.thread.safepoint();
                }

                Ins::BranchIfArgMismatch(offset, n) => {
                    if self.sp - n as usize != self.registers.fp {
                        self.registers.ip = (self.registers.ip as i32 + offset - 1) as usize;
                    }
                    self.thread.safepoint();
                }

                Ins::BranchIfMinArgMismatch(offset, n) => {
                    if (self.sp - n as usize) < self.registers.fp {
                        self.registers.ip = (self.registers.ip as i32 + offset - 1) as usize;
                    }
                    self.thread.safepoint();
                }

                Ins::Reset(index, n) => {
                    let index = index as usize;
                    let n = n as usize;

                    let after: *mut Value = &mut self.stack[self.registers.fp + index as usize];
                    self.close_upvalues(after);

                    for i in (self.registers.fp + index)..(self.registers.fp + index + n) {
                        self.stack[i] = Value::UNDEFINED;
                    }
                }

                Ins::MakeSyntax(i) => {
                    let transformer = self.pop();

                    if transformer.is_handle_of::<Procedure>() {
                        if i >= 0 {
                            let sym = self.registers.code.constants[i as usize]
                                .get_handle_of::<Identifier>()
                                .name
                                .get_symbol();
                            let special = SpecialForm {
                                kind: Form::Macro(transformer.get_handle_of()),
                                original_name: Some(sym.identifier()),
                            };
                            let special = self.mutator().allocate(special);
                            self.push(Value::new(special))?;
                        } else {
                            let special = SpecialForm {
                                kind: Form::Macro(transformer.get_handle_of()),
                                original_name: None,
                            };
                            let special = self.mutator().allocate(special);
                            self.push(Value::new(special))?;
                        }
                    } else {
                        let exc = Exception::eval(
                            self,
                            EvalError::MalformedTransformer,
                            &[transformer],
                            SourcePosition::unknown(),
                        );
                        self.error(exc)?;
                    }
                }

                Ins::Cons => {
                    let cdr = self.pop();
                    let car = self.pop();
                    let pair = self.make_pair(car, cdr);
                    self.push(pair)?;
                }

                Ins::Car => {
                    let pair = self.pop();
                    if unlikely(!pair.is_pair()) {
                        let exc = Exception::type_error(
                            self,
                            &[Type::Pair],
                            pair,
                            SourcePosition::unknown(),
                        );
                        return self.error(exc);
                    }
                    let car = pair.car();
                    self.push(car)?;
                }

                Ins::Cdr => {
                    let pair = self.pop();
                    if unlikely(!pair.is_pair()) {
                        let exc = Exception::type_error(
                            self,
                            &[Type::Pair],
                            pair,
                            SourcePosition::unknown(),
                        );
                        return self.error(exc);
                    }
                    let cdr = pair.cdr();
                    self.push(cdr)?;
                }

                Ins::AssertStruct(off, n) => {
                    let struct_type =
                        self.registers.code.constants[n as usize].get_handle_of::<StructType>();
                    let val = self.stack[self.registers.fp + off as usize];

                    if unlikely(!val.is_handle_of::<StructInstance>()) {
                        let exc = Exception::type_error(
                            self,
                            &[Type::StructType(Some(struct_type))],
                            val,
                            SourcePosition::unknown(),
                        );
                        return self.error(exc);
                    }

                    let instance = val.get_handle_of::<StructInstance>();

                    if unlikely(!instance.is_instance_of(struct_type)) {
                        let exc = Exception::type_error(
                            self,
                            &[Type::StructType(Some(struct_type))],
                            val,
                            SourcePosition::unknown(),
                        );
                        return self.error(exc);
                    }
                }

                Ins::MakeStruct(n) => {
                    let struct_type =
                        self.registers.code.constants[n as usize].get_handle_of::<StructType>();
                    let mut instance = StructInstance::new(self, struct_type);

                    for i in (0..struct_type.field_cnt_for_instance()).rev() {
                        let val = self.pop();
                        self.mutator().write_barrier(instance);
                        instance.field_set(i as _, val);
                    }

                    self.push(Value::new(instance))?;
                }

                Ins::StructRef(n) => {
                    let ix = self.pop();
                    let instance = self.pop().get_handle_of::<StructInstance>();
                    let struct_type =
                        self.registers.code.constants[n as usize].get_handle_of::<StructType>();
                    ix.assert_type(self, SourcePosition::unknown(), &[Type::Integer])?;
                    let ix = ix.get_int32() as u32;

                    let allowed_index = struct_type.init_field_cnt();

                    if unlikely(ix >= allowed_index) {
                        let exc = Exception::eval(
                            self,
                            EvalError::OutOfBounds,
                            &[Value::new(ix as i32), Value::new(instance)],
                            SourcePosition::unknown(),
                        );
                        return self.error(exc);
                    }
                    let actual_ix = if let Some(stype) = struct_type.super_type() {
                        stype.field_cnt_for_instance() + ix 
                    } else {
                        ix 
                    };
                    let field = instance.field_ref(actual_ix as _);
                    self.push(field)?;
                }

                Ins::StructSet(n) => {
                    let val = self.pop();
                    let ix = self.pop();

                    let mut instance = self.pop().get_handle_of::<StructInstance>();

                    let struct_type =
                        self.registers.code.constants[n as usize].get_handle_of::<StructType>();

                    ix.assert_type(self, SourcePosition::unknown(), &[Type::Integer])?;
                    let ix = ix.get_int32();
                    if unlikely(ix >= struct_type.field_cnt_for_instance() as i32) {
                        let exc = Exception::eval(
                            self,
                            EvalError::OutOfBounds,
                            &[Value::new(ix), Value::new(instance)],
                            SourcePosition::unknown(),
                        );
                        return self.error(exc);
                    }
                    self.mutator().write_barrier(instance);
                    instance.field_set(ix as _, val);
                }

                Ins::StructRefI(_n, ix) => {
                    let instance = self.pop().get_handle_of::<StructInstance>();

                    let field = instance.field_ref(ix as _);
                    self.push(field)?;
                }

                Ins::StructSetI(_n, ix) => {
                    let val = self.pop();
                    let mut instance = self.pop().get_handle_of::<StructInstance>();

                    self.mutator().write_barrier(instance);
                    instance.field_set(ix as _, val);
                }

                Ins::CheckStruct(n) => {
                    let struct_type =
                        self.registers.code.constants[n as usize].get_handle_of::<StructType>();
                    let val = self.pop();
                    if !val.is_handle_of::<StructInstance>() {
                        self.push(false)?;
                    } else {
                        let instance = val.get_handle_of::<StructInstance>();
                        self.push(instance.is_instance_of(struct_type))?;
                    }
                }

                Ins::CheckStructProperty(n) => {
                    let struct_prop =
                        self.registers.code.constants[n as usize].get_handle_of::<StructProperty>();
                    let val = self.pop();
                    let struct_type = if val.is_handle_of::<StructType>() {
                        val.get_handle_of::<StructType>()
                    } else if val.is_handle_of::<StructInstance>() {
                        val.get_handle_of::<StructInstance>().struct_type()
                    } else {
                        self.push(false)?;
                        continue;
                    };

                    self.push(struct_type.has_property(struct_prop))?;
                }

                Ins::StructPropertyAccessor(n, failure_result) => {
                    let struct_prop =
                        self.registers.code.constants[n as usize].get_handle_of::<StructProperty>();
                    let failure_result = if failure_result {
                        Some(self.pop())
                    } else {
                        None
                    };
                    let val = self.pop();
                    let struct_type = if val.is_handle_of::<StructType>() {
                        val.get_handle_of::<StructType>()
                    } else if val.is_handle_of::<StructInstance>() {
                        val.get_handle_of::<StructInstance>().struct_type()
                    } else {
                        let exc = Exception::type_error(
                            self,
                            &[Type::StructType(None)],
                            val,
                            SourcePosition::unknown(),
                        );
                        return self.error(exc);
                    };

                    for prop in struct_type.props.iter() {
                        if prop.car().get_handle_of::<StructProperty>() == struct_prop {
                            self.push(prop.cdr())?;
                            continue 'interp;
                        }
                    }

                    if let Some(failure_result) = failure_result {
                        if failure_result.is_handle_of::<Procedure>() {
                            let mut n = 0;
                            self.push(failure_result)?;
                            let proc = self.invoke(&mut n, 1)?;
                            match proc.kind {
                                ProcedureKind::Closure(_, _, ref newcaptured, ref newcode) => {
                                    self.registers
                                        .r#use(*newcode, *newcaptured, self.registers.fp);

                                    for i in 0..=n {
                                        self.stack[self.registers.fp - 1 + i] =
                                            self.stack[self.sp - n - 1 + i];
                                    }

                                    self.sp = self.registers.fp + n;
                                }

                                _ => {
                                    if self.registers.top_level() {
                                        let res = self.pop();
        
                                        self.sp = self.registers.initial_fp - 1;
                                        #[cfg(feature = "code-profiling")]
                                        {
                                            InsProfile::add(ins, start);
                                        }
                                        return Ok(res);
                                    } else {
                                        self.exit_frame();
                                    }
                                }
                            }
                        } else {
                            self.push(failure_result)?;
                        }
                    } else {
                        let exc = Exception::custom(
                            self,
                            "evaluation error",
                            "structure property $0 not found in $1",
                            &[Value::new(struct_prop), Value::new(struct_type)],
                            SourcePosition::unknown(),
                        );
                        return self.error(exc);
                    }
                }

                Ins::IsNull => {
                    let val = self.pop();
                    self.push(val.is_null())?;
                }

                Ins::IsPair => {
                    let val = self.pop();
                    self.push(val.is_pair())?;
                }

                Ins::IsVector => {
                    let val = self.pop();
                    self.push(val.is_vector())?;
                }

                Ins::Pack(n) => {
                    let mut list = Value::nil();

                    for _ in 0..n {
                        let val = self.pop();
                        list = self.make_pair(val, list);
                    }

                    let values = self.mutator().allocate(Values(list));
                    self.push(Value::new(values))?;
                }

                Ins::Flatpack(_n) => {
                    let _list = Value::nil();

                    todo!()
                }

                Ins::Unpack(n, overflow) => {
                    let val = self.top();

                    if val.is_void() {
                        if n == 0 {
                            self.pop();
                            if overflow {
                                self.push(Value::nil())?;
                            }
                        } else {
                            let exc = Exception::eval(
                                self,
                                EvalError::MultiValueCountError,
                                &[Value::new(n as i32), Value::nil()],
                                SourcePosition::unknown(),
                            );

                            self.error(exc)?;
                        }
                    } else if val.is_handle_of::<Values>() {
                        self.pop();
                        let mut m = n as i32;

                        let mut next = val.get_handle_of::<Values>().0;

                        let list = next;
                        while next.is_pair() {
                            let rest = next.cdr();
                            let value = next.car();

                            if m <= 0 {
                                if overflow {
                                    break;
                                } else {
                                    let exc = Exception::eval(
                                        self,
                                        EvalError::MultiValueCountError,
                                        &[Value::new(n as i32), list],
                                        SourcePosition::unknown(),
                                    );

                                    self.error(exc)?;
                                }
                            }

                            self.push(value)?;
                            m -= 1;
                            next = rest;
                        }

                        if m != 0 {
                            let exc = Exception::eval(
                                self,
                                EvalError::MultiValueCountError,
                                &[Value::new(n as i32), list],
                                SourcePosition::unknown(),
                            );

                            self.error(exc)?;
                        }

                        if overflow {
                            self.push(next)?;
                        }
                    } else {
                        if n == 1 {
                            if overflow {
                                self.push(Value::nil())?;
                            }
                        } else if n == 0 && overflow {
                            let val = self.pop();
                            let pair = self.make_pair(val, Value::nil());
                            self.push(pair)?;
                        } else {
                            let pair = self.make_pair(self.top(), Value::nil());
                            let exc = Exception::eval(
                                self,
                                EvalError::MultiValueCountError,
                                &[Value::new(n as i32), pair],
                                SourcePosition::unknown(),
                            );

                            self.error(exc)?;
                        }
                    }
                }

                Ins::MakeClosure(i, n, index) => {
                    let typ;
                    if i >= 0 {
                        let sym = self.registers.code.constants[i as usize].get_symbol();
                        typ = ClosureType::Named(Str::new(self.mutator(), sym.to_string()));
                    } else {
                        typ = if i == -2 {
                            ClosureType::Continuation
                        } else {
                            ClosureType::Anonymous
                        };
                    }

                    let captured = self.capture(n as _);

                    let proc = Procedure {
                        id: Procedure::new_id(),
                        module: self.registers.code.fragments[index as usize].module,
                        kind: ProcedureKind::Closure(
                            typ,
                            Value::UNDEFINED,
                            Some(captured),
                            self.registers.code.fragments[index as usize],
                        ),
                    };

                    let proc = self.mutator().allocate(proc);

                    self.push(Value::new(proc))?;
                }

                ins => todo!("{:?}", ins),
            }

            #[cfg(feature = "code-profiling")]
            {
                InsProfile::add(ins, start);
            }
        }
    }

    #[inline(always)]
    fn exit_frame(&mut self) {
        let fp = self.registers.fp;

        #[cfg(debug_assertions)]
        if !self.stack[fp - 2].is_int32() {
            unreachable!("malformed stack");
        }

        self.registers.ip = self.stack[fp - 2].get_int32() as _;
        #[cfg(debug_assertions)]
        if !self.stack[fp - 3].is_int32() {
            unreachable!("malformed stack");
        }
        let newfp = self.stack[fp - 3].get_int32() as usize;

        self.stack[fp - 3] = self.stack[self.sp - 1];

        self.sp = fp - 2;
        self.registers.fp = newfp as _;

        let proc = self.stack[newfp as usize - 1];
        #[cfg(debug_assertions)]
        if !proc.is_handle_of::<Procedure>() {
            unreachable!("malformed stack: procedure expected");
        }

        if let ProcedureKind::Closure(_, _, ref newcaptured, ref newcode) =
            proc.get_handle_of::<Procedure>().kind
        {
            self.registers.captured = *newcaptured;
            self.registers.code = *newcode;
        } else {
            #[cfg(debug_assertions)]
            unreachable!("trying to return to non-VM function");
            #[cfg(not(debug_assertions))]
            unsafe {
                std::hint::unreachable_unchecked();
            }
        }
    }

    fn global_ref(
        &mut self,
        library: Handle<Library>,
        id: Handle<Symbol>,
        gloc_out: &mut Option<Handle<Gloc>>,
    ) -> ScmResult {
        let binding = library_manager().find_binding(library, id, false, false);

        if let Some(binding) = binding {
            *gloc_out = Some(binding);

            return Gloc::get(self, binding);
        } else {
            let exc = Exception::custom(
                self,
                "evaluation error",
                "unbound variable: $0",
                &[id.into()],
                SourcePosition::unknown(),
            );

            self.error(exc)
        }
    }

    fn global_set(
        &mut self,
        lib: Handle<Library>,
        id: Handle<Symbol>,
        value: Value,
        gloc_out: &mut Option<Handle<Gloc>>,
    ) -> ScmResult<()> {
        let binding = library_manager().find_binding(lib, id, false, false);

        if let Some(binding) = binding {
            *gloc_out = Some(binding);
            Gloc::set(self, binding, value)?;
            Ok(())
        } else {
            let exc = Exception::custom(
                self,
                "evaluation error",
                "unbound variable: $0",
                &[id.into()],
                SourcePosition::unknown(),
            );

            self.error(exc)
        }
    }

    #[cold]
    pub fn error<R>(&mut self, exception: Handle<Exception>) -> ScmResult<R> {
        Err(Value::new(exception))
    }
}

thread_local! {
    static CURRENT_CONTEXT: Cell<*mut Context> = Cell::new(null_mut());
}

impl Drop for Context {
    fn drop(&mut self) {
        let ptr = self as *mut Self;
        self.rt.remove_context(ptr);
    }
}

#[allow(dead_code)]
#[derive(Clone, Copy)]
pub struct Registers {
    rid: usize,
    code: Handle<Code>,
    pub(crate) module: Value,
    captured: Option<Handle<Array<Handle<Upvalue>>>>,
    ip: usize,
    fp: usize,
    initial_fp: usize,
}

static RID: AtomicUsize = AtomicUsize::new(0);

impl Registers {
    pub(crate) fn new(
        code: Handle<Code>,
        module: Value,
        captured: Option<Handle<Array<Handle<Upvalue>>>>,
        fp: usize,
        root: bool,
    ) -> Self {
        Self {
            rid: if root {
                0
            } else {
                RID.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1
            },
            code,
            captured,
            module,
            ip: 0,
            fp,
            initial_fp: fp,
        }
    }

    pub(crate) fn r#use(
        &mut self,
        code: Handle<Code>,
        captured: Option<Handle<Array<Handle<Upvalue>>>>,
        fp: usize,
    ) {
        self.code = code;
        self.captured = captured;
        self.ip = 0;
        self.fp = fp;
    }

    pub fn top_level(&self) -> bool {
        self.fp == self.initial_fp
    }

    pub fn is_initialized(&self) -> bool {
        self.rid == 0 && self.code.instructions.len() > 0
    }
}

impl Object for Registers {
    fn trace(&self, visitor: &mut dyn Visitor) {
        self.code.trace(visitor);
        self.captured.trace(visitor);
        self.module.trace(visitor);
    }
}

/// Saved state of a thread per continuation.
pub struct SavedState {
    stack: ArrayList<Value>,
    sp: usize,
    registers: Registers,
    pub(crate) winders: Option<Handle<Winder>>,
}

impl Object for SavedState {
    fn trace(&self, visitor: &mut dyn Visitor) {
        self.stack.trace(visitor);
        self.registers.trace(visitor);
        if let Some(winders) = &self.winders {
            winders.trace(visitor);
        }
    }
}

impl Allocation for SavedState {}

impl Display for SavedState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Saved Context State:")?;
        writeln!(f, "  Stack(sp={}):", self.sp)?;
        for (i, v) in self.stack.iter().enumerate() {
            writeln!(f, "    {}: {}", i, v.to_string(false))?
        }
        writeln!(f, "  Registers:")?;
        writeln!(f, "    ip: {}", self.registers.ip)?;
        writeln!(f, "    fp: {}", self.registers.fp)?;
        writeln!(f, "    rid: {}", self.registers.rid)?;
        writeln!(f, "    code: {:p}", self.registers.code)?;
        Ok(())
    }
}

static WINDER_ID: AtomicI32 = AtomicI32::new(i32::MIN);

pub struct Winder {
    pub before: Handle<Procedure>,
    pub after: Handle<Procedure>,
    pub handlers: Option<Value>,
    pub next: Option<Handle<Winder>>,
    id: i32,
}

impl Winder {
    pub fn new(
        before: Handle<Procedure>,
        after: Handle<Procedure>,
        handlers: Option<Value>,
        next: Option<Handle<Winder>>,
    ) -> Self {
        Self {
            before,
            after,
            handlers,
            next,
            id: WINDER_ID.fetch_add(1, std::sync::atomic::Ordering::AcqRel),
        }
    }

    pub fn id(&self) -> i32 {
        self.id
    }

    pub fn count(&self) -> usize {
        let mut count = 0;
        let mut current = self;
        while let Some(next) = &current.next {
            count += 1;
            current = next;
        }
        count
    }

    pub fn common_prefix(
        this: Handle<Winder>,
        with: Option<Handle<Winder>>,
    ) -> Option<Handle<Winder>> {
        if let Some(with) = with {
            let mut this = this;
            let mut that = with;

            let this_len = this.count();
            let that_len = that.count();

            if this_len > that_len {
                for _ in 0..(this_len - that_len) {
                    this = this.next.unwrap();
                }
            } else if that_len > this_len {
                for _ in 0..(that_len - this_len) {
                    that = that.next.unwrap();
                }
            }

            loop {
                #[allow(irrefutable_let_patterns)]
                if let (this_winder, that_winder) = (this, that) {
                    if Handle::ptr_eq(&this_winder, &that_winder) {
                        return Some(this_winder);
                    } else {
                        this = this_winder.next.unwrap();
                        that = that_winder.next.unwrap();
                    }
                }
            }
        } else {
            None
        }
    }
}

impl Object for Winder {
    fn trace(&self, visitor: &mut dyn Visitor) {
        self.before.trace(visitor);
        self.after.trace(visitor);
        self.handlers.trace(visitor);
        self.next.trace(visitor);
    }
}

impl Allocation for Winder {}

impl PartialEq for Winder {
    fn eq(&self, other: &Self) -> bool {
        self as *const Self == other as *const Self
    }
}

impl Eq for Winder {}

impl Hash for Winder {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const Self).hash(state);
    }
}

impl UnwindSafe for Context {}
impl RefUnwindSafe for Context {}
