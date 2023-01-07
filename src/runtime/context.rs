#![allow(dead_code)]
use std::{
    cell::Cell,
    hash::Hash,
    intrinsics::likely,
    panic::{AssertUnwindSafe, RefUnwindSafe, UnwindSafe},
    ptr::null_mut,
    sync::atomic::AtomicUsize,
};

use r7rs_parser::{parser::{ParseError, Parser}, expr::NoIntern};
use rsgc::heap::root_processor::ThreadRootProcessor;

use crate::{
    data::exception::{Exception, SourcePosition},
    prelude::*,
    utilities::arraylist::ArrayList, compiler::r7rs_to_value,
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
    registers: Registers,
    winders: Option<Handle<Winder>>,
    rt: &'static mut Runtime,
    limit_sp: usize,
    thread: &'static mut Thread,

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
        let captured = rt.empty_array;
        let mut stack = Vec::with_capacity(512);
        stack.resize(512, Value::UNDEFINED);
        let this = Box::leak(Box::new(Self {
            registers: Registers::new(code, library_manager().null_module, captured, 0, true),
            winders: None,
            rt,
            limit_sp,
            thread,
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

    pub fn parse_exprs(&mut self, path: &str, fold_case: bool) -> Result<ArrayList<Value>, Handle<Exception>> {
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
                            exprs.push(self.mutator(), val);
                        }
                        Err(ParseError::Syntax(position, error)) => {
                            let exc = Exception::syntax(self, error, &[], SourcePosition {
                                position,
                                source_id: source_id as _ 
                            });

                            return Err(exc);
                        }

                        Err(ParseError::Lexical(position, error)) => {
                            let exc = Exception::lexical(self, error, &[], SourcePosition {
                                position,
                                source_id: source_id as _ 
                            });

                            return Err(exc);
                        }
                    }
                }

                return Ok(exprs);
            }
            Err(_) => {
                todo!("error");
            }
        }
    }

    pub fn eval_path(&mut self, path: &str, fold_case: bool) -> Result<Value, Handle<Exception>> {
        let exprs = self.parse(path, fold_case)?;

        let source_dir = std::path::Path::new(path).parent().unwrap();

        let loader = self.runtime().loader;
        let s = Str::new(self.mutator(), source_dir.display().to_string());
        let lib = library_manager().user_module;
        let list = Value::make_list_slice(self, &[exprs, Value::new(s), lib], Value::nil());
        self.apply(loader, list)
    }

    pub fn eval_path_in(&mut self, path: &str, fold_case: bool, lib: Handle<Library>) -> Result<Value, Handle<Exception>> {
        let exprs = self.parse(path, fold_case)?;

        let source_dir = std::path::Path::new(path).parent().unwrap();

        let loader = self.runtime().loader;
        let s = Str::new(self.mutator(), source_dir.display().to_string());
        let lib = Value::new(lib);
        let list = Value::make_list_slice(self, &[exprs, Value::new(s), lib], Value::nil());
        self.apply(loader, list)
    }



    #[inline(always)]
    pub fn push(&mut self, v: Value) {
        if self.sp < self.stack.len() {
            unsafe {
                *self.stack.get_unchecked_mut(self.sp as usize) = v;
            }
        } else {
            self.push_slow(v);
        }
        self.sp += 1;
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
    fn push_slow(&mut self, v: Value) {
        self.stack.push(v);

        if self.sp >= self.limit_sp {
            let exc = Exception::eval(
                self,
                EvalError::StackOverflow,
                &[],
                SourcePosition::unknown(),
            );
            self.error(exc);
        }
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
        for i in 0..self.sp {
            let v = unsafe { *self.stack.get_unchecked(i as usize) };
            v.trace(visitor);
        }
        self.registers.code.trace(visitor);
        self.registers.captured.trace(visitor);
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
    fn push_arguments(&mut self, arglist: Value) -> usize {
        let mut args = arglist;
        let mut n = 0;
        while args.is_pair() {
            let arg = args.car();
            self.push(arg);
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
            self.error(exc);
        }

        n
    }

    pub fn try_catch<R>(
        &mut self,
        closure: impl FnOnce(&mut Self) -> R,
    ) -> Result<R, Handle<Exception>> {
        let res = std::panic::catch_unwind(AssertUnwindSafe(|| {
            let res = closure(self);
            res
        }));

        res.map_err(|err| {
            if let Some(exc) = err.downcast_ref::<Handle<Exception>>() {
                exc.clone()
            } else {
                std::panic::resume_unwind(err);
            }
        })
    }

    pub fn try_finally<T, R>(
        &mut self,
        data: &mut T,
        closure: impl FnOnce(&mut Self, &mut T) -> R,
        finally: impl FnOnce(&mut Self, &mut T),
    ) -> R {
        let res = std::panic::catch_unwind(AssertUnwindSafe(|| {
            let res = closure(self, data);
            res
        }));

        let finally_res = std::panic::catch_unwind(AssertUnwindSafe(|| {
            finally(self, data);
        }));

        let val = res.unwrap_or_else(|err| {
            if let Some(exc) = err.downcast_ref::<Handle<Exception>>() {
                self.error(exc.clone());
            } else {
                std::panic::resume_unwind(err);
            }
        });

        finally_res.unwrap_or_else(|err| {
            if let Some(exc) = err.downcast_ref::<Handle<Exception>>() {
                self.error(exc.clone());
            } else {
                std::panic::resume_unwind(err);
            }
        });

        val
    }

    pub fn apply(&mut self, fun: Value, args: Value) -> Result<Value, Handle<Exception>> {
        self.try_catch(|ctx| unsafe {
            ctx.push(fun);

            let mut n = ctx.push_arguments(args); // throws

            let proc = ctx.invoke(&mut n, 1); // throws

            match proc.kind {
                ProcedureKind::Closure(_, _, ref captured, ref code) => {
                    ctx.execute_catch(*code, n, *captured) // throws
                }
                ProcedureKind::RawContinuation(_) => {
                    ctx.execute() // throws
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
                        ctx.error(exc);
                    }
                    let arg = ctx.pop();
                    let res = rules.expand(ctx, arg);

                    ctx.pop();
                    res
                }
                // native function executed
                _ => ctx.pop(),
            }
        })
    }

    fn invoke(&mut self, n: &mut usize, overhead: usize) -> Handle<Procedure> {
        let p = self.stack[self.sp - *n - 1];

        if !p.is_handle_of::<Procedure>() {
            let exc = Exception::eval(
                self,
                EvalError::NonApplicativeValue,
                &[p],
                SourcePosition::unknown(),
            );
            self.error(exc);
        }

        let mut proc = p.get_handle_of::<Procedure>();

        let res = AssertUnwindSafe(|| loop {
            if let ProcedureKind::Primitive(_, ref imp, _) = proc.kind {
                match imp {
                    Implementation::Eval(eval) => {
                        let this = unsafe { &mut *(self as *const Self as *mut Self) };
                        let generated = eval(self, &this.stack[this.sp - *n..this.sp]);
                        self.popn(*n + 1);

                        let proc = Procedure {
                            kind: ProcedureKind::Closure(
                                ClosureType::Anonymous,
                                Value::nil(),
                                self.runtime().empty_array,
                                generated,
                            ),
                            id: Procedure::new_id(),
                            module: generated.module,
                        };

                        let proc = self.mutator().allocate(proc);
                        self.push(Value::new(proc));

                        *n = 0;
                        return Some(proc);
                    }
                    Implementation::Apply(apply) => {
                        let this = unsafe { &mut *(self as *const Self as *mut Self) };
                        let (next, args) = apply(self, &this.stack[this.sp - *n..this.sp]);
                        self.popn(*n + 1);
                        self.push(Value::new(next));
                        for i in 0..args.len() {
                            self.push(args[i]);
                        }
                        *n = args.len();
                        proc = next;
                        continue;
                    }
                    Implementation::Native0(exec) => {
                        if *n != 0 {
                            let args = self.pop_as_list(*n as _);
                            let exc = Exception::argument_count(
                                self,
                                None,
                                0,
                                0,
                                args,
                                SourcePosition::unknown(),
                            );
                            self.error(exc);
                        }

                        self.popn(overhead);
                        let res = exec(self);
                        self.stack.push(res);
                        *n = 0;
                        return Some(proc);
                    }

                    Implementation::Native1(exec) => {
                        if *n != 1 {
                            let args = self.pop_as_list(*n as _);
                            let exc = Exception::argument_count(
                                self,
                                None,
                                1,
                                1,
                                args,
                                SourcePosition::unknown(),
                            );
                            self.error(exc);
                        }

                        self.popn(overhead);
                        let arg = self.pop();
                        let res = exec(self, arg);
                        self.stack.push(res);
                        *n = 0;
                        return Some(proc);
                    }

                    Implementation::Native2(exec) => {
                        if *n != 2 {
                            let args = self.pop_as_list(*n as _);
                            let exc = Exception::argument_count(
                                self,
                                None,
                                2,
                                2,
                                args,
                                SourcePosition::unknown(),
                            );
                            self.error(exc);
                        }

                        self.popn(overhead);
                        let arg2 = self.pop();
                        let arg1 = self.pop();
                        let res = exec(self, arg1, arg2);
                        self.stack.push(res);
                        *n = 0;
                        return Some(proc);
                    }

                    Implementation::Native3(exec) => {
                        if *n != 3 {
                            let args = self.pop_as_list(*n as _);
                            let exc = Exception::argument_count(
                                self,
                                None,
                                3,
                                3,
                                args,
                                SourcePosition::unknown(),
                            );
                            self.error(exc);
                        }

                        self.popn(overhead);
                        let arg3 = self.pop();
                        let arg2 = self.pop();
                        let arg1 = self.pop();
                        let res = exec(self, arg1, arg2, arg3);
                        self.stack.push(res);
                        *n = 0;
                        return Some(proc);
                    }

                    Implementation::Native4(exec) => {
                        if *n != 4 {
                            let args = self.pop_as_list(*n as _);
                            let exc = Exception::argument_count(
                                self,
                                None,
                                4,
                                4,
                                args,
                                SourcePosition::unknown(),
                            );
                            self.error(exc);
                        }

                        self.popn(overhead);
                        let arg4 = self.pop();
                        let arg3 = self.pop();
                        let arg2 = self.pop();
                        let arg1 = self.pop();
                        let res = exec(self, arg1, arg2, arg3, arg4);
                        self.stack.push(res);
                        *n = 0;
                        return Some(proc);
                    }

                    Implementation::Native0O(exec) => {
                        if *n == 0 {
                            self.popn(overhead as _);
                            let res = exec(self, None);
                            self.stack.push(res);
                        } else if *n == 1 {
                            let arg = self.pop();
                            self.popn(overhead);
                            let res = exec(self, Some(arg));
                            self.stack.push(res);
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
                            self.error(exc);
                        }
                        return Some(proc);
                    }

                    Implementation::Native1O(exec) => {
                        if *n == 1 {
                            let arg = self.pop();
                            self.popn(overhead);
                            let res = exec(self, arg, None);
                            self.stack.push(res);
                        } else if *n == 2 {
                            let arg2 = self.pop();
                            let arg1 = self.pop();
                            self.popn(overhead);
                            let res = exec(self, arg1, Some(arg2));
                            self.stack.push(res);
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
                            self.error(exc);
                        }
                        return Some(proc);
                    }

                    Implementation::Native2O(exec) => {
                        if *n == 2 {
                            let arg2 = self.pop();
                            let arg1 = self.pop();
                            self.popn(overhead);
                            let res = exec(self, arg1, arg2, None);
                            self.stack.push(res);
                        } else if *n == 3 {
                            let arg3 = self.pop();
                            let arg2 = self.pop();
                            let arg1 = self.pop();
                            self.popn(overhead);
                            let res = exec(self, arg1, arg2, Some(arg3));
                            self.stack.push(res);
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
                            self.error(exc);
                        }
                        return Some(proc);
                    }

                    Implementation::Native3O(exec) => {
                        if *n == 3 {
                            let arg3 = self.pop();
                            let arg2 = self.pop();
                            let arg1 = self.pop();
                            self.popn(overhead);
                            let res = exec(self, arg1, arg2, arg3, None);
                            self.stack.push(res);
                        } else if *n == 4 {
                            let arg4 = self.pop();
                            let arg3 = self.pop();
                            let arg2 = self.pop();
                            let arg1 = self.pop();
                            self.popn(overhead);
                            let res = exec(self, arg1, arg2, arg3, Some(arg4));
                            self.stack.push(res);
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
                            self.error(exc);
                        }
                        return Some(proc);
                    }

                    Implementation::Native0OO(exec) => {
                        if *n == 0 {
                            self.popn(overhead as _);
                            let res = exec(self, None, None);
                            self.stack.push(res);
                        } else if *n == 1 {
                            let arg = self.pop();
                            self.popn(overhead);
                            let res = exec(self, Some(arg), None);
                            self.stack.push(res);
                        } else if *n == 2 {
                            let arg2 = self.pop();
                            let arg1 = self.pop();
                            self.popn(overhead);
                            let res = exec(self, Some(arg1), Some(arg2));
                            self.stack.push(res);
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
                            self.error(exc);
                        }
                        return Some(proc);
                    }

                    Implementation::Native1OO(exec) => {
                        if *n == 1 {
                            let arg = self.pop();
                            self.popn(overhead);
                            let res = exec(self, arg, None, None);
                            self.stack.push(res);
                        } else if *n == 2 {
                            let arg2 = self.pop();
                            let arg1 = self.pop();
                            self.popn(overhead);
                            let res = exec(self, arg1, Some(arg2), None);
                            self.stack.push(res);
                        } else if *n == 3 {
                            let arg3 = self.pop();
                            let arg2 = self.pop();
                            let arg1 = self.pop();
                            self.popn(overhead);
                            let res = exec(self, arg1, Some(arg2), Some(arg3));
                            self.stack.push(res);
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
                            self.error(exc);
                        }

                        return Some(proc);
                    }

                    Implementation::Native2OO(exec) => {
                        if *n == 2 {
                            let arg2 = self.pop();
                            let arg1 = self.pop();
                            self.popn(overhead);
                            let res = exec(self, arg1, arg2, None, None);
                            self.stack.push(res);
                        } else if *n == 3 {
                            let arg3 = self.pop();
                            let arg2 = self.pop();
                            let arg1 = self.pop();
                            self.popn(overhead);
                            let res = exec(self, arg1, arg2, Some(arg3), None);
                            self.stack.push(res);
                        } else if *n == 4 {
                            let arg4 = self.pop();
                            let arg3 = self.pop();
                            let arg2 = self.pop();
                            let arg1 = self.pop();
                            self.popn(overhead);
                            let res = exec(self, arg1, arg2, Some(arg3), Some(arg4));
                            self.stack.push(res);
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
                            self.error(exc);
                        }

                        return Some(proc);
                    }

                    Implementation::Native3OO(exec) => {
                        if *n == 3 {
                            let arg3 = self.pop();
                            let arg2 = self.pop();
                            let arg1 = self.pop();
                            self.popn(overhead);
                            let res = exec(self, arg1, arg2, arg3, None, None);
                            self.stack.push(res);
                        } else if *n == 4 {
                            let arg4 = self.pop();
                            let arg3 = self.pop();
                            let arg2 = self.pop();
                            let arg1 = self.pop();
                            self.popn(overhead);
                            let res = exec(self, arg1, arg2, arg3, Some(arg4), None);
                            self.stack.push(res);
                        } else if *n == 5 {
                            let arg5 = self.pop();
                            let arg4 = self.pop();
                            let arg3 = self.pop();
                            let arg2 = self.pop();
                            let arg1 = self.pop();
                            self.popn(overhead);
                            let res = exec(self, arg1, arg2, arg3, Some(arg4), Some(arg5));
                            self.stack.push(res);
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
                            self.error(exc);
                        }

                        return Some(proc);
                    }

                    Implementation::Native0R(exec) => {
                        let this = unsafe { &mut *(self as *const Self as *mut Self) };
                        let args = &self.stack[(self.sp - *n)..self.sp];
                        let res = exec(this, args);
                        self.popn(*n as usize + overhead as usize);
                        self.stack.push(res);

                        return Some(proc);
                    }

                    Implementation::Native1R(exec) => {
                        let this = unsafe { &mut *(self as *const Self as *mut Self) };
                        if *n >= 1 {
                            let arg0 = self.stack[self.sp - *n];
                            let args = &self.stack[(self.sp - *n + 1)..self.sp];

                            let res = exec(this, arg0, args);
                            self.popn(*n as usize + overhead as usize);
                            self.stack.push(res);
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
                            self.error(exc);
                        }

                        return Some(proc);
                    }

                    Implementation::Native2R(exec) => {
                        let this = unsafe { &mut *(self as *const Self as *mut Self) };
                        if *n >= 2 {
                            let arg0 = self.stack[self.sp - *n];
                            let arg1 = self.stack[self.sp - *n + 1];
                            let args = &self.stack[(self.sp - *n + 2)..self.sp];
                            let res = exec(this, arg0, arg1, args);
                            self.popn(*n as usize + overhead as usize);
                            self.stack.push(res);
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
                            self.error(exc);
                        }

                        return Some(proc);
                    }

                    Implementation::Native3R(exec) => {
                        let this = unsafe { &mut *(self as *const Self as *mut Self) };
                        if *n >= 3 {
                            let arg0 = self.stack[self.sp - *n];
                            let arg1 = self.stack[self.sp - *n + 1];
                            let arg2 = self.stack[self.sp - *n + 2];
                            let args = &self.stack[(self.sp - *n + 3)..self.sp];

                            let res = exec(this, arg0, arg1, arg2, args);
                            self.popn(*n as usize + overhead as usize);
                            self.stack.push(res);
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
                            self.error(exc);
                        }
                        return Some(proc);
                    }
                }
            } else {
                return None;
            }
        });

        let result = std::panic::catch_unwind(|| res());

        match result {
            Ok(exec) => {
                if let Some(proc) = exec {
                    return proc;
                } else {
                    match proc.kind {
                        ProcedureKind::RawContinuation(ref vm_state) => {
                            if vm_state.registers.rid != self.registers.rid {
                                let exc = Exception::eval(
                                    self,
                                    EvalError::IllegalContinuationApplication,
                                    &[
                                        Value::new(proc),
                                        Value::encode_int32(self.registers.rid as _),
                                    ],
                                    SourcePosition::unknown(),
                                );
                                self.error(exc);
                            }

                            if *n != 1 {
                                let args = self.pop_as_list(*n as _);
                                let exc = Exception::argument_count(
                                    self,
                                    None,
                                    1,
                                    1,
                                    args,
                                    SourcePosition::unknown(),
                                );
                                self.error(exc);
                            }

                            let _arg = self.stack.pop();
                            // todo: push identity function and resume execution
                            self.restore_state(&vm_state);
                        }
                        _ => return proc,
                    }
                }
            }

            Err(mut e) => {
                if let Some(e) = e.downcast_mut::<Handle<Exception>>() {
                    e.attach_ctx(self, Some(proc));
                    self.error(*e);
                } else {
                    std::panic::resume_unwind(e);
                }
            }
        }

        todo!()
    }

    fn restore_state(&mut self, state: &SavedState) {
        {
            self.stack.clear();
            self.stack.extend_from_slice(&state.stack);
            self.sp = state.sp;
            self.registers.captured = state.registers.captured;
            self.registers.fp = state.registers.fp;
            self.registers.ip = state.registers.ip;
            self.registers.initial_fp = state.registers.initial_fp;
            self.registers.code = state.registers.code;
            self.registers.module = state.registers.module;
        }
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

    fn capture(&mut self, n: usize) -> Handle<Array<Value>> {
        let mut list = ArrayList::new(self.mutator());
        let mut i = self.sp - n;

        while i < self.sp {
            let val = self.stack[i];
            list.push(self.mutator(), val);
            i += 1;
        }

        self.sp -= n;
        Array::new(self.thread, list.len(), |_, ix| list[ix])
    }

    pub(crate) unsafe fn execute_named(&mut self, code: Handle<Code>, name: Handle<Str>) -> Value {
        let empty = self.runtime().empty_array;
        let proc = self.mutator().allocate(Procedure {
            id: Procedure::new_id(),
            kind: ProcedureKind::Closure(ClosureType::Named(name), Value::UNDEFINED, empty, code),
            module: code.module,
        });

        self.push(Value::new(proc));

        self.execute_catch(code, 0, empty)
    }

    pub(crate) unsafe fn execute_unnamed(&mut self, code: Handle<Code>) -> Value {
        let empty = self.runtime().empty_array;
        let proc = self.mutator().allocate(Procedure {
            id: Procedure::new_id(),
            kind: ProcedureKind::Closure(ClosureType::Anonymous, Value::UNDEFINED, empty, code),
            module: code.module,
        });

        self.push(Value::new(proc));

        self.execute_catch(code, 0, empty)
    }

    pub(crate) unsafe fn execute_catch(
        &mut self,
        code: Handle<Code>,
        args: usize,
        captured: Handle<Array<Value>>,
    ) -> Value {
        let saved_registers = self.registers;
        self.registers = Registers::new(
            code,
            Value::new(code.module),
            captured,
            self.sp - args,
            !saved_registers.is_initialized(),
        );

        let result = self.try_catch(|ctx| ctx.execute());

        match result {
            Ok(value) => {
                self.registers = saved_registers;
                value
            }
            Err(mut err) => {
                self.registers = saved_registers;
                err.attach_ctx(self, None);
                self.error(err);
            }
        }
    }

    pub(crate) unsafe fn execute(&mut self) -> Value {
        loop {
            let ip = self.registers.ip;
            self.registers.ip = ip + 1;
            debug_assert!(ip < self.registers.code.instructions.len());
            match *self.registers.code.instructions.get_unchecked(ip) {
                Ins::NoOp => continue,
                Ins::Pop => {
                    self.stack.pop();
                }
                Ins::Dup => {
                    let top = self.top();
                    self.push(top);
                }
                Ins::PushVoid => {
                    self.push(Value::void());
                }
                Ins::PushConstant(c) => {
                    self.push(self.registers.code.constants[c as usize]);
                }

                Ins::PushFixnum(n) => {
                    self.push(Value::new(n));
                }

                Ins::PushEof => {
                    self.push(Value::eof());
                }

                Ins::PushTrue => {
                    self.push(Value::new(true));
                }

                Ins::PushFalse => {
                    self.push(Value::new(false));
                }

                Ins::PushNull => {
                    self.push(Value::nil());
                }

                Ins::PushLocal(l) => {
                    let val = self.stack[self.registers.fp + l as usize];
                    self.push(val);
                }

                Ins::SetLocal(l) => {
                    let val = self.pop();
                    self.stack[self.registers.fp + l as usize] = val;
                }

                Ins::PushCaptured(l) => {
                    let val = self.registers.captured[l as usize];
                    self.push(val);
                }

                Ins::SetCaptured(l) => {
                    let val = self.pop();
                    self.thread.write_barrier(self.registers.captured);
                    self.registers.captured[l as usize] = val;
                }

                Ins::MakeVariableArgument(_) => {}
                Ins::MakeLocalVariable(_) => {}

                Ins::Pack(n) => {
                    let mut list = Value::nil();

                    for _ in 0..n {
                        let val = self.pop();
                        list = self.make_pair(val, list);
                    }

                    let values = self.mutator().allocate(Values(list));
                    self.push(Value::new(values));
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
                                self.push(Value::nil());
                            }
                        } else {
                            let exc = Exception::eval(
                                self,
                                EvalError::MultiValueCountError,
                                &[Value::new(n as i32), Value::nil()],
                                SourcePosition::unknown(),
                            );

                            self.error(exc);
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

                                    self.error(exc);
                                }
                            }

                            self.push(value);
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

                            self.error(exc);
                        }

                        if overflow {
                            self.push(next);
                        }
                    } else {
                        if n == 1 {
                            if overflow {
                                self.push(Value::nil());
                            }
                        } else if n == 0 && overflow {
                            let val = self.pop();
                            let pair = self.make_pair(val, Value::nil());
                            self.push(pair);
                        } else {
                            let pair = self.make_pair(self.top(), Value::nil());
                            let exc = Exception::eval(
                                self,
                                EvalError::MultiValueCountError,
                                &[Value::new(n as i32), pair],
                                SourcePosition::unknown(),
                            );

                            self.error(exc);
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
                            captured,
                            self.registers.code.fragments[index as usize],
                        ),
                    };

                    let proc = self.mutator().allocate(proc);

                    self.push(Value::new(proc));
                }

                Ins::PushGlobal(ix) => {
                    let mut v = self.registers.code.constants[ix as usize];

                    if likely(v.is_handle_of::<Gloc>()) {
                        v = Gloc::get(self, v.get_handle_of::<Gloc>());
                    } else {
                        let mut gloc = None;
                        v = self.global_ref(v.get_handle_of::<Symbol>(), &mut gloc);

                        if let Some(gloc) = gloc {
                            let thread = Thread::current();
                            self.registers
                                .code
                                .constants
                                .set(thread, ix as _, Value::new(gloc));
                        }
                    }

                    self.push(v);
                }

                Ins::SetGlobal(ix) => {
                    let value = self.pop();

                    let gloc = self.registers.code.constants[ix as usize];
                    if likely(gloc.is_handle_of::<Gloc>()) {
                        self.thread.write_barrier(gloc.get_handle());
                        Gloc::set(self, gloc.get_handle_of::<Gloc>(), value);
                    } else {
                        let mut gloc_ = None;
                        self.global_set(gloc.get_handle_of::<Symbol>(), value, &mut gloc_);

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
                    let sym = self.registers.code.constants[ix as usize].get_handle_of::<Symbol>();

                    if constant {
                        lm.define_const( self.registers.module.get_handle_of(), sym, value);
                    } else {
                        lm.define( self.registers.module.get_handle_of(), sym, value);
                    }
                }

                Ins::MakeFrame => {
                    self.push(Value::encode_int32(self.registers.fp as i32));
                    self.push(Value::UNDEFINED);
                }

                Ins::InjectFrame => {
                    let top = self.top();
                    self.push(Value::encode_int32(self.registers.fp as i32));
                    self.push(Value::UNDEFINED);
                    self.push(top);
                }

                Ins::Alloc(n) => {
                    if self.sp + n as usize > self.stack.len() {
                        if self.sp + n as usize > self.limit_sp {
                            let exc = Exception::eval(
                                self,
                                EvalError::StackOverflow,
                                &[],
                                SourcePosition::unknown(),
                            );
                            self.error(exc);
                        }

                        for _ in 0..(self.sp + n as usize - self.stack.len()) {
                            self.push(Value::UNDEFINED);
                        }
                    }
                    self.sp += n as usize;
                }

                Ins::AllocBelow(n) => {
                    let top = self.pop();

                    if self.sp + n as usize > self.stack.len() {
                        if self.sp + n as usize > self.limit_sp {
                            let exc = Exception::eval(
                                self,
                                EvalError::StackOverflow,
                                &[],
                                SourcePosition::unknown(),
                            );
                            self.error(exc);
                        }

                        for _ in 0..(self.sp + n as usize - self.stack.len()) {
                            self.push(Value::UNDEFINED);
                        }
                    }

                    self.sp += n as usize;
                    self.push(top);
                }

                Ins::CollectRest(n) => {
                    let mut rest = Value::nil();

                    while self.sp > self.registers.fp + n as usize {
                        let val = self.pop();
                        rest = self.make_pair(val, rest);
                    }

                    self.push(rest);
                }

                Ins::Call(n) => {
                    self.thread.safepoint();
                    self.stack[self.sp - n as usize - 2] =
                        Value::encode_int32(self.registers.ip as _);
                    for i in 0..self.sp {
                        println!("#{}: {}", self.sp, self.stack[i].to_string(false));
                    }
                    let mut m = n as usize;

                    if let ProcedureKind::Closure(_, _, ref newcaptured, ref newcode) =
                        self.invoke(&mut m, 3).kind
                    {
                        self.registers.r#use(*newcode, *newcaptured, self.sp - m);
                    }
                }

                Ins::TailCall(m) => {
                    self.thread.safepoint();
                    let mut n = m as usize;

                    let proc = self.invoke(&mut n, 1);

                    match proc.kind {
                        ProcedureKind::Closure(_, _, ref newcaptured, ref newcode) => {
                            self.registers
                                .r#use(*newcode, *newcaptured, self.registers.fp);

                            for i in 0..n {
                                self.stack[self.registers.fp - 1 + i] =
                                    self.stack[self.sp - n - 1 + i];
                            }

                            self.sp = self.registers.fp + n;
                        }
                        ProcedureKind::RawContinuation(_) => break,
                        _ => {
                            if self.registers.top_level() {
                                let res = self.pop();

                                self.sp = self.registers.initial_fp - 1;
                                return res;
                            } else {
                                self.exit_frame();
                            }
                        }
                    }
                }

                Ins::Return => {
                    if self.registers.top_level() {
                        let res = self.pop();

                        self.sp = self.registers.initial_fp - 1;
                        return res;
                    } else {
                        self.exit_frame();
                    }
                }

                Ins::Branch(offset) => {
                    self.registers.ip = (self.registers.ip as i32 + offset) as usize;
                }

                Ins::BranchIf(offset) => {
                    let val = self.pop();
                    if !val.is_false() {
                        self.registers.ip = (self.registers.ip as i32 + offset) as usize;
                    }
                }

                Ins::BranchIfNot(offset) => {
                    let val = self.pop();
                    if val.is_false() {
                        self.registers.ip = (self.registers.ip as i32 + offset) as usize;
                    }
                    self.thread.safepoint();
                }

                Ins::KeepOrBranchIfNot(offset) => {
                    let top = self.pop();
                    if top.is_false() {
                        self.registers.ip = (self.registers.ip as i32 + offset) as usize;
                    } else {
                        self.push(top);
                    }
                    self.thread.safepoint();
                }

                Ins::BranchIfArgMismatch(offset, n) => {
                    if self.sp - n as usize != self.registers.fp {
                        self.registers.ip = (self.registers.ip as i32 + offset) as usize;
                    }
                    self.thread.safepoint();
                }

                Ins::BranchIfMinArgMismatch(offset, n) => {
                    if (self.sp - n as usize) < self.registers.fp {
                        self.registers.ip = (self.registers.ip as i32 + offset) as usize;
                    }
                    self.thread.safepoint();
                }

                ins => todo!("{:?}", ins),
            }
        }
        Value::nil()
    }

    fn exit_frame(&mut self) {
        let fp = self.registers.fp;

        if !self.stack[fp - 2].is_int32() {
            unreachable!("malformed stack");
        }

        self.registers.ip = self.stack[fp - 2].get_int32() as _;

        if !self.stack[fp - 3].is_int32() {
            unreachable!("malformed stack");
        }
        let newfp = self.stack[fp - 3].get_int32() as usize;

        self.stack[fp - 3] = self.stack[self.sp - 1];

        self.sp = fp - 2;
        self.registers.fp = newfp as _;

        let proc = self.stack[newfp as usize - 1];

        if !proc.is_handle_of::<Procedure>() {
            unreachable!("malformed stack: procedure expected");
        }

        if let ProcedureKind::Closure(_, _, ref newcaptured, ref newcode) =
            proc.get_handle_of::<Procedure>().kind
        {
            self.registers.captured = *newcaptured;
            self.registers.code = *newcode;
        } else {
            unreachable!("trying to return to non-VM function");
        }
    }

    fn global_ref(&mut self, id: Handle<Symbol>, gloc_out: &mut Option<Handle<Gloc>>) -> Value {
        let binding = library_manager().find_binding(
            self.registers.module.get_handle_of::<Library>(),
            id,
            false,
            false,
        );

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

            self.error(exc);
        }
    }

    fn global_set(
        &mut self,
        id: Handle<Symbol>,
        value: Value,
        gloc_out: &mut Option<Handle<Gloc>>,
    ) {
        let binding = library_manager().find_binding(
            self.registers.module.get_handle_of::<Library>(),
            id,
            false,
            false,
        );

        if let Some(binding) = binding {
            *gloc_out = Some(binding);
            Gloc::set(self, binding, value);
        } else {
            let exc = Exception::custom(
                self,
                "evaluation error",
                "unbound variable: $0",
                &[id.into()],
                SourcePosition::unknown(),
            );

            self.error(exc);
        }
    }

    pub fn error(&mut self, exception: Handle<Exception>) -> ! {
        std::panic::resume_unwind(Box::new(exception))
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
    module: Value,
    captured: Handle<Array<Value>>,
    ip: usize,
    fp: usize,
    initial_fp: usize,
}

static RID: AtomicUsize = AtomicUsize::new(0);

impl Registers {
    pub(crate) fn new(
        code: Handle<Code>,
        module: Value,
        captured: Handle<Array<Value>>,
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

    pub(crate) fn r#use(&mut self, code: Handle<Code>, captured: Handle<Array<Value>>, fp: usize) {
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
    }
}

/// Saved state of a thread per continuation.
pub struct SavedState {
    stack: ArrayList<Value>,
    sp: usize,
    registers: Registers,
    winders: Option<Handle<Winder>>,
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

pub struct Winder {
    before: Handle<Procedure>,
    after: Handle<Procedure>,
    handlers: Option<Value>,
    next: Option<Handle<Winder>>,
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
        }
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
