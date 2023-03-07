use std::{
    hint::unreachable_unchecked,
    intrinsics::{likely, unlikely}, ops::FromResidual, convert::Infallible,
};

use dashmap::DashMap;
use once_cell::sync::Lazy;
use rsgc::{
    heap::root_processor::SimpleRoot,
    prelude::{Handle, Object},
    sync::mutex::Mutex,
    system::array::Array,
    thread::Thread,
};

use crate::{
    error::{wrong_contract, wrong_count},
    fun::get_proc_name,
    jit::cranelift::JIT,
    raise_exn,
    util::arraylist::ArrayList,
    value::{
        ClosedPrimitiveProcedure, Hdr, NativeProcedure, PrimitiveProcedure, Type, Value,
        CHAR_CACHE, SCHEME_MAX_ARGS,
    }, ports_v2::{SCM_PORT_CODEC_NATIVE, SCM_PORT_EOL_STYLE_NATIVE, SCM_PORT_ERROR_HANDLING_MODE_REPLACE},
};

pub struct Runtime {
    symbol_table: SymbolTable,
    contexts: Mutex<*mut Vm>,
    pub empty_vector: Value,
    global_roots: Mutex<Vec<Value>>,
    pub jit: Mutex<JIT>,
    pub(crate) native_transcoder: Value,
}

impl Runtime {
    pub fn get() -> &'static Self {
        &*RUNTIME
    }

    pub fn add_global_root(&self, value: Value) {
        self.global_roots.lock(true).push(value);
    }

    pub fn native_transcoder(&self) -> Value {
        self.native_transcoder.clone()
    }
}

pub struct SymbolTable {
    interned: DashMap<&'static str, Value>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            interned: DashMap::new(),
        }
    }

    pub fn intern(&self, name: impl AsRef<str>) -> Value {
        let name = name.as_ref();
        if let Some(value) = self.interned.get(name) {
            value.value().clone()
        } else {
            let thread = Thread::current();
            let value = Value::make_uninterned_symbol(thread, name);
            value.downcast_symbol().uninterned = false;
            self.interned.insert(value.symbol_str(), value.clone());
            value
        }
    }

    pub fn mutator(&self) -> &'static mut Thread {
        Thread::current()
    }
    
}

impl Object for Runtime {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.symbol_table.interned.iter().for_each(|entry| {
            entry.value().trace(visitor);
        });

        self.native_transcoder.trace(visitor);

        unsafe {
            let mut vm = *self.contexts.unsafe_get();
            while !vm.is_null() {
                (*vm).trace(visitor);
                vm = (*vm).next;
            }

            let jit = self.jit.unsafe_get();
            jit.trace(visitor);

            let roots = self.global_roots.lock(false);

            for root in roots.iter() {
                root.trace(visitor);
            }
        }
    }
}

static RUNTIME: Lazy<Runtime> = Lazy::new(|| {
    rsgc::heap::heap::heap().add_root(SimpleRoot::new("runtime", "rt", |processor| {
        RUNTIME.trace(processor.visitor());
    }));
    let empty_vector = crate::value::Vector::new(Thread::current(), 0, Value::make_void()).into();
    let native_transcoder = Value::make_byte_vector(Thread::current(), 3, 0);
    native_transcoder.byte_vector_set(0, SCM_PORT_CODEC_NATIVE);
    native_transcoder.byte_vector_set(1, SCM_PORT_EOL_STYLE_NATIVE);
    native_transcoder.byte_vector_set(2, SCM_PORT_ERROR_HANDLING_MODE_REPLACE);
    Runtime {
        symbol_table: SymbolTable::new(),
        empty_vector,
        global_roots: Mutex::new(vec![]),
        contexts: Mutex::new(std::ptr::null_mut()),
        jit: Mutex::new(JIT::default()),
        native_transcoder
    }
});

pub fn intern(name: impl AsRef<str>) -> Value {
    Runtime::get().symbol_table.intern(name)
}

struct VmHandle(*mut Vm);

thread_local! {
    static VM: VmHandle = unsafe {
        let vm = Box::leak(Box::new(Vm::new()));
        let mut contexts = RUNTIME.contexts.lock(true);
        vm.next = *contexts;
        if !vm.next.is_null() {
            (*vm.next).prev = vm;
        }
        *contexts = vm;
        VmHandle(vm)
    };
}

pub fn vm() -> &'static mut Vm {
    VM.with(|vm| unsafe { &mut *vm.0 })
}

/// The VM is the main entry point for the interpreter. It is created
/// per machine thread that enters Scheme code.
///
#[repr(C)]
pub struct Vm {
    next: *mut Vm,
    prev: *mut Vm,
    pub(crate) mutator: &'static mut Thread,
    sp: usize,
    stack: Vec<Value>,
    pub(crate) tail_rands: ArrayList<Value>,
    pub(crate) tail_rator: Value,
}

pub static RETURN_CONT: Lazy<Value> = Lazy::new(|| {
    let vm = vm();

    vm.make_return_cont()
});

impl Vm {
    fn new() -> Self {
        let mutator = Thread::current();
        let mut stack = Vec::with_capacity(1024);
        stack.resize(1024, Value::make_void());
        Self {
            next: std::ptr::null_mut(),
            prev: std::ptr::null_mut(),
            tail_rands: ArrayList::with_capacity(mutator, 16),
            mutator: unsafe { std::mem::transmute::<&mut Thread, &'static mut Thread>(mutator) },
            sp: 0,
            stack,
            tail_rator: Value::make_void(),
        }
    }
    pub fn make_char(&mut self, c: char) -> Value {
        if c.is_ascii() {
            return CHAR_CACHE[c as usize];
        }
        Value::make_char(self.mutator(), c)
    }

    pub fn mutator<'a>(&'a mut self) -> &'a mut Thread {
        &mut self.mutator
    }

    pub fn tail_apply(&mut self, rator: Value, rands: &[Value]) -> Trampoline {
        self.tail_rator = rator;
        // Clean up from the previous tail call
        self.tail_rands.clear();
        self.tail_rands.write_barrier(Thread::current());
        for rand in rands {
            self.tail_rands.push(self.mutator, *rand);
        }

        Trampoline::TailCall(true)
    }

    pub fn tail_apply_cont(&mut self, k: Value, rator: Value, rands: &[Value]) -> Trampoline {
        self.tail_rator = rator;
        // Clean up from the previous tail call
        self.tail_rands.clear();
        self.tail_rands.push(self.mutator, k);
        for rand in rands {
            self.tail_rands.push(self.mutator, *rand);
        }

        Trampoline::TailCall(false)
    }

    #[inline]
    fn push(&mut self, value: Value) -> Result<(), Value> {
        if unlikely(self.sp >= self.stack.len()) {
            return raise_exn!(FailOutOfMemory, &[], "stack overflow");
        }
        unsafe {
            *self.stack.get_unchecked_mut(self.sp) = value;
        }
        self.sp += 1;
        Ok(())
    }

    #[allow(dead_code)]
    #[inline]
    fn pop(&mut self) {
        self.sp -= 1;
    }

    #[inline]
    fn popn(&mut self, n: usize) {
        self.sp -= n;
    }

    fn make_return_cont(&mut self) -> Value {
        Value::make_returncont(self.mutator)
    }

    pub fn apply(&mut self, rator: Value, rands: &[Value]) -> Result<Value, Value> {
        // push actual procedure
        self.push(rator)?;
        let k = *RETURN_CONT;
        // push `#<return-continuation>` object, on invocation just returns `Ok(rands[0])`
        self.push(k)?;
        // push arguments
        for i in 0..rands.len() {
            self.push(rands[i])?;
        }

        unsafe { self.invoke(rands.len() + 1) }
    }

    pub fn apply_with_continuation(
        &mut self,
        k: Value,
        rator: Value,
        rands: &[Value],
    ) -> Result<Value, Value> {
        // push actual procedure
        self.push(rator)?;
        // push continuation
        self.push(k)?;
        // push arguments
        for i in 0..rands.len() {
            self.push(rands[i])?;
        }

        unsafe { self.invoke(rands.len() + 1) }
    }

    /// Trampoline for invoking a procedure. It handles tail calls and continuations.
    ///
    /// Stack layout for invocation:
    /// ```text
    /// | ... | proc | k | arg1 | arg2 | ... | argn |
    /// ```
    ///
    /// When fucntion returns `Trampoline::Return` the continuaiton `k` is invoked with the result of the procedure.
    ///
    /// This continuation is a special object that just returns the result of the procedure in case of [apply](Vm::apply)
    pub(crate) unsafe fn invoke(&mut self, mut n: usize) -> Result<Value, Value> {
        'apply: loop {
            self.mutator.safepoint();
            let this = &mut *(self as *mut Self);

            let rator = this.stack[self.sp - n - 1];
            let rands = &this.stack[self.sp - n..self.sp];
            if unlikely(!rator.procedurep()) {
                return wrong_contract(
                    "apply",
                    "procedure?",
                    0,
                    n as i32,
                    &this.stack[self.sp - n - 1..self.sp],
                );
            }

            let mut num_rands = rands.len() as i32;

            let mina;
            let maxa;
            let adjust;
            if rator.primitive_procedurep() {
                adjust = 1;
                num_rands -= 1;
                let proc = rator.downcast_primitive_proc();
                mina = proc.mina;
                maxa = proc.maxa;
            } else if rator.closed_primitive_procedurep() {
                adjust = 1;
                num_rands -= 1;
                let proc = rator.downcast_closed_primitive_proc();
                mina = proc.mina;
                maxa = proc.maxa;
            } else if rator.get_type() == Type::ReturnCont {
                adjust = 0;
                mina = 1;
                maxa = 1;
            } else if rator.get_type() == Type::Parameter {
                adjust = 0;
                num_rands -= 1;
                mina = 0;
                maxa = 1;
            } else {
                adjust = 0;
                // the check is done in native code, so we just skip it here.
                mina = 0;
                maxa = SCHEME_MAX_ARGS;
            }

            if unlikely((num_rands as i32) < mina || (num_rands as i32 > maxa && mina >= 0)) {
                return wrong_count(
                    get_proc_name(rator).unwrap_or("#<procedure>"),
                    mina,
                    maxa,
                    num_rands as _,
                    &rands[adjust..],
                );
            }

            let k = rands[0];
            if likely(rator.nativeprocedurep()) {
                let proc = rator.downcast_native_proc();

                match self.invoke_native(proc, n) {
                    Trampoline::Throw(val) => {
                        self.popn(n + 1);
                        return Err(val);
                    }
                    Trampoline::TailCall(_) => {
                        self.popn(n + 1);
                        self.push(self.tail_rator)?;
                        for i in 0..self.tail_rands.len() {
                            self.push(*self.tail_rands.get_unchecked(i))?;
                        }
                        n = self.tail_rands.len();
                        continue 'apply;
                    }
                    _ => unreachable_unchecked(),
                }
            } else if rator.primitive_procedurep() {
                let proc = rator.downcast_primitive_proc().code;

                match proc(self, rands[0], &rands[1..]) {
                    Trampoline::Return(val) => {
                        // invoke the continuation
                        self.popn(n + 1);
                        self.push(k)?;
                        self.push(val)?;
                        n = 1;
                        continue 'apply;
                    }
                    Trampoline::Throw(val) => {
                        // clean up stack from rator + rands and return the error
                        self.popn(n + 1);
                        return Err(val);
                    }
                    Trampoline::TailCall(push_cont) => {
                        // clean up stack from rator + rands
                        self.popn(n + 1);
                        // push new rator and rands
                        self.push(self.tail_rator)?;
                        if push_cont {
                            self.push(k)?;
                        }
                        for i in 0..self.tail_rands.len() {
                            self.push(*self.tail_rands.get_unchecked(i))?;
                        }
                        n = self.tail_rands.len() + push_cont as usize;
                        continue 'apply;
                    }
                }
            } else if rator.closed_primitive_procedurep() {
                let rator = rator.downcast_closed_primitive_proc();

                match (rator.code)(
                    self,
                    rands[0],
                    &rands[1..],
                    rator.captures,
                    rator.name.strsym(),
                ) {
                    Trampoline::Return(val) => {
                        // invoke the continuation
                        self.popn(n + 1);
                        self.push(k)?;
                        self.push(val)?;
                        n = 1;
                        continue 'apply;
                    }
                    Trampoline::Throw(val) => {
                        // clean up stack from rator + rands and return the error
                        self.popn(n + 1);
                        return Err(val);
                    }
                    Trampoline::TailCall(push_cont) => {
                        // clean up stack from rator + rands
                        self.popn(n + 1);
                        // push new rator and rands
                        self.push(self.tail_rator)?;
                        // push continuation if needed
                        if push_cont {
                            self.push(k)?;
                        }
                        for i in 0..self.tail_rands.len() {
                            self.push(self.tail_rands[i])?;
                        }

                        n = self.tail_rands.len() + push_cont as usize;
                        continue 'apply;
                    }
                }
            } else if rator.returncontp() {
                let val = rands[0];
                self.popn(n + 1);
                return Ok(val);
            } else if rator.parameterp() {
                match self.invoke_parameter(rator, &rands[1..]) {
                    Trampoline::Return(val) => {
                        self.popn(n + 1);
                        self.push(k)?;
                        self.push(val)?;
                        n = 1;
                        continue 'apply;
                    }
                    Trampoline::Throw(val) => {
                        self.popn(n + 1);
                        return Err(val);
                    }
                    Trampoline::TailCall(_) => {
                        self.popn(n + 1);
                        self.push(self.tail_rator)?;
                        for i in 0..self.tail_rands.len() {
                            self.push(self.tail_rands[i])?;
                        }

                        n = self.tail_rands.len() + 1;
                        continue 'apply;
                    }
                }
            } else {
                unreachable_unchecked()
            }
        }
    }

    /// Invokes JITed code. Calling convention is described like this:
    ///
    /// `fn(&mut Vm, env: Value, cont: Value, argv: *const Value, argc: usize, ret_tag: &mut u8) -> Value`
    /// where:
    /// - `env` is an array of captured variables
    /// - `cont` is a continuation for the procedure
    /// - `argv` is a pointer to an array of arguments
    /// - `argc` is the number of arguments
    ///
    /// # Notes:
    /// - `cont` is actually *before* the procedure and arguments on stack, we expect the JITed code to return [Trampoline::TailCall]
    /// that will invoke it. [Trampoline::Return] must never be returned from the JITed code, but it is still handled for
    /// potential future use.
    ///
    /// - `tag` is used to determinte what kind of return happened: tail-call or error or normal return. The actua value is returned
    /// in the `Value` returned by the function.
    ///
    /// # How does the function return if it always return tail-call?
    ///
    /// When Scheme code is entered from Rust, the "return" continuation is pushed on the stack, when it is invoked
    /// it simply returns the value that is passed to it to the Rust code. So if JITed code returns [Trampoline::TailCall],
    /// the continuation is invoked, which returns the value to the Rust code.
    #[inline(always)]
    unsafe fn invoke_native(&mut self, p: Handle<NativeProcedure>, n: usize) -> Trampoline {
        rsgc::force_on_stack(&p);
        let rands = &self.stack[self.sp - n..self.sp];

        let proc = unsafe {
            std::mem::transmute::<
                _,
                extern "C" fn(&mut Vm, Value, *const Value, usize, &mut u8) -> Value,
            >(p.code)
        };

        let mut tag = 0;

        let res = proc(self, p.captures, rands.as_ptr(), rands.len(), &mut tag);
        //rsgc::force_on_stack(&p);
        match tag {
            0 => unreachable_unchecked(),
            1 => Trampoline::Throw(res),
            2 => Trampoline::TailCall(true),
            _ => unreachable_unchecked(),
        }
    }

    fn invoke_parameter(&mut self, p: Value, rands: &[Value]) -> Trampoline {
        assert!(p.parameterp());
        let guard = *p.parameter_guard();

        if guard.procedurep() && rands.len() > 1 {
            let new_value = match self.apply(guard, &rands) {
                Ok(value) => value,
                Err(value) => return Trampoline::Throw(value),
            };

            self.mutator.write_barrier(p.handle());
            *p.parameter_value_mut() = new_value;
            Trampoline::Return(Value::make_void())
        } else if rands.len() > 0 {
            self.mutator.write_barrier(p.handle());
            *p.parameter_value_mut() = rands[0];
            Trampoline::Return(Value::make_void())
        } else {
            Trampoline::Return(*p.parameter_value())
        }
    }

    pub fn make_procedure(
        name: impl AsRef<str>,
        f: fn(&mut Vm, Value, &[Value]) -> Trampoline,
        mina: i32,
        maxa: i32,
    ) -> Value {
        let maxa = if maxa == -1 { SCHEME_MAX_ARGS } else { maxa };
        let mina = mina as u32;

        let proc = PrimitiveProcedure {
            hdr: Hdr::new(Type::PrimitiveProcedure),
            name: intern(name),
            mina: mina as _,
            maxa: maxa as _,
            code: f,
        };

        let this = unsafe { Value::encode_ptr(Thread::current().allocate(proc).as_ptr()) };
        this
    }

    pub fn make_closed_procedure(
        name: impl AsRef<str>,
        f: fn(&mut Vm, Value, &[Value], Handle<Array<Value>>, &str) -> Trampoline,
        mina: i32,
        maxa: i32,
        vars: &[Value],
    ) -> Value {
        let maxa = if maxa == -1 {
            SCHEME_MAX_ARGS as u32
        } else {
            maxa as u32
        };
        let mina = mina as u32;
        let vars = Array::new(Thread::current(), vars.len(), |_, i| vars[i]);

        let proc = ClosedPrimitiveProcedure {
            hdr: Hdr::new(Type::ClosedPrimitiveProcedure),
            name: intern(name),
            mina: mina as _,
            maxa: maxa as _,
            code: f,
            captures: vars,
        };

        unsafe { Value::encode_ptr(Thread::current().allocate(proc).as_ptr()) }
    }

    pub fn values(&mut self, values: &[Value]) -> Value {
        Value::make_values(self.mutator, values)
    }
}

impl Object for Vm {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        for i in 0..self.sp {
            self.stack[i].trace(visitor);
        }

        self.tail_rator.trace(visitor);
        self.tail_rands.trace(visitor);
    }
}

/// A trampoline is a value that is returned by a primitive procedures.
#[repr(C, u64)]
pub enum Trampoline {
    /// Return a value from a procedure.
    ///
    /// In case of primitive procedures, the value that is stored in the variant
    /// will be passed as an arugment to the continuation.
    Return(Value) = 0,
    /// Throw a value from a procedure.
    Throw(Value) = 1,
    /// Tail call a procedure
    TailCall(bool) = 2,
}

impl Into<Result<Value, Value>> for Trampoline {
    fn into(self) -> Result<Value, Value> {
        match self {
            Trampoline::Return(v) => Ok(v),
            Trampoline::Throw(v) => Err(v),
            Trampoline::TailCall(_) => unreachable!(),
        }
    }
}

impl Into<Result<bool, Value>> for Trampoline {
    fn into(self) -> Result<bool, Value> {
        match self {
            Trampoline::Return(v) => Ok(v.is_true()),
            Trampoline::Throw(v) => Err(v),
            Trampoline::TailCall(_) => unreachable!(),
        }
    }
}



/// Accepts a list of (parameter, value) pairs and a callback.
///
/// Parameters are set to the given values, and the callback is invoked.
///
/// After the callback returns, the parameters are restored to their original values.
pub fn parameterize<T>(
    parameters: &[(Value, Value)],
    callback: impl FnOnce() -> Result<T, Value>,
) -> Result<T, Value> {
    let mut saved = ArrayList::with_capacity(Thread::current(), parameters.len());
    for (p, v) in parameters.iter() {
        saved.push(Thread::current(), *p.parameter_value());

        let _ = vm().apply(*p, &[*v])?;
    }

    let result = callback();

    for (p, _) in parameters.iter().rev() {
        let _ = vm().apply(*p, &[saved.pop().unwrap()])?;
    }

    result
}

impl FromResidual<Result<Infallible, Value>> for Trampoline {
    fn from_residual(residual: Result<Infallible, Value>) -> Self {
        match residual {
            Ok(_) => unreachable!(),
            Err(v) => Trampoline::Throw(v),
        }
    }
}

impl FromResidual<Result<bool, Value>> for Trampoline {
    fn from_residual(residual: Result<bool, Value>) -> Self {
        match residual {
            Ok(v) => Trampoline::Return(v.into()),
            Err(v) => Trampoline::Throw(v),
        }
    }
}

impl FromResidual<Result<Value, Value>> for Trampoline {
    fn from_residual(residual: Result<Value, Value>) -> Self {
        match residual {
            Ok(v) => Trampoline::Return(v),
            Err(v) => Trampoline::Throw(v),
        }
    }
}

impl<T: Into<Value>> Into<Trampoline> for Result<T, Value> {
    fn into(self) -> Trampoline {
        match self {
            Ok(v) => Trampoline::Return(v.into()),
            Err(v) => Trampoline::Throw(v),
        }
    }
}

impl Into<Value> for () {
    fn into(self) -> Value {
        Value::make_void()
    }
}