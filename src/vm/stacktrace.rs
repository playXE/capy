
use crate::{runtime::{
    fun::{scm_make_subr, get_proc_name},
    module::{scm_define, scm_null_module},
    object::ScmResult,
    symbol::Intern,
    value::Value,
}, op::Opcode};

use super::{callframe::CallFrame, scm_vm, VM};

pub struct StackTrace<'a> {
    vm: &'a VM,
    cfr: *mut CallFrame,
}

impl<'a> StackTrace<'a> {
    pub fn new(vm: &'a VM) -> Self {
        Self {
            vm,
            cfr: vm.top_call_frame,
        }
    }
}

pub struct Context {
    cfr: *mut CallFrame,
}

impl Context {
    pub fn return_pc(&self) -> *const u8 {
        unsafe { (*self.cfr).return_pc() }
    }

    pub fn callee(&self) -> Value {
        unsafe { (*self.cfr).callee() }
    }

    pub fn argument_count(&self) -> usize {
        unsafe { (*self.cfr).argument_count() }
    }

    pub fn arguments(&self) -> &[Value] {
        unsafe { (*self.cfr).arguments() }
    }

    pub fn code_block(&self) -> Value {
        unsafe { (*self.cfr).code_block() }
    }
}

impl<'a> Iterator for StackTrace<'a> {
    type Item = Context;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cfr.is_null() {
            self.cfr = self.vm.prev_top_call_frame;
            if self.cfr.is_null() {
                return None;
            }
        }
        unsafe {
            let cfr = self.cfr;
          
            self.cfr = (*cfr).caller();
            Some(Context { cfr })
        }
    }
}

pub fn get_stacktrace_str(vm: &mut VM) -> String {
    let st = StackTrace::new(vm);
    let mut s = String::new();
    for frame in st {
        let callee = frame.callee();
        let ip = frame.return_pc();
        let _code_block = frame.code_block();
        
        let name = get_proc_name(callee);

        if let Some(name) = name {
            s.push_str(&format!("  at {}", name));
        } else {
            s.push_str("   at <unknown>");
        }

        if !ip.is_null() {
            s.push_str(&format!(":{:p} {}", ip, unsafe {
                std::mem::transmute::<_, Opcode>(ip.read())
            }));
        } else {
            s.push_str(" <native>");
        }

        s.push('\n');
    }

    s
}

extern "C" fn print_stacktrace(_: &mut CallFrame) -> ScmResult {
    let vm = scm_vm();

    for ctx in StackTrace::new(vm) {
        let callee = ctx.callee();

        println!("{:?}", callee);
    }

    ScmResult::ok(Value::encode_undefined_value())
}

pub(crate) fn init_stacktrace() {
    let module = scm_null_module().module();
    let subr = scm_make_subr("print-stacktrace", print_stacktrace, 0, 0);
    scm_define(module, "print-stacktrace".intern(), subr).unwrap();
}
