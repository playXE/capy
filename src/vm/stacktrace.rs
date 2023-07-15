use rsgc::prelude::Handle;

use crate::{
    runtime::{
        error::{srcloc_column, srcloc_line, srcloc_source},
        fun::scm_make_subr,
        module::{scm_define, scm_null_module},
        object::{CodeBlock, ScmResult},
        symbol::Intern,
        value::Value,
    },
    vm::scm_vm,
};

use super::{callframe::CallFrame, VM};

pub struct Frame {
    callee: Value,
    pc: *const u8,
    code_block: Option<Handle<CodeBlock>>,
    caller: *mut CallFrame,
}

pub struct StackTrace<'a> {
    vm: &'a mut VM,
}

impl<'a> StackTrace<'a> {
    pub fn new(vm: &'a mut VM) -> Self {
        Self { vm }
    }

    unsafe fn build(&self) -> String {
        let mut cfr = self.vm.top_call_frame;

        let mut pc = self.vm.ip;
        let mut out = String::new();
        let mut i = 0;
        out.push_str("Stack trace:\n");
        'collect: while !cfr.is_null() {
            let code_block = (*cfr).code_block;

            if code_block.is_code_block() {
                if !pc.is_null() {
                    if let Some(ranges) = code_block.code_block().ranges.as_ref() {
                        let off = pc.offset_from(code_block.code_block().code.as_ptr());
                        for ((start, end), srcloc, _) in ranges.iter().rev() {
                            let start = *start as isize;
                            let end = *end as isize;
                            
                            if off >= start && off < end {
                                let srcloc = *srcloc;
                                let name = code_block.code_block().name;
                                let source = srcloc_source(srcloc);
                                let line = srcloc_line(srcloc);
                                let col = srcloc_column(srcloc);
                                out.push_str(&format!(
                                    "  {}  {}\n\tat {}:{}:{}\n",
                                    i, name, source, line, col
                                ));

                                pc = (*cfr).return_pc;
                                cfr = (*cfr).caller;
                                i += 1;
                                continue 'collect;
                            }
                        }
                    }
                }
            }
            out.push_str(&format!(
                "  {}  {}\n\tat unknown location\n",
                i,
                (*cfr).callee()
            ));
            pc = (*cfr).return_pc;
            cfr = (*cfr).caller;

            i += 1;
        }

        out
    }

    pub fn get(&self) -> String {
        unsafe { self.build() }
    }
}

pub fn get_stacktrace_str(vm: &mut VM) -> String {
    unsafe { StackTrace::new(vm).build() }
}

extern "C" fn print_stacktrace(_: &mut CallFrame) -> ScmResult {
    println!("{}", get_stacktrace_str(scm_vm()));
    ScmResult::ok(Value::encode_undefined_value())
}

pub(crate) fn init_stacktrace() {
    let module = scm_null_module().module();
    let subr = scm_make_subr("print-stacktrace", print_stacktrace, 0, 0);
    scm_define(module, "print-stacktrace".intern(), subr).unwrap();
}
