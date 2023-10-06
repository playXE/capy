#![allow(unused_variables)]
use std::{
    hint::black_box,
    mem::{size_of, transmute},
    ptr::null,
};

use mmtk::{
    util::{Address, ObjectReference},
    vm::EdgeVisitor,
    AllocationSemantics, MutatorContext,
};
use rsetjmp::longjmp;

use super::{
    environment::{environment_get, scm_define},
    object::{scm_program_set_free_variable, Header, ScmCellHeader, TypeId},
    symbol::scm_intern,
    value::Value,
};
use crate::{
    bytecode::opcodes::{
        OP_ASSERT_NARGS_EE, OP_CAPTURE_CONTINUATION, OP_CONTINUATION_CALL, OP_MOV, OP_TAIL_CALL,
    },
    gc::stack::{approximate_stack_pointer, StackBounds},
    gc_protect,
    interpreter::{
        scm_call_n,
        stackframe::{
            frame_dynamic_link, frame_previous_sp, frame_virtual_return_address, StackElement,
        },
    },
    runtime::error::capture_stacktrace,
    utils::round_up,
    vm::thread::Thread,
};
use crate::{
    gc::ObjEdge,
    runtime::gsubr::{scm_define_subr, Subr},
};
use crate::{
    runtime::object::{scm_program_free_variable, scm_program_num_free_vars},
    vm::scm_virtual_machine,
};

#[repr(C)]
pub struct VMCont {
    pub header: ScmCellHeader,
    pub id: u64,
    pub vra: *const u8,
    pub mra: *const u8,
    pub fp_offset: isize,
    pub sp_offset: isize,
    pub stack_size: usize,
    pub stack_bottom: *mut StackElement,
}

impl VMCont {
    pub(crate) unsafe fn visit_edges<EV: EdgeVisitor<ObjEdge>>(&mut self, visitor: &mut EV) {
        let stack_top = self.stack_bottom.add(self.stack_size);
        let mut fp = stack_top.offset(-self.fp_offset);
        let mut sp = stack_top.offset(-self.sp_offset);
        while fp < stack_top {
            while sp < fp {
                let value = sp.cast::<Value>();

                if (*value).is_object() {
                    let edge = ObjEdge::from_address(Address::from_ptr(value));
                    visitor.visit_edge(edge);
                }

                sp = sp.offset(1);
            }

            sp = frame_previous_sp(fp);
            fp = frame_dynamic_link(fp).cast();
        }
    }
}

thread_local! {
    static STACK_BOUNDS: StackBounds = StackBounds::current_thread_stack_bounds();
}

/// Returns the size of the native stack
fn stack_size() -> usize {
    STACK_BOUNDS.with(|bounds| (bounds.origin as usize) - (approximate_stack_pointer() as usize))
}

/// Returns the start of the native stack
fn stack_origin() -> usize {
    STACK_BOUNDS.with(|bounds| bounds.origin as usize)
}

#[repr(C)]
pub struct ScmContinuation {
    pub(crate) header: ScmCellHeader,
    pub(crate) regs: rsetjmp::JmpBuf,
    pub(crate) vm_cont: Value,
    /// size of the captured stack
    pub(crate) csize: usize,
    /// start pointer of the native stack
    pub(crate) cstart: usize,
    /// end pointer of the native stack
    pub(crate) cend: usize,
    /// whether the continuation is fresh or not (was it invoked?)
    pub(crate) fresh: bool,
    pub(crate) cstack: *mut u8,
}

impl ScmContinuation {
    pub(crate) fn visit_edges<EV: EdgeVisitor<ObjEdge>>(&mut self, visitor: &mut EV) {
        self.vm_cont.visit_edge(visitor);
    }
}

unsafe fn capture_stack(
    thread: &mut Thread,
    stack_top: *mut StackElement,
    fp: *mut StackElement,
    sp: *mut StackElement,
    vra: *const u8,
    mra: *const u8,
) -> Value {
    let stack_size = stack_top.offset_from(sp) as usize;
    let alloc_size = round_up(
        size_of::<VMCont>() + stack_size * size_of::<StackElement>(),
        8,
        0,
    );

    let mem = thread
        .mutator()
        .alloc(alloc_size, 8, 0, AllocationSemantics::Default);
    let vm_cont = mem.to_mut_ptr::<VMCont>();

    unsafe {
        vm_cont.write(VMCont {
            header: ScmCellHeader {
                as_header: Header {
                    type_id: TypeId::VMCont,
                    pad: [0; 4],
                    flags: 0,
                },
            },
            id: thread.interpreter().entry_id,
            vra,
            mra,
            fp_offset: stack_top.offset_from(fp),
            sp_offset: stack_top.offset_from(sp),
            stack_bottom: (mem + size_of::<VMCont>()).to_mut_ptr(),
            stack_size,
        });
        let cont = &mut *vm_cont;
        std::ptr::copy_nonoverlapping(sp, cont.stack_bottom, stack_size);

        thread.mutator().post_alloc(
            ObjectReference::from_raw_address(mem),
            alloc_size,
            AllocationSemantics::Default,
        );

        Value::encode_object_value(transmute(cont))
    }
}

impl Thread {
    pub unsafe fn capture_current_stack(&mut self) -> Value {
        let vra = frame_virtual_return_address(self.interpreter().fp);
        let mra = null();
        let fp = frame_dynamic_link(self.interpreter().fp).cast();
        let sp = frame_previous_sp(self.interpreter().fp);
        let stack_top = self.interpreter().stack_top;

        capture_stack(self, stack_top, fp, sp, vra, mra)
    }

    pub unsafe fn make_continuation(&mut self, mut vm_cont: Value) -> Value {
        self.safepoint();
        let stack_origin = stack_origin();
        let addr = approximate_stack_pointer() as usize;
        let csize;
        let cstart;
        let cend;
        // compute the size of the stack and its end
        if addr < stack_origin {
            csize = stack_origin - addr;
            cstart = addr;
            cend = stack_origin;
        } else {
            csize = addr - stack_origin;
            cstart = stack_origin;
            cend = addr;
        }

        let alloc_size = round_up(size_of::<ScmContinuation>(), 8, 0);
        let cont = gc_protect!(self => vm_cont => self.mutator().alloc(alloc_size, 8, 0, AllocationSemantics::Default));
        let cont = cont.to_mut_ptr::<ScmContinuation>();

        let cstack = mmtk::memory_manager::malloc(csize).to_mut_ptr::<u8>();

        libc::memcpy(cstack.cast(), cstart as _, csize);

        cont.write(ScmContinuation {
            header: ScmCellHeader {
                as_header: Header {
                    type_id: TypeId::Continuation,
                    pad: [0; 4],
                    flags: 0,
                },
            },
            regs: self.interpreter().registers.read(),
            vm_cont,
            csize,
            cstart: cstart as _,
            cend,
            fresh: true,
            cstack,
        });

        self.mutator().post_alloc(
            ObjectReference::from_raw_address(Address::from_mut_ptr(cont)),
            alloc_size,
            AllocationSemantics::Default,
        );

        Value::encode_object_value(transmute(cont))
    }
}

static GOTO_CONTINUATION_CODE: &'static [u8] = &[OP_CONTINUATION_CALL, 0];

static CALL_WITH_CURRENT_CONTINUATION_CODE: &'static [u8] = &[
    OP_ASSERT_NARGS_EE,
    2,
    0,
    0, // (assert-nargs-ee 2)
    OP_MOV,
    1,
    0,
    0,
    0, // (mov 1 0)
    OP_CAPTURE_CONTINUATION,
    0, // (capture-continuation 0)
    OP_TAIL_CALL,
];

impl Thread {
    pub(crate) fn make_continuation_trampoline(&mut self, mut contregs: Value) -> Value {
        let program = gc_protect!(self => contregs => self.make_program::<false>(GOTO_CONTINUATION_CODE.as_ptr(), 1));
        scm_program_set_free_variable(program, self, 0, contregs);
        program
    }
}

/// Restores the continuation and jumps to it.
///
/// This code will recursively call itself until the stack size is large enough to fit the continuation.
///
/// # Safety
///
/// Inheretely unsafe, because it uses `longjmp` to jump to the continuation. All local variables that depend
/// on destructors will be broken.
#[inline(never)]
pub unsafe fn restore_cont_jump(k: &mut ScmContinuation) -> ! {
    let _unused_buf: [u8; 1000] = black_box([0; 1000]);

    let cur_stack_size = stack_size(); // approximate size of stack by doing (stack_origin - approximate_stack_pointer())

    if cur_stack_size <= (k.csize as usize + 1024) {
        restore_cont_jump(k);
    } else {
        k.fresh = false;
        libc::memcpy((*k).cstart as _, (*k).cstack as _, (*k).csize);
        longjmp(&k.regs, 1);
    }
}

pub(crate) unsafe fn capture_continuation(thread: &mut Thread) -> Value {
    let vm_cont = thread.capture_current_stack();
    let cont = thread.make_continuation(vm_cont);

    thread.make_continuation_trampoline(cont)
}

extern "C-unwind" fn continuation_p(_thread: &mut Thread, cont: &mut Value) -> Value {
    if !cont.is_program() {
        return Value::encode_bool_value(false);
    }

    if scm_program_num_free_vars(*cont) != 1 {
        return Value::encode_bool_value(false);
    }

    let cont = scm_program_free_variable(*cont, 0);
    if cont.type_of() != TypeId::Continuation {
        return Value::encode_bool_value(false);
    }

    Value::encode_bool_value(true)
}

extern "C-unwind" fn error(_thread: &mut Thread, val: &mut Value) -> Value {
    let stacktrace = unsafe { capture_stacktrace(_thread) };
    eprintln!("error: {}\n{}", val, stacktrace);
    std::process::abort();
}

pub fn raise_assertion_violation(
    thread: &mut Thread,
    who: Value,
    message: Value,
    irritant: Option<Value>,
) -> ! {
    let proc = environment_get(
        scm_virtual_machine().interaction_environment,
        scm_intern("assertion-violation"),
    );
    match proc {
        Ok(proc) => {
            let _ = match irritant {
                Some(irritant) => scm_call_n(thread, proc, &[who, message, irritant]),
                None => scm_call_n(thread, proc, &[who, message]),
            };
            unsafe {
                std::hint::unreachable_unchecked();
            }
        }

        Err(_) => {
            let message = match irritant {
                Some(irritant) => format!("{}: {}: {}", who, message, irritant),
                None => format!("{}: {}", who, message),
            };
            eprintln!("Pre-boot error: {}", message);
            std::process::abort();
        }
    }
}

pub fn wrong_number_of_arguments_violation(
    thread: &mut Thread,
    proc: Value,
    required_min: i32,
    required_max: i32,
    argc: usize,
    argv: &[&mut Value],
) -> ! {
    todo!()
}

#[inline(never)]
#[cold]
pub fn wrong_type_argument_violation(
    thread: &mut Thread,
    who: &str,
    position: usize,
    expected: &str,
    got: Value,
    argc: usize,
    argv: &[&mut Value],
) -> ! {
    println!(
        "wrong type argument violation: {} expected {}, got {}\n{}",
        who,
        expected,
        got,
        unsafe { capture_stacktrace(thread) }
    );
    todo!()
}

pub fn invalid_argument_violation(
    thread: &mut Thread,
    who: &str,
    description: &str,
    value: Value,
    pos: usize,
    argc: usize,
    argv: &[&mut Value],
) -> ! {
    println!(
        "invalid argument violation: {} {} {}\n{}",
        who,
        description,
        value,
        unsafe { capture_stacktrace(thread) }
    );
    todo!()
}

pub fn raise_error(thread: &mut Thread, who: &str, description: &str, code: i32) -> ! {
    todo!()
}

pub fn raise_error_argv(
    thread: &mut Thread,
    who: &str,
    description: &str,
    code: i32,
    argc: usize,
    argv: &[Value],
) -> ! {
    todo!()
}

pub fn scheme_raise(thread: &mut Thread, value: Value) -> ! {
    let raise_proc = environment_get(
        scm_virtual_machine().interaction_environment,
        scm_intern("raise"),
    )
    .unwrap();
    let _ = scm_call_n(thread, raise_proc, &[value]);

    unreachable!("raise returned")
}

pub(crate) fn init() {
    let unsafe_call_cc =
        Thread::current().make_program::<true>(CALL_WITH_CURRENT_CONTINUATION_CODE.as_ptr(), 0);
    scm_define(scm_intern("%call/cc"), unsafe_call_cc);
    scm_define_subr("continuation?", 1, 0, 0, Subr::F1(continuation_p));
    scm_define_subr("error", 0, 0, 1, Subr::F1(error));
}
