use std::{hint::black_box, mem::size_of};

use crate::{
    runtime::fun::scm_make_closed_native_procedure,
    vm::{
        callframe::CallFrame,
        scm_vm,
        setjmp::{longjmp, setjmp, JmpBuf},
        Winder, VM,
    },
};

use super::{
    error::wrong_contract,
    fun::{scm_make_subr, SCM_PRIM_CONTINUATION},
    list::scm_cons,
    module::{scm_capy_module, scm_define, scm_find_binding, scm_internal_module},
    object::{ObjectHeader, ScmResult, Type},
    symbol::Intern,
    value::Value,
};

extern "C" fn wind_down(_cfr: &mut CallFrame) -> ScmResult {
    unsafe {
        let vm = scm_vm();

        if let Some(winder) = vm.wind_down() {
            return ScmResult::ok(scm_cons(vm.mutator(), winder.before, winder.after));
        }

        ScmResult::ok(Value::encode_null_value())
    }
}

extern "C" fn wind_up(cfr: &mut CallFrame) -> ScmResult {
    let before = cfr.argument(0);
    let after = cfr.argument(1);

    let mut handlers = if cfr.argument_count() > 2 {
        Some(cfr.argument(2))
    } else {
        None
    };

    if !before.is_procedure() {
        return wrong_contract::<()>(
            "%wind-up",
            "procedure?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    if !after.is_procedure() {
        return wrong_contract::<()>(
            "%wind-up",
            "procedure?",
            1,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let vm = scm_vm();

    if let Some(ref mut handlers) = handlers {
        let cur = vm.current_handlers();
        *handlers = scm_cons(
            vm.mutator(),
            *handlers,
            cur.unwrap_or(Value::encode_null_value()),
        );
    }

    unsafe {
        vm.wind_up(before, after, handlers);
    }

    ScmResult::ok(Value::encode_undefined_value())
}

extern "C" fn wind_up_raise(cfr: &mut CallFrame) -> ScmResult {
    let before = cfr.argument(0);
    let after = cfr.argument(1);

    if !before.is_procedure() {
        return wrong_contract::<()>(
            "%wind-up-raise",
            "procedure?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    if !after.is_procedure() {
        return wrong_contract::<()>(
            "%wind-up-raise",
            "procedure?",
            1,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    match scm_vm().current_handlers() {
        Some(val) if val.is_pair() => {
            unsafe { scm_vm().wind_up(before, after, Some(val.cdr())) }

            ScmResult::ok(val.car())
        }

        _ => ScmResult::ok(false),
    }
}

extern "C" fn dynamic_wind_base(cfr: &mut CallFrame) -> ScmResult {
    let cont = cfr.argument(0);

    if !cont.is_closed_native_procedure() {
        return wrong_contract::<()>(
            "%dynamic-wind-base",
            "continuation?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let cont = cont.closed_native_procedure();

    if cont.len() != 1 && cont[0].is_xtype(Type::Continuation) {
        return wrong_contract::<()>(
            "%dynamic-wind-base",
            "continuation?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let cont = unsafe { std::mem::transmute::<_, Handle<Cont>>(cont[0]) };

    let base = scm_vm()
        .winders
        .zip(cont.vmcont.winders)
        .and_then(|(x, y)| Winder::common_prefix(x, y));

    ScmResult::ok(base.map(|x| x.id).unwrap_or(0))
}

extern "C" fn dynamic_wind_current(_cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(scm_vm().winders.map(|x| x.id).unwrap_or(0))
}

extern "C" fn dynamic_winders(cfr: &mut CallFrame) -> ScmResult {
    let cont = cfr.argument(0);

    if !cont.is_closed_native_procedure() {
        return wrong_contract::<()>(
            "%dynamic-wind-base",
            "continuation?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let cont = cont.closed_native_procedure();

    if cont.len() != 1 && cont[0].is_xtype(Type::Continuation) {
        return wrong_contract::<()>(
            "%dynamic-wind-base",
            "continuation?",
            0,
            cfr.argument_count() as i32,
            cfr.arguments(),
        )
        .into();
    }

    let cont = unsafe { std::mem::transmute::<_, Handle<Cont>>(cont[0]) };

    let base = scm_vm().winders;

    let mut res = Value::encode_null_value();
    let mut next = cont.vmcont.winders;

    loop {
        if let Some(winder) = next.filter(|&x| base.is_none() || x != base.unwrap()) {
            res = scm_cons(
                Thread::current(),
                scm_cons(Thread::current(), winder.before, winder.after),
                res,
            );
            next = winder.next;
        } else {
            break;
        }
    }

    ScmResult::ok(res)
}

use rsgc::{heap::stack::approximate_stack_pointer, prelude::Allocation, thread::Thread};
use rsgc::{
    heap::stack::StackBounds,
    prelude::{Handle, Object},
    system::arraylist::ArrayList,
};
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
pub struct Cont {
    pub(crate) header: ObjectHeader,
    pub(crate) jmpbuf: JmpBuf,
    /// size of the captured stack
    pub(crate) csize: usize,
    /// start pointer of the native stack
    pub(crate) cstart: usize,
    /// end pointer of the native stack
    pub(crate) cend: usize,
    /// whether the continuation is fresh or not (was it invoked?)
    pub(crate) fresh: bool,
    pub(crate) cstack: *mut u8,
    pub(crate) vmcont: VMCont,
}

unsafe impl Object for Cont {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        unsafe {
            visitor.visit_conservative(self.cstack.cast(), self.csize / size_of::<usize>());
            visitor.visit_conservative(
                &self.jmpbuf as *const JmpBuf as *const _,
                std::mem::size_of::<JmpBuf>(),
            );
            visitor.visit_conservative(self.vmcont.stack.as_ptr().cast(), self.vmcont.stack.len());
            self.vmcont.winders.trace(visitor);
            self.vmcont.stack.trace(visitor);
        }
    }
}

unsafe impl Allocation for Cont {
    const FINALIZE: bool = true;
    const DESTRUCTIBLE: bool = true;
}

impl Drop for Cont {
    fn drop(&mut self) {
        unsafe {
            libc::free(self.cstack.cast());
        }
    }
}
pub struct VMCont {
    pub(crate) stack: ArrayList<u8>,
    pub(crate) winders: Option<Handle<Winder>>,
    pub(crate) sp: *mut Value,
    pub(crate) top_call_frame: *mut CallFrame,
    pub(crate) top_entry_frame: *mut CallFrame,
    pub(crate) prev_top_call_frame: *mut CallFrame,
    pub(crate) prev_top_entry_frame: *mut CallFrame,
}

impl VM {
    fn capture_stack(&mut self) -> VMCont {
        let size = &self.stack[self.stack.len() - 1] as *const Value as usize - self.sp as usize;

        let mut stack = ArrayList::with(self.mutator(), size, size, |_, _| 0u8);

        unsafe {
            std::ptr::copy_nonoverlapping(self.sp as *const u8, stack.as_mut_ptr(), size);
        }

        VMCont {
            stack,
            winders: self.winders,
            sp: self.sp,
            top_call_frame: self.top_call_frame,
            top_entry_frame: self.top_entry_frame,
            prev_top_call_frame: self.prev_top_call_frame,
            prev_top_entry_frame: self.prev_top_entry_frame,
        }
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
unsafe fn restore_cont_jump(mut k: Handle<Cont>) -> ! {
    let _unused_buf: [u8; 1000] = black_box([0; 1000]);

    let cur_stack_size = stack_size(); // approximate size of stack by doing (stack_origin - approximate_stack_pointer())

    if cur_stack_size <= ((*k).csize as usize + 1024) {
        restore_cont_jump(k);
    } else {
        k.fresh = false;
        libc::memcpy((*k).cstart as _, (*k).cstack as _, (*k).csize);
        longjmp(&(*k).jmpbuf, 1);
    }
}

#[repr(C)]
union U {
    cont: Handle<Cont>,
    value: Value,
}

#[repr(C)]
struct ContResult {
    returned: bool,
    value: U,
}

/// Creates a continuation from the current state of the VM and native stack.
///
/// - Returns Ok(continuation) if the continuation was created successfully,
/// - Err(value) if the continuation was invoked.
unsafe fn make_continuation(vm: &mut VM) -> ContResult {
    let addr = approximate_stack_pointer() as usize;
    vm.mutator().safepoint();
    let start_stack = stack_origin();

    let csize;
    let cstart;
    let cend;
    // compute the size of the stack and its end
    if addr < start_stack {
        csize = start_stack - addr;
        cstart = addr;
        cend = start_stack;
    } else {
        csize = addr - start_stack;
        cstart = start_stack;
        cend = addr;
    }

    let vm_cont = vm.capture_stack();

    let mut cont = vm.mutator().allocate(Cont {
        header: ObjectHeader::new(Type::Continuation),
        jmpbuf: JmpBuf::new(),
        csize,
        cstart,
        cend,
        fresh: true,
        cstack: cstart as _,
        vmcont: vm_cont,
    });

    cont.csize = csize;
    cont.cstart = cstart;
    cont.cend = cend;
    cont.fresh = true;
    cont.cstack = libc::malloc(csize).cast();

    libc::memcpy(cont.cstack.cast(), cstart as _, csize);

    if setjmp(&mut cont.jmpbuf) == 0 {
        // fresh continuation
        ContResult {
            returned: false,
            value: U { cont },
        }
    } else {
        // continuation was invoked
        ContResult {
            returned: true,
            value: U {
                value: scm_vm().result,
            },
        }
    }
}

extern "C" fn restore_callback(cfr: &mut CallFrame) -> ScmResult {
    let val = cfr.argument(0);

    let cont = cfr.callee().closed_native_procedure()[0];

    assert!(cont.get_type() == Type::Continuation);

    unsafe {
        let cont: Handle<Cont> = std::mem::transmute(cont);
        let vm = scm_vm();

        vm.sp = cont.vmcont.sp;

        let offset = &vm.stack[vm.stack.len() - 1] as *const Value as usize - vm.sp as usize;
        let offset_in_value = offset / std::mem::size_of::<Value>();

        std::ptr::copy_nonoverlapping(
            cont.vmcont.stack.as_ptr().cast::<Value>(),
            vm.sp,
            offset_in_value,
        );

        vm.top_call_frame = cont.vmcont.top_call_frame;
        vm.top_entry_frame = cont.vmcont.top_entry_frame;
        vm.prev_top_call_frame = cont.vmcont.prev_top_call_frame;
        vm.prev_top_entry_frame = cont.vmcont.prev_top_entry_frame;
        vm.winders = cont.vmcont.winders;
        vm.result = val;

        restore_cont_jump(cont);
    }
}

extern "C" fn unprotected_call_cc(cfr: &mut CallFrame) -> ScmResult {
    let callback = cfr.argument(0);
    if !callback.is_procedure() {
        return wrong_contract::<()>("%unprotected-call/cc", "procedure?", 0, 1, &[callback])
            .into();
    }
    unsafe {
        let cont = make_continuation(scm_vm());

        if cont.returned {
            return ScmResult::ok(cont.value.value);
        }
        let vm = scm_vm();
        let mut subr = scm_make_closed_native_procedure(
            vm.mutator(),
            "%continuation".intern().into(),
            restore_callback,
            1,
            1,
            &[cont.value.cont.into()],
        );
        subr.header.flags |= SCM_PRIM_CONTINUATION as u32;
        ScmResult::tail(callback, &[subr.into()])
    }
}

pub(crate) fn init_cont() {
    let module = scm_internal_module().module();

    let subr = scm_make_subr("%wind-down", wind_down, 0, 0);
    scm_define(module, "%wind-down".intern(), subr).unwrap();

    let subr = scm_make_subr("%wind-up", wind_up, 2, 3);
    scm_define(module, "%wind-up".intern(), subr).unwrap();

    let subr = scm_make_subr("%wind-up-raise", wind_up_raise, 2, 2);
    scm_define(module, "%wind-up-raise".intern(), subr).unwrap();

    let subr = scm_make_subr("%dynamic-wind-base", dynamic_wind_base, 1, 1);
    scm_define(module, "%dynamic-wind-base".intern(), subr).unwrap();

    let subr = scm_make_subr("%dynamic-wind-current", dynamic_wind_current, 0, 0);
    scm_define(module, "%dynamic-wind-current".intern(), subr).unwrap();

    let subr = scm_make_subr("%dynamic-winders", dynamic_winders, 1, 1);
    scm_define(module, "%dynamic-winders".intern(), subr).unwrap();

    let subr = scm_make_subr("%unprotected-call/cc", unprotected_call_cc, 1, 1);
    scm_define(module, "%unprotected-call/cc".intern(), subr).unwrap();
}

pub fn with_exception_handler_subr() -> Value {
    let module = scm_capy_module().module();
    let subr = scm_find_binding(module, ".@with-exception-handler".intern(), 0);

    subr.map(|x| x.value)
        .unwrap_or(Value::encode_bool_value(false))
}
