use std::{
    hint::black_box,
    mem::{size_of, transmute},
    ptr::{null, NonNull},
};

use mmtk::{
    util::{Address, ObjectReference},
    vm::{edge_shape::SimpleEdge, EdgeVisitor},
    AllocationSemantics, MutatorContext,
};
use rsetjmp::longjmp;

use super::{
    environment::scm_define,
    object::{scm_program_set_free_variable, Header, ScmCellHeader, TypeId},
    symbol::scm_intern,
    value::Value,
};
use crate::runtime::gsubr::{scm_define_subr, Subr};
use crate::runtime::object::{
    scm_car, scm_cdr, scm_program_free_variable, scm_program_num_free_vars, scm_set_car,
    scm_set_cdr,
};
use crate::{
    bytecode::opcodes::{
        OP_ASSERT_NARGS_EE, OP_CAPTURE_CONTINUATION, OP_CONTINUATION_CALL, OP_MOV, OP_TAIL_CALL,
    },
    gc::stack::{approximate_stack_pointer, StackBounds},
    gc_protect,
    interpreter::stackframe::{
        frame_dynamic_link, frame_previous_sp, frame_virtual_return_address, StackElement,
    },
    utils::round_up,
    vm::thread::Thread,
};

/// Winder
///
/// Represents `dynamic-wind` frame. Entries are linked in a list.
#[repr(C)]
pub struct Winder {
    pub(crate) header: ScmCellHeader,
    pub(crate) id: u32,
    pub(crate) before: Value,
    pub(crate) after: Value,
    pub(crate) handlers: Option<Value>,
    pub(crate) next: Option<NonNull<Winder>>,
}

impl Winder {
    pub(crate) fn visit_edges<EV: EdgeVisitor<SimpleEdge>>(&mut self, visitor: &mut EV) {
        self.before.visit_edge(visitor);
        self.after.visit_edge(visitor);
        if let Some(handlers) = self.handlers.as_mut() {
            handlers.visit_edge(visitor);
        }

        if let Some(next) = self.next.as_mut() {
            let edge = SimpleEdge::from_address(Address::from_mut_ptr(next));
            visitor.visit_edge(edge);
        }
    }

    pub fn len(&self) -> usize {
        let mut ws = Some(self);
        let mut n = 0;
        while let Some(w) = ws {
            n += 1;
            unsafe {
                ws = w.next.map(|p| p.as_ref());
            }
        }

        n
    }

    pub fn common_prefix<'a>(this: &'a Self, other: &'a Self) -> Option<&'a Self> {
        let mut that = Some(other);
        let mut this = Some(this);

        let this_len = this.as_ref().map(|w| w.len()).unwrap_or(0);
        let that_len = that.as_ref().map(|w| w.len()).unwrap_or(0);

        unsafe {
            if this_len > that_len {
                for _ in that_len..this_len {
                    this = this.unwrap().next.map(|p| p.as_ref());
                }
            }

            if that_len > this_len {
                for _ in this_len..that_len {
                    that = that.unwrap().next.map(|p| p.as_ref());
                }
            }

            while let Some((this_winder, that_winder)) = this
                .zip(that)
                .filter(|(x, y)| *x as *const Winder != *y as *const Winder)
            {
                this = this_winder.next.map(|p| p.as_ref());
                that = that_winder.next.map(|p| p.as_ref());
            }

            this
        }
    }
}

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
    pub winders: Option<NonNull<Winder>>,
}

impl VMCont {
    pub(crate) unsafe fn visit_edges<EV: EdgeVisitor<SimpleEdge>>(&mut self, visitor: &mut EV) {
        let stack_top = self.stack_bottom.add(self.stack_size);
        let mut fp = stack_top.offset(-self.fp_offset);
        let mut sp = stack_top.offset(-self.sp_offset);
        while fp < stack_top {
            while sp < fp {
                let value = sp.cast::<Value>();

                if (*value).is_object() {
                    let edge = SimpleEdge::from_address(Address::from_ptr(value));
                    visitor.visit_edge(edge);
                }

                sp = sp.offset(1);
            }

            sp = frame_previous_sp(fp);
            fp = frame_dynamic_link(fp);
        }
        if let Some(winders) = self.winders.as_mut() {
            let edge = SimpleEdge::from_address(Address::from_mut_ptr(winders));
            visitor.visit_edge(edge);
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
    pub(crate) fn visit_edges<EV: EdgeVisitor<SimpleEdge>>(&mut self, visitor: &mut EV) {
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
            winders: thread.interpreter().winders,
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
        let fp = frame_dynamic_link(self.interpreter().fp);
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

extern "C-unwind" fn wind_down(thread: &mut Thread) -> Value {
    let winder = thread.interpreter().winders;
    if let Some(winder) = winder.map(|x| unsafe { x.as_ref() }) {
        thread.interpreter().winders = winder.next;
        let mut winder_val: Value = unsafe { transmute(winder) };
        let cons = gc_protect!(thread => winder_val => thread.make_cons::<false>(Value::encode_null_value(), Value::encode_null_value()));

        scm_set_car(cons, thread, winder_val.cast_as::<Winder>().before);
        scm_set_cdr(cons, thread, winder_val.cast_as::<Winder>().after);

        cons
    } else {
        Value::encode_null_value()
    }
}

fn current_handlers(thread: &mut Thread) -> Option<Value> {
    let mut winders = thread.interpreter().winders;

    while let Some(winder) = winders {
        if let Some(handler) = unsafe { winder.as_ref().handlers } {
            return Some(handler);
        }
        winders = unsafe { winder.as_ref().next };
    }

    None
}

extern "C-unwind" fn wind_up(
    thread: &mut Thread,
    before: &mut Value,
    after: &mut Value,
    handlers: &mut Value,
) -> Value {
    let mut handlers = if handlers.is_undefined() {
        None
    } else {
        Some(*handlers)
    };

    if let Some(ref mut handlers) = handlers {
        let mut cur = current_handlers(thread).unwrap_or(Value::encode_null_value());

        let mut h = *handlers;
        let pair = gc_protect!(thread => h, cur => thread.make_cons::<false>(Value::encode_null_value(), Value::encode_null_value()));

        scm_set_car(pair, thread, h);
        scm_set_cdr(pair, thread, cur);
        *handlers = pair;
    }
    let mut handlers = handlers.unwrap_or(Value::encode_null_value());
    let winder = gc_protect!(thread => handlers => thread.make_winder());
    winder.cast_as::<Winder>().after = *after;
    winder.cast_as::<Winder>().before = *before;
    winder.cast_as::<Winder>().handlers = if handlers.is_null() {
        None
    } else {
        Some(handlers)
    };
    winder.cast_as::<Winder>().next = thread.interpreter().winders.take();
    thread.interpreter().winders =
        Some(unsafe { NonNull::new_unchecked(winder.cast_as::<Winder>()) });
    Value::encode_undefined_value()
}

extern "C-unwind" fn wind_up_raise(
    thread: &mut Thread,
    before: &mut Value,
    after: &mut Value,
) -> Value {
    match current_handlers(thread) {
        Some(val) if val.is_pair() => {
            wind_up(
                thread,
                before,
                after,
                &mut scm_cdr(val), /* will be rooted inside the wind_up */
            );
            scm_car(val)
        }

        _ => Value::encode_bool_value(false),
    }
}

extern "C-unwind" fn dynamic_wind_base(thread: &mut Thread, cont: &mut Value) -> Value {
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

    let base = thread
        .interpreter()
        .winders
        .zip(
            cont.cast_as::<ScmContinuation>()
                .vm_cont
                .cast_as::<VMCont>()
                .winders,
        )
        .and_then(|(x, y)| unsafe { Winder::common_prefix(x.as_ref(), y.as_ref()) });
    Value::encode_int32(base.map(|x| x.id).unwrap_or(0) as _)
}

extern "C-unwind" fn dynamic_wind_current(thread: &mut Thread) -> Value {
    Value::encode_int32(
        thread
            .interpreter()
            .winders
            .map(|x| unsafe { x.as_ref().id })
            .unwrap_or(0) as _,
    )
}

extern "C-unwind" fn dynamic_winders(thread: &mut Thread, cont: &mut Value) -> Value {
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

    let mut base = thread
        .interpreter()
        .winders
        .map(|x| Value::encode_object_value(unsafe { transmute(x) }))
        .unwrap_or(Value::encode_null_value());
    let mut res = Value::encode_null_value();
    let mut next = cont
        .cast_as::<ScmContinuation>()
        .vm_cont
        .cast_as::<VMCont>()
        .winders
        .map(|x| Value::encode_object_value(unsafe { transmute(x) }))
        .unwrap_or(Value::encode_null_value());

    loop {
        // if let Some(winder) = next.filter(|&x| base.is_none() || x != base.unwrap()) {
        if next != base && !(next.is_null() && base.is_null()) {
            let mut cons = gc_protect!(thread => base, next, res => thread.make_cons::<false>(Value::encode_null_value(), Value::encode_null_value()));
            scm_set_car(cons, thread, next.cast_as::<Winder>().before);
            scm_set_cdr(cons, thread, next.cast_as::<Winder>().after);

            let cons2 = gc_protect!(thread => cons, base, next, res => thread.make_cons::<false>(Value::encode_null_value(), Value::encode_null_value()));

            scm_set_car(cons2, thread, cons);
            scm_set_cdr(cons2, thread, res);

            next = next
                .cast_as::<Winder>()
                .next
                .map(|x| Value::encode_object_value(unsafe { transmute(x) }))
                .unwrap_or(Value::encode_null_value());
        } else {
            break;
        }
    }

    res
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
    eprintln!("error: {}", val);
    std::process::abort();
}

pub(crate) fn init() {
    let unsafe_call_cc =
        Thread::current().make_program::<true>(CALL_WITH_CURRENT_CONTINUATION_CODE.as_ptr(), 0);
    scm_define(scm_intern("%call/cc"), unsafe_call_cc);
    scm_define_subr("%wind-down", 0, 0, 0, Subr::F0(wind_down));
    scm_define_subr("%wind-up", 2, 1, 0, Subr::F3(wind_up));
    scm_define_subr("%wind-raise", 2, 0, 0, Subr::F2(wind_up_raise));
    scm_define_subr("%dynamic-wind-base", 1, 0, 0, Subr::F1(dynamic_wind_base));
    scm_define_subr("%dynamic-wind-current", 0, 0, 0, Subr::F0(dynamic_wind_current));
    scm_define_subr("%dynamic-winders", 1, 0, 0, Subr::F1(dynamic_winders));
    scm_define_subr("continuation?", 1, 0, 0, Subr::F1(continuation_p));
    scm_define_subr("error", 0, 0, 1, Subr::F1(error));
}
