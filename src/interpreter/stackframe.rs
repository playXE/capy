use std::mem::{size_of, transmute};

use crate::{
    runtime::{
        arith,
        control::{invalid_argument_violation, wrong_type_argument_violation, VMCont},
        gsubr::{scm_define_subr, Subr},
        object::{scm_program_code, ScmCellHeader, TypeId, scm_vector_set},
        value::Value,
    },
    vm::{scm_virtual_machine, thread::Thread},
};

use super::InterpreterState;

/// Stack frame layout
///
/// ```text
/// | ...                            |
/// +================================+ <- fp + 2 = previous sp
/// | Return address                 |
/// +--------------------------------+
/// | Dynamic link (caller frame)    |
/// +================================+ <- fp
/// | Local 0                        |    |
/// +--------------------------------+    |
/// | Local 1                        |    v - lower address
/// +--------------------------------+
/// | ...                            |
/// +--------------------------------+
/// | Local N - 1                    |
/// \--------------------------------/ <- sp
///
///
/// ```
///
/// The stack grows down.
///
/// The calling convention is that a caller prepares a stack frame
/// consisting of the saved FP, the saved virtual return address, and the
/// saved machine return address of the calling function, followed by the
/// procedure and then the arguments to the call, in order. Thus in the
/// beginning of a call, the procedure being called is in slot 0,
/// the first argument is in slot 1, and the SP points to the last argument.
/// The number of arguments, including the procedure, is thus FP - SP.
///
/// After ensuring that the correct number of arguments have been passed,
/// a function will set the stack pointer to point to the last local slot.
/// This lets a function allocate the temporary space that it needs once
/// in the beginning of the call, instead of pushing and popping
/// the stack pointer during the call's extent.
///
/// When program returns, it returns its values in the slots starting from
/// local 0. The callee resets the stack point to the last value. In this
/// way caller knows how many values there are: it's the number of words
/// between the stack pointer and the slot at which the caller placed the
/// procedure.
///
/// After checking that the number of values returned is appropriate,
/// the caller shuffles the values around (if needed), and resets the
/// stack pointer back to its original value from before the call.
#[repr(C)]
pub struct StackFrame {
    pub dynamic_link: Value,
    pub vra: *const u32,
}

#[repr(C)]
pub union StackElement {
    pub as_value: Value,
    pub as_vcode: *const u8,
    pub as_mcode: *const u8,
    pub as_usize: usize,
    pub as_s64: i64,
    pub as_u64: u64,
    pub as_ptr: *const u8,
}

#[inline]
pub unsafe fn frame_previous_sp(fp: *mut StackElement) -> *mut StackElement {
    fp.add(2)
}

#[inline]
pub unsafe fn frame_virtual_return_address(fp: *mut StackElement) -> *const u8 {
    fp.add(1).read().as_vcode
}

#[inline]
pub unsafe fn set_frame_virtual_return_address(fp: *mut StackElement, vra: *const u8) {
    fp.add(1).cast::<*const u8>().write(vra);
}

#[inline]
pub unsafe fn frame_dynamic_link(fp: *mut StackElement) -> *mut StackFrame {
    let dl = fp.add(0).read().as_value;
    fp.add(dl.get_int32() as _) as _
}

#[inline]
pub unsafe fn set_frame_dynamic_link(fp: *mut StackElement, dl: *mut StackElement) {
    debug_assert!(dl >= fp);
    fp.add(0)
        .cast::<Value>()
        .write(Value::encode_int32(dl.offset_from(fp) as _));
}

#[inline]
pub unsafe fn frame_slot(fp: *mut StackElement, index: usize) -> *mut StackElement {
    fp.sub(index as usize).sub(1)
}

#[inline]
pub unsafe fn frame_local<'a>(fp: *mut StackElement, index: usize) -> &'a mut Value {
    &mut (*frame_slot(fp, index)).as_value
}

#[inline]
pub unsafe fn frame_num_locals(fp: *mut StackElement, sp: *mut StackElement) -> isize {
    fp.offset_from(sp)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FrameKind {
    VM,
    Cont,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct ScmFrame {
    pub(crate) header: ScmCellHeader,
    pub(crate) kind: FrameKind,
    pub(crate) stack_holder: *mut (),
    pub(crate) fp_offset: isize,
    pub(crate) sp_offset: isize,
    pub(crate) ip: *const u8,
}

impl Value {
    pub fn is_frame(&self) -> bool {
        self.type_of() == TypeId::Frame
    }

    pub fn frame(&self) -> &mut ScmFrame {
        debug_assert!(self.is_frame());
        self.cast_as()
    }
}

impl Thread {
    pub(crate) fn make_frame(&mut self, kind: FrameKind, frame_proto: &ScmFrame) -> Value {
        let p = self
            .alloc(size_of::<ScmFrame>(), TypeId::Frame)
            .cast::<ScmFrame>();

        unsafe {
            (*p).stack_holder = frame_proto.stack_holder;
            (*p).kind = kind;
            (*p).fp_offset = frame_proto.fp_offset;
            (*p).sp_offset = frame_proto.sp_offset;
            (*p).ip = frame_proto.ip;

            Value::encode_object_value(transmute(p))
        }
    }
}

impl ScmFrame {
    pub(crate) unsafe fn stack_top(&self) -> *mut StackElement {
        match self.kind {
            FrameKind::Cont => {
                let cont = self.stack_holder.cast::<VMCont>();
                (*cont).stack_bottom.add((*cont).stack_size)
            }

            FrameKind::VM => {
                let vm = self.stack_holder.cast::<InterpreterState>();
                (*vm).stack_top
            }
        }
    }

    pub fn source(&self) -> Option<(Value, u32, u32)> {
        let vm = scm_virtual_machine();
        vm.images.debug_info(self.ip)
    }

    pub fn fp(&self) -> *mut StackElement {
        unsafe { self.stack_top().sub(self.fp_offset as usize) }
    }

    pub fn sp(&self) -> *mut StackElement {
        unsafe { self.stack_top().sub(self.sp_offset as usize) }
    }

    pub fn num_locals(&self) -> isize {
        unsafe { frame_num_locals(self.fp(), self.sp()) }
    }

    pub unsafe fn previous(&self) -> Option<ScmFrame> {
        let mut frame = *self;
        let mut this_fp;
        let mut new_fp;
        let mut new_sp;
        let stack_top = self.stack_top();

        loop {
            this_fp = stack_top.sub(frame.fp_offset as usize);

            if this_fp == stack_top {
                return None;
            }

            new_fp = frame_dynamic_link(this_fp).cast::<StackElement>();

            if new_fp >= stack_top {
                return None;
            }

            new_sp = frame_previous_sp(this_fp);

            /*frame = ScmFrame {
                header: ScmCellHeader::new(TypeId::Frame),
                sp_offset: stack_top.offset_from(new_sp) as _,
                fp_offset: stack_top.offset_from(new_fp as _) as _,
                ip: frame_virtual_return_address(this_fp),
                stack_holder: frame.stack_holder,
                kind: frame.kind,
            };*/
            frame.fp_offset = stack_top.offset_from(new_fp as _) as _;
            frame.sp_offset = stack_top.offset_from(new_sp) as _;
            frame.ip = frame_virtual_return_address(this_fp);
            frame.stack_holder = frame.stack_holder;
            frame.kind = frame.kind;

            if frame.ip == scm_program_code(scm_virtual_machine().boot_continuation) {
                continue;
            }

            return Some(frame);
        }
    }
}

extern "C-unwind" fn frame_p(_thread: &mut Thread, val: &mut Value) -> Value {
    Value::encode_bool_value(val.is_frame())
}

extern "C-unwind" fn frame_address(thread: &mut Thread, frame: &mut Value) -> Value {
    if !frame.is_frame() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "frame-address",
            0,
            "frame",
            *frame,
            1,
            &[frame],
        )
    }

    let frame = frame.frame();
    thread.make_integer(frame.fp_offset as i64)
}

extern "C-unwind" fn frame_stack_pointer(thread: &mut Thread, frame: &mut Value) -> Value {
    if !frame.is_frame() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "frame-stack-pointer",
            0,
            "frame",
            *frame,
            1,
            &[frame],
        )
    }

    let frame = frame.frame();
    thread.make_integer(frame.sp_offset as i64)
}

extern "C-unwind" fn frame_instruction_pointer(thread: &mut Thread, frame: &mut Value) -> Value {
    if !frame.is_frame() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "frame-instruction-pointer",
            0,
            "frame",
            *frame,
            1,
            &[frame],
        )
    }

    let frame = frame.frame();
    thread.make_integer(frame.ip as i64)
}

extern "C-unwind" fn frame_return_address(thread: &mut Thread, frame: &mut Value) -> Value {
    if !frame.is_frame() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "frame-return-address",
            0,
            "frame",
            *frame,
            1,
            &[frame],
        )
    }

    let frame = frame.frame();
    unsafe { thread.make_integer(frame_virtual_return_address(frame.fp()) as i64) }
}

extern "C-unwind" fn subr_frame_dynamic_link(thread: &mut Thread, frame: &mut Value) -> Value {
    if !frame.is_frame() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "frame-dynamic-link",
            0,
            "frame",
            *frame,
            1,
            &[frame],
        )
    }

    let frame = frame.frame();
    unsafe { thread.make_integer(frame_dynamic_link(frame.fp()) as i64) }
}

pub extern "C-unwind" fn subr_frame_previous(thread: &mut Thread, frame: &mut Value) -> Value {
    if !frame.is_frame() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "frame-previous",
            0,
            "frame",
            *frame,
            1,
            &[frame],
        )
    }
    unsafe {
        let Some(prev) = frame.frame().previous() else {
            return Value::encode_bool_value(false);
        };

        thread.make_frame(prev.kind, &prev)
    }
}

extern "C-unwind" fn subr_frame_num_locals(thread: &mut Thread, frame: &mut Value) -> Value {
    if !frame.is_frame() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "frame-num-locals",
            0,
            "frame",
            *frame,
            1,
            &[frame],
        )
    }

    let frame = frame.frame();
    thread.make_integer(frame.num_locals() as i64)
}

extern "C-unwind" fn frame_procedure_name(thread: &mut Thread, frame: &mut Value) -> Value {
    if !frame.is_frame() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "frame-procedure-name",
            0,
            "frame",
            *frame,
            1,
            &[frame],
        )
    }

    let frame = frame.frame();
    let vm = scm_virtual_machine();
    let ip = frame.ip;
    let name = vm.images.program_name(ip);

    if let Some(name) = name {
        name
    } else {
        Value::encode_bool_value(false)
    }
}

extern "C-unwind" fn frame_source_location(thread: &mut Thread, frame: &mut Value) -> Value {
    if !frame.is_frame() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "frame-source-location",
            0,
            "frame",
            *frame,
            1,
            &[frame],
        )
    }

    let frame = frame.frame();
    let vm = scm_virtual_machine();
    let ip = frame.ip;
    let source = vm.images.debug_info(ip);

    if let Some((name, line, column)) = source {

        let line = thread.make_integer(line as i64);
        let column = thread.make_integer(column as i64);

        let vector = thread.make_vector::<false>(3, Value::encode_undefined_value());
        scm_vector_set(vector, thread, 0, name);
        scm_vector_set(vector, thread, 1, line);
        scm_vector_set(vector, thread, 2, column);

        vector
    } else {
        Value::encode_bool_value(false)
    }
}

extern "C-unwind" fn frame_local_ref(
    thread: &mut Thread,
    frame: &mut Value,
    ix: &mut Value,
) -> Value {
    if !frame.is_frame() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "frame-local-ref",
            0,
            "frame",
            *frame,
            1,
            &[frame, ix],
        )
    }

    if !arith::exact_nonnegative_integer_p(*ix) {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "frame-local-ref",
            1,
            "exact nonnegative integer",
            *ix,
            2,
            &[frame, ix],
        )
    }

    if !ix.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "frame-local-ref",
            1,
            "exact nonnegative 32 bit integer",
            *ix,
            2,
            &[frame, ix],
        )
    }

    let fp = frame.frame().fp();
    let sp = frame.frame().sp();

    unsafe {
        let i = ix.get_int32() as isize;

        if i < frame_num_locals(fp, sp) {
            frame_slot(fp, i as usize).read().as_value
        } else {
            invalid_argument_violation::<{ usize::MAX }>(
                thread,
                "frame-local-ref",
                "index out of range",
                *ix,
                1,
                2,
                &[frame, ix],
            )
        }
    }
}

extern "C-unwind" fn frame_local_set(
    thread: &mut Thread,
    frame: &mut Value,
    ix: &mut Value,
    val: &mut Value,
) -> Value {
    if !frame.is_frame() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "frame-local-set!",
            0,
            "frame",
            *frame,
            1,
            &[frame, ix, val],
        )
    }

    if !arith::exact_nonnegative_integer_p(*ix) {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "frame-local-set!",
            1,
            "exact nonnegative integer",
            *ix,
            2,
            &[frame, ix, val],
        )
    }

    if !ix.is_int32() {
        wrong_type_argument_violation::<{ usize::MAX }>(
            thread,
            "frame-local-set!",
            1,
            "exact nonnegative 32 bit integer",
            *ix,
            2,
            &[frame, ix, val],
        )
    }

    let fp = frame.frame().fp();
    let sp = frame.frame().sp();

    unsafe {
        let i = ix.get_int32() as isize;

        if i < frame_num_locals(fp, sp) {
            frame_slot(fp, i as usize).write(StackElement {
                as_value: val.clone(),
            });
            Value::encode_undefined_value()
        } else {
            invalid_argument_violation::<{ usize::MAX }>(
                thread,
                "frame-local-set!",
                "index out of range",
                *ix,
                1,
                2,
                &[frame, ix, val],
            )
        }
    }
}

pub(crate) fn init_frame_builtins() {
    scm_define_subr("frame?", 1, 0, 0, Subr::F1(frame_p));
    scm_define_subr("frame-address", 1, 0, 0, Subr::F1(frame_address));
    scm_define_subr(
        "frame-stack-pointer",
        1,
        0,
        0,
        Subr::F1(frame_stack_pointer),
    );
    scm_define_subr(
        "frame-instruction-pointer",
        1,
        0,
        0,
        Subr::F1(frame_instruction_pointer),
    );
    scm_define_subr(
        "frame-return-address",
        1,
        0,
        0,
        Subr::F1(frame_return_address),
    );
    scm_define_subr(
        "frame-dynamic-link",
        1,
        0,
        0,
        Subr::F1(subr_frame_dynamic_link),
    );
    scm_define_subr("frame-previous", 1, 0, 0, Subr::F1(subr_frame_previous));
    scm_define_subr("frame-num-locals", 1, 0, 0, Subr::F1(subr_frame_num_locals));
    scm_define_subr(
        "frame-procedure-name",
        1,
        0,
        0,
        Subr::F1(frame_procedure_name),
    );
    scm_define_subr("frame-local-ref", 2, 0, 0, Subr::F2(frame_local_ref));
    scm_define_subr("frame-local-set!", 3, 0, 0, Subr::F3(frame_local_set));
    scm_define_subr(
        "frame-source-location",
        1,
        0,
        0,
        Subr::F1(frame_source_location),
    );
}
