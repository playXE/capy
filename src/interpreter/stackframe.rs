use crate::runtime::value::Value;

/// Stack frame layout
///
/// ```text
/// | ...                            |
/// +================================+ <- fp + 3 = previous sp
/// | Dynamic link                   |
/// +--------------------------------+
/// | Virtual return address (vRA)   |
/// +--------------------------------+
/// | Machine return address (mRA)   |
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
    pub mra: *const u32,
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
    fp.add(3)
}

#[inline]
pub unsafe fn frame_machine_return_address(fp: *mut StackElement) -> *const u8 {
    fp.add(0).read().as_mcode
}

#[inline]
pub unsafe fn set_frame_machine_return_address(fp: *mut StackElement, mra: *const u8) {
    fp.add(0).cast::<*const u8>().write(mra);
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
pub unsafe fn frame_dynamic_link(fp: *mut StackElement) -> *mut StackElement {
    let dl = fp.add(2).read().as_usize;
    fp.add(dl)
}

#[inline]
pub unsafe fn set_frame_dynamic_link(fp: *mut StackElement, dl: *mut StackElement) {
    debug_assert!(dl >= fp);
    fp.add(2).cast::<usize>().write(dl.offset_from(fp) as _);
}

#[inline]
pub unsafe fn frame_slot(fp: *mut StackElement, index: isize) -> *mut StackElement {
    fp.offset(index).sub(1)
}

#[inline]
pub unsafe fn frame_local<'a>(fp: *mut StackElement, index: isize) -> &'a mut Value {
    &mut (*frame_slot(fp, index)).as_value
}

#[inline]
pub unsafe fn frame_num_locals(fp: *mut StackElement, sp: *mut StackElement) -> isize {
    fp.offset_from(sp)
}
