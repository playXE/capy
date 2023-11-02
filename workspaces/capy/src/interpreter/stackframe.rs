/* Stack frames
   ------------

   Stack frame layout
   ------------------

   | ...                          |
   +==============================+ <- fp + 2 = SCM_FRAME_PREVIOUS_SP (fp)
   | Virtual return address (vRA) |
   +------------------------------+
   | Dynamic link                 |
   +==============================+ <- fp
   | Local 0                      |
   +------------------------------+
   | Local 1                      |
   +------------------------------+
   | ...                          |
   +------------------------------+
   | Local N-1                    |
   \------------------------------/ <- sp

   The stack grows down.
   */

use crate::runtime::value::Value;

use super::StackElement;


#[inline(always)]
pub unsafe fn scm_frame_previous_sp(fp: *mut StackElement) -> *mut StackElement {
    fp.offset(2)
}
#[inline(always)]
pub unsafe fn scm_frame_virtual_return_address(fp: *mut StackElement) -> *const u8{
    fp.offset(1).read().as_vcode
}
#[inline(always)]
pub unsafe fn scm_set_frame_virtual_return_address(fp: *mut StackElement, vra: *const u8) {
    fp.offset(1).write(StackElement { as_vcode: vra })
}
#[inline(always)]
pub unsafe fn scm_frame_dynamic_link(fp: *mut StackElement) -> *mut StackElement {
    let dl = fp.offset(0).read().as_value.get_int32();
    fp.offset(dl as isize)
}
#[inline(always)]
pub unsafe fn scm_set_frame_dynamic_link(fp: *mut StackElement, dl: *mut StackElement) {
    let dl = dl.offset_from(fp);
    fp.offset(0).write(StackElement { as_value: Value::new(dl as i32) })
}

#[inline(always)]
pub unsafe fn scm_frame_slot(fp: *mut StackElement, i: usize) -> *mut StackElement {
    fp.sub(i).sub(1)
}

#[inline(always)]
pub unsafe fn scm_frame_local(fp: *mut StackElement, i: usize) -> Value {
    scm_frame_slot(fp, i).read().as_value
}

#[inline(always)]
pub unsafe fn scm_set_frame_local(fp: *mut StackElement, i: usize, v: Value) {
    scm_frame_slot(fp, i).write(StackElement { as_value: v })
}

#[inline(always)]
pub unsafe fn scm_frame_num_locals(fp: *mut StackElement, sp: *mut StackElement) -> usize {
    scm_frame_previous_sp(fp).offset_from(sp) as usize
}