use crate::runtime::value::Value;

pub mod stackframe;


#[derive(Clone, Copy)]
#[repr(C)]
pub union StackElement {
    pub as_usize: usize,
    pub as_vcode: *const u8,
    pub as_value: Value,
    pub as_f64: f64,
    pub as_u64: u64,
    pub as_s64: i64,

    pub as_ptr: *const u8,
}   