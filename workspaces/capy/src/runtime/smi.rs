use super::value::{
    int_to_smi, PlatformSmiTagging, Tagged, Value, SMI_SHIFT_SIZE, SMI_TAG, SMI_TAG_SIZE, SMI_MIN_VALUE, SMI_MAX_VALUE,
};

/// Smi represents integer Numbers that can be stored in 31 bits.
/// Smis are immediate which means they are NOT allocated in the heap.
/// The ptr value has the following format: `[31 bit signed int] 0`
/// For long smis it has the following format:
/// ```text
///     [32 bit signed int] [31 bits zero padding] 0
/// ```
/// Smi stands for small integer.
pub struct Smi;

impl Smi {
    #[inline(always)]
    pub const fn from_i32(value: i32) -> Tagged<Smi> {
        Tagged::<Smi>::new(int_to_smi(value))
    }
    #[inline(always)]
    pub const fn from_isize(value: i32) -> Tagged<Smi> {
        let smi_shift_bits = SMI_TAG_SIZE + SMI_SHIFT_SIZE;
        Tagged::<Smi>::new(((value as usize) << smi_shift_bits) | SMI_TAG)
    }
    #[inline(always)]
    pub const fn to_i32(value: Tagged<Value>) -> i32 {
        Tagged::<Smi>::new(value.ptr()).value()
    }
    #[inline(always)]
    pub const fn to_u32_smi(value: Tagged<Smi>) -> Tagged<Smi> {
        if value.value() <= 0 {
            return Smi::from_i32(0);
        }

        Self::from_i32(value.value() as u32 as i32)
    }
    #[inline(always)]
    pub const fn is_valid(value: isize) -> bool {
        PlatformSmiTagging::is_valid_smi(value)
    }

    #[inline(always)]
    pub const fn zero() -> Tagged<Smi> {
        Self::from_i32(0)
    }

    pub const MIN_VALUE: i32 = SMI_MIN_VALUE as _;
    pub const MAX_VALUE: i32 = SMI_MAX_VALUE as _;
}
