use super::encode::{Decode, Encode, InstructionStream};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct u24(pub [u8; 3]);

impl u24 {
    pub const fn new(value: u32) -> Self {
        // LE bytes
        let bytes = value.to_le_bytes();
        Self([bytes[0], bytes[1], bytes[2]])
    }

    pub const fn value(&self) -> u32 {
        u32::from_le_bytes([self.0[0], self.0[1], self.0[2], 0])
    }

    pub const fn fits(value: u32) -> bool {
        value < (1 << 24)
    }
}

impl std::fmt::Display for u24 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value())
    }
}

impl Encode for u24 {
    #[inline(always)]
    fn write(&self, gen: &mut impl InstructionStream) {
        for i in 0..3 {
            gen.write_u8(self.0[i]);
        }
    }
}

impl Decode for u24 {
    #[inline(always)]
    unsafe fn read(stream: *const u8) -> Self {
        Self([*stream, *stream.add(1), *stream.add(2)])
    }
}
