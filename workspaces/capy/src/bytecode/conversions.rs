use std::marker::PhantomData;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, std::marker::ConstParamTy)]
#[repr(u8)]
pub enum OpcodeSize {
    Narrow = 1,
    Wide16 = 2,
    Wide32 = 4,
}

pub struct TypeBySize<const SIZE: OpcodeSize>;

pub trait TypeBySizeTrait {
    type SignedType: num::Signed + num::Integer + num::ToPrimitive + num::FromPrimitive;
    type UnsignedType: num::Unsigned + num::Integer + num::ToPrimitive + num::FromPrimitive;
}

impl TypeBySizeTrait for TypeBySize<{ OpcodeSize::Narrow }> {
    type SignedType = i8;
    type UnsignedType = u8;
}

impl TypeBySizeTrait for TypeBySize<{ OpcodeSize::Wide16 }> {
    type SignedType = i16;
    type UnsignedType = u16;
}

impl TypeBySizeTrait for TypeBySize<{ OpcodeSize::Wide32 }> {
    type SignedType = i32;
    type UnsignedType = u32;
}

pub struct PaddingBySize<const SIZE: OpcodeSize>;

pub trait PaddingBySizeTrait {
    const VALUE: usize;
}

impl PaddingBySizeTrait for PaddingBySize<{ OpcodeSize::Narrow }> {
    const VALUE: usize = 0;
}

impl PaddingBySizeTrait for PaddingBySize<{ OpcodeSize::Wide16 }> {
    const VALUE: usize = 1;
}

impl PaddingBySizeTrait for PaddingBySize<{ OpcodeSize::Wide32 }> {
    const VALUE: usize = 1;
}

pub struct OpcodeIDWidthBySize<const SIZE: OpcodeSize>;

impl OpcodeIDWidthBySize<{ OpcodeSize::Narrow }> {
    pub const VALUE: usize = OpcodeSize::Narrow as usize;
}

impl OpcodeIDWidthBySize<{ OpcodeSize::Wide16 }> {
    pub const VALUE: usize = OpcodeSize::Narrow as usize;
}

impl OpcodeIDWidthBySize<{ OpcodeSize::Wide32 }> {
    pub const VALUE: usize = OpcodeSize::Narrow as usize;
}
/* 
pub trait OpcodeIDWidthBySizeTrait {
    type OpcodeType;
    const VALUE: usize;
}

impl OpcodeIDWidthBySizeTrait for OpcodeIDWidthBySize<{ OpcodeSize::Narrow }> {
    const VALUE: usize = OpcodeSize::Narrow as u8;
    type OpcodeType = u8;
}

impl OpcodeIDWidthBySizeTrait for OpcodeIDWidthBySize<{ OpcodeSize::Wide16 }> {
    const VALUE: usize = OpcodeSize::Wide16 as u8;
    type OpcodeType = u8;
}

impl OpcodeIDWidthBySizeTrait for OpcodeIDWidthBySize<{ OpcodeSize::Wide32 }> {
    const VALUE: usize = OpcodeSize::Wide32 as u8;
    type OpcodeType = u8;
}*/

pub struct Fits<T, const SIZE: OpcodeSize>(PhantomData<T>);
use num::FromPrimitive;

use super::{
    virtual_register::VirtualRegister, FIRST_CONSTANT_REGISTER_INDEX,
    FIRST_CONSTANT_REGISTER_INDEX16, FIRST_CONSTANT_REGISTER_INDEX32,
    FIRST_CONSTANT_REGISTER_INDEX8,
};
macro_rules! impl_fits_unsigned {
    ($($t: ty)*) => {
        $(
            impl Fits<$t, {OpcodeSize::Narrow}> {
                pub fn check(x: $t) -> bool {
                    <TypeBySize::<{OpcodeSize::Narrow}> as TypeBySizeTrait>::UnsignedType::from_u64(x as u64).is_some()
                }

                pub fn convert(x: $t) -> u8 {
                    <TypeBySize::<{OpcodeSize::Narrow}> as TypeBySizeTrait>::UnsignedType::from_u64(x as u64).unwrap() as u8
                }

                pub fn convert_back(x: u8) -> $t {
                    x as $t
                }
            }

            impl Fits<$t, {OpcodeSize::Wide16}> {
                pub fn check(x: $t) -> bool {
                    <TypeBySize::<{OpcodeSize::Wide16}> as TypeBySizeTrait>::UnsignedType::from_u64(x as u64).is_some()
                }

                pub fn convert(x: $t) -> u16 {
                    <TypeBySize::<{OpcodeSize::Wide16}> as TypeBySizeTrait>::UnsignedType::from_u64(x as u64).unwrap() as u16
                }

                pub fn convert_back(x: u16) -> $t {
                    x as $t
                }
            }

            impl Fits<$t, {OpcodeSize::Wide32}> {
                pub fn check(x: $t) -> bool {
                    <TypeBySize::<{OpcodeSize::Wide32}> as TypeBySizeTrait>::UnsignedType::from_u64(x as u64).is_some()
                }

                pub fn convert(x: $t) -> u32 {
                    <TypeBySize::<{OpcodeSize::Wide32}> as TypeBySizeTrait>::UnsignedType::from_u64(x as u64).unwrap() as u32
                }

                pub fn convert_back(x: u32) -> $t {
                    x as $t
                }
            }

        )*
    };
}

impl_fits_unsigned!(u8 u16 u32 u64 usize);

macro_rules! impl_fits_signed {
    ($($t: ty)*) => {
        $(
            impl Fits<$t, {OpcodeSize::Narrow}> {
                pub fn check(x: $t) -> bool {

                    <TypeBySize::<{OpcodeSize::Narrow}> as TypeBySizeTrait>::SignedType::from_i64(x as i64).is_some()
                }

                pub fn convert(x: $t) -> i8 {
                    <TypeBySize::<{OpcodeSize::Narrow}> as TypeBySizeTrait>::SignedType::from_i8(x as i8).unwrap()
                }

                pub fn convert_back(x: i8) -> $t {
                    <TypeBySize::<{OpcodeSize::Narrow}> as TypeBySizeTrait>::SignedType::from_i8(x as i8).unwrap() as $t
                }
            }

            impl Fits<$t, {OpcodeSize::Wide16}> {
                pub fn check(x: $t) -> bool {
                    <TypeBySize::<{OpcodeSize::Wide16}> as TypeBySizeTrait>::SignedType::from_i64(x as i64).is_some()
                }

                pub fn convert(x: $t) -> i16 {
                    <TypeBySize::<{OpcodeSize::Wide16}> as TypeBySizeTrait>::SignedType::from_i16(x as i16).unwrap()
                }

                pub fn convert_back(x: i16) -> $t {
                    <TypeBySize::<{OpcodeSize::Wide16}> as TypeBySizeTrait>::SignedType::from_i16(x as i16).unwrap() as $t
                }
            }

            impl Fits<$t, {OpcodeSize::Wide32}> {
                pub fn check(x: $t) -> bool {
                    <TypeBySize::<{OpcodeSize::Wide32}> as TypeBySizeTrait>::SignedType::from_i64(x as i64).is_some()
                }

                pub fn convert(x: $t) -> i32 {
                    <TypeBySize::<{OpcodeSize::Wide32}> as TypeBySizeTrait>::SignedType::from_i32(x as i32).unwrap()
                }

                pub fn convert_back(x: i32) -> $t {
                    <TypeBySize::<{OpcodeSize::Wide32}> as TypeBySizeTrait>::SignedType::from_i32(x as i32).unwrap() as $t
                }
            }


        )*
    };
}

impl_fits_signed!(i8 i16 i32 i64 isize);

// Narrow:
// -128..-1  local variables
//    0..15  arguments
//   16..127 constants
//

impl Fits<VirtualRegister, { OpcodeSize::Narrow }> {
    pub const fn check(r: VirtualRegister) -> bool {
        if r.is_constant() {
            return FIRST_CONSTANT_REGISTER_INDEX8 + r.to_constant_index() <= i8::MAX as i32;
        }

        r.offset() >= i8::MIN as i32 && r.offset() < FIRST_CONSTANT_REGISTER_INDEX8
    }

    pub const fn convert(r: VirtualRegister) -> i8 {
        if r.is_constant() {
            return (FIRST_CONSTANT_REGISTER_INDEX8 + r.to_constant_index()) as i8;
        }

        r.offset() as i8
    }

    pub const fn convert_back(r: i8) -> VirtualRegister {
        let i = r as i32;

        if i >= FIRST_CONSTANT_REGISTER_INDEX8 {
            return VirtualRegister::new(
                (i - FIRST_CONSTANT_REGISTER_INDEX8) + FIRST_CONSTANT_REGISTER_INDEX,
            );
        }

        VirtualRegister::new(i)
    }
}

// Wide16:
// -2**15..-1  local variables
//      0..64  arguments
//     64..2**15-1 constants
//
impl Fits<VirtualRegister, { OpcodeSize::Wide16 }> {
    pub const fn check(r: VirtualRegister) -> bool {
        if r.is_constant() {
            return FIRST_CONSTANT_REGISTER_INDEX16 + r.to_constant_index() <= i16::MAX as i32;
        }

        r.offset() >= i16::MIN as i32 && r.offset() < FIRST_CONSTANT_REGISTER_INDEX16
    }

    pub const fn convert(r: VirtualRegister) -> i16 {
        if r.is_constant() {
            return (FIRST_CONSTANT_REGISTER_INDEX16 + r.to_constant_index()) as i16;
        }

        r.offset() as i16
    }

    pub const fn convert_back(r: i16) -> VirtualRegister {
        let i = r as i32;

        if i >= FIRST_CONSTANT_REGISTER_INDEX16 {
            return VirtualRegister::new(
                (i - FIRST_CONSTANT_REGISTER_INDEX16) + FIRST_CONSTANT_REGISTER_INDEX,
            );
        }

        VirtualRegister::new(i)
    }
}

impl Fits<VirtualRegister, { OpcodeSize::Wide32 }> {
    pub const fn check(r: VirtualRegister) -> bool {
        if r.is_constant() {
            return FIRST_CONSTANT_REGISTER_INDEX32 + r.to_constant_index() <= i32::MAX as i32;
        }

        r.offset() >= i32::MIN as i32 && r.offset() < FIRST_CONSTANT_REGISTER_INDEX32
    }

    pub const fn convert(r: VirtualRegister) -> i32 {
        if r.is_constant() {
            return (FIRST_CONSTANT_REGISTER_INDEX32 + r.to_constant_index()) as i32;
        }

        r.offset() as i32
    }

    pub const fn convert_back(r: i32) -> VirtualRegister {
        let i = r as i32;

        if i >= FIRST_CONSTANT_REGISTER_INDEX32 {
            return VirtualRegister::new(
                (i - FIRST_CONSTANT_REGISTER_INDEX32) + FIRST_CONSTANT_REGISTER_INDEX,
            );
        }

        VirtualRegister::new(i)
    }
}
