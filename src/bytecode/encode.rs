use crate::bytecompiler::bytecodegenerator::BytecodeGenerator;
pub trait Encode {
    fn write(&self, gen: &mut BytecodeGenerator);
}

pub trait Decode: Sized {
    unsafe fn read(stream: *const u8) -> Self;
}

impl Encode for u8 {
    fn write(&self, gen: &mut BytecodeGenerator) {
        gen.write_u8(*self);
    }
}

impl Encode for u16 {
    fn write(&self, gen: &mut BytecodeGenerator) {
        gen.write_u16(*self);
    }
}

impl Encode for i32 {
    fn write(&self, gen: &mut BytecodeGenerator) {
        gen.write_u32(*self as u32);
    }
}

impl Encode for i16 {
    fn write(&self, gen: &mut BytecodeGenerator) {
        gen.write_u16(*self as u16);
    }
}

impl Encode for i8 {
    fn write(&self, gen: &mut BytecodeGenerator) {
        gen.write_u8(*self as u8);
    }
}