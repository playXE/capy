pub trait Encode {
    fn write(&self, gen: &mut impl InstructionStream);
}

pub trait Decode: Sized {
    unsafe fn read(stream: *const u8) -> Self;
    unsafe fn read_ref<'a>(stream: *const u8) -> &'a Self {
        &*stream.cast::<Self>()
    }
}

impl Encode for u8 {
    fn write(&self, gen: &mut impl InstructionStream) {
        gen.write_u8(*self);
    }
}

impl Encode for u16 {
    fn write(&self, gen: &mut impl InstructionStream) {
        gen.write_u16(*self);
    }
}

impl Encode for i32 {
    fn write(&self, gen: &mut impl InstructionStream) {
        gen.write_u32(*self as u32);
    }
}

impl Encode for i16 {
    fn write(&self, gen: &mut impl InstructionStream) {
        gen.write_u16(*self as u16);
    }
}

impl Encode for i8 {
    fn write(&self, gen: &mut impl InstructionStream) {
        gen.write_u8(*self as u8);
    }
}

impl Encode for u32 {
    fn write(&self, gen: &mut impl InstructionStream) {
        gen.write_u32(*self);
    }
}

impl Decode for u32 {
    unsafe fn read(stream: *const u8) -> Self {
        stream.cast::<u32>().read()
    }
}

impl Encode for bool {
    fn write(&self, gen: &mut impl InstructionStream) {
        gen.write_u8(*self as u8);
    }
}

impl Encode for u64 {
    fn write(&self, gen: &mut impl InstructionStream) {
        gen.write_u32(*self as u32);
        gen.write_u32((*self >> 32) as u32);
    }
}

impl Decode for u64 {
    unsafe fn read(stream: *const u8) -> Self {
        stream.cast::<u64>().read()
    }
}

pub trait InstructionStream {
    fn write_u8(&mut self, value: u8);
    fn write_u16(&mut self, value: u16);
    fn write_u32(&mut self, value: u32);
    fn write_finish(&mut self) {}
}
