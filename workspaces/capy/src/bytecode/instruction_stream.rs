use super::opcodes::BaseInstruction;

pub struct InstructionStream {
    instructions: Vec<u8>,
}

impl InstructionStream {
    pub fn at(&self, index: usize) -> InstructionStreamRef {
        InstructionStreamRef {
            instructions: &self.instructions,
            index,
        }
    }

    pub fn at_mut(&mut self, index: usize) -> InstructionStreamMut {
        InstructionStreamMut {
            instructions: &mut self.instructions,
            index,
        }
    }

    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    pub fn contains(&self, inst: *const BaseInstruction) -> bool {
        let start = self.instructions.as_ptr() as usize;
        let end = start + self.instructions.len();
        let inst = inst as usize;

        inst >= start && inst < end
    }

    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }
}

impl std::ops::Deref for InstructionStream {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.instructions
    }
}

impl std::ops::DerefMut for InstructionStream {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.instructions
    }
}

pub struct InstructionStreamRef<'a> {
    instructions: &'a Vec<u8>,
    index: usize,
}

impl<'a> InstructionStreamRef<'a> {
    fn unwrap(&self) -> *const BaseInstruction {
        &self.instructions[self.index] as *const u8 as _
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn is_valid(&self) -> bool {
        self.index < self.instructions.len()
    }

    pub fn ptr(&self) -> *const BaseInstruction {
        self.unwrap()
    }

    pub fn bytes(&self) -> &[u8] {
        unsafe {
            std::slice::from_raw_parts(self.unwrap() as *const u8, (*self.unwrap()).size())
        }
    }
}

impl<'a> PartialEq for InstructionStreamRef<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.instructions as *const _ == other.instructions as *const _ && self.index == other.index
    }
}

impl<'a> Eq for InstructionStreamRef<'a> {}

impl<'a> std::iter::Iterator for InstructionStreamRef<'a> {
    type Item = InstructionStreamRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_valid() {
            let ptr = self.unwrap();
            self.index += unsafe { (*ptr).size() };
            Some(InstructionStreamRef {
                instructions: self.instructions,
                index: self.index,
            })
        } else {
            None
        }
    }
}


pub struct InstructionStreamMut<'a> {
    instructions: &'a mut Vec<u8>,
    index: usize,
}

impl<'a> InstructionStreamMut<'a> {
    fn unwrap(&mut self) -> *mut BaseInstruction {
        &mut self.instructions[self.index] as *mut u8 as _
    }

    pub fn is_valid(&self) -> bool {
        self.index < self.instructions.len()
    }

    pub fn ptr(&mut self) -> *mut BaseInstruction {
        self.unwrap()
    }

    pub fn next(&mut self) -> Option<*mut BaseInstruction> {
        if self.is_valid() {
            let ptr = self.unwrap();
            self.index += unsafe { (*ptr).size() };
            Some(ptr)
        } else {
            None
        }
    }
}

impl<'a> PartialEq for InstructionStreamMut<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.instructions as *const _ == other.instructions as *const _ && self.index == other.index
    }
}

impl<'a> Eq for InstructionStreamMut<'a> {}


pub struct InstructionStreamWriter {
    stream: InstructionStream,
    position: usize,
    finalized: bool
}

impl std::ops::Deref for InstructionStreamWriter {
    type Target = InstructionStream;

    fn deref(&self) -> &Self::Target {
        &self.stream
    }
}

impl std::ops::DerefMut for InstructionStreamWriter {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.stream
    }
}

impl InstructionStreamWriter {
    pub fn new() -> Self {
        Self {
            stream: InstructionStream::new(),
            position: 0,
            finalized: false
        }
    }

    pub fn rewind(&mut self, r: &InstructionStreamRef) {
        self.instructions.resize(r.index, 0);
        self.position = r.index;
    }

    pub fn finalize(&mut self) -> Vec<u8> {
        self.finalized = true;
        self.instructions.shrink_to_fit();
        std::mem::take(&mut self.instructions)
    }

    pub fn const_ref(&self) -> InstructionStreamRef {
        InstructionStreamRef {
            instructions: &self.instructions,
            index: self.position,
        }
    }

    pub fn seek(&mut self, index: usize) {
        self.position = index;
    }

    pub fn position(&self) -> usize {
        self.position
    }

    pub fn set_instruction_buffer(&mut self, buffer: Vec<u8>) {
        self.instructions = buffer;
    }
}   

pub trait InstructionWrite<T> {
    fn write(&mut self, inst: T);
}

impl InstructionWrite<u8> for InstructionStreamWriter {
    fn write(&mut self, byte: u8) {
        assert!(!self.finalized);
        if self.position < self.instructions.len() {
            let pos = self.position;
            self.instructions[pos] = byte;
        } else {
            self.instructions.push(byte);
        }

        self.position += 1;
    }
}

impl InstructionWrite<u16> for InstructionStreamWriter {
    fn write(&mut self, short: u16) {
        let bytes = short.to_ne_bytes();
        self.write(bytes[0]);
        self.write(bytes[1]);
    }
}

impl InstructionWrite<u32> for InstructionStreamWriter {
    fn write(&mut self, int: u32) {
        let bytes = int.to_ne_bytes();
        self.write(bytes[0]);
        self.write(bytes[1]);
        self.write(bytes[2]);
        self.write(bytes[3]);
    }
}

impl InstructionWrite<i8> for InstructionStreamWriter {
    fn write(&mut self, byte: i8) {
        self.write(byte as u8);
    }
}

impl InstructionWrite<i16> for InstructionStreamWriter {
    fn write(&mut self, short: i16) {
        self.write(short as u16);
    }
}

impl InstructionWrite<i32> for InstructionStreamWriter {
    fn write(&mut self, int: i32) {
        self.write(int as u32);
    }
}

