use crate::runtime::object::ObjectHeader;
use rsgc::{
    prelude::{Allocation, Object},
    sync::mutex::RawMutex,
    system::arraylist::ArrayList,
    thread::Thread,
};
use std::io::Write;
use url::Url;

#[repr(C)]
pub struct BinaryOutputPort {
    pub(crate) header: ObjectHeader,
    pub(crate) lock: RawMutex,
    pub(crate) buffer: ArrayList<u8>,
    pub(crate) next: usize,
    pub(crate) output: Option<Box<dyn Write>>,
    pub(crate) url: Option<Url>,
}

impl Object for BinaryOutputPort {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.buffer.trace(visitor);
    }
}

impl Allocation for BinaryOutputPort {
    const DESTRUCTIBLE: bool = true;
    const FINALIZE: bool = true;
}

impl Drop for BinaryOutputPort {
    fn drop(&mut self) {
        if let Some(output) = self.output.take() {
            drop(output);
        }
    }
}

impl BinaryOutputPort {
    pub fn write(&mut self, byte: u8) -> Result<(), std::io::Error> {
        self.lock.lock(true);
        let result = self.write_byte(byte);
        self.lock.unlock();
        result
    }

    pub fn write_from(&mut self, buffer: &[u8]) -> Result<(), std::io::Error> {
        self.lock.lock(true);
        for byte in buffer {
            self.write_byte(*byte)?;
        }
        self.lock.unlock();
        Ok(())
    }

    pub fn current_buffer<'a>(&'a self) -> &'a [u8] {
        self.lock.lock(true);
        let result = &self.buffer[..self.next];
        self.lock.unlock();
        result
    }

    /// Returns true if there is at least one byte that can be written into the buffer
    fn writeable(&mut self) -> Result<(), std::io::Error> {
        if self.next < self.buffer.len() || self.output.is_none() {
            return Ok(());
        }

        self.flush_buffer()
    }

    fn write_byte(&mut self, byte: u8) -> Result<(), std::io::Error> {
        self.writeable()?;

        if self.next < self.buffer.len() {
            self.buffer[self.next] = byte;
        } else {
            self.buffer.push(Thread::current(), byte);
        }

        self.next += 1;

        Ok(())
    }

    fn flush_buffer(&mut self) -> Result<(), std::io::Error> {
        if let Some(output) = self.output.as_mut().filter(|_| self.next > 0) {
            output.write(&self.buffer[..self.next])?;
            self.next = 0;
        }
        Ok(())
    }
}
