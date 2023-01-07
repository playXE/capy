use std::{io::Read, net::TcpStream};

use crate::prelude::*;
pub enum BinaryInputSource {
    Null,
    File(std::fs::File),
    Stdin(std::io::Stdin),
    Tcp(TcpStream),
}

pub struct BytesSource {
    bytes: Handle<Array<u8>>,
    index: usize,
}

impl std::io::Read for BytesSource {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let mut i = 0;
        while i < buf.len() && self.index < self.bytes.len() {
            buf[i] = self.bytes[self.index];
            i += 1;
            self.index += 1;
        }
        Ok(i)
    }
}

impl std::io::Read for BinaryInputSource {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match self {
            BinaryInputSource::Null => Ok(0),
            BinaryInputSource::File(file) => file.read(buf),
            BinaryInputSource::Stdin(stdin) => stdin.read(buf),
            BinaryInputSource::Tcp(tcp) => tcp.read(buf),
        }
    }
}

struct BinaryInputData {
    buffer: Handle<Array<u8>>,
    buffer_size: usize,
    index: usize,
    eof: bool,
    input: BinaryInputSource,
    is_aborted: fn() -> bool,
}

impl BinaryInputData {
    fn readable(&mut self) -> bool {
        if self.eof || (self.is_aborted)() {
            return false;
        } else if self.index >= self.buffer_size {
            if let Ok(result) = self.input.read(&mut self.buffer) {
                self.buffer_size = result;
                self.index = 0;
                if result == 0 {
                    self.eof = true;
                    return false;
                }
            } else {
                self.index = 0;
                self.eof = true;
                return false;
            }
        }

        true
    }
}

pub struct BinaryInput {
    lock: Mutex<BinaryInputData>,
}

impl BinaryInput {
    /// A default abortion callback
    pub fn never_abort() -> bool {
        false
    }

    /// A default callback for checking if open is successful
    pub fn always_successful() -> bool {
        true
    }

    pub fn from_bytes(bytes: Handle<Array<u8>>, is_aborted: fn() -> bool) -> Self {
        Self {
            lock: Mutex::new(BinaryInputData {
                buffer: Array::new(Thread::current(), bytes.len(), |_, ix| bytes[ix]),
                buffer_size: 0,
                index: 0,
                eof: false,
                input: BinaryInputSource::Null,
                is_aborted,
            }),
        }
    }

    pub fn close(&mut self) {
        let mut data = self.lock.lock(true);

        if !matches!(data.input, BinaryInputSource::Null) {
            // dropping the input source will close the handles
            let _ = std::mem::replace(&mut data.input, BinaryInputSource::Null);
        }
    }

    pub fn next(&self) -> Option<u8> {
        let mut data = self.lock.lock(true);

        if !data.readable() {
            return None;
        }

        data.index += 1;
        Some(data.buffer[data.index - 1])
    }

    pub fn peek(&self) -> Option<u8> {
        let mut data = self.lock.lock(true);

        if !data.readable() {
            return None;
        }

        Some(data.buffer[data.index])
    }

    pub fn read(&self) -> Option<u8> {
        self.next()
    }

    pub fn read_many(&self, n: usize) -> Option<Handle<Array<u8>>> {
        let mut data = self.lock.lock(true);

        if n == 0 {
            return Some(Array::new(Thread::current(), 0, |_, _| unreachable!()));
        }

        let mut res = vec![];

        for _ in 0..n {
            if !data.readable() {
                if res.len() == 0 || data.eof {
                    return None;
                } else {
                    return Some(Array::new(Thread::current(), res.len(), |_, ix| res[ix]));
                }
            }

            res.push(data.buffer[data.index]);
            data.index += 1;
        }

        Some(Array::new(Thread::current(), res.len(), |_, ix| res[ix]))
    }

    pub fn read_into(&self, target: &mut [u8], start: usize, end: usize) -> Option<usize> {
        let mut data = self.lock.lock(true);

        if start >= target.len() || start >= end {
            return Some(0);
        }

        let to = if end > target.len() {
            target.len()
        } else {
            end
        };
        for i in start..to {
            if !data.readable() {
                return if i == start || !data.eof {
                    None 
                } else {
                    Some(i - start + 1)
                }
            }

            target[i] = data.buffer[data.index];
            data.index += 1;
        }

        Some(to - start)
    }
}
