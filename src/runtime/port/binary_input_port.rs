use crate::runtime::{
    object::{ObjectHeader, Type},
    value::Value,
};
use rsgc::{
    prelude::{Allocation, Handle, Object},
    sync::mutex::RawMutex,
    system::arraylist::ArrayList,
    thread::Thread,
};
use url::Url;

use std::io::Read;

#[repr(C)]
pub struct BinaryInputPort {
    pub(crate) header: ObjectHeader,
    pub(crate) lock: RawMutex,
    pub(crate) buffer: ArrayList<u8>,
    pub(crate) buffer_size: usize,
    pub(crate) index: usize,
    pub(crate) eof: bool,
    pub(crate) input: Option<Box<dyn Read>>,
    pub(crate) path: Option<Url>,
    pub(crate) is_aborted: Box<dyn Fn() -> bool>,
}

impl Object for BinaryInputPort {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.buffer.trace(visitor);
    }
}
impl Allocation for BinaryInputPort {
    const DESTRUCTIBLE: bool = true;
    const FINALIZE: bool = true;
}

impl Drop for BinaryInputPort {
    fn drop(&mut self) {
        if let Some(input) = self.input.take() {
            drop(input);
        }
    }
}

impl BinaryInputPort {
    fn readable(&mut self) -> Result<bool, std::io::Error> {
        if self.eof || (self.is_aborted)() {
            return Ok(false);
        } else if self.index >= self.buffer_size {
            let result = match self.input.as_mut().unwrap().read(&mut self.buffer) {
                Ok(v) => v,
                Err(e) => {
                    return Err(e);
                }
            };

            self.buffer_size = result;
            self.index = 0;
            if result == 0 {
                self.eof = true;
                return Ok(false);
            }
        } else {
            self.index = 0;
            self.eof = true;
            return Ok(false);
        }

        Ok(true)
    }

    pub fn next(&mut self) -> Result<Option<u8>, std::io::Error> {
        self.lock.lock(true);
        if !self.readable().map_err(|e| {
            self.lock.unlock();
            e
        })? {
            return Ok(None);
        }

        self.index += 1;

        let result = self.buffer[self.index - 1];
        self.lock.unlock();
        Ok(Some(result))
    }

    pub fn peek(&mut self) -> Result<Option<u8>, std::io::Error> {
        self.lock.lock(true);
        if !self.readable().map_err(|e| {
            self.lock.unlock();
            e
        })? {
            return Ok(None);
        }

        let result = self.buffer[self.index];
        self.lock.unlock();
        Ok(Some(result))
    }

    /// Reads up to `n` bytes into a new byte array. Returns None if all input is consumed
    /// or an error was encountered. Clients can disambiguate current state by checking value of `eof` field.
    pub fn read_many(&mut self, n: usize) -> Result<Option<Vec<u8>>, std::io::Error> {
        self.lock.lock(true);

        let mut res = Vec::with_capacity(n);

        for _ in 0..n {
            if !self.readable().map_err(|e| {
                self.lock.unlock();
                e
            })? {
                self.lock.unlock();
                return if res.len() == 0 || !self.eof {
                    Ok(None)
                } else {
                    Ok(Some(res))
                };
            }

            res.push(self.buffer[self.index]);
            self.index += 1;
        }
        self.lock.unlock();
        Ok(Some(res))
    }

    pub fn read_into(
        &mut self,
        target: &mut [u8],
        start: usize,
        end: usize,
    ) -> Result<Option<usize>, std::io::Error> {
        self.lock.lock(true);

        if start >= target.len() || start >= end {
            self.lock.unlock();
            return Ok(Some(0));
        }

        let to = if end > target.len() {
            target.len()
        } else {
            end
        };

        for i in start..to {
            if !self.readable().map_err(|e| {
                self.lock.unlock();
                e
            })? {
                self.lock.unlock();
                if i == start || !self.eof {
                    return Ok(None);
                } else {
                    return Ok(Some(i - start + 1));
                }
            }

            target[i] = self.buffer[self.index];
            self.index += 1;
        }

        self.lock.unlock();
        Ok(Some(to - start))
    }

    pub fn read_might_block(&self) -> bool {
        self.lock.lock(true);

        if self.eof {
            false
        } else if self.index >= self.buffer_size {
            true
        } else {
            false
        }
    }

    pub fn close(&mut self) {
        self.lock.lock(true);
        let _ = self.input.take();
        self.lock.unlock();
    }

    pub fn is_eof(&self) -> bool {
        self.lock.lock(true);
        let result = self.eof;
        self.lock.unlock();
        result
    }

    pub fn is_closed(&self) -> bool {
        self.lock.lock(true);
        let result = self.input.is_none();
        self.lock.unlock();
        result
    }

    pub fn url(&self) -> Option<&Url> {
        self.path.as_ref() // constant, no need to lock
    }

    pub fn from_bytes(
        data: &[u8],
        abortion_callback: Option<Box<dyn Fn() -> bool>>,
    ) -> Handle<BinaryInputPort> {
        let buffer = ArrayList::from_slice(Thread::current(), data);

        Thread::current().allocate(BinaryInputPort {
            header: ObjectHeader::new(Type::BinaryInputPort),
            lock: RawMutex::INIT,
            buffer,
            buffer_size: data.len(),
            index: 0,
            eof: false,
            input: None,
            path: None,
            is_aborted: abortion_callback.unwrap_or_else(|| Box::new(|| false)),
        })
    }

    pub fn from_url(
        url: &Url,
        capacity: usize,
        abortion_callback: Option<Box<dyn Fn() -> bool>>,
    ) -> Result<Handle<Self>, std::io::Error> {
        match url.scheme() {
            "http" | "https" => {
                todo!("web urls")
            }

            "file" => {
                let Ok(path) = url.to_file_path() else {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidInput,
                        "Invalid file path",
                    ));
                };

                if !path.exists() {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::NotFound,
                        "File not found",
                    ));
                }

                let input = std::fs::File::open(path)?;

                Self::from_input(
                    Box::new(input),
                    Some(url.clone()),
                    capacity,
                    abortion_callback,
                    || true,
                )
            }

            _ => {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    "Unsupported scheme",
                ));
            }
        }
    }

    pub fn from_input(
        mut input: Box<dyn Read>,
        path: Option<Url>,
        capacity: usize,
        abortion_callback: Option<Box<dyn Fn() -> bool>>,
        open_successful: impl FnOnce() -> bool,
    ) -> Result<Handle<Self>, std::io::Error> {
        let mut buffer = ArrayList::from_init(Thread::current(), capacity, 0);

        if !open_successful() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "Failed to open input",
            ));
        }

        let result = input.read(&mut buffer)?;

        Ok(Thread::current().allocate(BinaryInputPort {
            header: ObjectHeader::new(Type::BinaryInputPort),
            lock: RawMutex::INIT,
            buffer,
            buffer_size: result,
            index: 0,
            eof: result == 0,
            input: Some(input),
            path,
            is_aborted: abortion_callback.unwrap_or_else(|| Box::new(|| false)),
        }))
    }
}

impl Value {
    pub fn is_binary_input_port(self) -> bool {
        self.is_xtype(Type::BinaryInputPort)
    }

    pub fn binary_input_port(self) -> Handle<BinaryInputPort> {
        unsafe {
            debug_assert!(self.is_binary_input_port());
            std::mem::transmute(self)
        }
    }
}
