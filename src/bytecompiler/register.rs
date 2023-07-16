use std::backtrace;

use crate::bytecode::virtual_register::{virtual_register_for_local, VirtualRegister};

pub struct RegisterAllocator {
    registers: Vec<bool>,
    first_free: u16,
    stack_top: u16,
    // The index of the largest used register + 1 (e.g. the stack size required for the function)
    stack_size: u16,
}

impl RegisterAllocator {
    pub fn new() -> Self {
        Self {
            registers: vec![false; 256],
            first_free: 0,
            stack_top: 0,
            stack_size: 0,
        }
    }

    /// Returns the index immediately after the largest used register index
    pub fn stack_top(&self) -> u16 {
        self.stack_top
    }

    /// Returns the index of the largest ever used register + 1 (e.g. the stack size required for
    /// the function)
    pub fn stack_size(&self) -> u16 {
        self.stack_size
    }

    pub fn allocate(&mut self) -> Option<VirtualRegister> {
        if self.first_free < self.registers.len() as u16 {
            let register = self.first_free as u16;
            self.registers[register as usize] = true;

            if self.first_free == self.stack_top {
                self.stack_top += 1;
                self.stack_size = self.stack_size.max(self.stack_top);
            }

            let mut i = self.first_free;

            self.first_free = loop {
                if i >= self.registers.len() as u16 {
                    self.registers
                        .resize((self.registers.len() as f64 * 1.3) as _, false);
                    break i;
                }

                if !self.registers[i as usize] {
                    break i;
                }

                i += 1;
            };

            Some(virtual_register_for_local(register as _))
        } else {
            None
        }
    }

    pub fn free(&mut self, register: VirtualRegister) {
        println!(
            "free {} {}",
            register,
            std::backtrace::Backtrace::force_capture()
        );
        if register.to_local() as u16 + 1 == self.stack_top {
            self.pop_to(register.to_local() as _);
        } else {
            self.registers[register.to_local() as usize] = false;
            self.first_free = self.first_free.min(register.0 as u16);
        }
    }

    pub fn push(&mut self, size: u16) -> Option<VirtualRegister> {
        if size == 0 {
            None
        } else {
            let rbegin = self.stack_top;
            if rbegin + size > self.registers.len() as u16 {
                self.registers
                    .resize(((rbegin + size) as f64 * 1.3) as usize, false);
            }
            for i in rbegin..rbegin + size {
                self.registers[i as usize] = true;
            }

            if self.first_free == self.stack_top {
                self.first_free += size;
            }

            self.stack_top += size;
            self.stack_size = self.stack_size.max(self.stack_top);

            Some(virtual_register_for_local(rbegin as _))
        }
    }

    /// Free all registers past the given register, making the given register the new top of the
    /// stack. If the given register is >= to the current top, this will have no effect.
    pub fn pop_to(&mut self, new_top: u16) {
        if self.stack_top > new_top {
            for i in new_top..self.stack_top {
                self.registers[i as usize] = false;
            }

            self.stack_top = new_top;
            self.first_free = self.first_free.min(new_top);

            for i in (self.first_free..self.stack_top).rev() {
                if self.registers[i as usize] {
                    break;
                }

                self.stack_top = i;
            }
        }
    }
}
