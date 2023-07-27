use std::{cell::UnsafeCell, mem::size_of, ptr::null};

use mmtk::util::Address;

use crate::vm::thread::Thread;

use super::{super::runtime::object::ScmCellRef, virtual_memory::VirtualMemory};

pub struct HandleMemory {
    inner: UnsafeCell<HandleMemoryInner>,
}

impl HandleMemory {
    pub fn new() -> HandleMemory {
        HandleMemory {
            inner: UnsafeCell::new(HandleMemoryInner::new()),
        }
    }

    fn get_inner_mut(&self) -> &mut HandleMemoryInner {
        unsafe { &mut *self.inner.get() }
    }

    pub fn create_handle(&self, obj: ScmCellRef) -> Handle {
        let address = self.get_inner_mut().create_handle(obj.to_address());
        Handle(address.to_mut_ptr())
    }

    #[allow(dead_code)]
    #[cfg(test)]
    fn handle_address(&self, object_address: Address) {
        let inner = unsafe { &mut *self.inner.get() };
        inner.create_handle(object_address);
    }

    fn create_scope(&self) -> HandleScope {
        self.get_inner_mut().create_scope()
    }

    fn drop_scope(&self, scope: HandleScope) {
        self.get_inner_mut().drop_scope(scope)
    }

    pub fn iterate_for_gc(&self) -> HandleMemoryIter {
        self.raw_iterate()
    }

    fn raw_iterate(&self) -> HandleMemoryIter {
        let inner = self.get_inner_mut();

        HandleMemoryIter {
            mem: inner,
            next_block_idx: 0,
            next: Address::from_ptr(null::<()>()),
            limit: Address::from_ptr(null::<()>()),
        }
    }
}

pub struct HandleMemoryInner {
    /// All blocks, Box is important since HandleBlock
    /// is a big struct that needs to get moved/copied on resizes.
    blocks: Vec<VirtualMemory>,

    // Empty block that can be used for next HandleBlock allocation.
    empty_block: Option<VirtualMemory>,

    // Adress of next handle
    top: Address,
}

impl HandleMemoryInner {
    fn new() -> HandleMemoryInner {
        let reservation = allocate_block();
        let top = unsafe { get_block_first(Address::from_usize(reservation.start())) };

        HandleMemoryInner {
            blocks: vec![reservation],
            empty_block: None,
            top,
        }
    }

    #[inline(always)]
    fn create_handle(&mut self, object_address: Address) -> Address {
        let result = self.top;
        unsafe {
            *result.to_mut_ptr() = object_address;
        }

        self.top = result.add(size_of::<usize>());

        if is_handle_block_aligned(self.top) {
            self.create_handle_slow();
        }

        result
    }

    fn create_handle_slow(&mut self) {
        let reservation = std::mem::replace(&mut self.empty_block, None);
        let reservation = reservation.unwrap_or_else(|| allocate_block());
        self.top = unsafe { get_block_first(Address::from_usize(reservation.start())) };
        self.blocks.push(reservation);
    }

    #[inline(always)]
    fn create_scope(&mut self) -> HandleScope {
        HandleScope { last_top: self.top }
    }

    #[inline(always)]
    fn drop_scope(&mut self, scope: HandleScope) {
        if align_down_to_block_size(scope.last_top) == align_down_to_block_size(self.top) {
            assert!(scope.last_top <= self.top);
            self.top = scope.last_top;
        } else {
            self.drop_scope_slow(scope);
        }
    }

    fn drop_scope_slow(&mut self, scope: HandleScope) {
        self.top = scope.last_top;

        while !self.blocks.is_empty() {
            let block_start = self.blocks.last().expect("no element").start();

            if align_down_to_block_size(self.top).as_usize() == block_start {
                return;
            }

            // Drop memory reservation.
            let reservation = self.blocks.pop().expect("no element");

            if self.empty_block.is_none() {
                self.empty_block = Some(reservation);
            }
        }

        assert!(self.blocks.is_empty());
        assert_eq!(self.top, Address::from_ptr(null::<()>()));
    }
}

fn allocate_block() -> VirtualMemory {
    let reservation = VirtualMemory::allocate_aligned(
        HANDLE_BLOCK_SIZE,
        HANDLE_BLOCK_SIZE,
        false,
        "handle block",
    )
    .unwrap();
    unsafe {
        assert!(is_handle_block_aligned(Address::from_usize(
            reservation.start()
        )));
    }
    reservation
}

fn align_down_to_block_size(address: Address) -> Address {
    let aligned = address.as_usize() & !(HANDLE_BLOCK_SIZE - 1);
    unsafe { Address::from_usize(aligned) }
}

fn is_handle_block_aligned(block_start: Address) -> bool {
    block_start.is_aligned_to(HANDLE_BLOCK_SIZE)
}

fn get_block_limit(block_start: Address) -> Address {
    block_start.add(HANDLE_BLOCK_SIZE)
}

fn get_block_first(block_start: Address) -> Address {
    block_start.add(size_of::<usize>())
}

struct HandleScope {
    last_top: Address,
}

pub fn create_handle(obj: ScmCellRef) -> Handle {
    let thread = Thread::current();

    unsafe { thread.handles.assume_init_mut().create_handle(obj) }
}

const HANDLE_BLOCK_SIZE_BITS: usize = 16;
const HANDLE_BLOCK_SIZE: usize = 1 << HANDLE_BLOCK_SIZE_BITS;

#[allow(dead_code)]
struct BorderData {
    blocks: usize,
    element: usize,
}
#[repr(C)]
pub struct Handle(*mut ScmCellRef);
impl Handle {
    pub fn direct(self) -> ScmCellRef {
        unsafe { *self.0 }
    }

    pub fn direct_ptr(self) -> Address {
        self.raw_load()
    }

    // Internal method for dereferencing this handle, does not
    // check thread on purpose for testing purposes.
    fn raw_load(self) -> Address {
        unsafe { *self.0 }.to_address()
    }

    pub fn location(&self) -> Address {
        Address::from_ptr(self.0)
    }

    pub fn from_address(location: Address) -> Handle {
        Handle(location.to_mut_ptr())
    }
}

impl std::ops::Deref for Handle {
    type Target = ScmCellRef;

    fn deref(&self) -> &ScmCellRef {
        unsafe { &*self.0 }
    }
}

impl std::ops::DerefMut for Handle {
    fn deref_mut(&mut self) -> &mut ScmCellRef {
        unsafe { &mut *self.0 }
    }
}

// known limitation of #[derive(Copy, Clone)]
// traits need to be implemented manually
impl Copy for Handle {}
impl Clone for Handle {
    fn clone(&self) -> Handle {
        *self
    }
}

pub struct HandleMemoryIter<'a> {
    mem: &'a HandleMemoryInner,
    next_block_idx: usize,
    next: Address,
    limit: Address,
}

impl<'a> Iterator for HandleMemoryIter<'a> {
    type Item = Handle;

    fn next(&mut self) -> Option<Handle> {
        if self.next < self.limit {
            let current = Handle::from_address(self.next);
            self.next = self.next.add(size_of::<usize>());
            return Some(current);
        }

        if self.next_block_idx < self.mem.blocks.len() {
            let block_start =
                unsafe { Address::from_usize(self.mem.blocks[self.next_block_idx].start()) };
            let block_limit = get_block_limit(block_start);
            let first_handle = get_block_first(block_start);

            self.next = first_handle.add(size_of::<usize>());
            self.limit = if self.next_block_idx + 1 == self.mem.blocks.len() {
                // There should always be at least one handle in a block.
                assert!(block_start <= self.mem.top);
                assert!(self.mem.top < block_limit);
                self.mem.top
            } else {
                block_limit
            };

            self.next_block_idx += 1;

            return if first_handle < self.limit {
                Some(Handle::from_address(first_handle))
            } else {
                None
            };
        }

        None
    }
}

pub fn handle_scope<F: FnOnce() -> R, R>(f: F) -> R {
    let thread = Thread::current();

    let scope = unsafe { thread.handles.assume_init_mut().create_scope() };
    let result = f();
    unsafe { thread.handles.assume_init_mut().drop_scope(scope) };
    result
}
