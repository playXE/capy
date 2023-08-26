#![allow(dead_code)]
use std::{
    fmt::Debug,
    hash::Hash,
    mem::{size_of, transmute},
    ptr::{null_mut, NonNull},
    sync::atomic::{fence, AtomicBool, AtomicIsize, AtomicPtr, AtomicUsize, Ordering},
};

use mmtk::util::{conversions::raw_align_down, Address};

use crate::{
    runtime::value::Value,
    vm::{
        sync::{
            mutex::RawMutex,
            single_writer::{CriticalSection, SingleWriterSynchronizer},
        },
        thread::Thread,
    },
};

/// ObjStorage supports management of off-heap references to objects allocated
/// in the Scheme heap.  An ObjStorage object provides a set of Scheme object
/// references (obj values), which clients refer to via *mut Value handles to the
/// associated ObjStorage entries.  Clients allocate entries to create a
/// (possibly weak) reference to a Scheme object, use that reference, and release
/// the reference when no longer needed.
///
/// The garbage collector must know about all ObjStorage objects and their
/// reference strength.  ObjStorage provides the garbage collector with support
/// for iteration over all the allocated entries.
///
/// There are several categories of interaction with an ObjStorage object.
///
/// (1) allocation and release of entries, by the mutator or the VM.
/// (2) iteration by the garbage collector, possibly concurrent with mutator.
/// (3) iteration by other, non-GC, tools (only at safepoints).
/// (4) cleanup of unused internal storage, possibly concurrent with mutator.
///
/// A goal of ObjStorage is to make these interactions thread-safe, while
/// minimizing potential lock contention issues within and between these
/// categories.  In particular, support for concurrent iteration by the garbage
/// collector, under certain restrictions, is required.  Further, it must not
/// block nor be blocked by other operations for long periods.
///
/// Internally, ObjStorage is a set of Block objects, from which entries are
/// allocated and released.  A block contains an `[Value]` and a bitmask indicating
/// which entries are in use (have been allocated and not yet released).  New
/// blocks are constructed and added to the storage object when an entry
/// allocation request is made and there are no blocks with unused entries.
/// Blocks may be removed and deleted when empty.
///
/// There are two important (and somewhat intertwined) protocols governing
/// concurrent access to a storage object.  These are the Concurrent Iteration
/// Protocol and the Allocation Protocol.  See the ParState class for a
/// discussion of concurrent iteration and the management of thread
/// interactions for this protocol.  Similarly, see the allocate() function for
/// a discussion of allocation.

pub struct ObjStorage {
    inner: NonNull<ObjStorageInner>,
}

unsafe impl Send for ObjStorage {}
unsafe impl Sync for ObjStorage {}

impl ObjStorage {
    pub fn new(name: &'static str) -> Self {
        Self {
            inner: unsafe {
                NonNull::new_unchecked(Box::into_raw(Box::new(ObjStorageInner::new(name))))
            },
        }
    }

    pub fn allocate(&self) -> *mut Value {
        unsafe { (*self.inner.as_ptr()).allocate() }
    }

    pub fn release(&self, handle: *mut Value) {
        unsafe { (*self.inner.as_ptr()).release(handle) }
    }

    pub fn name(&self) -> &'static str {
        unsafe { (*self.inner.as_ptr()).name }
    }
}

impl Clone for ObjStorage {
    fn clone(&self) -> Self {
        unsafe {
            let inner = self.inner.as_ref();
            inner.refcount.fetch_add(1, Ordering::AcqRel);
            Self { inner: self.inner }
        }
    }
}

impl Drop for ObjStorage {
    fn drop(&mut self) {
        unsafe {
            let inner = self.inner.as_mut();
            if inner.refcount.fetch_sub(1, Ordering::AcqRel) == 1 {
                let _ = Box::from_raw(inner);
            }
        }
    }
}

struct ObjStorageInner {
    refcount: AtomicUsize,
    name: &'static str,
    active_array: *mut ActiveArray,
    allocation_list: AllocationList,
    deferred_updates: AtomicPtr<Block>,
    allocation_mutex: RawMutex,
    active_mutex: RawMutex,
    allocation_count: AtomicUsize,
    protect_active: SingleWriterSynchronizer,
    needs_cleanup: AtomicBool,
    concurrent_iteration_count: AtomicUsize,
}

#[repr(C)]
struct AllocationListEntry {
    prev: *mut Block,
    next: *mut Block,
}

#[repr(C)]
struct Block {
    data: [Value; size_of::<usize>() * 8],
    allocated_bitmask: AtomicUsize,
    owner_address: usize,
    memory: *mut u8,
    active_index: usize,
    allocation_list_entry: AllocationListEntry,
    deferred_updates_next: AtomicPtr<Block>,
    release_refcount: AtomicUsize,
}

impl Block {
    fn prev(&self) -> *mut Block {
        self.allocation_list_entry.prev
    }

    fn next(&self) -> *mut Block {
        self.allocation_list_entry.next
    }

    fn check_index(&self, index: usize) {
        debug_assert!(index < self.data.len());
    }

    fn get_pointer(&self, index: usize) -> *mut Value {
        self.check_index(index);
        unsafe { self.data.as_ptr().add(index) as _ }
    }

    fn allocated_bitmask(&self) -> usize {
        self.allocated_bitmask.load(Ordering::Relaxed)
    }

    fn bitmask_for_index(&self, index: usize) -> usize {
        self.check_index(index);
        1 << index
    }

    pub fn iterate<F>(&mut self, f: &mut F) -> bool 
    where F: FnMut(*mut Value) -> bool {
        let mut bitmask = self.allocated_bitmask();
        while bitmask != 0 {
            let index = bitmask.trailing_zeros() as usize;
            bitmask ^= self.bitmask_for_index(index);
            if !f(self.get_pointer(index)) {
                return false;
            }
        }

        true
    }
}

struct AllocationList {
    head: *mut Block,
    tail: *mut Block,
}

impl AllocationList {
    unsafe fn push_front(&mut self, block: *mut Block) {
        let old = self.head;
        if old.is_null() {
            self.head = block;
            self.tail = block;
        } else {
            (*block).allocation_list_entry.next = old;
            (*old).allocation_list_entry.prev = block;
            self.head = block;
        }
    }

    unsafe fn push_back(&mut self, block: *mut Block) {
        let old = self.tail;
        if old.is_null() {
            self.head = block;
            self.tail = block;
        } else {
            (*block).allocation_list_entry.prev = old;
            (*old).allocation_list_entry.next = block;
            self.tail = block;
        }
    }

    unsafe fn unlink(&mut self, block: *mut Block) {
        let block_entry = &mut (*block).allocation_list_entry;
        let prev_blk = block_entry.prev;
        let next_blk = block_entry.next;

        block_entry.prev = null_mut();
        block_entry.next = null_mut();

        if prev_blk.is_null() && next_blk.is_null() {
            self.head = null_mut();
            self.tail = null_mut();
        } else if prev_blk.is_null() {
            (*next_blk).allocation_list_entry.prev = null_mut();
            self.head = next_blk;
        } else if next_blk.is_null() {
            (*prev_blk).allocation_list_entry.next = null_mut();
            self.tail = prev_blk;
        } else {
            (*prev_blk).allocation_list_entry.next = next_blk;
            (*next_blk).allocation_list_entry.prev = prev_blk;
        }
    }

    unsafe fn contains(&self, block: *mut Block) -> bool {
        !(*block).next().is_null() || self.tail == block
    }
}

pub struct ActiveArray {
    size: usize,
    block_count: AtomicUsize,
    refcount: AtomicIsize,
    blocks: [*mut Block; 0],
}

impl ActiveArray {
    fn new(size: usize) -> Self {
        Self {
            size,
            block_count: AtomicUsize::new(0),
            refcount: AtomicIsize::new(0),
            blocks: [],
        }
    }

    fn create(size: usize) -> &'static mut Self {
        unsafe {
            let ptr = libc::malloc(size_of::<Self>() + size_of::<*mut Block>() * size) as *mut Self;
            ptr.write(Self::new(size));
            &mut *ptr
        }
    }

    fn block_count(&self) -> usize {
        self.block_count.load(Ordering::Relaxed)
    }

    fn block_count_acquire(&self) -> usize {
        self.block_count.load(Ordering::Acquire)
    }

    fn increment_refcount(&self) {
        self.refcount.fetch_add(1, Ordering::Relaxed);
    }

    fn decrement_refcount(&self) -> bool {
        self.refcount.fetch_sub(1, Ordering::Release) == 0
    }

    unsafe fn block_ptr(&self, index: usize) -> *mut *mut Block {
        self.blocks.as_ptr().add(index) as _
    }

    unsafe fn push(&self, block: *mut Block) -> bool {
        let index = self.block_count();
        if index < self.size {
            (*block).active_index = index;
            *self.block_ptr(index) = block;

            self.block_count.store(index + 1, Ordering::Release);
            true
        } else {
            false
        }
    }

    unsafe fn remove(&self, block: *mut Block) {
        let index = (*block).active_index;
        let last_index = self.block_count() - 1;
        let last_block = *self.block_ptr(last_index);
        (*last_block).active_index = index;
        self.block_count.store(last_index, Ordering::Release);
    }

    unsafe fn copy_from(&self, from: &Self) {
        let mut from_ptr = from.block_ptr(0);
        let mut to_ptr = self.block_ptr(0);

        for i in 0..from.block_count() {
            let block = *from_ptr;
            from_ptr = from_ptr.add(1);
            *to_ptr = block;
            to_ptr = to_ptr.add(1);
        }

        self.block_count
            .store(from.block_count(), Ordering::Release);
    }
}

const SECTION_SIZE: usize = size_of::<u8>() * 8;
const SECTION_COUNT: usize = size_of::<usize>() * 8;
const BLOCK_ALIGNMENT: usize = size_of::<Value>() * SECTION_SIZE;

impl Block {
    unsafe fn new(owner: &ObjStorageInner, memory: *mut u8) -> Self {
        Self {
            data: [Value::encode_empty_value(); size_of::<usize>() * 8],
            allocated_bitmask: AtomicUsize::new(0),
            owner_address: owner as *const _ as _,
            memory,
            active_index: 0,
            allocation_list_entry: AllocationListEntry {
                prev: null_mut(),
                next: null_mut(),
            },
            deferred_updates_next: AtomicPtr::new(null_mut()),
            release_refcount: AtomicUsize::new(0),
        }
    }

    const fn allocation_size() -> usize {
        size_of::<Self>() + BLOCK_ALIGNMENT - size_of::<usize>()
    }

    const fn allocation_alignment_shift() -> usize {
        BLOCK_ALIGNMENT.trailing_zeros() as usize
    }

    const fn is_full_bitmask(x: usize) -> bool {
        !x == 0
    }

    const fn is_empty_bitmask(x: usize) -> bool {
        x == 0
    }

    fn get_index(&self, ptr: *const Value) -> usize {
        unsafe { ptr.offset_from(self.get_pointer(0)) as _ }
    }

    fn bitmask_for_entry(&self, ptr: *const Value) -> usize {
        self.bitmask_for_index(self.get_index(ptr))
    }

    fn is_safe_to_delete(&self) -> bool {
        self.release_refcount.load(Ordering::Acquire) == 0
            && self.deferred_updates_next.load(Ordering::Acquire).is_null()
    }

    fn deferred_updates_next(&self) -> *mut Self {
        self.deferred_updates_next.load(Ordering::Relaxed)
    }

    fn set_deferred_updates_next(&self, next: *mut Self) {
        self.deferred_updates_next.store(next, Ordering::Relaxed);
    }

    fn contains(&self, ptr: *const Value) -> bool {
        unsafe {
            let base = self.get_pointer(0);
            base <= base && ptr < base.add(SECTION_COUNT)
        }
    }

    fn active_index(&self) -> usize {
        self.active_index
    }

    fn active_index_safe(&self) -> usize {
        unsafe {
            let atomic: &AtomicUsize = transmute(&self.active_index);
            atomic.load(Ordering::Relaxed)
        }
    }

    /// Merge new allocation bits into `allocated_bitmask`.  Only one thread at a
    /// time is ever allocating from a block, but other threads may concurrently
    /// release entries and clear bits in `allocated_bitmask`.
    /// precondition: `allocated_bitmask & add == 0`
    fn atomic_add_allocated(&self, add: usize) {
        let sum = self.allocated_bitmask.fetch_add(add, Ordering::Relaxed) + add;
        debug_assert_eq!(sum & add, add, "some already present: {:x}:{:x}", sum, add);
    }

    fn allocate(&self) -> *mut Value {
        let allocated = self.allocated_bitmask();
        assert!(!Self::is_full_bitmask(allocated));
        let index = (!allocated).trailing_zeros() as usize;
        self.atomic_add_allocated(self.bitmask_for_index(index));
        self.get_pointer(index)
    }

    fn allocate_all(&self) -> usize {
        let new_allocated = !self.allocated_bitmask();
        assert_ne!(new_allocated, 0, "block is full");

        self.atomic_add_allocated(new_allocated);
        new_allocated
    }

    unsafe fn new_block(owner: &ObjStorageInner) -> *mut Block {
        let size_needed = Self::allocation_size();
        let memory = mmtk::memory_manager::malloc(size_needed);
        let block_mem = memory.align_up(BLOCK_ALIGNMENT).to_mut_ptr::<Block>();
        block_mem.write(Block::new(owner, memory.to_mut_ptr()));
        block_mem
    }

    unsafe fn delete_block(block: *mut Block) {
        core::ptr::drop_in_place(block);
        let memory = (*block).memory;
        mmtk::memory_manager::free(Address::from_mut_ptr(memory));
    }

    /// This can return a false positive if ptr is not contained by some
    /// block.  For some uses, it is a precondition that ptr is valid,
    /// e.g. contained in some block in owner's `active_array`.  Other uses
    /// require additional validation of the result.
    unsafe fn block_for_ptr(owner: &ObjStorageInner, ptr: *const Value) -> *mut Block {
        let section_start = raw_align_down(ptr as usize, BLOCK_ALIGNMENT) as *mut Value;
        // Start with a guess that the containing section is the last section,
        // so the block starts section_count-1 sections earlier.
        let mut section = section_start.sub(SECTION_SIZE * (SECTION_COUNT - 1));
        // Walk up through the potential block start positions, looking for
        // the owner in the expected location.  If we're below the actual block
        // start position, the value at the owner position will be some oop
        // (possibly null), which can never match the owner.
        let owner_addr = owner as *const _ as usize;
        for i in 0..SECTION_COUNT {
            let candidate = section.cast::<Block>();
            if (*candidate).owner_address == owner_addr {
                return candidate;
            }
            section = section.add(SECTION_SIZE);
        }

        null_mut()
    }

    unsafe fn release_entries(&self, releasing: usize, owner: &ObjStorageInner) {
        self.release_refcount.fetch_add(1, Ordering::Relaxed);

        let mut old_allocated = self.allocated_bitmask();

        loop {
            let new_value = old_allocated ^ releasing;
            match self.allocated_bitmask.compare_exchange_weak(
                old_allocated,
                new_value,
                Ordering::AcqRel,
                Ordering::Relaxed,
            ) {
                Ok(_) => break,
                Err(x) => old_allocated = x,
            }
        }

        // Now that the bitmask has been updated, if we have a state transition
        // (updated bitmask is empty or old bitmask was full), atomically push
        // this block onto the deferred updates list.  Some future call to
        // reduce_deferred_updates will make any needed changes related to this
        // block and _allocation_list.  This deferral avoids _allocation_list
        // updates and the associated locking here.
        if releasing == old_allocated || Self::is_full_bitmask(old_allocated) {
            let this = self as *const _ as *mut Self;
            if {
                let is_null = self.deferred_updates_next().is_null();
                if is_null {
                    self.deferred_updates_next.store(this, Ordering::Relaxed);
                }

                is_null
            } {
                let mut head = owner.deferred_updates.load(Ordering::Relaxed);
                loop {
                    self.deferred_updates_next
                        .store(if head.is_null() { this } else { head }, Ordering::Relaxed);

                    match owner.deferred_updates.compare_exchange_weak(
                        head,
                        this,
                        Ordering::AcqRel,
                        Ordering::Relaxed,
                    ) {
                        Ok(_) => break,
                        Err(x) => head = x,
                    }
                }

                // Only request cleanup for to-empty transitions, not for from-full.
                // There isn't any rush to process from-full transitions.  Allocation
                // will reduce deferrals before allocating new blocks, so may process
                // some.  And the service thread will drain the entire deferred list
                // if there are any pending to-empty transitions.
                if releasing == old_allocated {
                    owner.needs_cleanup.store(true, Ordering::Relaxed);
                }
            }
        }

        self.release_refcount.fetch_sub(1, Ordering::Relaxed);
    }

    fn is_empty(&self) -> bool {
        Self::is_empty_bitmask(self.allocated_bitmask())
    }

    fn is_full(&self) -> bool {
        Self::is_full_bitmask(self.allocated_bitmask())
    }
}

impl ObjStorageInner {
    fn active_array_atomic(&self) -> &AtomicPtr<ActiveArray> {
        unsafe { transmute(&self.active_array) }
    }

    unsafe fn allocate(&mut self) -> *mut Value {
        self.allocation_mutex.lock(true);
        let block = self.block_for_allocation();

        let result = (*block).allocate();

        self.allocation_count.fetch_add(1, Ordering::Relaxed);

        if (*block).is_full() {
            self.allocation_list.unlink(block);
        }
        self.allocation_mutex.unlock();
        result
    }

    unsafe fn block_for_allocation(&mut self) -> *mut Block {
        loop {
            let block = self.allocation_list.head;

            if !block.is_null() {
                return block;
            } else if self.reduce_deferred_updates() {
            } else if self.try_add_block() {
            } else if !self.allocation_list.head.is_null() {
                // Trying to add a block failed, but some other thread added to the
                // list while we'd dropped the lock over the new block allocation.
            } else if !self.reduce_deferred_updates() {
                panic!("Failed to allocate block and failed to reduce deferred updates");
            }
        }
    }

    unsafe fn try_add_block(&mut self) -> bool {
        let block;
        {
            self.allocation_mutex.unlock();
            block = Block::new_block(self);
            self.allocation_mutex.lock(false);
        }

        if block.is_null() {
            return false;
        }

        if !(*self.active_array).push(block) {
            self.expand_active_array();
            assert!(
                (*self.active_array).push(block),
                "Failed to push block after expanding active array"
            );
        }

        self.allocation_list.push_back(block);
        true
    }

    unsafe fn expand_active_array(&mut self) {
        let old_array = self.active_array;
        let new_size = (*old_array).size * 2;

        let new_array = ActiveArray::create(new_size);

        new_array.copy_from(&*old_array);
        self.replace_active_array(new_array as *mut _);
        self.relinquish_block_array(old_array);
    }

    fn replace_active_array(&self, new_array: *mut ActiveArray) {
        unsafe {
            (*new_array).increment_refcount();
        }
        self.active_array_atomic()
            .store(new_array, Ordering::Release);
        self.protect_active.synchronize();
    }

    fn obtain_active_array(&self) -> *mut ActiveArray {
        let cs = CriticalSection::new(&self.protect_active);
        let result = self.active_array_atomic().load(Ordering::Acquire);
        unsafe {
            (*result).increment_refcount();
        }

        result
    }

    fn relinquish_block_array(&self, array: *mut ActiveArray) {
        unsafe {
            if (*array).decrement_refcount() {
                core::ptr::drop_in_place(array);
                mmtk::memory_manager::free(Address::from_mut_ptr(array));
            }
        }
    }
    /// Process one available deferred update.  Returns true if one was processed.
    unsafe fn reduce_deferred_updates(&mut self) -> bool {
        let mut block = self.deferred_updates.load(Ordering::Acquire);
        loop {
            if block.is_null() {
                return false;
            }

            let mut tail = (*block).deferred_updates_next();
            if block == tail {
                tail = null_mut();
            }

            match self.deferred_updates.compare_exchange_weak(
                block,
                tail,
                Ordering::AcqRel,
                Ordering::Relaxed,
            ) {
                Ok(_) => {
                    break;
                }
                Err(x) => block = x,
            }
        }

        (*block).set_deferred_updates_next(null_mut());
        // Ensure bitmask read after pop is complete, including clearing tail, for
        // ordering with release().  Without this, we may be processing a stale
        // bitmask state here while blocking a release() operation from recording
        // the deferred update needed for its bitmask change.
        fence(Ordering::Acquire);

        let allocated = (*block).allocated_bitmask();

        if Block::is_full_bitmask(allocated) {
            assert!(!self.allocation_list.contains(block));
        } else if self.allocation_list.contains(block) {
            // Block is in list.  If empty, move to the end for possible deletion.
            if Block::is_empty_bitmask(allocated) {
                self.allocation_list.unlink(block);
                self.allocation_list.push_back(block);
            }
        } else if Block::is_empty_bitmask(allocated) {
            // Block is empty and not in list. Add to back for possible deletion.
            self.allocation_list.push_back(block);
        } else {
            // Block is neither full nor empty, and not in list.  Add to front.
            self.allocation_list.push_front(block);
        }

        true
    }

    unsafe fn release(&self, ptr: *const Value) {
        let block = Block::block_for_ptr(self, ptr);
        debug_assert!(!block.is_null(), "Block not found for {:p}", ptr);
        (*block).release_entries((*block).bitmask_for_entry(ptr), self);
        self.allocation_count.fetch_sub(1, Ordering::Relaxed);
    }

    unsafe fn release_many(&self, ptrs: &[*const Value]) {
        let mut i = 0;
        while i < ptrs.len() {
            let block = Block::block_for_ptr(self, ptrs[i]);
            debug_assert!(
                !block.is_null(),
                "Block not found for {:p} in release_many",
                ptrs[i]
            );
            let mut releasing = 0;
            let mut count = 0;

            while i < ptrs.len() {
                let entry = ptrs[i];
                let block = Block::block_for_ptr(self, entry);

                if !(*block).contains(entry) {
                    break;
                }

                let entry_bitmask = (*block).bitmask_for_entry(entry);
                releasing |= entry_bitmask;
                count += 1;
                i += 1;
            }

            (*block).release_entries(releasing, self);
            self.allocation_count.fetch_sub(count, Ordering::Relaxed);

            i += 1;
        }
    }

    unsafe fn delete_empty_blocks<const SAFEPOINT: bool>(&mut self) -> bool {
        self.allocation_mutex.lock(SAFEPOINT);
        // Other threads could be adding to the empty block count or the
        // deferred update list while we're working.  Set an upper bound on
        // how many updates we'll process and blocks we'll try to release,
        // so other threads can't cause an unbounded stay in this function.
        // We add a bit of slop because the reduce_deferred_updates clause
        // can cause blocks to be double counted.  If there are few blocks
        // and many of them are deferred and empty, we might hit the limit
        // and spin the caller without doing very much work.  Otherwise,
        // we don't normally hit the limit anyway, instead running out of
        // work to do.
        let limit = self.block_count() + 10;

        for i in 0..limit {
            if self.reduce_deferred_updates() {
                if SAFEPOINT {
                    // be safepoint polite while looping
                    self.allocation_mutex.unlock();
                    Thread::current().safepoint();
                    self.allocation_mutex.lock(true);
                }
            } else {
                let block = self.allocation_list.tail;
                if block.is_null() || !(*block).is_empty() {
                    self.allocation_mutex.unlock();
                    return false;
                } else if !(*block).is_safe_to_delete() {
                    break; // look for other work while waiting for block to be safe to delete
                }

                // Try to delete the block.  First, try to remove from active_array.
                {
                    self.active_mutex.lock(SAFEPOINT);
                    // Don't interfere with an active concurrent iteration.
                    // Instead, give up immediately.  There is more work to do,
                    // but don't re-notify, to avoid useless spinning of the
                    // service thread.  Instead, iteration completion notifies.
                    if self.concurrent_iteration_count.load(Ordering::Relaxed) > 0 {
                        self.allocation_mutex.unlock();
                        self.active_mutex.unlock();
                        return true;
                    }

                    (*self.active_array).remove(block);
                    self.active_mutex.unlock();
                }

                self.allocation_list.unlink(block);

                if SAFEPOINT {
                    self.allocation_mutex.unlock();
                }

                Block::delete_block(block);

                if SAFEPOINT {
                    Thread::current().safepoint();
                    self.allocation_mutex.lock(SAFEPOINT);
                }
            }
        }
        self.needs_cleanup.store(true, Ordering::Relaxed);
        true
    }

    fn block_count(&self) -> usize {
        unsafe { with_active_array(self, |array| (*array).block_count()) }
    }

    fn new(name: &'static str) -> Self {
        Self {
            name,
            refcount: AtomicUsize::new(1),
            active_array: ActiveArray::create(8),
            active_mutex: RawMutex::INIT,
            allocation_list: AllocationList {
                head: null_mut(),
                tail: null_mut(),
            },
            deferred_updates: AtomicPtr::new(null_mut()),
            allocation_mutex: RawMutex::INIT,
            allocation_count: AtomicUsize::new(0),
            protect_active: SingleWriterSynchronizer::new(),
            needs_cleanup: AtomicBool::new(false),
            concurrent_iteration_count: AtomicUsize::new(0),
        }
    }

}

unsafe fn with_active_array<T>(
    storage: &ObjStorageInner,
    f: impl FnOnce(*mut ActiveArray) -> T,
) -> T {
    let active_array = storage.obtain_active_array();
    let result = f(active_array);
    storage.relinquish_block_array(active_array);
    result
}

struct ObjHandleInner {
    refcount: AtomicUsize,
    storage: ObjStorage,
    value: *mut Value,
}

/// A handle to Scheme object in the heap. It is allocated inside [`ObjStorage`].
///
/// The handle is automatically freed when reference count to it reaches zero.
pub struct ObjHandle {
    inner: NonNull<ObjHandleInner>,
}

impl ObjHandle {
    pub fn new(storage: &ObjStorage, value: Value) -> ObjHandle {
        let slot = storage.allocate();
        unsafe {
            *slot = value;

            let inner = Box::new(ObjHandleInner {
                refcount: AtomicUsize::new(1),
                storage: storage.clone(),
                value: slot,
            });

            ObjHandle {
                inner: NonNull::new_unchecked(Box::into_raw(inner)),
            }
        }
    }

    pub fn get(&self) -> Value {
        unsafe { *self.inner.as_ref().value }
    }

    pub fn set(&self, value: Value) {
        unsafe {
            *self.inner.as_ref().value = value;
        }
    }

    pub fn storage<'a>(&self) -> &'a ObjStorage {
        unsafe { &self.inner.as_ref().storage }
    }
}


impl Clone for ObjHandle {
    fn clone(&self) -> Self {
        unsafe {
            self.inner.as_ref().refcount.fetch_add(1, Ordering::AcqRel);
        }
        ObjHandle { inner: self.inner }
    }
}

impl Drop for ObjHandle {
    fn drop(&mut self) {
        unsafe {
            let inner = self.inner.as_ref();
            if inner.refcount.fetch_sub(1, Ordering::AcqRel) == 1 {
                inner.storage.release(inner.value);
                let _ = Box::from_raw(self.inner.as_ptr());
            }
        }
    }
}

impl PartialEq for ObjHandle {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl Eq for ObjHandle {}

impl Hash for ObjHandle {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}

impl Debug for ObjHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            write!(
                f,
                "#<obj-handle {:p} in '{}'>",
                self.inner.as_ref().value,
                self.inner.as_ref().storage.name()
            )
        }
    }
}

pub struct BasicParState {
    pub storage: ObjStorage,
    pub active_array: *mut ActiveArray,
    pub block_count: usize,
    pub next_block: AtomicUsize,
    pub estimated_thread_count: usize,
    pub concurrent: bool,
    pub num_dead: AtomicUsize,
}

pub struct IterationData {
    pub segment_start: usize,
    pub segment_end: usize,
    pub processed: usize,
}

impl BasicParState {
    pub fn new(storage: ObjStorage, estimated_thread_count: usize, concurrent: bool) -> Self {
        let mut this = Self {
            storage: storage.clone(),
            active_array: unsafe { storage.inner.as_ref().obtain_active_array() },
            block_count: 0,
            next_block: AtomicUsize::new(0),
            estimated_thread_count,
            concurrent,
            num_dead: AtomicUsize::new(0),
        };
        this.block_count = unsafe {
            (*this.active_array).block_count_acquire()
        };
        this.update_concurrent_iteration_count(true);
        this
    }

    fn update_concurrent_iteration_count(&self, inc: bool) {
        unsafe {
            if self.concurrent {
                self.storage.inner.as_ref().active_mutex.lock(false);
                if inc {
                    self.storage
                        .inner
                        .as_ref()
                        .concurrent_iteration_count
                        .fetch_add(1, Ordering::Relaxed);
                } else {
                    self.storage
                        .inner
                        .as_ref()
                        .concurrent_iteration_count
                        .fetch_sub(1, Ordering::Relaxed);
                }
            }
        }
    }

    pub fn claim_next_segment(&self, data: &mut IterationData) -> bool {
        data.processed += data.segment_end - data.segment_start;

        let mut start = self.next_block.load(Ordering::Acquire);
        if start >= self.block_count {
            return self.finish_iteration(data);
        }

        // Try to claim several at a time, but not *too* many.  We want to
        // avoid deciding there are many available and selecting a large
        // quantity, get delayed, and then end up claiming most or all of
        // the remaining largish amount of work, leaving nothing for other
        // threads to do.  But too small a step can lead to contention
        // over _next_block, esp. when the work per block is small.
        let max_step = 10;
        let remaining = self.block_count - start;
        let step = max_step.min(1 + (remaining / self.estimated_thread_count));

        let mut end = self.next_block.fetch_add(step, Ordering::Relaxed) + step;

        start = end - step;
        end = end.min(self.block_count);
        if start < self.block_count {
            data.segment_start = start;
            data.segment_end = end;
            return true;
        } else {
            return self.finish_iteration(data);
        }
    }

    pub fn finish_iteration(&self, data: &mut IterationData) -> bool {
        log::info!(
            "Parallel iteration on {}: blocks = {}, processed = {} ({:.2}%)",
            self.storage.name(),
            self.block_count,
            data.processed,
            (data.processed as f64 / self.block_count as f64) * 100.0
        );

        false
    }

    pub fn num_dead(&self) -> usize {
        self.num_dead.load(Ordering::Relaxed)
    }

    pub fn increment_num_dead(&self) {
        self.num_dead.fetch_add(1, Ordering::Relaxed);
    }

    pub fn iterate<F>(&self, mut f: F)
    where F: FnMut(*mut Value)
    {
        let mut f = |val: *mut Value| {
            f(val);
            true 
        };

        let mut data = IterationData {
            segment_start: 0,
            segment_end: 0,
            processed: 0,
        };

        while self.claim_next_segment(&mut data) {
            let mut i = data.segment_start;
            loop {
                let block = unsafe { *(*self.active_array).block_ptr(i)};
                unsafe {
                    (*block).iterate(&mut f);
                }

                i += 1;
                if i >= data.segment_end {
                    break;
                }
            }
        }
    } 
}

impl Drop for BasicParState {
    fn drop(&mut self) {
        self.update_concurrent_iteration_count(false);
    }
}

pub struct ParState<const CONCURRENT: bool> {
    pub basic_state: BasicParState,
}

impl<const CONCURRENT: bool> ParState<CONCURRENT> {
    pub fn new(storage: ObjStorage, estimated_thread_count: usize) -> Self {
        Self {
            basic_state: BasicParState::new(storage, estimated_thread_count, CONCURRENT),
        }
    }

    pub fn iterate<F>(&self, f: F)
    where F: FnMut(*mut Value)
    {
        self.basic_state.iterate(f);
    }
}