use super::memory_region::MemoryRegion;
use crate::utils::*;
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Protection {
    NoAccess,
    ReadOnly,
    ReadWrite,
    ReadExecute,
    ReadWriteExecute,
}

use core::{
    marker::PhantomData,
    ptr::null_mut,
    sync::atomic::{AtomicUsize, Ordering},
};

pub struct VirtualMemory<VM: VirtualMemoryImpl = PlatformVirtualMemory> {
    region: MemoryRegion,
    // Optional secondary mapping of region to a virtual space with different
    // protection, e.g. allowing code execution.
    alias: MemoryRegion,
    /// The underlying reservation not yet given back to the OS.
    /// Its address might disagree with region due to aligned allocations.
    /// Its size might disagree with region due to truncate.
    reserved: MemoryRegion,
    marker: PhantomData<&'static VM>,
}

unsafe impl<VM: VirtualMemoryImpl> Send for VirtualMemory<VM> {}
unsafe impl<VM: VirtualMemoryImpl> Sync for VirtualMemory<VM> {}

impl<VM: VirtualMemoryImpl> VirtualMemory<VM> {
    pub fn reserved(&self) -> &MemoryRegion {
        &self.reserved
    }

    pub fn region(&self) -> &MemoryRegion {
        &self.region
    }

    pub fn alias(&self) -> &MemoryRegion {
        &self.alias
    }

    pub fn page_size() -> usize {
        let size = PAGE_SIZE.load(Ordering::Relaxed);
        if size != 0 {
            return size;
        }

        PAGE_SIZE.store(VM::calculate_page_size(), Ordering::Relaxed);
        PAGE_SIZE.load(Ordering::Relaxed)
    }

    pub fn start(&self) -> usize {
        self.region.start()
    }

    pub fn end(&self) -> usize {
        self.region.end()
    }

    pub fn address(&self) -> *mut u8 {
        self.region.pointer()
    }

    pub fn size(&self) -> usize {
        self.region.size()
    }

    pub fn alias_offset(&self) -> usize {
        self.alias.start() - self.region.start()
    }

    pub fn contains(&self, addr: usize) -> bool {
        self.region.contains(addr)
    }

    pub fn contains_alias(&self, addr: usize) -> bool {
        self.alias_offset() != 0 && self.alias.contains(addr)
    }

    pub fn vm_owns_region(&self) -> bool {
        self.reserved.start() != 0
    }

    pub(crate) const fn new(
        region: MemoryRegion,
        alias: MemoryRegion,
        reserved: MemoryRegion,
    ) -> Self {
        VirtualMemory {
            region,
            alias,
            reserved,
            marker: PhantomData,
        }
    }

    pub fn allocate(size: usize, is_executable: bool, name: &str) -> Option<VirtualMemory<VM>> {
        VM::allocate_aligned(size as _, Self::page_size(), is_executable, name)
    }

    pub fn allocate_aligned(
        size: usize,
        alignment: usize,
        is_executable: bool,
        name: &str,
    ) -> Option<VirtualMemory<VM>> {
        VM::allocate_aligned(size as _, alignment, is_executable, name)
    }

    pub fn reserve(size: usize, alignment: usize) -> Option<VirtualMemory<VM>> {
        VM::reserve(size, alignment)
    }

    pub fn protect(&self, mode: Protection) {
        VM::protect(self.address().cast(), self.size(), mode)
    }

    pub fn truncate(&mut self, new_size: usize) {
        assert!(new_size <= self.size());
        assert!(is_aligned(new_size, Self::page_size(), 0));
        if self.reserved.size() == self.region.size() {
            if VM::free_sub_segment((self.start() + new_size) as _, self.size() - new_size) {
                self.reserved.set_size(new_size);

                if self.alias_offset() != 0 {
                    VM::free_sub_segment(
                        (self.alias.start() + new_size) as _,
                        self.alias.size() - new_size,
                    );
                }
            }
        }
        let region = self.region;
        self.region.subregion(&region, 0, new_size);
        let alias = self.alias;
        self.alias.subregion(&alias, 0, new_size);
    }

    pub fn in_same_page(addr0: usize, addr1: usize) -> bool {
        round_down(addr0, Self::page_size() as _) == round_down(addr1, Self::page_size() as _)
    }

    pub unsafe fn from_raw(pointer: *mut u8, size: usize) -> Self {
        let region = MemoryRegion::new(pointer, size);
        let reserved = MemoryRegion::new(null_mut(), 0);

        VirtualMemory {
            region,
            alias: region,
            reserved,
            marker: PhantomData,
        }
    }

    pub unsafe fn unsafe_release(&self) {
        VM::release(self);
    }

    pub fn null() -> Self {
        unsafe { Self::from_raw(null_mut(), 0) }
    }
}

impl<VM: VirtualMemoryImpl> Drop for VirtualMemory<VM> {
    fn drop(&mut self) {
        VM::release(self);
    }
}

pub unsafe trait VirtualMemoryImpl: 'static + Sized {
    fn calculate_page_size() -> usize;
    fn dontneed(address: *mut u8, size: usize) {
        let _ = address;
        let _ = size;
    }

    fn allocate_aligned(
        size: usize,
        alignment: usize,
        is_executable: bool,
        name: &str,
    ) -> Option<VirtualMemory<Self>>;
    fn reserve(size: usize, alignment: usize) -> Option<VirtualMemory<Self>>;
    fn commit(address: *mut u8, size: usize);
    fn decommit(address: *mut u8, size: usize);
    fn protect(address: *mut u8, size: usize, mode: Protection);
    fn free_sub_segment(address: *mut u8, size: usize) -> bool;
    fn release(mem: &VirtualMemory<Self>);
}

static PAGE_SIZE: AtomicUsize = AtomicUsize::new(0);

#[cfg(all(unix, not(target_os = "fuchsia")))]
pub mod posix {
    use crate::{
        gc::memory_region::MemoryRegion,
        gc::virtual_memory::VirtualMemory,
        utils::{
            is_aligned, is_aligned_usize, is_power_of_two, round_down, round_down_usize, round_up,
            round_up_usize,
        },
    };
    use core::ptr::null_mut;

    use super::{Protection, VirtualMemoryImpl};

    unsafe fn map(
        addr: *mut u8,
        length: usize,
        prot: i32,
        flags: i32,
        fd: i32,
        offset: isize,
    ) -> *mut u8 {
        let result = libc::mmap(addr.cast(), length, prot, flags, fd, offset as _);

        if result == libc::MAP_FAILED {
            panic!(
                "mmap({:p}, {}, {}, {}) failed: {}",
                addr,
                length,
                prot,
                flags,
                errno::errno()
            );
        }

        result.cast()
    }

    unsafe fn unmap(start: usize, end: usize) {
        let size = end - start;
        if size == 0 {
            return;
        }

        if libc::munmap(start as _, size) != 0 {
            panic!("munmap({:p}, {}) failed", start as *mut u8, size);
        }
    }

    unsafe fn generic_map_aligned(
        hint: *mut u8,
        prot: i32,
        size: isize,
        alignment: isize,
        allocated_size: isize,
        map_flags: i32,
    ) -> *mut u8 {
        let address = map(hint, allocated_size as _, prot, map_flags, -1, 0);

        let base = address as usize;
        let aligned_base = round_up_usize(base, alignment as _, 0);

        unmap(base, aligned_base);
        unmap(aligned_base + size as usize, base + allocated_size as usize);

        aligned_base as *mut u8
    }

    #[allow(dead_code)]
    unsafe fn map_aligned(
        hint: *mut u8,
        fd: i32,
        prot: i32,
        size: isize,
        alignment: isize,
        allocated_size: isize,
    ) -> *mut u8 {
        assert!(size <= allocated_size);

        let mut address = map(
            hint,
            allocated_size as _,
            libc::PROT_NONE,
            libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
            -1,
            0,
        );

        let base = address as usize;
        let aligned_base = round_up(base, alignment as _, 0);

        address = map(
            aligned_base as _,
            size as _,
            prot,
            libc::MAP_SHARED | libc::MAP_FIXED,
            fd,
            0,
        );
        assert_eq!(address, aligned_base as *mut u8);
        unmap(base, aligned_base);
        unmap(aligned_base + size as usize, base + allocated_size as usize);

        address
    }

    pub struct PosixVirtualMemory;

    unsafe impl VirtualMemoryImpl for PosixVirtualMemory {
        fn calculate_page_size() -> usize {
            extern "C" {
                fn getpagesize() -> isize;
            }

            unsafe { getpagesize() as _ }
        }

        #[allow(unused_mut)]
        fn allocate_aligned(
            size: usize,
            alignment: usize,
            is_executable: bool,
            name: &str,
        ) -> Option<super::VirtualMemory<Self>> {
            let _ = name;
            assert!(
                is_aligned(size, VirtualMemory::<Self>::page_size(), 0),
                "{} not aligned to page size",
                size
            );
            assert!(is_power_of_two(alignment));
            assert!(is_aligned(alignment, VirtualMemory::<Self>::page_size(), 0));

            let allocated_size = size + alignment - VirtualMemory::<Self>::page_size();

            let prot = libc::PROT_READ
                | libc::PROT_WRITE
                | if is_executable { libc::PROT_EXEC } else { 0 };

            let mut map_flags = libc::MAP_PRIVATE | libc::MAP_ANONYMOUS;

            #[cfg(any(
                target_os = "macos",
                target_os = "tvos",
                target_os = "watchos",
                target_os = "ios"
            ))]
            {
                if is_executable {
                    map_flags = libc::MAP_JIT;
                }
            }

            let address = unsafe {
                generic_map_aligned(
                    null_mut(),
                    prot,
                    size as _,
                    alignment as _,
                    allocated_size as _,
                    map_flags,
                )
            };

            let region = MemoryRegion::new(address.cast(), size);
            Some(VirtualMemory::new(region, region, region))
        }

        fn commit(address: *mut u8, size: usize) {
            let result = unsafe {
                libc::mmap(
                    address.cast(),
                    size,
                    libc::PROT_READ | libc::PROT_WRITE,
                    libc::MAP_PRIVATE | libc::MAP_ANONYMOUS | libc::MAP_FIXED,
                    -1,
                    0,
                )
            };

            if result == libc::MAP_FAILED {
                panic!("failed to commit: {}", errno::errno());
            }
        }

        fn decommit(address: *mut u8, size: usize) {
            assert!(is_aligned_usize(
                address as usize,
                VirtualMemory::<Self>::page_size(),
                0
            ));
            assert!(is_aligned_usize(
                size,
                VirtualMemory::<Self>::page_size(),
                0
            ));

            let result = unsafe {
                libc::mmap(
                    address.cast(),
                    size,
                    libc::PROT_NONE,
                    libc::MAP_PRIVATE | libc::MAP_ANONYMOUS | libc::MAP_NORESERVE | libc::MAP_FIXED,
                    -1,
                    0,
                )
            };

            if result == libc::MAP_FAILED {
                panic!("decommit failed");
            }
        }

        fn dontneed(address: *mut u8, size: usize) {
            let start_address = address as usize;
            let end_address = start_address + size;
            let page_address = round_down(start_address, VirtualMemory::<Self>::page_size() as _);

            loop {
                unsafe {
                    let ret = libc::madvise(
                        page_address as _,
                        end_address - page_address,
                        libc::MADV_DONTNEED,
                    );

                    if ret != 0 && errno::errno().0 == libc::EAGAIN {
                        continue;
                    }

                    if ret != 0 && errno::errno().0 != libc::ENOSYS {
                        panic!("madvise failed");
                    }

                    break;
                }
            }
        }

        fn free_sub_segment(address: *mut u8, size: usize) -> bool {
            let start = address as usize;
            unsafe {
                unmap(start, start + size);
            }

            true
        }

        fn protect(address: *mut u8, size: usize, mode: super::Protection) {
            let start_address = address as usize;
            let end_address = start_address + size;
            let page_address =
                round_down_usize(start_address, VirtualMemory::<Self>::page_size() as _);

            let prot = match mode {
                Protection::NoAccess => libc::PROT_NONE,
                Protection::ReadOnly => libc::PROT_READ,
                Protection::ReadWrite => libc::PROT_READ | libc::PROT_WRITE,
                Protection::ReadExecute => libc::PROT_READ | libc::PROT_EXEC,
                Protection::ReadWriteExecute => {
                    libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC
                }
            };

            unsafe {
                if libc::mprotect(page_address as _, end_address - page_address, prot) != 0 {
                    panic!(
                        "mprotect({:p}, {}, {}) failed",
                        page_address as *mut u8,
                        end_address - page_address,
                        prot
                    );
                }
            }
        }

        fn release(mem: &super::VirtualMemory<Self>) {
            if mem.vm_owns_region() {
                unsafe {
                    unmap(mem.reserved.start(), mem.reserved.end());
                    let alias_offset = mem.alias_offset();
                    if alias_offset != 0 {
                        unmap(
                            mem.reserved.start() + alias_offset,
                            mem.reserved.size() - alias_offset,
                        );
                    }
                }
            }
        }

        fn reserve(size: usize, alignment: usize) -> Option<super::VirtualMemory<Self>> {
            assert!(is_aligned(size, VirtualMemory::<Self>::page_size(), 0));
            assert!(is_power_of_two(alignment));
            assert!(is_aligned(alignment, VirtualMemory::<Self>::page_size(), 0));
            let allocated_size = size + alignment - VirtualMemory::<Self>::page_size();
            let address = unsafe {
                generic_map_aligned(
                    null_mut(),
                    libc::PROT_NONE,
                    size as _,
                    alignment as _,
                    allocated_size as _,
                    libc::MAP_PRIVATE | libc::MAP_ANONYMOUS | libc::MAP_NORESERVE,
                )
            };

            if address.is_null() {
                return None;
            }
            let region = MemoryRegion::new(address.cast(), size);
            Some(VirtualMemory::new(region, region, region))
        }
    }
}

#[cfg(windows)]
mod win {
    use winapi::um::memoryapi::*;
    use winapi::um::sysinfoapi::*;
    use winapi::um::winnt::*;

    use core::mem::MaybeUninit;
    use core::ptr::null_mut;

    use crate::{
        heap::memory_region::MemoryRegion,
        heap::virtual_memory::VirtualMemory,
        utils::{is_aligned, is_power_of_two, round_down, round_up},
    };

    use super::{Protection, VirtualMemoryImpl};

    pub struct WinVirtualMemory;

    fn allocate_aligned_impl(
        size: usize,
        alignment: usize,
        reserved_size: usize,
        prot: i32,
        out_reserved_address: Option<&mut *mut u8>,
    ) -> *mut u8 {
        unsafe {
            let address = VirtualAlloc(null_mut(), reserved_size as _, MEM_RESERVE as _, prot as _);

            if address.is_null() {
                return null_mut();
            }

            let aligned_address = round_up(address as _, alignment as _, 0);

            if VirtualAlloc(aligned_address as _, size, MEM_COMMIT, prot as _) as usize
                != aligned_address
            {
                VirtualFree(address as _, reserved_size as _, MEM_RELEASE);
                return null_mut();
            }
            if let Some(addr) = out_reserved_address {
                *addr = address as *mut u8;
            }
            aligned_address as _
        }
    }

    unsafe impl VirtualMemoryImpl for WinVirtualMemory {
        fn calculate_page_size() -> usize {
            unsafe {
                let mut info = MaybeUninit::<SYSTEM_INFO>::zeroed().assume_init();

                GetSystemInfo(&mut info as LPSYSTEM_INFO);

                info.dwPageSize as usize
            }
        }

        fn allocate_aligned(
            size: usize,
            alignment: usize,
            is_executable: bool,
            name: &str,
        ) -> Option<super::VirtualMemory<Self>> {
            let _ = name;
            assert!(is_aligned(size, VirtualMemory::<Self>::page_size(), 0));
            assert!(is_power_of_two(alignment));
            assert!(is_aligned(alignment, VirtualMemory::<Self>::page_size(), 0));

            let reserved_size = size + alignment - VirtualMemory::<Self>::page_size();

            let prot = if is_executable {
                PAGE_EXECUTE_READWRITE
            } else {
                PAGE_READWRITE
            };

            let mut reserved_address = null_mut();

            let aligned_address = allocate_aligned_impl(
                size,
                alignment,
                reserved_size,
                prot as _,
                Some(&mut reserved_address),
            );

            if aligned_address.is_null() {
                return None;
            }

            let region = MemoryRegion::new(aligned_address as _, size);
            let reserved = MemoryRegion::new(reserved_address as _, reserved_size);

            return Some(VirtualMemory::new(region, region, reserved));
        }

        fn reserve(size: usize, alignment: usize) -> Option<VirtualMemory<Self>> {
            assert!(is_aligned(size, VirtualMemory::<Self>::page_size(), 0));
            assert!(is_power_of_two(alignment));
            assert!(is_aligned(alignment, VirtualMemory::<Self>::page_size(), 0));

            let reserved_size = size + alignment - VirtualMemory::<Self>::page_size();

            unsafe {
                let reserved_address =
                    VirtualAlloc(null_mut(), reserved_size as _, MEM_RESERVE, PAGE_NOACCESS);

                if reserved_address.is_null() {
                    return None;
                }

                let aligned_address = round_up(reserved_address as usize, alignment, 0);

                let region = MemoryRegion::new(aligned_address as _, size);
                let reserved = MemoryRegion::new(reserved_address as _, reserved_size);

                Some(VirtualMemory::new(region, region, reserved))
            }
        }

        fn commit(address: *mut u8, size: usize) {
            unsafe {
                let result = VirtualAlloc(address as _, size as _, MEM_COMMIT, PAGE_READWRITE);

                if result.is_null() {
                    panic!("commit failed");
                }
            }
        }

        fn decommit(address: *mut u8, size: usize) {
            unsafe {
                let result = VirtualFree(address as _, size, MEM_DECOMMIT);

                if result as i32 == 0 {
                    panic!("failed to decommit");
                }
            }
        }

        fn release(mem: &VirtualMemory<Self>) {
            unsafe {
                if mem.vm_owns_region() {
                    VirtualFree(mem.reserved.pointer() as _, 0, MEM_RELEASE);
                }
            }
        }

        fn free_sub_segment(address: *mut u8, size: usize) -> bool {
            unsafe {
                if VirtualFree(address as _, size as _, MEM_DECOMMIT) == 0 {
                    return false;
                }

                true
            }
        }

        fn protect(address: *mut u8, size: usize, mode: Protection) {
            let start_address = address as usize;
            let end_address = start_address + size;
            let page_address = round_down(start_address, VirtualMemory::<Self>::page_size() as _);

            let prot = match mode {
                Protection::NoAccess => PAGE_NOACCESS,
                Protection::ReadOnly => PAGE_READONLY,
                Protection::ReadWrite => PAGE_READWRITE,
                Protection::ReadExecute => PAGE_EXECUTE_READ,
                Protection::ReadWriteExecute => PAGE_EXECUTE_READWRITE,
            };

            unsafe {
                VirtualProtect(
                    page_address as _,
                    (end_address - page_address) as _,
                    prot as _,
                    &mut 0,
                );
            }
        }
    }
}

#[cfg(miri)]
mod miri {
    use crate::{
        heap::memory_region::MemoryRegion,
        heap::virtual_memory::VirtualMemory,
        utils::{is_aligned, is_power_of_two, round_down, round_up},
    };

    use super::{Protection, VirtualMemoryImpl};

    pub struct MiriVirtualMemory;

    unsafe impl VirtualMemoryImpl for MiriVirtualMemory {
        fn calculate_page_size() -> usize {
            4096
        }

        fn allocate_aligned(
            size: usize,
            alignment: usize,
            _: bool,
            _: &str,
        ) -> Option<VirtualMemory<Self>> {
            assert!(is_aligned(size, VirtualMemory::<Self>::page_size(), 0));
            assert!(is_power_of_two(alignment));
            assert!(is_aligned(alignment, VirtualMemory::<Self>::page_size(), 0));

            let allocated_size = size + alignment - VirtualMemory::<Self>::page_size();

            let mut vec = std::vec::Vec::<u8>::with_capacity(allocated_size);
            let reserved_addr = vec.as_mut_ptr() as usize;
            core::mem::forget(vec);

            let aligned_address = round_up(reserved_addr as usize, alignment as _, 0);
            let region = MemoryRegion::new(aligned_address as _, size);
            let reserved = MemoryRegion::new(reserved_addr as _, allocated_size);

            Some(VirtualMemory::new(region, region, reserved))
        }

        fn reserve(size: usize, alignment: usize) -> Option<VirtualMemory<Self>> {
            let allocated_size = size + alignment - VirtualMemory::<Self>::page_size();

            let mut vec = std::vec::Vec::<u8>::with_capacity(allocated_size);
            let reserved_addr = vec.as_mut_ptr() as usize;
            core::mem::forget(vec);

            let aligned_address = round_up(reserved_addr as usize, alignment as _, 0);
            let region = MemoryRegion::new(aligned_address as _, size);
            let reserved = MemoryRegion::new(reserved_addr as _, allocated_size);

            Some(VirtualMemory::new(region, region, reserved))
        }

        fn commit(_: *mut u8, _: usize) {}

        fn decommit(_: *mut u8, _: usize) {}

        fn release(mem: &VirtualMemory<Self>) {
            unsafe {
                if mem.vm_owns_region() {
                    let _ = std::vec::Vec::<u8>::from_raw_parts(
                        mem.reserved.start() as _,
                        0,
                        mem.reserved.size(),
                    );
                }
            }
        }

        fn free_sub_segment(_: *mut u8, _: usize) -> bool {
            false
        }

        fn protect(_: *mut u8, _: usize, _: Protection) {}
    }
}

#[cfg(windows)]
pub type PlatformVirtualMemory = win::WinVirtualMemory;

#[cfg(all(not(miri), unix, not(target_os = "fuchsia")))]
pub type PlatformVirtualMemory = posix::PosixVirtualMemory;

#[cfg(miri)]
pub type PlatformVirtualMemory = miri::MiriVirtualMemory;

pub fn page_size() -> usize {
    VirtualMemory::<PlatformVirtualMemory>::page_size()
}
