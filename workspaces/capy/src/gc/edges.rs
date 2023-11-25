#![allow(dead_code, unused_imports)]
use std::sync::atomic::{AtomicUsize, Ordering};

use mmtk::{
    util::{Address, ObjectReference},
    vm::edge_shape::{Edge, MemorySlice, UnimplementedMemorySliceEdgeIterator},
};
use num_traits::{ToPrimitive, Unsigned};

use crate::{
    gc::CapyVM,
    runtime::{cell::CellReference, tagged::TaggedImpl, value::*},
};

use super::ptr_compr::HeapCompressionScheme;

pub const USE_COMPRESSED_OOPS: bool = cfg!(feature = "compressed-oops");
pub const SHIFT: usize = 3;

pub fn use_compressed_oops() -> bool {
    USE_COMPRESSED_OOPS
}

pub fn initialize_compressed_oops_base_and_shift() {
    HeapCompressionScheme::init_base(mmtk::memory_manager::starting_heap_address().as_usize())
}

/// Custom edge type implementation for CapyScheme.
///
/// We need this to be custom because of two things:
/// 1) Compressed pointers.
/// 2) Pointer tagging.
///
/// First of all, compressed pointers before actually being updated or scanned need to be
/// decompressed, and then again compressed if GC decides to move them.
/// Then, our pointer tagging scheme assumes that top bit is set to 1 for objects, so we
/// have to subtract 1 from the address to untag it and add 1 to tag it back.
///
/// Alternative is to use `scan_object_and_trace_edges` instead of `scan_object`
/// but it seems to be slower than enqueing edges properly.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ScmEdge {
    pub addr: Address,
}

impl ScmEdge {
    pub const LOG_BYTES_IN_EDGE: usize = if USE_COMPRESSED_OOPS { 2 } else { 3 };
    pub const BYTES_IN_EDGE: usize = 1 << Self::LOG_BYTES_IN_EDGE;

    const fn is_compressed(&self) -> bool {
        (self.addr.as_usize() & 0x1) == 0
    }

    const fn untagged_address(&self) -> Address {
        unsafe { Address::from_usize(self.addr.as_usize() << 1 >> 1) }
    }

    const fn tag_address(addr: Address) -> Address {
        unsafe { Address::from_usize(addr.as_usize() | (1 << 63usize)) }
    }

    fn x86_read_unaligned<T, const UNTAG: bool>(&self) -> T {
        unsafe {
            let slot = if UNTAG {
                self.untagged_address()
            } else {
                self.addr
            };

            let ptr = slot.to_ptr::<T>();
            ptr.read_unaligned()
        }
    }

    fn x86_write_unaligned<T, const UNTAG: bool>(&self, value: T) {
        unsafe {
            let slot = if UNTAG {
                self.untagged_address()
            } else {
                self.addr
            };

            let ptr = slot.to_mut_ptr::<T>();
            ptr.write_unaligned(value);
        }
    }

    fn compress(o: ObjectReference) -> u32 {
        HeapCompressionScheme::compress_object(o.to_raw_address().as_usize())
    }

    fn decompress(v: u32) -> ObjectReference {
        unsafe {
            ObjectReference::from_raw_address(Address::from_usize(
                HeapCompressionScheme::decompress_tagged(v as _),
            ))
        }
    }
}

impl From<&mut TaggedValue> for ScmEdge {
    fn from(value: &mut TaggedValue) -> Self {
        Self::from(value as *mut TaggedValue)
    }
}

impl<T: Unsigned + Copy + ToPrimitive> From<*mut TaggedImpl<T>> for ScmEdge {
    fn from(addr: *mut TaggedImpl<T>) -> Self {
        unsafe {
            let val = addr.read_unaligned();
            debug_assert!(val.is_cell());
            let address = Address::from_ptr(addr);
            Self {
                addr: if TaggedImpl::<T>::IS_FULL {
                    address
                } else {
                    Self::tag_address(address)
                },
            }
        }
    }
}

impl<T> From<*mut Tagged<CellReference<T>>> for ScmEdge {
    fn from(value: *mut Tagged<CellReference<T>>) -> Self {
        unsafe {
            let val = value.read_unaligned();
            debug_assert!(val.0.is_cell());
            let address = Address::from_ptr(value);

            Self { addr: address }
        }
    }
}

impl From<Address> for ScmEdge {
    fn from(value: Address) -> Self {
        Self { addr: value }
    }
}

impl Edge for ScmEdge {
    fn load(&self) -> ObjectReference {
        let res = if USE_COMPRESSED_OOPS {
            let res = if self.is_compressed() {
                Self::decompress(self.x86_read_unaligned::<u32, true>())
            } else {
                self.x86_read_unaligned::<ObjectReference, true>()
            };
            // untag by subtracting 1
            ObjectReference::from_raw_address(res.to_raw_address() - HEAP_OBJECT_TAG)
        } else {
            unsafe {
                let addr = self.addr.load::<Address>();

                ObjectReference::from_raw_address(addr - HEAP_OBJECT_TAG)
            }
        };
        res
    }

    fn store(&self, object: ObjectReference) {
        let raw = object.to_raw_address();

        // tag by adding 1
        let object = ObjectReference::from_raw_address(raw + HEAP_OBJECT_TAG);
        if USE_COMPRESSED_OOPS {
            if self.is_compressed() {
                self.x86_write_unaligned::<u32, true>(Self::compress(object));
            } else {
                self.x86_write_unaligned::<ObjectReference, false>(object);
            }
        } else {
            unsafe {
                self.addr.store(object);
            }
        }
    }
}

impl MemorySlice for ScmEdge {
    type Edge = ScmEdge;
    type EdgeIterator = UnimplementedMemorySliceEdgeIterator<ScmEdge>;
    fn bytes(&self) -> usize {
        todo!()
    }

    fn copy(_src: &Self, _tgt: &Self) {
        todo!()
    }

    fn iter_edges(&self) -> Self::EdgeIterator {
        todo!()
    }

    fn object(&self) -> Option<ObjectReference> {
        todo!()
    }

    fn start(&self) -> Address {
        todo!()
    }
}
