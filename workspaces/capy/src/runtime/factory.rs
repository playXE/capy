//! Object factory for CapyVM.
//!
//! This module defines functions to construct Scheme objects.

use std::{intrinsics::unlikely, mem::size_of};

use mmtk::vm::VMBinding;

use crate::{gc::CapyVM, gc_protect};

use super::{
    cell::{CellReference, CellTag, Pair, SchemeHeader, Vector, Flonum, Bytevector},
    thread::Thread,
    utils::round_up,
    value::{Value, TaggedValue, Tagged},
};

pub const fn align_allocation_inner(
    region: usize,
    alignment: usize,
    offset: usize,
    known_alignment: usize,
) -> usize {
    if alignment <= known_alignment || CapyVM::MAX_ALIGNMENT <= CapyVM::MIN_ALIGNMENT {
        return region;
    }

    let region = region as isize;
    let mask = (alignment - 1) as isize;
    let neg_off = -(offset as isize);

    let delta = neg_off.wrapping_sub(region) & mask;

    (region + delta) as _
}

pub const fn align_allocation(region: usize, alignment: usize, offset: usize) -> usize {
    align_allocation_inner(region, alignment, offset, CapyVM::MIN_ALIGNMENT)
}

macro_rules! alloc_small {
    ($immortal: expr, $thread: expr, $size: expr, $tag: expr, $($save: ident),*) => {
        {
            let mmtk = $thread.mmtk();
            let res = align_allocation(mmtk.bump_pointer, 8, 0 as _);
            if !$immortal {
                if unlikely(res + $size > mmtk.bump_limit) {
                    // slow-path: push $save variables to shadow-stack
                    // and invoke `alloc_slow`, may trigger GC
                    let res = gc_protect!($thread => $($save),* => {
                        $thread.mmtk().alloc_slow($size, $tag)
                    });
                    unsafe { std::mem::transmute::<_, CellReference>(res) }
                } else {
                    let objref: CellReference = unsafe { std::mem::transmute(res + 8) };
                    // SAFETY:
                    // - `objref` is not NULL and is aligned to 8 bytes.
                    // - `post_alloc` action is not required, all allocations that require it
                    // go to `alloc_slow`
                    //
                    {
                        *objref.header_mut() = SchemeHeader::new($tag);
                        mmtk.bump_pointer = res + $size;
                        objref
                    }
                }
            } else {
                let res = gc_protect!($thread => $($save),* => {
                    $thread.mmtk().alloc_immortal($size, $tag)
                });
                res
            }
        }
    }
}

macro_rules! alloc {
    ($immortal: expr, $thread: expr, $size: expr, $tag: expr, $($save: ident),*) => {
        {
            let mmtk = $thread.mmtk();
            let res = align_allocation(mmtk.bump_pointer, 8, 0);
            if !$immortal {
                if unlikely(res + $size > mmtk.bump_limit) || unlikely($size > mmtk.los_threshold) {
                    let res = gc_protect!($thread => $($save),* => {
                        $thread.mmtk().alloc_slow($size, $tag)
                    });
                    unsafe { std::mem::transmute::<_, CellReference>(res) }
                } else {
                    let objref = res as *mut SchemeHeader;
                    // SAFETY:
                    // - `objref` is not NULL and is aligned to 8 bytes.
                    // - `post_alloc` action is not required, all allocations that require it
                    // go to `alloc_slow`
                    //
                    unsafe {
                        objref.write(SchemeHeader::new($tag));
                        mmtk.bump_pointer = res + $size;
                        std::mem::transmute::<_, CellReference>(objref.add(1))
                    }
                }
            } else {
                let res = gc_protect!($thread => $($save),* => {
                    $thread.mmtk().alloc_immortal($size, $tag)
                });
                res
            }
        }
    }
}

impl Thread {
    #[inline]
    pub fn make_pair<const IMMORTAL: bool>(&mut self, mut car: TaggedValue, mut cdr: TaggedValue) -> Tagged<CellReference<Pair>> {
        let mut pair = alloc_small!(
            IMMORTAL,
            self,
            2 * size_of::<Value>() + size_of::<SchemeHeader>(),
            CellTag::PAIR,
            car,
            cdr
        );
        // SAFETY: No write-barrier for newly allocated objects
        unsafe {
            pair.value_set_unchecked(CellReference::<Pair>::CAR_OFFSET, car);
            pair.value_set_unchecked(CellReference::<Pair>::CDR_OFFSET, cdr);
            Tagged::from(pair.downcast_unchecked())
        }
        
    }

    #[inline]
    pub fn make_flonum<const IMMORTAL: bool>(&mut self, f: f64) -> Tagged<CellReference<Flonum>> {
        let mut flonum = alloc_small!(
            IMMORTAL,
            self,
            size_of::<Value>() + size_of::<SchemeHeader>(),
            CellTag::FLONUM,
        );

        flonum.word_set(0, f.to_bits() as _);

        unsafe { Tagged::from(flonum.downcast_unchecked()) } 
    }

    #[inline]
    pub fn make_vectorlike<const IMMORTAL: bool>(&mut self, len: usize, mut init: TaggedValue) -> Tagged<CellReference<Vector>> {
        let mut vec = alloc!(
            IMMORTAL,
            self,
            (len + 1) * size_of::<Value>() + size_of::<SchemeHeader>(),
            CellTag::VECTOR,
            init
        )
        .vector();
        // SAFETY: No write-barrier for newly allocated objects
        unsafe {
            vec.word_set(CellReference::<Vector>::LENGTH_OFFSET, len as _);
            for i in 0..len {
                vec.set_unchecked(i, init);
            }

            Tagged::from(vec.downcast_unchecked())
        }
        
        
    }

    #[inline]
    pub fn make_string<const IMMORTAL: bool>(&mut self, str: impl AsRef<str>) -> Tagged<CellReference<String>> {
        let str = str.as_ref();
        let len = str.len();
        let size = round_up(
            size_of::<Value>() * 1 + len + size_of::<SchemeHeader>(),
            8,
            0,
        );
        let mut string = alloc!(IMMORTAL, self, size, CellTag::STRING,).string();
        // SAFETY: String has enough memory allocated
        unsafe {
            (*string).as_bytes_mut().copy_from_slice(str.as_bytes());
            string.word_set(0, len as _);
            Tagged::from(string.downcast_unchecked())
        }
        
    }

    #[inline]
    pub fn make_bytevectorlike<const IMMORTAL: bool>(&mut self, len: usize, init: u8) -> Tagged<CellReference<Bytevector>> {
        let size = round_up(
            size_of::<Value>() * 1 + len + size_of::<SchemeHeader>(),
            8,
            0,
        );
        let mut vec = alloc!(IMMORTAL, self, size, CellTag::BYTEVECTOR,).bytevector();

        vec.word_set(0, len as _);
        vec.fill(init);

        unsafe { Tagged::from(vec.downcast_unchecked()) } 
    }

    #[inline]
    pub fn make_bytevectorlike_from_slice<const IMMORTAL: bool>(&mut self, slice: &[u8]) -> Tagged<CellReference<Bytevector>> {
        let len = slice.len();
        let size = round_up(
            size_of::<Value>() * 1 + len + size_of::<SchemeHeader>(),
            8,
            0,
        );
        let mut vec = alloc!(IMMORTAL, self, size, CellTag::BYTEVECTOR,).bytevector();

        vec.word_set(0, len as _);
        vec.copy_from_slice(slice);

        unsafe { Tagged::from(vec.downcast_unchecked()) }
    }
}
