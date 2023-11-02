//! Object factory for CapyVM.
//!
//! This module defines functions to construct Scheme objects.

use std::{intrinsics::unlikely, mem::size_of};

use crate::gc_protect;

use super::{
    cell::{CellReference, CellTag, Pair, SchemeHeader},
    thread::Thread,
    utils::round_up,
    value::Value,
};

macro_rules! alloc_small {
    ($immortal: expr, $thread: expr, $size: expr, $tag: expr, $($save: ident),*) => {
        {
            let mmtk = $thread.mmtk();
            let res = mmtk.bump_pointer;
            if !$immortal {
                if unlikely(res + $size > mmtk.bump_limit) {
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
                        mmtk.bump_pointer += $size;
                        std::mem::transmute::<_, CellReference>(objref)
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
            let res = mmtk.bump_pointer;
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
                        mmtk.bump_pointer += $size;
                        std::mem::transmute::<_, CellReference>(objref)
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
    pub fn make_pair<const IMMORTAL: bool>(&mut self, mut car: Value, mut cdr: Value) -> Value {
        let mut pair = alloc_small!(
            IMMORTAL,
            self,
            3 * size_of::<Value>(),
            CellTag::PAIR,
            car,
            cdr
        );
        // SAFETY: No write-barrier for newly allocated objects
        unsafe {
            pair.word_set_unchecked(CellReference::<Pair>::CAR_OFFSET, car);
            pair.word_set_unchecked(CellReference::<Pair>::CDR_OFFSET, cdr);
        }

        Value::new(pair)
    }

    #[inline]
    pub fn make_vectorlike<const IMMORTAL: bool>(&mut self, len: usize, mut init: Value) -> Value {
        let mut vec = alloc!(
            IMMORTAL,
            self,
            (len + 2) * size_of::<Value>(),
            CellTag::VECTOR,
            init
        )
        .vector();
        // SAFETY: No write-barrier for newly allocated objects
        unsafe {
            vec.u64_set(1, len as u64);
            for i in 0..len {
                vec.set_unchecked(i, init);
            }
        }
        Value::new(vec)
    }

    #[inline]
    pub fn make_string<const IMMORTAL: bool>(&mut self, str: impl AsRef<str>) -> Value {
        let str = str.as_ref();
        let len = str.len();
        let size = round_up(size_of::<Value>() * 2 + len, 8, 0);
        let mut string = alloc!(IMMORTAL, self, size, CellTag::STRING,).string();
        // SAFETY: String has enough memory allocated
        unsafe {
            (*string).as_bytes_mut().copy_from_slice(str.as_bytes());
            string.u64_set(1, len as u64);
        }
        Value::new(string)
    }

    #[inline]
    pub fn make_bytevectorlike<const IMMORTAL: bool>(&mut self, len: usize, init: u8) -> Value {
        let size = round_up(size_of::<Value>() * 2 + len, 8, 0);
        let mut vec = alloc!(IMMORTAL, self, size, CellTag::BYTEVECTOR,).bytevector();

        vec.u64_set(1, len as u64);
        vec.fill(init);

        Value::new(vec)
    }

    #[inline]
    pub fn make_bytevectorlike_from_slice<const IMMORTAL: bool>(&mut self, slice: &[u8]) -> Value {
        let len = slice.len();
        let size = round_up(size_of::<Value>() * 2 + len, 8, 0);
        let mut vec = alloc!(IMMORTAL, self, size, CellTag::BYTEVECTOR,).bytevector();

        vec.u64_set(1, len as u64);
        vec.copy_from_slice(slice);

        Value::new(vec)
    }
}
