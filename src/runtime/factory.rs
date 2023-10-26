//! Object factory for CapyVM.
//! 
//! This module defines functions to construct Scheme objects.

use std::{mem::size_of, intrinsics::{likely, unlikely}};

use crate::gc_protect;

use super::{thread::Thread, value::Value, cell::{CellReference, Pair, CellTag, SchemeHeader}};

macro_rules! alloc_small {
    ($thread: expr, $size: expr, $tag: expr, $($save: ident),*) => {
        {
            let mmtk = $thread.mmtk();
            let res = mmtk.bump_pointer;
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
        }
    }
}


impl Thread {
    #[inline]
    pub fn make_pair<const IMMORTAL: bool>(&mut self, mut car: Value, mut cdr: Value) -> Value {
        
        let mut pair = alloc_small!(self, 3 * size_of::<Value>(), CellTag::PAIR, car, cdr);
        // SAFETY: No write-barrier for newly allocated objects
        unsafe {
            pair.word_set_unchecked(CellReference::<Pair>::CAR_OFFSET, car);
            pair.word_set_unchecked(CellReference::<Pair>::CDR_OFFSET, cdr);
        }
   
        Value::new(pair)
    }
}

pub fn make_pair(car: Value, cdr: Value, thread: &mut Thread) -> Value {
    thread.make_pair::<false>(car, cdr)
}

pub fn make_list_of_n(n: usize, thread: &mut Thread) -> Value {
    let mut xs = Value::null();

    for i in (0..n).rev() {
        let prev = xs;
    
        xs = thread.make_pair::<false>(Value::new(i as i32), prev);
    }

    xs
} 