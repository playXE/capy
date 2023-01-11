//! Implementation of arbitrary precision integers. 
//! 
//! # Why not use num-bigint?
//! 
//! num-bigint is an excellent library, but sadly we can't use it with CapyScheme because 
//! our GC does not support finalization, this means all memory that is allocated for bigint
//! will be leaked.

use rsgc::prelude::{Object, Allocation};

use super::arraylist::ArrayList;

pub struct BigInt {
    uwords: ArrayList<u32>,
    negative: bool
}

impl BigInt {
    pub const BASE: u64 = u32::MAX as u64 + 1;
}

impl Object for BigInt {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.uwords.trace(visitor);
    }
}

impl Allocation for BigInt {}
