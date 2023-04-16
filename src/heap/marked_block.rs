use crate::bitmap;

use super::block_directory::BlockDirectory;

bitmap!(MarkBitmap, 32);

pub const ATOM_SIZE: usize = 16;
pub const BLOCK_SIZE: usize = 16 * 1024;
pub const BLOCK_MASK: usize = !(BLOCK_SIZE - 1);
pub const ATOMS_PER_BLOCK: usize = BLOCK_SIZE / ATOM_SIZE;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SweepMode {
    SweepOnly,
    SweepToFreeList,
}


/// A marked block is a page-aligned container for heap-allocated objects.
/// Objects are allocated within cells of the marked block. For a given
/// marked block, all cells have the same size. Objects smaller than the
/// cell size may be allocated in the marked block, in which case the
/// allocation suffers from internal fragmentation: wasted space whose
/// size is equal to the difference between the cell size and the object
/// size.
pub struct MarkedBlock {

}

pub struct Handle {
    directory: *mut BlockDirectory,
    block: *mut MarkedBlock,
    
}