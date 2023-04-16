pub mod bitfield;
pub mod bitmap;
pub mod fast_bitvec;
pub const fn nth_bit(n: usize) -> usize {
    if n >= std::mem::size_of::<usize>() * 8 {
        0
    } else {
        1 << n
    }
}

pub const fn right_nth_bit(n: usize) -> usize {
    nth_bit(n) - 1
}
