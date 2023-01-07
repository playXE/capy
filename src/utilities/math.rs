pub const fn sign_extend32(x: u32, b: usize) -> i32 {
    (x << (32 - b)) as i32 >> (32 - b)
}