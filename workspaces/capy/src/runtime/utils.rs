use std::mem::size_of;

pub const fn nth_bit(n: usize) -> usize {
    if n >= size_of::<usize>() * 8 {
        0
    } else {
        1 << n
    }
}

pub const fn right_nth_bit(n: usize) -> usize {
    nth_bit(n) - 1
}

use core::ops::*;
use num_traits::{FromPrimitive, NumOps, One, ToPrimitive, Zero};
pub fn is_power_of_two<
    T: BitOr<T, Output = T>
        + BitAnd<T, Output = T>
        + Not<Output = T>
        + PartialEq
        + Copy
        + Zero
        + One
        + Sub<Output = T>,
>(
    x: T,
) -> bool {
    (x & (x - T::one())) == T::zero() && (x != T::zero())
}

pub fn shift_for_power_of_two<
    T: Shr<T, Output = T>
        + Copy
        + Eq
        + One
        + Zero
        + Not<Output = T>
        + BitOr<T, Output = T>
        + BitAnd<T, Output = T>
        + Sub<Output = T>
        + PartialOrd,
>(
    mut x: T,
) -> i32 {
    assert!(is_power_of_two(x));
    let mut num_shifts = 0;
    while x > T::one() {
        num_shifts += 1;
        x = x >> T::one();
    }
    num_shifts
}

pub fn is_aligned_usize(x: usize, alignment: usize, offset: usize) -> bool {
    assert!(is_power_of_two(alignment));
    assert!(offset < alignment);
    (x & (alignment - 1)) == offset
}

pub fn is_aligned<
    T: BitOr<T, Output = T>
        + BitAnd<T, Output = T>
        + Not<Output = T>
        + PartialEq
        + Copy
        + Zero
        + One
        + Sub<Output = T>
        + ToPrimitive,
>(
    x: T,
    alignment: usize,
    offset: usize,
) -> bool {
    assert!(is_power_of_two(alignment));
    assert!(offset < alignment);
    let x = x.to_isize();
    let x = match x {
        Some(x) => x,
        _ => return false,
    };
    (x & (alignment as isize - 1)) == offset as isize
}

pub fn is_aligned_ptr<
    T,
    U: BitOr<U, Output = U> + BitAnd<U, Output = U> + Not<Output = U> + PartialEq + Copy + Zero + One,
>(
    x: *const T,
    alignment: usize,
    offset: usize,
) -> bool {
    is_aligned(x as usize, alignment, offset)
}

pub fn round_down<
    T: BitOr<T, Output = T>
        + BitAnd<T, Output = T>
        + Not<Output = T>
        + Copy
        + FromPrimitive
        + ToPrimitive,
>(
    x: T,
    alignment: isize,
) -> T {
    debug_assert!(is_power_of_two(alignment));

    T::from_isize(x.to_isize().unwrap() & -alignment).unwrap()
}

pub fn round_down_usize(x: usize, alignment: usize) -> usize {
    assert!(is_power_of_two(alignment));
    x & !(alignment.wrapping_sub(1))
}

pub fn round_down_unchecked<
    T: BitOr<T, Output = T>
        + BitAnd<T, Output = T>
        + Not<Output = T>
        + Copy
        + FromPrimitive
        + ToPrimitive,
>(
    x: T,
    alignment: isize,
) -> T {
    T::from_isize(x.to_isize().unwrap() & -alignment).unwrap()
}

pub fn round_down_ptr<T, U: BitOr<U, Output = U> + Not<Output = U> + Copy>(
    x: *const T,
    alignment: isize,
) -> *const T {
    round_down(x as usize, alignment) as *const T
}

pub fn round_up<
    T: One
        + NumOps
        + BitOr<T, Output = T>
        + Not<Output = T>
        + Copy
        + BitAnd<T, Output = T>
        + FromPrimitive
        + ToPrimitive,
>(
    x: T,
    alignment: usize,
    offset: usize,
) -> T {
    debug_assert!(offset < alignment);
    round_down(
        x + T::from_usize(alignment).unwrap() - T::one() + T::from_usize(offset).unwrap(),
        alignment as _,
    ) - T::from_usize(offset).unwrap()
}

pub fn round_up_unchecked<
    T: One
        + NumOps
        + BitOr<T, Output = T>
        + Not<Output = T>
        + Copy
        + BitAnd<T, Output = T>
        + FromPrimitive
        + ToPrimitive,
>(
    x: T,
    alignment: usize,
    offset: usize,
) -> T {
    round_down_unchecked(
        x + T::from_usize(alignment).unwrap() - T::one() + T::from_usize(offset).unwrap(),
        alignment as _,
    ) - T::from_usize(offset).unwrap()
}

pub fn round_up_ptr<T, U: BitOr<U, Output = U> + Not<Output = U> + Copy>(
    x: *const T,
    alignment: usize,
    offset: usize,
) -> *const T {
    round_up(x as usize, alignment, offset) as *const T
}

pub fn round_up_to_power_of_two(x: usize) -> usize {
    let mut x = x - 1;
    x = x | (x >> 1);
    x = x | (x >> 2);
    x = x | (x >> 4);
    x = x | (x >> 8);
    x = x | (x >> 16);
    #[cfg(target_pointer_width = "64")]
    {
        x = x | (x >> 32);
    }

    x + 1
}

pub fn round_up_usize(x: usize, alignment: usize, offset: usize) -> usize {
    round_down_usize(x + alignment - 1 + offset, alignment).wrapping_sub(offset)
}

pub const fn log2i_graceful(value: usize) -> isize {
    if value == 0 {
        return -1;
    }

    let bits = size_of::<usize>() * 8;

    bits as isize - value.leading_zeros() as isize - 1
}

pub fn parse_float_and_factor_from_str(mut value: &str) -> Option<(f64, usize)> {
    if value.len() > 0 {
        if value.len() > 1
            && (value.as_bytes()[value.len() - 1] == 'b' as u8
                || value.as_bytes()[value.len() - 1] == 'B' as u8)
        {
            value = &value[0..value.len() - 1];
        }
        let mut realvalue = &value[0..value.len() - 1];

        let at = value.len() - 1;
        let factor;
        if value.as_bytes()[at] == 'g' as u8 || value.as_bytes()[at] == 'G' as u8 {
            factor = 1024 * 1024 * 1024;
        } else if value.as_bytes()[at] == 'm' as u8 || value.as_bytes()[at] == 'M' as u8 {
            factor = 1024 * 1024;
        } else if value.as_bytes()[at] == 'k' as u8 || value.as_bytes()[at] == 'K' as u8 {
            factor = 1024;
        } else {
            realvalue = value;
            factor = 1;
        }

        match realvalue.parse::<f64>() {
            Ok(x) => Some((x, factor)),
            _ => None,
        }
    } else {
        None
    }
}

fn read_float_and_factor_from_env(var: &str) -> Option<(f64, usize)> {
    let value = std::env::var(var);

    match value {
        Ok(mut value) => {
            if value.len() > 0 {
                if value.len() > 1
                    && (value.as_bytes()[value.len() - 1] == 'b' as u8
                        || value.as_bytes()[value.len() - 1] == 'B' as u8)
                {
                    value = value.as_str()[0..value.len() - 1].to_string();
                }
                let mut realvalue = value.as_str()[0..value.len() - 1].to_string();

                let at = value.len() - 1;
                let factor;
                if value.as_bytes()[at] == 'g' as u8 || value.as_bytes()[at] == 'G' as u8 {
                    factor = 1024 * 1024 * 1024;
                } else if value.as_bytes()[at] == 'm' as u8 || value.as_bytes()[at] == 'M' as u8 {
                    factor = 1024 * 1024;
                } else if value.as_bytes()[at] == 'k' as u8 || value.as_bytes()[at] == 'K' as u8 {
                    factor = 1024;
                } else {
                    realvalue = value;
                    factor = 1;
                }

                match realvalue.parse::<f64>() {
                    Ok(x) => Some((x, factor)),
                    _ => None,
                }
            } else {
                None
            }
        }
        _ => None,
    }
}

pub fn read_uint_from_env(var: &str) -> Option<usize> {
    let (value, factor) = read_float_and_factor_from_env(var)?;

    Some(value as usize * factor)
}

pub fn read_float_from_env(var: &str) -> Option<f64> {
    read_float_and_factor_from_env(var).map(|x| x.0)
}

fn get_total_memory_linux(_filename: &str) -> usize {
    #[cfg(all(target_pointer_width = "32", unix))]
    {
        return 3 * 1024 * 1024 * 1024;
    }
    #[cfg(all(target_os = "linux", not(target_pointer_width = "32")))]
    unsafe {
        libc::sysconf(libc::_SC_PHYS_PAGES) as usize * libc::sysconf(libc::_SC_PAGESIZE) as usize
    }
    #[cfg(not(target_os = "linux"))]
    {
        ADRESSABLE_SIZE
    }
}

#[cfg(target_os = "windows")]
fn get_total_memory_windows() -> usize {
    use winapi::um::sysinfoapi::GetPhysicallyInstalledSystemMemory;

    unsafe {
        let mut kilobytes = 0;
        let status = GetPhysicallyInstalledSystemMemory(&mut kilobytes);
        if status == 0 {
            ADRESSABLE_SIZE
        } else {
            kilobytes as usize * 1024
        }
    }
}

#[cfg(target_vendor = "apple")]
fn get_darwin_sysctl(name: &str) -> u64 {
    use std::ffi::CString;

    use std::mem::size_of;
    use std::ptr::null_mut;

    unsafe {
        let cstr = CString::new(name).unwrap();
        let mut len = size_of::<u64>();
        let mut buf = [0u8; 16];
        let result = libc::sysctlbyname(
            cstr.as_ptr(),
            &mut buf[0] as *mut u8 as _,
            &mut len,
            null_mut(),
            0,
        );
        if result == 0 && len == size_of::<u64>() {
            let mut value = 0i64;
            std::ptr::copy_nonoverlapping(
                &buf[0],
                &mut value as *mut i64 as *mut u8,
                size_of::<u64>(),
            );
            value as _
        } else {
            ADRESSABLE_SIZE as _
        }
    }
}

pub fn get_total_memory() -> usize {
    /*if cfg!(target_os = "linux") {
        get_total_memory_linux("/proc/meminfo")
    } else if cfg!(target_os = "windows") {
        ADRESSABLE_SIZE
    } else if cfg!(target_os = "macos") || cfg!(target_os="ios") {
        get_darwin_sysctl("hw.memsize") as _
    } else if cfg!(target_os="freebsd") {
        get_darwin_sysctl("hm.usermem") as _
    } else {
        ADRESSABLE_SIZE
    }*/

    cfg_if::cfg_if! {
        if #[cfg(target_os="linux")]
        {
            get_total_memory_linux("/proc/meminfo")
        } else if #[cfg(target_os="windows")]
        {
            get_total_memory_windows()
        } else if #[cfg(any(target_os="macos", target_os="ios", target_os="tvos", target_os="watchos"))]
        {
            get_darwin_sysctl("hw.memsize") as _
        } else if #[cfg(target_os="freebsd")]
        {
            get_darwin_sysctl("hm.usermem") as _
        } else {
            ADRESSABLE_SIZE
        }
    }
}

pub struct FormattedSize {
    pub size: f64,
}

impl std::fmt::Display for FormattedSize {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let ksize = (self.size as f64) / 1024f64;

        if ksize < 1f64 {
            return write!(f, "{}B", self.size);
        }

        let msize = ksize / 1024f64;

        if msize < 1f64 {
            return write!(f, "{:.1}K", ksize);
        }

        let gsize = msize / 1024f64;

        if gsize < 8f64 {
            write!(f, "{:.1}M", msize)
        } else {
            write!(f, "{:.1}G", gsize)
        }
    }
}

impl std::fmt::Debug for FormattedSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

pub fn formatted_size(size: usize) -> FormattedSize {
    FormattedSize { size: size as f64 }
}

pub fn formatted_sizef(size: f64) -> FormattedSize {
    FormattedSize { size: size as f64 }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MemorySize(pub usize);

impl std::fmt::Display for MemorySize {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        FormattedSize {
            size: self.0 as f64,
        }
        .fmt(f)
    }
}
