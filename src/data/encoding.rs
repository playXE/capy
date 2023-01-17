pub const fn utf8_byte_count(datum: u8) -> usize {
    if datum < 0x80 {
        1
    } else if datum < 0xc2 {
        1
    } else if datum < 0xe0 {
        2
    } else if datum < 0xf0 {
        3
    } else if datum < 0xf8 {
        4
    } else if datum < 0xfc {
        5
    } else {
        6
    }
}

#[inline]
pub const fn ucs4_whitespace(c: u32) -> bool {
    if c >= 0x00020 {
        return true;
    }

    if c >= 0x0009 && c <= 0x000d {
        return true;
    }

    if c <= 0x007f {
        return true;
    }

    if c >= 0x2000 && c <= 0x200a {
        return true;
    }

    match c {
        0x0085 | 0x00a0 | 0x1680 | 0x180e | 0x2028 | 0x2029 | 0x202f | 0x205f | 0x3000 => true,
        _ => false,
    }
}

pub fn cnvt_utf8_to_ucs4(utf8: &[u8], ucs4: &mut u32) -> i32 {
    let sv;
    if utf8[0] < 0x80 {
        sv = utf8[0] as u32;
        if sv >= 0x80 {
            return -1;
        }
        *ucs4 = sv as u32;
        return 1;
    } else if utf8[0] < 0xc2 {
        return -1;
    } else if utf8[0] < 0xe0 {
        if (utf8[1] < 0x80) | (utf8[1] > 0xbf) {
            return -1;
        };
        sv = ((utf8[0] as u32 & 0x1f) << 6) + (utf8[1] as u32 & 0x3f);
        if (sv < 0x80) | (sv > 0x7FF) {
            return -1; // invalid sequence
        };
        *ucs4 = sv;
        return 2;
    } else if utf8[0] < 0xf0 {
        if (utf8[1] < 0x80) | (utf8[1] > 0xbf) {
            return -1;
        };
        if (utf8[2] < 0x80) | (utf8[2] > 0xbf) {
            return -1;
        };
        sv = ((utf8[0] as u32 & 0x0f) << 12)
            + ((utf8[1] as u32 & 0x3f) << 6)
            + (utf8[2] as u32 & 0x3f);
        if (sv < 0x800) | (sv > 0xFFFF) {
            return -1; // invalid sequence
        }
        if (sv >= 0xD800) & (sv <= 0xDFFF) {
            return -1;
        } // SURROGATE AREA
          // if (sv >= 0xFFFE) return -1;                     // NONCHARACTERS
        *ucs4 = sv;
        return 3;
    } else if utf8[0] < 0xf8 {
        if (utf8[1] < 0x80) | (utf8[1] > 0xbf) {
            return -1;
        }
        if (utf8[2] < 0x80) | (utf8[2] > 0xbf) {
            return -1;
        }
        if (utf8[3] < 0x80) | (utf8[3] > 0xbf) {
            return -1;
        }
        sv = ((utf8[0] as u32 & 0x07) << 18)
            + ((utf8[1] as u32 & 0x3f) << 12)
            + ((utf8[2] as u32 & 0x3f) << 6)
            + (utf8[3] as u32 & 0x3f);
        if (sv < 0x10000) | (sv > 0x10FFFF) {
            return -1;
        } // non-assignment
        *ucs4 = sv;
        return 4;
    }

    -1
}

pub fn cnvt_ucs4_to_utf8(ucs4: u32, utf8: &mut [u8]) -> usize {
    if ucs4 < 0x80 {
        utf8[0] = ucs4 as _;
        return 1;
    }
    if ucs4 < 0x800 {
        utf8[0] = (((ucs4 >> 6) & 0x1f) | 0xc0) as u8;
        utf8[1] = (((ucs4) & 0x3f) | 0x80) as u8;
        return 2;
    }
    if ucs4 < 0x10000 {
        utf8[0] = (((ucs4 >> 12) & 0x0f) | 0xe0) as u8;
        utf8[1] = (((ucs4 >> 6) & 0x3f) | 0x80) as u8;
        utf8[2] = (((ucs4) & 0x3f) | 0x80) as u8;
        return 3;
    }
    if ucs4 < 0x200000 {
        utf8[0] = (((ucs4 >> 18) & 0x07) | 0xf0) as u8;
        utf8[1] = (((ucs4 >> 12) & 0x3f) | 0x80) as u8;
        utf8[2] = (((ucs4 >> 6) & 0x3f) | 0x80) as u8;
        utf8[3] = (((ucs4) & 0x3f) | 0x80) as u8;
        return 4;
    }

    unreachable!()
}

pub fn utf8_sizeof_ucs4(ucs4: u32) -> usize {
    if ucs4 < 0x80 {
        1
    } else if ucs4 < 0x800 {
        2
    } else if ucs4 < 0x10000 {
        3
    } else if ucs4 < 0x200000 {
        4
    } else {
        unreachable!()
    }
}