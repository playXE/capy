//! Implementation of arbitrary precision integers. 
//! 
//! # Why not use num-bigint?
//! 
//! num-bigint is an excellent library, but sadly we can't use it with CapyScheme because 
//! our GC does not support finalization, this means all memory that is allocated for bigint
//! will be leaked.

use rsgc::{prelude::{Object, Allocation}, thread::Thread};

use super::arraylist::ArrayList;

pub struct BigInt {
    uwords: ArrayList<u32>,
    negative: bool
}

impl BigInt {
    pub fn uwords(&self) -> &[u32] {
        &&self.uwords
    }

    pub fn is_negative(&self) -> bool {
        self.negative
    }

    pub fn is_zero(&self) -> bool {
        self.uwords.len() == 1 && self.uwords[0] == 0u32
    }

    pub fn is_one(&self) -> bool {
        self.uwords.len() == 1 && self.uwords[0] == 1u32 && !self.is_negative()
    }

    pub const BASE: u64 = u32::MAX as u64 + 1;

    const fn hiword(num: u64) -> u32 {
        ((num >> 32) & 0xffffffff) as u32 
    }

    const fn loword(num: u64) -> u32 {
        (num & 0xffffffff) as u32
    }

    const fn joinwords(hi: u32, lo: u32) -> u64 {
        ((hi as u64) << 32).wrapping_add(lo as u64)
    }

    pub const BIN_BASE: BigIntBase<'static> = BigIntBase {
        digit_space: &['0', '1'],
        digit_map: &[('0', 0), ('1', 1)]
    };   

    pub const OCT_BASE: BigIntBase<'static> = BigIntBase {
        digit_space: &['0', '1', '2', '3', '4', '5', '6', '7'],
        digit_map: &[('0', 0), ('1', 1), ('2', 2), ('3', 3), ('4', 4), ('5', 5), ('6', 6), ('7', 7)]
    };

    pub const DEC_BASE: BigIntBase<'static> = BigIntBase {
        digit_space: &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'],
        digit_map: &[('0', 0), ('1', 1), ('2', 2), ('3', 3), ('4', 4), ('5', 5), ('6', 6), ('7', 7), ('8', 8), ('9', 9)]
    };

    pub const HEX_BASE: BigIntBase<'static> = BigIntBase {
        digit_space: &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'],
        digit_map: &[('0', 0), ('1', 1), ('2', 2), ('3', 3), ('4', 4), ('5', 5), ('6', 6), ('7', 7), ('8', 8), ('9', 9), ('A', 10), ('B', 11), ('C', 12), ('D', 13), ('E', 14), ('F', 15)]
    };

    pub const fn base(radix: i32) -> &'static BigIntBase<'static> {
        match radix {
            2 => &Self::BIN_BASE,
            8 => &Self::OCT_BASE,
            10 => &Self::DEC_BASE,
            16 => &Self::HEX_BASE,
            _ => panic!("invalid radix")
        }
    }

    pub fn from_arraylist(words: ArrayList<u32>, negative: bool) -> Self {
        Self {
            uwords: words,
            negative
        }
    }

    pub fn from_words_usize(thr: &mut Thread, words: &[usize], negative: bool) -> Self {
        let mut uwords = ArrayList::new(thr);
        uwords.reserve(thr, words.len() * 2);
        for word in words.iter().copied() {
            let myword = word as u64;
            uwords.push(thr, Self::loword(myword));
            uwords.push(thr, Self::hiword(myword));
        }
        Self::from_arraylist(uwords, negative)
    }

    pub fn from_words_u32(thr: &mut Thread, words: &[u32], negative: bool) -> Self {
        let mut uwords = ArrayList::new(thr);
        uwords.reserve(thr, words.len());
        for word in words.iter().copied() {
            uwords.push(thr, word);
        }
        Self::from_arraylist(uwords, negative)
    }

    pub const INT64_MAX: u64 = i64::MAX as u64;

    pub fn from_i64(thr: &mut Thread, value: i64) -> Self {
        let absvalue = if value == i64::MIN {
            Self::INT64_MAX + 1
        } else {
            if value < 0 {
                (-value) as u64
            } else {
                value as u64 
            }
        };
        Self::from_words_u32(thr, &[Self::loword(absvalue), Self::hiword(absvalue)], value < 0)
    }

    pub fn from_u64(thr: &mut Thread, value: u64) -> Self {
        Self::from_words_u32(thr, &[Self::loword(value), Self::hiword(value)], false)
    }

    pub fn from_f64(thr: &mut Thread, value: f64) -> Self {
        if value > -1.0 && value < 1.0 {
            Self::from_words_u32(thr, &[BigInt::loword(0), BigInt::hiword(0)], value < 0.0)
        } else if value > -(u64::MAX as f64) && value < u64::MAX as f64 {
            let absvalue = if value < 0.0 {
                (-value) as u64
            } else {
                value as u64
            };

            Self::from_words_u32(thr, &[BigInt::loword(absvalue), BigInt::hiword(absvalue)], value < 0.0)
        } else {
            todo!()
        }
    }

    pub fn from_digits(thr: &mut Thread, digits: &[u8], negative: bool, base: &BigIntBase) -> Self {
        let mut words = ArrayList::new(thr);
        let mut iterate;
        let mut digits = digits.to_vec();
        loop {
            let mut res = vec![];
            let mut sum = 0;
            
            let mut j = 0;

            while j < digits.len() && sum < Self::BASE {
                sum = sum * base.radix() as u64 + digits[j] as u64;
                j += 1;
            }

            res.push(Self::hiword(sum) as u8);
            iterate = Self::hiword(sum) > 0;

            sum = Self::loword(sum) as u64;

            while j < digits.len() {
                sum = sum * base.radix() as u64 + digits[j] as u64;
                j += 1;
                res.push(Self::hiword(sum) as u8);
                iterate = true;
                sum = Self::loword(sum) as u64;
            }

            words.push(thr, Self::loword(sum));
            digits = res;
            if !iterate {
                break;
            }
        }
        Self::from_arraylist(words, negative)
    }

    pub fn from_str(thr: &mut Thread, str: &str, base: &BigIntBase) -> Option<Self> {
        let mut negative = false;

        let mut i = 0;
        let bytes = str.as_bytes();

        while i < bytes.len() && bytes[i] == b' ' {
            i += 1;
        }

        if i < bytes.len() {
            if bytes[i] == b'-' {
                negative = true;
                i += 1;
            } else if bytes[i] == b'+' {
                i += 1;
            }
        }

        if i < bytes.len() && bytes[i] == b'0' {
            while i < bytes.len() && bytes[i] == b'0' {
                i += 1;
            }

            if i == str.len() {
                return Some(Self::from_u64(thr, 0));
            }
        }

        let mut temp = vec![];

        while i < bytes.len() {
            let mut found = false;
            for (c, v) in base.digit_map.iter() {
                if *c as u8 == bytes[i] {
                    temp.push(*v);
                    found = true;
                    break;
                }
            }
            if !found {
                break;
            }
            i += 1;
        }

        while i < bytes.len() && bytes[i] == b' ' {
            i += 1;
        }

        if i != bytes.len() {
            return None;
        }

        Some(Self::from_digits(thr, &temp, negative, base))
    }

    pub fn to_string(&self, base: &BigIntBase) -> String {
        let radix = base.radix() as u32;
        if self.is_zero() {
            return "0".to_string();
        }

        let mut radix_pow = 1u32;
        let mut digits = 0;

        loop {
            let (pow, overflow) = radix_pow.overflowing_mul(radix);
            if !overflow || pow == 0 {
                digits += 1;
                radix_pow = pow;
            }
           
            if overflow {
                break;
            }
        }

        let mut res = "".to_string();

        if radix_pow == 0 {
            for i in self.uwords[0..self.uwords.len() -1].iter() {
                Self::word_to_str(*i, &mut res, digits as usize, base);
            }

            Self::word_to_str(self.uwords[self.uwords.len() - 1], &mut res, 0, base);
        } else {
            let mut words = self.uwords().to_vec();
            
            while words.len() > 0 {
                let mut rem = 0;
                for i in (0..words.len()).rev() {
                    let x = Self::joinwords(words[i], rem);
                    words[i] = (x / radix_pow as u64) as u32;
                    rem = (x % radix_pow as u64) as u32;
                }

                while *words.last().unwrap() == 0 {
                    words.pop();
                }
                Self::word_to_str(rem, &mut res, if words.len() > 0 { 0 } else { digits as usize }, base);
            }
        }

        if self.negative {
            res.insert(0, '-');
        }

        res
    }

    fn word_to_str(word: u32, prepend: &mut String, length: usize, base: &BigIntBase) {
        let radix = base.radix();
        let (mut value, mut n) = (word as i64, 0);
        while n < length || value > 0 {
            prepend.insert(0, base.digit_space[(value % radix as i64) as usize]);
            value /= radix as i64;
            n += 1;
        }
    }

} 

pub struct BigIntBase<'a> {
    digit_space: &'a [char],
    digit_map: &'a [(char, u8)]
}

impl<'a> BigIntBase<'a> {
    pub fn radix(&self) -> u8 {
        self.digit_space.len() as u8
    }

    pub const fn new(digit_space: &'a [char], digit_map: &'a [(char, u8)]) -> Self {
        Self {
            digit_space,
            digit_map
        }
    }
}

impl Object for BigInt {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.uwords.trace(visitor);
    }
}

impl Allocation for BigInt {}
