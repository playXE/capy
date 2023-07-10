//! Implementation of arbitrary precision integers.
//!
//! # Why not use num-bigint?
//!
//! num-bigint is an excellent library, but sadly we can't use it with CapyScheme because
//! our GC does not support finalization, this means all memory that is allocated for bigint
//! will be leaked.

use std::{
    cmp::Ordering,
    hash::Hash,
    mem::{offset_of, size_of},
    ops::{Add, Div, Mul, Sub},
};

use rsgc::{
    prelude::{Allocation, Handle, Object},
    thread::Thread,
};

use super::object::*;

#[repr(C)]
pub struct BigInt {
    header: ObjectHeader,
    count: u32,
    negative: bool,
    pad: [u8; 3],
    uwords: [u32; 0],
}

impl BigInt {
    pub fn uwords<'a>(self: &'a Self) -> &'a [u32] {
        unsafe { std::slice::from_raw_parts(self.uwords.as_ptr(), self.count as usize) }
    }

    pub fn uwords_mut<'a>(self: &'a mut Handle<Self>) -> &'a mut [u32] {
        unsafe { std::slice::from_raw_parts_mut(self.uwords.as_mut_ptr(), self.count as usize) }
    }

    pub fn is_negative(&self) -> bool {
        self.negative
    }

    pub fn is_zero(&self) -> bool {
        self.uwords().len() == 1 && self.uwords()[0] == 0u32
    }

    pub fn is_one(self: Handle<Self>) -> bool {
        self.uwords().len() == 1 && self.uwords()[0] == 1u32 && !self.is_negative()
    }

    pub fn negate(self: Handle<Self>, thr: &mut Thread) -> Handle<Self> {
        Self::from_list(thr, self.uwords(), !self.is_negative())
    }

    pub const BASE: u64 = u32::MAX as u64 + 1;

    const fn hiword(num: u64) -> u32 {
        ((num >> 32) & 0xffffffff) as u32
    }

    const fn loword(num: u64) -> u32 {
        (num & 0xffffffff) as u32
    }

    const fn joinwords(lo: u32, hi: u32) -> u64 {
        ((hi as u64) << 32).wrapping_add(lo as u64)
    }

    pub const BIN_BASE: BigIntBase<'static> = BigIntBase {
        digit_space: &['0', '1'],
        digit_map: &[('0', 0), ('1', 1)],
    };

    pub const OCT_BASE: BigIntBase<'static> = BigIntBase {
        digit_space: &['0', '1', '2', '3', '4', '5', '6', '7'],
        digit_map: &[
            ('0', 0),
            ('1', 1),
            ('2', 2),
            ('3', 3),
            ('4', 4),
            ('5', 5),
            ('6', 6),
            ('7', 7),
        ],
    };

    pub const DEC_BASE: BigIntBase<'static> = BigIntBase {
        digit_space: &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'],
        digit_map: &[
            ('0', 0),
            ('1', 1),
            ('2', 2),
            ('3', 3),
            ('4', 4),
            ('5', 5),
            ('6', 6),
            ('7', 7),
            ('8', 8),
            ('9', 9),
        ],
    };

    pub const HEX_BASE: BigIntBase<'static> = BigIntBase {
        digit_space: &[
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
        ],
        digit_map: &[
            ('0', 0),
            ('1', 1),
            ('2', 2),
            ('3', 3),
            ('4', 4),
            ('5', 5),
            ('6', 6),
            ('7', 7),
            ('8', 8),
            ('9', 9),
            ('A', 10),
            ('B', 11),
            ('C', 12),
            ('D', 13),
            ('E', 14),
            ('F', 15),
        ],
    };

    pub const fn base(radix: i32) -> &'static BigIntBase<'static> {
        match radix {
            2 => &Self::BIN_BASE,
            8 => &Self::OCT_BASE,
            10 => &Self::DEC_BASE,
            16 => &Self::HEX_BASE,
            _ => panic!("invalid radix"),
        }
    }

    pub fn from_list(thr: &mut Thread, mut words: &[u32], negative: bool) -> Handle<Self> {
        while words.len() > 1 && words[words.len() - 1] == 0 {
            words = &words[..words.len() - 1];
        }

        let mut this = thr.allocate_varsize::<Self>(words.len());

        unsafe {
            let t = this.assume_init_mut();
            t.count = words.len() as _;
            t.negative = negative;
            t.header.typ = Type::BigNum;

            for i in 0..words.len() {
                t.uwords.as_mut_ptr().add(i).write(words[i]);
            }

            this.assume_init()
        }
    }

    pub fn from_words_usize(thr: &mut Thread, words: &[usize], negative: bool) -> Handle<Self> {
        let mut uwords = Vec::new();
        uwords.reserve(words.len() * 2);
        for word in words.iter().copied() {
            let myword = word as u64;
            uwords.push(Self::loword(myword));
            uwords.push(Self::hiword(myword));
        }
        Self::from_list(thr, &uwords, negative)
    }

    pub fn from_words_u32(thr: &mut Thread, words: &[u32], negative: bool) -> Handle<Self> {
        Self::from_list(thr, words, negative)
    }

    pub const INT64_MAX: u64 = i64::MAX as u64;

    pub fn from_i64(thr: &mut Thread, value: i64) -> Handle<Self> {
        let absvalue = if value == i64::MIN {
            Self::INT64_MAX + 1
        } else {
            if value < 0 {
                (-value) as u64
            } else {
                value as u64
            }
        };
        Self::from_words_u32(
            thr,
            &[Self::loword(absvalue), Self::hiword(absvalue)],
            value < 0,
        )
    }

    pub fn from_u64(thr: &mut Thread, value: u64) -> Handle<Self> {
        Self::from_words_u32(thr, &[Self::loword(value), Self::hiword(value)], false)
    }

    pub fn from_f64(thr: &mut Thread, value: f64) -> Handle<Self> {
        if value > -1.0 && value < 1.0 {
            Self::from_words_u32(thr, &[BigInt::loword(0), BigInt::hiword(0)], value < 0.0)
        } else if value > -(u64::MAX as f64) && value < u64::MAX as f64 {
            let absvalue = if value < 0.0 {
                (-value) as u64
            } else {
                value as u64
            };

            Self::from_words_u32(
                thr,
                &[BigInt::loword(absvalue), BigInt::hiword(absvalue)],
                value < 0.0,
            )
        } else {
            todo!()
        }
    }

    pub fn from_digits(
        thr: &mut Thread,
        digits: &[u8],
        negative: bool,
        base: &BigIntBase,
    ) -> Handle<Self> {
        let mut words = Vec::new();
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

            words.push(Self::loword(sum));
            digits = res;
            if !iterate {
                break;
            }
        }
        Self::from_list(thr, &words, negative)
    }

    pub fn from_str(thr: &mut Thread, str: &str, base: &BigIntBase) -> Option<Handle<Self>> {
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

    pub fn to_string(self: Handle<Self>, base: &BigIntBase) -> String {
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
            for i in self.uwords()[0..self.uwords().len() - 1].iter() {
                Self::word_to_str(*i, &mut res, digits as usize, base);
            }

            Self::word_to_str(self.uwords()[self.uwords().len() - 1], &mut res, 0, base);
        } else {
            let mut words = self.uwords().to_vec();

            while words.len() > 0 {
                let mut rem = 0;
                for i in (0..words.len()).rev() {
                    let x = Self::joinwords(words[i], rem);
                    words[i] = (x / radix_pow as u64) as u32;
                    rem = (x % radix_pow as u64) as u32;
                }

                while words.last().copied() == Some(0) {
                    words.pop();
                }
                Self::word_to_str(
                    rem,
                    &mut res,
                    if words.len() > 0 { digits as usize } else { 0 },
                    base,
                );
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

    pub fn i32(self: Handle<Self>) -> Option<i32> {
        if self.uwords().len() > 1 {
            return None;
        }

        let value = self.uwords()[0];
        if self.negative && value == i32::MAX as u32 + 1 {
            return Some(i32::MIN);
        }

        if value <= i32::MAX as u32 {
            return Some(if self.negative {
                -(value as i32)
            } else {
                value as i32
            });
        }

        None
    }

    pub fn u32(self: Handle<Self>) -> Option<u32> {
        if self.uwords().len() > 1 || self.negative {
            return None;
        }

        let value = self.uwords()[0];

        Some(value)
    }

    pub fn u64(self: Handle<Self>) -> Option<u64> {
        if self.uwords().len() > 2 || self.negative {
            return None;
        }

        let mut value = self.uwords()[0usize] as u64;

        if self.uwords().len() == 2 {
            value += self.uwords()[1] as u64 * Self::BASE;
        }
        Some(value)
    }

    pub fn i64(self: Handle<Self>) -> Option<i64> {
        if self.uwords().len() > 2 {
            return None;
        }

        let mut value = self.uwords()[0usize] as u64;

        if self.uwords().len() == 2 {
            value += self.uwords()[1] as u64 * Self::BASE;
        }

        if self.negative && value == i64::MAX as u64 + 1 {
            return Some(i64::MIN);
        }

        if value <= i64::MAX as u64 {
            return Some(if self.negative {
                -(value as i64)
            } else {
                value as i64
            });
        }

        None
    }

    pub fn f64(self: Handle<Self>) -> f64 {
        let mut res = 0.0;
        /*for word in self.uwords().iter().rev() {
            res = res * Self::BASE as f64 + (*word) as f64;
        }*/

        let words = self.uwords();
        for i in (0..words.len()).rev() {
            unsafe {
                res = res * Self::BASE as f64 + *words.get_unchecked(i) as f64;
            }
        }
        if self.negative {
            -res
        } else {
            res
        }
    }

    pub fn abs(self: Handle<Self>, thr: &mut Thread) -> Handle<Self> {
        if !self.negative {
            return self;
        }
        Self::from_words_u32(thr, self.uwords(), false)
    }

    pub fn compare(self: &Self, rhs: &Self) -> Ordering {
        if self.negative != rhs.negative {
            if self.negative {
                return Ordering::Less;
            } else {
                return Ordering::Greater;
            }
        }

        if self.negative {
            rhs.compare_digits(self)
        } else {
            self.compare_digits(rhs)
        }
    }

    fn compare_digits(self: &Self, rhs: &Self) -> Ordering {
        if self.uwords().len() != rhs.uwords().len() {
            if self.uwords().len() < rhs.uwords().len() {
                return Ordering::Less;
            } else {
                return Ordering::Greater;
            }
        }

        for i in 1..self.uwords().len() {
            let a = self.uwords()[self.uwords().len() - i];
            let b = rhs.uwords()[self.uwords().len() - i];
            if a != b {
                if a < b {
                    return Ordering::Less;
                } else {
                    return Ordering::Greater;
                }
            }
        }

        Ordering::Equal
    }

    fn plus_fast_digits<const N: usize>(
        thr: &mut Thread,
        negative: bool,
        b1: Handle<Self>,
        b2: Handle<BigInt>,
    ) -> Handle<Self> {
        let mut res = [0u32; N];
        let mut sum = 0u64;
        unsafe {
            let mut c = 0;
            for i in 0..b2.count {
                let i = i as usize;
                sum += *b1.uwords().get_unchecked(i) as u64;
                sum += *b2.uwords().get_unchecked(i) as u64;
                *res.get_unchecked_mut(c) = Self::loword(sum);
                c += 1;
                sum = Self::hiword(sum) as u64;
            }

            for i in b2.count..b1.count {
                let i = i as usize;
                sum += b1.uwords()[i] as u64;
                c += 1;
                *res.get_unchecked_mut(c) = Self::loword(sum);
                sum = Self::hiword(sum) as u64;
            }

            if sum > 0 {
                res[c] = sum as u32;
            }
        }
        Self::from_list(thr, &res, negative)
    }

    pub fn plus(self: Handle<Self>, thr: &mut Thread, rhs: Handle<Self>) -> Handle<Self> {
        if rhs.is_zero() {
            return self;
        }

        if self.negative != rhs.negative {
            let rhs = rhs.negate(thr);
            return self.minus(thr, rhs);
        }

        let (b1, b2) = if self.count < rhs.count {
            (rhs, self)
        } else {
            (self, rhs)
        };

        // When the number of digits is small, we execute a fast path that does not allocate
        // heap buffer for addition.
        if false {
            if b2.count <= 2 {
                return Self::plus_fast_digits::<2>(thr, self.negative, b1, b2);
            } else if b2.count <= 4 {
                return Self::plus_fast_digits::<4>(thr, self.negative, b1, b2);
            } else if b2.count <= 8 {
                return Self::plus_fast_digits::<8>(thr, self.negative, b1, b2);
            } else if b2.count <= 16 {
                return Self::plus_fast_digits::<16>(thr, self.negative, b1, b2);
            }
        }
        Self::plus_generic(thr, self.negative, b1, b2)
    }

    #[cold]
    fn plus_generic(
        thr: &mut Thread,
        negative: bool,
        b1: Handle<Self>,
        b2: Handle<BigInt>,
    ) -> Handle<BigInt> {
        let mut res = vec![];
        res.reserve(b1.count as _);

        let mut sum = 0u64;

        for i in 0..b2.count {
            let i = i as usize;
            sum += b1.uwords()[i] as u64;
            sum += b2.uwords()[i] as u64;
            res.push(Self::loword(sum));
            sum = Self::hiword(sum) as u64;
        }

        for i in b2.count..b1.count {
            let i = i as usize;
            sum += b1.uwords()[i] as u64;
            res.push(Self::loword(sum));
            sum = Self::hiword(sum) as u64;
        }

        if sum > 0 {
            res.push(Self::loword(sum));
        }

        Self::from_list(thr, &res, negative)
    }

    pub fn minus(self: Handle<Self>, thr: &mut Thread, rhs: Handle<Self>) -> Handle<Self> {
        if rhs.is_zero() {
            return self;
        }

        if self.negative != rhs.negative {
            let rhs = rhs.negate(thr);
            return self.plus(thr, rhs);
        }

        let cmp = self.compare_digits(&rhs);
        if cmp == Ordering::Equal {
            return BigInt::from_list(thr, &[0], false);
        }
        let negative = if cmp == Ordering::Less {
            !self.negative
        } else {
            self.negative
        };

        let (b1, b2) = if cmp == Ordering::Less {
            (rhs, self)
        } else {
            (self, rhs)
        };

        let mut res = vec![];
        let mut carry = 0;
        for i in 0..b2.count as usize {
            if (b1.uwords()[i] as u64) < b2.uwords()[i] as u64 + carry {
                res.push(
                    (Self::BASE + b1.uwords()[i] as u64 - b2.uwords()[i] as u64 - carry) as u32,
                );
                carry = 1;
            } else {
                res.push((b1.uwords()[i] as u64 - b2.uwords()[i] as u64 - carry) as u32);
                carry = 0;
            }
        }

        for i in b2.count as usize..b1.count as usize {
            if (b1.uwords()[i] as u64) < carry {
                res.push(u32::MAX as u32);
                carry = 1;
            } else {
                res.push((b1.uwords()[i] as u64 - carry) as u32);
                carry = 0;
            }
        }

        BigInt::from_list(thr, &res, negative)
    }

    pub fn times(self: Handle<Self>, thr: &mut Thread, rhs: Handle<Self>) -> Handle<Self> {
        let (b1, b2) = if self.count < rhs.count {
            (rhs, self)
        } else {
            (self, rhs)
        };      
       
        b1.times_generic(thr, b2)
    }

    fn times_generic(self: Handle<Self>, thr: &mut Thread, rhs: Handle<Self>) -> Handle<Self> {
        let mut res = vec![0; (self.count + rhs.count) as usize];
        unsafe {
            for i in 0..rhs.count as usize {
                let mut sum = 0;
                for j in 0..self.count as usize {
                    let mult = *self.uwords().get_unchecked(j) as u64
                        * *rhs.uwords().get_unchecked(i) as u64;
                    sum += *res.get_unchecked(i + j) as u64 + mult;
                    *res.get_unchecked_mut(i + j) = Self::loword(sum);
                    sum = Self::hiword(sum) as u64;
                }

                *res.get_unchecked_mut(i + self.uwords().len()) = Self::loword(sum);
            }

            BigInt::from_list(thr, &res, self.negative != rhs.negative)
        }
    }

    fn mult_sub(approx: u32, divis: &[u32], rem: &mut Vec<u32>, from: usize) {
        let mut sum = 0;
        let mut carry = 0;
        for j in 0..divis.len() {
            sum += divis[j] as u64 * approx as u64;
            let x = Self::loword(sum) as u64 + carry;

            if (rem[from + j] as u64) < x {
                rem[from + j] = (Self::BASE + rem[from + j] as u64 - x) as u32;
                carry = 1;
            } else {
                rem[from + j] = (rem[from + j] as u64 - x) as u32;
                carry = 0;
            }
            sum = Self::hiword(sum) as u64;
        }
    }

    fn sub_if_possible(divis: &[u32], rem: &mut Vec<u32>, from: usize) -> bool {
        let mut i = divis.len();

        while i > 0 && divis[i - 1] >= rem[from + i - 1] {
            if divis[i - 1] > rem[from + i - 1] {
                return false;
            }

            i -= 1;
        }

        let mut carry = 0;
        for j in 0..divis.len() {
            let x = divis[j] as u64 + carry;
            if (rem[from + j] as u64) < x {
                rem[from + j] = (Self::BASE + rem[from + j] as u64 - x) as u32;
                carry = 1;
            } else {
                rem[from + j] = (rem[from + j] as u64 - x) as u32;
                carry = 0;
            }
        }

        true
    }

    pub fn divided(
        self: Handle<Self>,
        thr: &mut Thread,
        rhs: Handle<Self>,
    ) -> (Handle<Self>, Handle<Self>) {
        if rhs.uwords().len() <= self.uwords().len() {
            return (BigInt::from_i64(thr, 0), self);
        } else {
            let neg = self.negative != rhs.negative;

            if rhs.uwords().len() == self.uwords().len() {
                match self.compare_digits(&rhs) {
                    Ordering::Equal => {
                        return (
                            BigInt::from_i64(thr, if neg { -1 } else { 1 }),
                            BigInt::from_i64(thr, 0),
                        )
                    }
                    Ordering::Less => return (BigInt::from_i64(thr, 0), self),
                    _ => (),
                }
            }

            let mut rem = self.uwords().to_vec();
            rem.push(0);
            let mut divis = rhs.uwords().to_vec();
            divis.push(0);

            let mut sizediff = rem.len() as isize - divis.len() as isize;

            let div = rhs.uwords()[rhs.uwords().len() - 1] as u64 + 1;
            let mut res = vec![0u32; sizediff as usize + 1];
            let mut divident = rem.len() - 2;
            loop {
                let mut x = Self::joinwords(rem[divident], rem[divident + 1]);
                let mut approx = x / div;
                while approx > 0 {
                    res[sizediff as usize] += approx as u32;
                    Self::mult_sub(approx as _, &divis, &mut rem, sizediff as _);
                    x = Self::joinwords(rem[divident], rem[divident + 1]);
                    approx = x / div;
                }

                if Self::sub_if_possible(&divis, &mut rem, sizediff as _) {
                    res[sizediff as usize] += 1;
                }

                divident -= 1;
                sizediff -= 1;

                if sizediff < 0 {
                    break;
                }
            }

            let quotient = BigInt::from_words_u32(thr, &res, neg);
            let remainder = BigInt::from_words_u32(thr, &rem, self.negative);
            (quotient, remainder)
        }
    }

    fn is_most_significant_bit_set(words: &[u32]) -> bool {
        (words.last().copied().unwrap() & (1 << (32 - 1))) != 0
    }

    fn from_two_complement(thr: &mut Thread, words: &mut [u32]) -> Handle<Self> {
        if Self::is_most_significant_bit_set(words) {
            let mut carry = true;

            for i in 0..words.len() {
                if carry {
                    (words[i], carry) = (!words[i]).overflowing_add(1);
                } else {
                    words[i] = !words[i]
                }
            }

            BigInt::from_words_u32(thr, words, true)
        } else {
            BigInt::from_words_u32(thr, words, false)
        }
    }

    fn two_complement_size(left: &[u32], right: &[u32]) -> usize {
        (left.len() + Self::is_most_significant_bit_set(left) as usize)
            .max(right.len() + Self::is_most_significant_bit_set(right) as usize)
    }

    pub fn to_power(self: Handle<Self>, thr: &mut Thread, exp: Handle<Self>) -> Handle<Self> {
        let (mut expo, mut radix) = (exp, self);
        let mut res = BigInt::from_i64(thr, 1);
        let two = BigInt::from_u64(thr, 2);
        while !expo.is_zero() {
            if !expo.and32(thr, 1, false).is_zero() {
                res = res.times(thr, radix);
            }

            expo = expo.divided(thr, two).0;
            radix = radix.times(thr, radix);
        }
        res
    }

    pub fn sqrt(self: Handle<Self>, thr: &mut Thread) -> Handle<BigInt> {
        assert!(
            !self.negative,
            "cannot compute square root of negative number"
        );

        if self.is_zero() || self.is_one() {
            return self;
        }

        let two = BigInt::from_u64(thr, 2);

        let mut y = self.divided(thr, two).0;
        let mut x = self.divided(thr, y).0;

        while y.compare(&x) == Ordering::Greater {
            y = x.plus(thr, y).divided(thr, two).0;
            x = self.divided(thr, y).0;
        }

        y
    }

    pub fn not(self: Handle<Self>, thr: &mut Thread) -> Handle<Self> {
        let one = BigInt::from_i64(thr, 1);
        self.negate(thr).minus(thr, one)
    }

    pub fn and(self: Handle<Self>, thr: &mut Thread, rhs: Handle<Self>) -> Handle<BigInt> {
        let (mut leftcarry, mut rightcarry) = (true, true);
        let size = Self::two_complement_size(self.uwords(), rhs.uwords());
        let mut res = vec![];
        res.reserve(size);

        for i in 0..size {
            let mut leftword = if i < self.uwords().len() {
                self.uwords()[i]
            } else {
                0
            };
            if self.negative {
                if leftcarry {
                    (leftword, leftcarry) = (!leftword).overflowing_add(1);
                } else {
                    leftword = !leftword;
                }
            }

            let mut rightword = if i < rhs.uwords().len() {
                rhs.uwords()[i]
            } else {
                0
            };

            if rhs.negative {
                if rightcarry {
                    (rightword, rightcarry) = (!rightword).overflowing_add(1);
                } else {
                    rightword = !rightword;
                }
            }

            res.push(leftword & rightword);
        }

        Self::from_two_complement(thr, &mut res)
    }

    pub fn and32(self: Handle<Self>, thr: &mut Thread, rhs: u32, negative: bool) -> Handle<BigInt> {
        let (mut leftcarry, mut rightcarry) = (true, true);
        let size = Self::two_complement_size(self.uwords(), &[rhs]);
        let mut res = vec![];
        res.reserve(size);

        for i in 0..size {
            let mut leftword = if i < self.uwords().len() {
                self.uwords()[i]
            } else {
                0
            };
            if self.negative {
                if leftcarry {
                    (leftword, leftcarry) = (!leftword).overflowing_add(1);
                } else {
                    leftword = !leftword;
                }
            }

            let mut rightword = if i < 1 { rhs as u32 } else { 0 };

            if negative {
                if rightcarry {
                    (rightword, rightcarry) = (!rightword).overflowing_add(1);
                } else {
                    rightword = !rightword;
                }
            }

            res.push(leftword & rightword);
        }

        Self::from_two_complement(thr, &mut res)
    }

    pub fn or32(self: Handle<Self>, thr: &mut Thread, rhs: u32, negative: bool) -> Handle<BigInt> {
        let (mut leftcarry, mut rightcarry) = (true, true);
        let size = Self::two_complement_size(self.uwords(), &[rhs]);
        let mut res = vec![];
        res.reserve(size);

        for i in 0..size {
            let mut leftword = if i < self.uwords().len() {
                self.uwords()[i]
            } else {
                0
            };
            if self.negative {
                if leftcarry {
                    (leftword, leftcarry) = (!leftword).overflowing_add(1);
                } else {
                    leftword = !leftword;
                }
            }

            let mut rightword = if i < 1 { rhs as u32 } else { 0 };

            if negative {
                if rightcarry {
                    (rightword, rightcarry) = (!rightword).overflowing_add(1);
                } else {
                    rightword = !rightword;
                }
            }

            res.push(leftword | rightword);
        }

        Self::from_two_complement(thr, &mut res)
    }

    pub fn xor32(self: Handle<Self>, thr: &mut Thread, rhs: u32, negative: bool) -> Handle<BigInt> {
        let (mut leftcarry, mut rightcarry) = (true, true);
        let size = Self::two_complement_size(self.uwords(), &[rhs]);
        let mut res = vec![];
        res.reserve(size);

        for i in 0..size {
            let mut leftword = if i < self.uwords().len() {
                self.uwords()[i]
            } else {
                0
            };
            if self.negative {
                if leftcarry {
                    (leftword, leftcarry) = (!leftword).overflowing_add(1);
                } else {
                    leftword = !leftword;
                }
            }

            let mut rightword = if i < 1 { rhs as u32 } else { 0 };

            if negative {
                if rightcarry {
                    (rightword, rightcarry) = (!rightword).overflowing_add(1);
                } else {
                    rightword = !rightword;
                }
            }

            res.push(leftword ^ rightword);
        }

        Self::from_two_complement(thr, &mut res)
    }

    pub fn or(self: Handle<Self>, thr: &mut Thread, rhs: Handle<Self>) -> Handle<BigInt> {
        let (mut leftcarry, mut rightcarry) = (true, true);
        let size = Self::two_complement_size(self.uwords(), rhs.uwords());
        let mut res = vec![];
        res.reserve(size);

        for i in 0..size {
            let mut leftword = if i < self.uwords().len() {
                self.uwords()[i]
            } else {
                0
            };
            if self.negative {
                if leftcarry {
                    (leftword, leftcarry) = (!leftword).overflowing_add(1);
                } else {
                    leftword = !leftword;
                }
            }

            let mut rightword = if i < rhs.uwords().len() {
                rhs.uwords()[i]
            } else {
                0
            };

            if rhs.negative {
                if rightcarry {
                    (rightword, rightcarry) = (!rightword).overflowing_add(1);
                } else {
                    rightword = !rightword;
                }
            }

            res.push(leftword | rightword);
        }

        Self::from_two_complement(thr, &mut res)
    }

    pub fn xor(self: Handle<Self>, thr: &mut Thread, rhs: Handle<Self>) -> Handle<BigInt> {
        let (mut leftcarry, mut rightcarry) = (true, true);
        let size = Self::two_complement_size(self.uwords(), rhs.uwords());
        let mut res = vec![];
        res.reserve(size);

        for i in 0..size {
            let mut leftword = if i < self.uwords().len() {
                self.uwords()[i]
            } else {
                0
            };
            if self.negative {
                if leftcarry {
                    (leftword, leftcarry) = (!leftword).overflowing_add(1);
                } else {
                    leftword = !leftword;
                }
            }

            let mut rightword = if i < rhs.uwords().len() {
                rhs.uwords()[i]
            } else {
                0
            };

            if rhs.negative {
                if rightcarry {
                    (rightword, rightcarry) = (!rightword).overflowing_add(1);
                } else {
                    rightword = !rightword;
                }
            }

            res.push(leftword ^ rightword);
        }

        Self::from_two_complement(thr, &mut res)
    }

    pub fn bit_size(self: Handle<Self>) -> u32 {
        self.count * 32
    }

    pub fn bit_count(self: Handle<Self>) -> u32 {
        if self.negative {
            return !self.not(Thread::current()).bit_count();
        }
        let mut count = 0;
        for word in self.uwords() {
            count += word.count_ones();
        }
        count
    }

    pub fn zero_count(self: Handle<Self>) -> u32 {
        if self.negative {
            return !self.not(Thread::current()).zero_count();
        }
        let mut count = 0;
        for word in self.uwords() {
            count += word.count_zeros();
        }
        count
    }

    pub fn first_bit_set(self: Handle<Self>) -> Option<usize> {
        if self.is_zero() {
            return None;
        }

        let mut i = 0;
        let mut carry = true;

        for word in self.uwords() {
            let mut word = *word;
            if self.negative {
                if carry {
                    (word, carry) = (!word).overflowing_add(1);
                } else {
                    word = !word;
                }
            }

            if word != 0 {
                return Some(i * 32 + word.trailing_zeros() as usize);
            }

            i += 1;
        }

        Some(i * 32)
    }

    pub fn last_bit_set(self: Handle<Self>) -> Option<usize> {
        if self.is_zero() {
            return None;
        }

        let number = if self.negative {
            self.not(Thread::current())
        } else {
            self
        };

        let mut i = number.uwords().len() - 1;
        while i > 0 && number.uwords()[i] == 0 {
            i -= 1;
        }

        return Some(i * 32 + 32 - number.uwords()[i].leading_zeros() as usize);
    }

    pub fn is_bit_set(self: Handle<Self>, bit: usize) -> bool {
        if self.is_zero() {
            return false;
        }
        let nword = bit / 32;
        let nbit = bit % 32;

        if nword >= self.uwords().len() {
            return self.negative;
        }

        if !self.negative {
            return (self.uwords()[nword] & (1 << nbit)) != 0;
        }

        let mut i = 0;
        let mut word = 0;
        let mut carry = true;

        while i <= nword {
            word = self.uwords()[i];

            if self.negative {
                if carry {
                    (word, carry) = (!word).overflowing_add(1);
                } else {
                    word = !word;
                }
            }

            i += 1;
        }

        (word & (1 << nbit)) != 0
    }

    pub fn set_bit(self: Handle<Self>, thr: &mut Thread, n: usize, value: bool) -> Handle<Self> {
        let mut words = self.uwords().to_vec();
        let nword = n / 32;
        let nbit = n % 32;

        let mut i = words.len();

        while i <= nword {
            words.push(0);
            i += 1;
        }

        if !self.negative {
            if value {
                words[nword] |= 1 << nbit;
            } else {
                words[nword] &= !(1 << nbit);
            }

            return BigInt::from_list(thr, &words, false);
        }

        if Self::is_most_significant_bit_set(&words) {
            words.push(0);
        }

        let mut carry = true;

        for i in 0..words.len() {
            if carry {
                (words[i], carry) = (!words[i]).overflowing_add(1);
            } else {
                words[i] = !words[i];
            }
        }

        if value {
            words[nword] |= 1 << nbit;
        } else {
            words[nword] &= !(1 << nbit);
        }

        Self::from_two_complement(thr, &mut words)
    }

    pub fn is_odd(self: Handle<Self>) -> bool {
        self.and32(Thread::current(), 1, false).is_one()
    }

    pub fn remainder_digit(self: Handle<Self>, denominator: i32) -> i32 {
        let count = self.count;

        let mut remainder = 0i64;

        for i in (0..=(count - 1)).rev() {
            remainder = ((remainder << 32) + self.uwords()[i as usize] as i64) % denominator as i64;
        }

        remainder as i32
    }
}

pub struct BigIntBase<'a> {
    digit_space: &'a [char],
    digit_map: &'a [(char, u8)],
}

impl<'a> BigIntBase<'a> {
    pub fn radix(&self) -> u8 {
        self.digit_space.len() as u8
    }

    pub const fn new(digit_space: &'a [char], digit_map: &'a [(char, u8)]) -> Self {
        Self {
            digit_space,
            digit_map,
        }
    }
}

unsafe impl Object for BigInt {}

unsafe impl Allocation for BigInt {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = size_of::<u32>();
    const VARSIZE_NO_HEAP_PTRS: bool = true;
    const VARSIZE_OFFSETOF_LENGTH: usize = 0;
    const VARSIZE_OFFSETOF_CAPACITY: usize = 0;
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(Self, uwords);
}

impl Add for &BigInt {
    type Output = Handle<BigInt>;
    fn add(self, rhs: Self) -> Self::Output {
        unsafe {
            let this = Handle::from_raw(self as *const _ as *mut _);
            let rhs = Handle::from_raw(rhs as *const _ as *mut _);
            BigInt::plus(this, Thread::current(), rhs)
        }
    }
}

impl Sub for &BigInt {
    type Output = Handle<BigInt>;
    fn sub(self, rhs: Self) -> Self::Output {
        unsafe {
            let this = Handle::from_raw(self as *const _ as *mut _);
            let rhs = Handle::from_raw(rhs as *const _ as *mut _);
            BigInt::minus(this, Thread::current(), rhs)
        }
    }
}

impl Div for &BigInt {
    type Output = Handle<BigInt>;
    fn div(self, rhs: Self) -> Self::Output {
        unsafe {
            let this = Handle::from_raw(self as *const _ as *mut _);
            let rhs = Handle::from_raw(rhs as *const _ as *mut _);
            BigInt::divided(this, Thread::current(), rhs).0
        }
    }
}

impl Mul for &BigInt {
    type Output = Handle<BigInt>;
    fn mul(self, rhs: Self) -> Self::Output {
        unsafe {
            let this = Handle::from_raw(self as *const _ as *mut _);
            let rhs = Handle::from_raw(rhs as *const _ as *mut _);
            BigInt::times(this, Thread::current(), rhs)
        }
    }
}

impl PartialEq for BigInt {
    fn eq(&self, other: &Self) -> bool {
        self.compare(other) == Ordering::Equal
    }
}

impl Eq for BigInt {}

impl PartialOrd for BigInt {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.compare(other))
    }
}

impl Ord for BigInt {
    fn cmp(&self, other: &Self) -> Ordering {
        self.compare(other)
    }
}

impl Hash for BigInt {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.uwords().hash(state);
        self.negative.hash(state);
    }
}

impl std::fmt::Display for BigInt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let radix = 10;
        if self.is_zero() {
            return write!(f, "0");
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
            for i in self.uwords()[0..self.uwords().len() - 1].iter() {
                Self::word_to_str(*i, &mut res, digits as usize, &BigInt::DEC_BASE);
            }

            Self::word_to_str(
                self.uwords()[self.uwords().len() - 1],
                &mut res,
                0,
                &BigInt::DEC_BASE,
            );
        } else {
            let mut words = self.uwords().to_vec();

            while words.len() > 0 {
                let mut rem = 0;
                for i in (0..words.len()).rev() {
                    let x = Self::joinwords(words[i], rem);
                    words[i] = (x / radix_pow as u64) as u32;
                    rem = (x % radix_pow as u64) as u32;
                }

                while words.last().copied() == Some(0) {
                    words.pop();
                }
                Self::word_to_str(
                    rem,
                    &mut res,
                    if words.len() > 0 { digits as usize } else { 0 },
                    &BigInt::DEC_BASE,
                );
            }
        }

        if self.negative {
            res.insert(0, '-');
        }

        write!(f, "{}", res)
    }
}
