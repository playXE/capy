use std::{
    cmp::Ordering,
    mem::{size_of, transmute},
};

use mmtk::{util::ObjectReference, AllocationSemantics, MutatorContext};

use crate::{gc::CapyVM, gc_protect, raise_exn, utils::round_up, vm::thread::Thread};

use super::{
    object::{ScmCellHeader, ScmComplex, ScmRational, TypeId},
    value::Value,
};

pub fn scm_to_u64(arg: Value) -> u64 {
    if arg.is_int32() {
        arg.get_int32() as u64
    } else {
        raise_exn!(
            FailContract,
            &[],
            "scm_to_uint64: not an exact integer: {}",
            arg
        )
    }
}

pub fn scm_to_u32(arg: Value) -> u32 {
    if arg.is_int32() {
        arg.get_int32() as u32
    } else {
        raise_exn!(
            FailContract,
            &[],
            "scm_to_u32: not an exact integer: {}",
            arg
        )
    }
}

pub fn scm_to_usize(arg: Value) -> usize {
    if arg.is_int32() {
        arg.get_int32() as usize
    } else {
        raise_exn!(
            FailContract,
            &[],
            "scm_to_usize: not an exact integer: {}",
            arg
        )
    }
}

const IEXPT_2N52: i64 = 0x10000000000000;
const IEXPT_2N53: i64 = 0x20000000000000;

#[repr(C)]
pub struct ScmBigInteger {
    pub(crate) header: ScmCellHeader,
    pub(crate) count: u32,
    negative: bool,
    uwords: [u32; 1],
}

impl ScmBigInteger {
    pub fn digits(&self) -> &[u32] {
        unsafe { std::slice::from_raw_parts(self.uwords.as_ptr(), self.count as usize) }
    }

    fn digits_mut(&mut self) -> &mut [u32] {
        unsafe { std::slice::from_raw_parts_mut(self.uwords.as_mut_ptr(), self.count as usize) }
    }

    fn ndigits(&self) -> u32 {
        self.count
    }

    pub fn is_zero(&self) -> bool {
        self.digits().len() == 1 && self.digits()[0] == 0
    }

    pub fn is_one(&self) -> bool {
        self.digits().len() == 1 && self.digits()[0] == 1 && !self.is_negative()
    }

    pub fn is_negative(&self) -> bool {
        self.negative
    }

    pub fn is_positive(&self) -> bool {
        !self.is_negative()
    }

    fn negate(&self, thread: &mut Thread) -> Value {
        let digits = self.digits().to_vec();
        let negative = !self.is_negative();

        let bn = thread.make_bignum_from_digits(&digits, negative);
        bn
    }

    const BASE: u64 = u32::MAX as u64 + 1;

    const fn hiword(x: u64) -> u32 {
        ((x >> 32) & 0xffffffff) as u32
    }

    const fn loword(x: u64) -> u32 {
        (x & 0xffffffff) as u32
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

    pub fn to_string_base(&self, base: &BigIntBase) -> String {
        let radix = base.radix() as u32;
        assert!(radix == 10);
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

        let mut res = String::with_capacity(8);

        let mut prepend = |word: u32, length: i32| {
            let radix = base.radix();
            let (mut value, mut n) = (word as i64, 0);
            while n < length || value > 0 {
                res.insert(0, base.digit_space[(value % radix as i64) as usize]);
                value /= radix as i64;
                n += 1;
            }
        };

        if radix_pow == 0 {
            for word in self.digits()[0..self.digits().len() - 1].iter() {
                prepend(*word, digits);
            }
            prepend(self.digits().last().copied().unwrap(), digits);
        } else {
            let mut words = self.digits().to_vec();

            while !words.is_empty() {
                let mut rem = 0;
                for i in (0..words.len()).rev() {
                    let x = Self::joinwords(words[i], rem);

                    words[i] = (x / radix_pow as u64) as u32;

                    rem = (x % radix_pow as u64) as u32;
                }

                while words.last().copied() == Some(0) {
                    words.pop();
                }
                prepend(rem, if words.len() > 0 { digits } else { 0 });
            }
        }

        if self.is_negative() {
            res.insert(0, '-');
        }

        res
    }

    pub fn i32(&self) -> Option<i32> {
        if self.digits().len() > 1 {
            return None;
        }

        let value = self.digits()[0];

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

    pub fn u32(&self) -> Option<u32> {
        if self.digits().len() > 1 || self.negative {
            return None;
        }

        Some(self.digits()[0])
    }

    pub fn i64(&self) -> Option<i64> {
        if self.digits().len() > 2 {
            return None;
        }

        let mut value = self.digits()[0usize] as u64;

        if self.digits().len() == 2 {
            value += self.digits()[1] as u64 * Self::BASE;
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

    pub fn u64(&self) -> Option<u64> {
        if self.digits().len() > 2 || self.negative {
            return None;
        }

        let mut value = self.digits()[0usize] as u64;

        if self.digits().len() == 2 {
            value += self.digits()[1] as u64 * Self::BASE;
        }

        Some(value)
    }

    pub fn f64(&self) -> f64 {
        let mut res = 0.0;

        let words = self.digits();

        for i in (0..words.len()).rev() {
            res = res * Self::BASE as f64 + words[i] as f64;
        }

        if self.negative {
            res = -res;
        }

        res
    }

    pub fn abs(this: Value, thread: &mut Thread) -> Value {
        if !this.get_bignum().negative {
            return this;
        }

        this.get_bignum().negate(thread)
    }

    pub fn compare(&self, other: &Self) -> Ordering {
        if self.negative != other.negative {
            if self.negative {
                return Ordering::Less;
            } else {
                return Ordering::Greater;
            }
        }

        if self.negative {
            other.compare_digits(self)
        } else {
            self.compare_digits(other)
        }
    }

    pub fn compare_digits(&self, rhs: &Self) -> Ordering {
        if self.digits().len() != rhs.digits().len() {
            if self.digits().len() < rhs.digits().len() {
                return Ordering::Less;
            } else {
                return Ordering::Greater;
            }
        }

        for i in 1..=self.digits().len() {
            let a = self.digits()[self.digits().len() - i];
            let b = rhs.digits()[self.digits().len() - i];
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

    fn plus_generic(thread: &mut Thread, negative: bool, b1: &Self, b2: &Self) -> Value {
        let mut res = Vec::with_capacity(b1.digits().len());

        let mut sum = 0u64;

        for i in 0..b2.digits().len() {
            let i = i as u32;
            sum += b1.digits()[i as usize] as u64;
            sum += b2.digits()[i as usize] as u64;
            res.push(Self::loword(sum));
            sum = Self::hiword(sum) as u64;
        }

        for i in b2.digits().len()..b1.digits().len() {
            let i = i as u32;
            sum += b1.digits()[i as usize] as u64;
            res.push(Self::loword(sum));
            sum = Self::hiword(sum) as u64;
        }

        if sum > 0 {
            res.push(Self::loword(sum));
        }

        thread.make_bignum_from_digits(&res, negative)
    }

    pub fn plus(mut this: Value, other: Value, thread: &mut Thread) -> Value {
        if other.get_bignum().is_zero() {
            return this;
        }

        if this.get_bignum().negative != other.get_bignum().negative {
            let rhs = gc_protect!(thread => this => other.get_bignum().negate(thread));
            return Self::minus(this, rhs, thread);
        }

        let (b1, b2) = if this.get_bignum().ndigits() < other.get_bignum().ndigits() {
            (other.get_bignum(), this.get_bignum())
        } else {
            (this.get_bignum(), other.get_bignum())
        };

        Self::plus_generic(thread, this.get_bignum().negative, b1, b2)
    }

    pub fn minus(mut this: Value, other: Value, thread: &mut Thread) -> Value {
        if other.get_bignum().is_zero() {
            return this;
        }

        if this.get_bignum().negative != other.get_bignum().negative {
            let rhs = gc_protect!(thread => this => other.get_bignum().negate(thread));
            return Self::plus(this, rhs, thread);
        }

        let cmp = this.get_bignum().compare_digits(other.get_bignum());
        if cmp == Ordering::Equal {
            return thread.make_bignum_from_digits(&[0], false);
        }

        let negative = match cmp {
            Ordering::Less => !this.get_bignum().negative,
            Ordering::Greater => this.get_bignum().negative,
            _ => unreachable!(),
        };

        let (b1, b2) = if cmp == Ordering::Less {
            (other.get_bignum(), this.get_bignum())
        } else {
            (this.get_bignum(), other.get_bignum())
        };

        let mut res = vec![];
        let mut carry = 0;

        for i in 0..b2.ndigits() as usize {
            if (b1.digits()[i] as u64) < (b2.digits()[i] as u64) + carry {
                res.push(
                    (Self::BASE + b1.digits()[i] as u64 - b2.digits()[i] as u64 - carry) as u32,
                );
                carry = 1;
            } else {
                res.push((b1.digits()[i] as u64 - (b2.digits()[i] as u64) - carry) as u32);
                carry = 0;
            }
        }

        for i in b2.ndigits() as usize..b1.ndigits() as usize {
            if (b1.digits()[i] as u64) < carry {
                res.push((Self::BASE + b1.digits()[i] as u64 - carry) as u32);
                carry = 1;
            } else {
                res.push((b1.digits()[i] as u64 - carry) as u32);
                carry = 0;
            }
        }

        debug_assert!(carry == 0);

        thread.make_bignum_from_digits(&res, negative)
    }

    pub fn times(this: Value, other: Value, thread: &mut Thread) -> Value {
        let (b1, b2) = if this.get_bignum().ndigits() < other.get_bignum().ndigits() {
            (other.get_bignum(), this.get_bignum())
        } else {
            (this.get_bignum(), other.get_bignum())
        };

        let mut res = vec![0; b1.ndigits() as usize + b2.ndigits() as usize];

        unsafe {
            for i in 0..b2.ndigits() as usize {
                let mut sum = 0;

                for j in 0..b1.ndigits() as usize {
                    let mult =
                        *b1.digits().get_unchecked(j) as u64 * *b2.digits().get_unchecked(i) as u64;
                    sum += *res.get_unchecked(i + j) as u64 + mult;
                    *res.get_unchecked_mut(i + j) = Self::loword(sum);
                    sum = Self::hiword(sum) as u64;
                }

                *res.get_unchecked_mut(i + b1.ndigits() as usize) = Self::loword(sum);
            }
        }

        thread.make_bignum_from_digits(
            &res,
            this.get_bignum().negative != other.get_bignum().negative,
        )
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

    pub fn divided(mut this: Value, other: Value, thread: &mut Thread) -> (Value, Value) {
        if other.get_bignum().digits().len() > this.get_bignum().digits().len() {
            let zero = gc_protect!(thread => this => thread.make_bignum_from_u64(0));
            return (zero, this);
        }

        let neg = this.get_bignum().negative != other.get_bignum().negative;

        if other.get_bignum().digits().len() == this.get_bignum().digits().len() {
            match this.get_bignum().compare_digits(&other.get_bignum()) {
                Ordering::Equal => {
                    let mut first = thread.make_bignum_from_i64(if neg { -1 } else { 1 });
                    let second = gc_protect!(thread => first => thread.make_bignum_from_u64(0));
                    return (first, second);
                }

                Ordering::Less => {
                    let zero = gc_protect!(thread => this => thread.make_bignum_from_u64(0));
                    return (zero, this);
                }
                _ => (),
            }
        }

        let mut rem = this.get_bignum().digits().to_vec();
        rem.push(0);
        let mut divis = other.get_bignum().digits().to_vec();
        divis.push(0);

        let mut sizediff = rem.len() as isize - divis.len() as isize;
        let div = other.get_bignum().digits()[other.get_bignum().ndigits() as usize - 1] as u64 + 1;
        let mut res = vec![0u32; sizediff as usize + 1];
        let mut divident = rem.len() as isize - 2;

        loop {
            let mut x = Self::joinwords(rem[divident as usize], rem[divident as usize + 1]);
            let mut approx = x / div;
            while approx > 0 {
                res[sizediff as usize] += approx as u32;
                Self::mult_sub(approx as _, &divis, &mut rem, sizediff as usize);
                x = Self::joinwords(rem[divident as usize], rem[divident as usize + 1]);
                approx = x / div;
            }

            if Self::sub_if_possible(&divis, &mut rem, sizediff as usize) {
                res[sizediff as usize] += 1;
            }

            divident -= 1;
            sizediff -= 1;
            if sizediff <= 0 {
                break;
            }
        }
        let neg = this.get_bignum().negative;
        let mut quotient = thread.make_bignum_from_digits(&res, neg);
        let remainder =
            gc_protect!(thread => quotient => thread.make_bignum_from_digits(&rem, neg));

        (quotient, remainder)
    }

    pub fn divide_digit(this: Value, other: u32, thread: &mut Thread) -> (Value, Value) {
        if other == 0 {
            unreachable!("divide_digit: division by zero")
        }

        let mut rem = this.get_bignum().digits().to_vec();
        let mut res = vec![0u32; rem.len()];

        let mut sizediff = rem.len() as isize - 1;
        let mut divident = rem.len() - 1;

        loop {
            let mut x = Self::joinwords(rem[divident], rem[divident - 1]);
            let mut approx = x / other as u64;
            while approx > 0 {
                res[sizediff as usize] += approx as u32;
                Self::mult_sub(approx as _, &[other], &mut rem, sizediff as usize);
                x = Self::joinwords(rem[divident], rem[divident - 1]);
                approx = x / other as u64;
            }

            if Self::sub_if_possible(&[other], &mut rem, sizediff as usize) {
                res[sizediff as usize] += 1;
            }

            divident -= 1;
            sizediff -= 1;
            if sizediff <= 0 {
                break;
            }
        }

        let neg = this.get_bignum().negative;
        let mut quotient = thread.make_bignum_from_digits(&res, neg);
        let remainder =
            gc_protect!(thread => quotient => thread.make_bignum_from_digits(&rem, neg));

        (quotient, remainder)
    }

    pub fn is_most_significant_bit_set(words: &[u32]) -> bool {
        (words.last().copied().unwrap() & (1 << (32 - 1))) != 0
    }

    pub fn from_two_complement(thread: &mut Thread, words: &mut [u32]) -> Value {
        if Self::is_most_significant_bit_set(words) {
            let mut carry = true;

            for i in 0..words.len() {
                if carry {
                    (words[i], carry) = (!words[i]).overflowing_add(1);
                } else {
                    words[i] = !words[i];
                }
            }

            thread.make_bignum_from_digits(words, true)
        } else {
            thread.make_bignum_from_digits(words, false)
        }
    }

    pub fn two_complement_size(left: &[u32], right: &[u32]) -> usize {
        (left.len() + Self::is_most_significant_bit_set(left) as usize)
            .max(right.len() + Self::is_most_significant_bit_set(right) as usize)
    }

    pub fn and32(this: Value, thread: &mut Thread, rhs: u32, negative: bool) -> Value {
        let (mut leftcarry, mut rightcarry) = (true, true);

        let size = Self::two_complement_size(this.get_bignum().digits(), &[rhs]);
        let mut res = Vec::with_capacity(size);

        for i in 0..size {
            let mut leftword = if i < this.get_bignum().digits().len() {
                this.get_bignum().digits()[i]
            } else {
                0
            };

            if this.get_bignum().negative {
                if leftcarry {
                    (leftword, leftcarry) = (!leftword).overflowing_add(1);
                } else {
                    leftword = !leftword;
                }
            }

            let mut rightword = if i < 1 { rhs } else { 0 };

            if negative {
                if rightcarry {
                    (rightword, rightcarry) = (!rightword).overflowing_add(1);
                } else {
                    rightword = !rightword;
                }
            }

            res.push(leftword & rightword);
        }

        Self::from_two_complement(thread, &mut res)
    }

    pub fn or32(this: Value, thread: &mut Thread, rhs: u32, negative: bool) -> Value {
        let (mut leftcarry, mut rightcarry) = (true, true);

        let size = Self::two_complement_size(this.get_bignum().digits(), &[rhs]);
        let mut res = Vec::with_capacity(size);

        for i in 0..size {
            let mut leftword = if i < this.get_bignum().digits().len() {
                this.get_bignum().digits()[i]
            } else {
                0
            };

            if this.get_bignum().negative {
                if leftcarry {
                    (leftword, leftcarry) = (!leftword).overflowing_add(1);
                } else {
                    leftword = !leftword;
                }
            }

            let mut rightword = if i < 1 { rhs } else { 0 };

            if negative {
                if rightcarry {
                    (rightword, rightcarry) = (!rightword).overflowing_add(1);
                } else {
                    rightword = !rightword;
                }
            }

            res.push(leftword | rightword);
        }

        Self::from_two_complement(thread, &mut res)
    }

    pub fn xor32(this: Value, thread: &mut Thread, rhs: u32, negative: bool) -> Value {
        let (mut leftcarry, mut rightcarry) = (true, true);

        let size = Self::two_complement_size(this.get_bignum().digits(), &[rhs]);
        let mut res = Vec::with_capacity(size);

        for i in 0..size {
            let mut leftword = if i < this.get_bignum().digits().len() {
                this.get_bignum().digits()[i]
            } else {
                0
            };

            if this.get_bignum().negative {
                if leftcarry {
                    (leftword, leftcarry) = (!leftword).overflowing_add(1);
                } else {
                    leftword = !leftword;
                }
            }

            let mut rightword = if i < 1 { rhs } else { 0 };

            if negative {
                if rightcarry {
                    (rightword, rightcarry) = (!rightword).overflowing_add(1);
                } else {
                    rightword = !rightword;
                }
            }

            res.push(leftword ^ rightword);
        }

        Self::from_two_complement(thread, &mut res)
    }

    pub fn not(this: Value, thread: &mut Thread) -> Value {
        let mut negated = this.get_bignum().negate(thread);
        let one = gc_protect!(thread => negated => thread.make_bignum_from_u64(1));
        Self::minus(negated, one, thread)
    }

    pub fn and(this: Value, other: Value, thread: &mut Thread) -> Value {
        let (mut leftcarry, mut rightcarry) = (true, true);
        let size =
            Self::two_complement_size(this.get_bignum().digits(), other.get_bignum().digits());
        let mut res = Vec::with_capacity(size);

        for i in 0..size {
            let mut leftword = if i < this.get_bignum().digits().len() {
                this.get_bignum().digits()[i]
            } else {
                0
            };

            if this.get_bignum().negative {
                if leftcarry {
                    (leftword, leftcarry) = (!leftword).overflowing_add(1);
                } else {
                    leftword = !leftword;
                }
            }

            let mut rightword = if i < other.get_bignum().digits().len() {
                other.get_bignum().digits()[i]
            } else {
                0
            };

            if other.get_bignum().negative {
                if rightcarry {
                    (rightword, rightcarry) = (!rightword).overflowing_add(1);
                } else {
                    rightword = !rightword;
                }
            }

            res.push(leftword & rightword);
        }

        Self::from_two_complement(thread, &mut res)
    }

    pub fn or(this: Value, other: Value, thread: &mut Thread) -> Value {
        let (mut leftcarry, mut rightcarry) = (true, true);
        let size =
            Self::two_complement_size(this.get_bignum().digits(), other.get_bignum().digits());
        let mut res = Vec::with_capacity(size);

        for i in 0..size {
            let mut leftword = if i < this.get_bignum().digits().len() {
                this.get_bignum().digits()[i]
            } else {
                0
            };

            if this.get_bignum().negative {
                if leftcarry {
                    (leftword, leftcarry) = (!leftword).overflowing_add(1);
                } else {
                    leftword = !leftword;
                }
            }

            let mut rightword = if i < other.get_bignum().digits().len() {
                other.get_bignum().digits()[i]
            } else {
                0
            };

            if other.get_bignum().negative {
                if rightcarry {
                    (rightword, rightcarry) = (!rightword).overflowing_add(1);
                } else {
                    rightword = !rightword;
                }
            }

            res.push(leftword | rightword);
        }

        Self::from_two_complement(thread, &mut res)
    }

    pub fn xor(this: Value, other: Value, thread: &mut Thread) -> Value {
        let (mut leftcarry, mut rightcarry) = (true, true);
        let size =
            Self::two_complement_size(this.get_bignum().digits(), other.get_bignum().digits());
        let mut res = Vec::with_capacity(size);

        for i in 0..size {
            let mut leftword = if i < this.get_bignum().digits().len() {
                this.get_bignum().digits()[i]
            } else {
                0
            };

            if this.get_bignum().negative {
                if leftcarry {
                    (leftword, leftcarry) = (!leftword).overflowing_add(1);
                } else {
                    leftword = !leftword;
                }
            }

            let mut rightword = if i < other.get_bignum().digits().len() {
                other.get_bignum().digits()[i]
            } else {
                0
            };

            if other.get_bignum().negative {
                if rightcarry {
                    (rightword, rightcarry) = (!rightword).overflowing_add(1);
                } else {
                    rightword = !rightword;
                }
            }

            res.push(leftword ^ rightword);
        }

        Self::from_two_complement(thread, &mut res)
    }

    pub fn bit_size(&self) -> u32 {
        self.count * 32
    }

    pub fn bit_count(this: Value, thread: &mut Thread) -> u32 {
        if this.get_bignum().is_negative() {
            let not = Self::not(this, thread);
            !Self::bit_count(not, thread)
        } else {
            let mut count = 0;
            for word in this.get_bignum().digits() {
                count += word.count_ones();
            }

            count
        }
    }

    pub fn zero_count(this: Value, thread: &mut Thread) -> u32 {
        if this.get_bignum().is_negative() {
            let not = Self::not(this, thread);
            !Self::zero_count(not, thread)
        } else {
            let mut count = 0;
            for word in this.get_bignum().digits() {
                count += word.count_zeros();
            }

            count
        }
    }

    pub fn first_bit_set(&self) -> Option<usize> {
        if self.is_zero() {
            return None;
        }

        let mut i = 0;
        let mut carry = true;

        for word in self.digits().iter() {
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

    pub fn last_bit_set(this: Value, thread: &mut Thread) -> Option<usize> {
        if this.get_bignum().is_zero() {
            return None;
        }

        let number = if this.get_bignum().negative {
            Self::not(this, thread)
        } else {
            this
        };

        let mut i = number.get_bignum().ndigits() as usize - 1;
        while i > 0 && number.get_bignum().digits()[i] == 0 {
            i -= 1;
        }

        return Some(i * 32 + number.get_bignum().digits()[i].leading_zeros() as usize);
    }

    pub fn is_bit_set(&self, bit: usize) -> bool {
        if self.is_zero() {
            return false;
        }

        let nword = bit / 32;
        let nbit = bit % 32;

        if nword >= self.digits().len() {
            return self.negative;
        }

        let mut i = 0;
        let mut word = 0;
        let mut carry = true;

        while i <= nword {
            word = self.digits()[i];

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

    pub fn set_bit(&self, thread: &mut Thread, n: usize, value: bool) -> Value {
        let mut words = self.digits().to_vec();
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

            return thread.make_bignum_from_digits(&words, false);
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

        Self::from_two_complement(thread, &mut words)
    }

    pub fn is_odd(this: Value, thread: &mut Thread) -> bool {
        Self::and32(this, thread, 1, false).get_bignum().is_one()
    }

    pub fn remainder_digit(&self, denominator: i32) -> i32 {
        let count = self.ndigits() as usize;

        let mut remainder = 0i64;

        for i in (0..=(count - 1)).rev() {
            remainder = ((remainder << 32) + self.digits()[i] as i64) % denominator as i64;
        }

        remainder as i32
    }

    pub fn shift_left(&self, thread: &mut Thread, x: i32) -> Value {
        let swords = x.wrapping_div(32);
        let sbits = x.wrapping_rem(32);

        let mut res = Vec::with_capacity(self.digits().len() + swords as usize);

        for _ in 0..swords {
            res.push(0);
        }

        let mut carry = 0u32;

        for word in self.digits().iter().copied() {
            res.push((word << sbits) | carry);
            carry = word >> (32 - sbits);
        }

        if carry != 0 {
            res.push(carry);
        }

        thread.make_bignum_from_digits(&res, self.negative)
    }

    pub fn shift_right(&self, thread: &mut Thread, x: i32) -> Value {
        let swords = x.wrapping_div(32);
        let sbits = x % 32;

        let mut res = Vec::with_capacity(self.digits().len() + swords as usize);
        let mut carry = 0;
        let mut i = self.digits().len() as isize - 1;
        while i >= swords as isize {
            let word = self.digits()[i as usize];
            res.push((word >> sbits as u32) | carry);
            carry = word << (32 - sbits as u32);
            i -= 1;
        }
        res.reverse();
        let x = thread.make_bignum_from_digits(&res, self.is_negative());
        if self.negative && carry > 0 {
            return Self::minus(x, thread.make_bignum_from_u64(1), thread);
        } else {
            x
        }
    }

    pub fn shift(this: Value, thread: &mut Thread, n: i32) -> Value {
        if n < 0 {
            this.get_bignum().shift_right(thread, -n)
        } else if n > 0 {
            this.get_bignum().shift_left(thread, n)
        } else {
            this
        }
    }

    pub fn sqrt(mut this: Value, thread: &mut Thread) -> Value {
        let b1 = this.get_bignum();
        if b1.is_zero() || b1.is_one() {
            return this;
        }

        let mut two = gc_protect!(thread => this => thread.make_bignum_from_u64(2));
        let mut y = gc_protect!(thread => this => Self::divided(this, two, thread).0);
        let mut x = gc_protect!(thread => this, y => Self::divided(this, y, thread).0);

        while y.get_bignum().compare(&x.get_bignum()) == Ordering::Greater {
            let tmp = gc_protect!(thread => this, two, x => Self::plus(x, y, thread));
            y = gc_protect!(thread => this, x => Self::divided(tmp, two, thread).0);
            x = gc_protect!(thread => this, y => Self::divided(this, y, thread).0);
        }

        y
    }

    pub fn to_power(this: Value, thread: &mut Thread, exp: Value) -> Value {
        let (mut expo, mut radix) = (exp, this);
        let mut res = gc_protect!(thread => expo, radix => thread.make_bignum_from_u64(1));
        let mut two = gc_protect!(thread => expo, radix, res => thread.make_bignum_from_u64(2));

        while !expo.get_bignum().is_zero() {
            let r =
                gc_protect!(thread => expo, radix, res, two => Self::and32(expo, thread, 1, false));
            if r.get_bignum().is_zero() {
                res =
                    gc_protect!(thread => expo, radix, res, two => Self::times(res, radix, thread));
            }

            expo = gc_protect!(thread => two, radix, res => Self::shift_right(expo.get_bignum(), thread, 1));
            radix = gc_protect!(thread => two, expo, res => Self::times(radix, radix, thread));
        }

        res
    }
}

impl Value {
    pub fn is_bignum(&self) -> bool {
        self.type_of() == TypeId::Bignum
    }

    pub fn get_bignum(&self) -> &mut ScmBigInteger {
        debug_assert!(self.is_bignum());
        self.cast_as::<ScmBigInteger>()
    }
}

pub const INT64_MAX: u64 = i64::MAX as u64;

pub enum Integer {
    UInt32(u32),
    Int32(i32),
    UInt64(u64),
    Int64(i64),
    Int128(i128),
}

impl Into<Integer> for i32 {
    fn into(self) -> Integer {
        Integer::Int32(self)
    }
}

impl Into<Integer> for u32 {
    fn into(self) -> Integer {
        if self > i32::MAX as u32 {
            Integer::UInt32(self)
        } else {
            Integer::Int32(self as i32)
        }
    }
}

impl Into<Integer> for i64 {
    fn into(self) -> Integer {
        if self as i32 as i64 == self {
            Integer::Int32(self as i32)
        } else {
            Integer::Int64(self)
        }
    }
}

impl Into<Integer> for u64 {
    fn into(self) -> Integer {
        Integer::UInt64(self)
    }
}

impl Into<Integer> for i128 {
    fn into(self) -> Integer {
        if self as i32 as i128 == self {
            Integer::Int32(self as i32)
        } else if self as i64 as i128 == self {
            Integer::Int64(self as i64)
        } else {
            Integer::Int128(self)
        }
    }
}

impl Thread {
    pub fn make_rational<const IMMORTAL: bool>(
        &mut self,
        mut numerator: Value,
        mut denominator: Value,
    ) -> Value {
        let size = round_up(size_of::<ScmRational>(), 8, 0);

        unsafe {
            let semantics = if IMMORTAL {
                AllocationSemantics::Immortal
            } else {
                AllocationSemantics::Default
            };

            let ptr = gc_protect!(self => numerator, denominator => self.mutator().alloc(size, 8, 0, semantics));

            ptr.store(ScmRational {
                header: ScmCellHeader::new(TypeId::Rational),
                numerator,
                denominator,
            });

            let objref = ObjectReference::from_address::<CapyVM>(ptr);
            self.mutator().post_alloc(objref, size, semantics);

            transmute(objref)
        }
    }

    pub fn make_complex<const IMMORTAL: bool>(
        &mut self,
        mut real: Value,
        mut imag: Value,
    ) -> Value {
        let size = round_up(size_of::<ScmComplex>(), 8, 0);

        unsafe {
            let semantics = if IMMORTAL {
                AllocationSemantics::Immortal
            } else {
                AllocationSemantics::Default
            };

            let ptr =
                gc_protect!(self => real, imag => self.mutator().alloc(size, 8, 0, semantics));

            ptr.store(ScmComplex {
                header: ScmCellHeader::new(TypeId::Complex),
                real,
                imag,
            });

            let objref = ObjectReference::from_address::<CapyVM>(ptr);
            self.mutator().post_alloc(objref, size, semantics);

            transmute(objref)
        }
    }

    pub fn make_integer<T>(&mut self, integer: T) -> Value
    where
        T: Into<Integer>,
    {
        match integer.into() {
            Integer::UInt32(x) => self.make_bignum_from_u64(x as u64),
            Integer::Int32(x) => Value::encode_int32(x),
            Integer::UInt64(x) => self.make_bignum_from_u64(x),
            Integer::Int64(x) => self.make_bignum_from_i64(x),
            Integer::Int128(x) => self.make_bignum_from_i64(x as i64),
        }
    }

    pub fn make_bignum(&mut self, ndigits: u32, negative: bool) -> Value {
        let size = round_up(
            size_of::<ScmBigInteger>() + (ndigits as usize - 1) * size_of::<u32>(),
            8,
            0,
        );

        unsafe {
            let semantics = if size > self.los_threshold {
                AllocationSemantics::Los
            } else {
                AllocationSemantics::Default
            };

            let ptr = self.mutator().alloc(size, 8, 0, semantics);

            ptr.store(ScmBigInteger {
                header: ScmCellHeader::new(TypeId::Bignum),
                negative,
                count: ndigits,
                uwords: [0],
            });

            let bn = ptr.to_mut_ptr::<ScmBigInteger>().as_mut().unwrap();
            bn.digits_mut()[0..ndigits as usize].fill(0);

            let objref = ObjectReference::from_address::<CapyVM>(ptr);
            self.mutator().post_alloc(objref, size, semantics);

            transmute(objref)
        }
    }

    pub fn make_bignum_from_digits(&mut self, mut digits: &[u32], sign: bool) -> Value {
        while digits.len() > 1 && digits[digits.len() - 1] == 0 {
            digits = &digits[..digits.len() - 1];
        }

        let bn = self.make_bignum(digits.len() as u32, sign);
        bn.get_bignum().digits_mut()[0..digits.len()].copy_from_slice(digits);
        bn
    }

    pub fn make_bignum_from_usize(&mut self, digits: &[usize], sign: bool) -> Value {
        let mut udigits = Vec::new();
        udigits.reserve(digits.len() * 2);

        for d in digits {
            udigits.push(ScmBigInteger::loword(*d as u64));
            udigits.push(ScmBigInteger::hiword(*d as u64));
        }

        self.make_bignum_from_digits(&udigits, sign)
    }

    pub fn make_bignum_from_i64(&mut self, value: i64) -> Value {
        let absvalue = if value == i64::MIN {
            INT64_MAX + 1
        } else {
            if value < 0 {
                (-value) as u64
            } else {
                value as u64
            }
        };

        self.make_bignum_from_digits(
            &[
                ScmBigInteger::loword(absvalue),
                ScmBigInteger::hiword(absvalue),
            ],
            value < 0,
        )
    }

    pub fn make_bignum_from_u64(&mut self, value: u64) -> Value {
        self.make_bignum_from_digits(
            &[ScmBigInteger::loword(value), ScmBigInteger::hiword(value)],
            false,
        )
    }

    pub fn make_bignum_from_f64(&mut self, value: f64) -> Value {
        if value > -1.0 && value < 1.0 {
            self.make_bignum_from_digits(
                &[ScmBigInteger::loword(0), ScmBigInteger::hiword(0)],
                value < 0.0,
            )
        } else if value > -(u64::MAX as f64) && value < u64::MAX as f64 {
            let absvalue = if value < 0.0 {
                (-value) as u64
            } else {
                value as u64
            };

            self.make_bignum_from_digits(
                &[
                    ScmBigInteger::loword(absvalue),
                    ScmBigInteger::hiword(absvalue),
                ],
                value < 0.0,
            )
        } else {
            todo!()
        }
    }

    pub fn make_bignum_from_str(&mut self, str: &str, base: &BigIntBase) -> Option<Value> {
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
                return Some(self.make_bignum_from_u64(0));
            }
        }

        let mut temp = vec![];

        while i < bytes.len() {
            let mut found = false;
            for (c, v) in base.digit_map.iter() {
                if bytes[i] == *c as u8 {
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

        Some(self.make_bignum_from_digits_u8(&temp, negative, base))
    }

    pub fn make_bignum_from_digits_u8(
        &mut self,
        digits: &[u8],
        sign: bool,
        base: &BigIntBase,
    ) -> Value {
        let mut words = vec![];
        let mut iterate;
        let mut digits = digits.to_vec();

        loop {
            let mut res = vec![];
            let mut sum = 0;

            let mut j = 0;

            while j < digits.len() && sum < ScmBigInteger::BASE {
                sum = sum * base.radix() as u64 + digits[j] as u64;
                j += 1;
            }

            res.push(ScmBigInteger::hiword(sum) as u8);
            iterate = ScmBigInteger::hiword(sum) > 0;

            sum = ScmBigInteger::loword(sum) as _;

            while j < digits.len() {
                sum = sum * base.radix() as u64 + digits[j] as u64;
                j += 1;
                res.push(ScmBigInteger::hiword(sum) as _);
                iterate = true;
                sum = ScmBigInteger::loword(sum) as u64;
            }

            words.push(ScmBigInteger::loword(sum) as _);
            digits = res;
            if !iterate {
                break;
            }
        }

        self.make_bignum_from_digits(&words, sign)
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

pub fn exact_integer_to_int32(obj: Value) -> Option<i32> {
    if obj.is_int32() {
        return Some(obj.get_int32());
    }

    if obj.is_bignum() {
        return obj.get_bignum().i32();
    }

    None
}

pub fn exact_integer_to_uint32(obj: Value) -> Option<u32> {
    if obj.is_int32() && obj.get_int32() >= 0 {
        return Some(obj.get_int32() as u32);
    }

    if obj.is_bignum() {
        return obj.get_bignum().u32();
    }

    None
}

pub fn exact_integer_to_int64(obj: Value) -> Option<i64> {
    if obj.is_int32() {
        return Some(obj.get_int32() as i64);
    }

    if obj.is_bignum() {
        return obj.get_bignum().i64();
    }

    None
}

pub fn exact_integer_to_uint64(obj: Value) -> Option<u64> {
    if obj.is_int32() && obj.get_int32() >= 0 {
        return Some(obj.get_int32() as u64);
    }

    if obj.is_bignum() {
        return obj.get_bignum().u64();
    }

    None
}

pub fn exact_integer_to_int16(obj: Value) -> Option<i16> {
    if obj.is_int32() {
        let val = obj.get_int32();
        if val >= i16::MIN as i32 && val <= i16::MAX as i32 {
            return Some(val as i16);
        }
    }

    None
}

pub fn exact_integer_to_uint16(obj: Value) -> Option<u16> {
    if obj.is_int32() {
        let val = obj.get_int32();
        if val >= 0 && val <= u16::MAX as i32 {
            return Some(val as u16);
        }
    }

    None
}

pub fn exact_integer_to_int8(obj: Value) -> Option<i8> {
    if obj.is_int32() {
        let val = obj.get_int32();
        if val >= i8::MIN as i32 && val <= i8::MAX as i32 {
            return Some(val as i8);
        }
    }

    None
}

pub fn exact_integer_to_uint8(obj: Value) -> Option<u8> {
    if obj.is_int32() {
        let val = obj.get_int32();
        if val >= 0 && val <= u8::MAX as i32 {
            return Some(val as u8);
        }
    }

    None
}

pub fn real_to_double(obj: Value) -> Value {
    if obj.is_int32() {
        return Value::encode_f64_value(obj.get_int32() as f64);
    } else if obj.is_double() {
        return obj;
    } else if obj.is_bignum() {
        return Value::encode_f64_value(obj.get_bignum().f64());
    } else if obj.is_rational() {
        return rational_to_double(obj);
    } else if obj.is_complex() {
        let cn = obj.get_complex();
        if n_zero_p(cn.imag) {
            return real_to_double(cn.real);
        }
    }
    unreachable!("real_to_double: {}", obj)
}

pub fn rational_to_double(obj: Value) -> Value {
    let rat = obj.get_rational();

    let nume = if rat.numerator.is_int32() {
        rat.numerator.get_int32() as f64
    } else if rat.numerator.is_bignum() {
        rat.numerator.get_bignum().f64()
    } else {
        unreachable!()
    };

    let deno = if rat.denominator.is_int32() {
        rat.denominator.get_int32() as f64
    } else if rat.denominator.is_bignum() {
        rat.denominator.get_bignum().f64()
    } else {
        unreachable!()
    };

    Value::encode_f64_value(nume / deno)
}

pub fn decode_double(n: f64) -> (i64, i32, i32) {
    let bits = n.to_bits();

    let mant_bits = bits & (IEXPT_2N52 - 1) as u64;
    let sign_bits = bits >> 63;
    let exp_bits = (bits >> 52) & 0x7ff;

    if n == 0.0 {
        return (0, 0, if sign_bits != 0 { -1 } else { 1 });
    }

    if n.is_nan() {
        return (0x18000000000000, 972, 1);
    }

    if n.is_infinite() {
        return (0x10000000000000, 972, if sign_bits != 0 { -1 } else { 1 });
    }

    let exp = if exp_bits != 0 {
        exp_bits as i64 - 1023
    } else {
        -1022
    } - 52;

    let sign = if sign_bits != 0 { -1 } else { 1 };

    let mant_bits = if exp_bits != 0 {
        mant_bits as i64 | IEXPT_2N52
    } else {
        mant_bits as i64
    };

    (mant_bits, exp as i32, sign)
}

#[allow(dead_code)]
fn pow10n(mut value: f64, mut n: i32) -> f64 {
    const BIGTENS: [f64; 5] = [1.0e+16, 1.0e+32, 1.0e+64, 1.0e+128, 1.0e+256];
    const TENS: [f64; 23] = [
        1.0, 1.0e+1, 1.0e+2, 1.0e+3, 1.0e+4, 1.0e+5, 1.0e+6, 1.0e+7, 1.0e+8, 1.0e+9, 1.0e+10,
        1.0e+11, 1.0e+12, 1.0e+13, 1.0e+14, 1.0e+15, 1.0e+16, 1.0e+17, 1.0e+18, 1.0e+19, 1.0e+20,
        1.0e+21, 1.0e+22,
    ];

    let mut inflate = true;

    if n < 0 {
        inflate = false;
        n = -n;
    }

    if n <= 22 {
        return if inflate {
            value * TENS[n as usize]
        } else {
            value / TENS[n as usize]
        };
    }

    if n > 511 {
        n = 511;
    }

    if (n & 0x0f) != 0 {
        if inflate {
            value *= TENS[(n & 0x0f) as usize];
        } else {
            value /= TENS[(n & 0x0f) as usize];
        }
    }

    n >>= 4;

    for i in 0..=4 {
        if (n & 1) != 0 {
            if inflate {
                value *= BIGTENS[i];
            } else {
                value /= BIGTENS[i];
            }
        }

        n >>= 1;
    }
    if value == 0.0 {
        return 1.0 * (-1074f64).powi(2);
    }
    value
}

fn oprtr_inexact_negate(_thread: &mut Thread, obj: Value) -> Value {
    if obj.is_int32() {
        return Value::encode_f64_value(-(obj.get_int32() as f64));
    }

    if obj.is_double() {
        return Value::encode_f64_value(-obj.get_double());
    }

    if obj.is_bignum() {
        return Value::encode_f64_value(-obj.get_bignum().f64());
    }

    if obj.is_rational() {
        let dbl = rational_to_double(obj);
        return Value::encode_f64_value(-dbl.get_double());
    }

    if obj.is_complex() {
        todo!()
    }

    unreachable!("oprtr_inexact_negate")
}

fn oprtr_norm_integer(thread: &mut Thread, obj: Value) -> Value {
    debug_assert!(obj.is_int32() || obj.is_bignum());

    if obj.is_bignum() {
        match obj.get_bignum().i32() {
            Some(normalized) => return Value::encode_int32(normalized),
            None => {
                thread.make_bignum_from_digits(obj.get_bignum().digits(), obj.get_bignum().negative)
            }
        }
    } else {
        obj
    }
}

fn oprtr_norm_complex(thread: &mut Thread, real: Value, imag: Value) -> Value {
    if imag.is_int32() && imag.get_int32() == 0 {
        return real;
    }

    if imag.is_bignum() && imag.get_bignum().is_zero() {
        return real;
    }

    if real.is_double() || imag.is_double() {
        todo!()
    }

    thread.make_complex::<false>(real, imag)
}

fn oprtr_reduce_fixnum_fixnum(thread: &mut Thread, numerator: Value, denominator: Value) -> Value {
    let mut nume = numerator.get_int32() as i64;
    let mut deno = denominator.get_int32() as i64;

    if deno == 1 {
        return numerator;
    }

    if deno == -1 {
        return arith_negate(thread, numerator);
    }

    if nume == 0 {
        return numerator;
    }

    if nume == 1 {
        if deno < 0 {
            let deno = thread.make_integer(-deno);
            return thread.make_rational::<false>(Value::encode_int32(-1), deno);
        }

        return thread.make_rational::<false>(numerator, denominator);
    }

    if nume == -1 {
        if deno < 0 {
            let deno = thread.make_integer(-deno);
            return thread.make_rational::<false>(Value::encode_int32(1), deno);
        }

        return thread.make_rational::<false>(numerator, denominator);
    }

    let mut ans_sign = 1;
    if nume < 0 {
        ans_sign = -ans_sign;
        nume = -nume;
    }

    if deno < 0 {
        ans_sign = -ans_sign;
        deno = -deno;
    }

    let mut n1 = nume;
    let mut n2 = deno;

    while n2 != 0 {
        let t = n2;
        n2 = n1 % n2;
        n1 = t;
    }

    let gcd = n1;

    if deno == gcd {
        return thread.make_integer(nume * ans_sign / gcd);
    }

    let mut deno = thread.make_integer(deno / gcd);
    let nume = gc_protect!(thread => deno => thread.make_integer(nume * ans_sign / gcd));

    thread.make_rational::<false>(nume, deno)
}

fn oprtr_reduce_fixnum_bignum(thread: &mut Thread, numerator: Value, denominator: Value) -> Value {
    if numerator.get_int32() == 0 {
        return numerator;
    }

    if numerator.get_int32() == 1 {
        if denominator.get_bignum().is_negative() {
            let deno = ScmBigInteger::negate(&denominator.get_bignum(), thread);
            return thread.make_rational::<false>(Value::encode_int32(-1), deno);
        }

        return thread.make_rational::<false>(numerator, denominator);
    }

    if numerator.get_int32() == -1 {
        if denominator.get_bignum().is_negative() {
            let deno = ScmBigInteger::negate(&denominator.get_bignum(), thread);
            return thread.make_rational::<false>(Value::encode_int32(1), deno);
        }

        return thread.make_rational::<false>(numerator, denominator);
    }

    let mut ans_sign = 1;
    let mut nume = numerator.get_int32() as i32;

    if nume < 0 {
        ans_sign = -ans_sign;
        nume = -nume;
    }

    if denominator.get_bignum().is_negative() {
        ans_sign = -ans_sign;
    }

    let mut n1 = ScmBigInteger::remainder_digit(&denominator.get_bignum(), nume);
    let mut n2 = nume;

    while n2 != 0 {
        let t = n2;
        n2 = n1 % n2;
        n1 = t;
    }

    let gcd = n1;
    nume /= gcd;
    if ans_sign < 0 {
        nume = -nume;
    }

    let (mut quo, _) = ScmBigInteger::divide_digit(denominator, gcd as _, thread);
    let ans_nume = gc_protect!(thread => quo => thread.make_integer(nume));
    quo.get_bignum().negative = ans_sign < 0;
    let ans_deno = quo
        .get_bignum()
        .i32()
        .map(Value::encode_int32)
        .unwrap_or(quo);

    if ans_deno == Value::encode_int32(1) {
        return ans_nume;
    }

    thread.make_rational::<false>(ans_nume, ans_deno)
}

fn oprtr_reduce_bignum_fixnum(thread: &mut Thread, numerator: Value, denominator: Value) -> Value {
    if denominator.get_int32() == 1 {
        return numerator;
    }

    if denominator.get_int32() == -1 {
        return arith_negate(thread, numerator);
    }

    let mut ans_sign = 1;
    let mut deno = denominator.get_int32() as i32;
    if numerator.get_bignum().is_negative() {
        ans_sign = -ans_sign;
    }

    if deno < 0 {
        ans_sign = -ans_sign;
        deno = -deno;
    }

    let mut n1 = ScmBigInteger::remainder_digit(&numerator.get_bignum(), deno);
    let mut n2 = deno;

    while n2 != 0 {
        let t = n2;
        n2 = n1 % n2;
        n1 = t;
    }

    let gcd = n1;
    deno /= gcd;

    let (mut quo, _) = ScmBigInteger::divide_digit(numerator, gcd as _, thread);
    quo.get_bignum().negative = ans_sign < 0;
    if deno == 1 {
        return quo
            .get_bignum()
            .i32()
            .map(Value::encode_int32)
            .unwrap_or(quo);
    }

    let ans_deno = gc_protect!(thread => quo => thread.make_integer(deno));
    let ans_nume = quo
        .get_bignum()
        .i32()
        .map(Value::encode_int32)
        .unwrap_or(quo);

    thread.make_rational::<false>(ans_nume, ans_deno)
}

fn oprtr_reduce(thread: &mut Thread, mut numerator: Value, mut denominator: Value) -> Value {
    debug_assert!(numerator.is_int32() || numerator.is_bignum());
    debug_assert!(denominator.is_int32() || denominator.is_bignum());

    if numerator.is_int32() {
        if denominator.is_int32() {
            return oprtr_reduce_fixnum_fixnum(thread, numerator, denominator);
        }

        return oprtr_reduce_fixnum_bignum(thread, numerator, denominator);
    }

    if denominator.is_int32() {
        return oprtr_reduce_bignum_fixnum(thread, numerator, denominator);
    }

    let n1_count;
    let n2_count;
    let mut ans_sign;
    let mut n1 = if numerator.is_bignum() {
        ans_sign = numerator.get_bignum().is_negative();
        n1_count = numerator.get_bignum().ndigits();
        numerator
    } else {
        ans_sign = numerator.get_int32() < 0;
        n1_count = 1;
        gc_protect!(thread => denominator, numerator => thread.make_bignum_from_i64(numerator.get_int32() as i64))
    };

    let mut n2 = if denominator.is_bignum() {
        if denominator.get_bignum().is_negative() {
            ans_sign = !ans_sign;
        }
        n2_count = denominator.get_bignum().ndigits();
        denominator
    } else {
        if denominator.get_int32() < 0 {
            ans_sign = !ans_sign;
        }
        n2_count = 1;
        gc_protect!(thread => n1, denominator, numerator => thread.make_bignum_from_i64(denominator.get_int32() as i64))
    };

    let mut divisor = gc_protect!(thread => n1, n2, denominator, numerator => thread.make_bignum_from_u64(if n1_count > n2_count {
        n1_count as _
    } else { n2_count as _ }));

    let mut shift = 0;

    while ((n1.get_bignum().digits()[0] | n2.get_bignum().digits()[0]) & 1) == 0 {
        n1 = gc_protect!(thread => n2, divisor, denominator, numerator => n1.get_bignum().shift_right(thread, 1));
        n2 = gc_protect!(thread => n1, divisor, denominator, numerator => n2.get_bignum().shift_right(thread, 1));
        shift += 1;
    }

    while !n1.get_bignum().is_zero() {
        // if (n1 is even)
        if (n1.get_bignum().digits()[0] & 1) == 0 {
            n1 = gc_protect!(thread => n2, divisor, denominator, numerator => n1.get_bignum().shift_right(thread, 1));
        // if (n2 is even)
        } else if (n2.get_bignum().digits()[0] & 1) == 0 {
            n2 = gc_protect!(thread => n1, divisor, denominator, numerator => n2.get_bignum().shift_right(thread, 1));
        } else {
            if n1.get_bignum().compare(&n2.get_bignum()) == Ordering::Less {
                // n2 = (n2 - n1) >> 1
                n2 = gc_protect!(thread => n1, divisor, denominator, numerator => ScmBigInteger::minus(n2, n1, thread));
                n2 = gc_protect!(thread => n1, divisor, denominator, numerator => n2.get_bignum().shift_right(thread, 1));
            } else {
                // n1 = (n1 - n2) >> 1
                n1 = gc_protect!(thread => n2, divisor, denominator, numerator => ScmBigInteger::minus(n1, n2, thread));
                n1 = gc_protect!(thread => n2, divisor, denominator, numerator => n1.get_bignum().shift_right(thread, 1));
            }
        }
    }
    if n2.get_bignum().is_one() && shift == 0 {
        if n_negative_p(denominator) {
            let mut negated_numerator =
                gc_protect!(thread => denominator => arith_negate(thread, numerator));
            let negated_denominator =
                gc_protect!(thread => negated_numerator => arith_negate(thread, denominator));

            return thread.make_rational::<false>(negated_numerator, negated_denominator);
        }

        return thread.make_rational::<false>(numerator, denominator);
    }

    divisor = n2;

    if numerator.is_bignum() {
        n1 = gc_protect!(thread => divisor, n2 => thread.make_bignum_from_digits(numerator.get_bignum().digits(), false));
    } else {
        let x = numerator.get_int32();
        n1 = gc_protect!(thread => divisor, n2 => thread.make_bignum_from_i64(if x < 0 { -x } else { x } as i64));
    }

    if denominator.is_bignum() {
        n2 = gc_protect!(thread => divisor, n1 => thread.make_bignum_from_digits(denominator.get_bignum().digits(), false));
    } else {
        let x = denominator.get_int32();
        n2 = gc_protect!(thread => divisor, n1 => thread.make_bignum_from_i64(if x < 0 { -x } else { x } as i64));
    }

    n1 = gc_protect!(thread => divisor, n2 => n1.get_bignum().shift_right(thread, shift));
    n1 = gc_protect!(thread => divisor, n2 => ScmBigInteger::divided(n1, divisor, thread).0);
    n1.get_bignum().negative = ans_sign;

    n2 = gc_protect!(thread => divisor, n1 => n2.get_bignum().shift_right(thread, shift));
    n2 = gc_protect!(thread => divisor, n1 => ScmBigInteger::divided(n2, divisor, thread).0);
    n2.get_bignum().negative = false;

    let ans_numerator = oprtr_norm_integer(thread, n1);
    let ans_denominator = oprtr_norm_integer(thread, n2);
    if ans_denominator == Value::encode_int32(1) {
        return ans_numerator;
    }

    return thread.make_rational::<false>(ans_numerator, ans_denominator);
}

fn oprtr_expt(thread: &mut Thread, mut lhs: Value, mut rhs: Value) -> Value {
    let mut n = rhs.get_int32();
    if n == 0 {
        return Value::encode_int32(1);
    }

    if n == 1 {
        return lhs;
    }

    if n < 0 {
        let expt = oprtr_expt(thread, lhs, Value::encode_int32(-n));
        return arith_inverse(thread, expt);
    }

    if !lhs.is_complex() && n_negative_p(lhs) {
        let negated = arith_negate(thread, lhs);
        let expt = oprtr_expt(thread, negated, rhs);
        if n & 1 != 0 {
            return arith_negate(thread, expt);
        }
        return expt;
    }

    if lhs == Value::encode_int32(0) {
        return lhs;
    }

    if lhs == Value::encode_int32(1) {
        return lhs;
    }

    if lhs.is_rational() {
        let mut expt_nume_rhs = gc_protect!(thread => lhs, rhs => oprtr_expt(thread, lhs.get_rational().numerator, rhs));
        let expt_deno_rhs = gc_protect!(thread => expt_nume_rhs => oprtr_expt(thread, lhs.get_rational().denominator, rhs));

        return oprtr_reduce(thread, expt_nume_rhs, expt_deno_rhs);
    }

    let mut ans = Value::encode_int32(1);
    loop {
        if n & 1 != 0 {
            if ans == Value::encode_int32(1) {
                ans = lhs;
            } else {
                ans = gc_protect!(thread => lhs => arith_mul(thread, ans, lhs));
            }

            if n == 1 {
                return ans;
            }
        }

        lhs = gc_protect!(thread => lhs, ans => arith_mul(thread, lhs, lhs));
        n >>= 1;
    }
}

pub fn number_p(obj: Value) -> bool {
    if obj.is_inline_number() {
        return true;
    }

    matches!(
        obj.type_of(),
        TypeId::Bignum | TypeId::Rational | TypeId::Complex
    )
}

pub fn integer_p(obj: Value) -> bool {
    if obj.is_int32() || obj.is_bignum() {
        return true;
    }

    if obj.is_rational() {
        return false;
    }

    if obj.is_double() {
        let flonum = obj.get_double();

        if flonum.is_infinite() || flonum.is_nan() {
            return false;
        }

        return flonum.fract() == 0.0; // flonum.trunc() == flonum
    }

    false
}

pub fn integer_value_p(obj: Value) -> bool {
    if integer_p(obj) {
        return true;
    }

    if obj.is_complex() {
        return n_zero_p(obj.get_complex().imag) && integer_value_p(obj.get_complex().real);
    }

    false
}

pub fn n_zero_p(obj: Value) -> bool {
    if obj.is_int32() {
        return obj.get_int32() == 0;
    }

    if obj.is_double() {
        return obj.get_double() == 0.0;
    }

    if obj.is_bignum() {
        return obj.get_bignum().is_zero();
    }

    if obj.is_rational() {
        return false;
    }

    if obj.is_complex() {
        return n_zero_p(obj.get_complex().real) && n_zero_p(obj.get_complex().imag);
    }

    unreachable!("n_zero_p")
}

pub fn n_negative_p(obj: Value) -> bool {
    if obj.is_int32() {
        return obj.get_int32() < 0;
    }

    if obj.is_double() {
        return obj.get_double() < 0.0;
    }

    if obj.is_bignum() {
        return obj.get_bignum().is_negative();
    }

    if obj.is_rational() {
        return n_negative_p(obj.get_rational().numerator);
    }

    if obj.is_complex() {
        return n_negative_p(obj.get_complex().real);
    }

    unreachable!("n_negative_p")
}

pub fn n_positive_p(obj: Value) -> bool {
    if obj.is_int32() {
        return obj.get_int32() > 0;
    }

    if obj.is_double() {
        return obj.get_double() > 0.0;
    }

    if obj.is_bignum() {
        return !obj.get_bignum().is_negative();
    }

    if obj.is_rational() {
        return n_positive_p(obj.get_rational().numerator);
    }

    if obj.is_complex() {
        return n_positive_p(obj.get_complex().real);
    }

    unreachable!("n_positive_p")
}

pub fn n_even_p(obj: Value) -> bool {
    if obj.is_int32() {
        return obj.get_int32() % 2 == 0;
    }

    if obj.is_double() {
        let flonum = obj.get_double();
        return (flonum * 0.5) == (flonum * 0.5).floor();
    }

    if obj.is_bignum() {
        return obj.get_bignum().digits()[0] & 1 == 0;
    }

    if obj.is_complex() {
        return n_even_p(obj.get_complex().real);
    }

    unreachable!("n_even_p")
}

pub fn n_exact_p(obj: Value) -> bool {
    if obj.is_int32() || obj.is_bignum() || obj.is_rational() {
        return true;
    }

    if obj.is_double() {
        return false;
    }

    if obj.is_complex() {
        return n_exact_p(obj.get_complex().real) && n_exact_p(obj.get_complex().imag);
    }

    unreachable!("n_exact_p")
}

pub fn exact_positive_integer_p(obj: Value) -> bool {
    if obj.is_int32() {
        return obj.get_int32() > 0;
    }

    if obj.is_bignum() {
        return !obj.get_bignum().is_negative() && !obj.get_bignum().is_zero();
    }

    false
}

pub fn exact_nonnegative_integer_p(obj: Value) -> bool {
    if obj.is_int32() {
        return obj.get_int32() >= 0;
    }

    if obj.is_bignum() {
        return !obj.get_bignum().is_negative();
    }

    false
}

pub fn exact_integer_p(obj: Value) -> bool {
    if obj.is_int32() || obj.is_bignum() {
        return true;
    }

    false
}

pub fn real_p(obj: Value) -> bool {
    obj.is_int32() || obj.is_bignum() || obj.is_rational() || obj.is_double()
}

pub fn real_value_p(obj: Value) -> bool {
    if real_p(obj) {
        return true;
    }

    if obj.is_complex() {
        return n_zero_p(obj.get_complex().imag);
    }

    true
}

pub fn rational_p(obj: Value) -> bool {
    if obj.is_int32() || obj.is_bignum() || obj.is_rational() {
        return true;
    }

    if obj.is_double() {
        let d = obj.get_double();

        if d.is_infinite() || d.is_nan() {
            return false;
        }
    }

    true
}

pub fn rational_value_p(obj: Value) -> bool {
    if rational_p(obj) {
        return true;
    }

    if obj.is_complex() {
        return n_zero_p(obj.get_complex().imag) && rational_p(obj.get_complex().real);
    }

    true
}

pub fn arith_magnitude(thread: &mut Thread, mut obj: Value) -> Value {
    if obj.is_complex() {
        if n_exact_p(obj) {
            if n_zero_p(obj.get_complex().real) {
                return arith_magnitude(thread, obj.get_complex().imag);
            }

            if n_zero_p(obj.get_complex().imag) {
                return arith_magnitude(thread, obj.get_complex().real);
            }

            let mut mul_real_real = gc_protect!(thread => obj => arith_mul(thread, obj.get_complex().real, obj.get_complex().real));
            let mul_imag_imag = gc_protect!(thread => mul_real_real => arith_mul(thread, obj.get_complex().imag, obj.get_complex().imag));
            let add = arith_add(thread, mul_real_real, mul_imag_imag);
            return arith_sqrt(thread, add);
        } else {
            let real = rational_to_double(obj.get_complex().real).get_double();
            let imag = rational_to_double(obj.get_complex().imag).get_double();
            if real.is_infinite() || imag.is_infinite() {
                return Value::encode_f64_value(f64::INFINITY);
            }

            let m = (real * real + imag * imag).sqrt();
            if m < f64::EPSILON || m.is_infinite() {
                return Value::encode_f64_value(imag / imag.atan2(real).sin());
            }

            return Value::encode_f64_value(m);
        }
    }

    if real_value_p(obj) {
        if n_negative_p(obj) {
            return arith_negate(thread, obj);
        }

        return obj;
    }

    unreachable!("wrong type")
}

pub fn n_equal_p(_thread: &mut Thread, mut lhs: Value, mut rhs: Value) -> bool {
    loop {
        if lhs.is_int32() {
            'fixnum_again: loop {
                if rhs.is_int32() {
                    return lhs.get_int32() == rhs.get_int32();
                }

                if rhs.is_double() {
                    return lhs.get_int32() as f64 == rhs.get_double();
                }

                if rhs.is_bignum() {
                    return false;
                }

                if rhs.is_rational() {
                    return false;
                }

                if rhs.is_complex() {
                    if n_zero_p(rhs.get_complex().imag) {
                        rhs = rhs.get_complex().real;
                        continue 'fixnum_again;
                    }

                    return false;
                }
                break;
            }
        }

        if lhs.is_double() {
            'flonum_again: loop {
                if rhs.is_int32() {
                    return lhs.get_double() == rhs.get_int32() as f64;
                }

                if rhs.is_double() {
                    return lhs.get_double() == rhs.get_double();
                }

                if rhs.is_bignum() {
                    return false;
                }

                if rhs.is_rational() {
                    return false;
                }

                if rhs.is_complex() {
                    if n_zero_p(rhs.get_complex().imag) {
                        rhs = rhs.get_complex().real;
                        continue 'flonum_again;
                    }

                    return false;
                }
                break;
            }
        }

        if lhs.is_bignum() {
            'bignum_again: loop {
                if rhs.is_int32() {
                    return false;
                }

                if rhs.is_double() {
                    if rhs.get_double() == lhs.get_bignum().f64() {
                        let exact = cnvt_to_exact(_thread, lhs);
                        return n_compare(_thread, exact, rhs) == Ordering::Equal;
                    }

                    return false;
                }

                if rhs.is_bignum() {
                    return lhs.get_bignum().compare(rhs.get_bignum()) == Ordering::Equal;
                }

                if rhs.is_rational() {
                    return true;
                }

                if rhs.is_complex() {
                    if n_zero_p(rhs.get_complex().imag) {
                        rhs = rhs.get_complex().real;
                        continue 'bignum_again;
                    }

                    return false;
                }
                break;
            }
        }

        if lhs.is_rational() {
            'rational_again: loop {
                if rhs.is_int32() {
                    return false;
                }

                if rhs.is_double() {
                    return rational_to_double(lhs).get_double() == rhs.get_double();
                }

                if rhs.is_bignum() {
                    return false;
                }

                if rhs.is_rational() {
                    if n_equal_p(
                        _thread,
                        lhs.get_rational().numerator,
                        rhs.get_rational().numerator,
                    ) {
                        return n_equal_p(
                            _thread,
                            lhs.get_rational().denominator,
                            rhs.get_rational().denominator,
                        );
                    }

                    return false;
                }

                if rhs.is_complex() {
                    if n_zero_p(rhs.get_complex().imag) {
                        rhs = rhs.get_complex().real;
                        continue 'rational_again;
                    }

                    return false;
                }

                break;
            }
        }

        if lhs.is_complex() {
            if n_zero_p(lhs.get_complex().imag) {
                lhs = lhs.get_complex().real;
                continue;
            }

            if rhs.is_int32() {
                return false;
            }

            if rhs.is_double() {
                return false;
            }

            if rhs.is_bignum() || rhs.is_rational() {
                return false;
            }

            if rhs.is_complex() {
                if n_equal_p(_thread, lhs.get_complex().real, rhs.get_complex().real) {
                    return n_equal_p(_thread, lhs.get_complex().imag, rhs.get_complex().imag);
                }

                return false;
            }
        }

        break;
    }

    unreachable!()
}

pub fn n_finite_p(obj: Value) -> bool {
    if obj.is_double() {
        return !obj.get_double().is_infinite() && !obj.get_double().is_nan();
    }
    true
}

pub fn n_compare(thread: &mut Thread, mut lhs: Value, mut rhs: Value) -> Ordering {
    'start_again: loop {
        if lhs.is_int32() {
            'fixnum_again: loop {
                if rhs.is_int32() {
                    let n = lhs.get_int32().wrapping_sub(rhs.get_int32());

                    if n == 0 {
                        return Ordering::Equal;
                    }

                    return if n < 0 {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    };
                }

                if rhs.is_double() {
                    let d = lhs.get_int32() as f64 - rhs.get_double();

                    if d == 0.0 {
                        return Ordering::Equal;
                    }

                    return if d < 0.0 {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    };
                }

                if rhs.is_bignum() {
                    let negative = rhs.get_bignum().is_negative();

                    return if negative {
                        Ordering::Greater
                    } else {
                        Ordering::Less
                    };
                }

                if rhs.is_rational() {
                    let mut nume = rhs.get_rational().numerator;
                    let deno = rhs.get_rational().denominator;

                    let deno = gc_protect!(thread => nume => arith_mul(thread, lhs, deno));

                    return n_compare(thread, deno, nume);
                }

                if rhs.is_complex() {
                    if n_zero_p(rhs.get_complex().imag) {
                        rhs = rhs.get_complex().real;
                        continue 'fixnum_again;
                    }
                }

                unreachable!()
            }
        }

        if rhs.is_double() {
            'flonum_again: loop {
                if rhs.is_int32() {
                    let d = lhs.get_double() - rhs.get_int32() as f64;

                    if d == 0.0 {
                        return Ordering::Equal;
                    }

                    return if d < 0.0 {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    };
                }

                if rhs.is_double() {
                    let d = lhs.get_double() - rhs.get_double();

                    if d == 0.0 {
                        return Ordering::Equal;
                    }

                    return if d < 0.0 {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    };
                }

                if rhs.is_bignum() {
                    let negative = rhs.get_bignum().is_negative();

                    return if negative {
                        Ordering::Greater
                    } else {
                        Ordering::Less
                    };
                }

                if rhs.is_rational() {
                    let mut nume = rhs.get_rational().numerator;
                    let deno = rhs.get_rational().denominator;

                    let deno = gc_protect!(thread => nume => arith_mul(thread, lhs, deno));

                    return n_compare(thread, deno, nume);
                }

                if rhs.is_complex() {
                    if n_zero_p(rhs.get_complex().imag) {
                        rhs = rhs.get_complex().real;
                        continue 'flonum_again;
                    }
                }

                unreachable!()
            }
        }

        if rhs.is_bignum() {
            'bignum_again: loop {
                if lhs.is_int32() {
                    let negative = lhs.get_int32() < 0;

                    return if negative {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    };
                }

                if lhs.is_double() {
                    let negative = lhs.get_double() < 0.0;

                    return if negative {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    };
                }

                if lhs.is_bignum() {
                    return ScmBigInteger::compare(&lhs.get_bignum(), &rhs.get_bignum());
                }

                if lhs.is_rational() {
                    let mut nume = lhs.get_rational().numerator;
                    let deno = lhs.get_rational().denominator;

                    let deno = gc_protect!(thread => nume => arith_mul(thread, deno, rhs));

                    return n_compare(thread, nume, deno);
                }

                if lhs.is_complex() {
                    if n_zero_p(lhs.get_complex().imag) {
                        lhs = lhs.get_complex().real;
                        continue 'bignum_again;
                    }
                }

                unreachable!()
            }
        }

        if rhs.is_rational() {
            let mut nume = rhs.get_rational().numerator;
            let mut deno = rhs.get_rational().denominator;

            'rational_again: loop {
                if rhs.is_int32() {
                    let mul = gc_protect!(thread => nume => arith_mul(thread, deno, rhs));

                    return n_compare(thread, nume, mul);
                }

                if rhs.is_double() {
                    let d = rational_to_double(lhs).get_double() - rhs.get_double();
                    if d == 0.0 {
                        return Ordering::Equal;
                    }

                    return if d < 0.0 {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    };
                }

                if rhs.is_bignum() {
                    let mul = gc_protect!(thread => nume => arith_mul(thread, deno, rhs));

                    return n_compare(thread, nume, mul);
                }

                if rhs.is_rational() {
                    let mut nume2 = rhs.get_rational().numerator;
                    let deno2 = rhs.get_rational().denominator;
                    let mut mul1 =
                        gc_protect!(thread => nume2, deno => arith_mul(thread, nume, deno2));
                    let mul2 = gc_protect!(thread => mul1 => arith_mul(thread, nume2, deno));

                    return n_compare(thread, mul1, mul2);
                }

                if rhs.is_complex() {
                    if n_zero_p(rhs.get_complex().imag) {
                        rhs = rhs.get_complex().real;
                        continue 'rational_again;
                    }
                }

                unreachable!()
            }
        }

        if rhs.is_complex() {
            if n_zero_p(rhs.get_complex().imag) {
                rhs = rhs.get_complex().real;
                continue 'start_again;
            }
        }

        break;
    }

    unreachable!()
}

pub fn arith_inverse(thread: &mut Thread, obj: Value) -> Value {
    if obj.is_int32() {
        if obj.get_int32() > 0 {
            if obj.get_int32() == 1 {
                return obj;
            }

            return thread.make_rational::<false>(Value::encode_int32(1), obj);
        }

        if obj.get_int32() == -1 {
            return obj;
        }

        let negated = arith_negate(thread, obj);

        return thread.make_rational::<false>(Value::encode_int32(-1), negated);
    }

    if obj.is_double() {
        return Value::encode_f64_value(1.0 / obj.get_double());
    }

    if obj.is_bignum() {
        if obj.get_bignum().negative {
            return thread.make_rational::<false>(Value::encode_int32(-1), obj);
        } else {
            return thread.make_rational::<false>(Value::encode_int32(1), obj);
        }
    }

    if obj.is_rational() {
        let rn = obj.get_rational();
        if !n_negative_p(rn.numerator) {
            if rn.numerator == Value::encode_int32(1) {
                return oprtr_norm_integer(thread, rn.denominator);
            }

            return thread.make_rational::<false>(rn.denominator, rn.numerator);
        }

        if rn.numerator == Value::encode_int32(-1) {
            return oprtr_norm_integer(thread, rn.denominator);
        }

        let n1 = arith_negate(thread, rn.numerator);
        let n2 = arith_negate(thread, rn.denominator);

        return thread.make_rational::<false>(n2, n1);
    }

    if obj.is_complex() {
        return arith_div(thread, Value::encode_int32(1), obj);
    }

    unreachable!()
}

pub fn arith_negate(thread: &mut Thread, obj: Value) -> Value {
    if obj.is_int32() {
        let n = obj.get_int32();

        if n == i32::MIN {
            return thread.make_bignum_from_i64(-(n as i64));
        }

        return Value::encode_int32(-n);
    }

    if obj.is_double() {
        return Value::encode_f64_value(-obj.get_double());
    }

    if obj.is_bignum() {
        return ScmBigInteger::negate(&obj.get_bignum(), thread);
    }

    if obj.is_rational() {
        let mut deno = obj.get_rational().denominator;
        let nume =
            gc_protect!(thread => deno => arith_negate(thread, obj.get_rational().numerator));

        return thread.make_rational::<false>(nume, deno);
    }

    if obj.is_complex() {
        let mut real = obj.get_complex().real;
        let mut imag = gc_protect!(thread => real => arith_negate(thread, obj.get_complex().imag));
        real = gc_protect!(thread => imag => arith_negate(thread, real));

        return thread.make_complex::<false>(real, imag);
    }

    unreachable!("arith_negate")
}

pub fn arith_bit_count(_thread: &mut Thread, obj: Value) -> Value {
    if obj.is_int32() {
        let n = obj.get_int32();

        if n == 0 {
            return Value::encode_int32(0);
        }

        if n > 0 {
            return Value::encode_int32(n.count_ones() as i32);
        } else {
            return Value::encode_int32((-n).count_ones() as i32);
        }
    }

    if obj.is_bignum() {
        return Value::encode_int32(obj.get_bignum().bit_size() as _);
    }

    unreachable!()
}

pub fn arith_first_bit_set(obj: Value) -> Value {
    if obj.is_int32() {
        let n = obj.get_int32();

        if n == 0 {
            return Value::encode_int32(-1);
        }

        return Value::encode_int32(n.trailing_zeros() as i32);
    }

    if obj.is_bignum() {
        return Value::encode_int32(obj.get_bignum().first_bit_set().unwrap_or(0) as _);
    }

    unreachable!()
}

pub fn arith_bit_length(obj: Value) -> Value {
    if obj.is_int32() {
        let n = obj.get_int32();

        if n == 0 {
            return Value::encode_int32(0);
        }

        let n2 = if n < 0 { -n } else { n };
        return Value::encode_int32(32 - n2.leading_zeros() as i32);
    }

    if obj.is_bignum() {
        return Value::encode_int32(obj.get_bignum().bit_size() as _);
    }

    unreachable!()
}

pub fn arith_lognot(thread: &mut Thread, obj: Value) -> Value {
    if obj.is_int32() {
        let n = obj.get_int32() as i64;
        let n = !n;
        if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
            return Value::encode_int32(n as i32);
        }

        return thread.make_integer(n);
    }

    if obj.is_bignum() {
        let bn = ScmBigInteger::not(obj, thread);
        bn.get_bignum().i32().map(Value::encode_int32).unwrap_or(bn)
    } else {
        unreachable!()
    }
}

pub fn arith_logand(thread: &mut Thread, lhs: Value, rhs: Value) -> Value {
    if lhs.is_int32() {
        if rhs.is_int32() {
            return Value::encode_int32(lhs.get_int32() & rhs.get_int32());
        }

        if rhs.is_bignum() {
            let bn = ScmBigInteger::and32(rhs, thread, lhs.get_int32() as _, lhs.get_int32() < 0);

            return bn.get_bignum().i32().map(Value::encode_int32).unwrap_or(bn);
        }
    }

    if lhs.is_bignum() {
        if rhs.is_int32() {
            let bn = ScmBigInteger::and32(
                lhs,
                thread,
                rhs.get_int32() as _,
                lhs.get_bignum().is_negative(),
            );

            return bn.get_bignum().i32().map(Value::encode_int32).unwrap_or(bn);
        }

        if rhs.is_bignum() {
            let bn = ScmBigInteger::and(lhs, rhs, thread);

            return bn.get_bignum().i32().map(Value::encode_int32).unwrap_or(bn);
        }
    }

    unreachable!()
}

pub fn arith_logior(thread: &mut Thread, lhs: Value, rhs: Value) -> Value {
    if lhs.is_int32() {
        if rhs.is_int32() {
            return Value::encode_int32(lhs.get_int32() | rhs.get_int32());
        }

        if rhs.is_bignum() {
            let bn = ScmBigInteger::or32(rhs, thread, lhs.get_int32() as _, lhs.get_int32() < 0);

            return bn.get_bignum().i32().map(Value::encode_int32).unwrap_or(bn);
        }
    }

    if lhs.is_bignum() {
        if rhs.is_int32() {
            let bn = ScmBigInteger::or32(
                lhs,
                thread,
                rhs.get_int32() as _,
                lhs.get_bignum().is_negative(),
            );

            return bn.get_bignum().i32().map(Value::encode_int32).unwrap_or(bn);
        }

        if rhs.is_bignum() {
            let bn = ScmBigInteger::or(lhs, rhs, thread);

            return bn.get_bignum().i32().map(Value::encode_int32).unwrap_or(bn);
        }
    }

    unreachable!()
}

pub fn arith_logxor(thread: &mut Thread, lhs: Value, rhs: Value) -> Value {
    if lhs.is_int32() {
        if rhs.is_int32() {
            return Value::encode_int32(lhs.get_int32() ^ rhs.get_int32());
        }

        if rhs.is_bignum() {
            let bn = ScmBigInteger::xor32(rhs, thread, lhs.get_int32() as _, lhs.get_int32() < 0);

            return bn.get_bignum().i32().map(Value::encode_int32).unwrap_or(bn);
        }
    }

    if lhs.is_bignum() {
        if rhs.is_int32() {
            let bn = ScmBigInteger::xor32(
                lhs,
                thread,
                rhs.get_int32() as _,
                lhs.get_bignum().is_negative(),
            );

            return bn.get_bignum().i32().map(Value::encode_int32).unwrap_or(bn);
        }

        if rhs.is_bignum() {
            let bn = ScmBigInteger::xor(lhs, rhs, thread);

            return bn.get_bignum().i32().map(Value::encode_int32).unwrap_or(bn);
        }
    }

    unreachable!()
}

pub fn arith_logash(thread: &mut Thread, lhs: Value, rhs: Value) -> Value {
    let shift = rhs.get_int32();

    if lhs.is_int32() {
        if shift <= 32 {
            let mut n = lhs.get_int32() as i64;

            if shift > 0 {
                n <<= shift;
            } else {
                n >>= -shift;
            }

            if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
                return Value::encode_int32(n as i32);
            }
        }

        let bn = thread.make_bignum_from_i64(lhs.get_int32() as _);
        return ScmBigInteger::shift(bn, thread, shift);
    }

    if lhs.is_bignum() {
        return ScmBigInteger::shift(lhs, thread, shift);
    }

    unreachable!()
}

pub fn arith_add(thread: &mut Thread, mut lhs: Value, mut rhs: Value) -> Value {
    let mut real;
    let mut imag;

    if lhs.is_int32() {
        if rhs.is_int32() {
            if let Some(result) = lhs.get_int32().checked_add(rhs.get_int32()) {
                return Value::encode_int32(result);
            } else {
                return thread.make_integer(lhs.get_int32() as i64 + rhs.get_int32() as i64);
            }
        }

        if rhs.is_double() {
            return Value::encode_f64_value(lhs.get_int32() as f64 + rhs.get_double());
        }

        if rhs.is_bignum() {
            let bn1 =
                gc_protect!(thread => rhs => thread.make_bignum_from_i64(lhs.get_int32() as i64));
            let sum = ScmBigInteger::plus(bn1, rhs, thread);

            return sum
                .get_bignum()
                .i32()
                .map(Value::encode_int32)
                .unwrap_or(sum);
        }

        if rhs.is_rational() {
            let mut mul = gc_protect!(thread => rhs => arith_mul(thread, lhs, rhs.get_rational().denominator));
            let add = gc_protect!(thread => mul, rhs => arith_add(thread, rhs.get_rational().numerator, mul));
            return oprtr_reduce(thread, add, rhs.get_rational().denominator);
        }

        if rhs.is_complex() {
            real = rhs.get_complex().real;
            imag = rhs.get_complex().imag;
            let add = gc_protect!(thread => imag => arith_add(thread, real, lhs));
            return thread.make_complex::<false>(add, imag);
        }
    }

    if lhs.is_double() {
        if rhs.is_int32() {
            return Value::encode_f64_value(lhs.get_double() + rhs.get_int32() as f64);
        }

        if rhs.is_double() {
            return Value::encode_f64_value(lhs.get_double() + rhs.get_double());
        }

        if rhs.is_bignum() {
            return Value::encode_f64_value(lhs.get_double() + rhs.get_bignum().f64());
        }

        if rhs.is_rational() {
            let d = rational_to_double(rhs);
            return Value::encode_f64_value(lhs.get_double() + d.get_double());
        }

        if rhs.is_complex() {
            real = rhs.get_complex().real;
            imag = rhs.get_complex().imag;
            let add = gc_protect!(thread => imag => arith_add(thread, real, lhs));
            return thread.make_complex::<false>(add, imag);
        }
    }

    if lhs.is_bignum() {
        if rhs.is_int32() {
            let bn1 =
                gc_protect!(thread => lhs => thread.make_bignum_from_i64(rhs.get_int32() as i64));
            let sum = ScmBigInteger::plus(lhs, bn1, thread);

            return sum
                .get_bignum()
                .i32()
                .map(Value::encode_int32)
                .unwrap_or(sum);
        }

        if rhs.is_double() {
            return Value::encode_f64_value(lhs.get_bignum().f64() + rhs.get_double());
        }

        if rhs.is_bignum() {
            let sum = ScmBigInteger::plus(lhs, rhs, thread);

            return sum
                .get_bignum()
                .i32()
                .map(Value::encode_int32)
                .unwrap_or(sum);
        }

        if rhs.is_rational() {
            let mut mul = gc_protect!(thread => rhs => arith_mul(thread, lhs, rhs.get_rational().denominator));
            let add = gc_protect!(thread => mul, rhs => arith_add(thread, rhs.get_rational().numerator, mul));
            return oprtr_reduce(thread, add, rhs.get_rational().denominator);
        }

        if rhs.is_complex() {
            real = rhs.get_complex().real;
            imag = rhs.get_complex().imag;
            let add = gc_protect!(thread => imag => arith_add(thread, real, lhs));
            return thread.make_complex::<false>(add, imag);
        }
    }

    if lhs.is_rational() {
        if rhs.is_int32() {
            let mut mul = gc_protect!(thread => lhs => arith_mul(thread, lhs.get_rational().denominator, rhs));
            let add = gc_protect!(thread => mul, lhs => arith_add(thread, lhs.get_rational().numerator, mul));
            return oprtr_reduce(thread, add, lhs.get_rational().denominator);
        }

        if rhs.is_double() {
            let d = rational_to_double(lhs);
            return Value::encode_f64_value(d.get_double() + rhs.get_double());
        }

        if rhs.is_bignum() {
            let mut mul = gc_protect!(thread => lhs => arith_mul(thread, lhs.get_rational().denominator, rhs));
            let add = gc_protect!(thread => mul, lhs => arith_add(thread, lhs.get_rational().numerator, mul));
            return oprtr_reduce(thread, add, lhs.get_rational().denominator);
        }

        if rhs.is_complex() {
            real = rhs.get_complex().real;
            imag = rhs.get_complex().imag;
            let add = gc_protect!(thread => imag => arith_add(thread, real, lhs));
            return thread.make_complex::<false>(add, imag);
        }
    }

    if rhs.is_complex() {
        real = rhs.get_complex().real;
        imag = rhs.get_complex().imag;

        if rhs.is_int32() {
            let add = gc_protect!(thread => imag => arith_add(thread, real, rhs));
            return thread.make_complex::<false>(add, imag);
        }

        if rhs.is_double() {
            let add = gc_protect!(thread => imag => arith_add(thread, real, rhs));
            let imag = cnvt_to_inexact(thread, imag);
            return thread.make_complex::<false>(add, imag);
        }

        if rhs.is_bignum() {
            let add = gc_protect!(thread => imag => arith_add(thread, real, rhs));
            return thread.make_complex::<false>(add, imag);
        }

        if rhs.is_rational() {
            let add = gc_protect!(thread => imag => arith_add(thread, real, rhs));
            return thread.make_complex::<false>(add, imag);
        }

        if rhs.is_complex() {
            real = gc_protect!(thread => imag => arith_add(thread, real, rhs));
            imag = gc_protect!(thread => real => arith_add(thread, imag, rhs.get_complex().imag));
            return oprtr_norm_complex(thread, real, imag);
        }
    }

    unreachable!("wrong datum type")
}

pub fn arith_sub(thread: &mut Thread, mut lhs: Value, mut rhs: Value) -> Value {
    let mut real;
    let mut imag;

    if lhs.is_int32() {
        if rhs.is_int32() {
            if let Some(result) = lhs.get_int32().checked_sub(rhs.get_int32()) {
                return Value::encode_int32(result);
            } else {
                return thread.make_integer(lhs.get_int32() as i64 - rhs.get_int32() as i64);
            }
        }

        if rhs.is_double() {
            return Value::encode_f64_value(lhs.get_int32() as f64 - rhs.get_double());
        }

        if rhs.is_bignum() {
            let bn1 =
                gc_protect!(thread => rhs => thread.make_bignum_from_i64(lhs.get_int32() as i64));
            let sum = ScmBigInteger::minus(bn1, rhs, thread);

            return sum
                .get_bignum()
                .i32()
                .map(Value::encode_int32)
                .unwrap_or(sum);
        }

        if rhs.is_rational() {
            let mut mul = gc_protect!(thread => rhs => arith_mul(thread, lhs, rhs.get_rational().denominator));
            let add = gc_protect!(thread => mul, rhs => arith_sub(thread, rhs.get_rational().numerator, mul));
            return oprtr_reduce(thread, add, rhs.get_rational().denominator);
        }

        if rhs.is_complex() {
            real = rhs.get_complex().real;
            imag = rhs.get_complex().imag;
            let mut imag = gc_protect!(thread => real => oprtr_inexact_negate(thread, imag));
            let add = gc_protect!(thread => imag => arith_sub(thread, lhs, real));
            return thread.make_complex::<false>(add, imag);
        }
    }

    if lhs.is_double() {
        if rhs.is_int32() {
            return Value::encode_f64_value(lhs.get_double() - rhs.get_int32() as f64);
        }

        if rhs.is_double() {
            return Value::encode_f64_value(lhs.get_double() - rhs.get_double());
        }

        if rhs.is_bignum() {
            return Value::encode_f64_value(lhs.get_double() - rhs.get_bignum().f64());
        }

        if rhs.is_rational() {
            let d = rational_to_double(rhs);
            return Value::encode_f64_value(lhs.get_double() - d.get_double());
        }

        if rhs.is_complex() {
            real = rhs.get_complex().real;
            imag = rhs.get_complex().imag;
            let add = gc_protect!(thread => imag => arith_sub(thread, real, lhs));
            return thread.make_complex::<false>(add, imag);
        }
    }

    if lhs.is_bignum() {
        if rhs.is_int32() {
            let bn1 =
                gc_protect!(thread => lhs => thread.make_bignum_from_i64(rhs.get_int32() as i64));
            let sum = ScmBigInteger::minus(lhs, bn1, thread);

            return sum
                .get_bignum()
                .i32()
                .map(Value::encode_int32)
                .unwrap_or(sum);
        }

        if rhs.is_double() {
            return Value::encode_f64_value(lhs.get_bignum().f64() - rhs.get_double());
        }

        if rhs.is_bignum() {
            let sum = ScmBigInteger::minus(lhs, rhs, thread);

            return sum
                .get_bignum()
                .i32()
                .map(Value::encode_int32)
                .unwrap_or(sum);
        }

        if rhs.is_rational() {
            let mut mul = gc_protect!(thread => rhs => arith_mul(thread, lhs, rhs.get_rational().denominator));
            let add = gc_protect!(thread => mul, rhs => arith_sub(thread, rhs.get_rational().numerator, mul));
            return oprtr_reduce(thread, add, rhs.get_rational().denominator);
        }

        if rhs.is_complex() {
            real = rhs.get_complex().real;
            imag = rhs.get_complex().imag;
            let add = gc_protect!(thread => imag => arith_sub(thread, real, lhs));
            return thread.make_complex::<false>(add, imag);
        }
    }

    if lhs.is_rational() {
        if rhs.is_int32() {
            let mut mul = gc_protect!(thread => lhs => arith_mul(thread, lhs.get_rational().denominator, rhs));
            let add = gc_protect!(thread => mul, lhs => arith_sub(thread, lhs.get_rational().numerator, mul));
            return oprtr_reduce(thread, add, lhs.get_rational().denominator);
        }

        if rhs.is_double() {
            let d = rational_to_double(lhs);
            return Value::encode_f64_value(d.get_double() - rhs.get_double());
        }

        if rhs.is_bignum() {
            let mut mul = gc_protect!(thread => lhs => arith_mul(thread, lhs.get_rational().denominator, rhs));
            let add = gc_protect!(thread => mul, lhs => arith_sub(thread, lhs.get_rational().numerator, mul));
            return oprtr_reduce(thread, add, lhs.get_rational().denominator);
        }

        if rhs.is_complex() {
            real = rhs.get_complex().real;
            imag = rhs.get_complex().imag;
            let mut add = gc_protect!(thread => imag => arith_sub(thread, real, lhs));
            let negated = gc_protect!(thread => add => arith_negate(thread, imag));
            return thread.make_complex::<false>(add, negated);
        }
    }

    if rhs.is_complex() {
        real = rhs.get_complex().real;
        imag = rhs.get_complex().imag;

        if rhs.is_int32() {
            let add = gc_protect!(thread => imag => arith_sub(thread, real, rhs));
            return thread.make_complex::<false>(add, imag);
        }

        if rhs.is_double() {
            let add = gc_protect!(thread => imag => arith_sub(thread, real, rhs));
            let imag = cnvt_to_inexact(thread, imag);
            return thread.make_complex::<false>(add, imag);
        }

        if rhs.is_bignum() {
            let add = gc_protect!(thread => imag => arith_sub(thread, real, rhs));
            return thread.make_complex::<false>(add, imag);
        }

        if rhs.is_rational() {
            let add = gc_protect!(thread => imag => arith_sub(thread, real, rhs));
            return thread.make_complex::<false>(add, imag);
        }

        if rhs.is_complex() {
            real = gc_protect!(thread => imag => arith_sub(thread, real, rhs));
            imag = gc_protect!(thread => real => arith_sub(thread, imag, rhs.get_complex().imag));
            return oprtr_norm_complex(thread, real, imag);
        }
    }

    unreachable!("wrong datum type")
}
pub fn arith_gcd(thread: &mut Thread, mut x: Value, mut y: Value) -> Value {
    while !n_zero_p(y) {
        let remainder = gc_protect!(thread => x,y => arith_remainder(thread, x, y));
        (x, y) = (y, remainder);
    }

    let ans = arith_abs(thread, x);

    ans
}

pub fn arith_abs(thread: &mut Thread, obj: Value) -> Value {
    if obj.is_int32() {
        let n = obj.get_int32();

        if n == i32::MIN {
            return thread.make_bignum_from_i64(-(n as i64));
        }

        return Value::encode_int32(n.abs());
    }

    if obj.is_double() {
        return Value::encode_f64_value(obj.get_double().abs());
    }

    if obj.is_bignum() {
        return ScmBigInteger::abs(obj, thread);
    }

    if obj.is_rational() {
        let mut deno = obj.get_rational().denominator;
        let nume = gc_protect!(thread => deno => arith_abs(thread, obj.get_rational().numerator));

        return thread.make_rational::<false>(nume, deno);
    }

    if obj.is_complex() {
        let mut real = obj.get_complex().real;
        let mut imag = gc_protect!(thread => real => arith_abs(thread, obj.get_complex().imag));
        real = gc_protect!(thread => imag => arith_abs(thread, real));

        return thread.make_complex::<false>(real, imag);
    }

    unreachable!("arith_abs")
}

pub fn arith_remainder(thread: &mut Thread, mut lhs: Value, mut rhs: Value) -> Value {
    let mut real;
    let mut imag;
    'start_again: loop {
        if lhs.is_int32() {
            if lhs.get_int32() == 0 {
                return lhs;
            }
            'fixnum_again: loop {
                if rhs.is_int32() {
                    let lhs = lhs.get_int32();
                    let rhs = rhs.get_int32();

                    if rhs == 0 {
                        unreachable!("arith_remainder: divide by zero")
                    }

                    return Value::encode_int32(lhs % rhs);
                }

                if rhs.is_double() {
                    let value = rhs.get_double();

                    if value == value.round() {
                        if value == 0.0 {
                            unreachable!("arith_remainder: divide by zero")
                        }

                        return Value::encode_f64_value(lhs.get_int32() as f64 % value);
                    }

                    assert!(false);
                }

                if rhs.is_bignum() {
                    let bn1 = gc_protect!(thread => rhs => thread.make_bignum_from_i64(lhs.get_int32() as i64));
                    let (_, rem) = ScmBigInteger::divided(bn1, rhs, thread);

                    return rem
                        .get_bignum()
                        .i32()
                        .map(Value::encode_int32)
                        .unwrap_or(rem);
                }

                if rhs.is_rational() {
                    assert!(false);
                }

                if rhs.is_complex() {
                    real = rhs.get_complex().real;
                    imag = rhs.get_complex().imag;
                    if n_zero_p(imag) {
                        rhs = real;
                        continue 'fixnum_again;
                    }

                    assert!(false);
                }

                break 'start_again;
            }
        }

        if lhs.is_double() {
            let value = lhs.get_double();
            if value == value.round() {
                if value == 0.0 {
                    return lhs;
                } else {
                    assert!(false);
                }
            }
            'flonum_again: loop {
                if rhs.is_int32() {
                    return Value::encode_f64_value(value % rhs.get_int32() as f64);
                }

                if rhs.is_double() {
                    let value2 = rhs.get_double();
                    if value2 == value2.round() {
                        if value2 == 0.0 {
                            unreachable!("arith_remainder: divide by zero")
                        }

                        return Value::encode_f64_value(value % value2);
                    }

                    assert!(false);
                }

                if rhs.is_bignum() {
                    return Value::encode_f64_value(value % rhs.get_bignum().f64());
                }

                if rhs.is_rational() {
                    assert!(false);
                }

                if rhs.is_complex() {
                    real = rhs.get_complex().real;
                    imag = rhs.get_complex().imag;
                    if n_zero_p(imag) {
                        rhs = real;
                        continue 'flonum_again;
                    }

                    assert!(false);
                }

                break 'start_again;
            }
        }

        if lhs.is_bignum() {
            'bignum_again: loop {
                if rhs.is_int32() {
                    let rhs = rhs.get_int32();

                    if rhs == 0 {
                        unreachable!("arith_remainder: divide by zero")
                    }

                    let (_quo, rem) = ScmBigInteger::divide_digit(lhs, rhs as _, thread);

                    return rem
                        .get_bignum()
                        .i32()
                        .map(Value::encode_int32)
                        .unwrap_or(rem);
                }

                if rhs.is_double() {
                    return Value::encode_f64_value(lhs.get_bignum().f64() % rhs.get_double());
                }

                if rhs.is_bignum() {
                    let (_quo, rem) = ScmBigInteger::divided(lhs, rhs, thread);

                    return rem
                        .get_bignum()
                        .i32()
                        .map(Value::encode_int32)
                        .unwrap_or(rem);
                }

                if rhs.is_rational() {
                    assert!(false);
                }

                if rhs.is_complex() {
                    real = rhs.get_complex().real;
                    imag = rhs.get_complex().imag;
                    if n_zero_p(imag) {
                        rhs = real;
                        continue 'bignum_again;
                    }

                    assert!(false);
                }

                break 'start_again;
            }
        }

        if lhs.is_rational() {
            assert!(false);
        }

        if lhs.is_complex() {
            real = lhs.get_complex().real;
            imag = lhs.get_complex().imag;
            if n_zero_p(imag) {
                lhs = real;
                continue 'start_again;
            }

            assert!(false);
        }
    }

    unreachable!()
}

pub fn arith_quotient(thread: &mut Thread, mut lhs: Value, mut rhs: Value) -> Value {
    let mut real;
    let mut imag;
    'start_again: loop {
        if lhs.is_int32() {
            if lhs.get_int32() == 0 {
                return lhs;
            }
            'fixnum_again: loop {
                if rhs.is_int32() {
                    let lhs = lhs.get_int32();
                    let rhs = rhs.get_int32();

                    if rhs == 0 {
                        unreachable!("arith_quotient: divide by zero")
                    }

                    return Value::encode_int32(lhs / rhs);
                }

                if rhs.is_double() {
                    let value = rhs.get_double();

                    if value == value.round() {
                        if value == 0.0 {
                            unreachable!("arith_quotient: divide by zero")
                        }

                        return Value::encode_f64_value(lhs.get_int32() as f64 / value);
                    }

                    assert!(false);
                }

                if rhs.is_bignum() {
                    let bn1 = gc_protect!(thread => rhs => thread.make_bignum_from_i64(lhs.get_int32() as i64));
                    let (quo, _) = ScmBigInteger::divided(bn1, rhs, thread);

                    return quo
                        .get_bignum()
                        .i32()
                        .map(Value::encode_int32)
                        .unwrap_or(quo);
                }

                if rhs.is_rational() {
                    assert!(false);
                }

                if rhs.is_complex() {
                    real = rhs.get_complex().real;
                    imag = rhs.get_complex().imag;
                    if n_zero_p(imag) {
                        rhs = real;
                        continue 'fixnum_again;
                    }

                    assert!(false);
                }

                break 'start_again;
            }
        }

        if lhs.is_double() {
            let value = lhs.get_double();
            if value == value.round() {
                if value == 0.0 {
                    return lhs;
                } else {
                    assert!(false);
                }
            }
            'flonum_again: loop {
                if rhs.is_int32() {
                    return Value::encode_f64_value(value / rhs.get_int32() as f64);
                }

                if rhs.is_double() {
                    let value2 = rhs.get_double();
                    if value2 == value2.round() {
                        if value2 == 0.0 {
                            unreachable!("arith_quotient: divide by zero")
                        }

                        return Value::encode_f64_value(value / value2);
                    }
                    assert!(false);
                }

                if rhs.is_bignum() {
                    return Value::encode_f64_value(value / rhs.get_bignum().f64());
                }

                if rhs.is_rational() {
                    assert!(false);
                }

                if rhs.is_complex() {
                    real = rhs.get_complex().real;
                    imag = rhs.get_complex().imag;
                    if n_zero_p(imag) {
                        rhs = real;
                        continue 'flonum_again;
                    }

                    assert!(false);
                }

                break 'start_again;
            }
        }

        if lhs.is_bignum() {
            'bignum_again: loop {
                if rhs.is_int32() {
                    let rhs = rhs.get_int32();

                    if rhs == 0 {
                        unreachable!("arith_quotient: divide by zero")
                    }

                    let (quo, _) = ScmBigInteger::divide_digit(lhs, rhs as _, thread);

                    return quo
                        .get_bignum()
                        .i32()
                        .map(Value::encode_int32)
                        .unwrap_or(quo);
                }

                if rhs.is_double() {
                    return Value::encode_f64_value(lhs.get_bignum().f64() / rhs.get_double());
                }

                if rhs.is_bignum() {
                    let (quo, _) = ScmBigInteger::divided(lhs, rhs, thread);

                    return quo
                        .get_bignum()
                        .i32()
                        .map(Value::encode_int32)
                        .unwrap_or(quo);
                }

                if rhs.is_rational() {
                    assert!(false);
                }

                if rhs.is_complex() {
                    real = rhs.get_complex().real;
                    imag = rhs.get_complex().imag;
                    if n_zero_p(imag) {
                        rhs = real;
                        continue 'bignum_again;
                    }

                    assert!(false);
                }

                break 'start_again;
            }
        }

        if lhs.is_rational() {
            assert!(false);
        }

        if lhs.is_complex() {
            real = lhs.get_complex().real;
            imag = lhs.get_complex().imag;
            if n_zero_p(imag) {
                lhs = real;
                continue 'start_again;
            }

            assert!(false);
        }

        unreachable!()
    }

    unreachable!()
}

pub fn arith_modulo(thread: &mut Thread, mut lhs: Value, mut rhs: Value) -> Value {
    let mut real;
    let mut imag;

    'start_again: loop {
        if lhs.is_int32() {
            if lhs.get_int32() == 0 {
                return lhs;
            }

            'fixnum_again: loop {
                if rhs.is_int32() {
                    if rhs.get_int32() == 0 {
                        return Value::encode_undefined_value();
                    }

                    let mut rem = lhs.get_int32() % rhs.get_int32();
                    if rem == 0 {
                        return Value::encode_int32(0);
                    }

                    if (rhs.get_int32() > 0) as i32 + (rem > 0) as i32 == 1 {
                        rem += rhs.get_int32();
                    }

                    return Value::encode_int32(rem);
                }

                if rhs.is_double() {
                    let value = rhs.get_double();

                    if value == value.round() {
                        if value == 0.0 {
                            return Value::encode_undefined_value();
                        }

                        let rem = lhs.get_int32() as f64 % value;
                        if rem == 0.0 {
                            return Value::encode_f64_value(0.0);
                        }
                        if (value > 0.0) as i32 + (rem > 0.0) as i32 == 1 {
                            return Value::encode_f64_value(rem + value);
                        }

                        return Value::encode_f64_value(rem);
                    }

                    assert!(false);
                }

                if rhs.is_bignum() {
                    let bn1 = gc_protect!(thread => rhs => thread.make_bignum_from_i64(lhs.get_int32() as i64));
                    let (_, rem) = ScmBigInteger::divided(bn1, rhs, thread);

                    return rem
                        .get_bignum()
                        .i32()
                        .map(Value::encode_int32)
                        .unwrap_or(rem);
                }

                if rhs.is_rational() {
                    assert!(false);
                }

                if rhs.is_complex() {
                    real = rhs.get_complex().real;
                    imag = rhs.get_complex().imag;
                    if n_zero_p(imag) {
                        rhs = real;
                        continue 'fixnum_again;
                    }

                    assert!(false);
                }

                break 'start_again;
            }
        }

        if lhs.is_double() {
            let value = lhs.get_double();
            if value == value.round() {
                if value == 0.0 {
                    return lhs;
                } else {
                    assert!(false);
                }
            }
            'flonum_again: loop {
                if rhs.is_int32() {
                    let rhs = rhs.get_int32();

                    if rhs == 0 {
                        return Value::encode_undefined_value();
                    }

                    let rem = value % rhs as f64;
                    if rem == 0.0 {
                        return Value::encode_f64_value(0.0);
                    }

                    if (rhs > 0) as i32 + (rem > 0.0) as i32 == 1 {
                        return Value::encode_f64_value(rem + rhs as f64);
                    }

                    return Value::encode_f64_value(rem);
                }

                if rhs.is_double() {
                    let value2 = rhs.get_double();
                    if value2 == value2.round() {
                        if value2 == 0.0 {
                            return Value::encode_undefined_value();
                        }

                        let rem = value % value2;
                        if rem == 0.0 {
                            return Value::encode_f64_value(0.0);
                        }

                        if (value2 > 0.0) as i32 + (rem > 0.0) as i32 == 1 {
                            return Value::encode_f64_value(rem + value2);
                        }

                        return Value::encode_f64_value(rem);
                    }

                    assert!(false);
                }

                if rhs.is_bignum() {
                    return Value::encode_f64_value(value % rhs.get_bignum().f64());
                }

                if rhs.is_rational() {
                    assert!(false);
                }

                if rhs.is_complex() {
                    real = rhs.get_complex().real;
                    imag = rhs.get_complex().imag;
                    if n_zero_p(imag) {
                        rhs = real;
                        continue 'flonum_again;
                    }

                    assert!(false);
                }

                break 'start_again;
            }
        }

        if lhs.is_bignum() {
            'bignum_again: loop {
                if rhs.is_int32() {
                    let rhs = rhs.get_int32();

                    if rhs == 0 {
                        return Value::encode_undefined_value();
                    }

                    let (_quo, rem) = ScmBigInteger::divide_digit(lhs, rhs as _, thread);

                    return rem
                        .get_bignum()
                        .i32()
                        .map(Value::encode_int32)
                        .unwrap_or(rem);
                }

                if rhs.is_double() {
                    return Value::encode_f64_value(lhs.get_bignum().f64() % rhs.get_double());
                }

                if rhs.is_bignum() {
                    let (_quo, rem) = ScmBigInteger::divided(lhs, rhs, thread);

                    return rem
                        .get_bignum()
                        .i32()
                        .map(Value::encode_int32)
                        .unwrap_or(rem);
                }

                if rhs.is_rational() {
                    assert!(false);
                }

                if rhs.is_complex() {
                    real = rhs.get_complex().real;
                    imag = rhs.get_complex().imag;
                    if n_zero_p(imag) {
                        rhs = real;
                        continue 'bignum_again;
                    }

                    assert!(false);
                }

                break 'start_again;
            }
        }

        if lhs.is_rational() {
            assert!(false);
        }

        if lhs.is_complex() {
            real = lhs.get_complex().real;
            imag = lhs.get_complex().imag;
            if n_zero_p(imag) {
                lhs = real;
                continue 'start_again;
            }

            assert!(false);
        }

        break 'start_again;
    }

    unreachable!()
}

pub fn arith_mul(thread: &mut Thread, mut lhs: Value, mut rhs: Value) -> Value {
    let mut real;
    let mut imag;

    if lhs.is_int32() {
        if lhs.get_int32() == 0 {
            return lhs;
        }

        if rhs.is_int32() {
            if let Some(res) = lhs.get_int32().checked_mul(rhs.get_int32()) {
                return Value::encode_int32(res);
            } else {
                return thread
                    .make_bignum_from_i64(lhs.get_int32() as i64 * rhs.get_int32() as i64);
            }
        }

        if rhs.is_double() {
            return Value::encode_f64_value(lhs.get_int32() as f64 * rhs.get_double());
        }

        if rhs.is_bignum() {
            let bn1 =
                gc_protect!(thread => rhs => thread.make_bignum_from_i64(lhs.get_int32() as i64));
            let prod = ScmBigInteger::times(bn1, rhs, thread);

            return prod
                .get_bignum()
                .i32()
                .map(Value::encode_int32)
                .unwrap_or(prod);
        }

        if rhs.is_rational() {
            if rhs.get_rational().numerator == Value::encode_int32(1) {
                return oprtr_reduce(thread, lhs, rhs.get_rational().denominator);
            }

            if rhs.get_rational().numerator == Value::encode_int32(-1) {
                let negated = gc_protect!(thread => rhs => arith_negate(thread, lhs));
                return oprtr_reduce(thread, negated, rhs.get_rational().denominator);
            }

            let mul = gc_protect!(thread => lhs, rhs => arith_mul(thread, lhs, rhs.get_rational().numerator));

            return oprtr_reduce(thread, mul, rhs.get_rational().denominator);
        }

        if rhs.is_complex() {
            real = rhs.get_complex().real;
            imag = rhs.get_complex().imag;

            real = gc_protect!(thread => imag => arith_mul(thread, lhs, real));
            imag = gc_protect!(thread => real => arith_mul(thread, lhs, imag));

            return thread.make_complex::<false>(real, imag);
        }
    }

    if lhs.is_double() {
        if rhs.is_int32() {
            return Value::encode_f64_value(lhs.get_double() * rhs.get_int32() as f64);
        }

        if rhs.is_double() {
            return Value::encode_f64_value(lhs.get_double() * rhs.get_double());
        }

        if rhs.is_bignum() {
            return Value::encode_f64_value(lhs.get_double() * rhs.get_bignum().f64());
        }

        if rhs.is_rational() {
            return Value::encode_f64_value(
                lhs.get_double() * rational_to_double(rhs).get_double(),
            );
        }

        if rhs.is_complex() {
            real = rhs.get_complex().real;
            imag = rhs.get_complex().imag;
            real = gc_protect!(thread => imag => arith_mul(thread, lhs, real));
            imag = gc_protect!(thread => real => arith_mul(thread, lhs, imag));

            return thread.make_complex::<false>(real, imag);
        }
    }

    if lhs.is_bignum() {
        if rhs.is_int32() {
            let bn1 =
                gc_protect!(thread => lhs => thread.make_bignum_from_i64(rhs.get_int32() as i64));
            let prod = ScmBigInteger::times(bn1, lhs, thread);

            return prod
                .get_bignum()
                .i32()
                .map(Value::encode_int32)
                .unwrap_or(prod);
        }

        if rhs.is_double() {
            return Value::encode_f64_value(lhs.get_bignum().f64() * rhs.get_double());
        }

        if rhs.is_bignum() {
            let prod = ScmBigInteger::times(lhs, rhs, thread);

            return prod
                .get_bignum()
                .i32()
                .map(Value::encode_int32)
                .unwrap_or(prod);
        }

        if rhs.is_rational() {
            if lhs.get_bignum().is_one() && !lhs.get_bignum().is_negative() {
                return oprtr_reduce(
                    thread,
                    rhs.get_rational().numerator,
                    rhs.get_rational().denominator,
                );
            }

            if lhs.get_bignum().is_one() {
                let negated = gc_protect!(thread => rhs => arith_negate(thread, rhs.get_rational().numerator));
                return oprtr_reduce(thread, negated, rhs.get_rational().denominator);
            }

            let mul = gc_protect!(thread => lhs, rhs => arith_mul(thread, lhs, rhs.get_rational().numerator));

            return oprtr_reduce(thread, mul, rhs.get_rational().denominator);
        }

        if rhs.is_complex() {
            real = rhs.get_complex().real;
            imag = rhs.get_complex().imag;
            real = gc_protect!(thread => imag => arith_mul(thread, lhs, real));
            imag = gc_protect!(thread => real => arith_mul(thread, lhs, imag));

            return thread.make_complex::<false>(real, imag);
        }
    }

    if lhs.is_rational() {
        if rhs.is_int32() {
            if lhs.get_rational().numerator == Value::encode_int32(1) {
                return oprtr_reduce(thread, rhs, lhs.get_rational().denominator);
            }

            if lhs.get_rational().numerator == Value::encode_int32(-1) {
                let negated = gc_protect!(thread => lhs => arith_negate(thread, rhs));
                return oprtr_reduce(thread, negated, lhs.get_rational().denominator);
            }

            let mul = gc_protect!(thread => lhs, rhs => arith_mul(thread, lhs.get_rational().numerator, rhs));

            return oprtr_reduce(thread, mul, lhs.get_rational().denominator);
        }

        if rhs.is_double() {
            return Value::encode_f64_value(
                rational_to_double(lhs).get_double() * rhs.get_double(),
            );
        }

        if rhs.is_bignum() {
            if rhs.get_bignum().is_one() && !rhs.get_bignum().is_negative() {
                return oprtr_reduce(
                    thread,
                    lhs.get_rational().numerator,
                    lhs.get_rational().denominator,
                );
            }

            if rhs.get_bignum().is_one() {
                let negated = gc_protect!(thread => lhs => arith_negate(thread, lhs.get_rational().numerator));
                return oprtr_reduce(thread, negated, lhs.get_rational().denominator);
            }

            let mul = gc_protect!(thread => lhs, rhs => arith_mul(thread, lhs.get_rational().numerator, rhs));

            return oprtr_reduce(thread, mul, lhs.get_rational().denominator);
        }

        if rhs.is_rational() {
            let mut nume = gc_protect!(thread => lhs, rhs => arith_mul(thread, lhs.get_rational().numerator, rhs.get_rational().numerator));
            let deno = gc_protect!(thread => nume => arith_mul(thread, lhs.get_rational().denominator, rhs.get_rational().denominator));

            return oprtr_reduce(thread, nume, deno);
        }

        if rhs.is_complex() {
            real = rhs.get_complex().real;
            imag = rhs.get_complex().imag;
            real = gc_protect!(thread => imag => arith_mul(thread, lhs, real));
            imag = gc_protect!(thread => real => arith_mul(thread, lhs, imag));

            return thread.make_complex::<false>(real, imag);
        }
    }

    if lhs.is_complex() {
        real = lhs.get_complex().real;
        imag = lhs.get_complex().imag;
        if rhs.is_int32() {
            if rhs.get_int32() == 0 {
                return rhs;
            }

            real = gc_protect!(thread => imag => arith_mul(thread, real, rhs));
            imag = gc_protect!(thread => real => arith_mul(thread, imag, rhs));

            return thread.make_complex::<false>(real, imag);
        }

        if rhs.is_double() {
            real = gc_protect!(thread => imag => arith_mul(thread, real, rhs));
            imag = gc_protect!(thread => real => arith_mul(thread, imag, rhs));

            return thread.make_complex::<false>(real, imag);
        }

        if rhs.is_bignum() {
            real = gc_protect!(thread => imag, rhs => arith_mul(thread, real, rhs));
            imag = gc_protect!(thread => real => arith_mul(thread, imag, rhs));

            return thread.make_complex::<false>(real, imag);
        }

        if rhs.is_rational() {
            real = gc_protect!(thread => imag, rhs => arith_mul(thread, real, rhs));
            imag = gc_protect!(thread => real => arith_mul(thread, imag, rhs));

            return thread.make_complex::<false>(real, imag);
        }

        if rhs.is_complex() {
            let mut mul_imag_imag = gc_protect!(thread => imag, rhs, real => arith_mul(thread, imag, rhs.get_complex().imag));
            let mul_real_real = gc_protect!(thread => mul_imag_imag, rhs => arith_mul(thread, real, rhs.get_complex().real));
            let mut sub = gc_protect!(thread =>mul_imag_imag => arith_sub(thread, mul_real_real, mul_imag_imag));
            let mul_real_imag =
                gc_protect!(thread => real, rhs => arith_mul(thread, real, rhs.get_complex().imag));
            let mul_imag_real =
                gc_protect!(thread => imag, rhs => arith_mul(thread, imag, rhs.get_complex().real));
            let add = gc_protect!(thread => sub => arith_add(thread, mul_real_imag, mul_imag_real));

            return thread.make_complex::<false>(sub, add);
        }
    }

    unreachable!("wrong datum type")
}

pub fn arith_div(thread: &mut Thread, mut lhs: Value, mut rhs: Value) -> Value {
    let real;
    let mut imag;

    if lhs.is_int32() {
        if lhs.get_int32() == 0 {
            if n_exact_p(rhs) {
                return lhs;
            }
            lhs = Value::encode_f64_value(0.0);
        } else if rhs.is_int32() {
            return oprtr_reduce_fixnum_fixnum(thread, lhs, rhs);
        } else if rhs.is_double() {
            return Value::encode_f64_value(lhs.get_int32() as f64 / rhs.get_double());
        } else if rhs.is_bignum() {
            return oprtr_reduce_fixnum_bignum(thread, lhs, rhs);
        } else if rhs.is_rational() {
            let mul = gc_protect!(thread => lhs, rhs => arith_mul(thread, lhs, rhs.get_rational().denominator));
            return oprtr_reduce(thread, mul, rhs.get_rational().numerator);
        } else if rhs.is_complex() {
            real = rhs.get_complex().real;
            imag = rhs.get_complex().imag;

            let mut mul_real_real = gc_protect!(thread => imag => arith_mul(thread, real, real));
            let mul_imag_imag =
                gc_protect!(thread => mul_real_real => arith_mul(thread, imag, imag));
            let mut r2 = arith_add(thread, mul_real_real, mul_imag_imag);
            let mut mul_real_lhs = gc_protect!(thread => r2 => arith_mul(thread, real, lhs));
            let mut div_r2_real_lhs =
                gc_protect!(thread => mul_real_lhs => arith_div(thread, mul_real_lhs, r2));
            let mul_real_imag =
                gc_protect!(thread => div_r2_real_lhs => arith_mul(thread, real, imag));
            let div_r2_real_imag =
                gc_protect!(thread => div_r2_real_lhs => arith_div(thread, mul_real_imag, r2));
            let negated =
                gc_protect!(thread => div_r2_real_lhs => arith_negate(thread, div_r2_real_imag));

            return thread.make_complex::<false>(div_r2_real_lhs, negated);
        }
    }

    if lhs.is_double() {
        if rhs.is_int32() {
            return Value::encode_f64_value(lhs.get_double() / rhs.get_int32() as f64);
        }

        if rhs.is_double() {
            return Value::encode_f64_value(lhs.get_double() / rhs.get_double());
        }

        if rhs.is_bignum() {
            return Value::encode_f64_value(lhs.get_double() / rhs.get_bignum().f64());
        }

        if rhs.is_rational() {
            return Value::encode_f64_value(
                lhs.get_double() / rational_to_double(rhs).get_double(),
            );
        }

        if rhs.is_complex() {
            real = rhs.get_complex().real;
            imag = rhs.get_complex().imag;

            let mut mul_real_real = gc_protect!(thread => imag => arith_mul(thread, real, real));
            let mul_imag_imag =
                gc_protect!(thread => mul_real_real => arith_mul(thread, imag, imag));
            let mut r2 = arith_add(thread, mul_real_real, mul_imag_imag);
            let mut mul_real_lhs = gc_protect!(thread => r2 => arith_mul(thread, real, lhs));
            let mut div_r2_real_lhs =
                gc_protect!(thread => mul_real_lhs => arith_div(thread, mul_real_lhs, r2));
            let mul_real_imag =
                gc_protect!(thread => div_r2_real_lhs => arith_mul(thread, real, imag));
            let div_r2_real_imag =
                gc_protect!(thread => div_r2_real_lhs => arith_div(thread, mul_real_imag, r2));
            let negated =
                gc_protect!(thread => div_r2_real_lhs => arith_negate(thread, div_r2_real_imag));

            return thread.make_complex::<false>(div_r2_real_lhs, negated);
        }
    }

    if lhs.is_bignum() {
        if rhs.is_int32() {
            return oprtr_reduce_bignum_fixnum(thread, lhs, rhs);
        }

        if rhs.is_double() {
            return Value::encode_f64_value(lhs.get_bignum().f64() / rhs.get_double());
        }

        if rhs.is_bignum() {
            return oprtr_reduce(thread, lhs, rhs);
        }

        if rhs.is_rational() {
            let mul = gc_protect!(thread => lhs, rhs => arith_mul(thread, lhs, rhs.get_rational().denominator));
            return oprtr_reduce(thread, mul, rhs.get_rational().numerator);
        }

        if rhs.is_complex() {
            real = rhs.get_complex().real;
            imag = rhs.get_complex().imag;

            let mut mul_real_real = gc_protect!(thread => imag => arith_mul(thread, real, real));
            let mul_imag_imag =
                gc_protect!(thread => mul_real_real => arith_mul(thread, imag, imag));
            let mut r2 = arith_add(thread, mul_real_real, mul_imag_imag);
            let mut mul_real_lhs = gc_protect!(thread => r2 => arith_mul(thread, real, lhs));
            let mut div_r2_real_lhs =
                gc_protect!(thread => mul_real_lhs => arith_div(thread, mul_real_lhs, r2));
            let mul_real_imag =
                gc_protect!(thread => div_r2_real_lhs => arith_mul(thread, real, imag));
            let div_r2_real_imag =
                gc_protect!(thread => div_r2_real_lhs => arith_div(thread, mul_real_imag, r2));
            let negated =
                gc_protect!(thread => div_r2_real_lhs => arith_negate(thread, div_r2_real_imag));

            return thread.make_complex::<false>(div_r2_real_lhs, negated);
        }
    }

    if lhs.is_rational() {
        if rhs.is_int32() {
            let deno = gc_protect!(thread => rhs => arith_mul(thread, lhs.get_rational().denominator, rhs));

            return oprtr_reduce(thread, rhs.get_rational().numerator, deno);
        }

        if rhs.is_double() {
            return Value::encode_f64_value(
                rational_to_double(lhs).get_double() / rhs.get_double(),
            );
        }

        if rhs.is_bignum() {
            if rhs.get_bignum().is_one() && !rhs.get_bignum().is_negative() {
                return oprtr_reduce(
                    thread,
                    lhs.get_rational().numerator,
                    lhs.get_rational().denominator,
                );
            }

            if rhs.get_bignum().is_one() {
                let negated = gc_protect!(thread => lhs => arith_negate(thread, lhs.get_rational().numerator));
                return oprtr_reduce(thread, negated, lhs.get_rational().denominator);
            }

            let mut nume = gc_protect!(thread => lhs, rhs => arith_mul(thread, lhs.get_rational().numerator, rhs));
            let deno = gc_protect!(thread => nume => arith_mul(thread, lhs.get_rational().denominator, rhs));

            return oprtr_reduce(thread, nume, deno);
        }

        if rhs.is_rational() {
            let mut nume = gc_protect!(thread => lhs, rhs => arith_mul(thread, lhs.get_rational().numerator, rhs.get_rational().denominator));
            let deno = gc_protect!(thread => nume => arith_mul(thread, lhs.get_rational().denominator, rhs.get_rational().numerator));

            return oprtr_reduce(thread, nume, deno);
        }

        if rhs.is_complex() {
            real = rhs.get_complex().real;
            imag = rhs.get_complex().imag;

            let mut mul_real_real = gc_protect!(thread => imag => arith_mul(thread, real, real));
            let mul_imag_imag =
                gc_protect!(thread => mul_real_real => arith_mul(thread, imag, imag));
            let mut r2 = arith_add(thread, mul_real_real, mul_imag_imag);
            let mut mul_real_lhs = gc_protect!(thread => r2 => arith_mul(thread, real, lhs));
            let mut div_r2_real_lhs =
                gc_protect!(thread => mul_real_lhs => arith_div(thread, mul_real_lhs, r2));
            let mul_real_imag =
                gc_protect!(thread => div_r2_real_lhs => arith_mul(thread, real, imag));
            let div_r2_real_imag =
                gc_protect!(thread => div_r2_real_lhs => arith_div(thread, mul_real_imag, r2));
            let negated =
                gc_protect!(thread => div_r2_real_lhs => arith_negate(thread, div_r2_real_imag));

            return thread.make_complex::<false>(div_r2_real_lhs, negated);
        }
    }

    if lhs.is_complex() {
        todo!("complex div")
    }

    unreachable!("wrong datum type")
}

pub fn cnvt_to_exact(thread: &mut Thread, obj: Value) -> Value {
    if obj.is_double() {
        if obj.get_double() == 0.0 {
            return Value::encode_int32(0);
        }

        let (mant, exp, sign) = decode_double(obj.get_double());

        if mant > 0 {
            if exp == 0 {
                return thread.make_integer(if sign > 0 { mant } else { -mant });
            }
        }

        if exp < 0 {
            let one = thread.make_bignum_from_i64(1);

            let denum = ScmBigInteger::shift_left(&one.get_bignum(), thread, -exp);
            let numer = thread.make_bignum_from_u64(mant as u64);

            numer.get_bignum().negative = sign < 0;

            return oprtr_reduce(thread, numer, denum);
        } else {
            let numer = thread.make_bignum_from_u64(mant as u64);
            let numer = ScmBigInteger::shift_left(&numer.get_bignum(), thread, exp);

            return oprtr_reduce(thread, numer, Value::encode_int32(1));
        }
    }

    if obj.is_complex() {
        if obj.get_complex().real.is_double() || obj.get_complex().imag.is_double() {
            let real = cnvt_to_exact(thread, obj.get_complex().real);
            let imag = cnvt_to_exact(thread, obj.get_complex().imag);

            return oprtr_norm_complex(thread, real, imag);
        }
    }

    obj
}

pub fn cnvt_to_inexact(thread: &mut Thread, obj: Value) -> Value {
    if obj.is_int32() {
        return Value::encode_f64_value(obj.get_int32() as f64);
    } else if obj.is_double() {
        return obj;
    } else if obj.is_bignum() {
        return Value::encode_f64_value(obj.get_bignum().f64());
    } else if obj.is_rational() {
        return rational_to_double(obj);
    } else if obj.is_complex() {
        if obj.get_complex().real.is_double() && obj.get_complex().imag.is_double() {
            return obj;
        }

        let real = cnvt_to_inexact(thread, obj.get_complex().real);
        let imag = cnvt_to_inexact(thread, obj.get_complex().imag);

        return oprtr_norm_complex(thread, real, imag);
    }

    unreachable!()
}

pub fn arith_expt(thread: &mut Thread, lhs: Value, mut rhs: Value) -> Value {
    if lhs.is_double() && lhs.get_double() == 0.0 {
        if rhs.is_complex() {
            if n_positive_p(rhs.get_complex().real) {
                return lhs;
            }
        } else {
            if n_positive_p(rhs) {
                return lhs;
            }
        }
    }

    if n_exact_p(rhs) {
        if rhs.is_int32() {
            if rhs.get_int32() == 0 {
                return Value::encode_int32(1);
            }

            if lhs.is_double() {
                return Value::encode_f64_value(lhs.get_double().powi(rhs.get_int32()));
            }

            return oprtr_expt(thread, lhs, rhs);
        }

        if rhs.is_bignum() {
            if real_value_p(lhs) {
                let n = rhs.get_bignum().f64();
                return Value::encode_f64_value(real_to_double(lhs).get_double().powf(n));
            }

            let log = gc_protect!(thread => rhs => arith_log(thread, lhs));
            let mul = arith_mul(thread, log, rhs);

            return arith_exp(thread, mul);
        }

        if rhs.is_rational() {
            let n = rational_to_double(rhs).get_double();

            if real_value_p(lhs) && !n_negative_p(lhs) {
                return Value::encode_f64_value(real_to_double(lhs).get_double().powf(n));
            }

            let log = gc_protect!(thread => rhs => arith_log(thread, lhs));
            let mul = arith_mul(thread, log, rhs);

            return arith_exp(thread, mul);
        }
    } else {
        if rhs.is_double() {
            if real_value_p(lhs) && !n_negative_p(lhs) {
                let n = rhs.get_double();

                return Value::encode_f64_value(real_to_double(lhs).get_double().powf(n));
            }

            let log = gc_protect!(thread => rhs => arith_log(thread, lhs));
            let mul = arith_mul(thread, log, rhs);

            return arith_exp(thread, mul);
        }

        let log = gc_protect!(thread => rhs => arith_log(thread, lhs));
        let mul = arith_mul(thread, log, rhs);

        return arith_exp(thread, mul);
    }

    unreachable!("wrong datum type")
}

pub fn arith_exp(thread: &mut Thread, obj: Value) -> Value {
    if obj.is_int32() {
        if obj == Value::encode_int32(0) {
            return Value::encode_int32(1);
        }

        return Value::encode_f64_value((obj.get_int32() as f64).exp());
    }

    if obj.is_complex() {
        let real = real_to_double(obj.get_complex().real).get_double();
        let imag = real_to_double(obj.get_complex().imag).get_double();
        let a = real.exp();
        return thread.make_complex::<false>(
            Value::encode_f64_value(a * imag.cos()),
            Value::encode_f64_value(a * imag.sin()),
        );
    }

    if real_value_p(obj) {
        let real = real_to_double(obj).get_double();

        return Value::encode_f64_value(real.exp());
    }

    unreachable!("wrong datum type")
}

pub fn arith_log(thread: &mut Thread, obj: Value) -> Value {
    if obj.is_int32() {
        let value = obj.get_int32();

        if value > 0 {
            if value == 1 {
                return Value::encode_int32(0);
            }

            return Value::encode_f64_value((value as f64).log2());
        }

        let real = value as f64;
        return thread.make_complex::<false>(
            Value::encode_f64_value(0.5 * (real * real)),
            Value::encode_f64_value(0.0f64.atan2(real)),
        );
    }

    if obj.is_complex() {
        let real = real_to_double(obj.get_complex().real).get_double();
        let imag = real_to_double(obj.get_complex().imag).get_double();
        return thread.make_complex::<false>(
            Value::encode_f64_value(0.5 * (real * real + imag * imag)),
            Value::encode_f64_value(imag.atan2(real)),
        );
    }

    if real_value_p(obj) {
        let real = real_to_double(obj).get_double();

        if real.is_infinite() && exact_positive_integer_p(obj) {
            let (sqrt, _) = arith_exact_integer_sqrt(thread, obj);
            let log = arith_log(thread, sqrt);
            return arith_add(thread, log, log);
        }

        if real > 0.0 {
            return Value::encode_f64_value(real.log2());
        }

        let imag = 0.0f64.atan2(real);
        if imag == 0.0 {
            return Value::encode_f64_value(0.5 * (real * real).log2());
        }

        return thread.make_complex::<false>(
            Value::encode_f64_value(0.5 * (real * real)),
            Value::encode_f64_value(imag),
        );
    }

    unreachable!("wrong datum type")
}

pub fn arith_sin(thread: &mut Thread, obj: Value) -> Value {
    if obj.is_int32() {
        if obj.get_int32() == 0 {
            return obj;
        }

        return Value::encode_f64_value((obj.get_int32() as f64).sin());
    }

    if obj.is_complex() {
        let real = real_to_double(obj.get_complex().real).get_double();
        let imag = real_to_double(obj.get_complex().imag).get_double();

        let e = imag.exp();
        let f = 1.0 / e;

        return thread.make_complex::<false>(
            Value::encode_f64_value(0.5 * real.sin() * (e + f)),
            Value::encode_f64_value(0.5 * real.cos() * (e - f)),
        );
    }

    if real_value_p(obj) {
        let real = real_to_double(obj).get_double();

        return Value::encode_f64_value(real.sin());
    }

    unreachable!("wrong datum type")
}

pub fn arith_cos(thread: &mut Thread, obj: Value) -> Value {
    if obj.is_int32() {
        if obj.get_int32() == 0 {
            return Value::encode_int32(1);
        }

        return Value::encode_f64_value((obj.get_int32() as f64).cos());
    }

    if obj.is_complex() {
        let real = real_to_double(obj.get_complex().real).get_double();
        let imag = real_to_double(obj.get_complex().imag).get_double();

        let e = imag.exp();
        let f = 1.0 / e;

        return thread.make_complex::<false>(
            Value::encode_f64_value(0.5 * real.cos() * (e + f)),
            Value::encode_f64_value(0.5 * real.sin() * (f - e)),
        );
    }

    if real_value_p(obj) {
        let real = real_to_double(obj).get_double();

        return Value::encode_f64_value(real.cos());
    }

    unreachable!("wrong datum type")
}

pub fn arith_tan(thread: &mut Thread, obj: Value) -> Value {
    if obj.is_int32() {
        if obj.get_int32() == 0 {
            return obj;
        }

        return Value::encode_f64_value((obj.get_int32() as f64).tan());
    }

    if obj.is_complex() {
        let real = real_to_double(obj.get_complex().real).get_double();
        let imag = real_to_double(obj.get_complex().imag).get_double();

        let e = (2.0 * imag).exp();
        let f = 1.0 / e;
        let d = (2.0 * real).cos() + 0.5 * (e + f);

        return thread.make_complex::<false>(
            Value::encode_f64_value((2.0 * real).sin() / d),
            Value::encode_f64_value(0.5 * (e - f) / d),
        );
    }

    if real_value_p(obj) {
        let real = real_to_double(obj).get_double();

        return Value::encode_f64_value(real.tan());
    }

    unreachable!("wrong datum type")
}

pub fn arith_sqrt(thread: &mut Thread, mut obj: Value) -> Value {
    if obj.is_int32() {
        let mut value = obj.get_int32();
        if value == 0 {
            return obj;
        }

        if value > 0 {
            let iroot = (value as f64).sqrt().floor() as i32;
            if iroot * iroot == value {
                return Value::encode_int32(iroot);
            }

            return Value::encode_f64_value((value as f64).sqrt());
        } else {
            value = -value;
            let iroot = (value as f64).sqrt().floor() as i32;
            if iroot * iroot == value {
                return thread
                    .make_complex::<false>(Value::encode_int32(0), Value::encode_int32(iroot));
            }

            return thread.make_complex::<false>(
                Value::encode_f64_value(0.0),
                Value::encode_f64_value((value as f64).sqrt()),
            );
        }
    }

    if obj.is_bignum() {
        let is_negative = obj.get_bignum().is_negative();
        let s = ScmBigInteger::sqrt(obj, thread);

        let s = s.get_bignum().i32().map(Value::encode_int32).unwrap_or(s);

        if is_negative {
            return thread.make_complex::<false>(Value::encode_int32(0), s);
        } else {
            return s;
        }
    }

    if obj.is_rational() {
        let mut numerator;
        let denominator;
        let mut complex = false;

        if n_negative_p(obj.get_rational().numerator) {
            let neg = gc_protect!(thread => obj => arith_negate(thread, obj));
            numerator = gc_protect!(thread => obj => arith_sqrt(thread, neg));
            complex = true;
        } else {
            numerator =
                gc_protect!(thread => obj => arith_sqrt(thread, obj.get_rational().numerator));
        }

        denominator = gc_protect!(thread => obj, numerator => arith_sqrt(thread, obj.get_rational().denominator));

        if numerator.is_int32() || numerator.is_bignum() {
            if denominator.is_int32() || denominator.is_bignum() {
                if complex {
                    return thread.make_complex::<false>(numerator, denominator);
                }
                return oprtr_reduce(thread, numerator, denominator);
            }
        }

        if complex {
            let div = gc_protect!(thread =>  => arith_div(thread, numerator, denominator));
            return thread.make_complex::<false>(Value::encode_f64_value(0.0), div);
        }

        return arith_div(thread, numerator, denominator);
    }

    if obj.is_double() {
        let s = obj.get_double().sqrt();
        if s < 0.0 {
            return thread.make_complex::<false>(
                Value::encode_f64_value(0.0),
                Value::encode_f64_value((-obj.get_double()).sqrt()),
            );
        }

        return Value::encode_f64_value(s);
    }

    todo!()
}

pub fn arith_asin(thread: &mut Thread, obj: Value) -> Value {
    let _cn = if real_value_p(obj) {
        let x = real_to_double(obj).get_double();

        if x >= -1.0 && x <= 1.0 {
            return Value::encode_f64_value(x.asin());
        }

        if x < 0.0 {
            let asin = (-x).asin();
            return Value::encode_f64_value(-asin);
        }

        thread.make_complex::<false>(Value::encode_f64_value(0.0), Value::encode_f64_value(x))
    } else {
        todo!()
    };

    todo!()
}

pub fn arith_floor(thread: &mut Thread, mut obj: Value) -> Value {
    if obj.is_int32() || obj.is_bignum() {
        return obj;
    }

    if obj.is_double() {
        return Value::encode_f64_value(obj.get_double().floor());
    }

    if obj.is_rational() {
        if n_negative_p(obj.get_rational().numerator) {
            let mut quotient = gc_protect!(thread => obj => arith_quotient(thread, obj.get_rational().numerator, obj.get_rational().denominator));
            let sub = gc_protect!(thread => obj, quotient => arith_sub(thread, obj, quotient));
            return sub;
        }

        return arith_quotient(
            thread,
            obj.get_rational().numerator,
            obj.get_rational().denominator,
        );
    }

    unreachable!("wrong datum type")
}

pub fn arith_integer_div(thread: &mut Thread, mut lhs: Value, rhs: Value) -> Value {
    if lhs.is_int32() {
        if rhs.is_int32() {
            let x = lhs.get_int32();
            let y = rhs.get_int32();

            let div;
            if x == 0 {
                div = 0
            } else if x > 0 {
                div = x / y
            } else if y > 0 {
                div = (x - y + 1) / y;
            } else {
                div = (x + y + 1) / y;
            }

            return Value::encode_int32(div);
        }
    }

    if lhs.is_double() || rhs.is_double() {
        let x = real_to_double(lhs).get_double();
        let y = real_to_double(rhs).get_double();

        return Value::encode_f64_value(if y > 0.0 {
            (x / y).floor()
        } else {
            -(x / -y).floor()
        });
    }

    if n_positive_p(rhs) {
        let div = arith_div(thread, lhs, rhs);
        return arith_floor(thread, div);
    }
    let neg = gc_protect!(thread => lhs => arith_negate(thread, rhs));
    let div = arith_div(thread, lhs, neg);
    let floor = arith_floor(thread, div);
    return arith_negate(thread, floor);
}

pub fn arith_exact_integer_sqrt(thread: &mut Thread, mut obj: Value) -> (Value, Value) {
    if obj.is_int32() {
        let value = obj.get_int32();
        if value == 0 {
            return (Value::encode_int32(0), Value::encode_int32(0));
        }

        let iroot = (value as f64).sqrt().floor() as i32;

        return (
            Value::encode_int32(iroot),
            Value::encode_int32(value - iroot * iroot),
        );
    }

    if obj.is_bignum() {
        let mut sqrt = gc_protect!(thread => obj => ScmBigInteger::sqrt(obj, thread));
        let mul = gc_protect!(thread => obj, sqrt => arith_mul(thread, sqrt, sqrt));
        let sub = arith_sub(thread, obj, mul);

        return (sqrt, sub);
    }

    unreachable!("wrong datum type")
}

pub fn cnvt_number_to_string(thread: &mut Thread, obj: Value, radix: i32) -> String {
    if obj.is_int32() {
        match radix {
            2 => return format!("{:b}", obj.get_int32()),
            8 => return format!("{:o}", obj.get_int32()),
            10 => return format!("{}", obj.get_int32()),
            16 => return format!("{:x}", obj.get_int32()),
            _ => unreachable!(),
        }
    }

    if obj.is_bignum() {
        let base = match radix {
            2 => &ScmBigInteger::BIN_BASE,
            8 => &ScmBigInteger::OCT_BASE,
            10 => &ScmBigInteger::DEC_BASE,
            16 => &ScmBigInteger::HEX_BASE,
            _ => unreachable!(),
        };
        return obj.get_bignum().to_string_base(base);
    }

    if obj.is_double() {
        return obj.get_double().to_string();
    }

    if obj.is_rational() {
        let nume = cnvt_number_to_string(thread, obj.get_rational().numerator, radix);
        let deno = cnvt_number_to_string(thread, obj.get_rational().denominator, radix);

        return format!("{}/{}", nume, deno);
    }

    assert!(obj.is_complex());
    let first = cnvt_number_to_string(thread, obj.get_complex().real, radix);
    let second = cnvt_number_to_string(thread, obj.get_complex().imag, radix);
    let need_plus = !second.starts_with('+') && !second.starts_with('-');
    if need_plus {
        return format!("{}+{}i", first, second);
    } else {
        return format!("{}{}i", first, second);
    }
}

fn ldexp(x: f64, exp: i32) -> f64 {
    x * (2.0f64).powi(exp)
}

fn nextfloat(z: f64) -> f64 {
    let (m, k, _sign) = decode_double(z);

    if m == IEXPT_2N53 - 1 {
        return ldexp(IEXPT_2N53 as _, k + 1);
    }

    ldexp((m + 1) as _, k)
}

fn prevfloat(z: f64) -> f64 {
    let (m, k, _sign) = decode_double(z);

    if m == IEXPT_2N52 {
        return ldexp((IEXPT_2N53 - 1) as _, k - 1);
    }

    ldexp((m - 1) as _, k)
}

/// Reference:
/// William D. Clinger.
/// How to read floating point numbers accurately
/// Proceedings of the ACM SIGPLAN 1990 conference on Programming language design and implementation, p.92-101, June 1990
pub fn algorithm_r(f: u128, e: i32, z0: f64) -> f64 {
    let mut z = z0;
    let x0;
    let pow10e;

    if e >= 0 {
        x0 = Some(f as i128 * 10i128.pow(e as _));
        pow10e = None;
    } else {
        x0 = Some(f as i128);
        pow10e = Some(10i128.pow((-e) as _));
    }

    loop {
        if z.is_infinite() {
            return z;
        }

        let (m, k, _) = decode_double(z);
        let x;
        let y;
        if e >= 0 {
            if k >= 0 {
                x = x0.unwrap();
                y = m as i128;
            } else {
                x = x0.unwrap().ilog(-(k as i128)) as i128;
                y = m as i128;
            }
        } else {
            if k >= 0 {
                x = f as i128;
                y = m as i128 * pow10e.unwrap();
            } else {
                x = 0;
                y = 0;
            }
        }

        let d = x - y;
        let mut d2 = (m.wrapping_add(m) as i128).wrapping_mul(d);

        let neg_d = d < 0;

        if neg_d {
            d2 = -d2;
        }

        let test = d2.cmp(&y);

        if test == Ordering::Less {
            if neg_d && m == IEXPT_2N52 && ucmp3(d2, d2, y) == Ordering::Greater {
                z = prevfloat(z);
                continue;
            }

            return z;
        }
        if test == Ordering::Equal {
            if (m & 1) == 0 {
                if neg_d && m == IEXPT_2N52 {
                    z = prevfloat(z);
                    continue;
                }
                return z;
            }

            return if neg_d { prevfloat(z) } else { nextfloat(z) };
        }
        z = if neg_d { prevfloat(z) } else { nextfloat(z) };
    }
}

pub fn ucmp3(n1: i128, n2: i128, n3: i128) -> Ordering {
    if n1 < n2 {
        return Ordering::Less;
    }

    if n1 > n2 {
        return Ordering::Greater;
    }

    if n1 < n3 {
        return Ordering::Less;
    }

    if n1 > n3 {
        return Ordering::Greater;
    }

    return Ordering::Equal;
}

use num::bigint::BigInt;
use num::BigRational;
use num::Complex;
use num::{Num, Rational32, ToPrimitive};

pub fn parse_number(thread: &mut Thread, text: &str, radix: u32) -> Option<Value> {
    if let Ok(num) = i32::from_str_radix(text, radix) {
        return Some(Value::encode_int32(num));
    } else if let Ok(num) = BigInt::from_str_radix(text, radix) {
        let s = num.to_string();
        return thread.make_bignum_from_str(&s, &ScmBigInteger::DEC_BASE);
    } else if let Ok(num) = f64::from_str_radix(text, radix) {
        return Some(Value::encode_f64_value(num));
    } else if let Some(num) = parse_rational(thread, text, radix) {
        return Some(num);
    } else if let Ok(num) = Complex::<f64>::from_str_radix(text, radix) {
        return Some(oprtr_norm_complex(
            thread,
            Value::encode_f64_value(num.re),
            Value::encode_f64_value(num.im),
        ));
    } else {
        None
    }
}

fn parse_rational(thread: &mut Thread, text: &str, radix: u32) -> Option<Value> {
    if let Ok(num) = Rational32::from_str_radix(text, radix) {
        if num.is_integer() {
            Some(Value::encode_int32(num.to_integer()))
        } else {
            Some(thread.make_rational::<false>(
                Value::encode_int32(*num.numer()),
                Value::encode_int32(*num.denom()),
            ))
        }
    } else if let Ok(num) = BigRational::from_str_radix(text, radix) {
        if num.is_integer() {
            let s = num.to_integer();

            if let Some(x) = s.to_i32() {
                return Some(Value::encode_int32(x));
            } else {
                let s = s.to_string();

                return thread.make_bignum_from_str(&s, &ScmBigInteger::DEC_BASE);
            }
        }

        let mut nume = num
            .numer()
            .to_i32()
            .map(|x| Value::encode_int32(x))
            .unwrap_or_else(|| {
                let s = num.numer().to_string();
                thread
                    .make_bignum_from_str(&s, &ScmBigInteger::DEC_BASE)
                    .unwrap()
            });
        let deno = gc_protect!(thread => nume => {
            num.denom().to_i32().map(|x| Value::encode_int32(x))
            .unwrap_or_else(|| {
                let s = num.denom().to_string();
                thread.make_bignum_from_str(&s, &ScmBigInteger::DEC_BASE).unwrap()
            })
        });

        Some(thread.make_rational::<false>(nume, deno))
    } else {
        None
    }
}

impl Value {
    pub fn is_number(self) -> bool {
        self.is_inline_number() || self.is_bignum() || self.is_rational() || self.is_complex()
    }
}

pub fn approximate_number(thread: &mut Thread, x: f64) -> Option<Value> {
    if let Some(val) = approximate(thread, x) {
        return Some(val);
    } else if let Some(val) = approximate_big_rational(thread, x) {
        return Some(val);
    } else {
        return None;
    }
}

pub fn approximate(thread: &mut Thread, x: f64) -> Option<Value> {
    const TOLERANCE: f64 = 1.0e-15;

    let mut y = x.abs();
    let mx = y * TOLERANCE;
    let (mut n1, mut d1) = (1i64, 0i64);
    let (mut n2, mut d2) = (0i64, 1i64);

    loop {
        if y.is_infinite() {
            return None;
        }

        if let Some(fy) = y.floor().to_i64() {
            (n1, n2) = (fy * n1 + n2, n1);
            (d1, d2) = (fy * d1 + d2, d1);
            y = 1.0 / (y - y.floor());
        } else {
            return None;
        }

        let check = (x.abs() - (n1 as f64) / (d1 as f64)) > mx;
        if !check {
            break;
        }
    }

    let numer = thread.make_integer(n1 * if x < 0.0 { -1 } else { 1 });
    let denom = thread.make_integer(d1);

    Some(thread.make_rational::<false>(numer, denom))
}

pub fn approximate_big_rational(thread: &mut Thread, x: f64) -> Option<Value> {
    const TOLERANCE: f64 = 1.0e-15;

    let mut y = x.abs();
    let mx = y * TOLERANCE;

    let (mut n1, mut d1) = (Value::encode_int32(0), Value::encode_int32(1));
    let (mut n2, mut d2) = (Value::encode_int32(1), Value::encode_int32(0));

    loop {
        if y.is_infinite() {
            return None;
        }

        let fy = Value::encode_int32(y.floor() as _);
        let mut temp = n1;
        n1 = gc_protect!(thread => temp, d1, d2 => arith_mul(thread, fy, n1));
        n1 = gc_protect!(thread => temp, d1, d2 => arith_add(thread, n1, n2));
        n2 = temp;
        d2 = d1;
        d1 = gc_protect!(thread => n1, n2, d2 => arith_mul(thread, fy, d1));
        d1 = gc_protect!(thread => n1, n2, d2 => arith_add(thread, d1, d2));
        y = 1.0 / (y - y.floor());

        let check = (x.abs() - (n1.get_bignum().f64() / d1.get_bignum().f64())) > mx;

        if !check {
            break;
        }
    }

    let n1 = if n1.is_bignum() {
        n1.get_bignum().i32().map(Value::encode_int32).unwrap_or(n1)
    } else {
        n1
    };
    let mut d1 = if d1.is_bignum() {
        d1.get_bignum().i32().map(Value::encode_int32).unwrap_or(d1)
    } else {
        d1
    };

    if x < 0.0 {
        let n1 = gc_protect!(thread => d1 => arith_negate(thread, n1));

        return Some(thread.make_rational::<false>(n1, d1));
    } else {
        return Some(thread.make_rational::<false>(n1, d1));
    }
}
