use core::fmt;
use std::hash::Hash;

use rsgc::prelude::{Allocation, Object};

use crate::data::pure_nan::pure_nan;

#[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
#[repr(C)]
pub struct Complex {
    pub re: f64,
    pub im: f64,
}

impl Complex {
    pub fn is_zero(self) -> bool {
        self.re == 0.0 && self.im == 0.0
    }

    pub fn is_real(self) -> bool {
        self.im == 0.0
    }

    pub fn is_finite(self) -> bool {
        self.re.is_finite() && self.im.is_finite()
    }

    pub fn is_nan(self) -> bool {
        self.re.is_nan() || self.im.is_nan()
    }

    pub fn is_infinite(self) -> bool {
        self.re.is_infinite() || self.im.is_infinite()
    }

    pub fn magnitude(self) -> f64 {
        if self.is_nan() {
            pure_nan()
        } else if self.is_finite() {
            self.re.abs().max(self.im.abs())
        } else {
            f64::INFINITY
        }
    }

    pub fn abs(self) -> f64 {
        self.re.hypot(self.im)
    }

    pub fn arg(self) -> f64 {
        self.im.atan2(self.re)
    }

    pub fn negate(self) -> Self {
        Self {
            re: -self.re,
            im: -self.im,
        }
    }

    pub fn i(self) -> Self {
        Self {
            re: -self.re,
            im: self.im,
        }
    }

    pub fn conjugate(self) -> Self {
        Self {
            re: self.re,
            im: -self.im,
        }
    }

    pub fn reciprocal(self) -> Self {
        if self.is_nan() {
            return self;
        } else if self.is_zero() {
            return Self::INFINITY;
        } else if self.is_infinite() {
            return Self::ZERO;
        }
        let denom = self.re * self.re + self.im * self.im;
        Self {
            re: self.re / denom,
            im: -self.im / denom,
        }
    }

    pub fn norm(self) -> f64 {
        self.re.hypot(self.im)
    }

    pub fn exp(self) -> Self {
        let abs = self.re.exp();
        Self {
            re: abs * self.im.cos(),
            im: abs * self.im.sin(),
        }
    }

    pub fn log(self) -> Self {
        Self {
            re: self.abs().ln(),
            im: self.arg(),
        }
    }

    pub fn sqrt(self) -> Self {
        let r = ((self.re + self.abs()) / 2.0).sqrt();
        let i = (-self.re + self.abs() / 2.0).sqrt();
        Self { re: r, im: i * self.im.signum() }
    }


    pub fn to_power(self, other: Self) -> Self {
        if self.is_zero() {
            if other.is_zero() {
                Self::new(1., 0.0)
            } else {
                Self::ZERO 
            }
        } else {
            self.log().times(other).exp()
        }
    }

    pub fn plus(self, rhs: Self) -> Self {
        Self {
            re: self.re + rhs.re,
            im: self.im + rhs.im,
        }
    }

    pub fn minus(self, rhs: Self) -> Self {
        Self {
            re: self.re - rhs.re,
            im: self.im - rhs.im,
        }
    }

    pub fn times_f64(self, rhs: f64) -> Self {
        Self {
            re: self.re * rhs,
            im: self.im * rhs,
        }
    }

    pub fn times(self, rhs: Self) -> Self {
        Self {
            re: self.re * rhs.re - self.im * rhs.im,
            im: self.re * rhs.im + self.im * rhs.re,
        }
    }

    pub fn divided(self, rhs: Self) -> Self {
        self.times(rhs.reciprocal())
    }

    pub fn divided_f64(self, rhs: f64) -> Self {
        Self {
            re: self.re / rhs,
            im: self.im / rhs,
        }
    }

    pub const fn new(re: f64, im: f64) -> Self {
        Self { re, im }
    }
    pub const INFINITY: Self = Self::new(f64::INFINITY, 0.0);

    pub const I: Self = Self::new(0., 1.);
    pub const ONE: Self = Self::new(1., 0.);
    pub const ZERO: Self = Self::new(0., 0.);
}

impl Object for Complex {}
impl Allocation for Complex {
    const NO_HEAP_PTRS: bool = true;
}

impl fmt::Display for Complex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.im == 0.0 || self.re.is_nan() || self.re.is_infinite() {
            write!(f, "{}", self.re)
        } else if self.re == 0.0 {
            write!(f, "{}i", self.im)
        } else {
            if self.im.signum() < 0.0 {
                write!(f, "{}{}i", self.re, self.im)
            } else {
                write!(f, "{} + {}i", self.re, self.im)
            }
        }
    }
}

impl Hash for Complex {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.re.to_bits().hash(state);
        self.im.to_bits().hash(state);
    }
}

impl Eq for Complex {}

