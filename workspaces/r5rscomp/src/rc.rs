use std::{mem::MaybeUninit, ptr::NonNull};

struct RcInner<T> {
    strong: u32,
    weak: u32,
    value: MaybeUninit<T>,
}

pub struct Rc<T: std::fmt::Debug> {
    inner: NonNull<RcInner<T>>,
}

impl<T: std::fmt::Debug> Rc<T> {
    #[cold]
    #[inline(never)]
    unsafe fn drop_slow(&mut self) {
        let inner = self.inner.as_mut();
        inner.value.assume_init_drop();
        inner.weak -= 1;
        if inner.weak == 0 {
            let _ = Box::from_raw(self.inner.as_ptr());
        }
    }

    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        this.inner == other.inner
    }

    pub fn as_ptr(this: &Self) -> *const T {
        unsafe { &mut *this.inner.as_ptr() }.value.as_mut_ptr()
    }
}

impl<T: std::fmt::Debug> Clone for Rc<T> {
    #[inline]
    fn clone(&self) -> Self {
        unsafe {
            let inner = &mut *self.inner.as_ptr();
            inner.strong += 1;
            Self { inner: self.inner }
        }
    }
}

impl<T: std::fmt::Debug> Drop for Rc<T> {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            let inner = &mut *self.inner.as_ptr();
            inner.strong -= 1;
            if inner.strong == 0 {
                self.drop_slow();
            }
        }
    }
}

impl<T: std::fmt::Debug> std::ops::Deref for Rc<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { &(*self.inner.as_ptr()).value.assume_init_ref() }
    }
}

impl<T: std::fmt::Debug> std::borrow::Borrow<T> for Rc<T> {
    #[inline]
    fn borrow(&self) -> &T {
        &**self
    }
}

impl<T: std::fmt::Debug> std::ops::DerefMut for Rc<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.inner.as_mut().value.assume_init_mut() }
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Rc<T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        (**self).fmt(f)
    }
}

impl<T: std::fmt::Debug> std::fmt::Display for Rc<T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", **self)        
    }
}

impl<T: std::fmt::Debug> std::cmp::PartialEq for Rc<T>
where
    T: std::cmp::PartialEq,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        (**self).eq(&**other)
    }
}

impl<T: std::fmt::Debug> std::cmp::Eq for Rc<T> where T: std::cmp::Eq {}

impl<T: std::fmt::Debug> std::cmp::PartialOrd for Rc<T>
where
    T: std::cmp::PartialOrd,
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (**self).partial_cmp(&**other)
    }
}

impl<T: std::fmt::Debug> std::cmp::Ord for Rc<T>
where
    T: std::cmp::Ord,
{
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (**self).cmp(&**other)
    }
}

impl<T: std::fmt::Debug> std::hash::Hash for Rc<T>
where
    T: std::hash::Hash,
{
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (**self).hash(state)
    }
}

impl<T: std::fmt::Debug> From<T> for Rc<T> {
    #[inline]
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

impl<T: std::fmt::Debug> std::convert::AsRef<T> for Rc<T> {
    #[inline]
    fn as_ref(&self) -> &T {
        &**self
    }
}

impl<T: std::fmt::Debug> Rc<T> {
    #[inline]
    pub fn new(value: T) -> Self {
        let inner = Box::new(RcInner {
            strong: 1,
            weak: 1,
            value: MaybeUninit::new(value),
        });

        Self {
            inner: NonNull::from(Box::leak(inner)),
        }
    }

    pub fn downgrade(this: &Self) -> Weak<T> {
        unsafe {
            let inner = &mut *this.inner.as_ptr();
            inner.weak += 1;
            Weak { inner: this.inner }
        }
    }

    #[inline]
    pub fn strong_count(this: &Self) -> usize {
        unsafe { (*this.inner.as_ptr()).strong as usize }
    }

    #[inline]
    pub fn weak_count(this: &Self) -> usize {
        unsafe { (*this.inner.as_ptr()).weak as usize }
    }
}

pub struct Weak<T> {
    inner: NonNull<RcInner<T>>,
}

impl<T: std::fmt::Debug> Weak<T> {
    #[inline]
    pub fn upgrade(&self) -> Option<Rc<T>> {
        unsafe {
            let inner = &mut *self.inner.as_ptr();
            if inner.strong == 0 {
                None
            } else {
                inner.strong += 1;
                Some(Rc { inner: self.inner })
            }
        }
    }

    #[inline]
    pub fn weak_count(this: &Self) -> usize {
        unsafe { (*this.inner.as_ptr()).weak as usize }
    }
}

impl<T> Drop for Weak<T> {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            let inner = &mut *self.inner.as_ptr();
            inner.weak -= 1;
            if inner.weak == 0 && inner.strong == 0 {
                let _ = Box::from_raw(self.inner.as_ptr());
            }
        }
    }
}

impl<T> Clone for Weak<T> {
    #[inline]
    fn clone(&self) -> Self {
        unsafe {
            let inner = &mut *self.inner.as_ptr();
            inner.weak += 1;
            Self { inner: self.inner }
        }
    }
}

impl<T> std::fmt::Debug for Weak<T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "#<weak {:p}>", self.inner)
        //self.upgrade().unwrap().fmt(f)
    }
}

impl<T: std::fmt::Debug> std::fmt::Display for Weak<T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.upgrade().unwrap().fmt(f)
    }
}
