use std::{
    hash::Hash,
    mem::ManuallyDrop,
    ptr::NonNull,
    sync::atomic::{AtomicU32, Ordering},
};

#[repr(C)]
struct Inner<T> {
    weak: AtomicU32,
    rc: AtomicU32,

    value: ManuallyDrop<T>,
}

pub struct P<T> {
    inner: NonNull<Inner<T>>,
}

impl<T> Hash for P<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.as_ptr().hash(state);
    }
}

impl<T> P<T> {
    pub fn new(value: T) -> Self {
        let inner = Box::into_raw(Box::new(Inner {
            rc: AtomicU32::new(1),
            weak: AtomicU32::new(1),
            value: ManuallyDrop::new(value),
        }));

        Self {
            inner: NonNull::new(inner).unwrap(),
        }
    }

    pub fn get(&self) -> &T {
        unsafe { &self.inner.as_ref().value }
    }

    pub fn get_mut(&mut self) -> &mut T {
        unsafe { &mut self.inner.as_mut().value }
    }

    pub fn as_ptr(&self) -> *const T {
        self.inner.as_ptr().cast()
    }

    pub fn as_mut_ptr(&mut self) -> *mut T {
        self.inner.as_ptr().cast()
    }

    #[inline(never)]
    unsafe fn drop_slow(&mut self) {
        ManuallyDrop::drop(&mut self.inner.as_mut().value);

        drop(Weak { inner: self.inner });
    }
}

impl<T> Clone for P<T> {
    fn clone(&self) -> Self {
        unsafe {
            self.inner
                .as_ptr()
                .as_mut()
                .unwrap()
                .rc
                .fetch_add(1, Ordering::Relaxed);
        }
        Self { inner: self.inner }
    }
}

impl<T> Drop for P<T> {
    fn drop(&mut self) {
        unsafe {
            if self.inner.as_ref().rc.fetch_sub(1, Ordering::Release) != 1 {
                return;
            }

            std::sync::atomic::fence(Ordering::Acquire);
            self.drop_slow();
        }
    }
}

impl<T> std::ops::Deref for P<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.get()
    }
}

impl<T> std::ops::DerefMut for P<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.get_mut()
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for P<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.get().fmt(f)
    }
}

impl<T: std::fmt::Display> std::fmt::Display for P<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.get().fmt(f)
    }
}

impl<T: PartialEq> PartialEq for P<T> {
    fn eq(&self, other: &Self) -> bool {
        self.get().eq(other.get())
    }
}

impl<T: Eq> Eq for P<T> {}

impl<T> AsRef<T> for P<T> {
    fn as_ref(&self) -> &T {
        self.get()
    }
}

impl<T> AsMut<T> for P<T> {
    fn as_mut(&mut self) -> &mut T {
        self.get_mut()
    }
}

#[allow(non_snake_case)]
pub fn P<T>(value: T) -> P<T> {
    P::new(value)
}

pub struct Weak<T> {
    inner: NonNull<Inner<T>>,
}

impl<T> Weak<T> {
    pub fn new(value: &P<T>) -> Self {
        /*unsafe {
            value.inner.as_ptr().as_mut().unwrap().weak += 1;
        }
        Self { inner: value.inner }*/

        unsafe {
            let mut cur = value.inner.as_ref().weak.load(Ordering::Relaxed);

            loop {
                if cur == u32::MAX {
                    std::hint::spin_loop();
                    cur = value.inner.as_ref().weak.load(Ordering::Relaxed);
                    continue;
                }

                match value.inner.as_ref().weak.compare_exchange_weak(
                    cur,
                    cur + 1,
                    Ordering::Acquire,
                    Ordering::Relaxed,
                ) {
                    Ok(_) => return Weak { inner: value.inner },

                    Err(old) => {
                        cur = old;
                    }
                }
            }
        }
    }

    pub fn upgrade(&self) -> Option<P<T>> {
        unsafe {
            /*if self.inner.as_ref().rc > 0 {
                self.inner.as_ptr().as_mut().unwrap().rc += 1;
                Some(P { inner: self.inner })
            } else {
                None
            }*/
            #[inline]
            fn checked_increment(n: u32) -> Option<u32> {
                if n == 0 {
                    None
                } else {
                    Some(n + 1)
                }
            }

            if self
                .inner
                .as_ref()
                .rc
                .fetch_update(Ordering::Acquire, Ordering::Relaxed, checked_increment)
                .is_ok()
            {
                Some(P { inner: self.inner })
            } else {
                None
            }
        }
    }
}

impl<T> Clone for Weak<T> {
    fn clone(&self) -> Self {
        unsafe {
            self.inner
                .as_ptr()
                .as_mut()
                .unwrap()
                .weak
                .fetch_add(1, Ordering::Relaxed);
        }
        Self { inner: self.inner }
    }
}

impl<T> Drop for Weak<T> {
    fn drop(&mut self) {
        unsafe {
            if self.inner.as_ref().weak.fetch_sub(1, Ordering::Release) == 1 {
                // destroy the box itself
                drop(Box::from_raw(self.inner.as_ptr()));
            }
        }
    }
}
