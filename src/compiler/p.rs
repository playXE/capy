use std::{mem::ManuallyDrop, ptr::NonNull, hash::Hash};

#[repr(C)]
struct Inner<T> {
    weak: u32,
    rc: u32,
    
    value: ManuallyDrop<T>,
}

pub struct P<T> {
    inner: NonNull<Inner<T>>,
}

impl<T> P<T> {
    pub fn new(value: T) -> Self {
        let inner = Box::into_raw(Box::new(Inner {
            rc: 1,
            weak: 1,
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
}

impl<T> Clone for P<T> {
    fn clone(&self) -> Self {
        unsafe {
            self.inner.as_ptr().as_mut().unwrap().rc += 1;
        }
        Self { inner: self.inner }
    }
}

impl<T> Drop for P<T> {
    fn drop(&mut self) {
        unsafe {
          
            self.inner.as_mut().rc -= 1;
            
            if self.inner.as_ref().rc == 0 {
              
                // destroy the contained object
                ManuallyDrop::drop(&mut self.inner.as_mut().value);

                // remove the implicit "strong weak" pointer now that we've
                // destroyed the contents.
                self.inner.as_mut().weak -= 1;

                if self.inner.as_ref().weak == 0 {
                    // destroy the box itself
                    drop(Box::from_raw(self.inner.as_ptr()));
                }
            }
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

impl<T: Hash> Hash for P<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.get().hash(state)
    }
}

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
        unsafe {
            value.inner.as_ptr().as_mut().unwrap().weak += 1;
        }
        Self { inner: value.inner }
    }

    pub fn upgrade(&self) -> Option<P<T>> {
        unsafe {
            if self.inner.as_ref().rc > 0 {
                self.inner.as_ptr().as_mut().unwrap().rc += 1;
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
            
            self.inner.as_ptr().as_mut().unwrap().weak += 1;
        }
        Self { inner: self.inner }
    }
}

impl<T> Drop for Weak<T> {
    fn drop(&mut self) {
        unsafe {
           
            self.inner.as_mut().weak -= 1;
            if self.inner.as_ref().weak == 0 {
                println!("ded");
                // destroy the box itself
                drop(Box::from_raw(self.inner.as_ptr()));
            }
        }
    }
}
