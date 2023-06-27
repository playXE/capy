use rsgc::{prelude::Handle, thread::Thread};

use crate::runtime::{
    object::{Bytevector, ObjectHeader, Type, Vector},
    value::Value,
};

#[macro_export]
macro_rules! make_vector {
    ($thread: expr; $($val: expr),*) => {
        {
            $crate::runtime::vector::make_vector_from_slice($thread, &[$($val),*])
        }
    };
    ($($val: expr),*) => {
        {
            $crate::runtime::vector::make_vector_from_slice(&mut crate::Thread::current(), &[$($val),*])
        }
    }
}

pub fn make_vector(thread: &mut Thread, size: usize) -> Handle<Vector> {
    let mut vec = thread.allocate_varsize::<Vector>(size);

    unsafe {
        let v = vec.assume_init_mut();
        v.object = ObjectHeader::new(Type::Vector);
        for i in 0..size {
            v.data.as_mut_ptr().add(i).write(Value::encode_null_value());
        }
        vec.assume_init()
    }
}

pub fn make_vector_from_slice(thread: &mut Thread, slice: &[Value]) -> Handle<Vector> {
    let mut vec = thread.allocate_varsize::<Vector>(slice.len());

    unsafe {
        let v = vec.assume_init_mut();
        v.object = ObjectHeader::new(Type::Vector);
        for i in 0..slice.len() {
            v.data.as_mut_ptr().add(i).write(slice[i]);
        }
        vec.assume_init()
    }
}

pub fn make_bytevector(thread: &mut Thread, size: usize) -> Handle<Bytevector> {
    let mut vec = thread.allocate_varsize::<Bytevector>(size);

    unsafe {
        let v = vec.assume_init_mut();
        v.object = ObjectHeader::new(Type::Bytevector);
        v.data.as_mut_ptr().write_bytes(0, size);
        vec.assume_init()
    }
}

pub fn make_bytevector_from_slice(thread: &mut Thread, slice: &[u8]) -> Handle<Bytevector> {
    let mut vec = thread.allocate_varsize::<Bytevector>(slice.len());

    unsafe {
        let v = vec.assume_init_mut();
        v.object = ObjectHeader::new(Type::Bytevector);
        v.data
            .as_mut_ptr()
            .copy_from_nonoverlapping(slice.as_ptr(), slice.len());
        vec.assume_init()
    }
}
