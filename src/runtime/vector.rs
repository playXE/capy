use rsgc::{prelude::Handle, system::array::Array, thread::Thread};

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
    // Array will be traced conservatively inside Bytevector::trace.
    let mut contents = Array::zeroed(thread, size);

    thread.allocate(Bytevector {
        object: ObjectHeader::new(Type::Bytevector),
        length: size as _,
        contents: &mut contents[0],
    })
}

pub fn make_bytevector_from_slice(thread: &mut Thread, slice: &[u8]) -> Handle<Bytevector> {
    let mut contents = Array::<u8>::copy_from(thread, slice);

    thread.allocate(Bytevector {
        object: ObjectHeader::new(Type::Bytevector),
        length: slice.len() as _,
        contents: &mut contents[0],
    })
}

pub unsafe fn make_bytevector_from_raw_parts(
    thread: &mut Thread,
    contents: *mut u8,
    length: usize,
) -> Handle<Bytevector> {
    thread.allocate(Bytevector {
        object: ObjectHeader::new(Type::Bytevector),
        length: length as _,
        contents,
    })
}

pub fn make_values(thread: &mut Thread, values: &[Value]) -> Handle<Vector> {
    let mut vec = thread.allocate_varsize::<Vector>(values.len());

    unsafe {
        let v = vec.assume_init_mut();
        v.object = ObjectHeader::new(Type::Values);
        for i in 0..values.len() {
            v.data.as_mut_ptr().add(i).write(values[i]);
        }
        vec.assume_init()
    }
}

pub fn make_values_n(thread: &mut Thread, n: usize) -> Handle<Vector> {
    let mut vec = thread.allocate_varsize::<Vector>(n);

    unsafe {
        let v = vec.assume_init_mut();
        v.object = ObjectHeader::new(Type::Values);
        for i in 0..n {
            v.data
                .as_mut_ptr()
                .add(i)
                .write(Value::encode_undefined_value());
        }
        vec.assume_init()
    }
}

pub fn scm_vector_copy(thread: &mut Thread, v: Handle<Vector>) -> Handle<Vector> {
    let mut vec = thread.allocate_varsize::<Vector>(v.len());

    unsafe {
        let v = vec.assume_init_mut();
        v.object = ObjectHeader::new(Type::Vector);
        for i in 0..v.len() {
            v.data.as_mut_ptr().add(i).write(v.data[i]);
        }
        vec.assume_init()
    }
}
