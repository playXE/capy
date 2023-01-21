use rsgc::{system::array::Array, prelude::{Object, Allocation, Handle}, thread::Thread};

use self::arraylist::ArrayList;

pub mod math;
pub mod arraylist;
pub mod string_builder;
pub mod bigint;
pub mod rational;
pub mod complex;

pub fn vec_to_gc<T: Object + Allocation + Clone>(thread: &mut Thread, vec: Vec<T>) -> Handle<Array<T>> {
    Array::new(thread, vec.len(), |_, x| vec[x].clone())
}

pub fn vec_to_arraylist<T: Object + Allocation>(thread: &mut Thread, vec: Vec<T>) -> arraylist::ArrayList<T> {
    let mut ls = ArrayList::new(thread);
    for x in vec {
        ls.push(thread, x);
    }

    ls
}