use rsgc::{prelude::Handle, thread::Thread};

use crate::runtime::object::{ObjectHeader, Str, Type};

pub fn make_string(thread: &mut Thread, s: impl AsRef<str>) -> Handle<Str> {
    let mut str = thread.allocate_varsize::<Str>(s.as_ref().len());
    let orig = s.as_ref();
    unsafe {
        let s = str.assume_init_mut();
        s.object = ObjectHeader::new(Type::Str);

        s.data
            .as_mut_ptr()
            .copy_from_nonoverlapping(orig.as_ptr(), orig.len());
        str.assume_init()
    }
}
