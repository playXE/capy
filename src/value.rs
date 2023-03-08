#![allow(dead_code)]
use memoffset::offset_of;
use rsgc::{
    heap::root_processor::SimpleRoot,
    prelude::{Allocation, Handle, Object, Thread, Visitor},
    system::array::Array,
};
use std::mem::size_of;

use crate::{
    ports_v2::{
        port_extract_string, port_input_pred, port_open_bytevector, port_output_pred, Port,
        SCM_PORT_DIRECTION_OUT,
    },
    print::Printer,
    util::string::String,
    vm::{intern, Runtime, Trampoline, Vm},
};

pub const SCHEME_MAX_ARGS: i32 = 0x3FFFFFFE;

/// A Scheme R7RS value.
///
/// We use simple pointer tagging. If the tag is 0, the value is a pointer to a heap-allocated object.
/// Otherwise, the value is a 32 bit integer. Note that `Value` is also 64 bits wide on 32 bit systems as well.
/// Someone should probably fix that to use 31 bit integers instead.
///
/// Notes on the implementation:
/// - We use a `repr(transparent)` wrapper around `u64` to ensure that `Value` is the same size as `u64`.
/// - Overhead of heap allocated objects is 16 bytes. 8 bytes for RSGC header and 8 bytes for our own [Hdr] that stores
///   Scheme type. So for example f64 boxed on heap takes 24 bytes.
/// - Some primitive values like `#t`, `#f`, `#null`, `#eof`, `#void`, `#undef` are stored as statics instead of
///   actually beeing allocated on heap. This is done to save memory and to make it easier to compare values.
///
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Value(u64);

impl Object for Value {
    fn trace(&self, visitor: &mut dyn Visitor) {
        if self.get_type() > Type::Integer {
            unsafe {
                let ptr = self.0 as *mut u8;
                let handle = Handle::<dyn Object>::from_raw(ptr);

                handle.trace(visitor);
            }
        }
    }
}

impl Allocation for Value {}

impl Value {
    pub unsafe fn encode_ptr<T>(ptr: *mut T) -> Value {
        Value(ptr as u64)
    }

    pub const fn raw(self) -> u64 {
        self.0
    }

    pub const fn ptrp(self) -> bool {
        !self.intp()
    }

    pub const fn make_int(val: i32) -> Value {
        Value((val as u64) << 1 | 0x1)
    }

    pub const fn int(self) -> i32 {
        (self.0 >> 1) as i32
    }

    pub fn make_true() -> Value {
        Value(&TRUE_VAL as *const Hdr as u64)
    }

    pub fn make_false() -> Value {
        Value(&FALSE_VAL as *const Hdr as u64)
    }

    pub fn make_undef() -> Value {
        Value(&UNDEF_VAL as *const Hdr as u64)
    }

    pub fn make_eof() -> Value {
        Value(&EOF_VAL as *const Hdr as u64)
    }

    pub fn make_void() -> Value {
        Value(&VOID_VAL as *const Hdr as u64)
    }

    pub fn make_null() -> Value {
        Value(&NULL_VAL as *const Hdr as u64)
    }

    pub fn null() -> Value {
        Value::make_null()
    }

    pub fn falsep(self) -> bool {
        self == Value::make_false()
    }

    pub fn truep(self) -> bool {
        self == Value::make_true()
    }

    pub fn nullp(self) -> bool {
        self == Value::make_null()
    }

    pub fn eofp(self) -> bool {
        self == Value::make_eof()
    }

    pub fn voidp(self) -> bool {
        self == Value::make_void()
    }

    pub fn undefp(self) -> bool {
        self == Value::make_undef()
    }

    pub const fn intp(self) -> bool {
        (self.0 & 0x1) != 0
    }

    pub fn get_type(self) -> Type {
        if self.intp() {
            return Type::Integer;
        }
        unsafe {
            let hdr = self.0 as *const Hdr;
            (*hdr).ty
        }
    }

    pub fn handle(self) -> Handle<dyn Object> {
        unsafe {
            let ptr = self.0 as *mut u8;
            Handle::<dyn Object>::from_raw(ptr)
        }
    }

    pub fn portp(self) -> bool {
        let t = self.get_type();
        t == Type::Port
    }

    pub fn downcast_port(self) -> Handle<Port> {
        self.handle().downcast::<Port>().unwrap()
    }
    pub fn output_portp(self) -> bool {
        if self.portp() {
            port_output_pred(self.downcast_port())
        } else {
            false
        }
    }

    pub fn input_portp(self) -> bool {
        if self.portp() {
            port_input_pred(self.downcast_port())
        } else {
            false
        }
    }

    /// Obtains value lock. If this value is not heap allocated it is a NO-OP.
    pub fn lock(self) -> ValueLock {
        if self.get_type() > Type::Integer {
            unsafe {
                let ptr = self.0 as *mut Hdr;
                (*ptr).lock.lock(true);
            }
        }

        ValueLock(self)
    }

    pub fn make_cons(car: Value, cdr: Value) -> Value {
        Value::make_pair(Thread::current(), car, cdr)
    }
}

pub struct ValueLock(Value);

impl Drop for ValueLock {
    fn drop(&mut self) {
        if self.0.get_type() > Type::Integer {
            unsafe {
                let ptr = self.0.raw() as *mut Hdr;
                (*ptr).lock.unlock();
            }
        }
    }
}

static TRUE_VAL: Hdr = Hdr::new(Type::True);
static FALSE_VAL: Hdr = Hdr::new(Type::False);
static UNDEF_VAL: Hdr = Hdr::new(Type::Undef);
static EOF_VAL: Hdr = Hdr::new(Type::Eof);
static VOID_VAL: Hdr = Hdr::new(Type::Void);
static NULL_VAL: Hdr = Hdr::new(Type::Null);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(u8)]
pub enum Type {
    True = 0,
    False = 1,
    Undef = 2,
    Eof = 3,
    Void = 4,
    Null = 5,

    Integer = 6,
    /*
    Heap objects start

    Note that it is VERY important that GCed object types are *after* non-GCed types. All types before `Integer` are
    allocated in static memory and are not GCed. All types after `Integer` are allocated on heap and are GCed.

     */
    Bignum,
    Rational,
    Double,
    Complex,

    Pair,
    Symbol,
    Str,
    Keyword,
    ByteVector,
    Vector,
    Char,
    Identifier,
    Gloc,
    Module,

    PrimitiveProcedure,
    ClosedPrimitiveProcedure,
    Parameter,
    ReturnCont,
    ProcStruct,
    NativeProcedure,

    Macro,
    SyntaxRules,

    Struct,
    StructureType,
    StructureProperty,

    Values,
    HashTable,
    Env,
    Boxed,
    Port,
}

impl Type {
    pub fn from_u8(t: u8) -> Option<Type> {
        if t > Type::Port as u8 {
            None
        } else {
            Some(unsafe { std::mem::transmute(t) })
        }
    }
}

/// A Scheme object header.
///
/// It stores object type and some internal flags.
#[repr(C, align(8))]
pub struct Hdr {
    pub(crate) ty: Type,
    pub(crate) immutable: bool,
    pub(crate) lock: rsgc::sync::mutex::RawMutex,
}

impl Hdr {
    pub const fn new(ty: Type) -> Hdr {
        Hdr {
            ty,
            immutable: false,
            lock: rsgc::sync::mutex::RawMutex::new(),
        }
    }
}

macro_rules! declare_typed {
    ($name:ident : $typ: ident {
        $(
           $v: vis $field:ident: $ty:ty
        ),*
    }) => {
        #[repr(C)]
        pub struct $name {
            pub(crate) hdr: Hdr,
            $(
                $v $field: $ty
            ),*
        }
        paste::paste!{
            impl $name {
                pub fn new(thread: &mut Thread, $($field: $ty),*) -> Value {
                    unsafe {
                        Value::encode_ptr(
                            thread.allocate($name {
                                hdr: Hdr::new(Type::$typ),
                                $($field),*
                            }).as_ptr())
                    }
                }

                pub fn get_type(&self) -> Type {
                    self.hdr.ty
                }

                pub fn get_hdr(&self) -> &Hdr {
                    &self.hdr
                }
                $(
                pub fn [<$field _offset>]() -> usize {
                    offset_of!($name, $field)
                }
                $v fn [<$field _ref>](&self) -> & $ty {
                    &self.$field
                }

                $v fn [<$field _mut>](&mut self) -> &mut $ty {
                    &mut self.$field
                }

                $v fn [<$field _set>](&mut self, val: $ty) {
                    self.$field = val;
                })*


            }

            impl Into<Value> for Handle<$name> {
                fn into(self) -> Value {
                    unsafe {
                        Value::encode_ptr(self.as_ptr())
                    }
                }
            }

            impl Value {
                pub fn [<$typ:lower p>](self) -> bool {
                    self.get_type() == Type::$typ
                }

                pub fn [<make_ $typ:lower>](thread: &mut Thread, $( $field: $ty),*) -> Value {
                    $name::new(thread, $($field),*)
                }

                pub fn [<downcast_ $typ:lower>](self) -> Handle<$name> {
                    cassert!(self.[<$typ:lower p>]());

                    unsafe {
                        std::mem::transmute(self)
                    }
                }

                $(
                    $v fn [<$typ:lower _ $field>]<'a>(self) -> &'a $ty {
                        cassert!(self.[<$typ:lower p>]());
                        unsafe {
                            let ptr = self.0 as *const $name;
                            &(*ptr).$field
                        }
                    }

                    $v fn [<$typ:lower _ $field _mut>]<'a>(self) -> &'a mut $ty {
                        cassert!(self.[<$typ:lower p>]());
                        unsafe {
                            let ptr = self.0 as *mut $name;
                            &mut (*ptr).$field
                        }
                    }

                    $v fn [<set_ $typ:lower _ $field>](self, val: $ty) {
                        cassert!(self.[<$typ:lower p>]());
                        unsafe {
                            let ptr = self.0 as *mut $name;
                            (*ptr).$field = val;
                        }
                    }
                )*
            }

            impl Allocation for $name {}
            impl Object for $name {
                fn trace(&self, _visitor: &mut dyn Visitor) {
                    $(
                        self.$field.trace(_visitor);
                    )*
                }
            }

        }
    };

    ($name:ident : $typ: ident (varsize $arr: ident: $vty: ty) {
        $(
           $v: vis $field:ident: $ty:ty
        ),*
    }) => {
        #[repr(C)]
        pub struct $name {
            pub(crate) hdr: Hdr,
            $(
                $v $field: $ty,
            )+
            $arr: [$vty; 0]
        }
        paste::paste!{
            impl $name {
                pub fn new($($field: $ty),*) -> $name {
                    $name {
                        hdr: Hdr::new(Type::$typ),
                        $($field,)*
                        $arr: []
                    }
                }

                pub fn get_type(&self) -> Type {
                    self.hdr.ty
                }

                pub fn get_hdr(&self) -> &Hdr {
                    &self.hdr
                }
                $(
                $v fn [<$field _ref>](&self) -> & $ty {
                    &self.$field
                }

                $v fn [<$field _mut>](&mut self) -> &mut $ty {
                    &mut self.$field
                }

                $v fn [<$field _set>](&mut self, val: $ty) {
                    self.$field = val;
                })*

                pub unsafe fn at(&self, idx: usize) -> &$vty {
                    self.$arr.get_unchecked(idx)
                }

                pub unsafe fn at_mut(&mut self, idx: usize) -> &mut $vty {
                    self.$arr.get_unchecked_mut(idx)
                }
            }

            impl Value {

                pub fn [<downcast_ $typ:lower>](self) -> Handle<$name> {
                    cassert!(self.[<$typ:lower p>]());

                    unsafe {
                        std::mem::transmute(self.0 as usize)
                    }
                }

                pub fn [<$typ:lower p>](self) -> bool {
                    self.get_type() == Type::$typ
                }

            }
        }
    };
}

declare_typed!(Double: Double {
    pub val: f64
});

declare_typed!(Pair: Pair {
    pub car: Value,
    pub cdr: Value
});

impl Value {
    pub fn double(self) -> f64 {
        cassert!(self.doublep());
        unsafe {
            let ptr = self.0 as *const Double;
            (*ptr).val
        }
    }
}

declare_typed!(Char: Char {
    pub val: char
});

impl Value {
    pub fn char(self) -> char {
        cassert!(self.charp());
        unsafe {
            let ptr = self.0 as *const Char;
            (*ptr).val
        }
    }

    pub fn make_char_cached(c: char) -> Value {
        if c.is_ascii() {
            unsafe { *CHAR_CACHE.get_unchecked(c as usize) }
        } else {
            Value::make_char(Thread::current(), c)
        }
    }

    pub fn car(self) -> Value {
        self.downcast_pair().car
    }

    pub fn cdr(self) -> Value {
        self.downcast_pair().cdr
    }

    pub fn caar(self) -> Value {
        self.car().car()
    }

    pub fn cadr(self) -> Value {
        self.cdr().car()
    }

    pub fn cdar(self) -> Value {
        self.car().cdr()
    }

    pub fn cddr(self) -> Value {
        self.cdr().cdr()
    }

    pub fn caaar(self) -> Value {
        self.car().caar()
    }

    pub fn caadr(self) -> Value {
        self.car().cadr()
    }

    pub fn cadar(self) -> Value {
        self.car().cdar()
    }

    pub fn caddr(self) -> Value {
        self.cdr().cadr()
    }

    pub fn cdaar(self) -> Value {
        self.car().cdar()
    }

    pub fn cdadr(self) -> Value {
        self.cdr().car().cdr()
    }

    pub fn cddar(self) -> Value {
        self.car().cddr()
    }

    pub fn cdddr(self) -> Value {
        self.cdr().cddr()
    }

    pub fn cadddr(self) -> Value {
        self.cdddr().car()
    }
}

#[repr(C)]
pub struct Str {
    hdr: Hdr,
    str: String,
}

impl Into<Value> for Handle<Str> {
    fn into(self) -> Value {
        unsafe { Value::encode_ptr(self.as_ptr()) }
    }
}

impl Object for Str {
    fn trace(&self, visitor: &mut dyn Visitor) {
        self.str.trace(visitor);
    }
}

impl Allocation for Str {}

impl Str {
    pub fn new(thread: &mut Thread, s: impl AsRef<str>) -> Value {
        let s = String::from_str(thread, s.as_ref());
        unsafe {
            Value::encode_ptr(
                thread
                    .allocate(Str {
                        hdr: Hdr::new(Type::Str),
                        str: s,
                    })
                    .as_ptr(),
            )
        }
    }

    pub fn len(&self) -> usize {
        self.str.len()
    }

    pub fn str(&self) -> &str {
        &self.str
    }

    pub fn str_mut(&mut self) -> &mut String {
        &mut self.str
    }
}

impl Value {
    pub fn strp(self) -> bool {
        self.get_type() == Type::Str
    }

    pub fn make_string(thread: &mut Thread, s: impl AsRef<str>) -> Value {
        Str::new(thread, s)
    }

    pub fn downcast_str(self) -> Handle<Str> {
        cassert!(self.strp());

        unsafe { std::mem::transmute(self.0 as usize) }
    }

    pub fn strsym<'a>(self) -> &'a str {
        cassert!(self.strp() || self.symbolp());
        if self.strp() {
            self.str()
        } else {
            self.symbol_str()
        }
    }

    pub fn str<'a>(self) -> &'a str {
        cassert!(self.strp());
        unsafe {
            let ptr = self.0 as *const Str;
            (*ptr).str()
        }
    }

    pub fn str_mut<'a>(self) -> &'a mut String {
        cassert!(self.strp());
        unsafe {
            let ptr = self.0 as *mut Str;
            (*ptr).str_mut()
        }
    }

    pub fn str_len(self) -> usize {
        cassert!(self.strp());
        unsafe {
            let ptr = self.0 as *const Str;
            (*ptr).len()
        }
    }

    pub fn make_str(thread: &mut Thread, s: impl AsRef<str>) -> Value {
        Str::new(thread, s)
    }
}

#[repr(C)]
pub struct Vector {
    hdr: Hdr,
    len: u32,
    pad: [u8; 3],
    data: [Value; 0],
}

impl Object for Vector {
    fn trace_range(&self, from: usize, to: usize, visitor: &mut dyn Visitor) {
        for i in from..to {
            unsafe {
                (*self.data.as_ptr().add(i)).trace(visitor);
            }
        }
    }
}

impl Allocation for Vector {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = size_of::<Value>();
    const VARSIZE_NO_HEAP_PTRS: bool = false;
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(Vector, len);
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(Vector, len);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(Vector, data);
}

impl Vector {
    pub fn new(thread: &mut Thread, len: u32, init: Value) -> Value {
        unsafe {
            let mut this = thread.allocate_varsize::<Self>(len as _);

            let this_mut = this.assume_init_mut();
            this_mut.hdr = Hdr::new(Type::Vector);
            this_mut.len = len;

            for i in 0..len {
                if init.get_type() > Type::Integer {
                    thread.write_barrier(init.handle());
                }

                this_mut.data.as_mut_ptr().add(i as _).write(init.clone());
            }

            Value::encode_ptr(this.as_ptr())
        }
    }

    pub fn at(&self, idx: usize) -> Value {
        unsafe { self.data.as_ptr().add(idx).read() }
    }

    pub fn set(&mut self, idx: usize, val: Value) {
        unsafe {
            *self.data.as_mut_ptr().add(idx) = val;
        }
    }

    pub fn as_slice(&self) -> &[Value] {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr(), self.len as usize) }
    }

    pub fn as_slice_mut(&mut self) -> &mut [Value] {
        unsafe { std::slice::from_raw_parts_mut(self.data.as_mut_ptr(), self.len as usize) }
    }

    pub fn len_offset() -> usize {
        offsetof!(Self, len)
    }

    pub fn data_offset() -> usize {
        offsetof!(Self, data)
    }
}

impl Value {
    pub fn valuesp(self) -> bool {
        self.get_type() == Type::Values
    }

    pub fn values_len(self) -> usize {
        cassert!(self.valuesp());
        unsafe {
            let ptr = self.0 as *const Vector;
            (*ptr).len as usize
        }
    }

    pub fn values_ref(self, idx: usize) -> Value {
        cassert!(self.valuesp());
        unsafe {
            let ptr = self.0 as *const Vector;
            (*ptr).at(idx)
        }
    }

    pub fn values_set(self, idx: usize, val: Value) {
        cassert!(self.valuesp());
        unsafe {
            let ptr = self.0 as *mut Vector;
            (*ptr).set(idx, val);
        }
    }

    pub fn make_values_n(thread: &mut Thread, len: usize, init: Value) -> Value {
        unsafe {
            let mut this = thread.allocate_varsize::<Vector>(len as _);

            let this_mut = this.assume_init_mut();
            this_mut.hdr = Hdr::new(Type::Values);
            this_mut.len = len as _;

            for i in 0..len {
                if init.get_type() > Type::Integer {
                    thread.write_barrier(init.handle());
                }
                this_mut.data.as_mut_ptr().add(i as _).write(init.clone());
            }

            Value::encode_ptr(this.as_ptr())
        }
    }

    pub fn make_values(thread: &mut Thread, init: &[Value]) -> Value {
        let len = init.len() as u32;
        unsafe {
            let mut this = thread.allocate_varsize::<Vector>(len as _);

            let this_mut = this.assume_init_mut();
            this_mut.hdr = Hdr::new(Type::Values);
            this_mut.len = len;

            for i in 0..len {
                let val = init[i as usize];
                if val.get_type() > Type::Integer {
                    thread.write_barrier(val.handle());
                }
                this_mut.data.as_mut_ptr().add(i as _).write(val.clone());
            }

            Value::encode_ptr(this.as_ptr())
        }
    }

    pub fn make_values_from_list(thread: &mut Thread, list: Value) -> Value {
        let mut len = 0;
        let mut ptr = list;
        while ptr.pairp() {
            len += 1;
            ptr = ptr.cdr();
        }

        unsafe {
            let mut this = thread.allocate_varsize::<Vector>(len as _);

            let this_mut = this.assume_init_mut();
            this_mut.hdr = Hdr::new(Type::Values);
            this_mut.len = len;

            ptr = list;
            for i in 0..len {
                let val = ptr.car();
                if val.get_type() > Type::Integer {
                    thread.write_barrier(val.handle());
                }
                this_mut.data.as_mut_ptr().add(i as _).write(val.clone());
                ptr = ptr.cdr();
            }

            Value::encode_ptr(this.as_ptr())
        }
    }

    pub fn values_to_list(self, thread: &mut Thread) -> Value {
        cassert!(self.valuesp());
        unsafe {
            let ptr = self.0 as *const Vector;
            let len = (*ptr).len as usize;
            let data = (*ptr).data.as_ptr();
            let mut list = Value::make_null();
            for i in (0..len).rev() {
                list = Value::cons(thread, *data.add(i), list);
            }
            list
        }
    }

    pub fn vectorp(self) -> bool {
        self.get_type() == Type::Vector
    }

    pub fn vector_len(self) -> usize {
        cassert!(self.vectorp());
        unsafe {
            let ptr = self.0 as *const Vector;
            (*ptr).len as usize
        }
    }

    pub fn vector_ref(self, idx: usize) -> Value {
        cassert!(self.vectorp());
        unsafe {
            let ptr = self.0 as *const Vector;
            (*ptr).at(idx)
        }
    }

    pub fn vector_at(self, idx: usize) -> Value {
        cassert!(self.vectorp());
        unsafe {
            let ptr = self.0 as *const Vector;
            (*ptr).at(idx)
        }
    }

    pub fn vector_set(self, idx: usize, val: Value) {
        cassert!(self.vectorp());
        unsafe {
            let ptr = self.0 as *mut Vector;
            (*ptr).set(idx, val);
        }
    }

    pub fn vector_as_slice<'a>(self) -> &'a [Value] {
        cassert!(self.vectorp());
        unsafe {
            let ptr = self.0 as *const Vector;
            (*ptr).as_slice()
        }
    }

    pub fn vector_as_slice_mut<'a>(self) -> &'a mut [Value] {
        cassert!(self.vectorp());
        unsafe {
            let ptr = self.0 as *mut Vector;
            (*ptr).as_slice_mut()
        }
    }

    pub fn make_vector(thread: &mut Thread, len: u32, init: Value) -> Value {
        if len == 0 {
            return Runtime::get().empty_vector;
        }
        Vector::new(thread, len, init)
    }

    pub fn downcast_vector<'a>(self) -> Handle<Vector> {
        cassert!(self.vectorp());
        unsafe { Handle::from_raw(self.0 as *mut u8) }
    }
}

#[repr(C)]
pub struct ByteVector {
    hdr: Hdr,
    len: u32,
    pad: [u8; 3],
    data: [u8; 0],
}

impl Object for ByteVector {}
impl Allocation for ByteVector {
    const VARSIZE: bool = true;
    const VARSIZE_ITEM_SIZE: usize = size_of::<u8>();
    const VARSIZE_NO_HEAP_PTRS: bool = true;
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(ByteVector, len);
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(ByteVector, len);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(ByteVector, data);
}

impl ByteVector {
    pub fn new(thread: &mut Thread, len: u32, init: u8) -> Value {
        unsafe {
            let mut this = thread.allocate_varsize::<Self>(len as _);

            let this_mut = this.assume_init_mut();
            this_mut.hdr = Hdr::new(Type::ByteVector);
            this_mut.len = len;

            for i in 0..len {
                this_mut.data.as_mut_ptr().add(i as _).write(init);
            }

            Value::encode_ptr(this.as_ptr())
        }
    }

    pub fn at(&self, idx: usize) -> u8 {
        unsafe { *self.data.as_ptr().add(idx) }
    }

    pub fn set(&mut self, idx: usize, val: u8) {
        unsafe {
            *self.data.as_mut_ptr().add(idx) = val;
        }
    }

    pub fn as_slice(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr(), self.len as usize) }
    }

    pub fn as_slice_mut(&mut self) -> &mut [u8] {
        unsafe { std::slice::from_raw_parts_mut(self.data.as_mut_ptr(), self.len as usize) }
    }
}

impl Value {
    pub fn byte_vectorp(self) -> bool {
        self.get_type() == Type::ByteVector
    }

    pub fn byte_vector_len(self) -> usize {
        cassert!(self.byte_vectorp());
        unsafe {
            let ptr = self.0 as *const ByteVector;
            (*ptr).len as usize
        }
    }

    pub fn byte_vector_ref(self, idx: usize) -> u8 {
        cassert!(self.byte_vectorp());
        unsafe {
            let ptr = self.0 as *const ByteVector;
            (*ptr).at(idx)
        }
    }

    pub fn byte_vector_at(self, idx: usize) -> u8 {
        cassert!(self.byte_vectorp());
        unsafe {
            let ptr = self.0 as *const ByteVector;
            (*ptr).at(idx)
        }
    }

    pub fn byte_vector_set(self, idx: usize, val: u8) {
        cassert!(self.byte_vectorp());
        unsafe {
            let ptr = self.0 as *mut ByteVector;
            (*ptr).set(idx, val);
        }
    }

    pub fn byte_vector_as_slice<'a>(self) -> &'a [u8] {
        cassert!(self.byte_vectorp());
        unsafe {
            let ptr = self.0 as *const ByteVector;
            (*ptr).as_slice()
        }
    }

    pub fn byte_vector_as_slice_mut<'a>(self) -> &'a mut [u8] {
        cassert!(self.byte_vectorp());
        unsafe {
            let ptr = self.0 as *mut ByteVector;
            (*ptr).as_slice_mut()
        }
    }

    pub fn make_byte_vector(thread: &mut Thread, len: u32, init: u8) -> Value {
        ByteVector::new(thread, len, init)
    }

    pub fn make_byte_vector_from(thread: &mut Thread, init: impl AsRef<[u8]>) -> Value {
        let init = init.as_ref();
        let len = init.len() as u32;
        let bv = ByteVector::new(thread, len, 0);

        bv.byte_vector_as_slice_mut().copy_from_slice(init);

        bv
    }

    pub fn downcast_byte_vector<'a>(self) -> Handle<ByteVector> {
        cassert!(self.byte_vectorp());
        unsafe { Handle::from_raw(self.0 as *mut u8) }
    }
}

/// Lazily initialized cache of ASCII characters.
///
/// Used to reduce the number of allocations for frequent characters.
pub static CHAR_CACHE: once_cell::sync::Lazy<[Value; 256]> = once_cell::sync::Lazy::new(|| {
    let mut chars = [Value::make_undef(); 256];
    let thread = Thread::current();
    for i in 0..256 {
        chars[i] = Value::make_char(thread, i as u8 as char);
    }

    rsgc::heap::heap::heap().add_root(SimpleRoot::new("char cache", "char-cache", |processor| {
        for i in 0..256 {
            CHAR_CACHE[i].trace(processor.visitor());
        }
    }));
    chars
});

#[repr(C)]
pub struct Symbol {
    hdr: Hdr,
    len: u32,
    pub(crate) generated: bool,
    pub(crate) uninterned: bool,
    data: [u8; 0],
}

impl Object for Symbol {}
impl Allocation for Symbol {
    const NO_HEAP_PTRS: bool = true;
    const VARSIZE: bool = true;
    const VARSIZE_NO_HEAP_PTRS: bool = true;
    const VARSIZE_ITEM_SIZE: usize = 1;
    const VARSIZE_OFFSETOF_CAPACITY: usize = offset_of!(Symbol, len);
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(Symbol, len);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(Symbol, data);
}

impl Symbol {
    pub fn new(thread: &mut Thread, s: impl AsRef<str>) -> Value {
        unsafe {
            let s = s.as_ref();
            let mut this = thread.allocate_varsize::<Self>(s.len() as _);

            let this_mut = this.assume_init_mut();
            this_mut.hdr = Hdr::new(Type::Symbol);
            this_mut.len = s.len() as _;
            this_mut.uninterned = true;

            for (i, c) in s.bytes().enumerate() {
                *this_mut.data.as_mut_ptr().add(i) = c;
            }

            Value::encode_ptr(this.as_ptr())
        }
    }

    pub fn as_str(&self) -> &str {
        unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                self.data.as_ptr(),
                self.len as usize,
            ))
        }
    }

    pub fn len(&self) -> usize {
        self.len as usize
    }
}

impl Value {
    pub fn symbolp(self) -> bool {
        self.get_type() == Type::Symbol
    }

    pub fn symbol_len(self) -> usize {
        cassert!(self.symbolp());
        unsafe {
            let ptr = self.0 as *const Symbol;
            (*ptr).len() as usize
        }
    }

    pub fn symbol_str<'a>(self) -> &'a str {
        cassert!(self.symbolp());
        unsafe {
            let ptr = self.0 as *const Symbol;
            (*ptr).as_str()
        }
    }

    pub fn make_uninterned_symbol(thread: &mut Thread, s: impl AsRef<str>) -> Value {
        let this = Symbol::new(thread, s);
        this.downcast_symbol().generated = true;
        this
    }

    pub fn downcast_symbol<'a>(self) -> Handle<Symbol> {
        cassert!(self.symbolp());
        unsafe { Handle::from_raw(self.0 as *mut u8) }
    }
}

declare_typed!(Identifier : Identifier {
    pub name: Value,
    pub module: Value,
    pub frames: Value
});

declare_typed!(Macro : Macro {
    pub name: Value,
    pub transformer: Value,

    pub identifierp: bool /* Macro xformer is called even when
                                      the macro appears in non-head position */
});

declare_typed!(StructType : StructureType {
    pub(crate) num_slots: i32,
    pub(crate) num_islots: i32,
    pub(crate) name_pos: i32,
    pub(crate) name: Value,
    pub(crate) accessor: Value,
    pub(crate) mutator: Value,
    pub(crate) uninit_val: Value,
    pub(crate) props: Handle<Array<Value>>,
    pub(crate) guard: Value,
    pub(crate) parent_types: Handle<Array<Value>>
});

declare_typed!(StructProperty : StructureProperty {
    pub(crate) name: Value,
    pub(crate) guard: Value,
    pub(crate) supers: Value,
    pub(crate) contract_name: Value
});

declare_typed!(Structure : Struct {
    pub(crate) stype: Handle<StructType>,
    pub(crate) slots: Handle<Array<Value>>
});

declare_typed!(NativeProcedure : NativeProcedure {
    pub(crate) code: usize, // extern "C" fn(&mut Vm, Value, Value, *const Value, usize, &mut u8) -> Value,
    pub(crate) mina: i32,
    pub(crate) maxa: i32,
    // Scheme vector or null
    pub(crate) captures: Value
});

#[repr(C)]
pub struct PrimitiveProcedure {
    pub(crate) hdr: Hdr,
    pub(crate) name: Value,
    pub(crate) mina: i32,
    pub(crate) maxa: i32,
    pub(crate) code: fn(&mut Vm, Value, &[Value]) -> Trampoline,
}

impl Object for PrimitiveProcedure {
    fn trace(&self, visitor: &mut dyn Visitor) {
        self.name.trace(visitor);
    }
}

impl Allocation for PrimitiveProcedure {}

#[repr(C)]
pub struct ClosedPrimitiveProcedure {
    pub(crate) hdr: Hdr,
    pub(crate) name: Value,
    pub(crate) mina: i32,
    pub(crate) maxa: i32,
    pub(crate) code: fn(&mut Vm, Value, &[Value], Handle<Array<Value>>, &str) -> Trampoline,
    pub(crate) captures: Handle<Array<Value>>,
}

impl Object for ClosedPrimitiveProcedure {
    fn trace(&self, visitor: &mut dyn Visitor) {
        self.name.trace(visitor);
        self.captures.trace(visitor);
    }
}

impl Allocation for ClosedPrimitiveProcedure {}

impl Value {
    pub fn procedurep(self) -> bool {
        let t = self.get_type();

        t >= Type::PrimitiveProcedure && t <= Type::NativeProcedure
    }

    pub fn primitive_procedurep(self) -> bool {
        let t = self.get_type();

        t == Type::PrimitiveProcedure
    }

    pub fn closed_primitive_procedurep(self) -> bool {
        let t = self.get_type();

        t == Type::ClosedPrimitiveProcedure
    }

    pub fn downcast_primitive_proc(self) -> Handle<PrimitiveProcedure> {
        cassert!(self.get_type() == Type::PrimitiveProcedure);
        unsafe { Handle::from_raw(self.0 as *mut u8) }
    }

    pub fn downcast_closed_primitive_proc(self) -> Handle<ClosedPrimitiveProcedure> {
        cassert!(self.get_type() == Type::ClosedPrimitiveProcedure);
        unsafe { Handle::from_raw(self.0 as *mut u8) }
    }

    pub fn downcast_native_proc(self) -> Handle<NativeProcedure> {
        cassert!(self.get_type() == Type::NativeProcedure);
        unsafe { Handle::from_raw(self.0 as *mut u8) }
    }

    pub fn exact_integerp(self) -> bool {
        self.intp() || self.get_type() == Type::Bignum
    }

    pub fn numberp(self) -> bool {
        self.intp() || self.get_type() == Type::Bignum || self.get_type() == Type::Double
    }

    pub fn is_true(self) -> bool {
        !self.falsep()
    }

    pub fn is_exact_integer(self) -> bool {
        self.intp() || self.get_type() == Type::Bignum
    }

    pub fn is_nonnegative_exact_smallint(self) -> bool {
        self.intp() && self.is_nonnegative()
    }

    pub fn is_nonnegative(self) -> bool {
        self.intp() && self.int() >= 0
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        {
            let port = Port::new(Thread::current());
            port_open_bytevector(
                port,
                intern("Debug"),
                SCM_PORT_DIRECTION_OUT,
                Value::make_false(),
                Value::make_false(),
            );
            let mut printer = Printer::new(crate::vm::vm(), port);
            match printer.write(*self) {
                Ok(_) => match printer.flush() {
                    Ok(_) => write!(f, "{}", port_extract_string(port).unwrap().str()),
                    Err(_) => write!(f, "Error flushing port"),
                },
                Err(_) => write!(f, "Error writing to port"),
            }
        }
    }
}

#[repr(C)]
pub struct Gloc {
    pub(crate) hdr: Hdr,
    pub(crate) module: Value,
    pub(crate) name: Value,
    pub(crate) value: Value,
    pub(crate) get: Option<fn(&mut Vm, Handle<Self>) -> Result<Value, Value>>,
    pub(crate) set: Option<fn(&mut Vm, Handle<Self>, Value) -> Result<(), Value>>,
}

impl Gloc {
    pub(crate) fn new(name: Value, module: Value) -> Self {
        Self {
            hdr: Hdr::new(Type::Gloc),
            name,
            module,
            value: Value::make_undef(),
            get: None,
            set: None,
        }
    }
}

impl Value {
    pub fn glocp(self) -> bool {
        self.get_type() == Type::Gloc
    }

    pub fn downcast_gloc(self) -> Handle<Gloc> {
        cassert!(self.get_type() == Type::Gloc);
        unsafe { Handle::from_raw(self.0 as *mut u8) }
    }
}

impl Object for Gloc {
    fn trace(&self, visitor: &mut dyn Visitor) {
        self.name.trace(visitor);
        self.value.trace(visitor);
        self.module.trace(visitor);
    }
}

impl Allocation for Gloc {}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

declare_typed! { Parameter : Parameter {
    pub value: Value,
    pub guard: Value
} }

declare_typed! { ReturnCont : ReturnCont {}}

declare_typed! { Boxed : Boxed {
    pub value: Value
} }
