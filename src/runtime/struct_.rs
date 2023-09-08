/* Structs are sequences of words where the first word points to the
  struct's vtable, and the rest are its slots.  The vtable indicates
  how many words are in the struct among other meta-information.  A
  vtable is itself a struct and as such has a vtable, and so on until
  you get to a root struct that is its own vtable.

    .--------+----------------- -
    | vtable | slot0 | slot1 |
    `--------+----------------- -
        |
        |
    .---v----+----------------- -
    | vtable | slot0 | slot1 |
    `--------+----------------- -
        |
       ...
        |
    .---v----+----------------- -
  .-| vtable | slot0 | slot1 |
  | `--------+----------------- -
  |     ^
  `-----'
*/

use std::mem::size_of;

use mmtk::{
    memory_manager::object_reference_write,
    util::{Address, ObjectReference},
    vm::{EdgeVisitor, RootsWorkFactory},
    AllocationSemantics, MutatorContext,
};

use crate::{
    gc::ObjEdge,
    raise_exn,
    runtime::object::{scm_symbol_str, ScmCellRef},
    vm::{scm_virtual_machine, thread::Thread},
};

use super::{
    arith::scm_to_usize,
    environment::scm_define,
    gsubr::{scm_define_subr, Subr},
    list::scm_length,
    object::{scm_car, scm_cdr, scm_string_str, ScmCellHeader, TypeId},
    symbol::scm_intern,
    value::{EncodedValueDescriptor, Value},
};

pub const SCM_VTABLE_BASE_LAYOUT: &str = "pwuhuhpwphuhuhuh";

pub const SCM_VTABLE_INDEX_LAYOUT: usize = 0;
pub const SCM_VTABLE_INDEX_FLAGS: usize = 1;
pub const SCM_VTABLE_INDEX_INSTANCE_FINALIZE: usize = 2;
pub const SCM_VTABLE_INDEX_INSTANCE_PRINTER: usize = 3;
pub const SCM_VTABLE_INDEX_NAME: usize = 4;
pub const SCM_VTABLE_INDEX_SIZE: usize = 5;
pub const SCM_VTABLE_INDEX_UNBOXED_FIELDS: usize = 6;
pub const SCM_VTABLE_INDEX_RESERVED_7: usize = 7;
pub const SCM_VTABLE_OFFSET_USER: usize = 8;

pub const SCM_VTABLE_FLAG_VALIDATED: u64 = 1 << 0;
pub const SCM_VTABLE_FLAG_VTABLE: u64 = 1 << 1;
pub const SCM_VTABLE_FLAG_APPLICABLE_VTABLE: u64 = 1 << 2;
pub const SCM_VTABLE_FLAG_APPLICABLE: u64 = 1 << 3;
pub const SCM_VTABLE_FLAG_SETTER_VTABLE: u64 = 1 << 4;
pub const SCM_VTABLE_FLAG_SETTER: u64 = 1 << 5;
pub const SCM_VTABLE_FLAG_RESERVED_0: u64 = 1 << 6;
pub const SCM_VTABLE_FLAG_RESERVED_1: u64 = 1 << 7;
pub const SCM_VTABLE_SMOB_0: u64 = 1 << 8;
pub const SCM_VTABLE_GOOPS_0: u64 = 1 << 9;
pub const SCM_VTABLE_GOOPS_1: u64 = 1 << 10;
pub const SCM_VTABLE_GOOPS_2: u64 = 1 << 11;
pub const SCM_VTABLE_GOOPS_3: u64 = 1 << 12;
pub const SCM_VTABLE_GOOPS_4: u64 = 1 << 13;
pub const SCM_VTABLE_FLAG_RESERVED_2: u64 = 1 << 14;
pub const SCM_VTABLE_FLAG_RESERVED_3: u64 = 1 << 15;
pub const SCM_VTABLE_USER_FLAG_SHIFT: u64 = 16;

impl Value {
    pub fn is_struct(&self) -> bool {
        self.type_of() == TypeId::Struct
    }

    pub fn is_struct_vtable(&self) -> bool {
        if !self.is_struct() {
            return false;
        }

        self.cast_as::<ScmStruct>()
            .vtable
            .cast_as::<ScmStruct>()
            .vtable_flag_is_set(SCM_VTABLE_FLAG_VTABLE)
    }
}

#[repr(C)]
pub struct ScmStruct {
    pub(crate) header: ScmCellHeader,
    pub(crate) vtable: Value,
    pub(crate) slots: [Value; 0],
}

impl Drop for ScmStruct {
    fn drop(&mut self) {
        if self.vtable_flag_is_set(SCM_VTABLE_FLAG_VTABLE) {
            mmtk::memory_manager::free(Address::from_ptr(self.vtable_unboxed_fields()));
        } else {
            let fin = self
                .vtable
                .cast_as::<ScmStruct>()
                .vtable_instance_finalizer();
            let fin: fn(*mut ()) = unsafe { std::mem::transmute(fin) };
            fin(self as *mut _ as _);
        }
    }
}

impl ScmStruct {
    pub fn slots(&self) -> *mut Value {
        self.slots.as_ptr() as _
    }

    pub fn data(&self) -> *mut u64 {
        self.slots().cast()
    }

    pub fn slot_ref(&self, index: usize) -> Value {
        unsafe { *self.slots().add(index) }
    }

    pub fn slot_set(&mut self, thread: &mut Thread, index: usize, val: Value) {
        unsafe {
            if val.is_object() {
                let slot =
                    ObjEdge::from_address_unchecked(Address::from_mut_ptr(self.slots().add(index)));
                object_reference_write(
                    thread.mutator(),
                    ObjectReference::from_raw_address(Address::from_ptr(self)),
                    slot,
                    ObjectReference::from_raw_address(val.get_object().to_address()),
                );
            } else {
                self.slots().add(index).write(val);
            }
        }
    }

    pub fn data_ref(&self, index: usize) -> u64 {
        unsafe { *self.data().add(index) }
    }

    pub fn data_set(&mut self, index: usize, val: u64) {
        unsafe {
            self.data().add(index).write(val);
        }
    }

    pub fn vtable_layout(&self) -> Value {
        self.slot_ref(SCM_VTABLE_INDEX_LAYOUT)
    }

    pub fn set_vtable_layout(&mut self, thread: &mut Thread, layout: Value) {
        self.slot_set(thread, SCM_VTABLE_INDEX_LAYOUT, layout);
    }

    pub fn vtable_flags(&self) -> u64 {
        self.data_ref(SCM_VTABLE_INDEX_FLAGS)
    }

    pub fn set_vtable_flags(&mut self, flags: u64) {
        unsafe {
            *self.data().add(SCM_VTABLE_INDEX_FLAGS) |= flags;
        }
    }

    pub fn vtable_flag_is_set(&self, flag: u64) -> bool {
        (self.vtable_flags() & flag) != 0
    }

    pub fn vtable_instance_finalizer(&self) -> *const u8 {
        self.data_ref(SCM_VTABLE_INDEX_INSTANCE_FINALIZE) as _
    }

    pub fn set_vtable_instance_finalizer(&mut self, finalizer: *const u8) {
        self.data_set(SCM_VTABLE_INDEX_INSTANCE_FINALIZE, finalizer as _);
    }

    pub fn vtable_instance_printer(&self) -> *const u8 {
        self.data_ref(SCM_VTABLE_INDEX_INSTANCE_PRINTER) as _
    }

    pub fn set_vtable_instance_printer(&mut self, printer: *const u8) {
        self.data_set(SCM_VTABLE_INDEX_INSTANCE_PRINTER, printer as _);
    }

    pub fn vtable_name(&self) -> Value {
        self.slot_ref(SCM_VTABLE_INDEX_NAME)
    }

    pub fn set_vtable_name(&mut self, thread: &mut Thread, name: Value) {
        self.slot_set(thread, SCM_VTABLE_INDEX_NAME, name);
    }

    pub fn vtable_size(&self) -> u64 {
        self.data_ref(SCM_VTABLE_INDEX_SIZE)
    }

    pub fn set_vtable_size(&mut self, size: u64) {
        self.data_set(SCM_VTABLE_INDEX_SIZE, size);
    }

    pub fn vtable_unboxed_fields(&self) -> *mut u32 {
        self.data_ref(SCM_VTABLE_INDEX_UNBOXED_FIELDS) as _
    }

    pub fn set_vtable_unboxed_fields(&mut self, unboxed_fields: *mut u32) {
        self.data_set(SCM_VTABLE_INDEX_UNBOXED_FIELDS, unboxed_fields as _);
    }

    pub fn vtable_field_is_unboxed(&mut self, field: usize) -> bool {
        if self.vtable_unboxed_fields().is_null() {
            return false;
        }

        unsafe {
            let fields = self.vtable_unboxed_fields();

            let slot = fields.add(field >> 5).read();
            let bit = slot & (1 << (field & 31));
            bit != 0
        }
    }

    pub(crate) fn visit_edges<EV: EdgeVisitor<ObjEdge>>(&mut self, visitor: &mut EV) {
        let vt = self.vtable.cast_as::<ScmStruct>();
        self.vtable.visit_edge(visitor);
        for field in 0..vt.vtable_size() as usize {
            if vt.vtable_field_is_unboxed(field) {
                continue;
            }

            unsafe {
                (*self.slots().add(field)).visit_edge(visitor);
            }
        }
    }
}

pub fn scm_struct_vtable(s: Value) -> Value {
    s.cast_as::<ScmStruct>().vtable
}

pub fn scm_struct_layout(s: Value) -> Value {
    scm_struct_vtable(s).cast_as::<ScmStruct>().vtable_layout()
}

pub fn scm_struct_name(s: Value) -> Value {
    scm_struct_vtable(s).cast_as::<ScmStruct>().vtable_name()
}

pub fn scm_struct_size(s: Value) -> u64 {
    scm_struct_vtable(s).cast_as::<ScmStruct>().vtable_size()
}

pub fn scm_struct_instance_finalizer(s: Value) -> *const u8 {
    scm_struct_vtable(s)
        .cast_as::<ScmStruct>()
        .vtable_instance_finalizer()
}

pub fn scm_struct_instance_printer(s: Value) -> *const u8 {
    scm_struct_vtable(s)
        .cast_as::<ScmStruct>()
        .vtable_instance_printer()
}

pub fn scm_struct_field_is_unboxed(s: Value, field: usize) -> bool {
    scm_struct_vtable(s)
        .cast_as::<ScmStruct>()
        .vtable_field_is_unboxed(field)
}

pub fn scm_struct_vtable_flags(s: Value) -> u64 {
    scm_struct_vtable(s).cast_as::<ScmStruct>().vtable_flags()
}

pub fn scm_struct_vtable_flag_is_set(s: Value, flag: u64) -> bool {
    scm_struct_vtable(s)
        .cast_as::<ScmStruct>()
        .vtable_flag_is_set(flag)
}

pub fn make_struct_layout(layout: &str) -> Value {
    let len = layout.len();

    if len % 2 != 0 {
        raise_exn!(Fail, &[], "odd length specification: {}", layout);
    }

    let mut x = 0;

    while x < layout.len() {
        let c = layout.as_bytes()[x];
        match c {
            b'u' | b'p' => {}
            _ => {
                raise_exn!(Fail, &[], "unrecognized field type: {}", c as char);
            }
        }
        let c = layout.as_bytes()[x + 1];
        match c {
            b'w' => {}
            b'h' => {}
            b'r' => {}
            _ => {
                raise_exn!(Fail, &[], "unrecognized ref specification: {}", c as char);
            }
        }
        x += 2;
    }

    scm_intern(layout)
}

extern "C-unwind" fn make_struct_layout_proc(_thread: &mut Thread, val: &mut Value) -> Value {
    if !val.is_string() {
        raise_exn!(
            FailContract,
            &[],
            "make-struct-layout: expected string, got {}",
            val
        );
    }

    make_struct_layout(scm_string_str(*val))
}

fn set_vtable_access_fields(vtable: Value) {
    fn free_unboxed_fields(vtable: *mut ()) {
        unsafe {
            let structure = vtable.cast::<ScmStruct>();
            let unboxed_fields = (*structure).vtable_unboxed_fields();
            if !unboxed_fields.is_null() {
                mmtk::memory_manager::free(Address::from_ptr(unboxed_fields));
            }
        }
    }
    let layout = vtable.cast_as::<ScmStruct>().vtable_layout();
    let c_layout = scm_symbol_str(layout);
    let len = c_layout.len();

    assert_eq!(len % 2, 0);
    let nfields = len / 2;
    let bitmask_size = (nfields + 31) / 32;
    let unboxed_fields =
        mmtk::memory_manager::malloc(bitmask_size * size_of::<u32>()).to_mut_ptr::<u32>();
    unsafe {
        unboxed_fields.write_bytes(0, bitmask_size);

        for field in 0..nfields {
            if c_layout.as_bytes()[field * 2] == b'u' {
                *unboxed_fields.add(field / 32) |= 1 << (field % 32);
            }
        }

        vtable.cast_as::<ScmStruct>().set_vtable_flags(0);
        vtable
            .cast_as::<ScmStruct>()
            .data_set(SCM_VTABLE_INDEX_SIZE, len as u64 / 2);
        vtable
            .cast_as::<ScmStruct>()
            .set_vtable_unboxed_fields(unboxed_fields);

        Thread::current().register_cleaner(
            vtable.get_object().object_reference(),
            crate::runtime::object::CleanerType::Drop(free_unboxed_fields),
        );
    }
}

fn is_valid_vtable_layout(layout: Value) -> bool {
    let c_layout = scm_symbol_str(layout);
    let len = c_layout.len();

    if len % 2 != 0 {
        return false;
    }

    let mut n = 0;
    while n < len {
        match c_layout.as_bytes()[n] {
            b'u' | b'p' => match c_layout.as_bytes()[n + 1] {
                b'w' | b'h' => {}
                _ => {
                    return false;
                }
            },
            _ => {
                return false;
            }
        }

        n += 2;
    }

    true
}

pub fn scm_i_struct_inherit_vtable_magic(vtable: Value, obj: Value) {
    let olayout;

    if !is_valid_vtable_layout(obj.cast_as::<ScmStruct>().vtable_layout()) {
        raise_exn!(
            Fail,
            &[],
            "invalid layout for new vtable: {}",
            obj.cast_as::<ScmStruct>().vtable_layout()
        );
    }

    set_vtable_access_fields(obj);

    olayout = scm_symbol_str(obj.cast_as::<ScmStruct>().vtable_layout());
    if olayout.len() <= SCM_VTABLE_BASE_LAYOUT.len() && olayout == SCM_VTABLE_BASE_LAYOUT {
        obj.cast_as::<ScmStruct>()
            .set_vtable_flags(SCM_VTABLE_FLAG_VTABLE);
    }

    if vtable
        .cast_as::<ScmStruct>()
        .vtable_flag_is_set(SCM_VTABLE_FLAG_SETTER_VTABLE)
    {}

    vtable
        .cast_as::<ScmStruct>()
        .set_vtable_flags(SCM_VTABLE_FLAG_VALIDATED);
}

fn struct_init(handle: Value, layout: Value, n_inits: usize, inits: &[Value]) {
    let n_fields = handle
        .cast_as::<ScmStruct>()
        .vtable
        .cast_as::<ScmStruct>()
        .vtable_size() as usize;
    let mut inits_idx = 0;
    for n in 0..n_fields {
        if inits_idx == n_inits || scm_symbol_str(layout).as_bytes()[n * 2 + 1] == b'h' {
            if handle
                .cast_as::<ScmStruct>()
                .vtable
                .cast_as::<ScmStruct>()
                .vtable_field_is_unboxed(n)
            {
                handle.cast_as::<ScmStruct>().data_set(n, 0);
            } else {
                handle.cast_as::<ScmStruct>().slot_set(
                    &mut Thread::current(),
                    n,
                    Value::encode_bool_value(false),
                );
            }
        } else {
            if handle.cast_as::<ScmStruct>().vtable_field_is_unboxed(n) {
                handle
                    .cast_as::<ScmStruct>()
                    .data_set(n, inits[inits_idx].get_raw() as _);
            } else {
                handle
                    .cast_as::<ScmStruct>()
                    .slot_set(&mut Thread::current(), n, inits[inits_idx]);
            }
            inits_idx += 1;
        }
    }
}

extern "C-unwind" fn struct_p(_thread: &mut Thread, val: &mut Value) -> Value {
    Value::encode_bool_value(val.is_struct())
}

extern "C-unwind" fn struct_vtable_p(_thread: &mut Thread, val: &mut Value) -> Value {
    Value::encode_bool_value(val.is_struct_vtable())
}

fn alloc_struct(thread: &mut Thread, vtable_bits: u64, n_words: usize) -> Value {
    let n_words = n_words + 2; /* 1 for vtable, 1 for cell header */
    unsafe {
        let size = n_words * size_of::<Value>();
        let mem = thread
            .mutator()
            .alloc(size, 8, 0, AllocationSemantics::Default);
        mem.store(ScmStruct {
            header: ScmCellHeader::new(TypeId::Struct),
            vtable: Value(EncodedValueDescriptor {
                as_int64: vtable_bits as _,
            }),
            slots: [],
        });

        let reference = ObjectReference::from_raw_address(mem);
        thread
            .mutator()
            .post_alloc(reference, size, AllocationSemantics::Default);
        fn drop_struct(ptr: *mut ()) {
            unsafe {
                core::ptr::drop_in_place(ptr.cast::<ScmStruct>());
            }
        }
        if vtable_bits != 0
            && !Value(EncodedValueDescriptor {
                as_int64: vtable_bits as _,
            })
            .cast_as::<ScmStruct>()
            .vtable_instance_finalizer()
            .is_null()
        {
            thread.register_cleaner(reference, super::object::CleanerType::Drop(drop_struct));
        }

        let obj = Value::encode_object_value(ScmCellRef(mem.as_usize()));

        obj
    }
}

pub fn scm_make_struct(vtable: Value, init: &[Value]) -> Value {
    let basic_size = vtable.cast_as::<ScmStruct>().vtable_size();
    let vm = scm_virtual_machine();
    let thread = Thread::current();
    // FIXME: Figure out the way to root slices
    mmtk::memory_manager::disable_collection(&vm.mmtk);
    let obj = alloc_struct(thread, vtable.get_raw() as _, basic_size as usize);
    mmtk::memory_manager::enable_collection(&vm.mmtk);
    struct_init(
        obj,
        vtable.cast_as::<ScmStruct>().vtable_layout(),
        init.len(),
        init,
    );
    /* If we're making a vtable, validate its layout and inherit
    flags. However we allow for separation of allocation and
    initialization, to humor GOOPS, so only validate if the layout was
    passed as an initarg. */
    if vtable
        .cast_as::<ScmStruct>()
        .vtable_flag_is_set(SCM_VTABLE_FLAG_VTABLE)
        && !obj.cast_as::<ScmStruct>().vtable_layout().is_false()
    {
        scm_i_struct_inherit_vtable_magic(vtable, obj);
    }

    obj
}

extern "C-unwind" fn allocate_struct_proc(
    thread: &mut Thread,
    vtable: &mut Value,
    nfields: &mut Value,
) -> Value {
    if !vtable.is_struct_vtable() {
        raise_exn!(
            FailContract,
            &[],
            "allocate-struct: not a struct vtable: {}",
            vtable
        );
    }

    let c_nfields = scm_to_usize(*nfields);

    let ret = alloc_struct(thread, vtable.get_raw() as _, c_nfields);
    struct_init(ret, vtable.cast_as::<ScmStruct>().vtable_layout(), 0, &[]);

    ret
}

extern "C-unwind" fn make_struct_simple(
    thread: &mut Thread,
    vtable: &mut Value,
    init: &mut Value,
) -> Value {
    if !vtable.is_struct_vtable() {
        raise_exn!(
            FailContract,
            &[],
            "make-struct/simple: not a struct vtable: {}",
            vtable
        );
    }
    let ilength = scm_length(*init).unwrap();
    if ilength != vtable.cast_as::<ScmStruct>().vtable_size() as usize {
        raise_exn!(
            FailContract,
            &[],
            "make-struct/simple: wrong number of initializers: {}, expected: {}",
            ilength,
            vtable.cast_as::<ScmStruct>().vtable_size()
        );
    }

    let size = (vtable.cast_as::<ScmStruct>().vtable_size() as usize + 2) * size_of::<Value>();
    let mem = thread
        .mutator()
        .alloc(size, 8, 0, AllocationSemantics::Default);
    unsafe {
        mem.store(ScmStruct {
            header: ScmCellHeader::new(TypeId::Struct),
            vtable: *vtable,
            slots: [],
        });

        let reference = ObjectReference::from_raw_address(mem);
        thread
            .mutator()
            .post_alloc(reference, size, AllocationSemantics::Default);

        let obj = Value::encode_object_value(ScmCellRef(mem.as_usize()));
        for i in 0..ilength {
            let initv = scm_car(*init);
            obj.cast_as::<ScmStruct>().slot_set(thread, i, initv);
            *init = scm_cdr(*init);
        }

        obj
    }
}

fn scm_i_make_vtable_vtable(thread: &mut Thread, fields: Value) -> Value {
    if !fields.is_string() {
        raise_exn!(
            FailContract,
            &[],
            "make-vtable: expected string, got {}",
            fields
        );
    }

    let layout = make_struct_layout(scm_string_str(fields));
    if !is_valid_vtable_layout(layout) {
        raise_exn!(FailContract, &[], "make-vtable: invalid layout: {}", layout);
    }

    let nfields = scm_symbol_str(layout).len() / 2;
    let obj = alloc_struct(thread, 0, nfields as _);
    {
        obj.cast_as::<ScmStruct>().vtable = obj;
        obj.cast_as::<ScmStruct>().set_vtable_layout(thread, layout);
        obj.cast_as::<ScmStruct>()
            .set_vtable_flags(SCM_VTABLE_FLAG_VTABLE);

        set_vtable_access_fields(obj);
        let st = obj.cast_as::<ScmStruct>();
        st.data_set(SCM_VTABLE_INDEX_INSTANCE_FINALIZE, 0);
        st.data_set(SCM_VTABLE_INDEX_INSTANCE_PRINTER, 0);
        st.slot_set(
            thread,
            SCM_VTABLE_INDEX_NAME,
            Value::encode_bool_value(false),
        );
        for n in SCM_VTABLE_OFFSET_USER..nfields {
            if st.vtable_field_is_unboxed(n) {
                st.data_set(n, 0);
            } else {
                st.slot_set(thread, n, Value::encode_bool_value(false));
            }
        }

        assert!(obj.is_struct_vtable(), "wtf {}", obj);

        obj
    }
}

extern "C-unwind" fn make_vtable_proc(
    thread: &mut Thread,
    fields: &mut Value,
    printer: &mut Value,
) -> Value {
    if printer.is_undefined() {
        *printer = Value::encode_bool_value(false);
    }
    let vt = scm_virtual_machine().struct_globals.standard_vtable_vtable;
    scm_make_struct(vt, &[make_struct_layout_proc(thread, fields), *printer])
}

extern "C-unwind" fn struct_ref_proc(
    _thread: &mut Thread,
    handle: &mut Value,
    pos: &mut Value,
) -> Value {
    if !handle.is_struct() {
        raise_exn!(
            FailContract,
            &[],
            "struct-ref: expected struct, got {}",
            handle
        );
    }

    let nfields = handle
        .cast_as::<ScmStruct>()
        .vtable
        .cast_as::<ScmStruct>()
        .vtable_size();
    let p = scm_to_usize(*pos);

    if p >= nfields as usize {
        raise_exn!(FailContract, &[], "struct-ref: index out of bounds: {}", p);
    }

    handle.cast_as::<ScmStruct>().slot_ref(p)
}

extern "C-unwind" fn struct_set_proc(
    thread: &mut Thread,
    handle: &mut Value,
    pos: &mut Value,
    val: &mut Value,
) -> Value {
    if !handle.is_struct() {
        raise_exn!(
            FailContract,
            &[],
            "struct-set!: expected struct, got {}",
            handle
        );
    }

    let nfields = handle
        .cast_as::<ScmStruct>()
        .vtable
        .cast_as::<ScmStruct>()
        .vtable_size();
    let p = scm_to_usize(*pos);

    if p >= nfields as usize {
        raise_exn!(FailContract, &[], "struct-set!: index out of bounds: {}", p);
    }

    handle.cast_as::<ScmStruct>().slot_set(thread, p, *val);
    *val
}

extern "C-unwind" fn struct_vtable_proc(_thread: &mut Thread, handle: &mut Value) -> Value {
    if !handle.is_struct() {
        raise_exn!(
            FailContract,
            &[],
            "struct-vtable: expected struct, got {}",
            handle
        );
    }

    handle.cast_as::<ScmStruct>().vtable
}

pub struct StructGlobals {
    pub standard_vtable: Value,
    pub required_vtable_fields: Value,
    pub required_applicable_fields: Value,
    pub required_applicable_with_setter_fields: Value,
    pub standard_vtable_vtable: Value,
    pub applicable_struct_vtable_vtable: Value,
    pub applicable_struct_with_setter_vtable_vtable: Value,
}

impl Default for StructGlobals {
    fn default() -> Self {
        Self {
            standard_vtable: Value::encode_bool_value(false),
            required_vtable_fields: Value::encode_bool_value(false),
            required_applicable_fields: Value::encode_bool_value(false),
            required_applicable_with_setter_fields: Value::encode_bool_value(false),
            standard_vtable_vtable: Value::encode_bool_value(false),
            applicable_struct_vtable_vtable: Value::encode_bool_value(false),
            applicable_struct_with_setter_vtable_vtable: Value::encode_bool_value(false),
        }
    }
}

impl StructGlobals {
    pub(crate) fn scan_roots(&mut self, factory: &mut impl RootsWorkFactory<ObjEdge>) {
        let mut edges = vec![];

        if self.standard_vtable.is_object() {
            let edge = ObjEdge::from_address(Address::from_mut_ptr(&mut self.standard_vtable));
            edges.push(edge);
        }

        if self.required_vtable_fields.is_object() {
            let edge =
                ObjEdge::from_address(Address::from_mut_ptr(&mut self.required_vtable_fields));
            edges.push(edge);
        }

        if self.required_applicable_fields.is_object() {
            let edge =
                ObjEdge::from_address(Address::from_mut_ptr(&mut self.required_applicable_fields));
            edges.push(edge);
        }

        if self.required_applicable_with_setter_fields.is_object() {
            let edge = ObjEdge::from_address(Address::from_mut_ptr(
                &mut self.required_applicable_with_setter_fields,
            ));
            edges.push(edge);
        }

        if self.standard_vtable_vtable.is_object() {
            let edge =
                ObjEdge::from_address(Address::from_mut_ptr(&mut self.standard_vtable_vtable));
            edges.push(edge);
        }

        if self.applicable_struct_vtable_vtable.is_object() {
            let edge = ObjEdge::from_address(Address::from_mut_ptr(
                &mut self.applicable_struct_vtable_vtable,
            ));
            edges.push(edge);
        }

        if self.applicable_struct_with_setter_vtable_vtable.is_object() {
            let edge = ObjEdge::from_address(Address::from_mut_ptr(
                &mut self.applicable_struct_with_setter_vtable_vtable,
            ));
            edges.push(edge);
        }

        factory.create_process_edge_roots_work(edges);
    }
}

pub(crate) fn init() {
    let t = Thread::current();
    scm_virtual_machine().struct_globals.required_vtable_fields =
        t.make_string::<false>(SCM_VTABLE_BASE_LAYOUT);
    scm_virtual_machine().struct_globals.standard_vtable_vtable = scm_i_make_vtable_vtable(
        t,
        scm_virtual_machine().struct_globals.required_vtable_fields,
    );
    scm_define(
        scm_intern("standard-vtable-fields"),
        scm_virtual_machine().struct_globals.required_vtable_fields,
    );
    scm_define(
        scm_intern("<standard-vtable>"),
        scm_virtual_machine().struct_globals.standard_vtable_vtable,
    );

    scm_define_subr(
        "make-struct-layout",
        1,
        0,
        0,
        Subr::F1(make_struct_layout_proc),
    );
    scm_define_subr("struct?", 1, 0, 0, Subr::F1(struct_p));
    scm_define_subr("struct-vtable?", 1, 0, 0, Subr::F1(struct_vtable_p));
    scm_define_subr("allocate-struct", 2, 0, 0, Subr::F2(allocate_struct_proc));
    scm_define_subr("make-struct/simple", 1, 0, 1, Subr::F2(make_struct_simple));
    scm_define_subr("make-vtable", 1, 1, 0, Subr::F2(make_vtable_proc));
    scm_define_subr("struct-ref", 2, 0, 0, Subr::F2(struct_ref_proc));
    scm_define_subr("struct-set!", 3, 0, 0, Subr::F3(struct_set_proc));
    scm_define_subr("struct-vtable", 1, 0, 0, Subr::F1(struct_vtable_proc));
}
