use crate::{gc_protect, runtime::list::scm_cons, vm::thread::Thread};

use super::{
    control::{invalid_argument_violation, wrong_type_argument_violation},
    gsubr::{scm_define_subr, Subr},
    object::{
        scm_bytevector_as_slice, scm_bytevector_as_slice_mut, scm_bytevector_length,
        scm_bytevector_ref, scm_set_cdr, scm_string_str, scm_symbol_str,
    },
    symbol::scm_intern,
    value::Value,
};

struct MutatorParam {
    bytes: *mut u8,
    little: bool,
}

impl MutatorParam {
    fn new(octets: i32, subr: &str, thread: &mut Thread, args: &[&mut Value]) -> Self {
        let mut this = Self {
            bytes: std::ptr::null_mut(),
            little: false,
        };

        if args.len() == 4 {
            if args[0].is_bytevector() {
                if args[1].is_int32() {
                    let bvector = scm_bytevector_as_slice_mut(*args[0]);
                    let offset = args[1].get_int32();

                    if offset >= 0 && (offset + octets) <= bvector.len() as i32 {
                        this.bytes = unsafe { bvector.as_mut_ptr().add(offset as usize) };
                        if args[3].is_symbol() {
                            if scm_symbol_str(*args[3]) == "little" {
                                this.little = true;
                                return this;
                            } else if scm_symbol_str(*args[3]) == "big" {
                                this.little = false;
                                return this;
                            }
                        }
                        wrong_type_argument_violation(
                            thread,
                            subr,
                            3,
                            "endianess",
                            *args[3],
                            args.len(),
                            args,
                        )
                    }
                }

                if args[1].is_int32() {
                    invalid_argument_violation(
                        thread,
                        subr,
                        "index out of bounds",
                        *args[1],
                        1,
                        args.len(),
                        args,
                    );
                }

                invalid_argument_violation(
                    thread,
                    subr,
                    "exact integer",
                    *args[1],
                    1,
                    args.len(),
                    args,
                );
            }

            wrong_type_argument_violation(
                thread,
                subr,
                0,
                "bytevector",
                *args[0],
                args.len(),
                args,
            );
        }

        this
    }
}

struct NativeMutatorParam {
    bytes: *mut u8,
}

impl NativeMutatorParam {
    fn new(octets: i32, subr: &str, thread: &mut Thread, args: &[&mut Value]) -> Self {
        let mut this = Self {
            bytes: std::ptr::null_mut(),
        };

        if args.len() == 4 {
            if args[0].is_bytevector() {
                if args[1].is_int32() {
                    let bvector = scm_bytevector_as_slice_mut(*args[0]);
                    let offset = args[1].get_int32();

                    if offset >= 0 && (offset + octets) <= bvector.len() as i32 {
                        this.bytes = unsafe { bvector.as_mut_ptr().add(offset as usize) };
                        return this;
                    }
                }

                if args[1].is_int32() {
                    invalid_argument_violation(
                        thread,
                        subr,
                        "index out of bounds",
                        *args[1],
                        1,
                        args.len(),
                        args,
                    );
                }

                invalid_argument_violation(
                    thread,
                    subr,
                    "exact integer",
                    *args[1],
                    1,
                    args.len(),
                    args,
                );
            }

            wrong_type_argument_violation(
                thread,
                subr,
                0,
                "bytevector",
                *args[0],
                args.len(),
                args,
            );
        }

        this
    }
}

struct AccessorParam {
    bytes: *const u8,
    little: bool,
}

impl AccessorParam {
    fn new(octets: i32, subr: &str, thread: &mut Thread, args: &[&mut Value]) -> Self {
        let mut this = Self {
            bytes: std::ptr::null(),
            little: false,
        };

        if args.len() == 3 {
            if args[0].is_bytevector() {
                if args[1].is_int32() {
                    let bvector = scm_bytevector_as_slice_mut(*args[0]);
                    let offset = args[1].get_int32();

                    if offset >= 0 && (offset + octets) <= bvector.len() as i32 {
                        this.bytes = unsafe { bvector.as_ptr().add(offset as usize) };
                        if args[2].is_symbol() {
                            if scm_symbol_str(*args[2]) == "little" {
                                this.little = true;
                                return this;
                            } else if scm_symbol_str(*args[2]) == "big" {
                                this.little = false;
                                return this;
                            }
                        }
                        wrong_type_argument_violation(
                            thread,
                            subr,
                            2,
                            "endianess",
                            *args[2],
                            args.len(),
                            args,
                        )
                    }
                }

                if args[1].is_int32() {
                    invalid_argument_violation(
                        thread,
                        subr,
                        "index out of bounds",
                        *args[1],
                        1,
                        args.len(),
                        args,
                    );
                }

                invalid_argument_violation(
                    thread,
                    subr,
                    "exact integer",
                    *args[1],
                    1,
                    args.len(),
                    args,
                );
            }

            wrong_type_argument_violation(
                thread,
                subr,
                0,
                "bytevector",
                *args[0],
                args.len(),
                args,
            );
        }

        this
    }
}

struct NativeAccessorParam {
    bytes: *const u8,
}

impl NativeAccessorParam {
    fn new(octets: i32, subr: &str, thread: &mut Thread, args: &[&mut Value]) -> Self {
        let mut this = Self {
            bytes: std::ptr::null(),
        };

        if args.len() == 2 {
            if args[0].is_bytevector() {
                if args[1].is_int32() {
                    let bvector = scm_bytevector_as_slice_mut(*args[0]);
                    let offset = args[1].get_int32();

                    if offset >= 0 && (offset + octets) <= bvector.len() as i32 {
                        this.bytes = unsafe { bvector.as_ptr().add(offset as usize) };
                        return this;
                    }
                }

                if args[1].is_int32() {
                    invalid_argument_violation(
                        thread,
                        subr,
                        "index out of bounds",
                        *args[1],
                        1,
                        args.len(),
                        args,
                    );
                }

                invalid_argument_violation(
                    thread,
                    subr,
                    "exact integer",
                    *args[1],
                    1,
                    args.len(),
                    args,
                );
            }

            wrong_type_argument_violation(
                thread,
                subr,
                0,
                "bytevector",
                *args[0],
                args.len(),
                args,
            );
        }

        this
    }
}

extern "C-unwind" fn bytevector_s8_ref(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
) -> Value {
    let param = NativeAccessorParam::new(1, "bytevector-s8-ref", thread, &[bvector, offset]);
    let bytes = param.bytes;

    unsafe { Value::encode_int32(bytes.read() as i8 as i32) }
}

extern "C-unwind" fn bytevector_u8_ref(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
) -> Value {
    let param = NativeAccessorParam::new(1, "bytevector-u8-ref", thread, &[bvector, offset]);
    let bytes = param.bytes;

    unsafe { Value::encode_int32(bytes.read() as u8 as i32) }
}

extern "C-unwind" fn bytevector_s8_set(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
    value: &mut Value,
    endianess: &mut Value,
) -> Value {
    let param = NativeMutatorParam::new(
        1,
        "bytevector-s8-set!",
        thread,
        &[bvector, offset, value, endianess],
    );
    let bytes = param.bytes;

    if value.is_int32() {
        let val = value.get_int32();

        if val >= i8::MIN as i32 && val <= i8::MAX as i32 {
            unsafe {
                bytes.write(val as i8 as u8);
            }
            return Value::encode_undefined_value();
        } else {
            invalid_argument_violation(
                thread,
                "bytevector-s8-set!",
                "value out of range",
                *value,
                2,
                4,
                &[bvector, offset, value, endianess],
            );
        }
    } else {
        wrong_type_argument_violation(
            thread,
            "bytevector-s8-set!",
            2,
            "exact integer",
            *value,
            4,
            &[bvector, offset, value, endianess],
        );
    }
}

extern "C-unwind" fn bytevector_u8_set(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
    value: &mut Value,
    endianess: &mut Value,
) -> Value {
    let param = NativeMutatorParam::new(
        1,
        "bytevector-u8-set!",
        thread,
        &[bvector, offset, value, endianess],
    );
    let bytes = param.bytes;

    if value.is_int32() {
        let val = value.get_int32();

        if val >= u8::MIN as i32 && val <= u8::MAX as i32 {
            unsafe {
                bytes.write(val as u8);
            }
            return Value::encode_undefined_value();
        } else {
            invalid_argument_violation(
                thread,
                "bytevector-u8-set!",
                "value out of range",
                *value,
                2,
                4,
                &[bvector, offset, value, endianess],
            );
        }
    } else {
        wrong_type_argument_violation(
            thread,
            "bytevector-u8-set!",
            2,
            "exact integer",
            *value,
            4,
            &[bvector, offset, value, endianess],
        );
    }
}

extern "C-unwind" fn bytevector_s16_ref(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
    endianess: &mut Value,
) -> Value {
    let param = AccessorParam::new(
        2,
        "bytevector-s16-ref",
        thread,
        &[bvector, offset, endianess],
    );
    let bytes = param.bytes;
    let little = param.little;

    unsafe {
        if little {
            Value::encode_int32(i16::from_le_bytes([bytes.read(), bytes.add(1).read()]) as i32)
        } else {
            Value::encode_int32(i16::from_be_bytes([bytes.read(), bytes.add(1).read()]) as i32)
        }
    }
}

extern "C-unwind" fn bytevector_u16_ref(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
    endianess: &mut Value,
) -> Value {
    let param = AccessorParam::new(
        2,
        "bytevector-u16-ref",
        thread,
        &[bvector, offset, endianess],
    );
    let bytes = param.bytes;
    let little = param.little;

    unsafe {
        if little {
            Value::encode_int32(u16::from_le_bytes([bytes.read(), bytes.add(1).read()]) as i32)
        } else {
            Value::encode_int32(u16::from_be_bytes([bytes.read(), bytes.add(1).read()]) as i32)
        }
    }
}

extern "C-unwind" fn bytevector_s32_ref(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
    endianess: &mut Value,
) -> Value {
    let param = AccessorParam::new(
        4,
        "bytevector-s32-ref",
        thread,
        &[bvector, offset, endianess],
    );
    let bytes = param.bytes;
    let little = param.little;

    unsafe {
        if little {
            Value::encode_int32(i32::from_le_bytes([
                bytes.read(),
                bytes.add(1).read(),
                bytes.add(2).read(),
                bytes.add(3).read(),
            ]))
        } else {
            Value::encode_int32(i32::from_be_bytes([
                bytes.read(),
                bytes.add(1).read(),
                bytes.add(2).read(),
                bytes.add(3).read(),
            ]))
        }
    }
}

extern "C-unwind" fn bytevector_u32_ref(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
    endianess: &mut Value,
) -> Value {
    let param = AccessorParam::new(
        4,
        "bytevector-u32-ref",
        thread,
        &[bvector, offset, endianess],
    );
    let bytes = param.bytes;
    let little = param.little;

    unsafe {
        if little {
            Value::encode_int32(u32::from_le_bytes([
                bytes.read(),
                bytes.add(1).read(),
                bytes.add(2).read(),
                bytes.add(3).read(),
            ]) as i32)
        } else {
            Value::encode_int32(u32::from_be_bytes([
                bytes.read(),
                bytes.add(1).read(),
                bytes.add(2).read(),
                bytes.add(3).read(),
            ]) as i32)
        }
    }
}

extern "C-unwind" fn bytevector_char_ref(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
    endianess: &mut Value,
) -> Value {
    let param = AccessorParam::new(
        4,
        "bytevector-char-ref",
        thread,
        &[bvector, offset, endianess],
    );
    let bytes = param.bytes;
    let little = param.little;

    unsafe {
        if little {
            Value::encode_char(char::from_u32_unchecked(u32::from_le_bytes([
                bytes.read(),
                bytes.add(1).read(),
                bytes.add(2).read(),
                bytes.add(3).read(),
            ])))
        } else {
            Value::encode_char(char::from_u32_unchecked(u32::from_be_bytes([
                bytes.read(),
                bytes.add(1).read(),
                bytes.add(2).read(),
                bytes.add(3).read(),
            ])))
        }
    }
}

extern "C-unwind" fn bytevector_s16_set(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
    value: &mut Value,
    endianess: &mut Value,
) -> Value {
    let param = MutatorParam::new(
        2,
        "bytevector-s16-set!",
        thread,
        &[bvector, offset, value, endianess],
    );
    let bytes = param.bytes;
    let little = param.little;

    if value.is_int32() {
        let val = value.get_int32();

        if val >= i16::MIN as i32 && val <= i16::MAX as i32 {
            unsafe {
                if little {
                    bytes.write(val as i16 as u8);
                    bytes.add(1).write((val >> 8) as i16 as u8);
                } else {
                    bytes.write((val >> 8) as i16 as u8);
                    bytes.add(1).write(val as i16 as u8);
                }
            }
            return Value::encode_undefined_value();
        } else {
            invalid_argument_violation(
                thread,
                "bytevector-s16-set!",
                "value out of range",
                *value,
                2,
                4,
                &[bvector, offset, value, endianess],
            );
        }
    } else {
        wrong_type_argument_violation(
            thread,
            "bytevector-s16-set!",
            2,
            "exact integer",
            *value,
            4,
            &[bvector, offset, value, endianess],
        );
    }
}

extern "C-unwind" fn bytevector_u16_set(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
    value: &mut Value,
    endianess: &mut Value,
) -> Value {
    let param = MutatorParam::new(
        2,
        "bytevector-u16-set!",
        thread,
        &[bvector, offset, value, endianess],
    );
    let bytes = param.bytes;
    let little = param.little;

    if value.is_int32() {
        let val = value.get_int32();

        if val >= u16::MIN as i32 && val <= u16::MAX as i32 {
            unsafe {
                if little {
                    bytes.write(val as u16 as u8);
                    bytes.add(1).write((val >> 8) as u16 as u8);
                } else {
                    bytes.write((val >> 8) as u16 as u8);
                    bytes.add(1).write(val as u16 as u8);
                }
            }
            return Value::encode_undefined_value();
        } else {
            invalid_argument_violation(
                thread,
                "bytevector-u16-set!",
                "value out of range",
                *value,
                2,
                4,
                &[bvector, offset, value, endianess],
            );
        }
    } else {
        wrong_type_argument_violation(
            thread,
            "bytevector-u16-set!",
            2,
            "exact integer",
            *value,
            4,
            &[bvector, offset, value, endianess],
        );
    }
}

extern "C-unwind" fn bytevector_s32_set(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
    value: &mut Value,
    endianess: &mut Value,
) -> Value {
    let param = MutatorParam::new(
        4,
        "bytevector-s32-set!",
        thread,
        &[bvector, offset, value, endianess],
    );
    let bytes = param.bytes;
    let little = param.little;

    if value.is_int32() {
        let val = value.get_int32();

        if val >= i32::MIN as i32 && val <= i32::MAX as i32 {
            unsafe {
                if little {
                    bytes.write(val as u32 as u8);
                    bytes.add(1).write((val >> 8) as u32 as u8);
                    bytes.add(2).write((val >> 16) as u32 as u8);
                    bytes.add(3).write((val >> 24) as u32 as u8);
                } else {
                    bytes.write((val >> 24) as u32 as u8);
                    bytes.add(1).write((val >> 16) as u32 as u8);
                    bytes.add(2).write((val >> 8) as u32 as u8);
                    bytes.add(3).write(val as u32 as u8);
                }
            }
            return Value::encode_undefined_value();
        } else {
            invalid_argument_violation(
                thread,
                "bytevector-s32-set!",
                "value out of range",
                *value,
                2,
                4,
                &[bvector, offset, value, endianess],
            );
        }
    } else {
        wrong_type_argument_violation(
            thread,
            "bytevector-s32-set!",
            2,
            "exact integer",
            *value,
            4,
            &[bvector, offset, value, endianess],
        );
    }
}

extern "C-unwind" fn bytevector_u32_set(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
    value: &mut Value,
    endianess: &mut Value,
) -> Value {
    let param = MutatorParam::new(
        4,
        "bytevector-u32-set!",
        thread,
        &[bvector, offset, value, endianess],
    );
    let bytes = param.bytes;
    let little = param.little;

    if value.is_int32() {
        let val = value.get_int32();

        if val >= u32::MIN as i32 && val <= u32::MAX as i32 {
            unsafe {
                if little {
                    bytes.write(val as u32 as u8);
                    bytes.add(1).write((val >> 8) as u32 as u8);
                    bytes.add(2).write((val >> 16) as u32 as u8);
                    bytes.add(3).write((val >> 24) as u32 as u8);
                } else {
                    bytes.write((val >> 24) as u32 as u8);
                    bytes.add(1).write((val >> 16) as u32 as u8);
                    bytes.add(2).write((val >> 8) as u32 as u8);
                    bytes.add(3).write(val as u32 as u8);
                }
            }
            return Value::encode_undefined_value();
        } else {
            invalid_argument_violation(
                thread,
                "bytevector-u32-set!",
                "value out of range",
                *value,
                2,
                4,
                &[bvector, offset, value, endianess],
            );
        }
    } else {
        wrong_type_argument_violation(
            thread,
            "bytevector-u32-set!",
            2,
            "exact integer",
            *value,
            4,
            &[bvector, offset, value, endianess],
        );
    }
}

extern "C-unwind" fn bytevector_char_set(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
    value: &mut Value,
    endianess: &mut Value,
) -> Value {
    let param = MutatorParam::new(
        4,
        "bytevector-char-set!",
        thread,
        &[bvector, offset, value, endianess],
    );
    let bytes = param.bytes;
    let little = param.little;

    if value.is_char() {
        let val = value.get_char();

        unsafe {
            if little {
                bytes.write(val as u32 as u8);
                bytes.add(1).write((val as u32 >> 8) as u32 as u8);
                bytes.add(2).write((val as u32 >> 16) as u32 as u8);
                bytes.add(3).write((val as u32 >> 24) as u32 as u8);
            } else {
                bytes.write((val as u32 >> 24) as u32 as u8);
                bytes.add(1).write((val as u32 >> 16) as u32 as u8);
                bytes.add(2).write((val as u32 >> 8) as u32 as u8);
                bytes.add(3).write(val as u32 as u8);
            }
        }
        return Value::encode_undefined_value();
    } else {
        wrong_type_argument_violation(
            thread,
            "bytevector-char-set!",
            2,
            "character",
            *value,
            4,
            &[bvector, offset, value, endianess],
        );
    }
}

extern "C-unwind" fn bytevector_u16_native_ref(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
) -> Value {
    let param =
        NativeAccessorParam::new(2, "bytevector-u16-native-ref", thread, &[bvector, offset]);
    let bytes = param.bytes;

    unsafe { Value::encode_int32(u16::from_ne_bytes([bytes.read(), bytes.add(1).read()]) as i32) }
}

extern "C-unwind" fn bytevector_s16_native_ref(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
) -> Value {
    let param =
        NativeAccessorParam::new(2, "bytevector-s16-native-ref", thread, &[bvector, offset]);
    let bytes = param.bytes;

    unsafe { Value::encode_int32(i16::from_ne_bytes([bytes.read(), bytes.add(1).read()]) as i32) }
}

extern "C-unwind" fn bytevector_u32_native_ref(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
) -> Value {
    let param =
        NativeAccessorParam::new(4, "bytevector-u32-native-ref", thread, &[bvector, offset]);
    let bytes = param.bytes;

    unsafe {
        Value::encode_int32(u32::from_ne_bytes([
            bytes.read(),
            bytes.add(1).read(),
            bytes.add(2).read(),
            bytes.add(3).read(),
        ]) as i32)
    }
}

extern "C-unwind" fn bytevector_s32_native_ref(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
) -> Value {
    let param =
        NativeAccessorParam::new(4, "bytevector-s32-native-ref", thread, &[bvector, offset]);
    let bytes = param.bytes;

    unsafe {
        Value::encode_int32(i32::from_ne_bytes([
            bytes.read(),
            bytes.add(1).read(),
            bytes.add(2).read(),
            bytes.add(3).read(),
        ]))
    }
}

extern "C-unwind" fn bytevector_u16_native_set(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
    value: &mut Value,
) -> Value {
    let param = NativeMutatorParam::new(
        2,
        "bytevector-u16-native-set!",
        thread,
        &[bvector, offset, value],
    );
    let bytes = param.bytes;

    if value.is_int32() {
        let val = value.get_int32();

        if val >= u16::MIN as i32 && val <= u16::MAX as i32 {
            unsafe {
                bytes.write(val as u16 as u8);
                bytes.add(1).write((val >> 8) as u16 as u8);
            }
            return Value::encode_undefined_value();
        } else {
            invalid_argument_violation(
                thread,
                "bytevector-u16-native-set!",
                "value out of range",
                *value,
                2,
                3,
                &[bvector, offset, value],
            );
        }
    } else {
        wrong_type_argument_violation(
            thread,
            "bytevector-u16-native-set!",
            2,
            "exact integer",
            *value,
            3,
            &[bvector, offset, value],
        );
    }
}

extern "C-unwind" fn bytevector_s16_native_set(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
    value: &mut Value,
) -> Value {
    let param = NativeMutatorParam::new(
        2,
        "bytevector-s16-native-set!",
        thread,
        &[bvector, offset, value],
    );
    let bytes = param.bytes;

    if value.is_int32() {
        let val = value.get_int32();

        if val >= i16::MIN as i32 && val <= i16::MAX as i32 {
            unsafe {
                bytes.write(val as i16 as u8);
                bytes.add(1).write((val >> 8) as i16 as u8);
            }
            return Value::encode_undefined_value();
        } else {
            invalid_argument_violation(
                thread,
                "bytevector-s16-native-set!",
                "value out of range",
                *value,
                2,
                3,
                &[bvector, offset, value],
            );
        }
    } else {
        wrong_type_argument_violation(
            thread,
            "bytevector-s16-native-set!",
            2,
            "exact integer",
            *value,
            3,
            &[bvector, offset, value],
        );
    }
}

extern "C-unwind" fn bytevector_u32_native_set(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
    value: &mut Value,
) -> Value {
    let param = NativeMutatorParam::new(
        4,
        "bytevector-u32-native-set!",
        thread,
        &[bvector, offset, value],
    );
    let bytes = param.bytes;

    if value.is_int32() {
        let val = value.get_int32();

        if val >= u32::MIN as i32 && val <= u32::MAX as i32 {
            unsafe {
                bytes.write(val as u32 as u8);
                bytes.add(1).write((val >> 8) as u32 as u8);
                bytes.add(2).write((val >> 16) as u32 as u8);
                bytes.add(3).write((val >> 24) as u32 as u8);
            }
            return Value::encode_undefined_value();
        } else {
            invalid_argument_violation(
                thread,
                "bytevector-u32-native-set!",
                "value out of range",
                *value,
                2,
                3,
                &[bvector, offset, value],
            );
        }
    } else {
        wrong_type_argument_violation(
            thread,
            "bytevector-u32-native-set!",
            2,
            "exact integer",
            *value,
            3,
            &[bvector, offset, value],
        );
    }
}

extern "C-unwind" fn bytevector_s32_native_set(
    thread: &mut Thread,
    bvector: &mut Value,
    offset: &mut Value,
    value: &mut Value,
) -> Value {
    let param = NativeMutatorParam::new(
        4,
        "bytevector-s32-native-set!",
        thread,
        &[bvector, offset, value],
    );
    let bytes = param.bytes;

    if value.is_int32() {
        let val = value.get_int32();

        if val >= i32::MIN as i32 && val <= i32::MAX as i32 {
            unsafe {
                bytes.write(val as u32 as u8);
                bytes.add(1).write((val >> 8) as u32 as u8);
                bytes.add(2).write((val >> 16) as u32 as u8);
                bytes.add(3).write((val >> 24) as u32 as u8);
            }
            return Value::encode_undefined_value();
        } else {
            invalid_argument_violation(
                thread,
                "bytevector-s32-native-set!",
                "value out of range",
                *value,
                2,
                3,
                &[bvector, offset, value],
            );
        }
    } else {
        wrong_type_argument_violation(
            thread,
            "bytevector-s32-native-set!",
            2,
            "exact integer",
            *value,
            3,
            &[bvector, offset, value],
        );
    }
}

extern "C-unwind" fn native_endianess(_: &mut Thread) -> Value {
    if cfg!(target_endian = "little") {
        scm_intern("little")
    } else {
        scm_intern("big")
    }
}

extern "C-unwind" fn bytevector_p(_: &mut Thread, val: &mut Value) -> Value {
    Value::encode_bool_value(val.is_bytevector())
}

extern "C-unwind" fn make_bytevector(
    thread: &mut Thread,
    len: &mut Value,
    init: &mut Value,
) -> Value {
    let val = if init.is_undefined() {
        0
    } else if init.is_int32() {
        let val = init.get_int32();
        if val >= i8::MIN as i32 && val <= u8::MAX as i32 {
            val as u8
        } else {
            invalid_argument_violation(
                thread,
                "make-bytevector",
                "value out of range",
                *init,
                2,
                3,
                &[len, init],
            );
        }
    } else {
        wrong_type_argument_violation(
            thread,
            "make-bytevector",
            2,
            "exact integer",
            *init,
            3,
            &[len, init],
        );
    };

    if len.is_int32() {
        let length = len.get_int32();

        if length >= 0 {
            let bvector = thread.make_bytevector::<false>(length as _, val as _);
            return bvector;
        } else {
            invalid_argument_violation(
                thread,
                "make-bytevector",
                "negative length",
                *len,
                1,
                2,
                &[len, init],
            );
        }
    } else {
        wrong_type_argument_violation(
            thread,
            "make-bytevector",
            1,
            "exact integer",
            *len,
            2,
            &[len, init],
        );
    }
}

extern "C-unwind" fn bytevector_length(thread: &mut Thread, bvector: &mut Value) -> Value {
    if bvector.is_bytevector() {
        let length = scm_bytevector_length(*bvector);
        return Value::encode_int32(length as _);
    } else {
        wrong_type_argument_violation(
            thread,
            "bytevector-length",
            0,
            "bytevector",
            *bvector,
            1,
            &[bvector],
        );
    }
}

extern "C-unwind" fn bytevector_eq(thread: &mut Thread, bv1: &mut Value, bv2: &mut Value) -> Value {
    if bv1.is_bytevector() && bv2.is_bytevector() {
        let bv1 = scm_bytevector_as_slice(*bv1);
        let bv2 = scm_bytevector_as_slice(*bv2);

        if bv1.len() == bv2.len() {
            for (b1, b2) in bv1.iter().zip(bv2.iter()) {
                if b1 != b2 {
                    return Value::encode_bool_value(false);
                }
            }
            return Value::encode_bool_value(true);
        } else {
            return Value::encode_bool_value(false);
        }
    } else {
        wrong_type_argument_violation(
            thread,
            "bytevector=?",
            0,
            "bytevector",
            *bv1,
            1,
            &[bv1, bv2],
        );
    }
}

extern "C-unwind" fn bytevector_fill(
    thread: &mut Thread,
    bvector: &mut Value,
    val: &mut Value,
) -> Value {
    if bvector.is_bytevector() {
        let bv = scm_bytevector_as_slice_mut(*bvector);
        let value = if val.is_undefined() {
            0
        } else if val.is_int32() {
            let value = val.get_int32();
            if value >= i8::MIN as i32 && value <= u8::MAX as i32 {
                value as u8
            } else {
                invalid_argument_violation(
                    thread,
                    "bytevector-fill!",
                    "value out of range",
                    *val,
                    2,
                    3,
                    &[bvector, val],
                );
            }
        } else {
            wrong_type_argument_violation(
                thread,
                "bytevector-fill!",
                2,
                "exact integer",
                *val,
                3,
                &[bvector, val],
            );
        };

        for b in bv.iter_mut() {
            *b = value;
        }
        return Value::encode_undefined_value();
    } else {
        wrong_type_argument_violation(
            thread,
            "bytevector-fill!",
            0,
            "bytevector",
            *bvector,
            1,
            &[bvector, val],
        );
    }
}

/// `bytevector-copy!`
extern "C-unwind" fn bytevector_destructive_copy(
    thread: &mut Thread,
    bv1: &mut Value,
    source: &mut Value,
    bv2: &mut Value,
    target: &mut Value,
    size: &mut Value,
) -> Value {
    if !bv1.is_bytevector() {
        wrong_type_argument_violation(
            thread,
            "bytevector-copy!",
            0,
            "bytevector",
            *bv1,
            1,
            &[bv1, source, bv2, target, size],
        );
    }

    if !source.is_int32() {
        wrong_type_argument_violation(
            thread,
            "bytevector-copy!",
            1,
            "exact integer",
            *source,
            1,
            &[bv1, source, bv2, target, size],
        );
    }

    if !bv2.is_bytevector() {
        wrong_type_argument_violation(
            thread,
            "bytevector-copy!",
            2,
            "bytevector",
            *bv2,
            1,
            &[bv1, source, bv2, target, size],
        );
    }

    if !target.is_int32() {
        wrong_type_argument_violation(
            thread,
            "bytevector-copy!",
            3,
            "exact integer",
            *target,
            1,
            &[bv1, source, bv2, target, size],
        );
    }

    if !size.is_int32() {
        wrong_type_argument_violation(
            thread,
            "bytevector-copy!",
            4,
            "exact integer",
            *size,
            1,
            &[bv1, source, bv2, target, size],
        );
    }

    let bv1s = scm_bytevector_as_slice_mut(*bv1);
    let bv2s = scm_bytevector_as_slice_mut(*bv2);

    let src = source.get_int32();
    let dst = target.get_int32();
    let sz = size.get_int32();

    if src < 0 || src > bv1s.len() as i32 {
        invalid_argument_violation(
            thread,
            "bytevector-copy!",
            "source out of range",
            *source,
            1,
            2,
            &[bv1, source, bv2, target, size],
        );
    }

    if dst < 0 || dst > bv2s.len() as i32 {
        invalid_argument_violation(
            thread,
            "bytevector-copy!",
            "target out of range",
            *target,
            1,
            2,
            &[bv1, source, bv2, target, size],
        );
    }

    if src + sz <= bv1s.len() as i32 && dst + sz <= bv2s.len() as i32 {
        //bv2[dst as usize..(dst + sz) as usize]
        //    .copy_from_slice(&bv1[src as usize..(src + sz) as usize]);
        unsafe {
            // use memmove to allow overlapping regions
            libc::memmove(
                bv2s.as_mut_ptr().add(dst as usize).cast(),
                bv1s.as_mut_ptr().add(src as usize).cast(),
                sz as usize,
            );
        }
        return Value::encode_undefined_value();
    } else {
        invalid_argument_violation(
            thread,
            "bytevector-copy!",
            "source or target out of range",
            *size,
            1,
            2,
            &[bv1, source, bv2, target, size],
        );
    }
}

/// `(bytevector-copy <source>) -> <bytevector>`
extern "C-unwind" fn bytevector_copy(thread: &mut Thread, bv: &mut Value) -> Value {
    if !bv.is_bytevector() {
        wrong_type_argument_violation(thread, "bytevector-copy", 0, "bytevector", *bv, 1, &[bv]);
    }

    let bvs = scm_bytevector_as_slice(*bv);
    let bvc = thread.make_bytevector::<false>(bvs.len() as _, 0);

    let bvcs = scm_bytevector_as_slice_mut(bvc);

    bvcs.copy_from_slice(bvs);

    bvc
}

extern "C-unwind" fn utf8_string(thread: &mut Thread, bvector: &mut Value) -> Value {
    if bvector.is_bytevector() {
        let bv = scm_bytevector_as_slice(*bvector);

        let s = String::from_utf8_lossy(bv);
        let s = thread.make_string::<false>(&s);
        s
    } else {
        wrong_type_argument_violation(
            thread,
            "utf8-string",
            0,
            "bytevector",
            *bvector,
            1,
            &[bvector],
        );
    }
}

/// `string->utf8/nul
extern "C-unwind" fn string_utf8_nul(thread: &mut Thread, s: &mut Value) -> Value {
    if s.is_string() {
        let len = scm_string_str(*s).len();
        let bv = thread.make_bytevector::<false>(len + 1, 0);
        let bvs = &mut scm_bytevector_as_slice_mut(bv)[..len];
        bvs.copy_from_slice(scm_string_str(*s).as_bytes());
        bv
    } else {
        wrong_type_argument_violation(thread, "string->utf8/nul", 0, "string", *s, 1, &[s]);
    }
}

extern "C-unwind" fn string_utf8(thread: &mut Thread, s: &mut Value) -> Value {
    if s.is_string() {
        let bv = thread.make_bytevector::<false>(scm_string_str(*s).len(), 0);
        let bvs = &mut scm_bytevector_as_slice_mut(bv);
        bvs.copy_from_slice(scm_string_str(*s).as_bytes());
        bv
    } else {
        wrong_type_argument_violation(thread, "string->utf8", 0, "string", *s, 1, &[s]);
    }
}

extern "C-unwind" fn bytevector_to_list(thread: &mut Thread, bv: &mut Value) -> Value {
    if bv.is_bytevector() {
        let mut list = Value::encode_null_value();

        for i in 0..scm_bytevector_length(*bv) {
            let byte = scm_bytevector_ref(*bv, i);
            let new = gc_protect!(thread => list => scm_cons(Value::encode_int32(byte as _), Value::encode_null_value()));
            scm_set_cdr(new, thread, list);
            list = new;
        }

        list
    } else {
        wrong_type_argument_violation(thread, "bytevector->list", 0, "bytevector", *bv, 1, &[bv]);
    }
}

pub(crate) fn init() {
    scm_define_subr("bytevector->list", 1, 0, 0, Subr::F1(bytevector_to_list));
    scm_define_subr("bytevector?", 1, 0, 0, Subr::F1(bytevector_p));
    scm_define_subr("native-endianess", 0, 0, 0, Subr::F0(native_endianess));
    scm_define_subr("make-bytevector", 1, 1, 0, Subr::F2(make_bytevector));
    scm_define_subr("bytevector-length", 1, 0, 0, Subr::F1(bytevector_length));
    scm_define_subr("bytevector=?", 2, 0, 0, Subr::F2(bytevector_eq));
    scm_define_subr("bytevector-fill!", 2, 0, 0, Subr::F2(bytevector_fill));
    scm_define_subr(
        "bytevector-copy!",
        5,
        0,
        0,
        Subr::F5(bytevector_destructive_copy),
    );
    scm_define_subr("bytevector-copy", 1, 0, 0, Subr::F1(bytevector_copy));
    scm_define_subr("utf8->string", 1, 0, 0, Subr::F1(utf8_string));
    scm_define_subr("string->utf8/nul", 1, 0, 0, Subr::F1(string_utf8_nul));
    scm_define_subr("string->utf8", 1, 0, 0, Subr::F1(string_utf8));

    scm_define_subr("bytevector-u8-ref", 2, 0, 0, Subr::F2(bytevector_u8_ref));
    scm_define_subr("bytevector-s8-ref", 2, 0, 0, Subr::F2(bytevector_s8_ref));
    scm_define_subr("bytevector-u16-ref", 3, 0, 0, Subr::F3(bytevector_u16_ref));
    scm_define_subr("bytevector-s16-ref", 3, 0, 0, Subr::F3(bytevector_s16_ref));
    scm_define_subr("bytevector-u32-ref", 3, 0, 0, Subr::F3(bytevector_u32_ref));
    scm_define_subr("bytevector-s32-ref", 3, 0, 0, Subr::F3(bytevector_s32_ref));
    scm_define_subr(
        "bytevector-char-ref",
        3,
        0,
        0,
        Subr::F3(bytevector_char_ref),
    );
    scm_define_subr("bytevector-u8-set!", 3, 1, 0, Subr::F4(bytevector_u8_set));
    scm_define_subr("bytevector-s8-set!", 3, 1, 0, Subr::F4(bytevector_s8_set));
    scm_define_subr("bytevector-u16-set!", 4, 0, 0, Subr::F4(bytevector_u16_set));
    scm_define_subr("bytevector-s16-set!", 4, 0, 0, Subr::F4(bytevector_s16_set));
    scm_define_subr("bytevector-u32-set!", 4, 0, 0, Subr::F4(bytevector_u32_set));
    scm_define_subr("bytevector-s32-set!", 4, 0, 0, Subr::F4(bytevector_s32_set));
    scm_define_subr(
        "bytevector-char-set!",
        4,
        0,
        0,
        Subr::F4(bytevector_char_set),
    );

    scm_define_subr(
        "bytevector-u16-native-ref",
        2,
        0,
        0,
        Subr::F2(bytevector_u16_native_ref),
    );
    scm_define_subr(
        "bytevector-s16-native-ref",
        2,
        0,
        0,
        Subr::F2(bytevector_s16_native_ref),
    );
    scm_define_subr(
        "bytevector-u32-native-ref",
        2,
        0,
        0,
        Subr::F2(bytevector_u32_native_ref),
    );
    scm_define_subr(
        "bytevector-s32-native-ref",
        2,
        0,
        0,
        Subr::F2(bytevector_s32_native_ref),
    );
    scm_define_subr(
        "bytevector-u16-native-set!",
        3,
        0,
        0,
        Subr::F3(bytevector_u16_native_set),
    );

    scm_define_subr(
        "bytevector-u16-native-set!",
        3,
        0,
        0,
        Subr::F3(bytevector_u16_native_set),
    );
    scm_define_subr(
        "bytevector-s16-native-set!",
        3,
        0,
        0,
        Subr::F3(bytevector_s16_native_set),
    );
    scm_define_subr(
        "bytevector-u32-native-set!",
        3,
        0,
        0,
        Subr::F3(bytevector_u32_native_set),
    );
    scm_define_subr(
        "bytevector-s32-native-set!",
        3,
        0,
        0,
        Subr::F3(bytevector_s32_native_set),
    );
}
