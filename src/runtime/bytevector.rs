use crate::runtime::value::scm_int;
use crate::runtime::vector::make_bytevector;
use crate::{
    raise_exn,
    vm::{callframe::CallFrame, scm_vm},
};
use std::ptr::write_bytes;

use super::{
    error::wrong_contract,
    fun::scm_make_subr,
    module::{scm_capy_module, scm_define},
    number::*,
    object::ScmResult,
    symbol::Intern,
    value::Value,
    vector::make_bytevector_from_slice,
};

extern "C" fn bytevector_copy_x(cfr: &mut CallFrame) -> ScmResult {
    let source = cfr.argument(0);
    let source_start = cfr.argument(1);
    let target = cfr.argument(2);
    let target_start = cfr.argument(3);
    let len = cfr.argument(4);

    if !source.is_bytevector() {
        return wrong_contract::<()>(
            "bytevector-copy!",
            "bytevector?",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    if !target.is_bytevector() {
        return wrong_contract::<()>(
            "bytevector-copy!",
            "bytevector?",
            2,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    let c_source_start = scm_to_usize(source_start).ok_or_else(|| {
        wrong_contract::<()>(
            "bytevector-copy!",
            "exact-positive-integer?",
            1,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .unwrap_err()
    })?;

    let c_target_start = scm_to_usize(target_start).ok_or_else(|| {
        wrong_contract::<()>(
            "bytevector-copy!",
            "exact-positive-integer?",
            3,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .unwrap_err()
    })?;

    let c_len = scm_to_usize(len).ok_or_else(|| {
        wrong_contract::<()>(
            "bytevector-copy!",
            "exact-positive-integer?",
            4,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .unwrap_err()
    })?;

    let c_source = source.bytevector().contents;
    let c_target = target.bytevector().contents;

    let c_source_len = source.bytevector().length as usize;
    let c_target_len = target.bytevector().length as usize;

    if c_source_len < c_source_start || c_source_len.wrapping_sub(c_source_start) < c_len {
        return raise_exn!(
            (),
            Fail,
            &[],
            "bytevector-copy!: source start is out of range: {}",
            c_source_start
        )
        .into();
    }

    if c_target_len < c_target_start || c_target_len.wrapping_sub(c_target_start) < c_len {
        return raise_exn!(
            (),
            Fail,
            &[],
            "bytevector-copy!: target start is out of range: {}",
            c_target_start
        )
        .into();
    }

    unsafe {
        std::ptr::copy(
            c_source.add(c_source_start),
            c_target.add(c_target_start),
            c_len,
        );
    }

    ScmResult::ok(Value::encode_undefined_value())
}

extern "C" fn bytevector_copy(cfr: &mut CallFrame) -> ScmResult {
    let bv = cfr.argument(0);

    if !bv.is_bytevector() {
        return wrong_contract::<()>(
            "bytevector-copy",
            "bytevector?",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    let bv = make_bytevector_from_slice(scm_vm().mutator(), &bv.bytevector());

    ScmResult::ok(bv)
}

extern "C" fn make_bytevector_proc(cfr: &mut CallFrame) -> ScmResult {
    let k = cfr.argument(0);
    let fill = if cfr.argument_count() > 1 {
        cfr.argument(1)
    } else {
        Value::encode_int32(0)
    };

    let k = scm_to_usize(k).ok_or_else(|| {
        wrong_contract::<()>(
            "make-bytevector",
            "exact-positive-integer?",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .unwrap_err()
    })?;
    let fill = if fill.is_int32() {
        let fill = fill.get_int32();
        if fill < -128 || fill > 255 {
            return raise_exn!(
                (),
                Fail,
                &[],
                "make-bytevector: fill value must be in range -128..255, got: {}",
                fill
            )
            .into();
        }

        fill as u8
    } else {
        return wrong_contract::<()>("make-bytevector", "exact-integer?", 1, 2, cfr.arguments())
            .into();
    };

    let bv = make_bytevector(scm_vm().mutator(), k);

    unsafe {
        write_bytes(bv.contents, fill, k);
    }

    ScmResult::ok(bv)
}

extern "C" fn bytevector_length(cfr: &mut CallFrame) -> ScmResult {
    let bv = cfr.argument(0);

    if !bv.is_bytevector() {
        return wrong_contract::<()>(
            "bytevector-length",
            "bytevector?",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    ScmResult::ok(scm_int(bv.bytevector().length as _))
}

extern "C" fn bytevector_eq(cfr: &mut CallFrame) -> ScmResult {
    let bv1 = cfr.argument(0);
    let bv2 = cfr.argument(1);

    if !bv1.is_bytevector() {
        return wrong_contract::<()>(
            "bytevector=?",
            "bytevector?",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    if !bv2.is_bytevector() {
        return wrong_contract::<()>(
            "bytevector=?",
            "bytevector?",
            1,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    let bv1 = bv1.bytevector();
    let bv2 = bv2.bytevector();

    if bv1.length != bv2.length {
        return ScmResult::ok(Value::encode_bool_value(false));
    }

    let bv1p = bv1.contents;
    let bv2p = bv2.contents;

    unsafe {
        let cmp = libc::memcmp(bv1p.cast(), bv2p.cast(), bv1.length as usize);

        ScmResult::ok(Value::encode_bool_value(cmp == 0))
    }
}

extern "C" fn bytevector_fill(cfr: &mut CallFrame) -> ScmResult {
    let bv = cfr.argument(0);
    let fill = cfr.argument(1);

    if !bv.is_bytevector() {
        return wrong_contract::<()>(
            "bytevector-fill!",
            "bytevector?",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    if !fill.is_int32() {
        return wrong_contract::<()>(
            "bytevector-fill!",
            "exact-integer?",
            1,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    let bv = bv.bytevector();
    let fill = fill.get_int32();

    if fill < -128 || fill > 255 {
        return raise_exn!(
            (),
            Fail,
            &[],
            "bytevector-fill!: fill value must be in range -128..255, got: {}",
            fill
        )
        .into();
    }

    unsafe {
        write_bytes(bv.contents, fill as u8, bv.length as usize);
    }

    ScmResult::ok(Value::encode_undefined_value())
}

extern "C" fn bytevector_u8_ref(cfr: &mut CallFrame) -> ScmResult {
    let bv = cfr.argument(0);
    let k = cfr.argument(1);

    if !bv.is_bytevector() {
        return wrong_contract::<()>(
            "bytevector-u8-ref",
            "bytevector?",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    if !k.is_int32() {
        return wrong_contract::<()>(
            "bytevector-u8-ref",
            "exact-integer?",
            1,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    let bv = bv.bytevector();
    let k = scm_to_usize(k).ok_or_else(|| {
        wrong_contract::<()>(
            "bytevector-u8-ref",
            "exact-positive-integer?",
            1,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .unwrap_err()
    })?;

    if k >= bv.length as usize {
        return raise_exn!(
            (),
            Fail,
            &[],
            "bytevector-u8-ref: index {} out of range [0, {})",
            k,
            bv.length
        )
        .into();
    }

    ScmResult::ok(unsafe { Value::encode_int32(bv.contents.offset(k as isize).read() as _) })
}

extern "C" fn bytevector_u8_set(cfr: &mut CallFrame) -> ScmResult {
    let bv = cfr.argument(0);
    let k = cfr.argument(1);
    let byte = cfr.argument(2);

    if !bv.is_bytevector() {
        return wrong_contract::<()>(
            "bytevector-u8-set!",
            "bytevector?",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    if !k.is_int32() {
        return wrong_contract::<()>(
            "bytevector-u8-set!",
            "exact-integer?",
            1,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    if !byte.is_int32() {
        return wrong_contract::<()>(
            "bytevector-u8-set!",
            "exact-integer?",
            2,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    let bv = bv.bytevector();
    let k = scm_to_usize(k).ok_or_else(|| {
        wrong_contract::<()>(
            "bytevector-u8-set!",
            "exact-positive-integer?",
            1,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .unwrap_err()
    })?;

    if k >= bv.length as usize {
        return raise_exn!(
            (),
            Fail,
            &[],
            "bytevector-u8-set!: index {} out of range [0, {})",
            k,
            bv.length
        )
        .into();
    }

    let byte = byte.get_int32();

    if byte < -128 || byte > 255 {
        return raise_exn!(
            (),
            Fail,
            &[],
            "bytevector-u8-set!: byte value must be in range -128..255, got: {}",
            byte
        )
        .into();
    }

    unsafe {
        *bv.contents.offset(k as isize) = byte as u8;
    }

    ScmResult::ok(Value::encode_undefined_value())
}

extern "C" fn bytevector_s8_ref(cfr: &mut CallFrame) -> ScmResult {
    let bv = cfr.argument(0);
    let k = cfr.argument(1);

    if !bv.is_bytevector() {
        return wrong_contract::<()>(
            "bytevector-s8-ref",
            "bytevector?",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    if !k.is_int32() {
        return wrong_contract::<()>(
            "bytevector-s8-ref",
            "exact-integer?",
            1,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    let bv = bv.bytevector();
    let k = scm_to_usize(k).ok_or_else(|| {
        wrong_contract::<()>(
            "bytevector-s8-ref",
            "exact-positive-integer?",
            1,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .unwrap_err()
    })?;

    if k >= bv.length as usize {
        return raise_exn!(
            (),
            Fail,
            &[],
            "bytevector-s8-ref: index {} out of range [0, {})",
            k,
            bv.length
        )
        .into();
    }

    ScmResult::ok(unsafe {
        Value::encode_int32(bv.contents.cast::<i8>().offset(k as isize).read() as _)
    })
}

extern "C" fn bytevector_s8_set(cfr: &mut CallFrame) -> ScmResult {
    let bv = cfr.argument(0);
    let k = cfr.argument(1);
    let byte = cfr.argument(2);

    if !bv.is_bytevector() {
        return wrong_contract::<()>(
            "bytevector-s8-set!",
            "bytevector?",
            0,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    if !k.is_int32() {
        return wrong_contract::<()>(
            "bytevector-s8-set!",
            "exact-integer?",
            1,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    if !byte.is_int32() {
        return wrong_contract::<()>(
            "bytevector-s8-set!",
            "exact-integer?",
            2,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .into();
    }

    let bv = bv.bytevector();
    let k = scm_to_usize(k).ok_or_else(|| {
        wrong_contract::<()>(
            "bytevector-s8-set!",
            "exact-positive-integer?",
            1,
            cfr.argument_count() as _,
            cfr.arguments(),
        )
        .unwrap_err()
    })?;

    if k >= bv.length as usize {
        return raise_exn!(
            (),
            Fail,
            &[],
            "bytevector-s8-set!: index {} out of range [0, {})",
            k,
            bv.length
        )
        .into();
    }

    let byte = byte.get_int32();

    if byte < -128 || byte > 127 {
        return raise_exn!(
            (),
            Fail,
            &[],
            "bytevector-s8-set!: byte value must be in range -128..127, got: {}",
            byte
        )
        .into();
    }

    unsafe {
        *bv.contents.cast::<i8>().offset(k as isize) = byte as i8;
    }

    ScmResult::ok(Value::encode_undefined_value())
}




scm_symbol!(SYM_LITTLE_ENDIAN, "little");
scm_symbol!(SYM_BIG_ENDIAN, "big");

static mut SCM_NATIVE_ENDIAN: Value = Value::encode_empty_value();

pub fn scm_native_byteorder() -> Value {
    unsafe { SCM_NATIVE_ENDIAN }
}

pub(crate) fn init_bytevector() {
    unsafe {
        SCM_NATIVE_ENDIAN = if cfg!(target_endian = "little") {
            *SYM_LITTLE_ENDIAN
        } else {
            *SYM_BIG_ENDIAN
        };
    }
    let module = scm_capy_module().module();

    let subr = scm_make_subr("bytevector-copy!", bytevector_copy_x, 5, 5);
    scm_define(module, "bytevector-copy!".intern(), subr).unwrap();

    let subr = scm_make_subr("bytevector-copy", bytevector_copy, 1, 1);
    scm_define(module, "bytevector-copy".intern(), subr).unwrap();

    let subr = scm_make_subr("make-bytevector", make_bytevector_proc, 1, 2);
    scm_define(module, "make-bytevector".intern(), subr).unwrap();

    let subr = scm_make_subr("bytevector-length", bytevector_length, 1, 1);
    scm_define(module, "bytevector-length".intern(), subr).unwrap();

    let subr = scm_make_subr("bytevector=?", bytevector_eq, 2, 2);
    scm_define(module, "bytevector=?".intern(), subr).unwrap();

    let subr = scm_make_subr("bytevector-fill!", bytevector_fill, 2, 2);
    scm_define(module, "bytevector-fill!".intern(), subr).unwrap();

    let subr = scm_make_subr("bytevector-u8-ref", bytevector_u8_ref, 2, 2);
    scm_define(module, "bytevector-u8-ref".intern(), subr).unwrap();

    let subr = scm_make_subr("bytevector-u8-set!", bytevector_u8_set, 3, 3);
    scm_define(module, "bytevector-u8-set!".intern(), subr).unwrap();

    let subr = scm_make_subr("bytevector-s8-ref", bytevector_s8_ref, 2, 2);
    scm_define(module, "bytevector-s8-ref".intern(), subr).unwrap();

    let subr = scm_make_subr("bytevector-s8-set!", bytevector_s8_set, 3, 3);
    scm_define(module, "bytevector-s8-set!".intern(), subr).unwrap();
    macro_rules! integer_ops {
        ($(($name: ident, $t:ty, $len: expr)),*) => {
            paste::paste! {
                $(
                    extern "C" fn [<bytevector_ $name _ref>]<const NATIVE: bool>(cfr: &mut CallFrame) -> ScmResult {
                        let bv = cfr.argument(0);
                        let k = cfr.argument(1);
                        let endianess = if NATIVE {
                            scm_native_byteorder()
                        } else { cfr.argument(2) };
                        const LIT: &'static str = concat!("bytevector-", stringify!($name),"-ref");

                        let k = scm_to_usize(k).ok_or_else(|| {
                            wrong_contract::<()>(
                                LIT,
                                "exact-positive-integer?",
                                1,
                                cfr.argument_count() as _,
                                cfr.arguments(),
                            )
                            .unwrap_err()
                        })?;

                        if !bv.is_bytevector() {
                            return wrong_contract::<()>(
                                LIT,
                                "bytevector?",
                                0,
                                cfr.argument_count() as _,
                                cfr.arguments(),
                            )
                            .into();
                        }

                        if !endianess.is_symbol() {
                            return wrong_contract::<()>(
                                LIT,
                                "symbol?",
                                2,
                                cfr.argument_count() as _,
                                cfr.arguments(),
                            )
                            .into();
                        }

                        let bv = bv.bytevector();

                        let bv_len = bv.length as usize;

                        if bv_len < k
                            || bv_len.wrapping_sub(k) < ($len / 8) {
                            println!("{} {} {}", bv_len, bv_len.wrapping_sub(k), ($len / 8));
                            return raise_exn!((),
                                Fail,
                                &[],
                                "{}: index out of range: {}",
                                LIT,
                                k
                            ).into();
                        }

                        unsafe {
                            let v: $t = bv.contents.add(k).cast::<$t>().read_unaligned();

                            ScmResult::ok([<scm_make_ $name>](v, endianess == scm_native_byteorder()))
                        }
                    }

                    extern "C" fn [<bytevector_ $name _set>]<const NATIVE: bool>(cfr: &mut CallFrame) -> ScmResult {
                        let bv = cfr.argument(0);
                        let k = cfr.argument(1);
                        let endianess = if NATIVE {
                            scm_native_byteorder()
                        } else {
                            cfr.argument(2)
                        };

                        let value = if NATIVE {
                            cfr.argument(2)
                        } else {
                            cfr.argument(3)
                        };

                        const LIT: &'static str = concat!("bytevector-", stringify!($name), "-set!");

                        let k = scm_to_usize(k).ok_or_else(|| {
                            wrong_contract::<()>(
                                LIT,
                                "exact-positive-integer?",
                                1,
                                cfr.argument_count() as _,
                                cfr.arguments(),
                            )
                            .unwrap_err()
                        })?;

                        if !bv.is_bytevector() {
                            return wrong_contract::<()>(
                                LIT,
                                "bytevector?",
                                0,
                                cfr.argument_count() as _,
                                cfr.arguments(),
                            )
                            .into();
                        }

                        if !endianess.is_symbol() {
                            return wrong_contract::<()>(
                                LIT,
                                "symbol?",
                                2,
                                cfr.argument_count() as _,
                                cfr.arguments(),
                            )
                            .into();
                        }


                        let bv = bv.bytevector();

                        let bv_len = bv.length as usize;

                        if bv_len < k
                            || bv_len.wrapping_sub(k) < ($len / 8) {
                            return raise_exn!((),
                                Fail,
                                &[],
                                "{}: index out of range: {}",
                                LIT,
                                k
                            ).into();
                        }

                        let mut val = [<scm_ $name>](value).ok_or_else(|| {
                            raise_exn!((), Fail, &[], "{}: {} is invalid {} value", LIT, value, stringify!($name)).unwrap_err()
                        })?;
                        if !NATIVE && endianess != scm_native_byteorder() {
                            val = val.swap_bytes();
                        }
                        unsafe {
                            bv.contents.add(k).cast::<$t>().write_unaligned(val);
                        }
                        ScmResult::ok(Value::encode_undefined_value())
                    }

                    let subr = scm_make_subr(concat!("bytevector-", stringify!($name),"-ref"),  [<bytevector_ $name _ref>]::<false>, 3, 3);
                    scm_define(module, concat!("bytevector-", stringify!($name),"-ref").intern(), subr).unwrap();
                    let subr = scm_make_subr(concat!("bytevector-", stringify!($name),"-native-ref"),  [<bytevector_ $name _ref>]::<true>, 2, 2);
                    scm_define(module, concat!("bytevector-", stringify!($name),"-native-ref").intern(), subr).unwrap();

                    let subr = scm_make_subr(concat!("bytevector-", stringify!($name),"-set!"),  [<bytevector_ $name _set>]::<false>, 4, 4);
                    scm_define(module, concat!("bytevector-", stringify!($name),"-set!").intern(), subr).unwrap();
                    let subr = scm_make_subr(concat!("bytevector-", stringify!($name),"-native-set!"),  [<bytevector_ $name _set>]::<true>, 3, 3);
                    scm_define(module, concat!("bytevector-", stringify!($name),"-native-set!").intern(), subr).unwrap();

                )*
            }
        };
    }

    integer_ops!(
        (s16, i16, 16),
        (u16, u16, 16),
        (s32, i32, 32),
        (u32, u32, 32),
        (s64, i64, 64),
        (u64, u64, 64)
    );

    macro_rules! float_ops {
        ($(($name: ident, $t:ty, $len: expr)),*) => {
            paste::paste! {
                $(
                    extern "C" fn [<bytevector_ $name _ref>]<const NATIVE: bool>(cfr: &mut CallFrame) -> ScmResult {
                        let bv = cfr.argument(0);
                        let k = cfr.argument(1);
                        let endianess = if NATIVE {
                            scm_native_byteorder()
                        } else { cfr.argument(2) };
                        const LIT: &'static str = concat!("bytevector-", stringify!($name),"-ref");

                        let k = scm_to_usize(k).ok_or_else(|| {
                            wrong_contract::<()>(
                                LIT,
                                "exact-positive-integer?",
                                1,
                                cfr.argument_count() as _,
                                cfr.arguments(),
                            )
                            .unwrap_err()
                        })?;

                        if !bv.is_bytevector() {
                            return wrong_contract::<()>(
                                LIT,
                                "bytevector?",
                                0,
                                cfr.argument_count() as _,
                                cfr.arguments(),
                            )
                            .into();
                        }

                        if !endianess.is_symbol() {
                            return wrong_contract::<()>(
                                LIT,
                                "symbol?",
                                2,
                                cfr.argument_count() as _,
                                cfr.arguments(),
                            )
                            .into();
                        }

                        let bv = bv.bytevector();

                        let bv_len = bv.length as usize;

                        if bv_len < k
                            || bv_len.wrapping_sub(k) < ($len / 8) {
                            return raise_exn!((),
                                Fail,
                                &[],
                                "{}: index out of range: {}",
                                LIT,
                                k
                            ).into();
                        }

                        unsafe {
                            let v: $t = bv.contents.add(k).cast::<$t>().read_unaligned();

                            ScmResult::ok(Value::encode_untrusted_f64_value(v as _))
                        }
                    }

                    extern "C" fn [<bytevector_ $name _set>]<const NATIVE: bool>(cfr: &mut CallFrame) -> ScmResult {
                        let bv = cfr.argument(0);
                        let k = cfr.argument(1);
                        let endianess = if NATIVE {
                            scm_native_byteorder()
                        } else {
                            cfr.argument(2)
                        };

                        let value = if NATIVE {
                            cfr.argument(2)
                        } else {
                            cfr.argument(3)
                        };

                        const LIT: &'static str = concat!("bytevector-", stringify!($name), "-set!");

                        let k = scm_to_usize(k).ok_or_else(|| {
                            wrong_contract::<()>(
                                LIT,
                                "exact-positive-integer?",
                                1,
                                cfr.argument_count() as _,
                                cfr.arguments(),
                            )
                            .unwrap_err()
                        })?;

                        if !bv.is_bytevector() {
                            return wrong_contract::<()>(
                                LIT,
                                "bytevector?",
                                0,
                                cfr.argument_count() as _,
                                cfr.arguments(),
                            )
                            .into();
                        }

                        if !endianess.is_symbol() {
                            return wrong_contract::<()>(
                                LIT,
                                "symbol?",
                                2,
                                cfr.argument_count() as _,
                                cfr.arguments(),
                            )
                            .into();
                        }


                        let bv = bv.bytevector();

                        let bv_len = bv.length as usize;

                        if bv_len < k
                            || bv_len.wrapping_sub(k) < ($len / 8) {
                            return raise_exn!((),
                                Fail,
                                &[],
                                "{}: index out of range: {}",
                                LIT,
                                k
                            ).into();
                        }
                        if !value.is_double() {
                            return wrong_contract::<()>(
                                LIT,
                                "real?",
                                2,
                                cfr.argument_count() as _,
                                cfr.arguments(),
                            )
                            .into();
                        }
                        let val = value.get_double();

                        unsafe {
                            bv.contents.add(k).cast::<$t>().write_unaligned(val as _);
                        }
                        ScmResult::ok(Value::encode_undefined_value())
                    }

                    let subr = scm_make_subr(concat!("bytevector-", stringify!($name),"-ref"),  [<bytevector_ $name _ref>]::<false>, 3, 3);
                    scm_define(module, concat!("bytevector-", stringify!($name),"-ref").intern(), subr).unwrap();
                    let subr = scm_make_subr(concat!("bytevector-", stringify!($name),"-native-ref"),  [<bytevector_ $name _ref>]::<true>, 2, 2);
                    scm_define(module, concat!("bytevector-", stringify!($name),"-native-ref").intern(), subr).unwrap();

                    let subr = scm_make_subr(concat!("bytevector-", stringify!($name),"-set!"),  [<bytevector_ $name _set>]::<false>, 4, 4);
                    scm_define(module, concat!("bytevector-", stringify!($name),"-set!").intern(), subr).unwrap();
                    let subr = scm_make_subr(concat!("bytevector-", stringify!($name),"-native-set!"),  [<bytevector_ $name _set>]::<true>, 3, 3);
                    scm_define(module, concat!("bytevector-", stringify!($name),"-native-set!").intern(), subr).unwrap();

                )*
            }
        };
    }

    float_ops!(
        (f32, f32, 32),
        (f64, f64, 64)
    );
}
