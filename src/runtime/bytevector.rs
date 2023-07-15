use crate::{
    raise_exn,
    vm::{callframe::CallFrame, scm_vm},
};

use super::{
    error::{out_of_range, wrong_contract},
    fun::scm_make_subr,
    module::{scm_capy_module, scm_define},
    number::scm_to_usize,
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

scm_symbol!(SYM_LITTLE_ENDIAN, "little-endian");
scm_symbol!(SYM_BIG_ENDIAN, "big-endian");
use crate::runtime::value::*;
pub fn scm_native_byteorder() -> Value {
    if cfg!(target_endian = "little") {
        *SYM_LITTLE_ENDIAN
    } else {
        *SYM_BIG_ENDIAN
    }
}

pub(crate) fn init_bytevector() {
    let module = scm_capy_module().module();

    let subr = scm_make_subr("bytevector-copy!", bytevector_copy_x, 5, 5);
    scm_define(module, "bytevector-copy!".intern(), subr);

    let subr = scm_make_subr("bytevector-copy", bytevector_copy, 1, 1);

    macro_rules! integer_ops {
        ($(($name: ident, $t:ty, $len: expr, $literal: literal)),*) => {
            paste::paste! {
                $(
                    extern "C" fn [<bytevector_ $name _ref>](cfr: &mut CallFrame) -> ScmResult {
                        let bv = cfr.argument(0);
                        let k = cfr.argument(1);
                        let endianess = cfr.argument(2);

                        let k = scm_to_usize(k).ok_or_else(|| {
                            wrong_contract::<()>(
                                $literal,
                                "exact-positive-integer?",
                                1,
                                cfr.argument_count() as _,
                                cfr.arguments(),
                            )
                            .unwrap_err()
                        })?;

                        if !bv.is_bytevector() {
                            return wrong_contract::<()>(
                                $literal,
                                "bytevector?",
                                0,
                                cfr.argument_count() as _,
                                cfr.arguments(),
                            )
                            .into();
                        }

                        if !endianess.is_symbol() {
                            return wrong_contract::<()>(
                                $literal,
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
                                $literal,
                                k
                            ).into();
                        }

                        unsafe {
                            let mut v: $t = bv.contents.add(k).cast::<$t>().read_unaligned();

                            if endianess == *SYM_BIG_ENDIAN {
                                v = v.to_be();
                            } else {
                                v = v.to_le();
                            }

                            ScmResult::ok(scm_int(v as _))
                        }
                    }
                )*
            }
        };
    }

    integer_ops!((s16, i16, 16, "bytevector-u16-ref"));
}
