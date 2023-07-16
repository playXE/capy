use crate::vm::{callframe::CallFrame, scm_vm};

use super::{arith::*, error::wrong_contract, object::{ScmResult, MAX_ARITY}, value::Value, module::{scm_capy_module, scm_define}, fun::scm_make_subr, symbol::Intern};

extern "C" fn bitwise_bit_count(cfr: &mut CallFrame) -> ScmResult {
    if !scm_is_exact_integer(cfr.argument(0)) {
        return wrong_contract::<()>("bitwise-bit-count", "exact-integer?", 0, 1, cfr.arguments())
            .into();
    }

    ScmResult::ok(arith_bitcount(cfr.argument(0)).unwrap())
}

extern "C" fn bitwise_first_bit_set(cfr: &mut CallFrame) -> ScmResult {
    if !scm_is_exact_integer(cfr.argument(0)) {
        return wrong_contract::<()>(
            "bitwise-first-bit-set",
            "exact-integer?",
            0,
            1,
            cfr.arguments(),
        )
        .into();
    }

    ScmResult::ok(arith_first_bit_set(cfr.argument(0)).unwrap())
}

extern "C" fn bitwise_last_bit_set(cfr: &mut CallFrame) -> ScmResult {
    if !scm_is_exact_integer(cfr.argument(0)) {
        return wrong_contract::<()>(
            "bitwise-last-bit-set",
            "exact-integer?",
            0,
            1,
            cfr.arguments(),
        )
        .into();
    }

    ScmResult::ok(arith_last_bit_set(cfr.argument(0)).unwrap())
}

extern "C" fn bitwise_not(cfr: &mut CallFrame) -> ScmResult {
    if !scm_is_exact_integer(cfr.argument(0)) {
        return wrong_contract::<()>("bitwise-not", "exact-integer?", 0, 1, cfr.arguments()).into();
    }

    ScmResult::ok(arith_lognot(scm_vm(), cfr.argument(0)).unwrap())
}

extern "C" fn bitwise_and(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 2 {
        if !scm_is_exact_integer(cfr.argument(0)) {
            return wrong_contract::<()>(
                "bitwise-and",
                "exact-integer?",
                0,
                2,
                cfr.arguments(),
            )
            .into();
        }

        if !scm_is_exact_integer(cfr.argument(1)) {
            return wrong_contract::<()>(
                "bitwise-and",
                "exact-integer?",
                1,
                2,
                cfr.arguments(),
            )
            .into();
        }

        ScmResult::ok(arith_logand(scm_vm(), cfr.argument(0), cfr.argument(1)).unwrap())
    } else {
        if cfr.argument_count() == 1 {
            if !scm_is_exact_integer(cfr.argument(0)) {
                return wrong_contract::<()>(
                    "bitwise-and",
                    "exact-integer?",
                    0,
                    1,
                    cfr.arguments(),
                )
                .into();
            }

            return ScmResult::ok(cfr.argument(0))
        }

        if cfr.argument_count() == 0 {
            return ScmResult::ok(Value::encode_int32(-1))
        }

        for i in 0..cfr.argument_count() {
            if !scm_is_exact_integer(cfr.argument(i)) {
                return wrong_contract::<()>(
                    "bitwise-and",
                    "exact-integer?",
                    i as _,
                    cfr.argument_count() as _,
                    cfr.arguments(),
                )
                .into();
            }
        }

        let mut acc = cfr.argument(0);

        for i in 1..cfr.argument_count() {
            acc = arith_logand(scm_vm(), acc, cfr.argument(i)).unwrap();
        }

        ScmResult::ok(acc)
    }
}

extern "C" fn bitwise_xor(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 2 {
        if !scm_is_exact_integer(cfr.argument(0)) {
            return wrong_contract::<()>(
                "bitwise-xor",
                "exact-integer?",
                0,
                2,
                cfr.arguments(),
            )
            .into();
        }

        if !scm_is_exact_integer(cfr.argument(1)) {
            return wrong_contract::<()>(
                "bitwise-xor",
                "exact-integer?",
                1,
                2,
                cfr.arguments(),
            )
            .into();
        }

        ScmResult::ok(arith_logxor(scm_vm(), cfr.argument(0), cfr.argument(1)).unwrap())
    } else {
        if cfr.argument_count() == 1 {
            if !scm_is_exact_integer(cfr.argument(0)) {
                return wrong_contract::<()>(
                    "bitwise-xor",
                    "exact-integer?",
                    0,
                    1,
                    cfr.arguments(),
                )
                .into();
            }

            return ScmResult::ok(cfr.argument(0))
        }

        if cfr.argument_count() == 0 {
            return ScmResult::ok(Value::encode_int32(-1))
        }

        for i in 0..cfr.argument_count() {
            if !scm_is_exact_integer(cfr.argument(i)) {
                return wrong_contract::<()>(
                    "bitwise-xor",
                    "exact-integer?",
                    i as _,
                    cfr.argument_count() as _,
                    cfr.arguments(),
                )
                .into();
            }
        }

        let mut acc = cfr.argument(0);

        for i in 1..cfr.argument_count() {
            acc = arith_logxor(scm_vm(), acc, cfr.argument(i)).unwrap();
        }

        ScmResult::ok(acc)
    }
}

extern "C" fn bitwise_or(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument_count() == 2 {
        if !scm_is_exact_integer(cfr.argument(0)) {
            return wrong_contract::<()>(
                "bitwise-or",
                "exact-integer?",
                0,
                2,
                cfr.arguments(),
            )
            .into();
        }

        if !scm_is_exact_integer(cfr.argument(1)) {
            return wrong_contract::<()>(
                "bitwise-or",
                "exact-integer?",
                1,
                2,
                cfr.arguments(),
            )
            .into();
        }

        ScmResult::ok(arith_logand(scm_vm(), cfr.argument(0), cfr.argument(1)).unwrap())
    } else {
        if cfr.argument_count() == 1 {
            if !scm_is_exact_integer(cfr.argument(0)) {
                return wrong_contract::<()>(
                    "bitwise-or",
                    "exact-integer?",
                    0,
                    1,
                    cfr.arguments(),
                )
                .into();
            }

            return ScmResult::ok(cfr.argument(0))
        }

        if cfr.argument_count() == 0 {
            return ScmResult::ok(Value::encode_int32(-1))
        }

        for i in 0..cfr.argument_count() {
            if !scm_is_exact_integer(cfr.argument(i)) {
                return wrong_contract::<()>(
                    "bitwise-or",
                    "exact-integer?",
                    i as _,
                    cfr.argument_count() as _,
                    cfr.arguments(),
                )
                .into();
            }
        }

        let mut acc = cfr.argument(0);

        for i in 1..cfr.argument_count() {
            acc = arith_logior(scm_vm(), acc, cfr.argument(i)).unwrap();
        }

        ScmResult::ok(acc)
    }
}

extern "C" fn bitiwise_arithmetic_shift(cfr: &mut CallFrame) -> ScmResult {
    if !scm_is_exact_integer(cfr.argument(0)) {
        return wrong_contract::<()>(
            "bitwise-arithmetic-shift",
            "exact-integer?",
            0,
            2,
            cfr.arguments(),
        )
        .into();
    }

    if !scm_is_exact_integer(cfr.argument(1)) {
        return wrong_contract::<()>(
            "bitwise-arithmetic-shift",
            "exact-integer?",
            1,
            2,
            cfr.arguments(),
        )
        .into();
    }

    ScmResult::ok(arith_logash(scm_vm(), cfr.argument(0), cfr.argument(1)).unwrap())
}

pub(crate) fn init_bitwise() {
    let module = scm_capy_module().module();
    
    let subr = scm_make_subr("bitwise-bit-count", bitwise_bit_count, 1, 1);
    scm_define(module, "bitwise-bit-count".intern(), subr).unwrap();

    let subr = scm_make_subr("bitwise-first-bit-set", bitwise_first_bit_set, 1, 1);
    scm_define(module, "bitwise-first-bit-set".intern(), subr).unwrap();

    let subr = scm_make_subr("bitwise-last-bit-set", bitwise_last_bit_set, 1, 1);
    scm_define(module, "bitwise-last-bit-set".intern(), subr).unwrap();

    let subr = scm_make_subr("bitwise-not", bitwise_not, 1, 1);
    scm_define(module, "bitwise-not".intern(), subr).unwrap();

    let subr = scm_make_subr("bitwise-and", bitwise_and, 0, MAX_ARITY);
    scm_define(module, "bitwise-and".intern(), subr).unwrap();

    let subr = scm_make_subr("bitwise-xor", bitwise_xor, 0, MAX_ARITY);
    scm_define(module, "bitwise-xor".intern(), subr).unwrap();

    let subr = scm_make_subr("bitwise-ior", bitwise_or, 0, MAX_ARITY);
    scm_define(module, "bitwise-ior".intern(), subr).unwrap();

    let subr = scm_make_subr("bitwise-arithmetic-shift", bitiwise_arithmetic_shift, 2, 2);
    scm_define(module, "bitwise-arithmetic-shift".intern(), subr).unwrap();
}
