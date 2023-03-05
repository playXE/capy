use super::cranelift::FunctionTranslator;
use crate::{
    compiler::{is_global_ref, is_ref, ref_name},
    value::{Type, Value as ScmValue},
};
use cranelift::prelude::{types, Block, InstBuilder, IntCC, Value};

pub const INLINEABLE_BINOPS: [&str; 10] = ["+", "-", "*", "/", "%", ">", "<", ">=", "<=", "="];
pub const INLINEABLE_UNOPS: [&str; 7] = ["+", "-", "not", "boolean?", "pair?", "number?", "null?"];

pub fn codegen_inline_binop<'a>(
    fx: &mut FunctionTranslator<'a>,
    bin_op: &str,
    lhs: Value,
    rhs: Value,
) -> (Value, Block) {
    let is_smallint = fx.builder.create_block();
    let done = fx.builder.create_block();
    let slowpath = fx.builder.create_block();

    fx.builder.append_block_param(done, types::I64);
    fx.builder.set_cold_block(slowpath);

    {
        let check_lhs = fx.check_smallint(lhs);
        let check_rhs = fx.check_smallint(rhs);
        let check = fx.builder.ins().band(check_lhs, check_rhs);
        fx.builder
            .ins()
            .brif(check, is_smallint, &[], slowpath, &[]);
    }

    {
        fx.builder.switch_to_block(is_smallint);
        let lhs_unboxed = fx.unbox_smallint(lhs);
        let rhs_unboxed = fx.unbox_smallint(rhs);

        let (val, has_overflow) = match bin_op {
            "+" => {
                let (val, has_overflow) = fx.builder.ins().iadd_cout(lhs_unboxed, rhs_unboxed);
                let val = fx.make_smallint(val);
                (val, has_overflow)
            }

            "-" => {
                // fx.builder.ins().isub_bout(lhs_unboxed, rhs_unboxed) - not implemented in ISLE for i32?

                let val = fx.builder.ins().isub(lhs_unboxed, rhs_unboxed);

                let rhs_is_negative =
                    fx.builder
                        .ins()
                        .icmp_imm(IntCC::SignedLessThan, rhs_unboxed, 0);
                let sgt = fx
                    .builder
                    .ins()
                    .icmp(IntCC::SignedGreaterThan, val, lhs_unboxed);
                let val = fx.make_smallint(val);
                (val, fx.builder.ins().bxor(rhs_is_negative, sgt))
            }
            "*" => {
                let lhs = fx.builder.ins().sextend(types::I64, lhs_unboxed);
                let rhs = fx.builder.ins().sextend(types::I64, rhs_unboxed);
                let val = fx.builder.ins().imul(lhs, rhs);

                let has_underflow =
                    fx.builder
                        .ins()
                        .icmp_imm(IntCC::SignedLessThan, val, -(1 << 31));
                let has_overflow =
                    fx.builder
                        .ins()
                        .icmp_imm(IntCC::SignedGreaterThan, val, (1 << 31) - 1);
                let val = fx.builder.ins().ireduce(types::I32, val);
                let val = fx.make_smallint(val);
                (val, fx.builder.ins().bor(has_underflow, has_overflow))
            }
            "/" => {
                let rhs_is_zero = fx.builder.ins().icmp_imm(IntCC::Equal, rhs_unboxed, 0);
                let rhs_not_zero = fx.builder.create_block();
                fx.builder
                    .ins()
                    .brif(rhs_is_zero, slowpath, &[], rhs_not_zero, &[]);
                fx.builder.switch_to_block(rhs_not_zero);

                let val = fx.builder.ins().sdiv(lhs_unboxed, rhs_unboxed);
                let val = fx.make_smallint(val);
                (val, fx.builder.ins().iconst(types::I32, 0))
            }
            "%" => {
                let rhs_is_zero = fx.builder.ins().icmp_imm(IntCC::Equal, rhs_unboxed, 0);
                let rhs_not_zero = fx.builder.create_block();
                fx.builder
                    .ins()
                    .brif(rhs_is_zero, slowpath, &[], rhs_not_zero, &[]);
                fx.builder.switch_to_block(rhs_not_zero);

                let val = fx.builder.ins().srem(lhs_unboxed, rhs_unboxed);
                let val = fx.make_smallint(val);
                (val, fx.builder.ins().iconst(types::I32, 0))
            }

            ">" => {
                let val = fx
                    .builder
                    .ins()
                    .icmp(IntCC::SignedGreaterThan, lhs_unboxed, rhs_unboxed);

                let t = fx
                    .builder
                    .ins()
                    .iconst(types::I64, ScmValue::make_true().raw() as i64);
                let f = fx
                    .builder
                    .ins()
                    .iconst(types::I64, ScmValue::make_false().raw() as i64);

                let val = fx.builder.ins().select(val, t, f);

                (val, fx.builder.ins().iconst(types::I32, 0))
            }

            ">=" => {
                let val = fx.builder.ins().icmp(
                    IntCC::SignedGreaterThanOrEqual,
                    lhs_unboxed,
                    rhs_unboxed,
                );

                let t = fx
                    .builder
                    .ins()
                    .iconst(types::I64, ScmValue::make_true().raw() as i64);
                let f = fx
                    .builder
                    .ins()
                    .iconst(types::I64, ScmValue::make_false().raw() as i64);

                let val = fx.builder.ins().select(val, t, f);

                (val, fx.builder.ins().iconst(types::I32, 0))
            }

            "<" => {
                let val = fx
                    .builder
                    .ins()
                    .icmp(IntCC::SignedLessThan, lhs_unboxed, rhs_unboxed);

                let t = fx
                    .builder
                    .ins()
                    .iconst(types::I64, ScmValue::make_true().raw() as i64);
                let f = fx
                    .builder
                    .ins()
                    .iconst(types::I64, ScmValue::make_false().raw() as i64);

                let val = fx.builder.ins().select(val, t, f);

                (val, fx.builder.ins().iconst(types::I32, 0))
            }

            "<=" => {
                let val =
                    fx.builder
                        .ins()
                        .icmp(IntCC::SignedLessThanOrEqual, lhs_unboxed, rhs_unboxed);

                let t = fx
                    .builder
                    .ins()
                    .iconst(types::I64, ScmValue::make_true().raw() as i64);
                let f = fx
                    .builder
                    .ins()
                    .iconst(types::I64, ScmValue::make_false().raw() as i64);

                let val = fx.builder.ins().select(val, t, f);

                (val, fx.builder.ins().iconst(types::I32, 0))
            }

            "=" => {
                let val = fx
                    .builder
                    .ins()
                    .icmp(IntCC::Equal, lhs_unboxed, rhs_unboxed);

                let t = fx
                    .builder
                    .ins()
                    .iconst(types::I64, ScmValue::make_true().raw() as i64);
                let f = fx
                    .builder
                    .ins()
                    .iconst(types::I64, ScmValue::make_false().raw() as i64);

                let val = fx.builder.ins().select(val, t, f);

                (val, fx.builder.ins().iconst(types::I32, 0))
            }

            _ => unreachable!("Cannot inline binop: {}", bin_op),
        };

        fx.builder
            .ins()
            .brif(has_overflow, slowpath, &[], done, &[val]);
        fx.builder.switch_to_block(done);
        (fx.builder.block_params(done)[0], slowpath)
    }
}

pub const INLINEABLE_TYPECHECKS: &[&str] = &[
    "number?",
    "procedure?",
    "pair?",
    "vector?",
    "string?",
    "symbol?",
    "char?",
    "boolean?",
    "null?",
    "eof?",
    "void?",
    "false?",
    "true?",
    "smallint?",
    "flonum?",
    "rational?",
    "bytevector?",
    "port?",
    "values?",
    "box?",
    "parameter?",
    "char?",
];

pub fn inline_type_check<'a>(fx: &mut FunctionTranslator<'a>, op: &str, val: Value) -> Value {
    match op {
        "number?" => {
            let is_smallint = fx.check_smallint(val);
            let heap_check = fx.builder.create_block();
            let done = fx.builder.create_block();
            fx.builder.append_block_param(done, types::I64);
            let t = fx
                .builder
                .ins()
                .iconst(types::I64, ScmValue::make_true().raw() as i64);
            fx.builder
                .ins()
                .brif(is_smallint, done, &[t], heap_check, &[]);
            fx.builder.switch_to_block(heap_check);
            let checked = fx.heap_check_in_range(val, Type::Integer, Type::Complex);
            fx.builder.ins().jump(done, &[checked]);
            fx.builder.switch_to_block(done);
            fx.builder.block_params(done)[0]
        }
        "procedure?" => {
            fx.heap_check_in_range(val, Type::PrimitiveProcedure, Type::NativeProcedure)
        }
        "pair?" => fx.heap_check(val, Type::Pair),
        "vector?" => fx.heap_check(val, Type::Vector),
        "string?" => fx.heap_check(val, Type::Str),
        "symbol?" => fx.heap_check(val, Type::Symbol),
        "char?" => fx.heap_check(val, Type::Char),
        "boolean?" => fx.heap_check_in_range(val, Type::True, Type::False),
        "null?" => fx.heap_check(val, Type::Null),
        "eof?" => {
            let is_eof =
                fx.builder
                    .ins()
                    .icmp_imm(IntCC::Equal, val, ScmValue::make_eof().raw() as i64);
            let t = fx
                .builder
                .ins()
                .iconst(types::I64, ScmValue::make_true().raw() as i64);
            let f = fx
                .builder
                .ins()
                .iconst(types::I64, ScmValue::make_false().raw() as i64);

            fx.builder.ins().select(is_eof, t, f)
        }
        "void?" => {
            let is_void =
                fx.builder
                    .ins()
                    .icmp_imm(IntCC::Equal, val, ScmValue::make_void().raw() as i64);
            let t = fx
                .builder
                .ins()
                .iconst(types::I64, ScmValue::make_true().raw() as i64);
            let f = fx
                .builder
                .ins()
                .iconst(types::I64, ScmValue::make_false().raw() as i64);

            fx.builder.ins().select(is_void, t, f)
        }

        "false?" => {
            let is_false =
                fx.builder
                    .ins()
                    .icmp_imm(IntCC::Equal, val, ScmValue::make_false().raw() as i64);
            let t = fx
                .builder
                .ins()
                .iconst(types::I64, ScmValue::make_true().raw() as i64);
            let f = fx
                .builder
                .ins()
                .iconst(types::I64, ScmValue::make_false().raw() as i64);

            fx.builder.ins().select(is_false, t, f)
        }

        "true?" => {
            let is_true =
                fx.builder
                    .ins()
                    .icmp_imm(IntCC::Equal, val, ScmValue::make_true().raw() as i64);
            let t = fx
                .builder
                .ins()
                .iconst(types::I64, ScmValue::make_true().raw() as i64);
            let f = fx
                .builder
                .ins()
                .iconst(types::I64, ScmValue::make_false().raw() as i64);

            fx.builder.ins().select(is_true, t, f)
        }

        "smallint?" => {
            let is_smallint = fx.check_smallint(val);
            let t = fx
                .builder
                .ins()
                .iconst(types::I64, ScmValue::make_true().raw() as i64);
            let f = fx
                .builder
                .ins()
                .iconst(types::I64, ScmValue::make_false().raw() as i64);

            fx.builder.ins().select(is_smallint, t, f)
        }

        "flonum?" => fx.heap_check(val, Type::Double),
        "rational?" => fx.heap_check(val, Type::Rational),

        "bytevector?" => fx.heap_check(val, Type::ByteVector),

        "port?" => fx.heap_check_in_range(val, Type::StringInputPort, Type::ByteVectorOutputPort),

        "values?" => fx.heap_check(val, Type::Values),

        "box?" => fx.heap_check(val, Type::Boxed),

        "parameter?" => fx.heap_check(val, Type::Parameter),

        _ => unreachable!("Cannot inline type check: {}", op),
    }
}

pub fn try_inline_application<'a>(
    fx: &mut FunctionTranslator<'a>,
    proc: ScmValue,
    args: &[Value],
) -> bool {
    if !(is_ref(proc) && is_global_ref(proc)) {
        return false;
    }
    let name = ref_name(proc);

    if INLINEABLE_BINOPS.contains(&name.strsym()) && args.len() == 3 {
        let (val, slowpath) = codegen_inline_binop(fx, name.strsym(), args[1], args[2]);
        let cont = args[0];
        let lhs = args[1];
        let rhs = args[2];

        fx.add_slow_path(move |fx| {
            // emit unoptimized code with loading the proc from the global table
            // and calling it.
            fx.builder.switch_to_block(slowpath);
            let proc = fx.generate_global_reference(name);
            fx.gen_tail_call(proc, &[cont, lhs, rhs]);
        });

        // invoke the continuation with the result of the inlined operation
        fx.gen_tail_call(cont, &[val]);

        true
    } else if INLINEABLE_TYPECHECKS.contains(&name.strsym()) && args.len() == 2 {
        let val = inline_type_check(fx, name.strsym(), args[1]);

        // invoke the continuation with the result of the inlined operation
        fx.gen_tail_call(args[0], &[val]);

        true
    } else {
        false
    }
}
