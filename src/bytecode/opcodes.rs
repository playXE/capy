use super::encode::*;
use super::virtual_register::*;
use crate::bytecompiler::*;
use bytecodegenerator::*;
#[macro_export]
macro_rules! for_each_opcode {
    ($m: path) => {
        $m! {
            (op_enter, "enter", {})
            (op_nop, "nop", {})
            (op_mov, "mov", { dst: VirtualRegister, src: VirtualRegister })
            (op_movi, "movi", { dst: VirtualRegister, imm: i32 })
            (op_null, "null", { dst: VirtualRegister })
            (op_undefined, "undefined", { dst: VirtualRegister })
            (op_true, "true", { dst: VirtualRegister })
            (op_false, "false", { dst: VirtualRegister })
            (op_global_ref, "global-ref", { dst: VirtualRegister, constant: u16 })
            (op_global_set, "global-set", { src: VirtualRegister, constant: u16 })
            (op_make_closure, "make-closure", { dest: VirtualRegister, src: u16, argc: u16 }) // srcDest, argv, argc
            (op_closure_capture, "closure-capture", { dst: VirtualRegister, index: u16 , src: VirtualRegister }) // dst, index, src
            (op_call, "call", { dst: VirtualRegister, callee: VirtualRegister, argv: u16, argc: u16 }) // srcDest, argv, argc
            (op_tail_call, "tail-call", { callee: VirtualRegister, argv: u16, argc: u16 }) // src, argv, argc
            (op_return, "return", { src: VirtualRegister })
            (op_apply, "apply", { dst: VirtualRegister, rator: VirtualRegister, rands: VirtualRegister }) // dst, src, args
            (op_add_s16, "add.i16", { dst: VirtualRegister, src: VirtualRegister, imm: i16 })
            (op_add_s8, "add.i8", { dst: VirtualRegister, src: VirtualRegister, imm: i8 })
            (op_add_s32, "add.i32", { dst: VirtualRegister, src: VirtualRegister, imm: i32 })
            (op_sub_s16, "sub.i16", { dst: VirtualRegister, src: VirtualRegister, imm: i16 })
            (op_sub_s8, "sub.i8", { dst: VirtualRegister, src: VirtualRegister, imm: i8 })
            (op_sub_s32, "sub.i32", { dst: VirtualRegister, src: VirtualRegister, imm: i32 })
            (op_mul_s16, "mul.i16", { dst: VirtualRegister, src: VirtualRegister, imm: i16 })
            (op_mul_s8, "mul.i8", { dst: VirtualRegister, src: VirtualRegister, imm: i8 })
            (op_mul_s32, "mul.i32", { dst: VirtualRegister, src: VirtualRegister, imm: i32 })
            (op_quotient_s16, "quotient.i16", { dst: VirtualRegister, src: VirtualRegister, imm: i16 })
            (op_quotient_s8, "quotient.i8", { dst: VirtualRegister, src: VirtualRegister, imm: i8 })
            (op_quotient_s32, "quotient.i32", { dst: VirtualRegister, src: VirtualRegister, imm: i32 })
            (op_remainder_s16, "remainder.i16", { dst: VirtualRegister, src: VirtualRegister, imm: i16 })
            (op_remainder_s8, "remainder.i8", { dst: VirtualRegister, src: VirtualRegister, imm: i8 })
            (op_remainder_s32, "remainder.i32", { dst: VirtualRegister, src: VirtualRegister, imm: i32 })
            (op_add, "add", { dst: VirtualRegister, lhs: VirtualRegister, rhs: VirtualRegister })
            (op_sub, "sub", { dst: VirtualRegister, lhs: VirtualRegister, rhs: VirtualRegister })
            (op_mul, "mul", { dst: VirtualRegister, lhs: VirtualRegister, rhs: VirtualRegister })
            (op_div, "div", { dst: VirtualRegister, lhs: VirtualRegister, rhs: VirtualRegister })
            (op_quotient, "quotient", {dst: VirtualRegister, lhs: VirtualRegister,  rhs: VirtualRegister})
            (op_remainder, "remainder", { dst: VirtualRegister, lhs: VirtualRegister, rhs: VirtualRegister })
            (op_abs, "abs", {dst: VirtualRegister, src: VirtualRegister })
            (op_number_equal, "number-equal", { dst: VirtualRegister, lhs: VirtualRegister, rhs: VirtualRegister })
            (op_less, "less", { dst: VirtualRegister, lhs: VirtualRegister, rhs: VirtualRegister})
            (op_less_equal, "less-equal", { dst: VirtualRegister, lhs: VirtualRegister, rhs: VirtualRegister })
            (op_greater, "greater", { dst: VirtualRegister, lhs: VirtualRegister, rhs: VirtualRegister })
            (op_greater_equal, "greater-equal", { dst: VirtualRegister, lhs: VirtualRegister, rhs: VirtualRegister })
            (op_not, "not", { dst: VirtualRegister, src: VirtualRegister })
            (op_assert_arg_count, "assert-arg-count", { argc: u16 })
            (op_assert_min_arg_count, "assert-min-arg-count", { argc: u16 })
            (op_no_matching_arg_count, "no-matching-arg-count", {})
            (op_collect_rest, "collect-rest", { dest: VirtualRegister, arg: u16 })
            (op_box, "box", { dst: VirtualRegister, src: VirtualRegister })
            (op_box_ref, "box-ref", { dst: VirtualRegister, src: VirtualRegister })
            (op_box_set, "box-set", { box_: VirtualRegister, value: VirtualRegister })
            (op_define,  "define", { value: VirtualRegister, constant: u16 })
            (op_define_const, "define-const", { value: VirtualRegister, constant: u16 })
            (op_define_module, "define-module", { name: u16 })
            (op_closure_ref, "closure-ref", { dst: VirtualRegister, index: u16 })
            (op_closure_set, "closure-set", { src: VirtualRegister, index: u16 })
            (op_closure_ref_unbox, "closure-ref-unbox", { dst: VirtualRegister, index: u16 })
            (op_closure_ref_box, "closure-ref-box", { src: VirtualRegister, index: u16 })
            (op_branch, "branch", { offset: i32 })
            (op_branch_if, "branch.true", { src: VirtualRegister,  offset: i32 })
            (op_branch_if_not, "branch.false", { src: VirtualRegister, offset: i32 })
            (op_branch_less, "branch.lt", { lhs: VirtualRegister, rhs: VirtualRegister, offset: i32 })
            (op_branch_less_equal, "branch.le", { lhs: VirtualRegister, rhs: VirtualRegister, offset: i32 })
            (op_branch_greater, "branch.gt", { lhs: VirtualRegister, rhs: VirtualRegister, offset: i32 })
            (op_branch_greater_equal, "branch.ge", { lhs: VirtualRegister, rhs: VirtualRegister, offset: i32 })
            (op_branch_number_equal, "branch.neq", { lhs: VirtualRegister, rhs: VirtualRegister, offset: i32 })
            (op_branch_eq, "branch.eq", { lhs: VirtualRegister, rhs: VirtualRegister, offset: i32 })
            (op_branch_eqv, "branch.eqv", { lhs: VirtualRegister, rhs: VirtualRegister, offset: i32 })
            (op_branch_equal, "branch.equal", { lhs: VirtualRegister, rhs: VirtualRegister, offset: i32 })
            (op_is_number, "is-number", { dst: VirtualRegister, src: VirtualRegister })
            (op_is_complex, "is-complex", { dst: VirtualRegister, src: VirtualRegister })
            (op_is_real, "is-real", {dst: VirtualRegister, src: VirtualRegister })
            (op_is_rational, "is-rational", {dst: VirtualRegister, src: VirtualRegister})
            (op_is_integer, "is-integer", {dst: VirtualRegister, src: VirtualRegister})
            (op_is_exact_integer, "is-exact-integer", {dst: VirtualRegister, src: VirtualRegister})
            (op_is_exact_nonnegative_integer, "is-exact-nonnegative-integer", {dst: VirtualRegister, src: VirtualRegister})
            (op_is_exact_positive_integer, "is-exact-positive-integer", {dst: VirtualRegister, src: VirtualRegister})
            (op_is_fixnum, "is-fixnum", {dst: VirtualRegister, src: VirtualRegister})
            (op_is_inexact_real, "is-inexact-real", {dst: VirtualRegister, src: VirtualRegister})
            (op_is_flonum, "is-flonum", {dst: VirtualRegister, src: VirtualRegister})
            (op_is_exact, "is-exact", {dst: VirtualRegister, src: VirtualRegister})
            (op_is_inexact, "is-inexact", { dst: VirtualRegister, src: VirtualRegister })
            (op_is_pair, "is-pair", { dst: VirtualRegister, src: VirtualRegister })
            (op_is_null, "is-null", { dst: VirtualRegister, src: VirtualRegister })
            (op_is_undef, "is-undef", { dst: VirtualRegister, src: VirtualRegister })
            (op_is_list, "is-list", { dst: VirtualRegister, src: VirtualRegister })
            (op_is_vector, "is-vector", { dst: VirtualRegister, src: VirtualRegister })
            (op_is_tuple, "is-tuple", { dst: VirtualRegister, src: VirtualRegister })
            (op_is_struct_type, "is-struct-type", { dst: VirtualRegister, src: VirtualRegister })
            (op_is_struct_type_property, "is-struct-type-property", { dst: VirtualRegister, src: VirtualRegister })
            (op_is_struct, "is-struct", { dst: VirtualRegister, src: VirtualRegister })
            (op_list, "list", { dst: VirtualRegister, argv: i16, argc: u16})
            (op_cons, "cons", { dst: VirtualRegister, car: VirtualRegister, cdr: VirtualRegister })
            (op_car, "car", { dst: VirtualRegister, src: VirtualRegister })
            (op_cdr, "cdr", { cdr: VirtualRegister, src: VirtualRegister })
            (op_set_car, "set-car", { dst: VirtualRegister, src: VirtualRegister })
            (op_set_cdr, "set-cdr", { dst: VirtualRegister, src: VirtualRegister })
            (op_vector, "vector", { dst: VirtualRegister, argv: i16, argc: u16 })
            (op_make_vector, "make-vector", { dst: VirtualRegister, count: VirtualRegister, fill: VirtualRegister })
            (op_vector_ref, "vector-ref", { dst: VirtualRegister, src: VirtualRegister, index: VirtualRegister })
            (op_vector_refi, "vector-ref.u16", { dst: VirtualRegister, src: VirtualRegister, index: u16 })
            (op_vector_set, "vector-set", { vector: VirtualRegister, index: VirtualRegister, src: VirtualRegister })
            (op_vector_seti, "vector-set.u16", { vector: VirtualRegister, index: u16, src: VirtualRegister })
            (op_vector_length, "vector-length", { dst: VirtualRegister, src: VirtualRegister })
            (op_tuple, "tuple", { dst: VirtualRegister, argv: i16, argc: u16 })
            (op_tuple_ref, "tuple-ref", { dst: VirtualRegister, src: VirtualRegister, index: VirtualRegister })
            (op_tuple_set, "tuple-set", { dst: VirtualRegister, index: VirtualRegister, src: VirtualRegister })
            (op_tuple_refi, "tuple-ref.u16", { dst: VirtualRegister, src: VirtualRegister, index: u16 })
            (op_tuple_seti, "tuple-set.u16", { dst: VirtualRegister, index: u16,  src: VirtualRegister })
            (op_list_to_vector, "list->vector", { dst: VirtualRegister, src: VirtualRegister })
            (op_vector_append, "vector-append", { dst: VirtualRegister, src1: VirtualRegister, src2: VirtualRegister })
            (op_struct_prop_pred, "is-struct-property-of", { dst: VirtualRegister, src: VirtualRegister, constant: u16 })
            (op_struct_ref, "struct-ref", { dst: VirtualRegister, src: VirtualRegister, index: VirtualRegister })
            (op_struct_set, "struct-set", { dst: VirtualRegister, src: VirtualRegister, index: VirtualRegister })
            (op_struct_pred, "struct-pred", {dst: VirtualRegister, src: VirtualRegister, constant: u16 })
            (op_equal, "equal", { dst: VirtualRegister, src1: VirtualRegister, src2: VirtualRegister })
            (op_eqv, "eqv", { dst: VirtualRegister, src1: VirtualRegister, src2: VirtualRegister })
            (op_eq, "eq", { dst: VirtualRegister, src1: VirtualRegister, src2: VirtualRegister })
            (op_last, "last", {})
        }
    }
}

macro_rules! decl_opcodes {
    ($(($name: ident, $str: literal, { $($field: ident : $t: ty),* }))*) => {
        $(
            paste::paste! {
                #[repr(C, packed)]
                #[derive(Copy, Clone, PartialEq, Eq, Debug)]
                pub struct [<$name: camel>] {
                    $(pub $field: $t),*
                }

                impl [<$name: camel>] {
                    pub const NAME: &'static str = $str;
                    #[inline(always)]
                    pub const fn new($($field: $t),*) -> Self {
                        Self {
                            $($field),*
                        }
                    }

                    $(
                        #[inline(always)]
                        pub fn $field(&self) -> $t {
                            self.$field
                        }
                    )*
                }

                impl Encode for [<$name: camel>] {
                    #[inline(always)]
                    fn write(&self, gen: &mut BytecodeGenerator) {
                        gen.write_u8(paste::paste!([<$name:upper>]));
                        $(
                            self.$field().write(gen);
                        )*
                    }
                }

                impl Decode for [<$name: camel>] {
                    #[inline(always)]
                    unsafe fn read(stream: *const u8) -> Self {
                        debug_assert_eq!(stream.wrapping_sub(1).read(), paste::paste!([<$name:upper>]));
                        stream.cast::<Self>().read_unaligned()
                    }
                }

                impl std::fmt::Display for [<$name: camel>] {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        write!(f, "{}", Self::NAME)?;
                        let fields: &[String] = &[
                            $(
                                self.$field().to_string()
                            ),*
                        ];

                        // print comma separated fields
                        if !fields.is_empty() {
                            write!(f, " {}", fields.join(", "))?;
                        }
                        Ok(())
                    }
                }
            }
        )*
    }
}

macro_rules! decl_constants {
    (($name: ident, $str: literal, {$($field: ident : $t: ty),*}) $($rest:tt)*) => {

        paste::paste! { pub const [<$name:upper>]:  u8 = 0; }
        decl_constants!(@parse 1, $($rest)*);
    };

    (@parse $cursor: expr, ($name: ident, $str: literal, {$($field: ident : $t: ty),*}) $($rest:tt)*) => {
        paste::paste! {
            pub const [<$name:upper>]: u8 = $cursor;
        }
        decl_constants!(@parse $cursor + 1, $($rest)*);
    };
    (@parse $cursor: expr, ) => {}
}

for_each_opcode!(decl_opcodes);
for_each_opcode!(decl_constants);

macro_rules! disassemble {
    ($(($name: ident, $str: literal, {$($field: ident : $t: ty), *}))*) => {

            pub unsafe fn disassemble_from_stream<T: std::fmt::Write>(op: u8, stream: &mut *const u8, out: &mut T) -> std::fmt::Result {
                paste::paste! {
                    match op {
                        $(
                            [<$name: upper>] => {
                                paste::paste! {
                                    let op = [<$name: camel>]::read(*stream);
                                    *stream = stream.add(std::mem::size_of::<[<$name: camel>]>());
                                    write!(out, "{}", op)
                                }
                            }
                        ),+
                        _ => unreachable!("{}", op)
                    }
                }
            }

    };
}

for_each_opcode!(disassemble);
