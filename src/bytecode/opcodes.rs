use super::encode::*;

#[macro_export]
macro_rules! for_each_opcode {
    ($m: path) => {
        $m! {
            (op_enter, "enter", {})
            (op_nop, "nop", {})
            // relative to FP load 
            (op_load, "load", { offset: u32}) 
            // relative to FP store of value on top of stack 
            (op_store, "store", { offset: u32 })
            (op_tail_call, "tail-call", {})
            (op_call, "call", { argc: u32 })
            (op_ret, "ret", {})
            (op_gloc_ref, "gloc-ref", { index: u32 })
            (op_gloc_set, "gloc-set!", { index: u32 })
            (op_static_ref, "static-ref", { offset: u32 })
            (op_static_set, "static-set!", { offset: u32 })
            (op_push_i32, "push.i32", { value: i32 })
            (op_push_double, "push.double", { lo: u32, hi: u32 })
            (op_push_true, "push.true", {})
            (op_push_false, "push.false", {})
            (op_push_null, "push.null", {})
            (op_push_undef, "push.undef", {})


            (op_equal_cmp, "equal.cmp", {})
            (op_eqv_cmp, "eqv.cmp", {})
            (op_eq_cmp, "eq.cmp", {})
            (op_num_cmp, "num.cmp", {})
            (op_jmp, "jmp", { offset: i32 })
            (op_jl, "jl", { offset: i32 })
            (op_jle, "jle", { offset: i32 })
            (op_jg, "jg", { offset: i32 })
            (op_jge, "jge", { offset: i32 })
            (op_je, "je", { offset: i32 })
            (op_jne, "jne", { offset: i32 })

            (op_closure, "closure", { argc: u32, offset: u32 })
            (op_closure_ref, "closure-ref", { index: u32 })
            (op_closure_set, "closure-set!", { index: u32 })
            (op_car, "car", {})
            (op_cdr, "cdr", {})
            (op_cons, "cons", {})
            (op_set_car, "set-car!", {})
            (op_set_cdr, "set-cdr!", {})
            (op_list, "list", { argc: u32 })
            (op_listp, "list?", {})
            (op_nullp, "null?", {})
            (op_pairp, "pair?", {})
            (op_symbolp, "symbol?", {})
            (op_procedurep, "procedure?", {})
            (op_stringp, "string?", {})
            (op_numberp, "number?", {})
            (op_integerp, "integer?", {})
            (op_flonump, "flonum?", {})
            (op_booleanp, "boolean?", {})
            (op_vectorp, "vector?", {})
            (op_fixnump, "fixnum?", {})
            (op_vector_ref, "vector-ref", {  })
            (op_vector_set, "vector-set!", { })
            (op_vector, "vector", { argc: u32 })
            (op_vector_ref_imm, "vector-ref-imm", { index: u32 })
            (op_vector_set_imm, "vector-set-imm!", { index: u32 })
        

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
                    fn write(&self, gen: &mut impl InstructionStream) {
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

#[allow(unused_macros)]
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

//for_each_opcode!(disassemble);