use crate::runtime::object::ScmProgram;
use crate::runtime::value::Value;

use super::encode::*;
use super::u24::*;

#[macro_export]
macro_rules! for_each_opcode {
    ($m: path) => {
        $m! {
            (op_halt, "halt", {})
            (op_enter, "enter", { offset: i32 })
            // tells interpreter that we're in loop, might enter JITed code from here.
            (op_loop_hint, "loop-hint", { offset: i32 })
            (op_nop, "nop", {})
            // Call a procedure. `proc` is the local corresponding to a procedure.
            // The three values below `proc` will be overwritten by the saved call
            // frame data. The new frame will have space for NLOCALS locals: one
            // for the procedure, and the rest for the arguments which should already
            // have been pushed onto the stack.
            //
            //
            // When the call returns, execution proceeds with the next
            // instruction There may be any number of values on the return stack;
            // the precise number can be had by subtracting the address of
            // slot `proc - 1` from the post-call SP.
            (op_call, "call", { proc: u24, nlocals: u24 })
            // Call a procedure in the same compilation unit.
            //
            // This instruction is just like "call", except that instead of
            // dereferencing `proc` to find the call target, the call target is
            // known to be at `label`, a signed 32 bit offset from
            // the current IP.
            (op_call_label, "call-label", { proc: u24, nlocals: u24, label: u32 })
            // Tail-call the procedure in slot 0 with the arguments in the current stack frame.
            // Requires that the procedure and all of the arguments have already been
            // shuffled into position
            (op_tail_call, "tail-call", {})
            // Same as `call-label` but now in tail position
            (op_tail_call_label, "tail-call-label", { label: u32 })
            // Return all values from a call frame
            (op_return_values, "return-values", {})
            // Receive a single return value from a call whose procedure was in `proc`,
            // asserting that the call actually returned at least one value. Afterwards,
            // resets the frame to `nlocals` locals.
            (op_receive, "receive", { dest: u16, proc: u16, nlocals: u24 })
            // Receive a return of multiple values from a call whose procedure was
            // in PROC.  If fewer than NVALUES values were returned, signal an
            // error.  Unless ALLOW-EXTRA? is true, require that the number of
            // return values equals NVALUES exactly.  After receive-values has
            // run, the values can be copied down via `mov'.
            (op_receive_values, "receive-values", { proc: u24, allow_extra: bool, nvalues: u24 })
            (op_assert_nargs_ee, "assert-nargs-ee", { n: u24 })
            (op_assert_nargs_ge, "assert-nargs-ge", { n: u24 })
            (op_assert_nargs_le, "assert-nargs-le", { n: u24 })
            (op_assert_nargs_ee_locals, "assert-nargs-ee/locals", { expected: u16, nlocals: u16 })
            (op_check_arguments, "arguments<=?", { expected: u24 })
            (op_check_positional_arguments, "positional-arguments<=?", { nreq: u24, expected: u24 })
            (op_bind_kwards, "bind-kwargs", { nreq: u24, flags: u8, nreq_and_opt: u24, ntotal: u24, kw_offset: u32 })
            (op_bind_rest, "bind-rest", { dst: u24 })
            (op_alloc_frame, "alloc-frame", { nlocals: u24 })
            (op_reset_frame, "reset-frame", { nlocals: u24 })
            (op_mov, "mov", { dst: u16, src: u16 })
            (op_long_mov, "long-mov", { dst: u24, src: u24 })
            (op_long_fmov, "long-fmov", { dst: u24, src: u24 })
            (op_push, "push", { src: u24 })
            (op_pop, "pop", { dst: u24 })
            (op_drop, "drop", { n: u24 })
            (op_shuffle_down, "shuffle-down", { from: u16, to: u16 })
            (op_expand_apply_argument, "expand-apply-argument", {})
            (op_subr_call, "subr-call", { idx: u24 })
            (op_foreign_call, "foreign-call", { cif_idx: u16, ptr_idx: u16 })
            (op_call_intrinsic, "call-intrinsic", { intrinsic: u32 })
            (op_call_intrinsic_val, "call-intrinsic-val", { a: u24, intrinsic: u32 })
            (op_call_intrinsic_val_val, "call-intrinsic-val-val", { a: u16, b: u16, intrinsic: u32 })
            (op_call_intrinsic_ret, "call-intrinsic-ret", { dst: u24, intrinsic: u32 })
            (op_call_intrinsic_ret_val, "call-intrinsic-ret-val", { dst: u16, a: u16, intrinsic: u32 })
            (op_call_intrinsic_ret_val_val, "call-intrinsic-ret-val-val", { dst: u16, a: u16, b: u16, intrinsic: u32 })
            (op_call_intrinsic_val_val_val, "call-intrinsic-val-val-val", { a: u16, b: u16, c: u16, intrinsic: u32 })
            // Reads a value from instruction stream and sets `dst` to it.
            (op_make_immediate, "make-immediate",  { dst: u16, value: u64 })
            // Reads a value from constant pool and sets `dst` to it.
            (op_make_non_immediate, "make-non-immediate", { dst: u24, offset: u32 })
            (op_cons, "cons", { dst: u16, car: u16, cdr: u16 })
            (op_box, "box", { dst: u16, src: u16 })
            (op_box_ref, "box-ref", { dst: u16, src: u16 })
            (op_box_set, "box-set", { dst: u16, src: u16 })

            (op_make_vector, "make-vector", { dst: u16, len: u32 })
            (op_vector_ref, "vector-ref", { dst: u16, src: u16, idx: u16 })
            (op_vector_ref_imm, "vector-ref/immediate", { dst: u16, src: u16, idx: u32 })
            (op_vector_set, "vector-set", { dst: u16, idx: u16, src: u16 })
            (op_vector_set_imm, "vector-set/immediate", { dst: u16, idx: u32, src: u16 })
            (op_vector_length, "vector-length", { dst: u16, src: u16 })
            (op_program_ref, "program-ref", { dst: u16, src: u24, idx: u16 })
            (op_program_ref_imm, "program-ref/immediate", { dst: u16, src: u24, idx: u32 })
            (op_program_set, "program-set", { dst: u24, idx: u16, src: u16 })
            (op_program_set_imm, "program-set/immediate", { dst: u24, idx: u32, src: u16 })

            // Creates a new program object with `vcode` at `offset` and `nfree` free variables.
            (op_make_program, "make-program", { dst: u16, nfree: u32, offset: i32 })
            // Fetches global variable from constant pool and sets `dst` to it.
            (op_global_ref, "global-ref", { dst: u24, offset: u32})
            // Sets global variable from constant pool to `src`.
            (op_global_set, "global-set", { src: u24, offset: u32})
            (op_not, "not", { dst: u16, src: u16 })
            (op_add, "add", { dst: u16, a: u16, b: u16 })
            (op_add_imm, "add/immediate", { dst: u16, a: u16, b: i32 })
            (op_sub, "sub", { dst: u16, a: u16, b: u16 })
            (op_sub_imm, "sub/immediate", { dst: u16, a: u16, b: i32 })
            (op_div, "div", { dst: u16, a: u16, b: u16 })
            (op_quotient, "quotient", { dst: u16, a: u16, b: u16 })
            (op_quotient_imm, "quotient/immediate", { dst: u16, a: u16, b: i32 })
            (op_mul, "mul", { dst: u16, a: u16, b: u16 })
            (op_mul_imm, "mul/immediate", { dst: u16, a: u16, b: i32 })
            (op_mod, "mod", { dst: u16, a: u16, b: u16 })
            (op_mod_imm, "mod/immediate", { dst: u16, a: u16, b: i32 })
            (op_bitand, "bitand", { dst: u16, a: u16, b: u16 })
            (op_bitand_imm, "bitand/immediate", { dst: u16, a: u16, b: i32 })
            (op_bitor, "bitor", { dst: u16, a: u16, b: u16 })
            (op_bitor_imm, "bitor/immediate", { dst: u16, a: u16, b: i32 })
            (op_bitxor, "bitxor", { dst: u16, a: u16, b: u16 })
            (op_bitxor_imm, "bitxor/immediate", { dst: u16, a: u16, b: i32 })
            (op_bitnot, "bitnot", { dst: u16, src: u16 })
            (op_arithmetic_shift, "arithmetic-shift", { dst: u16, a: u16, b: u16 })
            (op_arithmetic_shift_imm, "arithmetic-shift/immediate", { dst: u16, a: u16, b: i32 })
            (op_less, "<", { dst: u16, a: u16, b: u16 })
            (op_less_imm, "</immediate", { dst: u16, a: u16, b: i32 })
            (op_greater, ">", { dst: u16, a: u16, b: u16 })
            (op_greater_imm, ">/immediate", { dst: u16, a: u16, b: i32 })
            (op_less_equal, "<=", { dst: u16, a: u16, b: u16 })
            (op_less_equal_imm, "<=/immediate", { dst: u16, a: u16, b: i32 })
            (op_greater_equal, ">=", { dst: u16, a: u16, b: u16 })
            (op_greater_equal_imm, ">=/immediate", { dst: u16, a: u16, b: i32 })
            (op_numerically_equal, "=", { dst: u16, a: u16, b: u16 })
            (op_equal_imm, "=/immediate", { dst: u16, a: u16, b: i32 })
            (op_eq, "eq?", { dst: u16, a: u16, b: u16 })
            (op_eqv, "eqv?", { dst: u16, a: u16, b: u16 })
            (op_equal, "equal?", { dst: u16, a: u16, b: u16 })
            (op_heap_tag_eq, "heap-tag-eq?", { dst: u16, src: u24, tag: u32} )
            (op_immediate_tag_eq, "immediate-tag-eq?", { dst: u16, src: u24, tag: u32 })
            (op_is_false, "false?", { dst: u16, src: u16 })
            (op_is_null, "null?", { dst: u16, src: u16 })
            (op_is_undefined, "undefined?", { dst: u16, src: u16 })
            (op_is_true, "true?", { dst: u16, src: u16 })
            (op_is_int32, "int32?", { dst: u16, src: u16 })
            (op_is_char, "char?", { dst: u16, src: u16 })
            (op_is_flonum, "flonum?", { dst: u16, src: u16 })

            (op_j, "j", { offset: i32 })
            (op_jz, "jz", { src: u16, offset: i32})
            (op_jnz, "jnz", { src: u16, offset: i32})

            (op_car, "car", { dst: u16, src: u16 })
            (op_cdr, "cdr", { dst: u16, src: u16 })
            (op_bind_optionals, "bind-optionals", { nargs: u24 })

            (op_continuation_call, "continuation-call", { contregs: u8 })
            (op_capture_continuation, "capture-continuation", { dst: u8 })
        }
    };
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

                        gen.write_finish();
                    }
                }

                impl Decode for [<$name: camel>] {
                    #[inline(always)]
                    unsafe fn read(stream: *const u8) -> Self {
                        debug_assert_eq!(stream.wrapping_sub(1).read(), paste::paste!([<$name:upper>]), "expected opcode {} {:x} but found {:x}", stringify!($name), paste::paste!([<$name:upper>]), stream.wrapping_sub(1).read());
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
                            write!(f, " {}", fields.join(" "))?;
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
                                    write!(out, "({})", op)
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

pub fn disassemble<const ADDR_INSN: bool>(vcode: &[u8]) {
    let mut pc = vcode.as_ptr();
    let end = unsafe { pc.add(vcode.len()) };
    let start_pc = pc;
    unsafe {
        while pc < end {
            let start = pc;

            let op = pc.read();
            pc = pc.add(1);
            let mut out = String::new();

            match op {
                op if op == OP_J => {
                    let j = OpJ::read(pc);
                    pc = pc.add(std::mem::size_of::<OpJ>());
                    let target = pc.offset(j.offset as isize);
                    let diff = target.offset_from(start_pc);
                    if !ADDR_INSN {
                        out.push_str(&format!("(j {}) ; => {}", j.offset(), diff));
                    } else {
                        out.push_str(&format!("(j {}) ; => {:p}", j.offset(), target));
                    }
                }

                op if op == OP_JNZ => {
                    let j = OpJnz::read(pc);
                    pc = pc.add(std::mem::size_of::<OpJnz>());
                    let target = pc.offset(j.offset as isize);
                    let diff = target.offset_from(start_pc);
                    if !ADDR_INSN {
                        out.push_str(&format!("(jnz {} {}) ; => {}", j.src(), j.offset(), diff));
                    } else {
                        out.push_str(&format!(
                            "(jnz {} {}) ; => {:p}",
                            j.src(),
                            j.offset(),
                            target
                        ));
                    }
                }

                op if op == OP_JZ => {
                    let j = OpJz::read(pc);
                    pc = pc.add(std::mem::size_of::<OpJz>());
                    let target = pc.offset(j.offset as isize);
                    let diff = target.offset_from(start_pc);
                    if !ADDR_INSN {
                        out.push_str(&format!("(jz {} {}) ; => {}", j.src(), j.offset(), diff));
                    } else {
                        out.push_str(&format!(
                            "(jz {} {}) ; => {:p}",
                            j.src(),
                            j.offset(),
                            target
                        ));
                    }
                }
                op if op == OP_MAKE_IMMEDIATE => {
                    let make_immediate = OpMakeImmediate::read(pc);
                    pc = pc.add(std::mem::size_of::<OpMakeImmediate>());
                    out.push_str(&format!(
                        "(make-immediate {} {:x})",
                        make_immediate.dst(),
                        make_immediate.value()
                    ))
                }

                op if op == OP_MAKE_PROGRAM => {
                    let make_program = OpMakeProgram::read(pc);
                    pc = pc.add(std::mem::size_of::<OpMakeProgram>());
                    let label = pc.offset(make_program.offset() as isize);

                    let diff = label.offset_from(start_pc);
                    if !ADDR_INSN {
                        out.push_str(&format!(
                            "(make-program {} {} {}) ; program at {:<02}",
                            make_program.dst(),
                            make_program.nfree(),
                            make_program.offset(),
                            diff,
                        ));
                    } else {
                        out.push_str(&format!(
                            "(make-program {} {} {}) ; program at {:p}",
                            make_program.dst(),
                            make_program.nfree(),
                            make_program.offset(),
                            label,
                        ));
                    }
                }
                _ => {
                    disassemble_from_stream(op, &mut pc, &mut out).unwrap();
                }
            }
            if !ADDR_INSN {
                let diff = start.offset_from(start_pc);
                println!("{:<02}:\t{}", diff, out);
            } else {
                println!("{:p}:\t{}", start, out);
            }
        }
    }
}

pub const CAPY_BYTECODE_MAGIC: u32 = 0x43504330;

#[repr(C, packed)]
pub struct JITFunctionData {
    pub mcode: u64,
    pub counter: u32,
    pub start: i32,
    pub end: i32,
}

pub fn disassemble_program(value: Value) {
    if !value.is_program() {
        panic!("not a program");
    }

    let vcode = value.cast_as::<ScmProgram>().vcode;

    unsafe {
        let first = vcode.read();
        if first != OP_ENTER {
            eprintln!("<no data>");
        } else {
            let offset = vcode.add(1).cast::<i32>().read();
            let data = vcode.add(5).offset(offset as isize).cast::<JITFunctionData>();

            let start = (*data).start;
            let end = (*data).end;
            let vcode_start = data.cast::<u8>().offset(start as _);
            let vcode_end = data.cast::<u8>().offset(end as _);
            let vcode_len = vcode_end.offset_from(vcode_start) as usize;
            let vcode: &[u8] = std::slice::from_raw_parts(vcode_start, vcode_len);
            disassemble::<true>(vcode)
        }
    }
}