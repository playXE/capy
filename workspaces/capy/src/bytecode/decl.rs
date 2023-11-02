#[macro_export]
macro_rules! for_each_opcode {
    ($m: ident) => {
        $m! {
            (op_halt,                       "halt",                     {}),
            (op_enter,                      "enter",                    { data: i32 }),
            (op_loop_hint,                  "loop-hint",                { data: i32 }),
            (op_call,                       "call",                     { proc: u16, nlocals: u16 }),
            (op_tail_call,                  "tail-call",                { }),
            (op_call_label,                 "call-label",               { offset: i32 }),
            (op_tail_call_label,            "tail-call-label",          { offset: i32 }),
            (op_return_values,              "return-values",            { }),
            (op_receive,                    "receive",                  { dst: u16, proc: u16, nlocals: u16 }),
            (op_recieve_values,             "receive-values",           { proc: u16, allow_extra: bool, nvalues: u16 }),
            (op_assert_args_ee,             "assert-args-ee",           { expected: u16 }),
            (op_assert_args_ge,             "assert-args-ge",           { expected: u16 }),
            (op_assert_args_le,             "assert-args-le",           { expected: u16 }),
            (op_check_arguments,            "arguments<=?",             { expected: u16 }),
            (op_check_positional_arguments, "positional-arguments<=?",  { expected: u16 }),
            (op_bind_rest,                  "bind-rest",                { dst: u16 }),
            (op_alloc_frame,                "alloc-frame",              { nlocals: u16 }),
            (op_reset_frame,                "reset-frame",              { nlocals: u16 }),   
            (op_mov,                        "mov",                      { dst: u16, src: u16 }),
            (op_fmov,                       "fmov",                     { dst: u16, src: u16 }),
            (op_shuffle_down,               "shuffle-down",             { from: u16, to: u16 }),
            (op_expand_apply_argument,      "expand-apply-argument",    { }),
            (op_subr_call,                  "subr-call",                { idx: u32 }),
            (op_continuation_call,          "continuation-call",        { contregs: u16 }),
            (op_capture_continuation,       "capture-continuation",     { dst: u16 }),
            (op_make_fixnum,                "make-fixnum",              { dst: u16, value: i32 }),
            (op_make_immediate,             "make-immediate",           { dst: u16, value: u64 }),
            (op_make_non_immediate,         "make-non-immediate",       { dst: u16, offset: i32 }),
            (op_allocate_words,             "allocate-words",           { dst: u16, size: u16 }),
            (op_allocate_words_immediate,   "allocate-words/immediate", { dst: u16, size: u16 }),
            (op_scm_ref,                    "scm-ref",                  { dst: u16, obj: u16, idx: u16 }),
            (op_scm_set,                    "scm-set!",                 { obj: u16, idx: u16, val: u16 }),
            (op_scm_ref_immediate,          "scm-ref/immediate",        { dst: u16, obj: u16, idx: u16 }),
            (op_scm_set_immediate,          "scm-set!/immediate",       { obj: u16, idx: u16, val: u16 }),
            (op_add,                        "+",                        { dst: u16, lhs: u16, rhs: u16 }),
            (op_sub,                        "-",                        { dst: u16, lhs: u16, rhs: u16 }),
            (op_mul,                        "*",                        { dst: u16, lhs: u16, rhs: u16 }),
            (op_div,                        "/",                        { dst: u16, lhs: u16, rhs: u16 }),
            (op_quotient,                   "quotient",                 { dst: u16, lhs: u16, rhs: u16 }),
            (op_remainder,                  "remainder",                { dst: u16, lhs: u16, rhs: u16 }),
            (op_numerically_equal,          "=",                        { dst: u16, lhs: u16, rhs: u16 }),
            (op_numerically_less,           "<",                        { dst: u16, lhs: u16, rhs: u16 }),
            (op_numerically_greater,        ">",                        { dst: u16, lhs: u16, rhs: u16 }),
            (op_numerically_less_equal,     "<=",                       { dst: u16, lhs: u16, rhs: u16 }),
            (op_numerically_greater_equal,  ">=",                       { dst: u16, lhs: u16, rhs: u16 }),
            (op_eq,                         "eq?",                      { dst: u16, lhs: u16, rhs: u16 }),
            (op_eqv,                        "eqv?",                     { dst: u16, lhs: u16, rhs: u16 }),
            (op_static_ref,                 "static-ref",               { dst: u16, offset: i32 }),
            (op_static_set,                 "static-set!",              { src: u16, offset: i32 }),
            (op_static_patch,               "static-path",              { dst_offset: i32, src_offset: i32 }),
            (op_heap_tag,                   "cell-tag",                 { dst: u16, src: u16 }),
            (op_set_heap_tag,               "set-cell-tag!",            { dst: u16, src: u16 }),
            (op_check_heap_tag,             "cell-tag=?",               { dst: u16, src: u16, tag: u16 }),
            (op_j,                          "j",                        { offset: i32 }),
            (op_brtrue,                     "br.true",                  { src: u16, offset: i32 }),
            (op_brfalse,                    "br.false",                 { src: u16, offset: i32 }),
            (op_jtable,                     "jtable",                   { idx: u16, len: u32, offset: i32 }),
            (op_eq_immediate,               "eq-immediate?",            { dst: u16, src: u16, imm: u16 }),
            (op_u8_ref,                     "u8-ref",                   { dst: u16, src: u16, idx: u16 }),
            (op_u8_set,                     "u8-set!",                  { src: u16, idx: u16, val: u16 }),
            (op_u16_ref,                    "u16-ref",                  { dst: u16, src: u16, idx: u16 }),
            (op_u16_set,                    "u16-set!",                 { src: u16, idx: u16, val: u16 }),
            (op_u32_ref,                    "u32-ref",                  { dst: u16, src: u16, idx: u16 }),
            (op_u32_set,                    "u32-set!",                 { src: u16, idx: u16, val: u16 }),
            (op_u64_ref,                    "u64-ref",                  { dst: u16, src: u16, idx: u16 }),
            (op_u64_set,                    "u64-set!",                 { src: u16, idx: u16, val: u16 }),
            (op_s8_ref,                     "s8-ref",                   { dst: u16, src: u16, idx: u16 }),
            (op_s8_set,                     "s8-set!",                  { src: u16, idx: u16, val: u16 }),
            (op_s16_ref,                    "s16-ref",                  { dst: u16, src: u16, idx: u16 }),
            (op_s16_set,                    "s16-set!",                 { src: u16, idx: u16, val: u16 }),
            (op_s32_ref,                    "s32-ref",                  { dst: u16, src: u16, idx: u16 }),
            (op_s32_set,                    "s32-set!",                 { src: u16, idx: u16, val: u16 }),
            (op_s64_ref,                    "s64-ref",                  { dst: u16, src: u16, idx: u16 }),
            (op_s64_set,                    "s64-set!",                 { src: u16, idx: u16, val: u16 }),
            (op_f64_ref,                    "f64-ref",                  { dst: u16, src: u16, idx: u16 }),
            (op_f64_set,                    "f64-set!",                 { src: u16, idx: u16, val: u16 }),
            (op_f32_ref,                    "f32-ref",                  { dst: u16, src: u16, idx: u16 }),
            (op_f32_set,                    "f32-set!",                 { src: u16, idx: u16, val: u16 }),
            (op_throw_value,                "throw/value",              { val: u16, key_subr_and_message: i32 }),
            (op_throw_value_data,           "throw/value+data",         { val: u16, key_subr_and_message: i32 }),
            (op_word_ref,                   "word-ref",                 { dst: u16, src: u16, idx: u16 }),
            (op_word_set,                   "word-set!",                { src: u16, idx: u16, val: u16 }),
            (op_word_ref_immediate,         "word-ref/immediate",       { dst: u16, src: u16, idx: u16 }),
            (op_word_set_immediate,         "word-set!/immediate",      { src: u16, idx: u16, val: u16 }),
            (op_tag_char,                   "tag-char",                 { dst: u16, src: u16 }),
            (op_untag_char,                 "untag-char",               { dst: u16, src: u16 }),
            (op_tag_fixnum,                 "tag-fixnum",               { dst: u16, src: u16 }),
            (op_untag_fixnum,               "untag-fixnum",             { dst: u16, src: u16 }),
            (op_tag_f64,                    "tag-f64",                  { dst: u16, src: u16 }),
            (op_untag_f64,                  "untag-f64",                { dst: u16, src: u16 }),
            (op_uadd,                       "uadd",                     { dst: u8, lhs: u8, rhs: u8 }),
            (op_usub,                       "usub",                     { dst: u8, lhs: u8, rhs: u8 }),
            (op_umul,                       "umul",                     { dst: u8, lhs: u8, rhs: u8 }),
            (op_uadd_immediate,             "uadd/immediate",           { dst: u8, lhs: u8, rhs: u8 }),
            (op_usub_immediate,             "usub/immediate",           { dst: u8, lhs: u8, rhs: u8 }),
            (op_umul_immediate,             "umul/immediate",           { dst: u8, lhs: u8, rhs: u8 }),
            (op_ulogand,                    "ulogand",                  { dst: u8, lhs: u8, rhs: u8 }),
            (op_ulogior,                    "ulogior",                  { dst: u8, lhs: u8, rhs: u8 }),
            (op_ulogsub,                    "ulogsub",                  { dst: u8, lhs: u8, rhs: u8 }),
            (op_ulogxor,                    "ulogxor",                  { dst: u8, lhs: u8, rhs: u8 }),
            (op_ursh,                       "ursh",                     { dst: u8, lhs: u8, rhs: u8 }),
            (op_srsh,                       "srsh",                     { dst: u8, lhs: u8, rhs: u8 }),
            (op_ulsh,                       "ulsh",                     { dst: u8, lhs: u8, rhs: u8 }),
            (op_ursh_immediate,             "ursh/immediate",           { dst: u8, lhs: u8, rhs: u8 }),
            (op_srsh_immediate,             "srsh/immediate",           { dst: u8, lhs: u8, rhs: u8 }),
            (op_ulsh_immediate,             "ulsh/immediate",           { dst: u8, lhs: u8, rhs: u8 }),
            (op_u64_eq,                     "u64=?",                    { dst: u8, lhs: u8, rhs: u8 }),
            (op_u64_lt,                     "u64<?",                    { dst: u8, lhs: u8, rhs: u8 }),
            (op_u64_gt,                     "u64>?",                    { dst: u8, lhs: u8, rhs: u8 }),
            (op_u64_le,                     "u64<=?",                   { dst: u8, lhs: u8, rhs: u8 }),
            (op_u64_ge,                     "u64>=?",                   { dst: u8, lhs: u8, rhs: u8 }),
            (op_s64_to_f64,                 "s64->f64",                 { dst: u16, src: u16 }),
            (op_call_scm_scm,               "call-scm-scm",             { a: u8, b: u8, idx: u32}),
            (op_call_scm_scm_scm,           "call-scm-scm-scm",         { a: u8, b: u8, c: u8, idx: u32}),
            (op_call_scm_uimm_scm,          "call-scm-uimm-scm",        { a: u8, b: u8, imm: u8, idx: u32}),
            (op_call_scm_from_scmn_scmn,    "call-scm<-scmn-scmn",      { dst: u16, a: i32, b: i32, idx: u32}),
            (op_call_scm,                   "call-scm",                 { a: u16, idx: u32}),
            (op_call_scm_sz_u32,            "call-scm-sz-u32",          { a: u8, b: u8, c: u8, idx: u32}),
            (op_call_scm_from,              "call-scm<-",               { dst: u16, idx: u32 }),
            (op_call_s64_from_scm,          "call-s64<-scm",            { dst: u16, a: u8, idx: u32 }),
            (op_call_scm_from_u64,          "call-scm<-u64",            { dst: u16, a: u8, idx: u32 }),
            (op_call_scm_from_s64,          "call-scm<-s64",            { dst: u16, a: u8, idx: u32 }),
            (op_call_scm_from_scm,          "call-scm<-scm",            { dst: u16, a: u8, idx: u32 }),
            (op_call_f64_from_scm,          "call-f64<-scm",            { dst: u16, a: u8, idx: u32 }),
            (op_call_u64_from_scm,          "call-u64<-scm",            { dst: u16, a: u8, idx: u32 }),
            (op_call_scm_from_scm_scm,      "call-scm<-scm-scm",        { dst: u16, a: u8, b: u8, idx: u32 }),
            (op_call_scm_from_scm_uimm,     "call-scm<-scm-uimm",       { dst: u16, a:u8, b: u8,  idx: u32 }),
            (op_call_scm_from_scm_u64,      "call-scm<-scm-u64",        { dst: u16, a: u8, b: u8, idx: u32 }),
            (op_last,                       "last",                     { } ),
        }
    }
}

macro_rules! decl_opcodes {
    ($(($name: ident, $str: literal, { $($field: ident : $t: ty),* }),)+) => {
        $(
            paste::paste! {
                #[repr(C, packed)]
                #[derive(Copy, Clone, PartialEq, Eq, Debug)]
                pub struct [<$name: camel>] {
                    pub opcode: u8,
                    $(pub $field: $t),*
                }

                impl [<$name: camel>] {
                    pub const NAME: &'static str = $str;
                    #[inline(always)]
                    pub const fn new($($field: $t),*) -> Self {
                        Self {
                            opcode: paste::paste!([<$name:upper>]),
                            $($field),*
                        }
                    }
                    #[inline(always)]
                    pub const fn opcode(&self) -> u8 {
                        //self.opcode
                        paste::paste!([<$name:upper>])
                    }

                    $(
                        #[inline(always)]
                        pub const fn $field(&self) -> $t {
                            self.$field
                        }
                    )*
                    #[allow(unused_parens, unused_variables)]
                    #[inline(always)]
                    pub unsafe fn decode(stream: &mut *const u8) -> ($($t),*) {
                        let op = stream.read();
                        debug_assert_eq!(op, paste::paste!([<$name:upper>]), "expected opcode {} {:x} but found {:x}", stringify!($name), paste::paste!([<$name:upper>]), op);
                        let op = stream.cast::<[<$name: camel>]>();
                        *stream = stream.add(std::mem::size_of::<[<$name: camel>]>());
                        ( $(
                            (*op).$field()
                        ),* )
                    }
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
                        debug_assert_eq!(stream.read(), paste::paste!([<$name:upper>]), "expected opcode {} {:x} but found {:x}", stringify!($name), paste::paste!([<$name:upper>]), stream.wrapping_sub(1).read());
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
    (($name: ident, $str: literal, {$($field: ident : $t: ty),*}), $($rest:tt)*) => {

        paste::paste! { pub const [<$name:upper>]:  u8 = 0; }
        decl_constants!(@parse 1, $($rest)*);
    };

    (@parse $cursor: expr, ($name: ident, $str: literal, {$($field: ident : $t: ty),*}), $($rest:tt)*) => {
        paste::paste! {
            pub const [<$name:upper>]: u8 = $cursor;
        }
        decl_constants!(@parse $cursor + 1, $($rest)*);
    };
    (@parse $cursor: expr, ) => {}
}

for_each_opcode!(decl_opcodes);
for_each_opcode!(decl_constants);
pub trait Encode {
    fn write(&self, gen: &mut impl InstructionStream);
}

pub trait Decode: Sized {
    unsafe fn read(stream: *const u8) -> Self;
    unsafe fn read_ref<'a>(stream: *const u8) -> &'a Self {
        &*stream.cast::<Self>()
    }
}

impl Encode for u8 {
    fn write(&self, gen: &mut impl InstructionStream) {
        gen.write_u8(*self);
    }
}

impl Encode for u16 {
    fn write(&self, gen: &mut impl InstructionStream) {
        gen.write_u16(*self);
    }
}

impl Encode for i32 {
    fn write(&self, gen: &mut impl InstructionStream) {
        gen.write_u32(*self as u32);
    }
}

impl Encode for i16 {
    fn write(&self, gen: &mut impl InstructionStream) {
        gen.write_u16(*self as u16);
    }
}

impl Encode for i8 {
    fn write(&self, gen: &mut impl InstructionStream) {
        gen.write_u8(*self as u8);
    }
}

impl Encode for u32 {
    fn write(&self, gen: &mut impl InstructionStream) {
        gen.write_u32(*self);
    }
}

impl Decode for u32 {
    unsafe fn read(stream: *const u8) -> Self {
        stream.cast::<u32>().read()
    }
}

impl Encode for bool {
    fn write(&self, gen: &mut impl InstructionStream) {
        gen.write_u8(*self as u8);
    }
}

impl Encode for u64 {
    fn write(&self, gen: &mut impl InstructionStream) {
        gen.write_u32(*self as u32);
        gen.write_u32((*self >> 32) as u32);
    }
}

impl Decode for u64 {
    unsafe fn read(stream: *const u8) -> Self {
        stream.cast::<u64>().read()
    }
}

pub trait InstructionStream {
    fn write_u8(&mut self, value: u8);
    fn write_u16(&mut self, value: u16);
    fn write_u32(&mut self, value: u32);
    fn write_finish(&mut self) {}
}