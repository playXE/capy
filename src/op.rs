use std::marker::PhantomData;

macro_rules! op {
    ($($op: ident $val: expr),*) => {
        $(
            pub const $op: u8 = $val;
        )*

        pub fn op_name(op: u8) -> &'static str {
            match op {
                $($val => stringify!($op),)*
                _ => "UNKNOWN",
            }
        }
    };
}

op! {
    ACC_NULL 0,
    ACC_TRUE 1,
    ACC_FALSE 2,
    ACC_UNDEFINED 3,
    ACC_INT32 4,
    ACC_STORE 5, // Stores the value in the accumulator to the given register
    ACC_LOAD 6, // Loads the value from the given register to the accumulator
    
    ACC_GLOBAL 7,
    ACC_ENV 8,
    ACC_BOX 9, // `AccBox rN` boxes the value in the register `rN` and stores it in the accumulator
    ACC_UNBOX 10, // `AccUnbox rN` unboxes the value in the register `rN` and stores it in the accumulator
    BOX_SET 11, // `BoxSet rN` sets the value in the register `rN` to the value in the accumulator

    ACC_FIELD 12,
    ACC_COMPUTED_FIELD 13,
    SET_FIELD 14,
    SET_COMPUTED_FIELD 15,

    CALL 16,
    TAIL_CALL 17,
    RETURN 18,
    BOOL 19,
    IS_NOT_NULL 20,
    IS_NOT_UNDEFINED 21,
    IS_NULL 22,

    ADD 23,
    SUB 24,
    MUL 25,
    DIV 26,
    MOD 27,
    SHL 28,
    SHR 29,
    USHR 30,
    OR 31,
    AND 32,
    XOR 33,
    NOT 34,
    NEG 35,
    EQ 36,
    NEQ 37,
    LT 38,
    GT 39,
    LTE 40,
    GTE 41,
    INSTANCEOF 42,
    TYPEOF 43,
    CLASSOF 44,

    SET_GLOBAL 45,

    JUMP 46,
    JUMP_IF_FALSE 47,
    JUMP_IF_TRUE 48,

    MAKE_CLOSURE 49,
    
    LAST 128,

    WIDE16 129,
    WIDE32 130
}


pub const OP_NARROW: u8 = 0;
pub const OP_WIDE16: u8 = 1;
pub const OP_WIDE32: u8 = 2;



macro_rules! opcode_decl {
    ($name: ident $op_b: ident => {
        args: {
            $($arg: ident: $arg_ty: ty),*
        },  
        meta: {
            $($meta: ident: $meta_ty: ty),*
        }
    }) => {
        
    };
}


pub trait Fits<T: Copy, const SIZE: u8, const C: bool> {
    type Target;
    fn check(e: T) -> bool;

    fn convert_to_target(e: T) -> Self::Target;
    fn convert_to_source(e: Self::Target) -> T;
}