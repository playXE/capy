use crate::runtime::{value::{TaggedValue, Tagged}, cell::CellReference, code_block::CodeBlock};

use super::register::{Register, AsBits};

#[repr(C)]
pub struct ProtoCallFrame<'a> {
    pub code_block_value: Register,
    pub callee_value: Register,
    pub arg_count_and_code_origin_value: Register,
    pub padded_arg_count: u32,
    pub args: *mut TaggedValue,
    pub marker: std::marker::PhantomData<&'a [TaggedValue]>,
}

impl<'a> ProtoCallFrame<'a> {
    pub fn clear_current_vpc(&mut self) {
        *self.arg_count_and_code_origin_value.tag_mut() = 0;
    }

    pub fn set_arg_count(&mut self, arg_count: u32) {
        *self.arg_count_and_code_origin_value.payload_mut() = arg_count;
    }

    pub fn argument_count(&self) -> usize {
        self.arg_count_and_code_origin_value.payload() as _ 
    }   

    pub fn argument(&self, index: usize) -> TaggedValue {
        assert!(index < self.argument_count());
        unsafe { *self.args.add(index) }
    }

    pub fn set_argument(&mut self, index: usize, value: TaggedValue) {
        assert!(index < self.argument_count());
        unsafe { *self.args.add(index) = value }
    }

    pub fn new(code_block: Tagged<CellReference<CodeBlock>>, callee: TaggedValue, arg_count: u32, other_args: &'a mut [TaggedValue]) -> Self {
        Self {
            code_block_value: Register { code_block },
            callee_value: Register { compressed: callee },
            arg_count_and_code_origin_value: Register {
                bits: AsBits {
                    payload: arg_count,
                    tag: 0,
                }
            },
            padded_arg_count: arg_count,
            args: other_args.as_mut_ptr(),
            marker: std::marker::PhantomData,
        }
    }

    pub fn callee(&self) -> TaggedValue {
        unsafe { self.callee_value.compressed }
    }

    pub fn set_callee(&mut self, callee: TaggedValue) {
        self.callee_value.compressed = callee;
    }

    pub fn code_block(&self) -> Tagged<CellReference<CodeBlock>> {
       unsafe { self.code_block_value.code_block } 
    }

    pub fn set_code_block(&mut self, code_block: Tagged<CellReference<CodeBlock>>) {
        self.code_block_value.code_block = code_block;
    }




}
