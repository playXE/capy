use std::mem::size_of;

use b3::macroassembler::{
    assembler::{
        abstract_macro_assembler::*,
        link_buffer::LinkBuffer,
        macro_assembler_x86_common::{RelationalCondition, ResultCondition},
        TargetMacroAssembler,
    },
    jit::gpr_info::*, wtf::executable_memory_handle::CodeRef,
};
use memoffset::offset_of;

use crate::{
    jit::helpers::AssemblyHelpers,
    runtime::vm::{VMEntryRecord, VM},
};

use super::value::ScmValue;

/// Opaque type representing Scheme call frames.
///
/// Layout:
/// ```mustfail
///      Layout of CallFrame
///
///   |          ......            |   |
///   +----------------------------+   |
///   |           argN             |   v  lower address
///   +----------------------------+
///   |           arg1             |
///   +----------------------------+
///   |           arg0             |
///   +----------------------------+
///   |       argumentCount        |
///   +----------------------------+
///   |          callee            |
///   +----------------------------+
///   |        codeBlock           |
///   +----------------------------+
///   |      return-address        |
///   +----------------------------+
///   |       callerFrame          |
///   +----------------------------+  <- callee's rbp is pointing this address
///   |          local0            |
///   +----------------------------+
///   |          local1            |
///   +----------------------------+
///   |          localN            |
///   +----------------------------+
///   |          ......            |
/// ```

pub struct CallFrame {}

impl CallFrame {
    pub unsafe fn caller_frame_and_pc(frame: *mut CallFrame) -> *mut CallFrameAndPC {
        frame as *mut CallFrameAndPC
    }

    pub unsafe fn return_pc(frame: *mut CallFrame) -> *mut u8 {
        (*Self::caller_frame_and_pc(frame)).return_pc
    }

    pub unsafe fn caller_frame(frame: *mut CallFrame) -> *mut u8 {
        (*Self::caller_frame_and_pc(frame)).return_pc
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(usize)]
pub enum CallFrameSlot {
    CodeBlock = CallFrameAndPC::SIZE_IN_REGISTERS,
    Callee = CallFrameSlot::CodeBlock as usize + 1,
    ArgumentCount = CallFrameSlot::Callee as usize + 1,
    FirstArgument = CallFrameSlot::ArgumentCount as usize + 1,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub struct CallFrameAndPC {
    pub caller_frame: *mut CallFrame,
    pub return_pc: *mut u8,
}

impl CallFrameAndPC {
    pub const SIZE_IN_REGISTERS: usize = 2 * size_of::<usize>() / size_of::<usize>();
}

#[repr(C)]
pub struct ProtoCallFrame<'a> {
    pub code_block: ScmValue,
    pub callee: ScmValue,
    pub arg_count: usize,
    pub padded_arg_count: u32,
    pub args: *const ScmValue,
    pub marker: std::marker::PhantomData<&'a [ScmValue]>,
}

impl<'a> ProtoCallFrame<'a> {
    pub fn new(
        code_block: ScmValue,
        callee: ScmValue,
        arg_count: usize,
        args: &'a [ScmValue],
    ) -> Self {
        let padded_arg_count = arg_count as u32;
        let frame = ProtoCallFrame {
            code_block,
            callee,
            arg_count,
            padded_arg_count,
            args: args.as_ptr(),
            marker: std::marker::PhantomData,
        };
        frame
    }
}

pub fn generate_vm_entry(
    make_call: impl FnOnce(&mut TargetMacroAssembler, u8, u8, u8, u8),
) -> LinkBuffer {
    let mut masm = TargetMacroAssembler::new();

    masm.function_prologue();
    masm.push_callee_saves();

    let entry = ARGUMENT_GPR0;
    let vm = ARGUMENT_GPR1;
    let proto_cfr = ARGUMENT_GPR2;

    masm.vm_entry_record(
        TargetMacroAssembler::FRAME_POINTER_REGISTER,
        TargetMacroAssembler::STACK_POINTER_REGISTER,
    );

    masm.store64(
        vm,
        Address::new(
            TargetMacroAssembler::STACK_POINTER_REGISTER,
            offset_of!(VMEntryRecord, vm) as i32,
        ),
    );
    masm.load64(Address::new(vm, offset_of!(VM, top_call_frame) as i32), T4);
    masm.store64(
        T4,
        Address::new(
            TargetMacroAssembler::STACK_POINTER_REGISTER,
            offset_of!(VMEntryRecord, prev_top_call_frame) as i32,
        ),
    );
    masm.load64(Address::new(vm, offset_of!(VM, top_entry_frame) as i32), T4);
    masm.store64(
        T4,
        Address::new(
            TargetMacroAssembler::STACK_POINTER_REGISTER,
            offset_of!(VMEntryRecord, prev_top_entry_frame) as i32,
        ),
    );
    masm.load64(
        Address::new(proto_cfr, offset_of!(ProtoCallFrame, callee) as i32),
        T4,
    );
    masm.store64(
        T4,
        Address::new(
            TargetMacroAssembler::STACK_POINTER_REGISTER,
            offset_of!(VMEntryRecord, callee) as i32,
        ),
    );

    masm.load32(
        Address::new(
            proto_cfr,
            offset_of!(ProtoCallFrame, padded_arg_count) as i32,
        ),
        T4,
    );
    masm.add64(4i32, T4);
    masm.lshift64(3i32, T4);
    masm.sub64_rrr(
        TargetMacroAssembler::STACK_POINTER_REGISTER,
        T4,
        TargetMacroAssembler::STACK_POINTER_REGISTER,
    );
    masm.mov(3i32, T3);

    {
        let copy_header_loop = masm.label();
        // Copy the CodeBlock/Callee/ArgumentCount from protoCallFrame into the callee frame.
        masm.sub64(1i32, T3);
        masm.load64(
            BaseIndex::new(proto_cfr, T3, Scale::TimesEight, 8, Extend::None),
            T5,
        );
        masm.store64(
            T5,
            BaseIndex::new(
                TargetMacroAssembler::STACK_POINTER_REGISTER,
                T3,
                Scale::TimesEight,
                8,
                Extend::None,
            ),
        );
        let j = masm.branch_test64(ResultCondition::NotZero, T3, T3);
        j.link_to(&mut masm, copy_header_loop);
    }

    masm.load64(
        Address::new(proto_cfr, offset_of!(ProtoCallFrame, arg_count) as _),
        T4,
    );
    masm.sub64(1i32, T4);
    masm.load64(
        Address::new(proto_cfr, offset_of!(ProtoCallFrame, padded_arg_count) as _),
        T5,
    );
    masm.sub64(1i32, T5);

    let jcopy_args = masm.branch64(RelationalCondition::Equal, T4, T5);
    masm.mov(ScmValue::encode_undefined_value().get_raw(), T3);

    {
        let fill_extra_args_loop = masm.label();
        masm.sub64(1i32, T5);
        masm.store64(
            T3,
            BaseIndex::new(
                TargetMacroAssembler::STACK_POINTER_REGISTER,
                T5,
                Scale::TimesEight,
                8,
                Extend::None,
            ),
        );
        let j = masm.branch64(RelationalCondition::NotEqual, T4, T5);
        j.link_to(&mut masm, fill_extra_args_loop);
    }

    {
        let copy_args = masm.label();
        jcopy_args.link_to(&mut masm, copy_args);
        masm.load64(
            Address::new(proto_cfr, offset_of!(ProtoCallFrame, args) as i32),
            T3,
        );

        let copy_args_loop = masm.label();

        let jcopy_args_done = masm.branch_test64(ResultCondition::Zero, T4, T4);

        masm.sub64(1i32, T4);
        masm.load64(
            BaseIndex::new(T3, T4, Scale::TimesEight, 0, Extend::None),
            T5,
        );
        masm.store64(
            T5,
            BaseIndex::new(
                TargetMacroAssembler::STACK_POINTER_REGISTER,
                T4,
                Scale::TimesEight,
                8,
                Extend::None,
            ),
        );
        let j = masm.jump();

        j.link_to(&mut masm, copy_args_loop);
        let copy_args_done = masm.label();
        jcopy_args_done.link_to(&mut masm, copy_args_done);
    }

    masm.store64(
        TargetMacroAssembler::FRAME_POINTER_REGISTER,
        Address::new(vm, offset_of!(VM, top_call_frame) as i32),
    );
    masm.store64(
        TargetMacroAssembler::STACK_POINTER_REGISTER,
        Address::new(vm, offset_of!(VM, top_entry_frame) as i32),
    );

    make_call(&mut masm, entry, proto_cfr, T3, T4);

    masm.vm_entry_record(TargetMacroAssembler::FRAME_POINTER_REGISTER, T4);

    masm.load64(Address::new(T4, offset_of!(VMEntryRecord, vm) as i32), vm);

    masm.load64(
        Address::new(T4, offset_of!(VMEntryRecord, prev_top_call_frame) as i32),
        T2,
    );
    masm.store64(T2, Address::new(vm, offset_of!(VM, top_call_frame) as i32));
    masm.load64(
        Address::new(T4, offset_of!(VMEntryRecord, prev_top_entry_frame) as i32),
        T2,
    );
    masm.store64(T2, Address::new(vm, offset_of!(VM, top_entry_frame) as i32));

    masm.function_epilogue();
    masm.ret();

    LinkBuffer::from_macro_assembler(&mut masm)
}

use once_cell::sync::Lazy;

struct VMEntryRecordCodeRef {
    code_ref: CodeRef,
}

unsafe impl Sync for VMEntryRecordCodeRef {}
unsafe impl Send for VMEntryRecordCodeRef {}

static VM_ENTRY_RECORD: Lazy<VMEntryRecordCodeRef> = Lazy::new(|| {
    let mut masm = TargetMacroAssembler::new();
    masm.vm_entry_record(ARGUMENT_GPR0, RETURN_VALUE_GPR);
    masm.ret();

    let mut lb = LinkBuffer::from_macro_assembler(&mut masm);
    VMEntryRecordCodeRef { code_ref: lb.finalize_without_disassembly() }
});

pub unsafe fn vm_entry_record(entry_frame: *const CallFrame) -> *mut VMEntryRecord {
    let func: extern "C" fn(*const CallFrame) -> *mut VMEntryRecord =
        std::mem::transmute(VM_ENTRY_RECORD.code_ref.start());

    func(entry_frame)
}