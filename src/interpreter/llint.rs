#![allow(dead_code, unused_variables, unused_imports)]
//! Low-level interpreter for CapyVM bytecode.

use std::{
    collections::HashMap,
    mem::{offset_of, size_of, MaybeUninit},
    ptr::{null, null_mut},
};

use macroassembler::{
    assembler::{
        abstract_macro_assembler::{
            AbsoluteAddress, Address, BaseIndex, Extend, Label, Operand, Scale,
        },
        link_buffer::LinkBuffer,
        *,
    },
    jit::gpr_info::*,
    jit::{fpr_info::*, helpers::AssemblyHelpers},
    wtf::executable_memory_handle::CodeRef,
};

use crate::{
    bytecode::opcodes::*,
    runtime::{object::ScmProgram, value::Value},
    vm::thread::Thread,
};

use super::{entry_record::VMEntryRecord, InterpreterState};

static mut LLINT_TRAMPOLINE: MaybeUninit<CodeRef> = MaybeUninit::uninit();
static mut LLINT_ENTRYPOINT: *const u8 = null();
static mut LLINT_RETURN_ADDRESS: *const u8 = null();

pub const THREAD: u8 = CS1;
pub const PC: u8 = CS2;
pub const NUMBER_TAG: u8 = CS3;
pub const NOT_CELL_MASK: u8 = CS4;

pub const DISPATCH_TABLE: u8 = T4;

/// Generates the LLInt trampoline. This piece of code will save all callee-saves
/// and set `NOT_CELL_MASK_REGISTER` and `NUMBER_TAG_REGISTER` to their respective values.
/// Then it jumps to the LLInt entrypoint.
pub fn generate_trampoline() {
    let mut masm = TargetMacroAssembler::new();

    masm.emit_function_prologue();
    masm.push(CS0);
    masm.push(CS1);
    masm.push(CS2);
    masm.push(CS3);
    masm.push(CS4);

    masm.mov(Value::NOT_CELL_MASK, NOT_CELL_MASK);
    masm.mov(Value::NUMBER_TAG, NUMBER_TAG);

    masm.load64(
        AbsoluteAddress::new(unsafe { &LLINT_ENTRYPOINT as *const *const u8 as _ }),
        T0,
    );
    masm.far_jump(T0);

    let ret = masm.label();
    masm.pop(CS4);
    masm.pop(CS3);
    masm.pop(CS2);
    masm.pop(CS1);
    masm.pop(CS0);
    masm.emit_function_epilogue();
    masm.ret();
    unsafe {
        let mut lb = LinkBuffer::from_macro_assembler(&mut masm).unwrap();
        let addr = lb.rx_location_of(ret);
        LLINT_RETURN_ADDRESS = addr;
        let fin = lb.finalize_without_disassembly();
        LLINT_TRAMPOLINE = MaybeUninit::new(fin);
    }
}

static mut LLINT_DISPATCH_TABLE_POINTER: *mut *const u8 = null_mut();
use macroassembler::assembler::abstract_macro_assembler::Call;
pub struct LLIntGenerator {
    masm: TargetMacroAssembler,
    slowpaths: Vec<Box<dyn FnOnce(&mut TargetMacroAssembler)>>,
    labels: HashMap<u8, Label>,

    calls: Vec<Call>,
    llint_entry: Label,
}

impl std::ops::Deref for LLIntGenerator {
    type Target = TargetMacroAssembler;

    fn deref(&self) -> &Self::Target {
        &self.masm
    }
}

impl std::ops::DerefMut for LLIntGenerator {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.masm
    }
}

cfg_if::cfg_if! {
    if #[cfg(any(target_arch="x86_64", target_arch="arm64", target_arch="riscv64"))]
    {
        pub const CALLEE_SAVE_REGISTER_COUNT: usize = 0;
    } else {
        pub const CALLEE_SAVE_REGISTER_COUNT: usize = 0;
    }
}

pub const CALLEE_SAVE_REGISTER_SAVE_SIZE: usize = CALLEE_SAVE_REGISTER_COUNT * size_of::<usize>();

pub const VM_ENTRY_TOTAL_FRAME_SIZE: usize =
    (CALLEE_SAVE_REGISTER_SAVE_SIZE + size_of::<VMEntryRecord>() + 16 - 1) & !(16 - 1);

cfg_if::cfg_if! {
    if #[cfg(any(target_arch="x86_64", target_arch="arm64", target_arch="riscv64"))]
    {
        pub const CALLEE_SAVE_SPACE_AS_REGISTERS: usize = 4;
    } else {
        compile_error!("Unsupported architecture");
    }
}

pub const CALLEE_SAVE_SPACE_STACK_ALIGNED: usize =
    (CALLEE_SAVE_SPACE_AS_REGISTERS * size_of::<usize>() + 16 - 1) & !(16 - 1);

impl LLIntGenerator {
    pub fn new() -> Self {
        Self {
            masm: TargetMacroAssembler::new(),
            slowpaths: vec![],
            labels: HashMap::new(),
            calls: vec![],
            llint_entry: Label::unset(),
        }
    }

    pub fn begin_handler(&mut self, name: &str, op: u8) {
        let label = self.label();
        self.labels.insert(op, label);

        let _ = name;
    }

    pub fn vm_entry_record(&mut self, entry_frame_pointer: u8, result_reg: u8) {
        self.masm.sub64_rrr(
            entry_frame_pointer,
            VM_ENTRY_TOTAL_FRAME_SIZE as i32,
            result_reg,
        );
    }

    pub fn function_prologue(&mut self) {
        #[cfg(target_arch = "x86_64")]
        {
            self.masm
                .push_to_save_gpr(TargetMacroAssembler::FRAME_POINTER_REGISTER);
        }
        #[cfg(any(target_arch = "arm64", target_arch = "riscv64"))]
        {
            self.masm.push_pair(
                TargetMacroAssembler::FRAME_POINTER_REGISTER,
                TargetMacroAssembler::LINK_REGISTER,
            );
        }

        self.masm.mov(
            TargetMacroAssembler::STACK_POINTER_REGISTER,
            TargetMacroAssembler::FRAME_POINTER_REGISTER,
        );
    }

    pub fn function_epilogue(&mut self) {
        self.masm.mov(
            TargetMacroAssembler::FRAME_POINTER_REGISTER,
            TargetMacroAssembler::STACK_POINTER_REGISTER,
        );
        #[cfg(target_arch = "x86_64")]
        {
            self.masm
                .pop_to_restore_gpr(TargetMacroAssembler::FRAME_POINTER_REGISTER);
        }
        #[cfg(any(target_arch = "arm64", target_arch = "riscv64"))]
        {
            self.masm.pop_pair(
                TargetMacroAssembler::FRAME_POINTER_REGISTER,
                TargetMacroAssembler::LINK_REGISTER,
            );
        }
    }

    pub fn push_callee_saves(&mut self) {}
    pub fn pop_callee_saves(&mut self) {}

    pub fn preserve_caller_pc_and_cfr(&mut self) {
        #[cfg(target_arch = "x86_64")]
        {
            self.masm.push(TargetMacroAssembler::FRAME_POINTER_REGISTER);
        }

        #[cfg(any(target_arch = "arm64", target_arch = "riscv64"))]
        {
            self.masm.push_pair(
                TargetMacroAssembler::FRAME_POINTER_REGISTER,
                TargetMacroAssembler::LINK_REGISTER,
            );
        }

        self.masm.mov(
            TargetMacroAssembler::STACK_POINTER_REGISTER,
            TargetMacroAssembler::FRAME_POINTER_REGISTER,
        );
    }

    pub fn restore_caller_pc_and_cfr(&mut self) {
        self.masm.mov(
            TargetMacroAssembler::FRAME_POINTER_REGISTER,
            TargetMacroAssembler::STACK_POINTER_REGISTER,
        );

        #[cfg(target_arch = "x86_64")]
        {
            self.masm.pop(TargetMacroAssembler::FRAME_POINTER_REGISTER);
        }

        #[cfg(any(target_arch = "arm64", target_arch = "riscv64"))]
        {
            self.masm.pop_pair(
                TargetMacroAssembler::FRAME_POINTER_REGISTER,
                TargetMacroAssembler::LINK_REGISTER,
            );
        }
    }

    pub fn preserve_callee_saves_used_by_llint(&mut self) {
        self.masm.sub64(
            CALLEE_SAVE_SPACE_STACK_ALIGNED as i32,
            TargetMacroAssembler::STACK_POINTER_REGISTER,
        );
        cfg_if::cfg_if! {
            if #[cfg(all(target_arch="x86_64", not(windows)))]
            {
                self.masm.store64(CS4, Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -8));
                self.masm.store64(CS3, Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -16));
                self.masm.store64(CS2, Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -24));
                self.masm.store64(CS1, Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -32));
            } else if #[cfg(all(target_arch="x86_64", windows))]
            {
                self.masm.store64(CS6, Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -8));
                self.masm.store64(CS5, Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -16));
                self.masm.store64(CS4, Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -24));
                self.masm.store64(CS3, Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -32));
            } else if #[cfg(target_arch="riscv64")]
            {
                self.masm.store64(CS9, Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -8));
                self.masm.store64(CS8, Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -16));
                self.masm.store64(CS7, Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -24));
                self.masm.store64(CS6, Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -32));
            }
        }
    }

    pub fn restore_callee_saves_used_by_llint(&mut self) {
        cfg_if::cfg_if! {
            if #[cfg(all(target_arch="x86_64", not(windows)))]
            {
                self.masm.load64(Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -8), CS4);
                self.masm.load64(Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -16), CS3);
                self.masm.load64(Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -24), CS2);
                self.masm.load64(Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -32), CS1);
            } else if #[cfg(all(target_arch="x86_64", windows))]
            {
                self.masm.load64(Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -8), CS6);
                self.masm.load64(Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -16), CS5);
                self.masm.load64(Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -24), CS4);
                self.masm.load64(Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -32), CS3);
            } else if #[cfg(target_arch="riscv64")]
            {
                self.masm.load64(Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -8), CS9);
                self.masm.load64(Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -16), CS8);
                self.masm.load64(Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -24), CS7);
                self.masm.load64(Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, -32), CS6);
            }
        }
    }

    pub fn copy_callee_saves_to_entry_frame_callee_saves_buffer(&mut self, entry_frame: u8) {
        self.vm_entry_record(entry_frame, entry_frame);
        self.masm.lea64(
            Address::new(
                entry_frame,
                offset_of!(VMEntryRecord, callee_save_registers_buffer) as i32,
            ),
            entry_frame,
        );

        #[cfg(target_arch = "arm64")]
        {
            self.masm.store_pair64(CS0, CS1, entry_frame, 0);
            self.masm.store_pair64(CS2, CS3, entry_frame, 16);
            self.masm.store_pair64(CS4, CS5, entry_frame, 32);
            self.masm.store_pair64(CS6, CS7, entry_frame, 48);
            self.masm.store_pair64(CS8, CS9, entry_frame, 64);
            self.masm
                .store_Pair64(FPREG_CS0, FPREG_CS1, entry_frame, 80);
            self.masm
                .store_Pair64(FPREG_CS2, FPREG_CS3, entry_frame, 96);
            self.masm
                .store_Pair64(FPREG_CS4, FPREG_CS5, entry_frame, 112);
            self.masm
                .store_Pair64(FPREG_CS6, FPREG_CS7, entry_frame, 128);
        }
        #[cfg(all(target_arch = "x86_64", not(windows)))]
        {
            self.masm.store64(CS0, Address::new(entry_frame, 0));
            self.masm.store64(CS1, Address::new(entry_frame, 8));
            self.masm.store64(CS2, Address::new(entry_frame, 16));
            self.masm.store64(CS3, Address::new(entry_frame, 24));
            self.masm.store64(CS4, Address::new(entry_frame, 32));
        }

        #[cfg(all(target_arch = "x86_64", windows))]
        {
            self.masm.store64(CS0, Address::new(entry_frame, 0));
            self.masm.store64(CS1, Address::new(entry_frame, 8));
            self.masm.store64(CS2, Address::new(entry_frame, 16));
            self.masm.store64(CS3, Address::new(entry_frame, 24));
            self.masm.store64(CS4, Address::new(entry_frame, 32));
            self.masm.store64(CS5, Address::new(entry_frame, 40));
            self.masm.store64(CS6, Address::new(entry_frame, 48));
        }

        #[cfg(target_arch = "riscv64")]
        {
            self.masm.store64(CS0, Address::new(entry_frame, 0));
            self.masm.store64(CS1, Address::new(entry_frame, 8));
            self.masm.store64(CS2, Address::new(entry_frame, 16));
            self.masm.store64(CS3, Address::new(entry_frame, 24));
            self.masm.store64(CS4, Address::new(entry_frame, 32));
            self.masm.store64(CS5, Address::new(entry_frame, 40));
            self.masm.store64(CS6, Address::new(entry_frame, 48));
            self.masm.store64(CS7, Address::new(entry_frame, 56));
            self.masm.store64(CS8, Address::new(entry_frame, 64));
            self.masm.store64(CS9, Address::new(entry_frame, 72));
            self.masm.store64(CS10, Address::new(entry_frame, 80));
            self.masm.store64(FPREG_CS0, Address::new(entry_frame, 88));
            self.masm.store64(FPREG_CS1, Address::new(entry_frame, 96));
            self.masm.store64(FPREG_CS2, Address::new(entry_frame, 104));
            self.masm.store64(FPREG_CS3, Address::new(entry_frame, 112));
            self.masm.store64(FPREG_CS4, Address::new(entry_frame, 120));
            self.masm.store64(FPREG_CS5, Address::new(entry_frame, 128));
            self.masm.store64(FPREG_CS6, Address::new(entry_frame, 136));
            self.masm.store64(FPREG_CS7, Address::new(entry_frame, 144));
            self.masm.store64(FPREG_CS8, Address::new(entry_frame, 152));
            self.masm.store64(FPREG_CS9, Address::new(entry_frame, 160));
            self.masm
                .store64(FPREG_CS10, Address::new(entry_frame, 168));
            self.masm
                .store64(FPREG_CS11, Address::new(entry_frame, 176));
        }
    }

    pub fn copy_callee_saves_to_vm_entry_frame_callee_saves_buffer(&mut self, vm: u8, temp: u8) {
        self.masm.load64(
            Address::new(
                vm,
                offset_of!(Thread, interpreter) as i32
                    + offset_of!(InterpreterState, top_entry_frame) as i32,
            ),
            temp,
        );
        self.copy_callee_saves_to_entry_frame_callee_saves_buffer(temp);
    }

    pub fn restore_callee_saves_from_vm_entry_frame_callee_saves_buffer(
        &mut self,
        vm: u8,
        temp: u8,
    ) {
        self.masm.load64(
            Address::new(
                vm,
                offset_of!(Thread, interpreter) as i32
                    + offset_of!(InterpreterState, top_entry_frame) as i32,
            ),
            temp,
        );
        self.vm_entry_record(temp, temp);

        self.masm.lea64(
            Address::new(
                temp,
                offset_of!(VMEntryRecord, callee_save_registers_buffer) as i32,
            ),
            temp,
        );

        #[cfg(target_arch = "arm64")]
        {
            self.masm.load_pair64(temp, 0, CS0, CS1);
            self.masm.load_pair64(temp, 16, CS2, CS3);
            self.masm.load_pair64(temp, 32, CS4, CS5);
            self.masm.load_pair64(temp, 48, CS6, CS7);
            self.masm.load_pair64(temp, 64, CS8, CS9);
            self.masm.load_Pair64(temp, 80, FPREG_CS0, FPREG_CS1);
            self.masm.load_Pair64(temp, 96, FPREG_CS2, FPREG_CS3);
            self.masm.load_Pair64(temp, 112, FPREG_CS4, FPREG_CS5);
            self.masm.load_Pair64(temp, 128, FPREG_CS6, FPREG_CS7);
        }

        #[cfg(all(target_arch = "x86_64", not(windows)))]
        {
            self.masm.load64(Address::new(temp, 0), CS0);
            self.masm.load64(Address::new(temp, 8), CS1);
            self.masm.load64(Address::new(temp, 16), CS2);
            self.masm.load64(Address::new(temp, 24), CS3);
            self.masm.load64(Address::new(temp, 32), CS4);
        }
        #[cfg(all(target_arch = "x86_64", windows))]
        {
            self.masm.load64(Address::new(temp, 0), CS0);
            self.masm.load64(Address::new(temp, 8), CS1);
            self.masm.load64(Address::new(temp, 16), CS2);
            self.masm.load64(Address::new(temp, 24), CS3);
            self.masm.load64(Address::new(temp, 32), CS4);
            self.masm.load64(Address::new(temp, 40), CS5);
            self.masm.load64(Address::new(temp, 48), CS6);
        }

        #[cfg(target_arch = "riscv64")]
        {
            self.masm.load64(Address::new(temp, 0), CS0);
            self.masm.load64(Address::new(temp, 8), CS1);
            self.masm.load64(Address::new(temp, 16), CS2);
            self.masm.load64(Address::new(temp, 24), CS3);
            self.masm.load64(Address::new(temp, 32), CS4);
            self.masm.load64(Address::new(temp, 40), CS5);
            self.masm.load64(Address::new(temp, 48), CS6);
            self.masm.load64(Address::new(temp, 56), CS7);
            self.masm.load64(Address::new(temp, 64), CS8);
            self.masm.load64(Address::new(temp, 72), CS9);
            self.masm.load64(Address::new(temp, 80), CS10);
            self.masm.load64(Address::new(temp, 88), FPREG_CS0);
            self.masm.load64(Address::new(temp, 96), FPREG_CS1);
            self.masm.load64(Address::new(temp, 104), FPREG_CS2);
            self.masm.load64(Address::new(temp, 112), FPREG_CS3);
            self.masm.load64(Address::new(temp, 120), FPREG_CS4);
            self.masm.load64(Address::new(temp, 128), FPREG_CS5);
            self.masm.load64(Address::new(temp, 136), FPREG_CS6);
            self.masm.load64(Address::new(temp, 144), FPREG_CS7);
            self.masm.load64(Address::new(temp, 152), FPREG_CS8);
            self.masm.load64(Address::new(temp, 160), FPREG_CS9);
            self.masm.load64(Address::new(temp, 168), FPREG_CS10);
            self.masm.load64(Address::new(temp, 176), FPREG_CS11);
        }
    }

    pub fn preserve_return_address_after_call(&mut self, destination_register: u8) {
        #[cfg(target_arch = "x86_64")]
        {
            self.masm.pop(destination_register);
        }

        #[cfg(any(target_arch = "riscv64", target_arch = "arm64"))]
        {
            self.masm
                .mov(TargetMacroAssembler::LINK_REGISTER, destination_register);
        }
    }

    pub fn do_vm_entry(&mut self, mut make_call: impl FnMut(&mut Self)) {
        self.function_prologue();
        self.push_callee_saves();

        let entry = ARGUMENT_GPR0;
        let vm = ARGUMENT_GPR1;
        let proto_call_frame = ARGUMENT_GPR2;

        self.vm_entry_record(
            TargetMacroAssembler::FRAME_POINTER_REGISTER,
            TargetMacroAssembler::STACK_POINTER_REGISTER,
        );

        self.masm.load64(
            Address::new(
                vm,
                offset_of!(Thread, interpreter) as i32
                    + offset_of!(InterpreterState, top_call_frame) as i32,
            ),
            T4,
        );
        self.masm.store64(
            T4,
            Address::new(
                TargetMacroAssembler::STACK_POINTER_REGISTER,
                offset_of!(VMEntryRecord, prev_top_call_frame) as i32,
            ),
        );
        self.masm.load64(
            Address::new(
                vm,
                offset_of!(Thread, interpreter) as i32
                    + offset_of!(InterpreterState, top_entry_frame) as i32,
            ),
            T4,
        );
        self.masm.store64(
            T4,
            Address::new(
                TargetMacroAssembler::STACK_POINTER_REGISTER,
                offset_of!(VMEntryRecord, prev_top_entry_frame) as i32,
            ),
        );

        /*
           TODO: Copy args from proto call frame here
        */

        self.masm.store64(
            TargetMacroAssembler::STACK_POINTER_REGISTER,
            Address::new(
                vm,
                offset_of!(Thread, interpreter) as i32
                    + offset_of!(InterpreterState, top_call_frame) as i32,
            ),
        );
        self.masm.store64(
            TargetMacroAssembler::FRAME_POINTER_REGISTER,
            Address::new(
                vm,
                offset_of!(Thread, interpreter) as i32
                    + offset_of!(InterpreterState, top_entry_frame) as i32,
            ),
        );

        self.masm.mov(vm, THREAD);
        make_call(self);

        self.vm_entry_record(TargetMacroAssembler::FRAME_POINTER_REGISTER, T4);

        self.masm.load64(
            Address::new(T4, offset_of!(VMEntryRecord, prev_top_call_frame) as i32),
            T2,
        );
        self.masm.store64(
            T2,
            Address::new(
                vm,
                offset_of!(Thread, interpreter) as i32
                    + offset_of!(InterpreterState, top_call_frame) as i32,
            ),
        );
        self.masm.load64(
            Address::new(T4, offset_of!(VMEntryRecord, prev_top_entry_frame) as i32),
            T2,
        );
        self.masm.store64(
            T2,
            Address::new(
                vm,
                offset_of!(Thread, interpreter) as i32
                    + offset_of!(InterpreterState, top_entry_frame) as i32,
            ),
        );

        self.masm.sub64_rrr(
            TargetMacroAssembler::FRAME_POINTER_REGISTER,
            CALLEE_SAVE_REGISTER_SAVE_SIZE as i32,
            TargetMacroAssembler::STACK_POINTER_REGISTER,
        );
        self.pop_callee_saves();
        self.function_epilogue();
        self.masm.ret();
    }

    pub fn frame_locals_count(&mut self, dest: u8) {
        self.masm.sub64_rrr(
            TargetMacroAssembler::FRAME_POINTER_REGISTER,
            TargetMacroAssembler::STACK_POINTER_REGISTER,
            dest,
        );
    }

    pub fn frame_locals_count_from(&mut self, slot: u8, dest: u8) {
        self.frame_locals_count(dest);
        self.masm.sub64_rrr(slot, dest, dest);
    }

    pub fn frame_locals_count_from_constant(&mut self, slot: u32, dest: u8) {
        self.frame_locals_count(dest);
        self.masm.sub64_rrr(dest, slot as i32, dest);
    }

    pub fn alloc_frame_const(&mut self, n: usize) {
        self.masm.sub64_rrr(
            TargetMacroAssembler::FRAME_POINTER_REGISTER,
            n as i32,
            TargetMacroAssembler::STACK_POINTER_REGISTER,
        );
    }

    pub fn reset_frame_const(&mut self, n: usize) {
        self.masm.sub64_rrr(
            TargetMacroAssembler::FRAME_POINTER_REGISTER,
            n as i32,
            TargetMacroAssembler::STACK_POINTER_REGISTER,
        );
    }

    pub fn alloc_frame(&mut self, n: u8) {
        self.masm.sub64_rrr(
            TargetMacroAssembler::FRAME_POINTER_REGISTER,
            n,
            TargetMacroAssembler::STACK_POINTER_REGISTER,
        );
    }

    pub fn reset_frame(&mut self, n: u8) {
        self.masm.sub64_rrr(
            TargetMacroAssembler::FRAME_POINTER_REGISTER,
            n,
            TargetMacroAssembler::STACK_POINTER_REGISTER,
        );
    }

    pub fn load_fp_slot(&mut self, index: u8, dest: u8) {
        self.lshift64(3i32, index);
        self.sub64_rrr(index, TargetMacroAssembler::FRAME_POINTER_REGISTER, dest);
        self.load64(Address::new(dest, -8), dest);
    }

    pub fn store_fp_slot(&mut self, src: u8, index: u8, temp: u8) {
        self.lshift64(3i32, index);
        self.sub64_rrr(index, TargetMacroAssembler::FRAME_POINTER_REGISTER, temp);
        self.store64(src, Address::new(temp, -8));
    }

    pub fn load_fp_slot_constant(&mut self, index: isize, dest: u8) {
        let index = (index as i32 * 8) - 8;
        self.load64(
            Address::new(TargetMacroAssembler::FRAME_POINTER_REGISTER, index),
            dest,
        );
    }

    pub fn fp_slot(&mut self, index: u8, dest: u8) {
        self.lshift64(3i32, index);
        self.sub64_rrr(index, TargetMacroAssembler::FRAME_POINTER_REGISTER, dest);
        self.lea64(Address::new(dest, -8), dest);
    }

    pub fn fp_slot_from(&mut self, src: u8, index: u8, dest: u8) {
        self.lshift64(3i32, index);
        self.sub64_rrr(index, TargetMacroAssembler::FRAME_POINTER_REGISTER, dest);
        self.lea64(Address::new(dest, -8), dest);
    }

    pub fn load_variable(&mut self, index: u8, dest: u8) {
        self.load64(
            BaseIndex::new(
                TargetMacroAssembler::STACK_POINTER_REGISTER,
                index,
                Scale::TimesEight,
                0,
                Extend::None,
            ),
            dest,
        );
    }

    pub fn store_variable(&mut self, src: u8, index: u8) {
        self.store64(
            src,
            BaseIndex::new(
                TargetMacroAssembler::STACK_POINTER_REGISTER,
                index,
                Scale::TimesEight,
                0,
                Extend::None,
            ),
        );
    }

    pub fn variable_ref(&mut self, index: u8, dest: u8) {
        #[cfg(target_arch = "x86_64")]
        {
            self.assembler.leaq_mr_scaled(
                0,
                TargetMacroAssembler::STACK_POINTER_REGISTER,
                index,
                Scale::TimesEight as u8,
                dest,
            );
        }

        #[cfg(not(target_arch = "x86_64"))]
        {
            self.lshift64(3i32, index);
            self.add64_rrr(TargetMacroAssembler::STACK_POINTER_REGISTER, index, dest);
        }
    }

    pub fn current_program(&mut self, dest: u8) {
        self.load_fp_slot_constant(0, dest);
    }

    pub fn next_instruction(&mut self) {
        // Load opcode byte from PC
        self.load8(Address::new(PC, 0), T0);

        // FIXME: To cache or not to cache dispatch table in register?
        //self.mov(unsafe { AbsoluteAddress::new(LLINT_DISPATCH_TABLE_POINTER as _) }, T1);
        // jump to corresponding opcode handler
        self.far_jump(BaseIndex::new(T4, T0, Scale::TimesEight, 0, Extend::None));
    }

    /// Dispatch to next instruction. Will add `advance_reg` value to PC.
    pub fn dispatch(&mut self, advance_reg: impl Into<Operand>) {
        self.add64(advance_reg, PC);
        self.next_instruction();
    }

    pub fn op_enter(&mut self) {
        self.begin_handler("enter", OP_ENTER);

        // add size_of::<I32>() + 1 to skip opcode itself
        self.dispatch(size_of::<OpEnter>() as i32 + 1);
    }

    pub fn op_halt(&mut self) {
        self.begin_handler("halt", OP_HALT);

        let frame_size = 2;
        let first_value = frame_size;
        self.frame_locals_count_from_constant(first_value, T0);
        // check if we try to return multiple-values to Rust code
        let multi_values_branch = self.branch64(RelationalCondition::NotEqual, 1i32, T0);
        {
            self.load_fp_slot_constant(first_value as _, T0);
        }
        multi_values_branch.link(&mut **self);
        {
            // TOOD: Multiple values return
            self.illegal_instruction();
        }
        self.restore_callee_saves_used_by_llint();
        self.restore_caller_pc_and_cfr();
        self.ret();
    }

    pub fn op_loop_hint(&mut self) {
        self.begin_handler("loop-hint", OP_LOOP_HINT);

        self.dispatch(size_of::<OpLoopHint>() as i32 + 1);
    }

    pub fn op_call(&mut self) {
        self.begin_handler("call", OP_CALL);

        // Pass thread pointer as argument
        self.mov(THREAD, ARGUMENT_GPR0);
        self.load16(
            Address::new(PC, offset_of!(OpCall, proc) as i32 + 1),
            ARGUMENT_GPR1,
        );
        self.load16(
            Address::new(PC, offset_of!(OpCall, nlocals) as i32 + 1),
            ARGUMENT_GPR2,
        );

        // set SP to point to slot *before* space allocated for call
        self.add64_rrr(ARGUMENT_GPR1, 3i32, T0);
        /*
        | local N            |
        |--------------------| <- sp will be set here so `call` on x64/arm64/riscv64 would set `slot for return PC` correctly
        | slot for return PC |
        |--------------------|
        | slot for caller FP | <- set by `llint_entry` once it is invoked in function prologue
        |--------------------|
        | procedure to call  |
        |--------------------| <-- fp + call_opcode.proc points here
        | argument 1         |

         */
        self.fp_slot(T0, TargetMacroAssembler::STACK_POINTER_REGISTER); // sp = fp.add(proc_slot + 3);

        // pushes RIP to stack, `llint_entry` will push FP and then allocate space for `nlocals`.
        let call_to_entry = self.near_call();
        self.calls.push(call_to_entry); // repatch later to point to `llint_entry`

        self.dispatch(size_of::<OpCall>() as i32 + 1);
    }

    pub fn op_tail_call(&mut self) {
        self.begin_handler("tail-call", OP_TAIL_CALL);
        let j = self.near_tail_call();
        self.calls.push(j);
    }

    pub fn op_shuffle_down(&mut self) {
        self.frame_locals_count(T0);
        let from = T1;
        let to = T2;
        self.load16(
            Address::new(PC, offset_of!(OpShuffleDown, from) as i32 + 1),
            from,
        );
        self.load16(
            Address::new(PC, offset_of!(OpShuffleDown, to) as i32 + 1),
            to,
        );

        self.push_to_save_gpr(THREAD);
        self.push_to_save_gpr(PC);
        let n = PC;
        let tmp = THREAD;

        self.mov(0i32, n);

        let start = self.label();
        self.add64_rrr(from, n, tmp);
        let gt = self.branch64(RelationalCondition::GreaterThanOrEqual, tmp, T0);
        {
            self.load_fp_slot(tmp, tmp);
            self.add64_rrr(to, n, T4);
            self.store_fp_slot(tmp, T4, T4);
            self.add64(1i32, n);
            let j = self.jump();
            j.link_to(&mut **self, start);
        }

        gt.link(&mut **self);

        self.add64_rrr(to, n, to);
        self.reset_frame(to);

        self.pop_to_restore_gpr(PC);
        self.pop_to_restore_gpr(THREAD);
        self.mov(
            unsafe { AbsoluteAddress::new(LLINT_DISPATCH_TABLE_POINTER as _) },
            T4,
        );
    }

    pub fn llint_entry(&mut self) {
        self.function_prologue();
        // allocate slot for `nlocals` from `OP_CALL`
        self.reset_frame(ARGUMENT_GPR2);
        self.load_fp_slot_constant(0, T0);
        // FIXME: Assumes `T0` contains procedure, do check and throw exception when needed later
        self.load64(Address::new(T0, offset_of!(ScmProgram, vcode) as i32), PC);
        self.mov(
            AbsoluteAddress::new(unsafe { LLINT_DISPATCH_TABLE_POINTER as _ }),
            T4,
        );
        self.dispatch(0i32);
    }

    pub fn op_receive(&mut self) {
        let proc = T0;
        self.load16(Address::new(PC, offset_of!(OpReceive, proc) as i32 + 1), proc);
        self.frame_locals_count(T1);

        let check = self.branch64(RelationalCondition::LessThanOrEqual, T1, T0);
        self.load_fp_slot(proc, T1);
        self.load16(Address::new(PC, offset_of!(OpReceive, dest) as i32 + 1), T0);
        self.store_fp_slot(T1, T0, T0);
        self.load16(Address::new(PC, offset_of!(OpReceive, nlocals) as i32 + 1), T0);
        self.reset_frame(T0);

    }
}
