#![allow(dead_code)]
use crate::{vm::thread::Thread, runtime::value::Value, interpreter::stackframe::frame_local};

use super::ir::{ValueId, JITState, self};

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Variable {
    Loaded(ValueId),
    Unloaded,
}

pub struct RecordingInterpreter {
    thread: &'static mut Thread,
    stack: Vec<Variable>,
    sp: isize,
    fp: isize,
    jit: JITState,
}

impl RecordingInterpreter {
    /// Stack grows down in interpreter but here we use a stack that grows up.
    /// So we need to convert `fp + index` to correct index for growing up stack
    fn get_fp_relative_slot(&mut self, index: usize) -> &mut Variable {
        let _addr = self.fp + index as isize;
        todo!()
    }


    unsafe fn sp_ref(&mut self, ix: i32) -> (Value, ValueId) {
        let value = self.thread.interpreter().sp.offset(ix as _).read().as_value;
        let spix = self.sp + ix as isize;
        let ir_ref = match self.stack[spix as usize] {
            Variable::Loaded(ir_ref) => ir_ref,
            Variable::Unloaded => {
                let constant = ir::Value {
                    typ: ir::Type::Fixnum,
                    id: ValueId(0),
                    children: vec![],
                    data: ir::ValueData::Int32(spix as _)
                };
                let constant = self.jit.add_value(constant);
                let load_var = ir::Value {
                    typ: ir::Type::Any,
                    id: ValueId(0),
                    children: vec![constant],
                    data: ir::ValueData::None
                };
                let load_var = self.jit.add_value(load_var);

                self.stack[spix as usize] = Variable::Loaded(load_var);

                load_var
            }
        };

        (value, ir_ref)
    }

    unsafe fn sp_set(&mut self, ix: i32, val: Value, ir_ref: ValueId) {
        self.thread.interpreter().sp.offset(ix as _).cast::<Value>().write(val);
        let spix = self.sp + ix as isize;
        self.stack[spix as usize] = Variable::Loaded(ir_ref);
    }

    fn frame_slot(&mut self, index: usize) -> usize {
        self.fp as usize - index - 1
    }

    unsafe fn fp_set(&mut self, ix: usize, val: Value, ir_ref: ValueId) {
        *frame_local(self.thread.interpreter().fp, ix) = val;
        let ix = self.frame_slot(ix);
        self.stack[ix] = Variable::Loaded(ir_ref);
    }

    unsafe fn alloc_frame(&mut self, num_locals: usize) {
        let sp = self.thread.interpreter().fp.sub(num_locals);
        self.thread.interpreter().sp = sp;

        // grow our stack of IR values now
        let sp = self.fp - num_locals as isize;
        self.sp = sp;

    }
}