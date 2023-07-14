use super::{
    fun::scm_make_subr_inliner,
    module::{scm_define, scm_scheme_module},
    object::{ObjectHeader, ScmResult, Tuple, Type, MAX_ARITY},
    symbol::Intern,
    value::Value,
};
use crate::{
    compile::{make_iform, Asm, AsmOperand, IForm},
    op::Opcode,
    vm::callframe::CallFrame,
};
use rsgc::{prelude::Handle, system::arraylist::ArrayList, thread::Thread};

extern "C" fn tuple(cfr: &mut CallFrame) -> ScmResult {
    let t = Thread::current();

    let mut tuple = scm_make_tuple(t, cfr.argument_count());

    for i in 0..cfr.argument_count() {
        t.write_barrier(tuple);
        tuple[i] = cfr.argument(i);
    }

    ScmResult::ok(tuple)
}

extern "C" fn tuple_p(cfr: &mut CallFrame) -> ScmResult {
    ScmResult::ok(cfr.argument(0).is_tuple())
}

extern "C" fn tuple_ref(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument(0).is_tuple()
        && cfr.argument(1).is_int32()
        && cfr.argument(0).tuple().len() > cfr.argument(1).get_int32() as usize
    {
        return ScmResult::ok(cfr.argument(0).tuple()[cfr.argument(1).get_int32() as usize]);
    }

    ScmResult::ok(false)
}

extern "C" fn tuple_set(cfr: &mut CallFrame) -> ScmResult {
    if cfr.argument(0).is_tuple()
        && cfr.argument(1).is_int32()
        && cfr.argument(0).tuple().len() > cfr.argument(1).get_int32() as usize
    {
        let mut tuple = cfr.argument(0).tuple();
        Thread::current().write_barrier(tuple);
        tuple[cfr.argument(1).get_int32() as usize] = cfr.argument(2);
        return ScmResult::ok(tuple);
    }

    ScmResult::ok(false)
}

pub fn scm_make_tuple(thread: &mut Thread, size: usize) -> Handle<Tuple> {
    let mut vec = thread.allocate_varsize::<Tuple>(size);

    unsafe {
        let v = vec.assume_init_mut();
        v.object = ObjectHeader::new(Type::Tuple);
        for i in 0..size {
            v.data.as_mut_ptr().add(i).write(Value::encode_null_value());
        }
        vec.assume_init()
    }
}

pub fn scm_make_tuple_from_slice(thread: &mut Thread, slice: &[Value]) -> Handle<Tuple> {
    let mut vec = thread.allocate_varsize::<Tuple>(slice.len());

    unsafe {
        let v = vec.assume_init_mut();
        v.object = ObjectHeader::new(Type::Tuple);
        for i in 0..slice.len() {
            v.data.as_mut_ptr().add(i).write(slice[i]);
        }
        vec.assume_init()
    }
}

pub(crate) fn init_tuple() {
    let module = scm_scheme_module().module();

    let subr = scm_make_subr_inliner("tuple", tuple, 0, MAX_ARITY, |iforms, _| {
        if iforms.len() >= u16::MAX as usize {
            return None;
        }

        let operands =
            ArrayList::from_slice(Thread::current(), &[AsmOperand::I16(iforms.len() as _)]);

        Some(make_iform(IForm::Asm(Asm {
            op: Opcode::Tuple,
            args: ArrayList::from_slice(Thread::current(), iforms),
            operands: Some(operands),
            exits: false,
            pushes: true,
            ic: false,
        })))
    });
    scm_define(module, "tuple".intern(), subr).unwrap();

    let subr = scm_make_subr_inliner("tuple?", tuple_p, 1, 1, |iforms, _| {
        if iforms.len() != 1 {
            return None;
        }

        Some(make_iform(IForm::Asm(Asm {
            op: Opcode::IsTuple,
            args: ArrayList::from_slice(Thread::current(), iforms),
            operands: None,
            exits: false,
            pushes: true,
            ic: false,
        })))
    });
    scm_define(module, "tuple?".intern(), subr).unwrap();

    let subr = scm_make_subr_inliner("tuple-ref", tuple_ref, 2, 2, |iforms, _| {
        if iforms.len() != 2 {
            return None;
        }
        match &*iforms[1] {
            IForm::Const(x) => {
                if x.is_int32() {
                    if x.get_int32() < u16::MAX as i32 && x.get_int32() >= 0 {
                        return Some(make_iform(IForm::Asm(Asm {
                            op: Opcode::TupleRefI,
                            args: ArrayList::from_slice(Thread::current(), &[iforms[0]]),
                            operands: Some(ArrayList::from_slice(
                                Thread::current(),
                                &[AsmOperand::I16(x.get_int32() as _)],
                            )),
                            exits: false,
                            pushes: true,
                            ic: false,
                        })));
                    }
                }
            }

            _ => (),
        }

        Some(make_iform(IForm::Asm(Asm {
            op: Opcode::TupleRef,
            args: ArrayList::from_slice(Thread::current(), iforms),
            operands: None,
            exits: false,
            pushes: true,
            ic: false,
        })))
    });
    scm_define(module, "tuple-ref".intern(), subr).unwrap();

    let subr = scm_make_subr_inliner("tuple-set!", tuple_set, 3, 3, |iforms, _| {
        if iforms.len() != 3 {
            return None;
        }
        match &*iforms[1] {
            IForm::Const(x) => {
               
                if x.is_int32() {
                    if x.get_int32() < u16::MAX as i32 && x.get_int32() >= 0 {
                       
                        return Some(make_iform(IForm::Asm(Asm {
                            op: Opcode::TupleSetI,
                            args: ArrayList::from_slice(Thread::current(), &[iforms[0], iforms[2]]),
                            operands: Some(ArrayList::from_slice(
                                Thread::current(),
                                &[AsmOperand::I16(x.get_int32() as _)],
                            )),
                            exits: false,
                            pushes: false,
                            ic: false,
                        })));
                    }
                }
            }

            _ => (),
        }

        Some(make_iform(IForm::Asm(Asm {
            op: Opcode::TupleSet,
            args: ArrayList::from_slice(Thread::current(), iforms),
            operands: None,
            exits: false,
            pushes: false,
            ic: false,
        })))
    });
    scm_define(module, "tuple-set!".intern(), subr).unwrap();
}
