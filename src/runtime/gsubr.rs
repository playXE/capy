use std::{
    mem::size_of,
    ptr::{null_mut, NonNull},
    sync::atomic::{AtomicU32, Ordering},
};

use crate::{
    bytecode::{opcodes::*, u24::u24},
    interpreter::stackframe::StackElement,
    utils::round_up_to_power_of_two,
    vm::{sync::mutex::Mutex, thread::Thread},
};

use super::{environment::scm_define, symbol::scm_intern, value::Value};

struct SubrState {
    subrs: Vec<Subr>,
    names: Vec<&'static str>,
    code_arena: Option<NonNull<CodeArena>>,
}

#[repr(C)]
struct CodeArena {
    next: Option<NonNull<Self>>,
    size: usize,
    used: usize,
    data: [u8; 0],
}

static SUBR_STATE: Mutex<SubrState> = Mutex::new(SubrState {
    subrs: Vec::new(),
    names: Vec::new(),
    code_arena: None,
});

unsafe fn alloc(byte_size: usize) -> *mut u8 {
    let byte_size = round_up_to_power_of_two(byte_size);

    let mut state = SUBR_STATE.lock(true);

    if state
        .code_arena
        .filter(|x| x.as_ref().size.wrapping_sub(x.as_ref().used) < byte_size)
        .is_none()
    {
        let mut chunk_size;
        let avg_code_size = 6 * size_of::<u32>() + size_of::<JITFunctionData>();
        chunk_size = 1500 * avg_code_size;
        if chunk_size < byte_size {
            chunk_size = byte_size;
        }
        let mem = mmtk::memory_manager::malloc(size_of::<CodeArena>() + chunk_size);
        mem.store(CodeArena {
            next: state.code_arena,
            size: chunk_size,
            used: 0,
            data: [],
        });

        state.code_arena = NonNull::new(mem.to_mut_ptr());
    }

    let ret = state
        .code_arena
        .unwrap()
        .as_mut()
        .data
        .as_mut_ptr()
        .add(state.code_arena.unwrap().as_mut().used);
    state.code_arena.unwrap().as_mut().used += byte_size;

    ret.write_bytes(0, byte_size);
    ret
}

pub fn is_primitive_code(ptr: *const u8) -> bool {
    let state = SUBR_STATE.lock(true);

    unsafe {
        let mut current = state.code_arena;

        while let Some(arena) = current.map(|x| x.as_ref()) {
            let start = arena.data.as_ptr();
            let end = start.add(arena.used);

            if ptr >= start && ptr < end {
                return true;
            }

            current = arena.next;
        }
    }

    false
}

pub fn scm_i_alloc_primitive_code_with_instrumentation(
    size: usize,
    write_ptr: &mut *mut u8,
) -> *mut u8 {
    let byte_size = (2 * size) * size_of::<i32>();
    let padded_byte_size = round_up_to_power_of_two(byte_size);
    unsafe {
        let ptr = alloc(padded_byte_size + size_of::<JITFunctionData>());
        let data = ptr.add(padded_byte_size).cast::<JITFunctionData>();

        let ret = ptr;

        ret.write(OP_ENTER);
        ret.add(1).cast::<i32>().write(padded_byte_size as i32 - 5);

        *write_ptr = ret.add(size_of::<i32>() + 1);

        data.write(JITFunctionData {
            mcode: 0,
            counter: 0,
            start: -(padded_byte_size as i32),
            end: -(padded_byte_size as i32 - byte_size as i32),
        });
        ret
    }
}

fn alloc_subr_code(subr_idx: u32, code: &[u8]) -> *const u8 {
    let post = {
        let subr_call = OpSubrCall::new(u24::new(subr_idx));

        let mut buf = [0u8; 1 + size_of::<OpSubrCall>() + 1];
        let mut ptr = buf.as_mut_ptr();
        unsafe {
            ptr.write(OP_SUBR_CALL);
            ptr.add(1).cast::<OpSubrCall>().write(subr_call);
            ptr = ptr.add(1 + size_of::<OpSubrCall>());
            ptr.write(OP_RETURN_VALUES);
        }
        buf
    };
    let mut write = null_mut();
    let ret = scm_i_alloc_primitive_code_with_instrumentation(code.len() + 3, &mut write);

    unsafe {
        write.copy_from_nonoverlapping(code.as_ptr(), code.len());
        write = write.add(code.len());
        write.copy_from_nonoverlapping(post.as_ptr(), post.len());
    }
    ret
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum ArityKind {
    Nullary = 0,
    Req = 1,
    Opt = 2,
    Rest = 4,
    ReqOpt = ArityKind::Req as u8 | ArityKind::Opt as u8,
    OptRest = ArityKind::Opt as u8 | ArityKind::Rest as u8,
    ReqRest = ArityKind::Req as u8 | ArityKind::Rest as u8,
    ReqOptRest = ArityKind::Req as u8 | ArityKind::Opt as u8 | ArityKind::Rest as u8,
}

fn get_subr_stub_code(subr_idx: u32, nreq: usize, nopt: usize, rest: usize) -> *const u8 {
    let mut kind = ArityKind::Nullary as u8;

    if nreq != 0 {
        kind |= ArityKind::Req as u8;
    }

    if nopt != 0 {
        kind |= ArityKind::Opt as u8;
    }

    if rest != 0 {
        kind |= ArityKind::Rest as u8;
    }

    let kind = unsafe { std::mem::transmute::<_, ArityKind>(kind) };

    match kind {
        ArityKind::Nullary | ArityKind::Req => {
            let nargs = u24::new(nreq as u32 + 1);
            alloc_subr_code(
                subr_idx,
                &[OP_ASSERT_NARGS_EE, nargs.0[0], nargs.0[1], nargs.0[2]],
            )
        }

        ArityKind::Opt => {
            let nopt = u24::new(nopt as u32 + 1);
            let code = &[
                OP_ASSERT_NARGS_LE,
                nopt.0[0],
                nopt.0[1],
                nopt.0[2],
                OP_BIND_OPTIONALS,
                nopt.0[0],
                nopt.0[1],
                nopt.0[2],
            ];

            alloc_subr_code(subr_idx, code)
        }

        ArityKind::Rest => {
            let dst = u24::new(1);
            alloc_subr_code(subr_idx, &[OP_BIND_REST, dst.0[0], dst.0[1], dst.0[2]])
        }

        ArityKind::ReqOpt => {
            let nreq_nopt = u24::new(nreq as u32 + nopt as u32 + 1);
            let nreq = u24::new(nreq as u32 + 1);
            let code = &[
                OP_ASSERT_NARGS_GE,
                nreq.0[0],
                nreq.0[1],
                nreq.0[2],
                OP_ASSERT_NARGS_LE,
                nreq_nopt.0[0],
                nreq_nopt.0[1],
                nreq_nopt.0[2],
                OP_BIND_OPTIONALS,
                nreq_nopt.0[0],
                nreq_nopt.0[1],
                nreq_nopt.0[2],
            ];

            alloc_subr_code(subr_idx, code)
        }

        ArityKind::ReqRest => {
            let nreq = u24::new(nreq as u32 + 1);

            let code = &[
                OP_ASSERT_NARGS_GE,
                nreq.0[0],
                nreq.0[1],
                nreq.0[2],
                OP_BIND_REST,
                nreq.0[0],
                nreq.0[1],
                nreq.0[2],
            ];

            alloc_subr_code(subr_idx, code)
        }

        ArityKind::OptRest => {
            let nopt = u24::new(nopt as u32 + 1);

            let code = &[
                OP_BIND_OPTIONALS,
                nopt.0[0],
                nopt.0[1],
                nopt.0[2],
                OP_BIND_REST,
                nopt.0[0],
                nopt.0[1],
                nopt.0[2],
            ];

            alloc_subr_code(subr_idx, code)
        }

        ArityKind::ReqOptRest => {
            let nreq_nopt = u24::new(nreq as u32 + nopt as u32 + 1);
            let nreq = u24::new(nreq as u32 + 1);

            let code = &[
                OP_ASSERT_NARGS_GE,
                nreq.0[0],
                nreq.0[1],
                nreq.0[2],
                OP_BIND_OPTIONALS,
                nreq_nopt.0[0],
                nreq_nopt.0[1],
                nreq_nopt.0[2],
                OP_BIND_REST,
                nreq_nopt.0[0],
                nreq_nopt.0[1],
                nreq_nopt.0[2],
            ];

            alloc_subr_code(subr_idx, code)
        }
    }
}

static NEXT_SUBR_IDX: AtomicU32 = AtomicU32::new(0);

fn alloc_subr_idx(subr: Subr) -> u32 {
    let mut state = SUBR_STATE.lock(true);

    let idx = NEXT_SUBR_IDX.fetch_add(1, Ordering::SeqCst);

    if idx > 0xffffff {
        panic!("too many subrs");
    }

    state.subrs.push(subr);

    idx
}

fn record_subr_name(subr_idx: u32, name: &'static str) {
    let mut state = SUBR_STATE.lock(true);

    if subr_idx >= state.names.len() as u32 {
        state.names.resize(subr_idx as usize + 1, "<unknown>");
    }

    state.names[subr_idx as usize] = name;
}

fn create_subr(
    define: bool,
    name: &'static str,
    nreq: u32,
    nopt: u32,
    rest: u32,
    func: Subr,
) -> Value {
    let idx = alloc_subr_idx(func);
    record_subr_name(idx, name);

    let code = get_subr_stub_code(idx, nreq as _, nopt as _, rest as _);

    let program = Thread::current().make_program::<true>(code, 0);
    if define {
        scm_define(scm_intern(name), program);
    }
    program
}

pub fn scm_define_subr(name: &'static str, nreq: u32, nopt: u32, rest: u32, func: Subr) -> Value {
    create_subr(true, name, nreq, nopt, rest, func)
}

pub fn scm_make_subr(name: &'static str, nreq: u32, nopt: u32, rest: u32, func: Subr) -> Value {
    create_subr(false, name, nreq, nopt, rest, func)
}

#[derive(Clone, Copy)]
pub enum Subr {
    F0(extern "C-unwind" fn(&mut Thread) -> Value),
    F1(extern "C-unwind" fn(&mut Thread, &mut Value) -> Value),
    F2(extern "C-unwind" fn(&mut Thread, &mut Value, &mut Value) -> Value),
    F3(extern "C-unwind" fn(&mut Thread, &mut Value, &mut Value, &mut Value) -> Value),
    F4(extern "C-unwind" fn(&mut Thread, &mut Value, &mut Value, &mut Value, &mut Value) -> Value),
    F5(
        extern "C-unwind" fn(
            &mut Thread,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
        ) -> Value,
    ),
    F6(
        extern "C-unwind" fn(
            &mut Thread,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
        ) -> Value,
    ),
    F7(
        extern "C-unwind" fn(
            &mut Thread,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
        ) -> Value,
    ),
    F8(
        extern "C-unwind" fn(
            &mut Thread,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
        ) -> Value,
    ),
    F9(
        extern "C-unwind" fn(
            &mut Thread,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
        ) -> Value,
    ),
    F10(
        extern "C-unwind" fn(
            &mut Thread,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
            &mut Value,
        ) -> Value,
    ),
}

pub unsafe fn scm_apply_subr(thread: &mut Thread, sp: *mut StackElement, idx: u32) -> Value {
    let subr = SUBR_STATE.lock(true).subrs[idx as usize];

    macro_rules! arg {
        ($i: expr) => {
            &mut (&mut *sp.add($i)).as_value
        };
    }

    match subr {
        Subr::F0(f) => f(thread),
        Subr::F1(f) => f(thread, arg!(0)),
        Subr::F2(f) => f(thread, arg!(1), arg!(0)),
        Subr::F3(f) => f(thread, arg!(2), arg!(1), arg!(0)),
        Subr::F4(f) => f(thread, arg!(3), arg!(2), arg!(1), arg!(0)),
        Subr::F5(f) => f(thread, arg!(4), arg!(3), arg!(2), arg!(1), arg!(0)),
        Subr::F6(f) => f(thread, arg!(5), arg!(4), arg!(3), arg!(2), arg!(1), arg!(0)),
        Subr::F7(f) => f(
            thread,
            arg!(6),
            arg!(5),
            arg!(4),
            arg!(3),
            arg!(2),
            arg!(1),
            arg!(0),
        ),
        Subr::F8(f) => f(
            thread,
            arg!(7),
            arg!(6),
            arg!(5),
            arg!(4),
            arg!(3),
            arg!(2),
            arg!(1),
            arg!(0),
        ),
        Subr::F9(f) => f(
            thread,
            arg!(8),
            arg!(7),
            arg!(6),
            arg!(5),
            arg!(4),
            arg!(3),
            arg!(2),
            arg!(1),
            arg!(0),
        ),
        Subr::F10(f) => f(
            thread,
            arg!(9),
            arg!(8),
            arg!(7),
            arg!(6),
            arg!(5),
            arg!(4),
            arg!(3),
            arg!(2),
            arg!(1),
            arg!(0),
        ),
    }
}
