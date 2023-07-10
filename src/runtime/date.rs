use std::mem::MaybeUninit;

use rsgc::thread::Thread;

use crate::vm::{callframe::CallFrame, interpreter::apply_list};

use super::{
    error::wrong_contract,
    fun::scm_make_subr,
    list::{scm_is_list, scm_list},
    module::{scm_capy_module, scm_define},
    object::ScmResult,
    symbol::Intern,
    value::scm_int,
    vector::make_values,
};

pub fn scm_get_inexact_monotonic_milliseconds() -> f64 {
    let mut tp: MaybeUninit<libc::timespec> = MaybeUninit::uninit();

    unsafe {
        if libc::clock_gettime(libc::CLOCK_MONOTONIC, tp.as_mut_ptr()) == 0 {
            let tp = tp.assume_init();
            (tp.tv_sec as f64) * 1000.0 + (tp.tv_nsec as f64) / 1_000_000.0
        } else {
            scm_get_inexact_milliseconds()
        }
    }
}

pub fn scm_get_inexact_milliseconds() -> f64 {
    let mut now: MaybeUninit<libc::timeval> = MaybeUninit::uninit();

    unsafe {
        if libc::gettimeofday(now.as_mut_ptr(), std::ptr::null_mut()) == 0 {
            let now = now.assume_init();
            (now.tv_sec as f64) * 1000.0 + (now.tv_usec as f64) / 1000.0
        } else {
            0.0
        }
    }
}

pub fn scm_get_thread_milliseconds() -> usize {
    #[cfg(windows)]
    {
        return 0;
    }

    #[cfg(not(windows))]
    {
        let mut rusage: MaybeUninit<libc::rusage> = MaybeUninit::uninit();

        unsafe {
            if libc::getrusage(libc::RUSAGE_THREAD, rusage.as_mut_ptr()) == 0 {
                let usage = rusage.assume_init();
                let s = usage.ru_utime.tv_sec as u64 + usage.ru_stime.tv_sec as u64;
                let u = usage.ru_utime.tv_usec as u64 + usage.ru_stime.tv_usec as u64;

                return (s * 1000 + u / 1000) as usize;
            } else {
                usize::MAX
            }
        }
    }
}

extern "C" fn time_apply(cfr: &mut CallFrame) -> ScmResult {
    let proc = cfr.argument(0);
    let lst = cfr.argument(1);

    if !proc.is_procedure() {
        return wrong_contract::<()>("time-apply", "procedure?", 0, 2, cfr.arguments()).into();
    }

    if !scm_is_list(lst) {
        return wrong_contract::<()>("time-apply", "list?", 1, 2, cfr.arguments()).into();
    }

    let start = scm_get_inexact_monotonic_milliseconds();
    let cpustart = scm_get_thread_milliseconds();
    let mut v = apply_list(proc, lst)?;
    let cpuend = scm_get_thread_milliseconds();
    let end = scm_get_inexact_monotonic_milliseconds();

    let dur = end - start;
    let cpudur = cpuend - cpustart;

    if v.is_values() {
        v = scm_list(Thread::current(), &v.values())
    } else {
        v = scm_list(Thread::current(), &[v])
    }

    ScmResult::ok(make_values(
        Thread::current(),
        &[v, scm_int(cpudur as _), scm_int(dur as _)],
    ))
}

pub(crate) fn init_date() {
    let module = scm_capy_module().module();

    let subr = scm_make_subr("time-apply", time_apply, 2, 2);
    scm_define(module, "time-apply".intern().into(), subr.into()).unwrap();
}
