use super::JmpBuf;
#[naked_function::naked]
#[cfg(not(windows))]
pub unsafe extern "C" fn setjmp(_: *mut JmpBuf) -> i32 {
    asm!(
        "mov [rdi],    rbx",     // Store caller saved registers
        "mov [rdi+8],  rbp",     // ^
        "mov [rdi+16], r12",     // ^
        "mov [rdi+24], r13",     // ^
        "mov [rdi+32], r14",     // ^
        "mov [rdi+40], r15",     // ^
        "lea rdx,      [rsp+8]", // go one value up (as if setjmp wasn't called)
        "mov [rdi+48], rdx",     // Store the new rsp pointer in env[7]
        "mov rdx,      [rsp]",   // go one value up (as if setjmp wasn't called)
        "mov [rdi+56], rdx",     // Store the address we will resume at in env[8]
        "xor eax,      eax",     // Always return 0
        "ret",
    )
}

#[naked_function::naked]
#[cfg(not(windows))]
pub unsafe extern "C" fn longjmp(_: *const JmpBuf, _: i32) -> ! {
    asm!(
        "xor eax, eax",
        "cmp esi, 1",
        "adc eax, esi",
        "mov rbx, [rdi]",
        "mov rbp, [rdi + 8]",
        "mov r12, [rdi + 16]",
        "mov r13, [rdi + 24]",
        "mov r14, [rdi + 32]",
        "mov r15, [rdi + 40]",
        "mov rsp, [rdi + 48]",
        "jmp [rdi + 56]"
    )
}