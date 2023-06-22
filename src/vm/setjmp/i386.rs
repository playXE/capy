use crate::JmpBuf;
#[naked_function::naked]
pub unsafe extern "C" fn longjmp(_: *mut JmpBuf, _: i32) -> ! {
    asm!(
        "
        mov edx, [esp+4]
        mov eax, [esp+8]
        cmp eax, 1
        adc eax, 0
        mov ebx, [edx]
        mov esi, [edx+4]
        mov edi, [edx+8]
        mov ebp, [edx+12]
        mov esp, [edx+16]
        jmp [edx+20]
        
        "
    )
}

#[naked_function::naked]
pub unsafe extern "C" fn setjmp(_: *mut JmpBuf) -> i32 {
    asm!(
        "
        mov eax, [esp+4]
        mov ebx, [esp]
        mov esi, [esp+4]
        mov edi, [esp+8]
        mov ebp, [esp+12]
        lea ecx, [esp+4]
        mov [eax+16], ecx
        mov ecx, [esp]
        mov [eax+20], ecx
        xor eax, eax
        ret
        "
    )
}