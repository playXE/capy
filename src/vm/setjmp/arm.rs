use crate::JmpBuf;
#[naked_function::naked]
pub unsafe extern "C" fn setjmp(_: *mut JmpBuf) -> i32 {
    asm!(
        "
            mov ip,r0
            stmia ip!,{{v1,v2,v3,v4,v5,v6,sl,fp}}
            mov r2,sp
            stmia ip!,{{r2,lr}}
            mov r0,#0
        
            adr r1,1f
            ldr r2,1f
            ldr r1,[r1,r2]
        2:	
            tst r1,#0x40
            beq 2f
            .fpu vfp
            vstmia ip!, {{d8-d15}}
            .fpu softvfp
            .eabi_attribute 10, 0
            .eabi_attribute 27, 0
            bx lr  
        "
    )
}

#[naked_function::naked]
pub unsafe extern "C" fn longjmp(_: *mut JmpBuf, _: i32) -> !{ 
    asm!(
        "mov ip,r0
        movs r0,r1
        moveq r0,#1
        ldmia ip!, {{v1,v2,v3,v4,v5,v6,sl,fp}}
        ldmia ip!, {{r2,lr}}
        mov sp,r2
    
        adr r1,1f
        ldr r2,1f
        ldr r1,[r1,r2]
        tst r1,#0x40
        beq 2f
        .fpu vfp
        vldmia ip!, {{d8-d15}}
        .fpu softvfp
        .eabi_attribute 10, 0
        .eabi_attribute 27, 0
        bx lr
        "
    )
}