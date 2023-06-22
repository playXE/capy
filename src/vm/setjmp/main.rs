use setjmp_rs::*;
fn main() {
    unsafe {
        let mut env = JumpBuf::new();

        let ret = setjmp(&mut env);

        if ret == 0 {
    
            longjmp(&mut env, 1);
        } else {
           drop(env);
        }
    }
}
