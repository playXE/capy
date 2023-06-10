pub fn scm_error(err: &str) -> ! {
    todo!("throw error: {}", err)
}

#[macro_export]
macro_rules! scm_error {
    ($fmt: literal) => {
        $crate::vm::scm_error($fmt)
    };

    ($fmt: literal, $($arg: expr),*) => {
        $crate::vm::scm_error(&format!($fmt, $($arg),*))
    };
}