fn main() {
    #[cfg(any(target_arch="x86_64", target_arch="riscv64"))]
    {
        // enables low-level interpreter
        println!("cargo:rustc-cfg=llint");
    }
}