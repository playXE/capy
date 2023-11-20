fn main() {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let workspace_dir = std::path::Path::new(manifest_dir)
        .parent()
        .unwrap()
        .parent()
        .unwrap();
    println!(
        "cargo:warning=Setting current directory to {}",
        workspace_dir.display()
    );
    std::env::set_current_dir(workspace_dir).unwrap();
    let scm = option_env!("SCHEME").unwrap_or("scheme");

    let structs_file = format!("{}/structs.rs", std::env::var("OUT_DIR").unwrap());
    let macros_file = format!("{}/macros.rs", std::env::var("OUT_DIR").unwrap());

    let mut cmd = std::process::Command::new(scm);
    cmd.arg("--script")
        .arg("generator/dsl.scm")
        .arg(&structs_file)
        .arg(&macros_file);

    let _output = cmd.spawn().unwrap().wait_with_output().unwrap();

    println!("cargo:warning=Bytecode definitions generated");
    println!("cargo:warning=Structs at {}", structs_file);
    println!("cargo:warning=Macros at {}", macros_file);
    println!("cargo:rerun-if-changed=generator/def.scm");
    println!("cargo:rerun-if-changed=generator/dsl.scm");
}
