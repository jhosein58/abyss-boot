use std::env;
use std::path::PathBuf;

fn main() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let libs_dir = PathBuf::from(manifest_dir).join("libs");

    println!("cargo:rustc-link-search=native={}", libs_dir.display());

    println!("cargo:rustc-link-lib=static=tcc");

    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap();
    if target_os == "linux" {
        println!("cargo:rustc-link-lib=dylib=dl");
        println!("cargo:rustc-link-lib=dylib=m");
    }

    println!("cargo:rerun-if-changed=libs/libtcc.a");
}
