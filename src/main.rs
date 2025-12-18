use std::{
    fs,
    io::Read,
    os::raw::{c_char, c_int, c_void},
    time::Instant,
};

use abyss::AbyssJit;
use abyss_analyzer::{flattener::Flattener, ir::Ir};
use abyss_codegen::{ctarget::ctarget::CTarget, director::Director};
use abyss_parser::parser::Parser;

fn main() {
    let mut input = String::new();
    fs::File::open("main.a")
        .unwrap()
        .read_to_string(&mut input)
        .expect("file 'main.a' not found.");

    println!("Compiling...");

    let t = Instant::now();

    let mut parser = Parser::new(&input);
    println!("{}", parser.format_errors("main.a"));
    let program = parser.parse_program();

    let flattener = Flattener::new();
    let flat_program = flattener.flatten(program);

    let ir = Ir::build(&flat_program);

    let mut target = CTarget::new();
    let mut compiler = Director::new(&mut target);
    compiler.process_program(&ir);

    //println!("{}", target.get_code());

    let c_code = target.get_code();

    unsafe extern "C" {
        fn printf(format: *const c_char, ...) -> c_int;
        fn memset(s: *mut c_void, c: c_int, n: usize) -> *mut c_void;
        fn memcpy(dest: *mut c_void, src: *const c_void, n: usize) -> *mut c_void;

        fn malloc(size: usize) -> *mut c_void;
        fn realloc(ptr: *mut c_void, size: usize) -> *mut c_void;
        fn free(ptr: *mut c_void);

        fn exit(status: c_int) -> !;
    }

    let mut jit = AbyssJit::new().unwrap();

    jit.add_function("printf", printf as *const c_void);
    jit.add_function("memset", memset as *const c_void);
    jit.add_function("memcpy", memcpy as *const c_void);
    jit.add_function("malloc", malloc as *const c_void);
    jit.add_function("realloc", realloc as *const c_void);
    jit.add_function("free", free as *const c_void);
    jit.add_function("exit", exit as *const c_void);

    jit.compile(c_code).expect("Compile error");
    jit.finalize().expect("Relocation error");

    println!("Finished in: {}ms", t.elapsed().as_millis());
    println!("Running...\n");

    type FnType = extern "C" fn();

    let func = jit
        .get_function::<FnType>("entry")
        .expect("Function not found");

    func();
}
