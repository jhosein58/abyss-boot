use std::{fs, io::Read, time::Instant};

use abyss_codegen::{
    director::Director,
    jit::CraneliftTarget,
    target::{Target, Type},
};
use abyss_parser::parser::Parser;

pub unsafe extern "C" fn print_i(n: i64) {
    println!("{}", n);
}

pub unsafe extern "C" fn print_f(n: f64) {
    println!("{}", n);
}

fn main() {
    let mut input = String::new();
    fs::File::open("main.a")
        .unwrap()
        .read_to_string(&mut input)
        .unwrap();

    println!("Code:\n{}", input);

    println!("Compiling...");

    let t = Instant::now();

    let symbols = [
        ("print_i", print_i as *const u8),
        ("print_f", print_f as *const u8),
    ];
    let mut target = CraneliftTarget::new(&symbols);
    target.declare_extern_function("print_i", &[("n".to_string(), Type::Int)], Type::Void);
    target.declare_extern_function("print_f", &[("n".to_string(), Type::Float)], Type::Void);

    let mut director = Director::new(&mut target);

    let mut parser = Parser::new(&input);
    let program = parser.parse_program();
    println!("{}", parser.format_errors("test.a"));
    //dbg!(&program);
    director.process_program(&program);

    println!("Finished in: {}ms", t.elapsed().as_millis());
    println!("Running...\n");
    let _ = target.run_fn("main");
    //println!("\n{}\n", res.unwrap());
}
