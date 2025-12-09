use abyss_codegen::{director::Director, jit::CraneliftTarget};
use abyss_parser::ast::{BinaryOp, Expr, Function, Lit, Program, Stmt};

fn main() {
    // fn fib(n: int) -> int {
    //     if n <= 1 {
    //         ret n;
    //     }
    //     ret fib(n - 1) + fib(n - 2);
    // }

    // fn main() -> int {
    //     ret fib(10);
    // }

    let fib_body = vec![
        // if n <= 1 { ret n; }
        Stmt::If(
            Expr::Binary(
                Box::new(Expr::Ident("n".to_string())),
                BinaryOp::Lte,
                Box::new(Expr::Lit(Lit::Int(1))),
            ),
            // Then block: ret n;
            vec![Stmt::Ret(Expr::Ident("n".to_string()))],
            None,
        ),
        // ret fib(n - 1) + fib(n - 2);
        Stmt::Ret(Expr::Binary(
            // Left: fib(n - 1)
            Box::new(Expr::Call(
                "fib".to_string(),
                vec![Expr::Binary(
                    Box::new(Expr::Ident("n".to_string())),
                    BinaryOp::Sub,
                    Box::new(Expr::Lit(Lit::Int(1))),
                )],
            )),
            BinaryOp::Add,
            // Right: fib(n - 2)
            Box::new(Expr::Call(
                "fib".to_string(),
                vec![Expr::Binary(
                    Box::new(Expr::Ident("n".to_string())),
                    BinaryOp::Sub,
                    Box::new(Expr::Lit(Lit::Int(2))),
                )],
            )),
        )),
    ];

    let fib_function = Function {
        name: "fib".to_string(),
        params: vec![("n".to_string(), "int".to_string())], // ورودی n: int
        return_type: Some("int".to_string()),
        body: fib_body,
    };

    let main_body = vec![Stmt::Ret(Expr::Call(
        "fib".to_string(),
        vec![Expr::Lit(Lit::Int(10))],
    ))];

    let main_function = Function {
        name: "main".to_string(),
        params: vec![],
        return_type: Some("int".to_string()),
        body: main_body,
    };

    let program = Program {
        functions: vec![fib_function, main_function],
    };

    let mut clif_target = CraneliftTarget::new();
    let mut director = Director::new(&mut clif_target);

    println!("--- Compiling Program (Fibonacci Recursive) ---");
    director.process_program(&program);

    println!("--- Running JIT Code ---");
    match clif_target.run_fn("main") {
        Ok(result) => {
            println!("Recursion Test Passed!");
            println!("fib(10) Result: {}", result);

            assert_eq!(result, 55);
        }
        Err(e) => println!("Runtime Error: {}", e),
    }
}
