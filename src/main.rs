use abyss_lexer::{Lexer, cursor::Cursor, scanner::Scanner};

fn main() {
    let input = r#"
        ==
"#
    .to_string();

    println!("Code: \n{}", input);

    let lexer = Lexer::new(&input);

    let mut tokens = vec![];

    for token in lexer {
        tokens.push(token);
    }

    dbg!(tokens);

    // let lexer = Lexer::new(input);
    // let mut parser = Parser::new(lexer);
    // let program = parser.parse_program();

    // dbg!(program);

    //     let c_target = CTarget::new();
    //     let mut director = Director::new(c_target);

    //     director.run(program);

    //     let result = director.conclude();
    //     fs::write("main.c", &result.buffer).expect("failed to write C file");

    //     Command::new("gcc")
    //         .args(&["main.c", "-o", "out"])
    //         .status()
    //         .expect("failed to compile C code");

    //     Command::new("./out")
    //         .status()
    //         .expect("failed to execute generated program");
}
