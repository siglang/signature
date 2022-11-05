// use std::time::Instant;
// use sntk_compiler::compiler::{Compiler, CompilerTrait};
// use sntk_core::parser::parser::{Parser, ParserTrait};
// use sntk_ir::interpreter::InterpreterBase;

fn main() {
    //     let parsed = Parser::from(
    //         r#"
    // auto x = fn(a: number, b: string) ->
    //     fn() -> string ->
    //         fn() -> string[] -> [typeof a, typeof b];

    // print(x(1, "hello")(), "typeof:", typeof x);
    // "#
    //         .to_string(),
    //     )
    //     .parse_program();

    //     let start_time = Instant::now();

    //     match &mut Compiler::new(parsed).compile_program() {
    //         Ok(interpreter) => interpreter.run(),
    //         Err(error) => println!("{error}"),
    //     }

    //     println!("Time: {:?}s", start_time.elapsed().as_secs_f64());
}
