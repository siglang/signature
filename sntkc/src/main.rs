#![allow(unused_imports)]

use sntk_compiler::compiler::Compiler;
use sntk_core::{
    parser::parser::Parser,
    tokenizer::{lexer::Lexer, token::TokenKind},
};
use sntk_ir::interpreter::IrInterpreter;
use std::time::Instant;

fn main() {
    let mut start = Instant::now();

    let source_code = r#"
declare println = fn(number[]) -> boolean; // todo

type F = fn(boolean) -> number[];

auto x = fn(a: number, b: number, spread c: number) ->
    fn(string) -> F
{
    return fn(x: string) -> Foo =>
            fn(y: boolean) -> number[] => c;
};
println(x(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)("foo")(true));
    "#
    .trim_start();

    #[allow(unused_mut)]
    let mut lexer = Lexer::new(source_code.to_string());

    // let mut next_token = lexer.next_token();
    // while next_token.token_type != TokenKind::EOF {
    //     println!("{:?}", next_token);
    //     next_token = lexer.next_token();
    // }

    let parsed = Parser::new(lexer).parse_program();

    println!("Parsing Elapsed: {}s", start.elapsed().as_secs_f64());
    // println!("{:#?}", parsed);

    match Compiler::new(parsed).compile_program() {
        Ok(instructions) => {
            println!("Compiling Elapsed: {}s", start.elapsed().as_secs_f64());
            start = Instant::now();

            let mut ir_interpreter = IrInterpreter::new(instructions);

            match ir_interpreter.eval() {
                Ok(_) => println!("Interpreting Elapsed: {}s", start.elapsed().as_secs_f64()),
                Err(error) => println!("{:?}", error),
            }
        }
        Err(e) => println!("{:?}", e),
    }
}
