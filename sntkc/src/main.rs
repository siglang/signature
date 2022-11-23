use sntk_compiler::compiler::{Compiler, CompilerTrait};
use sntk_core::{
    parser::parser::{Parser, ParserBase, ParserTrait},
    tokenizer::lexer::{Lexer, LexerTrait},
};
use sntk_ir::interpreter::{InterpreterTrait, IrInterpreter};
use std::time::Instant;

fn main() {
    let mut start = Instant::now();

    let source_code = r#"
auto a = 1;
auto foo = fn(x: number, y: string) -> number[] -> [x, x, x];

println(typeof foo, foo(1, "hello"), ["hello", "world"]);
    "#
    .trim_start();

    match Compiler::new(Parser::new(Lexer::new(source_code.to_string())).parse_program()).compile_program() {
        Ok(instructions) => {
            println!("Compiling Elapsed: {}s", start.elapsed().as_secs_f64());
            start = Instant::now();

            let mut ir_interpreter = IrInterpreter::new(instructions);

            match ir_interpreter.run() {
                Ok(_) => println!("Interpreting Elapsed: {}s", start.elapsed().as_secs_f64()),
                Err(error) => println!("{:?}", error),
            }
        }
        Err(e) => println!("{}", e),
    }
}
