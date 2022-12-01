use sntk_compiler::compiler::{Compiler, CompilerTrait};
use sntk_core::{
    parser::parser::{Parser, ParserBase, ParserTrait},
    tokenizer::lexer::{Lexer, LexerTrait},
};
// use std::time::Instant;

fn main() {
    // let mut start = Instant::now();

    let source_code = r#"
auto a = 2;
auto foo = fn(x: number, y: string) ->
    fn(number) -> number[]
{
    println(x, y);
    return fn(n: number) -> number[] -> [x * n, x * n + 1, x * n + 2];
};

println(foo(-1, "Hello, World!")(10));
    "#
    .trim_start();

    match Compiler::new(Parser::new(Lexer::new(source_code.to_string())).parse_program()).compile_program() {
        Ok(instructions) => {
            // println!("Compiling Elapsed: {}s", start.elapsed().as_secs_f64());
            println!("{:#?}", instructions);
            // start = Instant::now();

            // let mut ir_interpreter = IrInterpreter::new(instructions);

            // match ir_interpreter.run() {
            //     Ok(_) => println!("Interpreting Elapsed: {}s", start.elapsed().as_secs_f64()),
            //     Err(error) => println!("{:?}", error),
            // }
        }
        Err(e) => println!("{}", e),
    }
}
