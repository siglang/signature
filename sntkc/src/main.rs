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
auto x = 1;
println(typeof x); // number
auto z = {
    auto y = [1, 2, 3, 4, 5];
    println(typeof y, typeof x);

    return "hello";
};

println(typeof z, z);
    "#
    .trim_start();

    match Compiler::new(Parser::new(Lexer::new(source_code.to_string())).parse_program()).compile_program() {
        Ok(instructions) => {
            println!("Compiling Elapsed: {}s", start.elapsed().as_secs_f64());
            start = Instant::now();

            let mut ir_interpreter = IrInterpreter::new(instructions);

            ir_interpreter.run();
        }
        Err(e) => println!("{}", e),
    }

    println!("Interpreting Elapsed: {}s", start.elapsed().as_secs_f64());
}
