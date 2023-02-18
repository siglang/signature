use clap::Parser as _;
pub mod arguments;

use crate::arguments::Cli;
use compiler::compiler::Compiler;
use ir_interpreter::interpreter::IrInterpreter;
use parser::{parser::parser::Parser, tokenizer::lexer::Lexer};
use std::{fs::read_to_string, time::Instant};

fn main() {
    let Cli {
        source,
        debug,
        .. // todo
    } = Cli::parse();

    let source_code = match read_to_string(source) {
        Ok(source) => source,
        Err(e) => panic!("Error reading source file: {}", e),
    };

    let mut start = Instant::now();

    let lexer = Lexer::new(source_code.to_string());
    let parsed = Parser::new(lexer).parse_program();

    if debug {
        println!("Parsing Elapsed: {}s", start.elapsed().as_secs_f64())
    }

    match Compiler::new(parsed).compile_program() {
        Ok(instructions) => {
            if debug {
                println!("Compiling Elapsed: {}s", start.elapsed().as_secs_f64());
            }
            start = Instant::now();

            let mut ir_interpreter = IrInterpreter::new(instructions);

            match ir_interpreter.eval() {
                Ok(_) => {
                    if debug {
                        println!("Interpreting Elapsed: {}s", start.elapsed().as_secs_f64());
                    }
                }
                Err(error) => println!("{:?}", error),
            }
        }
        Err(e) => println!("{e}"),
    }
}
