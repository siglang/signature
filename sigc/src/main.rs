mod arguments;
use std::fs;

use clap::Parser as _;
use compiler::compiler::Compiler;
use parser::{parser::Parser, tokenizer::Lexer};

fn main() {
    let args = arguments::Cli::parse();
    let content = fs::read_to_string(args.source).unwrap();

    let lexer = Lexer::new(content);
    let mut parser = Parser::new(lexer);
    let ir = Compiler(parser.parse_program()).compile_program().unwrap();

    println!("{ir:#?}");
}
