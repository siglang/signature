mod arguments;
use std::fs;

use analyzer::analyzer::Analyzer;
use clap::Parser as _;
use parser::{tokenizer::Lexer, Parser};

fn main() {
    let args = arguments::Cli::parse();
    let content = fs::read_to_string(args.source).unwrap();

    let lexer = Lexer::new(content.as_str());
    let mut parser = Parser::new(lexer);

    match parser.parse_program() {
        Ok(ast) => {
            // println!("AST: {:#?}", ast);
            match Analyzer::new(ast).analyze() {
                Ok(ret) => println!("Return type: {ret}"),
                Err(error) => println!("{error}"),
            }
        }
        Err(errors) => {
            for error in errors {
                println!("{error}");
            }
        }
    }
}
