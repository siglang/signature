mod arguments;
mod evaluator;

use analyzer::analyzer::Analyzer;
use clap::Parser as _;
use evaluator::evaluator::Evaluator;
use parser::{tokenizer::Lexer, Parser};
use std::fs;

fn main() {
    let args = arguments::Cli::parse();
    let content = fs::read_to_string(args.source).unwrap();

    let lexer = Lexer::new(content.as_str());
    let mut parser = Parser::new(lexer);

    match parser.parse_program() {
        Ok(ast) => {
            // println!("AST: {:#?}", ast);
            match Analyzer::new(ast.clone()).analyze() {
                Ok(ret) => {
                    println!("Analyzed return type: {ret:?}");
                    if args.eval {
                        if let Err(error) = Evaluator::new(ast).evaluate() {
                            println!("Evaluate Error; {error}");
                        }
                    }
                }
                Err(error) => println!("Semantic Error; {error}"),
            }
        }
        Err(errors) => {
            for error in errors {
                println!("Parsing Error; {error}");
            }
        }
    }
}
