use sntk_compiler::compiler::{Compiler, CompilerTrait};
use sntk_core::parser::parser::{Parser, ParserTrait};
use sntk_ir::interpreter::InterpreterBase;

fn main() {
    let mut compiler = Compiler::new(
        Parser::from(
            r#"
let x: record string: number = record {
    "foo": 1,
    "bar": 2,
};
            "#
            .to_string(),
        )
        .parse_program(),
    );

    match &mut compiler.compile_program() {
        Ok(interpreter) => interpreter.run(),
        Err(error) => println!("{error}"),
    }
}
