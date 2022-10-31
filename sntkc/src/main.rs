use sntk_compiler::compiler::{Compiler, CompilerTrait};
use sntk_core::parser::parser::{Parser, ParserTrait};
use sntk_ir::interpreter::InterpreterBase;

fn main() {
    let parsed = Parser::from(
        r#"
let x: void = fn(x: number) -> number -> x * 10;

print(x(10));
    "#
        .to_string(),
    )
    .parse_program();

    match &mut Compiler::new(parsed).compile_program() {
        Ok(interpreter) => interpreter.run(),
        Err(error) => println!("{error}"),
    }
}
