use sntk_compiler::compiler::{Compiler, CompilerTrait};
use sntk_core::parser::parser::{Parser, ParserTrait};
use sntk_ir::interpreter::InterpreterBase;

fn main() {
    let parsed = Parser::from(
        r#"
let x: fn(number, string) -> boolean[] =
    fn(a: number, b: string) -> boolean[]
{
    return [true, false];
};

print(x(1, "hello"));
"#
        .to_string(),
    )
    .parse_program();

    match &mut Compiler::new(parsed).compile_program() {
        Ok(interpreter) => interpreter.run(),
        Err(error) => println!("{error}"),
    }
}
