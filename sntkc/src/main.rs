use sntk_compiler::compiler::{Compiler, CompilerTrait};
use sntk_core::parser::parser::{Parser, ParserTrait};
use sntk_ir::interpreter::InterpreterBase;

fn main() {
    let mut compiler = Compiler::new(
        Parser::from(
            r#"
let a: fn(number, number) -> fn() -> number =
    fn(a: number, b: string) -> fn() -> number
{
    print(a, b);

    return fn(x: number) -> number -> a * x;
};

print(a(10, "hello")(5));

print(fn(x: number) -> number {
    print(x);
}(10));x
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
