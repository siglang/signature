#![allow(unused_imports)]

use sntk_compiler::compiler::{Compiler, CompilerTrait};
use sntk_core::parser::parser::{Parser, ParserTrait};
use sntk_ir::{
    code::{BinaryOp, BinaryOpEq, Block, Instruction},
    interpreter::{Interpreter, InterpreterBase},
    stack::{LiteralValue, Value},
};

fn main() {
    let mut compiler = Compiler::new(
        Parser::from(
            r#"
let a: fn(number, number) -> fn() -> number =
    fn(a: number, b: string) -> fn() -> number
{
    print(a, b);
    return fn() -> number -> a * 10;
};

print(a);
            "#
            .to_string(),
        )
        .parse_program(),
    );
    let mut compiled = compiler.compile_program().unwrap();
    compiled.run();

    // let mut interpreter = Interpreter::new(vec![
    //     Instruction::LoadConst(Value::LiteralValue(LiteralValue::Boolean(false))),
    //     Instruction::If(
    //         Block(vec![
    //             Instruction::LoadConst(Value::LiteralValue(LiteralValue::String("hello world".to_string()))),
    //             Instruction::LoadGlobal("print".to_string()),
    //             Instruction::CallFunction(1),
    //         ]),
    //         None,
    //     ),
    // ]);

    // interpreter.run();
}
