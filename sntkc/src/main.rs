#![allow(unused_imports)]

use sntk_bytecode::{
    code::{BinaryOp, Instruction},
    interpreter::{Interpreter, InterpreterBase},
    stack::{LiteralValue, Value},
};
use sntk_compiler::compiler::{Compiler, CompilerTrait};
use sntk_core::parser::parser::{Parser, ParserTrait};

fn main() {
    let mut compiler = Compiler::new(
        Parser::from(
            r#"
let x: number = 10;
{
    let y: number = 20;
    let z: number = x + y;
    print(x + y + z, x, y, z);
};
print(x);
print(-1, 2 / 3, [1, 2, [3, 4, 5]], "abcd", !true);
    "#,
        )
        .parse_program(),
    );
    let mut compiled = compiler.compile_program().unwrap();
    compiled.run();

    // let mut interpreter = Interpreter::new(vec![
    //     Instruction::LoadConst(Value::LiteralValue(LiteralValue::Number(5.0))),
    //     Instruction::LoadConst(Value::LiteralValue(LiteralValue::Number(10.0))),
    //     Instruction::BinaryOp(BinaryOp::Add),
    //     Instruction::StoreName("a".to_string()),
    //     Instruction::LoadName("a".to_string()),
    //     Instruction::LoadGlobal("print".to_string()),
    //     Instruction::CallFunction(1),
    //     Instruction::LoadName("a".to_string()),
    //     Instruction::StoreName("b".to_string()),
    //     Instruction::LoadName("a".to_string()),
    //     Instruction::LoadConst(Value::LiteralValue(LiteralValue::String("*".to_string()))),
    //     Instruction::LoadName("b".to_string()),
    //     Instruction::LoadConst(Value::LiteralValue(LiteralValue::String("is".to_string()))),
    //     Instruction::LoadName("a".to_string()),
    //     Instruction::LoadName("b".to_string()),
    //     Instruction::BinaryOp(BinaryOp::Mul),
    //     Instruction::LoadGlobal("print".to_string()),
    //     Instruction::CallFunction(5),
    // ]);

    // interpreter.run();
}
