#![allow(unused_imports)]

use sntk_ir::{
    code::{BinaryOp, Instruction, BinaryOpEq},
    interpreter::{Interpreter, InterpreterBase},
    stack::{LiteralValue, Value},
};
use sntk_compiler::compiler::{Compiler, CompilerTrait};
use sntk_core::parser::parser::{Parser, ParserTrait};

fn main() {
    //     let mut compiler = /*Compiler::new(*/
    //         Parser::from(
    //             r#"
    // if 3 > 2 {
    //     print("hello world");
    //     return true;
    // };
    //     "#.to_string(),
    //         )
    //         .parse_program();
    //     // );
    //     // let mut compiled = compiler.compile_program().unwrap();
    //     // compiled.run();
    //     println!("{:#?}", compiler);

    let mut interpreter = Interpreter::new(vec![
        Instruction::LoadConst(Value::LiteralValue(LiteralValue::Boolean(true))),
        Instruction::JumpIfFalse(6),
        Instruction::LoadConst(Value::LiteralValue(LiteralValue::String("hello world".to_string()))),
        Instruction::LoadGlobal("print".to_string()),
        Instruction::CallFunction(1),
        Instruction::Jump(7),
        Instruction::LoadConst(Value::LiteralValue(LiteralValue::String("hello world x".to_string()))),
        Instruction::LoadGlobal("print".to_string()),
        Instruction::CallFunction(1),
    ]);

    interpreter.run();
}
