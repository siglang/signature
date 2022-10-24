#![allow(unused_imports)]

use sntk_bytecode::{
    code::{BinaryOp, Instruction},
    interpreter::{Interpreter, InterpreterBase},
    stack::{LiteralValue, Value},
};
use sntk_compiler::compiler::{Compiler, CompilerTrait};
use sntk_core::parser::parser::{Parser, ParserTrait};

fn main() {
    // let mut compiler = Compiler::new(Parser::from("let a: number = 5; print(a);").parse_program());
    // let compiled = compiler.compile_program();
    // println!("{:?}", compiled);
    // compiled.unwrap().run();

    let mut interpreter = Interpreter::new(vec![
        Instruction::LoadConst(Value::LiteralValue(LiteralValue::Number(5.0))),
        Instruction::LoadConst(Value::LiteralValue(LiteralValue::Number(10.0))),
        Instruction::BinaryOp(BinaryOp::Add),
        Instruction::StoreName("a".to_string()),
        Instruction::LoadName("a".to_string()),
        Instruction::LoadGlobal("print".to_string()),
        Instruction::CallFunction(1),
        Instruction::LoadName("a".to_string()),
        Instruction::StoreName("b".to_string()),
        Instruction::LoadName("a".to_string()),
        Instruction::LoadConst(Value::LiteralValue(LiteralValue::String("*".to_string()))),
        Instruction::LoadName("b".to_string()),
        Instruction::LoadConst(Value::LiteralValue(LiteralValue::String("is".to_string()))),
        Instruction::LoadName("a".to_string()),
        Instruction::LoadName("b".to_string()),
        Instruction::BinaryOp(BinaryOp::Mul),
        Instruction::LoadGlobal("print".to_string()),
        Instruction::CallFunction(5),
    ]);

    interpreter.run();
}
