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
let a: number = 3;
let b: number = 2;

let x: number = if a < b {
    print("a < b");

    return 1;
} else if a > b {
    print("a > b");

    return 2;
    print("This will not be printed");
} else {
    print("a == b");

    return 3;
};

print(x);
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
