use std::time::Instant;

use sntk_compiler::compiler::{Compiler, CompilerTrait};
use sntk_core::{
    parser::parser::{Parser, ParserBase, ParserTrait},
    tokenizer::{
        lexer::{Lexer, LexerTrait},
        token::Tokens,
    },
};
use sntk_ir::{
    instruction::{Instruction, InstructionType, IrExpression, LiteralValue},
    interpreter::{InterpreterTrait, IrInterpreter},
};

fn main() {
    let start = Instant::now();

    let mut ir_interpreter = IrInterpreter::new(vec![
        Instruction::new(
            InstructionType::StoreName("x".to_string(), IrExpression::Literal(LiteralValue::Number(1.))),
            (0, 0),
        ),
        Instruction::new(
            InstructionType::StoreName(
                "a".to_string(),
                IrExpression::If(
                    Box::new(IrExpression::Prefix(
                        Tokens::Bang,
                        Box::new(IrExpression::Infix(
                            Box::new(IrExpression::Identifier("x".to_string())),
                            Tokens::NEQ,
                            Box::new(IrExpression::Literal(LiteralValue::Number(1.))),
                        )),
                    )),
                    Box::new(IrExpression::Block(vec![
                        Instruction::new(
                            InstructionType::StoreName("c".to_string(), IrExpression::Literal(LiteralValue::Number(2.))),
                            (0, 0),
                        ),
                        Instruction::new(InstructionType::Return(IrExpression::Identifier("c".to_string())), (0, 0)),
                    ])),
                    Box::new(Some(IrExpression::Block(vec![Instruction::new(
                        InstructionType::Return(IrExpression::Literal(LiteralValue::Number(5.))),
                        (0, 0),
                    )]))),
                ),
            ),
            (0, 0),
        ),
        Instruction::new(
            InstructionType::StoreName(
                "q".to_string(),
                IrExpression::Literal(LiteralValue::Array(vec![
                    IrExpression::Literal(LiteralValue::Number(1.)),
                    IrExpression::Literal(LiteralValue::Number(2.)),
                    IrExpression::Literal(LiteralValue::Number(3.)),
                ])),
            ),
            (0, 0),
        ),
        Instruction::new(
            InstructionType::Expression(IrExpression::Call(
                Box::new(IrExpression::Identifier("println".to_string())),
                vec![
                    IrExpression::Identifier("a".to_string()),
                    IrExpression::Prefix(
                        Tokens::Minus,
                        Box::new(IrExpression::Index(
                            Box::new(IrExpression::Identifier("q".to_string())),
                            Box::new(IrExpression::Literal(LiteralValue::Number(2.))),
                        )),
                    ),
                ],
            )),
            (0, 0),
        ),
    ]);

    ir_interpreter.run();

    println!("Elapsed: {}s", start.elapsed().as_secs_f64());

    let source_code = r#"
let x: number = 1;
let a: number = if !(x != 1) {
    let c: number = 2;
    return c * 10;
} else {
    return 5;
};
let q: number[] = [1, 2, 3];
println(a, -(q[2]));
    "#;

    match Compiler::new(Parser::new(Lexer::new(source_code.to_string())).parse_program()).compile_program() {
        Ok(instructions) => {
            let mut ir_interpreter = IrInterpreter::new(instructions);

            ir_interpreter.run();
        }
        Err(e) => println!("{}", e),
    }
}
