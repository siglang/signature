use std::time::Instant;

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
                    Box::new(IrExpression::Literal(LiteralValue::Boolean(true))),
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
            InstructionType::Expression(IrExpression::Call("println".to_string(), vec![IrExpression::Identifier("a".to_string())])),
            (0, 0),
        ),
    ]);

    ir_interpreter.run();

    println!("Elapsed: {}s", start.elapsed().as_secs_f64());
}
