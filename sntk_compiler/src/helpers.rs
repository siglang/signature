use crate::compiler::{CompileResult, Compiler, CompilerTrait};
use sntk_core::parser::ast::{ArrayLiteral, BooleanLiteral, Expression, NumberLiteral, Program, RecordLiteral, Statement, StringLiteral};
use sntk_ir::{
    code::Block,
    value::{LiteralValue, Value},
};

pub fn literal_value(expression: Expression) -> Value {
    match expression {
        Expression::NumberLiteral(NumberLiteral { value, .. }) => Value::LiteralValue(LiteralValue::Number(value)),
        Expression::BooleanLiteral(BooleanLiteral { value, .. }) => Value::LiteralValue(LiteralValue::Boolean(value)),
        Expression::StringLiteral(StringLiteral { value, .. }) => Value::LiteralValue(LiteralValue::String(value)),
        Expression::ArrayLiteral(ArrayLiteral { elements, .. }) => {
            Value::LiteralValue(LiteralValue::Array(elements.into_iter().map(literal_value).collect()))
        }
        Expression::RecordLiteral(RecordLiteral { /* pairs, */ .. }) => {
            unimplemented!()
        }
        Expression::FunctionLiteral { .. } => unimplemented!(),
        value => panic!("Unexpected value: {:?}", value),
    }
}

pub fn compile_block(statements: Vec<Statement>) -> CompileResult<Block> {
    Ok(Block(Compiler::new(Program::new(statements)).compile_program()?.instructions))
}
