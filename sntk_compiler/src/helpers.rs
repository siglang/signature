use sntk_core::parser::ast::{ArrayLiteral, BooleanLiteral, Expression, NumberLiteral, ObjectLiteral, Program, Statement, StringLiteral};
use sntk_ir::{
    code::Block,
    stack::{LiteralValue, Value},
};
use std::collections::HashMap;

use crate::compiler::{CompileResult, Compiler, CompilerTrait};

pub fn literal_value(expression: Expression) -> Value {
    match expression {
        Expression::NumberLiteral(NumberLiteral { value, .. }) => Value::LiteralValue(LiteralValue::Number(value)),
        Expression::BooleanLiteral(BooleanLiteral { value, .. }) => Value::LiteralValue(LiteralValue::Boolean(value)),
        Expression::StringLiteral(StringLiteral { value, .. }) => Value::LiteralValue(LiteralValue::String(value)),
        Expression::ArrayLiteral(ArrayLiteral { elements, .. }) => {
            Value::LiteralValue(LiteralValue::Array(elements.into_iter().map(literal_value).collect()))
        }
        Expression::ObjectLiteral(ObjectLiteral { pairs, .. }) => {
            let mut object = HashMap::new();

            for (key, value) in pairs {
                object.insert(key.value, literal_value(value));
            }

            Value::LiteralValue(LiteralValue::Object(object))
        }
        value => panic!("Unexpected value: {:?}", value),
    }
}

pub fn compile_block(statements: Vec<Statement>) -> CompileResult<Block> {
    Ok(Block(Compiler::new(Program::new(statements)).compile_program()?.instructions))
}
