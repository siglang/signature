#![allow(unused_imports)] // TODO: remove this

use core::panic;

use crate::{
    compiler::CompileResult,
    error::{CompileError, TypeError, EXPECTED_DATA_TYPE, UNKNOWN_ARRAY_TYPE},
    helpers::literal_value,
    type_error,
};
use sntk_core::parser::ast::{DataType, Expression, FunctionType, Position};
use sntk_ir::{
    code::{BinaryOp, Block, Instruction, UnaryOp},
    interpreter::{Interpreter, InterpreterBase},
    value::{LiteralValue, Value},
};

#[derive(Debug, PartialEq, Clone)]
pub struct Type(pub DataType);

pub trait TypeTrait {
    fn get_type(value: &Value, position: &Position) -> CompileResult<Type>;
    fn get_type_from_expression(expression: &Expression, position: &Position) -> CompileResult<Type>;
    fn get_data_type(value: &Value, position: &Position) -> CompileResult<DataType>;
    fn get_data_type_from_expression(expression: &Expression, position: &Position) -> CompileResult<DataType>;
    fn get_type_from_instruction(instruction: Vec<Instruction>, position: &Position) -> CompileResult<Type>;
    fn get_data_type_from_instruction(instruction: Vec<Instruction>, position: &Position) -> CompileResult<DataType>;
    fn eq_from_value(&self, value: &Value, position: &Position) -> CompileResult<bool>;
    fn eq_from_type(&self, other: &Type) -> bool;
}

impl TypeTrait for Type {
    fn get_type(value: &Value, position: &Position) -> CompileResult<Type> {
        match value {
            Value::LiteralValue(literal_value) => match literal_value {
                LiteralValue::Number(_) => Ok(Type(DataType::Number)),
                LiteralValue::Boolean(_) => Ok(Type(DataType::Boolean)),
                LiteralValue::String(_) => Ok(Type(DataType::String)),
                LiteralValue::Array(elements) => {
                    let mut data_type = DataType::Array(Box::new(DataType::Unknown));

                    if elements.is_empty() {
                        return Err(type_error!(UNKNOWN_ARRAY_TYPE; data_type; position.clone();));
                    }

                    data_type = Type::get_data_type(&elements[0], position)?;

                    for element in elements {
                        if !Type::eq_from_value(&Type(data_type.clone()), element, position)? {
                            return Err(type_error!(EXPECTED_DATA_TYPE; data_type, Type::get_data_type(element, position)?; position.clone();));
                        }
                    }

                    Ok(Type(DataType::Array(Box::new(data_type))))
                }
                #[allow(unused_variables)]
                LiteralValue::Function { parameters, body } => {
                    unimplemented!();

                    // let return_type = match body[body.len() - 1] {
                    //     Instruction::Return => {
                    //         match &body[body.len() - 2] {
                    //             Instruction::LoadConst(value) => Type::get_data_type(value, position)?,
                    //             _ => DataType::Boolean, // TODO: Change this to Unit type
                    //         }
                    //     }
                    //     _ => DataType::Boolean,
                    // };

                    // Ok(Type(DataType::Fn(FunctionType::new(
                    //     None, // TODO: Generic type
                    //     parameters.iter().map(|parameter| parameter.1.clone()).collect(),
                    //     return_type,
                    // ))))
                }
            },
            Value::Identifier(_) => unimplemented!(),
            Value::Return(value) => Type::get_type(value, position),
        }
    }

    fn get_type_from_expression(expression: &Expression, position: &Position) -> CompileResult<Type> {
        Type::get_type(&literal_value(expression.clone())?, position)
    }

    fn get_data_type(value: &Value, position: &Position) -> CompileResult<DataType> {
        Ok(Type::get_type(value, position)?.0)
    }

    fn get_data_type_from_expression(expression: &Expression, position: &Position) -> CompileResult<DataType> {
        Ok(Type::get_type_from_expression(expression, position)?.0)
    }

    fn get_type_from_instruction(instruction: Vec<Instruction>, position: &Position) -> CompileResult<Type> {
        match instruction.last().unwrap() {
            Instruction::LoadConst(value) => Type::get_type(value, position),
            Instruction::BinaryOp(op) => match op {
                BinaryOp::Add => {
                    let left = Type::get_type_from_instruction(Vec::from(&instruction[0..instruction.len() - 1]), position)?;
                    let right = Type::get_type_from_instruction(Vec::from(&instruction[1..instruction.len()]), position)?;

                    if left.eq_from_type(&right) {
                        Ok(left)
                    } else {
                        Err(type_error!(EXPECTED_DATA_TYPE; left.0, right.0; position.clone();))
                    }
                }
                BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => Ok(Type(DataType::Number)),
            },
            Instruction::BinaryOpEq(_) => Ok(Type(DataType::Boolean)),
            Instruction::UnaryOp(op) => match op {
                UnaryOp::Not => Ok(Type(DataType::Boolean)),
                UnaryOp::Minus => Ok(Type(DataType::Number)),
            },
            Instruction::Block(Block(instructions)) => Type::get_type_from_instruction(instructions.to_vec(), position),
            Instruction::Return => Type::get_type_from_instruction(Vec::from(&instruction[instruction.len() - 2..1]), position),
            _ => panic!("Invalid instruction"),
        }
    }

    fn get_data_type_from_instruction(instruction: Vec<Instruction>, position: &Position) -> CompileResult<DataType> {
        Ok(Type::get_type_from_instruction(instruction, position)?.0)
    }

    fn eq_from_value(&self, value: &Value, position: &Position) -> CompileResult<bool> {
        match self {
            Type(DataType::Number) => Ok(Type::get_type(value, position)? == Type(DataType::Number)),
            Type(DataType::Boolean) => Ok(Type::get_type(value, position)? == Type(DataType::Boolean)),
            Type(DataType::String) => Ok(Type::get_type(value, position)? == Type(DataType::String)),
            Type(DataType::Array(_)) => Ok(Type::get_type(value, position)? == self.clone()),
            Type(DataType::Fn(_)) => Ok(Type::get_type(value, position)? == self.clone()),
            Type(DataType::Generic(_)) => unimplemented!(),
            Type(DataType::Custom(_)) => unimplemented!(),
            Type(DataType::Void) => unimplemented!(),
            Type(DataType::Unknown) => unimplemented!(),
        }
    }

    fn eq_from_type(&self, other: &Type) -> bool {
        self.0 == other.0
    }
}
