#![allow(unused_imports)] // TODO: remove this

use crate::{
    compiler::CompileResult,
    error::{CompileError, TypeError, EXPECTED_DATA_TYPE, UNKNOWN_ARRAY_TYPE},
    helpers::literal_value,
    type_error,
};
use sntk_core::parser::ast::{DataType, Expression, FunctionType, Position};
use sntk_ir::{
    code::Instruction,
    value::{LiteralValue, Value}, interpreter::{Interpreter, InterpreterBase},
};

#[derive(Debug, PartialEq, Clone)]
pub struct TypeSystem(pub DataType);

pub trait TypeSystemTrait {
    fn get_type(value: &Value, position: &Position) -> CompileResult<TypeSystem>;
    fn get_type_from_expression(expression: &Expression, position: &Position) -> CompileResult<TypeSystem>;
    fn get_data_type(value: &Value, position: &Position) -> CompileResult<DataType>;
    fn get_data_type_from_expression(expression: &Expression, position: &Position) -> CompileResult<DataType>;
    fn eq_from_value(&self, value: &Value, position: &Position) -> CompileResult<bool>;
    fn eq_from_type(&self, other: &TypeSystem) -> bool;
}

impl TypeSystemTrait for TypeSystem {
    fn get_type(value: &Value, position: &Position) -> CompileResult<TypeSystem> {
        match value {
            Value::LiteralValue(literal_value) => match literal_value {
                LiteralValue::Number(_) => Ok(TypeSystem(DataType::Number)),
                LiteralValue::Boolean(_) => Ok(TypeSystem(DataType::Boolean)),
                LiteralValue::String(_) => Ok(TypeSystem(DataType::String)),
                LiteralValue::Array(elements) => {
                    let mut data_type = DataType::Array(Box::new(DataType::Unknown));

                    if elements.is_empty() {
                        return Err(type_error!(UNKNOWN_ARRAY_TYPE; data_type; position.clone();));
                    }

                    data_type = TypeSystem::get_data_type(&elements[0], position)?;

                    for element in elements {
                        if !TypeSystem::eq_from_value(&TypeSystem(data_type.clone()), element, position)? {
                            return Err(type_error!(EXPECTED_DATA_TYPE; data_type, TypeSystem::get_data_type(element, position)?; position.clone();));
                        }
                    }

                    Ok(TypeSystem(DataType::Array(Box::new(data_type))))
                }
                #[allow(unused_variables)]
                LiteralValue::Function { parameters, body } => {
                    unimplemented!();

                    // let return_type = match body[body.len() - 1] {
                    //     Instruction::Return => {
                    //         match &body[body.len() - 2] {
                    //             Instruction::LoadConst(value) => TypeSystem::get_data_type(value, position)?,
                    //             _ => DataType::Boolean, // TODO: Change this to Unit type
                    //         }
                    //     }
                    //     _ => DataType::Boolean,
                    // };

                    // Ok(TypeSystem(DataType::Fn(FunctionType::new(
                    //     None, // TODO: Generic type
                    //     parameters.iter().map(|parameter| parameter.1.clone()).collect(),
                    //     return_type,
                    // ))))
                }
            },
            Value::Identifier(_) => unimplemented!(),
            Value::Return(value) => TypeSystem::get_type(value, position),
        }
    }

    fn get_type_from_expression(expression: &Expression, position: &Position) -> CompileResult<TypeSystem> {
        TypeSystem::get_type(&literal_value(expression.clone())?, position)
    }

    fn get_data_type(value: &Value, position: &Position) -> CompileResult<DataType> {
        Ok(TypeSystem::get_type(value, position)?.0)
    }

    fn get_data_type_from_expression(expression: &Expression, position: &Position) -> CompileResult<DataType> {
        Ok(TypeSystem::get_type_from_expression(expression, position)?.0)
    }

    fn eq_from_value(&self, value: &Value, position: &Position) -> CompileResult<bool> {
        match self {
            TypeSystem(DataType::Number) => Ok(TypeSystem::get_type(value, position)? == TypeSystem(DataType::Number)),
            TypeSystem(DataType::Boolean) => Ok(TypeSystem::get_type(value, position)? == TypeSystem(DataType::Boolean)),
            TypeSystem(DataType::String) => Ok(TypeSystem::get_type(value, position)? == TypeSystem(DataType::String)),
            TypeSystem(DataType::Array(_)) => Ok(TypeSystem::get_type(value, position)? == self.clone()),
            TypeSystem(DataType::Fn(_)) => Ok(TypeSystem::get_type(value, position)? == self.clone()),
            TypeSystem(DataType::Generic(_)) => unimplemented!(),
            TypeSystem(DataType::Custom(_)) => unimplemented!(),
            TypeSystem(DataType::Void) => unimplemented!(),
            TypeSystem(DataType::Unknown) => unimplemented!(),
        }
    }

    fn eq_from_type(&self, other: &TypeSystem) -> bool {
        self.0 == other.0
    }
}
