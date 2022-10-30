use sntk_core::parser::ast::{DataType, Expression};
use sntk_ir::value::{LiteralValue, Value};

use crate::helpers::literal_value;

#[derive(Debug, PartialEq, Clone)]
pub struct TypeSystem(pub DataType);

pub trait TypeSystemTrait {
    fn get_type(value: &Value) -> TypeSystem;
    fn get_type_from_expression(expression: &Expression) -> TypeSystem;
    fn get_data_type(value: &Value) -> DataType;
    fn get_data_type_from_expression(expression: &Expression) -> DataType;
    fn eq_from_type(&self, other: &TypeSystem) -> bool;
    fn eq_from_value(&self, value: &Value) -> bool;
}

impl TypeSystemTrait for TypeSystem {
    fn get_type(value: &Value) -> TypeSystem {
        match value {
            Value::LiteralValue(literal_value) => match literal_value {
                LiteralValue::Number(_) => TypeSystem(DataType::Number),
                LiteralValue::Boolean(_) => TypeSystem(DataType::Boolean),
                LiteralValue::String(_) => TypeSystem(DataType::String),
                LiteralValue::Array(_) => unimplemented!(),
                LiteralValue::Object(_) => unimplemented!(),
                LiteralValue::Function { .. } => unimplemented!(),
            },
            Value::Identifier(_) => unimplemented!(),
            Value::Return(value) => TypeSystem::get_type(value),
        }
    }

    fn get_type_from_expression(expression: &Expression) -> TypeSystem {
        TypeSystem::get_type(&literal_value(expression.clone()))
    }

    fn get_data_type(value: &Value) -> DataType {
        TypeSystem::get_type(value).0
    }

    fn get_data_type_from_expression(expression: &Expression) -> DataType {
        TypeSystem::get_type_from_expression(expression).0
    }

    fn eq_from_type(&self, other: &TypeSystem) -> bool {
        self.0 == other.0
    }

    fn eq_from_value(&self, value: &Value) -> bool {
        match self {
            TypeSystem(DataType::Number) => TypeSystem::get_type(value) == TypeSystem(DataType::Number),
            TypeSystem(DataType::Boolean) => TypeSystem::get_type(value) == TypeSystem(DataType::Boolean),
            TypeSystem(DataType::String) => TypeSystem::get_type(value) == TypeSystem(DataType::String),
            TypeSystem(DataType::Array(_)) => unimplemented!(),
            TypeSystem(DataType::Object(_)) => unimplemented!(),
            TypeSystem(DataType::Fn(_)) => unimplemented!(),
            TypeSystem(DataType::Generic(_)) => unimplemented!(),
            TypeSystem(DataType::Custom(_)) => unimplemented!(),
            TypeSystem(DataType::Void) => unimplemented!(),
        }
    }
}
