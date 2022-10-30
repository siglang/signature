use crate::{
    compiler::CompileResult,
    error::{CompileError, TypeError, EXPECTED_DATA_TYPE, UNKNOWN_ARRAY_TYPE, /* UNKNOWN_OBJECT_TYPE */},
    helpers::literal_value,
    type_error,
};
use sntk_core::parser::ast::{DataType, Expression, /* ObjectType, */ Position};
use sntk_ir::value::{LiteralValue, Value};

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
                LiteralValue::Object(/* pairs */ _) => {
                    // let mut data_type = DataType::Object(ObjectType(Box::new(DataType::Unknown), Box::new(DataType::Unknown)));

                    // if pairs.is_empty() {
                    //     return Err(type_error!(UNKNOWN_OBJECT_TYPE; data_type; position.clone();));
                    // }

                    // let first_pair = &pairs;

                    // // data_type = TypeSystem::get_data_type(&pairs.values()

                    // Ok(())

                    unimplemented!()
                }
                LiteralValue::Function { .. } => unimplemented!(),
            },
            Value::Identifier(_) => unimplemented!(),
            Value::Return(value) => TypeSystem::get_type(value, position),
        }
    }

    fn get_type_from_expression(expression: &Expression, position: &Position) -> CompileResult<TypeSystem> {
        TypeSystem::get_type(&literal_value(expression.clone()), position)
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
            TypeSystem(DataType::Array(_)) => unimplemented!(),
            TypeSystem(DataType::Object(_)) => unimplemented!(),
            TypeSystem(DataType::Fn(_)) => unimplemented!(),
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
