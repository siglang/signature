use crate::{
    compiler::CompileResult,
    error::{CompileError, TypeError, EXPECTED_DATA_TYPE, UNEXPECTED_PARAMETER_LENGTH, UNKNOWN_ARRAY_TYPE},
    helpers::literal_value,
    type_error,
};
use sntk_core::parser::ast::{DataType, Expression, FunctionType, Position};
use sntk_ir::{
    code::{BinaryOp, Block, Instruction, UnaryOp},
    value::{LiteralValue, Value},
};

#[derive(Debug, PartialEq, Clone)]
pub struct Type(pub DataType);

pub trait TypeTrait {
    fn get_type(value: &Value, position: &Position) -> CompileResult<Type>;
    fn get_type_from_expression(expression: &Expression, position: &Position) -> CompileResult<Type>;
    fn get_data_type(value: &Value, position: &Position) -> CompileResult<DataType>;
    fn get_data_type_from_expression(expression: &Expression, position: &Position) -> CompileResult<DataType>;
    fn get_type_from_instruction(instruction: Vec<Instruction>, position: &Position) -> Option<CompileResult<Type>>;
    fn get_data_type_from_instruction(instruction: Vec<Instruction>, position: &Position) -> Option<CompileResult<DataType>>;
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
                LiteralValue::Array(elements) => expand_array_type(elements, position, None),
                LiteralValue::Function(parameters, body, return_type) => expand_function_type(parameters, body, return_type, position, None),
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

    fn get_type_from_instruction(instruction: Vec<Instruction>, position: &Position) -> Option<CompileResult<Type>> {
        Some(match instruction.last()? /* TODO: Error Handling */ {
            Instruction::LoadConst(value) => Type::get_type(value, position),
            Instruction::BinaryOp(op) => match op {
                BinaryOp::Add => {
                    let left = match Type::get_type_from_instruction(Vec::from(&instruction[0..instruction.len() - 1]), position)? {
                        Ok(t) => t,
                        Err(e) => return Some(Err(e)),
                    };
                    let right = match Type::get_type_from_instruction(Vec::from(&instruction[1..instruction.len()]), position)? {
                        Ok(t) => t,
                        Err(e) => return Some(Err(e)),
                    };

                    if left.eq_from_type(&right) {
                        Ok(left)
                    } else {
                        Err(type_error! { EXPECTED_DATA_TYPE; left.0, right.0; position.clone(); })
                    }
                }
                BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => Ok(Type(DataType::Number)),
            },
            Instruction::BinaryOpEq(_) => Ok(Type(DataType::Boolean)),
            Instruction::UnaryOp(op) => match op {
                UnaryOp::Not => Ok(Type(DataType::Boolean)),
                UnaryOp::Minus => Ok(Type(DataType::Number)),
            },
            Instruction::Block(Block(instructions)) => {
                Type::get_type_from_instruction(instructions.clone(), position)?
            }
            Instruction::Return => match Type::get_type_from_instruction(Vec::from(&instruction[instruction.len() - 2..1]), position)? {
                Ok(t) => Ok(t),
                Err(e) => return Some(Err(e)),
            },
            _ => panic!("Invalid instruction"),
        })
    }

    fn get_data_type_from_instruction(instruction: Vec<Instruction>, position: &Position) -> Option<CompileResult<DataType>> {
        Some(Type::get_type_from_instruction(instruction, position)?.map(|t| t.0))
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

#[inline]
pub fn expand_array_type(elements: &Vec<Value>, position: &Position, t_data_type /* Comparison target */: Option<&DataType>) -> CompileResult<Type> {
    let data_type = match t_data_type {
        Some(data_type) => data_type.clone(),
        None => {
            if elements.is_empty() {
                return Err(type_error! { UNKNOWN_ARRAY_TYPE; DataType::Number, DataType::Boolean, DataType::String; position.clone(); });
            }

            DataType::Array(Box::new(Type::get_data_type(&elements[0], position)?))
        }
    };

    let element_type = match data_type.clone() {
        DataType::Array(element) => *element,
        data_type => {
            return Err(
                type_error! { EXPECTED_DATA_TYPE; data_type, DataType::Array(Box::new(Type::get_data_type(&elements[0], position)?)); position.clone(); },
            )
        }
    };

    for element in elements {
        if !Type::get_type(element, position)?.eq_from_type(&Type(element_type.clone())) {
            return Err(type_error! { EXPECTED_DATA_TYPE; element_type.clone(), Type::get_data_type(element, position)?; position.clone(); });
        }
    }

    if let Some(t_data_type) = t_data_type {
        if Type(t_data_type.clone()) != Type(data_type.clone()) {
            return Err(type_error! { EXPECTED_DATA_TYPE; t_data_type.clone(), data_type.clone(); position.clone(); });
        }
    }

    Ok(Type(data_type))
}

#[inline]
pub fn expand_function_type(
    parameters: &Vec<(String, DataType)>,
    body: &Block,
    return_type: &DataType,
    position: &Position,
    t_data_type /* Comparison target */: Option<&DataType>,
) -> CompileResult<Type> {
    let data_type = match t_data_type {
        Some(data_type) => data_type.clone(),
        None => {
            if body.0.is_empty() {
                DataType::Fn(FunctionType(
                    None,
                    parameters.clone().iter().map(|x| x.clone().1).collect(),
                    Box::new(DataType::Void),
                ))
            } else {
                DataType::Fn(FunctionType(
                    None,
                    parameters.clone().iter().map(|x| x.clone().1).collect(),
                    Box::new(
                        Type::get_type_from_instruction(body.0.clone(), position)
                            .transpose()?
                            .map(|t| t.0)
                            .unwrap_or(DataType::Void),
                    ),
                ))
            }
        }
    };

    let function_type = match data_type.clone() {
        DataType::Fn(function_type) => function_type,
        data_type => {
            return Err(type_error! {
                EXPECTED_DATA_TYPE;
                data_type,
                DataType::Fn(FunctionType(
                    None,
                    parameters.clone().iter().map(|x| x.clone().1).collect(),
                    Box::new(DataType::Void),
                ));
                position.clone();
            });
        }
    };

    if parameters.len() != function_type.1.len() {
        return Err(type_error! { UNEXPECTED_PARAMETER_LENGTH; ; position.clone(); });
    }

    for (i, parameter) in parameters.iter().enumerate() {
        if parameter.1 != function_type.1[i] {
            return Err(type_error! { EXPECTED_DATA_TYPE; function_type.1[i], parameter.1; position.clone(); });
        }
    }

    let body_return_type = if body.0.is_empty() {
        DataType::Void
    } else {
        Type::get_type_from_instruction(body.0.clone(), position)
            .transpose()?
            .map(|t| t.0)
            .unwrap_or(DataType::Void)
    };

    let function_return_type = return_type;

    if function_return_type != &body_return_type {
        return Err(type_error! { EXPECTED_DATA_TYPE; function_return_type, body_return_type; position.clone(); });
    }

    if let Some(t_data_type) = t_data_type {
        let original_function_type = FunctionType(
            None,
            parameters.clone().iter().map(|x| x.clone().1).collect(),
            Box::new(return_type.clone()),
        );

        if Type(t_data_type.clone()) != Type(DataType::Fn(original_function_type.clone())) {
            return Err(type_error! { EXPECTED_DATA_TYPE; t_data_type.clone(), DataType::Fn(original_function_type); position.clone(); });
        }
    }

    Ok(Type(data_type))
}
