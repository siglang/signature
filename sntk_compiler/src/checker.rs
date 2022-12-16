use crate::{compiler::CompileResult, TypeError, TypeErrorKind};
use sntk_core::{
    parser::ast::{DataType, FunctionType, Parameter, Position},
    tokenizer::token::Tokens,
};
use sntk_ir::instruction::{Identifier, InstructionType, IrExpression, LiteralValue};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeEnvironment {
    pub types: HashMap<Identifier, DataType>,
    pub parent: Option<Box<TypeEnvironment>>,
}

impl TypeEnvironment {
    #[inline]
    pub fn new(parent: Option<&TypeEnvironment>) -> Self {
        Self {
            types: HashMap::new(),
            parent: parent.map(|parent| Box::new(parent.clone())),
        }
    }

    pub fn get(&self, name: &Identifier) -> Option<DataType> {
        match self.types.get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.get(name),
                None => None,
            },
        }
    }

    #[inline]
    pub fn set(&mut self, name: &Identifier, value: &DataType) {
        self.types.insert(name.to_string(), value.clone());
    }
}

pub fn get_type_from_ir_expression(
    expression: &IrExpression,
    types: &TypeEnvironment,
    data_type: Option<&DataType>,
    position: &Position,
) -> CompileResult<DataType> {
    match expression.clone() {
        IrExpression::Identifier(identifier) => match types.get(&identifier) {
            Some(data_type) => Ok(data_type),
            None => Err(TypeError::new(TypeErrorKind::UndefinedIdentifier(identifier.to_string()), *position)),
        },
        IrExpression::Literal(literal) => get_type_from_literal_value(&literal, types, data_type, position),
        IrExpression::Block(block) => get_type_from_ir_expression(
            match block.last() {
                Some(instruction) => match instruction.instruction {
                    InstructionType::Return(ref expression) | InstructionType::StoreName(_, ref expression) => expression,
                    _ => return Ok(DataType::Boolean),
                },
                None => return Ok(DataType::Boolean),
            },
            types,
            data_type,
            position,
        ),
        IrExpression::If(condition, consequence, alternative) => {
            let condition_type = get_type_from_ir_expression(&condition, types, data_type, position)?;
            let consequence_type = get_type_from_ir_expression(&consequence, types, data_type, position)?;
            let alternative_type = match *alternative {
                Some(alternative) => get_type_from_ir_expression(&alternative, types, data_type, position)?,
                None => DataType::Boolean,
            };

            if condition_type == DataType::Boolean {
                if consequence_type == alternative_type {
                    Ok(consequence_type)
                } else {
                    Err(TypeError::new(
                        TypeErrorKind::ExpectedDataType(consequence_type.to_string(), alternative_type.to_string()),
                        *position,
                    ))
                }
            } else {
                Err(TypeError::new(
                    TypeErrorKind::ExpectedDataType(DataType::Boolean.to_string(), condition_type.to_string()),
                    *position,
                ))
            }
        }
        IrExpression::Call(function, arguments) => {
            let function_type = get_type_from_ir_expression(&function, types, data_type, position)?;

            match function_type {
                DataType::Fn(FunctionType(_, parameters, return_type)) => {
                    let mut arguments_len = arguments.len();

                    for (index, ((parameter, spread), argument)) in parameters.iter().zip(arguments.iter()).enumerate() {
                        let argument_type = get_type_from_ir_expression(argument, types, data_type, position)?;

                        if *spread {
                            if parameter != &argument_type {
                                return Err(TypeError::new(
                                    TypeErrorKind::ExpectedDataType(parameter.to_string(), argument_type.to_string()),
                                    *position,
                                ));
                            }

                            arguments_len = index + 1;

                            break;
                        }

                        if parameter != &argument_type {
                            return Err(TypeError::new(
                                TypeErrorKind::ExpectedDataType(parameter.to_string(), argument_type.to_string()),
                                *position,
                            ));
                        }
                    }

                    if parameters.len() != arguments_len {
                        return Err(TypeError::new(
                            TypeErrorKind::ExpectedArguments(parameters.len(), arguments.len()),
                            *position,
                        ));
                    }

                    Ok(*return_type)
                }
                _ => Err(TypeError::new(TypeErrorKind::NotCallable(function_type.to_string()), *position)),
            }
        }
        IrExpression::Index(left, index) => {
            let left_type = get_type_from_ir_expression(&left, types, data_type, position)?;
            let index_type = get_type_from_ir_expression(&index, types, data_type, position)?;

            match left_type {
                DataType::Array(data_type) => {
                    if index_type == DataType::Number {
                        Ok(*data_type)
                    } else {
                        Err(TypeError::new(
                            TypeErrorKind::ExpectedDataType(DataType::Number.to_string(), index_type.to_string()),
                            *position,
                        ))
                    }
                }
                _ => Err(TypeError::new(TypeErrorKind::NotIndexable(left_type.to_string()), *position)),
            }
        }
        IrExpression::Prefix(_, expression) => get_type_from_ir_expression(&expression, types, data_type, position),
        IrExpression::Infix(left, operator, right) => {
            let left_type = get_type_from_ir_expression(&left, types, data_type, position)?;
            let right_type = get_type_from_ir_expression(&right, types, data_type, position)?;

            match operator {
                Tokens::Plus | Tokens::Minus | Tokens::Asterisk | Tokens::Slash | Tokens::Percent => {
                    if left_type == DataType::Number && right_type == DataType::Number {
                        Ok(DataType::Number)
                    } else {
                        Err(TypeError::new(
                            TypeErrorKind::ExpectedDataType(DataType::Number.to_string(), left_type.to_string()),
                            *position,
                        ))
                    }
                }
                Tokens::EQ | Tokens::NEQ | Tokens::LT | Tokens::GT | Tokens::LTE | Tokens::GTE => {
                    if left_type == right_type {
                        Ok(DataType::Boolean)
                    } else {
                        Err(TypeError::new(
                            TypeErrorKind::ExpectedDataType(left_type.to_string(), right_type.to_string()),
                            *position,
                        ))
                    }
                }
                _ => unreachable!(),
            }
        }
    }
}

pub fn get_type_from_literal_value(
    literal: &LiteralValue,
    types: &TypeEnvironment,
    data_type: Option<&DataType>,
    position: &Position,
) -> CompileResult<DataType> {
    match literal {
        LiteralValue::Number(_) => Ok(DataType::Number),
        LiteralValue::String(_) => Ok(DataType::String),
        LiteralValue::Boolean(_) => Ok(DataType::Boolean),
        LiteralValue::Array(elements) => {
            let mut element_type = DataType::Unknown;

            for element in elements {
                let data_type = get_type_from_ir_expression(element, types, data_type, position)?;

                if element_type == DataType::Unknown {
                    element_type = data_type;
                } else if element_type != data_type {
                    return Err(TypeError::new(
                        TypeErrorKind::ExpectedDataType(element_type.to_string(), data_type.to_string()),
                        *position,
                    ));
                }
            }

            match data_type {
                Some(data_type) => {
                    if element_type == DataType::Unknown {
                        element_type = match data_type {
                            DataType::Array(data_type) => *data_type.clone(),
                            _ => unreachable!(),
                        };
                    }

                    if data_type == &DataType::Array(Box::new(element_type.clone())) {
                        return Err(TypeError::new(
                            TypeErrorKind::ExpectedDataType(data_type.to_string(), DataType::Array(Box::new(element_type)).to_string()),
                            *position,
                        ));
                    }
                }
                None => {
                    if element_type == DataType::Unknown {
                        return Err(TypeError::new(TypeErrorKind::UnknownArrayType, *position));
                    }
                }
            }

            Ok(DataType::Array(Box::new(element_type)))
        }
        LiteralValue::Function(parameters, body, return_type, _) => {
            let mut types = TypeEnvironment::new(Some(types));

            for Parameter { name, data_type, .. } in parameters {
                types.set(&name.value, data_type);
            }

            let block_return_type = Box::new(get_type_from_ir_expression(
                &IrExpression::Block(body.clone()),
                &types,
                data_type,
                position,
            )?);

            let function_type = Ok(DataType::Fn(FunctionType(
                None,
                parameters
                    .iter()
                    .map(|Parameter { data_type, spread, .. }| (data_type.clone(), *spread))
                    .collect(),
                block_return_type.clone(),
            )))?;

            if return_type.clone() != *block_return_type {
                return Err(TypeError::new(
                    TypeErrorKind::ExpectedDataType(return_type.to_string(), block_return_type.to_string()),
                    *position,
                ));
            }

            if let Some(data_type) = data_type {
                if data_type != &function_type {
                    return Err(TypeError::new(
                        TypeErrorKind::ExpectedDataType(data_type.to_string(), function_type.to_string()),
                        *position,
                    ));
                }
            }

            Ok(function_type)
        }
    }
}
