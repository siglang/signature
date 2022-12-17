use crate::{compiler::CompileResult, TypeError, TypeErrorKind};
use sntk_core::{
    parser::ast::{DataType, FunctionType, Parameter, Position},
    tokenizer::token::Tokens,
};
use sntk_ir::instruction::{InstructionType, IrExpression, LiteralValue};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct DeclaredTypes {
    pub types: HashMap<String, DataType>,
    pub parent: Option<Box<DeclaredTypes>>,
}

impl DeclaredTypes {
    #[inline]
    pub fn new(parent: Option<&DeclaredTypes>) -> Self {
        Self {
            types: HashMap::new(),
            parent: parent.map(|parent| Box::new(parent.clone())),
        }
    }

    pub fn get(&self, name: &String) -> Option<DataType> {
        match self.types.get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.get(name),
                None => None,
            },
        }
    }

    #[inline]
    pub fn set(&mut self, name: &String, value: &DataType) {
        self.types.insert(name.to_string(), value.clone());
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CustomTypes {
    pub types: HashMap<String, DataType>,
    pub parent: Option<Box<CustomTypes>>,
}

impl CustomTypes {
    #[inline]
    pub fn new(parent: Option<&CustomTypes>) -> Self {
        Self {
            types: HashMap::new(),
            parent: parent.map(|parent| Box::new(parent.clone())),
        }
    }

    pub fn get(&self, name: &String) -> Option<DataType> {
        match self.types.get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.get(name),
                None => None,
            },
        }
    }

    #[inline]
    pub fn set(&mut self, name: &String, value: &DataType) {
        self.types.insert(name.to_string(), value.clone());
    }
}

#[derive(Debug, Clone)]
pub struct Checker {
    data_type: Option<DataType>,
    declares: DeclaredTypes,
    customs: CustomTypes,
    position: Position,
}

impl Checker {
    #[inline]
    pub fn new(data_type: Option<&DataType>, declares: &DeclaredTypes, customs: &CustomTypes, position: &Position) -> CompileResult<Self> {
        Ok(Self {
            data_type: match data_type {
                Some(data_type) => Some(custom_data_type(data_type, customs, position)?),
                None => None,
            },
            declares: declares.clone(),
            customs: customs.clone(),
            position: *position,
        })
    }

    pub fn get_type_from_ir_expression(&self, expression: &IrExpression) -> CompileResult<DataType> {
        let result = match expression.clone() {
            IrExpression::Identifier(identifier) => match self.declares.get(&identifier) {
                Some(data_type) => Ok(data_type),
                None => Err(TypeError::new(TypeErrorKind::UndefinedIdentifier(identifier.to_string()), self.position)),
            },
            IrExpression::Literal(literal) => self.get_type_from_literal_value(&literal),
            IrExpression::Block(block) => self.get_type_from_ir_expression(match block.last() {
                Some(instruction) => match instruction.instruction {
                    InstructionType::Return(ref expression) | InstructionType::StoreName(_, ref expression) => expression,
                    _ => return Ok(DataType::Boolean),
                },
                None => return Ok(DataType::Boolean),
            }),
            IrExpression::If(condition, consequence, alternative) => {
                let condition_type = self.get_type_from_ir_expression(&condition)?;
                let consequence_type = self.get_type_from_ir_expression(&consequence)?;
                let alternative_type = match *alternative {
                    Some(alternative) => self.get_type_from_ir_expression(&alternative)?,
                    None => DataType::Boolean,
                };

                if condition_type == DataType::Boolean {
                    if consequence_type == alternative_type {
                        Ok(consequence_type)
                    } else {
                        Err(TypeError::new(
                            TypeErrorKind::ExpectedDataType(consequence_type.to_string(), alternative_type.to_string()),
                            self.position,
                        ))
                    }
                } else {
                    Err(TypeError::new(
                        TypeErrorKind::ExpectedDataType(DataType::Boolean.to_string(), condition_type.to_string()),
                        self.position,
                    ))
                }
            }
            IrExpression::Call(function, arguments) => {
                let function_type = self.get_type_from_ir_expression(&function)?;

                match function_type {
                    DataType::Fn(FunctionType(_, parameters, return_type)) => {
                        let mut arguments_len = arguments.len();

                        for (index, ((parameter, spread), argument)) in parameters.iter().zip(arguments.iter()).enumerate() {
                            let argument_type = self.get_type_from_ir_expression(argument)?;

                            if *spread {
                                if parameter != &argument_type {
                                    return Err(TypeError::new(
                                        TypeErrorKind::ExpectedDataType(parameter.to_string(), argument_type.to_string()),
                                        self.position,
                                    ));
                                }

                                arguments_len = index + 1;

                                break;
                            }

                            if parameter != &argument_type {
                                return Err(TypeError::new(
                                    TypeErrorKind::ExpectedDataType(parameter.to_string(), argument_type.to_string()),
                                    self.position,
                                ));
                            }
                        }

                        if parameters.len() != arguments_len {
                            return Err(TypeError::new(
                                TypeErrorKind::ExpectedArguments(parameters.len(), arguments.len()),
                                self.position,
                            ));
                        }

                        Ok(*return_type)
                    }
                    _ => Err(TypeError::new(TypeErrorKind::NotCallable(function_type.to_string()), self.position)),
                }
            }
            IrExpression::Index(left, index) => {
                let left_type = self.get_type_from_ir_expression(&left)?;
                let index_type = self.get_type_from_ir_expression(&index)?;

                match left_type {
                    DataType::Array(data_type) => {
                        if index_type == DataType::Number {
                            Ok(*data_type)
                        } else {
                            Err(TypeError::new(
                                TypeErrorKind::ExpectedDataType(DataType::Number.to_string(), index_type.to_string()),
                                self.position,
                            ))
                        }
                    }
                    _ => Err(TypeError::new(TypeErrorKind::NotIndexable(left_type.to_string()), self.position)),
                }
            }
            IrExpression::Prefix(_, expression) => self.get_type_from_ir_expression(&expression),
            IrExpression::Infix(left, operator, right) => {
                let left_type = self.get_type_from_ir_expression(&left)?;
                let right_type = self.get_type_from_ir_expression(&right)?;

                match operator {
                    Tokens::Plus | Tokens::Minus | Tokens::Asterisk | Tokens::Slash | Tokens::Percent => {
                        if left_type == DataType::Number && right_type == DataType::Number {
                            Ok(DataType::Number)
                        } else {
                            Err(TypeError::new(
                                TypeErrorKind::ExpectedDataType(DataType::Number.to_string(), left_type.to_string()),
                                self.position,
                            ))
                        }
                    }
                    Tokens::EQ | Tokens::NEQ | Tokens::LT | Tokens::GT | Tokens::LTE | Tokens::GTE => {
                        if left_type == right_type {
                            Ok(DataType::Boolean)
                        } else {
                            Err(TypeError::new(
                                TypeErrorKind::ExpectedDataType(left_type.to_string(), right_type.to_string()),
                                self.position,
                            ))
                        }
                    }
                    _ => unreachable!(),
                }
            }
        };

        custom_data_type(&result?, &self.customs, &self.position)
    }

    fn get_type_from_literal_value(&self, literal: &LiteralValue) -> CompileResult<DataType> {
        match literal {
            LiteralValue::Number(_) => Ok(DataType::Number),
            LiteralValue::String(_) => Ok(DataType::String),
            LiteralValue::Boolean(_) => Ok(DataType::Boolean),
            LiteralValue::Array(elements) => {
                let mut element_type = DataType::Unknown;

                for element in elements {
                    let data_type = self.get_type_from_ir_expression(element)?;

                    if element_type == DataType::Unknown {
                        element_type = data_type;
                    } else if element_type != data_type {
                        return Err(TypeError::new(
                            TypeErrorKind::ExpectedDataType(element_type.to_string(), data_type.to_string()),
                            self.position,
                        ));
                    }
                }

                match &self.data_type {
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
                                self.position,
                            ));
                        }
                    }
                    None => {
                        if element_type == DataType::Unknown {
                            return Err(TypeError::new(TypeErrorKind::UnknownArrayType, self.position));
                        }
                    }
                }

                Ok(DataType::Array(Box::new(element_type)))
            }
            LiteralValue::Function(parameters, body, return_type, _) => {
                let block_return_type = Box::new(self.get_type_from_ir_expression(&IrExpression::Block(body.clone()))?);

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
                        self.position,
                    ));
                }

                if let Some(data_type) = &self.data_type {
                    if data_type != &function_type {
                        return Err(TypeError::new(
                            TypeErrorKind::ExpectedDataType(data_type.to_string(), function_type.to_string()),
                            self.position,
                        ));
                    }
                }

                Ok(function_type)
            }
        }
    }
}

pub fn custom_data_type(data_type: &DataType, customs: &CustomTypes, position: &Position) -> CompileResult<DataType> {
    Ok(match data_type {
        DataType::Custom(name) => match customs.get(name) {
            Some(custom) => custom,
            None => return Err(TypeError::new(TypeErrorKind::UndefinedType(name.clone()), *position)),
        },
        DataType::Fn(FunctionType(generics, parameters, return_type)) => {
            let return_type = custom_data_type(return_type, customs, position)?;

            DataType::Fn(FunctionType(generics.clone(), parameters.clone(), Box::new(return_type)))
        }
        _ => data_type.clone(),
    })
}
