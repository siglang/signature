use crate::{compiler::CompileResult, TypeError, TypeErrorKind};
use parser::{
    parser::{DataType, DataTypeKind, FunctionType, Parameter, ParameterKind, Position},
    tokenizer::TokenKind,
};
use ir_interpreter::instruction::{InstructionType, IrExpression, LiteralValue};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct DeclaredTypes {
    pub types: HashMap<String, DataType>,
    pub parent: Option<Box<DeclaredTypes>>,
}

impl DeclaredTypes {
    #[inline]
    pub fn new(parent: Option<DeclaredTypes>) -> Self {
        Self {
            types: HashMap::new(),
            parent: parent.map(Box::new),
        }
    }

    pub fn get(&self, identifier: String) -> Option<DataType> {
        match self.types.get(&identifier) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.get(identifier),
                None => None,
            },
        }
    }

    #[inline]
    pub fn set(&mut self, identifier: String, value: DataType) {
        self.types.insert(identifier, value);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CustomTypes {
    pub types: HashMap<String, DataType>,
    pub parent: Option<Box<CustomTypes>>,
}

impl CustomTypes {
    #[inline]
    pub fn new(parent: Option<CustomTypes>) -> Self {
        Self {
            types: HashMap::new(),
            parent: parent.map(Box::new),
        }
    }

    pub fn get(&self, identifier: String) -> Option<DataType> {
        match self.types.get(&identifier) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.get(identifier),
                None => None,
            },
        }
    }

    #[inline]
    pub fn set(&mut self, identifier: String, value: DataType) {
        self.types.insert(identifier, value);
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
    pub fn new(data_type: Option<&DataType>, declares: &DeclaredTypes, customs: &CustomTypes, position: Position) -> CompileResult<Self> {
        Ok(Self {
            data_type: data_type.map(|data_type| custom_data_type(data_type, customs)).transpose()?,
            declares: declares.clone(),
            customs: customs.clone(),
            position,
        })
    }

    pub fn from_ir_expression(&self, expression: &IrExpression) -> CompileResult<DataType> {
        let result = match expression.clone() {
            IrExpression::Identifier(identifier) => match self.declares.get(identifier.clone()) {
                Some(data_type) => Ok(data_type),
                None => Err(TypeError::new::<&str>(
                    TypeErrorKind::UndefinedIdentifier(identifier),
                    None,
                    self.position,
                )),
            },
            IrExpression::Literal(literal) => self.from_literal_value(&literal),
            IrExpression::Block(block) => self.from_ir_expression(match block.last() {
                Some(instruction) => match instruction.instruction {
                    InstructionType::Return(ref expression) | InstructionType::Storeidentifier(_, ref expression) => expression,
                    _ => return Ok(DataType::new(DataTypeKind::Boolean, self.position)),
                },
                None => return Ok(DataType::new(DataTypeKind::Boolean, self.position)),
            }),
            IrExpression::If(condition, consequence, alternative) => {
                let condition_type = self.from_ir_expression(&condition)?;
                let consequence_type = self.from_ir_expression(&consequence)?;
                let alternative_type = match *alternative {
                    Some(alternative) => self.from_ir_expression(&alternative)?,
                    None => return Err(TypeError::new::<&str>(TypeErrorKind::IfExpressionWithoutAlternative, None, self.position)),
                };

                // check if condition is boolean
                if condition_type.data_type == DataTypeKind::Boolean {
                    // check if consequence and alternative are the same type
                    if consequence_type == alternative_type {
                        Ok(consequence_type)
                    }
                    // if consequence and alternative are not the same type
                    else {
                        Err(TypeError::new::<&str>(
                            TypeErrorKind::ExpectedDataType(consequence_type.to_string(), alternative_type.to_string()),
                            None,
                            self.position,
                        ))
                    }
                }
                // if condition is not boolean
                else {
                    Err(TypeError::new::<&str>(
                        TypeErrorKind::ExpectedDataType(DataTypeKind::Boolean.to_string(), condition_type.to_string()),
                        None,
                        self.position,
                    ))
                }
            }
            IrExpression::Call(function, arguments) => {
                let function_type = self.from_ir_expression(&function)?;

                match function_type.data_type {
                    DataTypeKind::Fn(FunctionType { parameters, return_type, .. }) => {
                        let mut arguments_len = arguments.len();

                        for (index, ((parameter, kind), argument)) in parameters.iter().zip(arguments.iter()).enumerate() {
                            let argument_type = self.from_ir_expression(argument)?;

                            if let ParameterKind::Spread = kind {
                                let parameter = DataType::new(DataTypeKind::Array(Box::new(parameter.clone())), self.position);
                                if parameter != argument_type {
                                    return Err(TypeError::new::<&str>(
                                        TypeErrorKind::ExpectedDataType(parameter.to_string(), argument_type.to_string()),
                                        None,
                                        self.position,
                                    ));
                                }

                                arguments_len = index + 1;
                                break;
                            }

                            // if parameter and argument are not the same type
                            if parameter != &argument_type {
                                return Err(TypeError::new::<&str>(
                                    TypeErrorKind::ExpectedDataType(parameter.to_string(), argument_type.to_string()),
                                    None,
                                    self.position,
                                ));
                            }
                        }

                        // if parameters and arguments are not the same length
                        if parameters.len() != arguments_len {
                            return Err(TypeError::new::<&str>(
                                TypeErrorKind::ExpectedArguments(parameters.len(), arguments.len()),
                                None,
                                self.position,
                            ));
                        }

                        Ok(*return_type)
                    }
                    _ => Err(TypeError::new::<&str>(
                        TypeErrorKind::NotCallable(function_type.to_string()),
                        None,
                        self.position,
                    )),
                }
            }
            IrExpression::Index(left, index) => {
                let left_type = self.from_ir_expression(&left)?;
                let index_type = self.from_ir_expression(&index)?;

                match left_type.data_type {
                    DataTypeKind::Array(data_type) => {
                        if index_type.data_type == DataTypeKind::Number {
                            Ok(*data_type)
                        } else {
                            Err(TypeError::new::<&str>(
                                TypeErrorKind::ExpectedDataType(DataTypeKind::Number.to_string(), index_type.to_string()),
                                None,
                                self.position,
                            ))
                        }
                    }
                    _ => Err(TypeError::new::<&str>(
                        TypeErrorKind::NotIndexable(left_type.to_string()),
                        None,
                        self.position,
                    )),
                }
            }
            IrExpression::Prefix(_, expression) => self.from_ir_expression(&expression),
            IrExpression::Infix(operator, left, right) => Ok({
                let left_type = self.from_ir_expression(&left)?;
                let right_type = self.from_ir_expression(&right)?;

                DataType::new(
                    match operator {
                        TokenKind::Plus | TokenKind::Minus | TokenKind::Asterisk | TokenKind::Slash | TokenKind::Percent => {
                            if left_type.data_type == DataTypeKind::Number && right_type.data_type == DataTypeKind::Number {
                                Ok(DataTypeKind::Number)
                            } else {
                                Err(TypeError::new::<&str>(
                                    TypeErrorKind::ExpectedDataType(DataTypeKind::Number.to_string(), left_type.to_string()),
                                    None,
                                    self.position,
                                ))
                            }
                        }
                        TokenKind::EQ | TokenKind::NEQ | TokenKind::LT | TokenKind::GT | TokenKind::LTE | TokenKind::GTE => {
                            if left_type == right_type {
                                Ok(DataTypeKind::Boolean)
                            } else {
                                Err(TypeError::new::<&str>(
                                    TypeErrorKind::ExpectedDataType(left_type.to_string(), right_type.to_string()),
                                    None,
                                    self.position,
                                ))
                            }
                        }
                        _ => unreachable!(),
                    }?,
                    self.position,
                )
            }),
        };

        custom_data_type(&result?, &self.customs)
    }

    fn from_literal_value(&self, literal: &LiteralValue) -> CompileResult<DataType> {
        Ok(DataType::new(
            match literal {
                LiteralValue::Number(_) => Ok(DataTypeKind::Number),
                LiteralValue::String(_) => Ok(DataTypeKind::String),
                LiteralValue::Boolean(_) => Ok(DataTypeKind::Boolean),
                LiteralValue::Array(elements) => {
                    let mut element_type = DataTypeKind::Unknown;

                    // check elements
                    for element in elements {
                        let data_type = self.from_ir_expression(element)?;

                        if element_type == DataTypeKind::Unknown {
                            element_type = data_type.data_type; // default element type
                        }
                        // if element type is not the same as the default element type
                        else if element_type != data_type.data_type {
                            return Err(TypeError::new::<&str>(
                                TypeErrorKind::ExpectedDataType(element_type.to_string(), data_type.to_string()),
                                None,
                                self.position,
                            ));
                        }
                    }

                    match &self.data_type {
                        Some(data_type) => {
                            if element_type == DataTypeKind::Unknown {
                                element_type = match data_type.data_type.clone() {
                                    DataTypeKind::Array(data_type) => data_type.data_type.clone(),
                                    _ => unreachable!(),
                                };
                            }

                            if data_type.data_type != DataTypeKind::Array(Box::new(DataType::new(element_type.clone(), self.position))) {
                                return Err(TypeError::new::<&str>(
                                    TypeErrorKind::ExpectedDataType(
                                        data_type.to_string(),
                                        DataTypeKind::Array(Box::new(DataType::new(element_type, self.position))).to_string(),
                                    ),
                                    None,
                                    self.position,
                                ));
                            }
                        }
                        None => {
                            if element_type == DataTypeKind::Unknown {
                                return Err(TypeError::new::<&str>(TypeErrorKind::UnknownArrayType, None, self.position));
                            }
                        }
                    }

                    Ok(DataTypeKind::Array(Box::new(DataType::new(element_type, self.position))))
                }
                LiteralValue::Function(parameters, body, return_type, _) => {
                    let block_return_type = Box::new(self.from_ir_expression(&IrExpression::Block(body.clone()))?);

                    let function_type = DataTypeKind::Fn(FunctionType {
                        generics: None,
                        parameters: parameters
                            .iter()
                            .map(|Parameter { data_type, kind, .. }| (data_type.clone(), *kind))
                            .collect(),
                        return_type: block_return_type.clone(),
                    });

                    if return_type.clone() == DataTypeKind::Auto || return_type.clone() != block_return_type.data_type {
                        return Err(TypeError::new::<&str>(
                            TypeErrorKind::ExpectedDataType(return_type.to_string(), block_return_type.to_string()),
                            None,
                            self.position,
                        ));
                    }

                    if let Some(data_type) = &self.data_type {
                        if data_type.data_type != function_type {
                            return Err(TypeError::new::<&str>(
                                TypeErrorKind::ExpectedDataType(data_type.to_string(), function_type.to_string()),
                                None,
                                self.position,
                            ));
                        }
                    }

                    Ok(function_type)
                }
            }?,
            self.position,
        ))
    }
}

pub fn custom_data_type(data_type: &DataType, customs: &CustomTypes) -> CompileResult<DataType> {
    let data_type_ @ DataType { data_type, position } = data_type;

    Ok(match &data_type {
        DataTypeKind::Custom(identifier) => match customs.get(identifier.clone()) {
            Some(custom) => custom,
            None => return Err(TypeError::new::<&str>(TypeErrorKind::UndefinedType(identifier.clone()), None, *position)),
        },
        DataTypeKind::Fn(FunctionType {
            generics,
            parameters,
            return_type,
        }) => DataType::new(
            DataTypeKind::Fn(FunctionType {
                generics: generics.clone(),
                parameters: parameters.clone(),
                return_type: Box::new(custom_data_type(return_type, customs)?),
            }),
            *position,
        ),
        _ => data_type_.clone(),
    })
}
