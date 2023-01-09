use crate::{compiler::CompileResult, TypeError, TypeErrorKind};
use sntk_core::{
    parser::ast::{DataType, DataTypeKind, FunctionType, Parameter, Position},
    tokenizer::token::TokenKind,
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
    pub fn new(parent: Option<DeclaredTypes>) -> Self {
        Self {
            types: HashMap::new(),
            parent: parent.map(Box::new),
        }
    }

    pub fn get(&self, name: String) -> Option<DataType> {
        match self.types.get(&name) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.get(name),
                None => None,
            },
        }
    }

    #[inline]
    pub fn set(&mut self, name: String, value: DataType) {
        self.types.insert(name, value);
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

    pub fn get(&self, name: String) -> Option<DataType> {
        match self.types.get(&name) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.get(name),
                None => None,
            },
        }
    }

    #[inline]
    pub fn set(&mut self, name: String, value: DataType) {
        self.types.insert(name, value);
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

    pub fn get_type_from_ir_expression(&self, expression: &IrExpression) -> CompileResult<DataType> {
        let result = match expression.clone() {
            IrExpression::Identifier(identifier) => match self.declares.get(identifier.clone()) {
                Some(data_type) => Ok(data_type),
                None => Err(TypeError::new(TypeErrorKind::UndefinedIdentifier(identifier), self.position, 3)),
            },
            IrExpression::Literal(literal) => self.get_type_from_literal_value(&literal),
            IrExpression::Block(block) => self.get_type_from_ir_expression(match block.last() {
                Some(instruction) => match instruction.instruction {
                    InstructionType::Return(ref expression) | InstructionType::StoreName(_, ref expression) => expression,
                    _ => return Ok(DataType::new(DataTypeKind::Boolean, self.position)),
                },
                None => return Ok(DataType::new(DataTypeKind::Boolean, self.position)),
            }),
            IrExpression::If(condition, consequence, alternative) => {
                let condition_type = self.get_type_from_ir_expression(&condition)?;
                let consequence_type = self.get_type_from_ir_expression(&consequence)?;
                let alternative_type = match *alternative {
                    Some(alternative) => self.get_type_from_ir_expression(&alternative)?,
                    None => return Err(TypeError::new(TypeErrorKind::IfExpressionWithoutAlternative, self.position, 100)),
                };

                // check if condition is boolean
                if condition_type.data_type == DataTypeKind::Boolean {
                    // check if consequence and alternative are the same type
                    if consequence_type == alternative_type {
                        Ok(consequence_type)
                    }
                    // if consequence and alternative are not the same type
                    else {
                        Err(TypeError::new(
                            TypeErrorKind::ExpectedDataType(consequence_type.to_string(), alternative_type.to_string()),
                            self.position,
                            4,
                        ))
                    }
                }
                // if condition is not boolean
                else {
                    Err(TypeError::new(
                        TypeErrorKind::ExpectedDataType(DataTypeKind::Boolean.to_string(), condition_type.to_string()),
                        self.position,
                        5,
                    ))
                }
            }
            IrExpression::Call(function, arguments) => {
                let function_type = self.get_type_from_ir_expression(&function)?;

                match function_type.data_type {
                    DataTypeKind::Fn(FunctionType { parameters, return_type, .. }) => {
                        let mut arguments_len = arguments.len();

                        for (index, ((parameter, spread), argument)) in parameters.iter().zip(arguments.iter()).enumerate() {
                            let argument_type = self.get_type_from_ir_expression(argument)?;

                            // if parameter is spread
                            if *spread {
                                let parameter = DataType::new(DataTypeKind::Array(Box::new(parameter.clone())), self.position);
                                if parameter != argument_type {
                                    return Err(TypeError::new(
                                        TypeErrorKind::ExpectedDataType(parameter.to_string(), argument_type.to_string()),
                                        self.position,
                                        6,
                                    ));
                                }

                                arguments_len = index + 1;
                                break;
                            }

                            // if parameter and argument are not the same type
                            if parameter != &argument_type {
                                return Err(TypeError::new(
                                    TypeErrorKind::ExpectedDataType(parameter.to_string(), argument_type.to_string()),
                                    self.position,
                                    7,
                                ));
                            }
                        }

                        // if parameters and arguments are not the same length
                        if parameters.len() != arguments_len {
                            return Err(TypeError::new(
                                TypeErrorKind::ExpectedArguments(parameters.len(), arguments.len()),
                                self.position,
                                8,
                            ));
                        }

                        Ok(*return_type)
                    }
                    _ => Err(TypeError::new(TypeErrorKind::NotCallable(function_type.to_string()), self.position, 9)),
                }
            }
            IrExpression::Index(left, index) => {
                let left_type = self.get_type_from_ir_expression(&left)?;
                let index_type = self.get_type_from_ir_expression(&index)?;

                match left_type.data_type {
                    DataTypeKind::Array(data_type) => {
                        if index_type.data_type == DataTypeKind::Number {
                            Ok(*data_type)
                        } else {
                            Err(TypeError::new(
                                TypeErrorKind::ExpectedDataType(DataTypeKind::Number.to_string(), index_type.to_string()),
                                self.position,
                                10,
                            ))
                        }
                    }
                    _ => Err(TypeError::new(TypeErrorKind::NotIndexable(left_type.to_string()), self.position, 11)),
                }
            }
            IrExpression::Prefix(_, expression) => self.get_type_from_ir_expression(&expression),
            IrExpression::Infix(left, operator, right) => Ok({
                let left_type = self.get_type_from_ir_expression(&left)?;
                let right_type = self.get_type_from_ir_expression(&right)?;

                DataType::new(
                    match operator {
                        TokenKind::Plus | TokenKind::Minus | TokenKind::Asterisk | TokenKind::Slash | TokenKind::Percent => {
                            if left_type.data_type == DataTypeKind::Number && right_type.data_type == DataTypeKind::Number {
                                Ok(DataTypeKind::Number)
                            } else {
                                Err(TypeError::new(
                                    TypeErrorKind::ExpectedDataType(DataTypeKind::Number.to_string(), left_type.to_string()),
                                    self.position,
                                    12,
                                ))
                            }
                        }
                        TokenKind::EQ | TokenKind::NEQ | TokenKind::LT | TokenKind::GT | TokenKind::LTE | TokenKind::GTE => {
                            if left_type == right_type {
                                Ok(DataTypeKind::Boolean)
                            } else {
                                Err(TypeError::new(
                                    TypeErrorKind::ExpectedDataType(left_type.to_string(), right_type.to_string()),
                                    self.position,
                                    13,
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

    fn get_type_from_literal_value(&self, literal: &LiteralValue) -> CompileResult<DataType> {
        Ok(DataType::new(
            match literal {
                LiteralValue::Number(_) => Ok(DataTypeKind::Number),
                LiteralValue::String(_) => Ok(DataTypeKind::String),
                LiteralValue::Boolean(_) => Ok(DataTypeKind::Boolean),
                LiteralValue::Array(elements) => {
                    let mut element_type = DataTypeKind::Unknown;

                    // check elements
                    for element in elements {
                        let data_type = self.get_type_from_ir_expression(element)?;

                        if element_type == DataTypeKind::Unknown {
                            element_type = data_type.data_type; // default element type
                        }
                        // if element type is not the same as the default element type
                        else if element_type != data_type.data_type {
                            return Err(TypeError::new(
                                TypeErrorKind::ExpectedDataType(element_type.to_string(), data_type.to_string()),
                                self.position,
                                14,
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
                                return Err(TypeError::new(
                                    TypeErrorKind::ExpectedDataType(
                                        data_type.to_string(),
                                        DataTypeKind::Array(Box::new(DataType::new(element_type, self.position))).to_string(),
                                    ),
                                    self.position,
                                    15,
                                ));
                            }
                        }
                        None => {
                            if element_type == DataTypeKind::Unknown {
                                return Err(TypeError::new(TypeErrorKind::UnknownArrayType, self.position, 16));
                            }
                        }
                    }

                    Ok(DataTypeKind::Array(Box::new(DataType::new(element_type, self.position))))
                }
                LiteralValue::Function(parameters, body, return_type, _) => {
                    let block_return_type = Box::new(self.get_type_from_ir_expression(&IrExpression::Block(body.clone()))?);

                    let function_type = DataTypeKind::Fn(FunctionType {
                        generics: None,
                        parameters: parameters
                            .iter()
                            .map(|Parameter { data_type, spread, .. }| (data_type.clone(), *spread))
                            .collect(),
                        return_type: block_return_type.clone(),
                    });

                    if return_type.clone() == DataTypeKind::Auto || return_type.clone() != block_return_type.data_type {
                        return Err(TypeError::new(
                            TypeErrorKind::ExpectedDataType(return_type.to_string(), block_return_type.to_string()),
                            self.position,
                            17,
                        ));
                    }

                    if let Some(data_type) = &self.data_type {
                        if data_type.data_type != function_type {
                            return Err(TypeError::new(
                                TypeErrorKind::ExpectedDataType(data_type.to_string(), function_type.to_string()),
                                self.position,
                                18,
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
        DataTypeKind::Custom(name) => match customs.get(name.clone()) {
            Some(custom) => custom,
            None => return Err(TypeError::new(TypeErrorKind::UndefinedType(name.clone()), *position, 19)),
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

#[cfg(test)]
mod tests {
    use super::{Checker, CustomTypes, DeclaredTypes};
    use sntk_core::{
        parser::ast::{DataType, DataTypeKind, FunctionType, Position},
        tokenizer::token::TokenKind,
    };
    use sntk_ir::instruction::{Instruction, InstructionType, IrExpression, LiteralValue};
    use std::collections::HashMap;

    const POSITION: Position = Position(1, 0);

    #[test]
    fn identifier_type_test() {
        let declarations = DeclaredTypes {
            types: HashMap::from([("a".to_string(), DataType::new(DataTypeKind::Number, POSITION))]),
            parent: Some(Box::new(DeclaredTypes {
                types: HashMap::from([("a".to_string(), DataType::new(DataTypeKind::String, POSITION))]),
                parent: None,
            })),
        };

        assert_eq!(
            Checker::new(None, &declarations, &CustomTypes::new(None), POSITION)
                .unwrap()
                .get_type_from_ir_expression(&IrExpression::Identifier("a".to_string()))
                .unwrap(),
            DataType::new(DataTypeKind::Number, POSITION)
        );
    }

    #[test]
    fn block_return_type_test() {
        let declarations = DeclaredTypes {
            types: HashMap::from([
                ("a".to_string(), DataType::new(DataTypeKind::Number, POSITION)),
                ("b".to_string(), DataType::new(DataTypeKind::Number, POSITION)),
            ]),
            parent: None,
        };

        assert_eq!(
            Checker::new(None, &declarations, &CustomTypes::new(None), POSITION)
                .unwrap()
                .get_type_from_ir_expression(&IrExpression::Block(vec![
                    Instruction::new(
                        InstructionType::StoreName("a".to_string(), IrExpression::Literal(LiteralValue::Number(5.))),
                        POSITION
                    ),
                    Instruction::new(
                        InstructionType::StoreName("b".to_string(), IrExpression::Literal(LiteralValue::Number(5.))),
                        POSITION
                    ),
                    Instruction::new(
                        InstructionType::Return(IrExpression::Infix(
                            Box::new(IrExpression::Identifier("a".to_string())),
                            TokenKind::Plus,
                            Box::new(IrExpression::Identifier("b".to_string()))
                        )),
                        POSITION
                    )
                ]))
                .unwrap(),
            DataType::new(DataTypeKind::Number, POSITION)
        );
    }

    #[test]
    fn if_return_type_test() {
        let declarations = DeclaredTypes {
            types: HashMap::from([
                ("a".to_string(), DataType::new(DataTypeKind::Number, POSITION)),
                ("b".to_string(), DataType::new(DataTypeKind::Number, POSITION)),
            ]),
            parent: None,
        };

        let consequence = IrExpression::Block(vec![Instruction::new(
            InstructionType::Return(IrExpression::Literal(LiteralValue::Number(5.))),
            POSITION,
        )]);
        let alternative = IrExpression::Block(vec![Instruction::new(
            InstructionType::Return(IrExpression::Literal(LiteralValue::Number(10.))),
            POSITION,
        )]);

        assert_eq!(
            Checker::new(None, &declarations, &CustomTypes::new(None), POSITION)
                .unwrap()
                .get_type_from_ir_expression(&IrExpression::If(
                    Box::new(IrExpression::Literal(LiteralValue::Boolean(true))),
                    Box::new(consequence),
                    Box::new(Some(alternative))
                ))
                .unwrap(),
            DataType::new(DataTypeKind::Number, POSITION)
        );
    }

    #[test]
    fn call_return_type_test() {
        let declarations = DeclaredTypes {
            types: HashMap::from([(
                "a".to_string(),
                DataType::new(
                    DataTypeKind::Fn(FunctionType {
                        generics: None,
                        parameters: vec![
                            (DataType::new(DataTypeKind::Number, POSITION), false),
                            (DataType::new(DataTypeKind::String, POSITION), true),
                        ],
                        return_type: Box::new(DataType::new(DataTypeKind::Number, POSITION)),
                    }),
                    POSITION,
                ),
            )]),
            parent: None,
        };

        assert_eq!(
            Checker::new(None, &declarations, &CustomTypes::new(None), POSITION)
                .unwrap()
                .get_type_from_ir_expression(&IrExpression::Call(
                    Box::new(IrExpression::Identifier("a".to_string())),
                    vec![
                        IrExpression::Literal(LiteralValue::Number(5.)),
                        IrExpression::Literal(LiteralValue::Array(vec![
                            IrExpression::Literal(LiteralValue::String("foo".to_string())),
                            IrExpression::Literal(LiteralValue::String("bar".to_string())),
                            IrExpression::Literal(LiteralValue::String("baz".to_string())),
                        ]))
                    ]
                ))
                .unwrap(),
            DataType::new(DataTypeKind::Number, POSITION)
        );
    }

    #[test]
    fn index_type() {
        assert_eq!(
            Checker::new(None, &DeclaredTypes::new(None), &CustomTypes::new(None), POSITION)
                .unwrap()
                .get_type_from_ir_expression(&IrExpression::Index(
                    Box::new(IrExpression::Literal(LiteralValue::Array(vec![
                        IrExpression::Literal(LiteralValue::Number(5.)),
                        IrExpression::Literal(LiteralValue::Number(10.)),
                        IrExpression::Literal(LiteralValue::Number(15.)),
                    ]))),
                    Box::new(IrExpression::Literal(LiteralValue::Number(1.)))
                ))
                .unwrap(),
            DataType::new(DataTypeKind::Number, POSITION)
        );
    }

    #[test]
    fn prefix_type() {
        assert_eq!(
            Checker::new(None, &DeclaredTypes::new(None), &CustomTypes::new(None), POSITION)
                .unwrap()
                .get_type_from_ir_expression(&IrExpression::Prefix(
                    TokenKind::Minus,
                    Box::new(IrExpression::Literal(LiteralValue::Number(5.)))
                ))
                .unwrap(),
            DataType::new(DataTypeKind::Number, POSITION)
        );
    }

    #[test]
    fn infix_type() {
        assert_eq!(
            Checker::new(None, &DeclaredTypes::new(None), &CustomTypes::new(None), POSITION)
                .unwrap()
                .get_type_from_ir_expression(&IrExpression::Infix(
                    Box::new(IrExpression::Literal(LiteralValue::Number(5.))),
                    TokenKind::EQ,
                    Box::new(IrExpression::Literal(LiteralValue::Number(10.)))
                ))
                .unwrap(),
            DataType::new(DataTypeKind::Boolean, POSITION)
        );
    }

    #[test]
    fn literal_type_test() {
        assert_eq!(
            Checker::new(None, &DeclaredTypes::new(None), &CustomTypes::new(None), POSITION)
                .unwrap()
                .get_type_from_ir_expression(&IrExpression::Literal(LiteralValue::Number(10.0)))
                .unwrap(),
            DataType::new(DataTypeKind::Number, POSITION)
        );
    }
}
