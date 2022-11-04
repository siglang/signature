use crate::{
    compiler::CompileResult,
    error::{CompileError, TypeError, EXPECTED_DATA_TYPE, UNEXPECTED_PARAMETER_LENGTH, UNKNOWN_ARRAY_TYPE, UNKNOWN_TYPE},
    helpers::literal_value,
    type_error,
};
use sntk_core::parser::ast::{DataType, Expression, FunctionType, Position};
use sntk_ir::{
    code::{BinaryOp, Block, Instruction, UnaryOp},
    value::{LiteralValue, Value},
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub struct Type(pub DataType);

pub type SharedTypeEnvironment = Rc<RefCell<TypeEnvironment>>;

#[rustfmt::skip]
pub trait TypeTrait {
    fn get_type(value: &Value, position: &Position, type_environment: SharedTypeEnvironment) -> CompileResult<Type>;
    fn get_type_from_expression(expression: &Expression, position: &Position, type_environment: SharedTypeEnvironment) -> CompileResult<Type>;
    fn get_data_type(value: &Value, position: &Position, type_environment: SharedTypeEnvironment) -> CompileResult<DataType>;
    fn get_data_type_from_expression(expression: &Expression, position: &Position, type_environment: SharedTypeEnvironment) -> CompileResult<DataType>;
    fn get_type_from_instruction(instruction: Vec<Instruction>, position: &Position, type_environment: SharedTypeEnvironment) -> Option<CompileResult<Type>>;
    fn get_data_type_from_instruction(instruction: Vec<Instruction>, position: &Position, type_environment: SharedTypeEnvironment) -> Option<CompileResult<DataType>>;
    fn eq_from_value(&self, value: &Value, position: &Position, type_environment: SharedTypeEnvironment) -> CompileResult<bool>;
    fn eq_from_type(&self, other: &Type) -> bool;
}

impl TypeTrait for Type {
    fn get_type(value: &Value, position: &Position, type_environment: SharedTypeEnvironment) -> CompileResult<Type> {
        match value {
            Value::LiteralValue(literal_value) => match literal_value {
                LiteralValue::Number(_) => Ok(Type(DataType::Number)),
                LiteralValue::Boolean(_) => Ok(Type(DataType::Boolean)),
                LiteralValue::String(_) => Ok(Type(DataType::String)),
                LiteralValue::Array(elements) => expand_array_type(elements, position, None, type_environment),
                LiteralValue::Function(parameters, body, return_type) => {
                    expand_function_type(parameters, body, return_type, position, None, type_environment)
                }
            },
            Value::Identifier(identifier) => {
                if let Some(data_type) = type_environment.borrow().get(identifier) {
                    Ok(Type(data_type.clone()))
                } else {
                    Err(type_error! { UNKNOWN_TYPE; identifier; position.clone(); })
                }
            }
            Value::Return(value) => Type::get_type(value, position, type_environment),
        }
    }

    fn get_type_from_expression(expression: &Expression, position: &Position, type_environment: SharedTypeEnvironment) -> CompileResult<Type> {
        Type::get_type(
            &literal_value(expression.clone(), Rc::clone(&type_environment))?,
            position,
            Rc::clone(&type_environment),
        )
    }

    fn get_data_type(value: &Value, position: &Position, type_environment: SharedTypeEnvironment) -> CompileResult<DataType> {
        Ok(Type::get_type(value, position, type_environment)?.0)
    }

    fn get_data_type_from_expression(
        expression: &Expression,
        position: &Position,
        type_environment: SharedTypeEnvironment,
    ) -> CompileResult<DataType> {
        Ok(Type::get_type_from_expression(expression, position, type_environment)?.0)
    }

    fn get_type_from_instruction(
        instruction: Vec<Instruction>,
        position: &Position,
        type_environment: SharedTypeEnvironment,
    ) -> Option<CompileResult<Type>> {
        Some(match instruction.last()? /* TODO: Error Handling */ {
            Instruction::LoadConst(value) => Type::get_type(value, position, type_environment),
            Instruction::BinaryOp(op) => match op {
                BinaryOp::Add => {
                    let left = match Type::get_type_from_instruction(Vec::from(&instruction[0..instruction.len() - 1]), position, Rc::clone(&type_environment))? {
                        Ok(t) => t,
                        Err(e) => return Some(Err(e)),
                    };
                    let right = match Type::get_type_from_instruction(Vec::from(&instruction[1..instruction.len()]), position, type_environment)? {
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
                Type::get_type_from_instruction(instructions.clone(), position, type_environment)?
            }
            Instruction::Return => match Type::get_type_from_instruction(Vec::from(&instruction[instruction.len() - 2..1]), position, type_environment)? {
                Ok(t) => Ok(t),
                Err(e) => return Some(Err(e)),
            },
            _ => panic!("Invalid instruction"),
        })
    }

    fn get_data_type_from_instruction(
        instruction: Vec<Instruction>,
        position: &Position,
        type_environment: SharedTypeEnvironment,
    ) -> Option<CompileResult<DataType>> {
        Some(Type::get_type_from_instruction(instruction, position, type_environment)?.map(|t| t.0))
    }

    fn eq_from_value(&self, value: &Value, position: &Position, type_environment: SharedTypeEnvironment) -> CompileResult<bool> {
        match self {
            Type(DataType::Number) => Ok(Type::get_type(value, position, type_environment)? == Type(DataType::Number)),
            Type(DataType::Boolean) => Ok(Type::get_type(value, position, type_environment)? == Type(DataType::Boolean)),
            Type(DataType::String) => Ok(Type::get_type(value, position, type_environment)? == Type(DataType::String)),
            Type(DataType::Array(_)) => Ok(Type::get_type(value, position, type_environment)? == self.clone()),
            Type(DataType::Fn(_)) => Ok(Type::get_type(value, position, type_environment)? == self.clone()),
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

#[derive(Debug, PartialEq, Clone)]
pub struct TypeEnvironment {
    pub types: HashMap<String, DataType>,
    pub parent: Option<Box<SharedTypeEnvironment>>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_with_parent(parent: SharedTypeEnvironment) -> Self {
        Self {
            types: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn get(&self, name: &str) -> Option<DataType> {
        match self.types.get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.borrow().get(name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: String, value: DataType) {
        self.types.insert(name, value);
    }
}

#[inline]
pub fn expand_array_type(
    elements: &Vec<Value>,
    position: &Position,
    comparison_target: Option<&DataType>,
    type_environment: SharedTypeEnvironment,
) -> CompileResult<Type> {
    let comparison_target_expand = match comparison_target {
        Some(data_type) => data_type.clone(),
        None => {
            if elements.is_empty() {
                return Err(type_error! { UNKNOWN_ARRAY_TYPE; DataType::Number, DataType::Boolean, DataType::String; position.clone(); });
            }

            DataType::Array(Box::new(Type::get_data_type(&elements[0], position, Rc::clone(&type_environment))?))
        }
    };

    let element_type = match comparison_target_expand.clone() {
        DataType::Array(element) => *element,
        data_type => {
            return Err(
                type_error! { EXPECTED_DATA_TYPE; data_type, DataType::Array(Box::new(Type::get_data_type(&elements[0], position, type_environment)?)); position.clone(); },
            )
        }
    };

    for element in elements {
        if !Type::get_type(element, position, Rc::clone(&type_environment))?.eq_from_type(&Type(element_type.clone())) {
            return Err(
                type_error! { EXPECTED_DATA_TYPE; element_type, Type::get_data_type(element, position, type_environment)?; position.clone(); },
            );
        }
    }

    if let Some(data_type) = comparison_target {
        if Type(data_type.clone()) != Type(comparison_target_expand.clone()) {
            return Err(type_error! { EXPECTED_DATA_TYPE; data_type, comparison_target_expand; position.clone(); });
        }
    }

    Ok(Type(comparison_target_expand))
}

#[inline]
pub fn expand_function_type(
    parameters: &Vec<(String, DataType)>,
    body: &Block,
    return_type: &DataType,
    position: &Position,
    comparison_target: Option<&DataType>,
    type_environment: SharedTypeEnvironment,
) -> CompileResult<Type> {
    let comparison_target_expand = match comparison_target {
        Some(data_type) => data_type.clone(),
        None => {
            if body.0.is_empty() {
                DataType::Fn(FunctionType(
                    None,
                    parameters.clone().iter().map(|x| x.clone().1).collect(),
                    Box::new(DataType::Void),
                ))
            } else {
                let return_type = Box::new(
                    Type::get_type_from_instruction(body.0.clone(), position, Rc::clone(&type_environment))
                        .transpose()?
                        .map(|t| t.0)
                        .unwrap_or(DataType::Void),
                );

                DataType::Fn(FunctionType(None, parameters.clone().iter().map(|x| x.clone().1).collect(), return_type))
            }
        }
    };

    let function_type = match comparison_target_expand.clone() {
        DataType::Fn(function_type) => function_type,
        data_type => {
            let function = DataType::Fn(FunctionType(
                None,
                parameters.clone().iter().map(|x| x.clone().1).collect(),
                Box::new(DataType::Void),
            ));

            return Err(type_error! { EXPECTED_DATA_TYPE; data_type, function; position.clone(); });
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
        Type::get_type_from_instruction(body.0.clone(), position, type_environment)
            .transpose()?
            .map(|t| t.0)
            .unwrap_or(DataType::Void)
    };

    if return_type != &body_return_type {
        return Err(type_error! { EXPECTED_DATA_TYPE; return_type, body_return_type; position.clone(); });
    }

    if let Some(data_type) = comparison_target {
        let function = FunctionType(
            None,
            parameters.clone().iter().map(|x| x.clone().1).collect(),
            Box::new(return_type.clone()),
        );

        if Type(data_type.clone()) != Type(DataType::Fn(function.clone())) {
            return Err(type_error! { EXPECTED_DATA_TYPE; data_type.clone(), DataType::Fn(function); position.clone(); });
        }
    }

    Ok(Type(comparison_target_expand))
}
