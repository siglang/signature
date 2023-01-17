use crate::{
    builtin::builtin_function,
    instruction::{Instruction, InstructionType, IrExpression, LiteralValue},
    RuntimeError, RuntimeErrorKind,
};
use sntk_core::{parser::Position, tokenizer::TokenKind};
use std::{collections::HashMap, fmt};

#[derive(Clone, PartialEq)]
pub struct IrEnvironment {
    pub values: HashMap<String, LiteralValue>,
    pub parent: Option<Box<IrEnvironment>>,
}

impl IrEnvironment {
    #[inline]
    pub fn new(parent: Option<IrEnvironment>) -> Self {
        Self {
            values: HashMap::new(),
            parent: parent.map(Box::new),
        }
    }

    pub fn get(&self, name: String) -> Option<LiteralValue> {
        match self.values.get(&name) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.get(name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: String, value: LiteralValue) {
        self.values.insert(name, value);
    }
}

impl fmt::Debug for IrEnvironment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{ values: {:?}, parent: {:?} }}", self.values.keys(), self.parent)
    }
}

#[derive(Debug, Clone)]
pub struct IrInterpreter {
    pub instructions: Vec<Instruction>,
    pub environment: IrEnvironment,
}

pub type Result<T> = std::result::Result<T, crate::RuntimeError>;

impl IrInterpreter {
    pub fn new(instructions: Vec<Instruction>) -> Self {
        Self {
            instructions,
            environment: IrEnvironment::new(None),
        }
    }

    pub fn new_with_environment(instructions: Vec<Instruction>, environment: IrEnvironment) -> Self {
        Self { instructions, environment }
    }

    pub fn eval(&mut self) -> Result<()> {
        for instruction in self.instructions.clone().iter() {
            self.eval_instruction(instruction)?;
        }

        Ok(())
    }

    pub fn last(&mut self) -> Result<LiteralValue> {
        self.clone()
            .instructions
            .last()
            .map(|instruction| {
                let position = Position(instruction.position.0, instruction.position.1);
                match instruction.instruction.clone() {
                    InstructionType::Return(expression) => self.eval_expression(&expression, &position),
                    _ => Ok(LiteralValue::Boolean(false)),
                }
            })
            .unwrap_or(Ok(LiteralValue::Boolean(false)))
    }

    pub fn eval_instruction(&mut self, instruction: &Instruction) -> Result<()> {
        let position = Position(instruction.position.0, instruction.position.1);

        match instruction.instruction.clone() {
            InstructionType::StoreName(name, expression) => {
                let expression = self.eval_expression(&expression, &position)?;
                self.environment.set(name, expression);
            }
            InstructionType::Expression(expression) => {
                self.eval_expression(&expression, &position)?;
            }
            InstructionType::Return(_) | InstructionType::None => {}
        }

        Ok(())
    }

    pub fn eval_expression(&mut self, expression: &IrExpression, position: &Position) -> Result<LiteralValue> {
        match expression {
            IrExpression::Identifier(name) => match self.environment.get(name.clone()) {
                Some(value) => Ok(value),
                None => Err(RuntimeError::new(RuntimeErrorKind::UndefinedVariable(name.to_string()), *position)),
            },
            IrExpression::Literal(value) => match value {
                LiteralValue::Array(array) => {
                    let array = array
                        .iter()
                        .map(|expression| self.eval_expression(expression, position))
                        .collect::<std::result::Result<Vec<LiteralValue>, RuntimeError>>()?
                        .iter()
                        .map(|value| IrExpression::Literal(value.clone()))
                        .collect();

                    Ok(LiteralValue::Array(array))
                }
                _ => Ok(value.clone()),
            },
            IrExpression::Block(block) => {
                let mut interpreter = IrInterpreter::new_with_environment(block.clone(), IrEnvironment::new(Some(self.environment.clone())));
                interpreter.eval()?;
                Ok(interpreter.last()?)
            }
            IrExpression::If(condition, consequence, alternative) => {
                let condition = self.eval_expression(condition, position)?;

                match condition {
                    LiteralValue::Boolean(true) => self.eval_expression(consequence, position),
                    LiteralValue::Boolean(false) => match *alternative.clone() {
                        Some(alternative) => self.eval_expression(&alternative, position),
                        None => Ok(LiteralValue::Boolean(false)),
                    },
                    _ => unreachable!(),
                }
            }
            IrExpression::Call(function, arguments) => {
                let arguments = arguments
                    .iter()
                    .map(|argument| self.eval_expression(argument, position))
                    .collect::<std::result::Result<Vec<_>, _>>()?;

                let function = match *function.clone() {
                    IrExpression::Identifier(name) => match self.environment.get(name.clone()) {
                        Some(value) => value,
                        None => {
                            return match builtin_function(&name) {
                                Some(function) => Ok(function(arguments.iter().collect())),
                                None => Err(RuntimeError::new(RuntimeErrorKind::UndefinedVariable(name.to_string()), *position)),
                            };
                        }
                    },
                    _ => self.eval_expression(function, position)?,
                };

                let (parameters, body, mut environment) = match function {
                    LiteralValue::Function(parameters, block, _, environment) => (
                        parameters.iter().map(|parameter| parameter.name.value.clone()).collect::<Vec<_>>(),
                        block,
                        match environment {
                            Some(environment) => environment,
                            None => IrEnvironment::new(Some(self.environment.clone())),
                        },
                    ),
                    value => return Err(RuntimeError::new(RuntimeErrorKind::NotAFunction(value.to_string()), *position)),
                };

                for (parameter, argument) in parameters.iter().zip(arguments.iter()) {
                    environment.set(parameter.clone(), argument.clone());
                }

                let mut interpreter = IrInterpreter::new_with_environment(body, environment.clone());
                interpreter.eval()?;

                let last = match interpreter.last()? {
                    LiteralValue::Function(parameters, body, return_type, function_environment) => LiteralValue::Function(
                        parameters,
                        body,
                        return_type,
                        Some(IrEnvironment::new(Some(match function_environment {
                            Some(environment) => environment,
                            None => environment,
                        }))),
                    ),
                    value => value,
                };

                Ok(last)
            }
            IrExpression::Index(left, index) => {
                let (left, index) = (self.eval_expression(left, position)?, self.eval_expression(index, position)?);

                match (left, index) {
                    (LiteralValue::Array(array), LiteralValue::Number(index)) => {
                        let index = index as usize;

                        match array.get(index) {
                            Some(value) => self.eval_expression(value, position),
                            None => Err(RuntimeError::new(RuntimeErrorKind::IndexOutOfBounds(index), *position)),
                        }
                    }
                    (left, _) => Err(RuntimeError::new(RuntimeErrorKind::NotAnArray(left.to_string()), *position)),
                }
            }
            IrExpression::Prefix(operator, right) => {
                let right = self.eval_expression(right, position)?;

                match (operator, right) {
                    (TokenKind::Minus, LiteralValue::Number(right)) => Ok(LiteralValue::Number(-right)),
                    (TokenKind::Bang, LiteralValue::Boolean(right)) => Ok(LiteralValue::Boolean(!right)),
                    (operator, _) => Err(RuntimeError::new(RuntimeErrorKind::InvalidOperator(operator.to_string()), *position)),
                }
            }
            IrExpression::Infix(operator, left, right) => {
                let (left, right) = (self.eval_expression(left, position)?, self.eval_expression(right, position)?);

                match (left, right) {
                    (LiteralValue::Number(left), LiteralValue::Number(right)) => match operator {
                        TokenKind::Plus => Ok(LiteralValue::Number(left + right)),
                        TokenKind::Minus => Ok(LiteralValue::Number(left - right)),
                        TokenKind::Asterisk => Ok(LiteralValue::Number(left * right)),
                        TokenKind::Slash => Ok(LiteralValue::Number(left / right)),
                        TokenKind::EQ => Ok(LiteralValue::Boolean(left == right)),
                        TokenKind::NEQ => Ok(LiteralValue::Boolean(left != right)),
                        TokenKind::LT => Ok(LiteralValue::Boolean(left < right)),
                        TokenKind::LTE => Ok(LiteralValue::Boolean(left <= right)),
                        TokenKind::GT => Ok(LiteralValue::Boolean(left > right)),
                        TokenKind::GTE => Ok(LiteralValue::Boolean(left >= right)),
                        _ => Err(RuntimeError::new(RuntimeErrorKind::InvalidOperator(operator.to_string()), *position)),
                    },
                    (LiteralValue::String(left), LiteralValue::String(right)) => match operator {
                        TokenKind::Plus => Ok(LiteralValue::String(format!("{}{}", left, right))),
                        TokenKind::EQ => Ok(LiteralValue::Boolean(left == right)),
                        TokenKind::NEQ => Ok(LiteralValue::Boolean(left != right)),
                        TokenKind::LT => Ok(LiteralValue::Boolean(left < right)),
                        TokenKind::LTE => Ok(LiteralValue::Boolean(left <= right)),
                        TokenKind::GT => Ok(LiteralValue::Boolean(left > right)),
                        TokenKind::GTE => Ok(LiteralValue::Boolean(left >= right)),
                        _ => Err(RuntimeError::new(RuntimeErrorKind::InvalidOperator(operator.to_string()), *position)),
                    },
                    (LiteralValue::Boolean(left), LiteralValue::Boolean(right)) => match operator {
                        TokenKind::EQ => Ok(LiteralValue::Boolean(left == right)),
                        TokenKind::NEQ => Ok(LiteralValue::Boolean(left != right)),
                        _ => Err(RuntimeError::new(RuntimeErrorKind::InvalidOperator(operator.to_string()), *position)),
                    },
                    (left, right) => Err(RuntimeError::new(
                        RuntimeErrorKind::InvalidOperands(left.to_string(), right.to_string(), operator.to_string()),
                        *position,
                    )),
                }
            }
        }
    }
}
