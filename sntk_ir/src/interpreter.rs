use crate::{
    builtin::get_builtin_function,
    instruction::{Identifier, Instruction, InstructionType, IrExpression, LiteralValue},
    runtime_error, INDEX_OUT_OF_BOUNDS, INVALID_OPERANDS, INVALID_OPERATOR, NOT_A_ARRAY, NOT_A_FUNCTION, UNDEFINED_VARIABLE,
};
use sntk_core::{parser::ast::Position, tokenizer::token::Tokens};
use std::{borrow::Cow, collections::HashMap};

#[derive(Debug, Clone, PartialEq)]
pub struct IrEnvironment {
    pub values: HashMap<Identifier, LiteralValue>,
    pub parent: Option<Box<IrEnvironment>>,
}

impl IrEnvironment {
    #[inline]
    pub fn new(parent: Option<&IrEnvironment>) -> Self {
        Self {
            values: HashMap::new(),
            parent: parent.map(|parent| Box::new(parent.clone())),
        }
    }

    pub fn get(&self, name: &Identifier) -> Option<LiteralValue> {
        match self.values.get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.get(name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: &Identifier, value: &LiteralValue) {
        self.values.insert(name.to_string(), value.clone());
    }
}

#[derive(Debug, Clone)]
pub struct IrInterpreter {
    pub instructions: Vec<Instruction>,
    pub environment: IrEnvironment,
}

pub type RuntimeError<T> = Result<T, crate::RuntimeError>;

pub trait IrInterpreterBase {
    fn new(instructions: Vec<Instruction>) -> Self;
    fn new_with_environment(instructions: Vec<Instruction>, environment: &IrEnvironment) -> Self;
    fn eval(&mut self) -> RuntimeError<()>;
    fn last(&mut self) -> RuntimeError<LiteralValue>;
}

pub trait IrInterpreterTrait {
    fn eval_instruction(&mut self, instruction: &Instruction) -> RuntimeError<()>;
    fn eval_expression(&mut self, expression: &IrExpression, position: &Position) -> RuntimeError<LiteralValue>;
}

impl IrInterpreterBase for IrInterpreter {
    fn new(instructions: Vec<Instruction>) -> Self {
        Self {
            instructions,
            environment: IrEnvironment::new(None),
        }
    }

    fn new_with_environment(instructions: Vec<Instruction>, environment: &IrEnvironment) -> Self {
        Self {
            instructions,
            environment: environment.clone(),
        }
    }

    fn eval(&mut self) -> RuntimeError<()> {
        for instruction in self.instructions.clone().iter() {
            self.eval_instruction(instruction)?;
        }

        Ok(())
    }

    fn last(&mut self) -> RuntimeError<LiteralValue> {
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
}

impl IrInterpreterTrait for IrInterpreter {
    fn eval_instruction(&mut self, instruction: &Instruction) -> RuntimeError<()> {
        let position = Position(instruction.position.0, instruction.position.1);

        match instruction.instruction.clone() {
            InstructionType::StoreName(name, expression) => {
                let expression = self.eval_expression(&expression, &position)?;

                self.environment.set(&name, &expression);
            }
            InstructionType::Expression(expression) => {
                self.eval_expression(&expression, &position)?;
            }
            #[allow(unused_variables)]
            InstructionType::Return(expression) => {
                // println!("{:?}", self.eval_expression(&expression, position)?);
                // ^ for debugging. TODO: remove this.
            }
        }

        Ok(())
    }

    /**
        Identifier(Identifier),                                              /* identifier */        Literal(LiteralValue),                                               /* literal */
        Block(Block),                                                        /* block */
        If(Box<IrExpression>, Box<IrExpression>, Box<Option<IrExpression>>), /* condition, consequence, alternative */
        Call(Box<IrExpression>, Vec<IrExpression>),                          /* function, arguments */
        Index(Box<IrExpression>, Box<IrExpression>),                         /* left, index */
        Prefix(Tokens, Box<IrExpression>),                                   /* operator, right */
        Infix(Box<IrExpression>, Tokens, Box<IrExpression>),                 /* left, operator, right */
    */
    fn eval_expression(&mut self, expression: &IrExpression, position: &Position) -> RuntimeError<LiteralValue> {
        match expression {
            IrExpression::Identifier(name) => match self.environment.get(name) {
                Some(value) => Ok(value),
                None => Err(runtime_error(UNDEFINED_VARIABLE, Cow::Borrowed(&[name]), position)),
            },
            IrExpression::Literal(value) => Ok(value.clone()),
            IrExpression::Block(block) => {
                let mut interpreter = IrInterpreter::new_with_environment(block.clone(), &IrEnvironment::new(Some(&self.environment)));
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
                    .collect::<Result<Vec<_>, _>>()?;

                let function = match *function.clone() {
                    IrExpression::Identifier(name) => match self.environment.get(&name) {
                        Some(value) => value,
                        None => {
                            return match get_builtin_function(&name) {
                                Some(function) => Ok(function(arguments.iter().collect())),
                                None => Err(runtime_error(UNDEFINED_VARIABLE, Cow::Borrowed(&[&name]), position)),
                            };
                        }
                    },
                    _ => self.eval_expression(function, position)?,
                };

                let (parameters, body) = match function {
                    LiteralValue::Function(parameters, block, _) => {
                        (parameters.iter().map(|parameter| parameter.0.clone()).collect::<Vec<_>>(), block)
                    }
                    value => return Err(runtime_error(NOT_A_FUNCTION, Cow::Borrowed(&[&value.to_string()]), position)),
                };

                let mut environment = IrEnvironment::new(Some(&self.environment));

                for (parameter, argument) in parameters.iter().zip(arguments.iter()) {
                    environment.set(parameter, argument);
                }

                let mut interpreter = IrInterpreter::new_with_environment(body, &environment);
                interpreter.eval()?;

                Ok(interpreter.last()?)
            }
            IrExpression::Index(left, index) => {
                let (left, index) = (self.eval_expression(left, position)?, self.eval_expression(index, position)?);

                match (left, index) {
                    (LiteralValue::Array(array), LiteralValue::Number(index)) => {
                        let index = index as usize;

                        match array.get(index) {
                            Some(value) => self.eval_expression(value, position),
                            None => Err(runtime_error(INDEX_OUT_OF_BOUNDS, Cow::Borrowed(&[&index.to_string()]), position)),
                        }
                    }
                    (left, _) => Err(runtime_error(NOT_A_ARRAY, Cow::Borrowed(&[&left.to_string()]), position)),
                }
            }
            IrExpression::Prefix(operator, right) => {
                let right = self.eval_expression(right, position)?;

                match (operator, right) {
                    (Tokens::Minus, LiteralValue::Number(right)) => Ok(LiteralValue::Number(-right)),
                    (Tokens::Bang, LiteralValue::Boolean(right)) => Ok(LiteralValue::Boolean(!right)),
                    (operator, _) => Err(runtime_error(INVALID_OPERATOR, Cow::Borrowed(&[&operator.to_string()]), position)),
                }
            }
            IrExpression::Infix(left, operator, right) => {
                let (left, right) = (self.eval_expression(left, position)?, self.eval_expression(right, position)?);

                match (left, right) {
                    (LiteralValue::Number(left), LiteralValue::Number(right)) => match operator {
                        Tokens::Plus => Ok(LiteralValue::Number(left + right)),
                        Tokens::Minus => Ok(LiteralValue::Number(left - right)),
                        Tokens::Asterisk => Ok(LiteralValue::Number(left * right)),
                        Tokens::Slash => Ok(LiteralValue::Number(left / right)),
                        Tokens::EQ => Ok(LiteralValue::Boolean(left == right)),
                        Tokens::NEQ => Ok(LiteralValue::Boolean(left != right)),
                        Tokens::LT => Ok(LiteralValue::Boolean(left < right)),
                        Tokens::LTE => Ok(LiteralValue::Boolean(left <= right)),
                        Tokens::GT => Ok(LiteralValue::Boolean(left > right)),
                        Tokens::GTE => Ok(LiteralValue::Boolean(left >= right)),
                        _ => Err(runtime_error(INVALID_OPERATOR, Cow::Borrowed(&[&operator.to_string()]), position)),
                    },
                    (LiteralValue::String(left), LiteralValue::String(right)) => match operator {
                        Tokens::Plus => Ok(LiteralValue::String(format!("{}{}", left, right))),
                        Tokens::EQ => Ok(LiteralValue::Boolean(left == right)),
                        Tokens::NEQ => Ok(LiteralValue::Boolean(left != right)),
                        Tokens::LT => Ok(LiteralValue::Boolean(left < right)),
                        Tokens::LTE => Ok(LiteralValue::Boolean(left <= right)),
                        Tokens::GT => Ok(LiteralValue::Boolean(left > right)),
                        Tokens::GTE => Ok(LiteralValue::Boolean(left >= right)),
                        _ => Err(runtime_error(INVALID_OPERATOR, Cow::Borrowed(&[&operator.to_string()]), position)),
                    },
                    (LiteralValue::Boolean(left), LiteralValue::Boolean(right)) => match operator {
                        Tokens::EQ => Ok(LiteralValue::Boolean(left == right)),
                        Tokens::NEQ => Ok(LiteralValue::Boolean(left != right)),
                        _ => Err(runtime_error(INVALID_OPERATOR, Cow::Borrowed(&[&operator.to_string()]), position)),
                    },
                    (left, right) => Err(runtime_error(
                        INVALID_OPERANDS,
                        Cow::Borrowed(&[&left.to_string(), &right.to_string(), &operator.to_string()]),
                        position,
                    )),
                }
            }
        }
    }
}
