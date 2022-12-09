use sntk_core::{parser::ast::Position, tokenizer::token::Tokens};
use sntk_proc::with_position;

use crate::{
    instruction::{Identifier, Instruction, InstructionType, IrExpression, LiteralValue},
    runtime_error, INVALID_OPERANDS, INVALID_OPERATOR, NOT_A_FUNCTION, UNDEFINED_VARIABLE,
};
use std::collections::HashMap;

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

        Ok(match instruction.instruction.clone() {
            InstructionType::StoreName(name, expression) => {
                let expression = self.eval_expression(&expression, &position)?;

                self.environment.set(&name, &expression);
            }
            InstructionType::Expression(expression) => {
                self.eval_expression(&expression, &position)?;
            }
            #[allow(unused_variables)]
            InstructionType::Return(expression) => {
                println!("{:?}", self.eval_expression(&expression, &position)?);
                // ^ for debugging. TODO: remove this.

                ()
            }
        })
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
    #[with_position]
    fn eval_expression(&mut self, expression: &IrExpression) -> RuntimeError<LiteralValue> {
        match expression {
            IrExpression::Identifier(name) => match self.environment.get(name) {
                Some(value) => Ok(value),
                None => Err(runtime_error! { UNDEFINED_VARIABLE; name; &position }),
            },
            IrExpression::Literal(value) => Ok(value.clone()),
            IrExpression::Block(block) => {
                let mut interpreter = IrInterpreter::new_with_environment(block.clone(), &IrEnvironment::new(Some(&self.environment)));
                interpreter.eval()?;
                Ok(interpreter.last()?)
            }
            IrExpression::If(..) => unimplemented!(),
            IrExpression::Call(function, arguments) => {
                let (parameters, body) = match self.eval_expression(function, &position)? {
                    LiteralValue::Function(parameters, block, _) => {
                        (parameters.iter().map(|parameter| parameter.0.clone()).collect::<Vec<_>>(), block)
                    }
                    value => return Err(runtime_error! { NOT_A_FUNCTION; value; &position }),
                };

                let arguments = arguments
                    .iter()
                    .map(|argument| self.eval_expression(argument, &position))
                    .collect::<Result<Vec<_>, _>>()?;

                let mut environment = IrEnvironment::new(Some(&self.environment));

                for (parameter, argument) in parameters.iter().zip(arguments.iter()) {
                    environment.set(parameter, argument);
                }

                let mut interpreter = IrInterpreter::new_with_environment(body.clone(), &environment);
                interpreter.eval()?;

                Ok(interpreter.last()?)
            }
            IrExpression::Index(..) => unimplemented!(),
            IrExpression::Prefix(..) => unimplemented!(),
            IrExpression::Infix(left, operator, right) => {
                let (left, right) = (self.eval_expression(left, &position)?, self.eval_expression(right, &position)?);

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
                        _ => Err(runtime_error! { INVALID_OPERATOR; operator; &position }),
                    },
                    (LiteralValue::String(left), LiteralValue::String(right)) => match operator {
                        Tokens::Plus => Ok(LiteralValue::String(format!("{}{}", left, right))),
                        Tokens::EQ => Ok(LiteralValue::Boolean(left == right)),
                        Tokens::NEQ => Ok(LiteralValue::Boolean(left != right)),
                        Tokens::LT => Ok(LiteralValue::Boolean(left < right)),
                        Tokens::LTE => Ok(LiteralValue::Boolean(left <= right)),
                        Tokens::GT => Ok(LiteralValue::Boolean(left > right)),
                        Tokens::GTE => Ok(LiteralValue::Boolean(left >= right)),
                        _ => Err(runtime_error! { INVALID_OPERATOR; operator; &position }),
                    },
                    (LiteralValue::Boolean(left), LiteralValue::Boolean(right)) => match operator {
                        Tokens::EQ => Ok(LiteralValue::Boolean(left == right)),
                        Tokens::NEQ => Ok(LiteralValue::Boolean(left != right)),
                        _ => Err(runtime_error! { INVALID_OPERATOR; operator; &position }),
                    },
                    (left, right) => Err(runtime_error! { INVALID_OPERANDS; left, right, operator; &position }),
                }
            }
        }
    }
}
