use sntk_core::parser::ast::Position;
use sntk_proc::with_position;

use crate::instruction::{Identifier, Instruction, InstructionType, IrExpression, LiteralValue};
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
    fn eval(&mut self) -> RuntimeError<()>;
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

    fn eval(&mut self) -> RuntimeError<()> {
        for instruction in self.instructions.clone().iter() {
            self.eval_instruction(instruction)?;
        }

        Ok(())
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
            _ => todo!(),
        })
    }

    #[with_position]
    fn eval_expression(&mut self, expression: &IrExpression) -> RuntimeError<LiteralValue> {
        match expression {
            _ => todo!(),
        }
    }
}
