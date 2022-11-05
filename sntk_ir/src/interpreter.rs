use sntk_core::tokenizer::token::Tokens;

use crate::{
    builtin::get_builtin_function,
    environment::IrEnvironment,
    instruction::{Identifier, Instruction, InstructionType, IrExpression, LiteralValue},
};

#[derive(Debug, Clone, PartialEq)]
pub struct IrInterpreter {
    pub instructions: Vec<Instruction>,
    pub environment: IrEnvironment,
}

pub trait InterpreterTrait {
    fn new(instructions: Vec<Instruction>) -> Self;
    fn new_with_environment(instructions: Vec<Instruction>, environment: IrEnvironment) -> Self;
    fn run(&mut self);
}

pub trait InstructionHandler {
    fn interpret_store_name(&mut self, identifier: &Identifier, literal: &IrExpression);
    fn to_expression(&mut self, expression: &IrExpression) -> LiteralValue;
}

impl InterpreterTrait for IrInterpreter {
    fn new(instructions: Vec<Instruction>) -> Self {
        Self {
            instructions,
            environment: IrEnvironment::new(None),
        }
    }

    fn new_with_environment(instructions: Vec<Instruction>, environment: IrEnvironment) -> Self {
        Self { instructions, environment }
    }

    fn run(&mut self) {
        for instruction in self.clone().instructions.iter() {
            match &instruction.instruction {
                InstructionType::StoreName(identifier, literal) => self.interpret_store_name(identifier, literal),
                InstructionType::Expression(expression) => _ = self.to_expression(expression),
                InstructionType::Return(_) => {}
            }
        }
    }
}

impl InstructionHandler for IrInterpreter {
    fn interpret_store_name(&mut self, identifier: &Identifier, literal: &IrExpression) {
        let literal = self.to_expression(literal);
        self.environment.set(identifier.clone(), literal);
    }

    fn to_expression(&mut self, expression: &IrExpression) -> LiteralValue {
        match expression {
            IrExpression::Identifier(identifier) => match self.environment.get(&identifier.clone()) {
                Some(literal) => literal,
                None => panic!("Undefined identifier: {}", identifier),
            },
            IrExpression::Literal(literal) => literal.clone(),
            IrExpression::Block(block) => {
                let mut interpreter = IrInterpreter::new_with_environment(block.clone(), IrEnvironment::new(Some(self.environment.clone())));

                interpreter.run();

                if let InstructionType::Return(literal) = interpreter.instructions.last().unwrap().instruction.clone() {
                    interpreter.to_expression(&literal)
                } else {
                    LiteralValue::Boolean(true)
                }
            }
            IrExpression::If(condition, consequence, alternative) => {
                let condition = self.to_expression(condition);

                if let LiteralValue::Boolean(condition) = condition {
                    if condition {
                        self.to_expression(consequence)
                    } else {
                        self.to_expression(&alternative.clone().unwrap())
                    }
                } else {
                    unreachable!()
                }
            }
            IrExpression::Call(identifier, arguments) => {
                let arguments = arguments
                    .iter()
                    .map(|argument| self.to_expression(argument))
                    .collect::<Vec<LiteralValue>>();

                match self.environment.get(&identifier.clone()) {
                    Some(LiteralValue::Function(parameters, body)) => {
                        let mut environment = IrEnvironment::new(Some(self.environment.clone()));

                        for (parameter, argument) in parameters.iter().zip(arguments.iter()) {
                            environment.set(parameter.clone(), argument.clone());
                        }

                        let mut interpreter = IrInterpreter::new_with_environment(body, environment);

                        interpreter.run();

                        if let InstructionType::Return(literal) = interpreter.instructions.last().unwrap().instruction.clone() {
                            interpreter.to_expression(&literal)
                        } else {
                            LiteralValue::Boolean(true)
                        }
                    }
                    Some(_) => panic!("Cannot call non-function"),
                    None => match get_builtin_function(identifier) {
                        Some(function) => function(arguments),
                        None => panic!("Undefined identifier: {}", identifier),
                    },
                }
            }
            IrExpression::Index(left, index) => {
                let left = self.to_expression(left);
                let index = self.to_expression(index);

                match (left, index) {
                    (LiteralValue::Array(array), LiteralValue::Number(index)) => match array.get(index as usize) {
                        Some(literal) => self.to_expression(literal),
                        None => panic!("Index out of bounds"),
                    },
                    _ => unreachable!(),
                }
            }
            IrExpression::Prefix(operator, right) => {
                let right = self.to_expression(right);

                match (operator, right) {
                    (Tokens::Bang, LiteralValue::Boolean(right)) => LiteralValue::Boolean(!right),
                    (Tokens::Minus, LiteralValue::Number(right)) => LiteralValue::Number(-right),
                    (operator, right) => unreachable!("Invalid prefix operator: {} {}", operator, right),
                }
            }
            IrExpression::Infix(left, operator, right) => {
                let left = self.to_expression(left);
                let right = self.to_expression(right);

                match (left.clone(), operator, right.clone()) {
                    (LiteralValue::Number(left), Tokens::Plus, LiteralValue::Number(right)) => LiteralValue::Number(left + right),
                    (LiteralValue::Number(left), Tokens::Minus, LiteralValue::Number(right)) => LiteralValue::Number(left - right),
                    (LiteralValue::Number(left), Tokens::Asterisk, LiteralValue::Number(right)) => LiteralValue::Number(left * right),
                    (LiteralValue::Number(left), Tokens::Slash, LiteralValue::Number(right)) => LiteralValue::Number(left / right),
                    (LiteralValue::Number(left), Tokens::Percent, LiteralValue::Number(right)) => LiteralValue::Number(left % right),
                    (LiteralValue::Number(left), Tokens::LT, LiteralValue::Number(right)) => LiteralValue::Boolean(left < right),
                    (LiteralValue::Number(left), Tokens::GT, LiteralValue::Number(right)) => LiteralValue::Boolean(left > right),
                    (LiteralValue::Number(left), Tokens::LTE, LiteralValue::Number(right)) => LiteralValue::Boolean(left < right),
                    (LiteralValue::Number(left), Tokens::GTE, LiteralValue::Number(right)) => LiteralValue::Boolean(left > right),
                    (_, Tokens::EQ, _) => LiteralValue::Boolean(left == right),
                    (_, Tokens::NEQ, _) => LiteralValue::Boolean(left != right),
                    (left, operator, right) => unreachable!("Invalid infix operator: {} {} {}", left, operator, right),
                }
            }
        }
    }
}
