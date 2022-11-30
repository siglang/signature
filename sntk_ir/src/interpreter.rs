use sntk_core::{parser::ast::Position, tokenizer::token::Tokens};
use sntk_proc::with_position;

use crate::{
    builtin::get_builtin_function,
    environment::IrEnvironment,
    instruction::{Block, Identifier, Instruction, InstructionType, IrExpression, LiteralValue},
    runtime_error, RuntimeError, CANNOT_CALL_NON_FUNCTION, INDEX_OUT_OF_BOUNDS, UNDEFINED_IDENTIFIER,
};

#[derive(Debug, Clone, PartialEq)]
pub struct IrInterpreter {
    pub instructions: Vec<Instruction>,
    pub environment: IrEnvironment,
}

pub type IrInterpreterResult<T> = Result<T, RuntimeError>;

pub trait InterpreterTrait {
    fn new(instructions: Vec<Instruction>) -> Self;
    fn new_with_environment(instructions: Vec<Instruction>, environment: IrEnvironment) -> Self;
    fn run(&mut self) -> IrInterpreterResult<()>;
}

pub trait InstructionHandler {
    fn interpret_store_name(&mut self, identifier: &Identifier, literal: &IrExpression, position: &Position) -> IrInterpreterResult<()>;
    fn to_expression(&mut self, expression: &IrExpression, position: &Position) -> IrInterpreterResult<LiteralValue>;
}

impl InterpreterTrait for IrInterpreter {
    #[inline]
    fn new(instructions: Vec<Instruction>) -> Self {
        Self {
            instructions,
            environment: IrEnvironment::new(None),
        }
    }

    #[inline]
    fn new_with_environment(instructions: Vec<Instruction>, environment: IrEnvironment) -> Self {
        Self { instructions, environment }
    }

    fn run(&mut self) -> IrInterpreterResult<()> {
        for Instruction {
            instruction,
            position: (line, column),
        } in self.clone().instructions.iter()
        {
            let position = &Position::new(*line, *column);

            match instruction {
                InstructionType::StoreName(identifier, literal) => self.interpret_store_name(identifier, literal, position)?,
                InstructionType::Expression(expression) => _ = self.to_expression(expression, position)?,
                InstructionType::Return(_) => {}
            }
        }

        Ok(())
    }
}

impl InstructionHandler for IrInterpreter {
    #[with_position]
    fn interpret_store_name(&mut self, identifier: &Identifier, literal: &IrExpression) -> IrInterpreterResult<()> {
        let literal = self.to_expression(literal, position)?;
        self.environment.set(identifier, &literal);

        Ok(())
    }

    #[with_position]
    fn to_expression(&mut self, expression: &IrExpression) -> IrInterpreterResult<LiteralValue> {
        match expression {
            IrExpression::Identifier(identifier) => self.ir_expression_identifier(identifier, position),
            IrExpression::Block(block) => self.ir_expression_block(block, position),
            IrExpression::If(condition, consequence, alternative) => self.ir_expression_if(condition, consequence, alternative, position),
            IrExpression::Call(function, arguments) => self.ir_expression_call(function, arguments.clone(), position),
            IrExpression::Index(left, index) => self.ir_expression_index(left, index, position),
            IrExpression::Prefix(operator, right) => self.ir_expression_prefix(operator, right, position),
            IrExpression::Infix(left, operator, right) => self.ir_expression_infix(left, operator, right, position),
            IrExpression::Literal(literal) => Ok(match literal {
                LiteralValue::Array(array) => LiteralValue::Array(
                    array
                        .iter()
                        .map(|expression| self.to_expression(expression, position))
                        .collect::<IrInterpreterResult<Vec<LiteralValue>>>()?
                        .iter()
                        .map(|literal| IrExpression::Literal(literal.clone()))
                        .collect(),
                ),
                _ => literal.clone(),
            }),
        }
    }
}

impl IrInterpreter {
    #[with_position]
    fn ir_expression_identifier(&mut self, identifier: &String) -> IrInterpreterResult<LiteralValue> {
        match self.environment.get(&identifier.clone()) {
            Some(literal) => Ok(literal),
            None => Err(runtime_error! { UNDEFINED_IDENTIFIER; identifier; &position }),
        }
    }

    #[with_position]
    fn ir_expression_block(&mut self, block: &Block) -> IrInterpreterResult<LiteralValue> {
        let mut interpreter = IrInterpreter::new_with_environment(block.clone(), IrEnvironment::new(Some(self.environment.clone())));

        interpreter.run()?;

        let last = match interpreter.instructions.last() {
            Some(instruction) => instruction,
            None => return Ok(LiteralValue::Boolean(true)),
        };

        if let InstructionType::Return(literal) = last.instruction.clone() {
            interpreter.to_expression(&literal, position)
        } else {
            Ok(LiteralValue::Boolean(true))
        }
    }

    #[with_position]
    #[rustfmt::skip]
    fn ir_expression_if(&mut self, condition: &IrExpression, consequence: &IrExpression, alternative: &Option<IrExpression>) -> IrInterpreterResult<LiteralValue> {
        let condition = self.to_expression(condition, position)?;

        if let LiteralValue::Boolean(condition) = condition {
            if condition {
                self.to_expression(consequence, position)
            } else if let Some(ref alternative) = *alternative {
                self.to_expression(alternative, position)
            } else {
                Ok(LiteralValue::Boolean(true))
            }
        } else {
            unreachable!()
        }
    }

    #[with_position]
    fn ir_expression_call(&mut self, function: &IrExpression, arguments: Vec<IrExpression>) -> IrInterpreterResult<LiteralValue> {
        let arguments = arguments
            .iter()
            .map(|argument| self.to_expression(argument, position))
            .collect::<IrInterpreterResult<Vec<LiteralValue>>>()?;

        macro_rules! function_impl {
            ($parameters:expr; $body:expr) => {{
                let mut environment = IrEnvironment::new(Some(self.environment.clone()));

                for (parameter, argument) in $parameters.iter().zip(arguments.iter()) {
                    environment.set(&parameter.0, &argument);
                }

                let mut interpreter = IrInterpreter::new_with_environment($body, environment);

                interpreter.run()?;

                let last = match interpreter.instructions.last() {
                    Some(instruction) => instruction,
                    None => return Ok(LiteralValue::Boolean(true)),
                };

                if let InstructionType::Return(literal) = last.instruction.clone() {
                    interpreter.to_expression(&literal, position)
                } else {
                    Ok(LiteralValue::Boolean(true))
                }
            }};
        }

        match function.clone() {
            IrExpression::Identifier(identifier) => match self.environment.get(&identifier) {
                Some(LiteralValue::Function(parameters, body, _)) => function_impl! { parameters; body },
                Some(_) => Err(runtime_error! { CANNOT_CALL_NON_FUNCTION; identifier; &position }),
                None => match get_builtin_function(identifier.as_str()) {
                    Some(function) => Ok(function(arguments.iter().collect())),
                    None => Err(runtime_error! { UNDEFINED_IDENTIFIER; identifier; &position }),
                },
            },
            IrExpression::Literal(LiteralValue::Function(parameters, body, _)) => function_impl! { parameters; body },
            _ => unreachable!(),
        }
    }

    #[with_position]
    fn ir_expression_index(&mut self, left: &IrExpression, index: &IrExpression) -> IrInterpreterResult<LiteralValue> {
        let left = self.to_expression(left, position)?;
        let index = self.to_expression(index, position)?;

        match (left, index) {
            (LiteralValue::Array(array), LiteralValue::Number(index)) => match array.get(index as usize) {
                Some(literal) => self.to_expression(literal, position),
                None => Err(runtime_error! { INDEX_OUT_OF_BOUNDS; index; &position }),
            },
            _ => unreachable!(),
        }
    }

    #[with_position]
    fn ir_expression_prefix(&mut self, operator: &Tokens, right: &IrExpression) -> IrInterpreterResult<LiteralValue> {
        let right = self.to_expression(right, position)?;

        Ok(match (operator, right) {
            (Tokens::Bang, LiteralValue::Boolean(right)) => LiteralValue::Boolean(!right),
            (Tokens::Minus, LiteralValue::Number(right)) => LiteralValue::Number(-right),
            (operator, right) => unreachable!("Invalid prefix operator: {} {}", operator, right),
        })
    }

    #[with_position]
    fn ir_expression_infix(&mut self, left: &IrExpression, operator: &Tokens, right: &IrExpression) -> IrInterpreterResult<LiteralValue> {
        let left = self.to_expression(left, position)?;
        let right = self.to_expression(right, position)?;

        macro_rules! match_infix {
            ($($t:ident, $op:tt, $ty:ident);*) => {
                match (left.clone(), operator, right.clone()) {
                    $(
                        (LiteralValue::Number(left), Tokens::$t, LiteralValue::Number(right)) => LiteralValue::$ty(left $op right),
                    )*
                    (_, Tokens::EQ, _) => LiteralValue::Boolean(left == right),
                    (_, Tokens::NEQ, _) => LiteralValue::Boolean(left != right),
                    (left, operator, right) => unreachable!("Invalid infix operator: {} {} {}", left, operator, right),
                }
            }
        }

        Ok(match_infix! {
            Plus, +, Number;
            Minus, -, Number;
            Asterisk, *, Number;
            Slash, /, Number;
            Percent, %, Number;
            LT, <, Boolean;
            LTE, <=, Boolean;
            GT, >, Boolean;
            GTE, >=, Boolean
        })
    }
}
