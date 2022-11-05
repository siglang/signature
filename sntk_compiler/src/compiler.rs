use crate::{error::CompileError, helpers::ast_position_to_tuple};
use sntk_core::parser::ast::{
    ArrayLiteral, AutoStatement, BlockExpression, BooleanLiteral, CallExpression, Expression, FunctionLiteral, Identifier, IfExpression,
    IndexExpression, InfixExpression, LetStatement, NumberLiteral, PrefixExpression, Program, ReturnStatement, Statement, StringLiteral,
};
use sntk_ir::instruction::{Instruction, InstructionType, IrExpression, LiteralValue};

#[derive(Debug)]
pub struct Compiler {
    pub program: Program,
}

pub type CompileResult<T> = Result<T, CompileError>;

pub trait CompilerTrait {
    fn new(program: Program) -> Self;
    fn compile_program(&self) -> CompileResult<Vec<Instruction>>;
    fn compile_statement(&self, statement: &Statement) -> CompileResult<Instruction>;
    fn compile_expression(&self, expression: &Expression) -> CompileResult<IrExpression>;
}

impl CompilerTrait for Compiler {
    fn new(program: Program) -> Self {
        Self { program }
    }

    fn compile_program(&self) -> CompileResult<Vec<Instruction>> {
        let mut instructions = Vec::new();

        if !self.program.errors.is_empty() {
            return Err(CompileError::ParsingError(self.program.errors.clone()));
        }

        for statement in self.program.statements.iter() {
            instructions.push(self.compile_statement(statement)?);
        }

        Ok(instructions)
    }

    fn compile_statement(&self, statement: &Statement) -> CompileResult<Instruction> {
        Ok(match statement {
            Statement::LetStatement(LetStatement { name, value, position, .. }) => Instruction::new(
                InstructionType::StoreName(name.value.clone(), self.compile_expression(value)?),
                ast_position_to_tuple(position),
            ),
            Statement::AutoStatement(AutoStatement { name, value, position }) => Instruction::new(
                InstructionType::StoreName(name.value.clone(), self.compile_expression(value)?),
                ast_position_to_tuple(position),
            ),
            Statement::ReturnStatement(ReturnStatement { value, position }) => {
                Instruction::new(InstructionType::Return(self.compile_expression(value)?), ast_position_to_tuple(position))
            }
            Statement::TypeStatement(_) | Statement::StructStatement(_) => unimplemented!(),
            Statement::ExpressionStatement(expression) => Instruction::new(
                InstructionType::Expression(self.compile_expression(&expression.expression)?),
                ast_position_to_tuple(&expression.position),
            ),
        })
    }

    fn compile_expression(&self, expression: &Expression) -> CompileResult<IrExpression> {
        Ok(match expression {
            Expression::Identifier(Identifier { value, .. }) => IrExpression::Identifier(value.clone()),
            Expression::BlockExpression(BlockExpression { statements, .. }) => {
                let mut instructions = Vec::new();

                for statement in statements.iter() {
                    instructions.push(self.compile_statement(statement)?);
                }

                IrExpression::Block(instructions)
            }
            Expression::PrefixExpression(PrefixExpression { operator, right, .. }) => {
                IrExpression::Prefix(operator.clone(), Box::new(self.compile_expression(right)?))
            }
            Expression::InfixExpression(InfixExpression { operator, left, right, .. }) => IrExpression::Infix(
                Box::new(self.compile_expression(left)?),
                operator.clone(),
                Box::new(self.compile_expression(right)?),
            ),
            Expression::IfExpression(IfExpression {
                condition,
                consequence,
                alternative,
                ..
            }) => IrExpression::If(
                Box::new(self.compile_expression(condition)?),
                Box::new(self.compile_expression(&Expression::BlockExpression(*consequence.clone()))?),
                Box::new(
                    alternative
                        .clone()
                        .map(|alternative| self.compile_expression(&Expression::BlockExpression(*alternative)))
                        .transpose()?,
                ),
            ),
            Expression::FunctionLiteral(FunctionLiteral { parameters, body, .. }) => {
                let mut instructions = Vec::new();

                for statement in body.statements.iter() {
                    instructions.push(self.compile_statement(statement)?);
                }

                IrExpression::Literal(LiteralValue::Function(
                    parameters.clone().iter().map(|x| x.0.value.clone()).collect(),
                    instructions,
                ))
            }
            Expression::CallExpression(CallExpression { function, arguments, .. }) => {
                let mut arguments_compiled = Vec::new();

                for argument in arguments.iter() {
                    arguments_compiled.push(self.compile_expression(argument)?);
                }

                IrExpression::Call(Box::new(self.compile_expression(function)?), arguments_compiled)
            }
            Expression::TypeofExpression(_) => todo!(),
            Expression::IndexExpression(IndexExpression { left, index, .. }) => {
                IrExpression::Index(Box::new(self.compile_expression(left)?), Box::new(self.compile_expression(index)?))
            }
            Expression::StringLiteral(StringLiteral { value, .. }) => IrExpression::Literal(LiteralValue::String(value.clone())),
            Expression::NumberLiteral(NumberLiteral { value, .. }) => IrExpression::Literal(LiteralValue::Number(*value)),
            Expression::BooleanLiteral(BooleanLiteral { value, .. }) => IrExpression::Literal(LiteralValue::Boolean(*value)),
            Expression::ArrayLiteral(ArrayLiteral { elements, .. }) => {
                let mut elements_compiled = Vec::new();

                for element in elements.iter() {
                    elements_compiled.push(self.compile_expression(element)?);
                }

                IrExpression::Literal(LiteralValue::Array(elements_compiled))
            }
            Expression::StructLiteral(_) => todo!(),
        })
    }
}
