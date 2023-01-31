use crate::{
    checker::{custom_data_type, Checker, CustomTypes, DeclaredTypes},
    CompileError, TypeError, TypeErrorKind,
};
use sntk_core::parser::{
    ArrayLiteral, AutoStatement, BlockExpression, BooleanLiteral, CallExpression, DataType, DataTypeKind, DeclareStatement, Expression,
    ExpressionStatement, FunctionLiteral, Identifier, IfExpression, IndexExpression, InfixExpression, LetStatement, NumberLiteral, Parameter,
    ParameterKind, PrefixExpression, Program, ReturnStatement, Statement, StringLiteral, TypeStatement, TypeofExpression,
};
use sntk_ir::instruction::{Instruction, InstructionType, IrExpression, LiteralValue};

#[derive(Debug)]
pub struct Compiler {
    pub program: Program,
    pub declares: DeclaredTypes,
    pub customs: CustomTypes,
}

pub type CompileResult<T> = Result<T, CompileError>;

impl Compiler {
    #[inline]
    pub fn new(program: Program) -> Self {
        Self {
            program,
            declares: DeclaredTypes::new(None),
            customs: CustomTypes::new(None),
        }
    }

    #[inline]
    pub fn new_with(program: Program, declares: DeclaredTypes, customs: CustomTypes) -> Self {
        Self { program, declares, customs }
    }

    pub fn compile_program(&mut self) -> CompileResult<Vec<Instruction>> {
        let mut instructions = Vec::new();

        if !self.program.errors.is_empty() {
            return Err(CompileError::ParsingError(self.program.errors.clone()));
        }

        for statement in self.program.statements.clone().iter() {
            instructions.push(self.compile_statement(statement)?);
        }

        Ok(instructions)
    }

    pub fn compile_statement(&mut self, statement: &Statement) -> CompileResult<Instruction> {
        Ok(match statement {
            Statement::LetStatement(LetStatement {
                name,
                value,
                position,
                data_type,
            }) => {
                let value = self.compile_expression(value)?;
                let value_type = Checker::new(Some(data_type), &self.declares, &self.customs, *position)?.get_type_from_ir_expression(&value)?;

                if data_type != &value_type {
                    return Err(TypeError::new(
                        TypeErrorKind::ExpectedDataType(data_type.to_string(), value_type.to_string()),
                        Some("check the type of the value you are trying to assign to the variable"),
                        *position,
                    ));
                }

                self.declares.set(name.value.clone(), data_type.clone());

                Instruction::new(InstructionType::StoreName(name.value.clone(), value), *position)
            }
            Statement::AutoStatement(AutoStatement { name, value, position }) => {
                let value = self.compile_expression(value)?;

                self.declares.set(
                    name.value.clone(),
                    Checker::new(None, &self.declares, &self.customs, *position)?.get_type_from_ir_expression(&value)?,
                );

                Instruction::new(InstructionType::StoreName(name.value.clone(), value), *position)
            }
            Statement::ReturnStatement(ReturnStatement { value, position }) => {
                Instruction::new(InstructionType::Return(self.compile_expression(value)?), *position)
            }
            Statement::TypeStatement(TypeStatement {
                name, data_type, position, ..
            }) => {
                self.customs.set(name.value.clone(), data_type.clone());

                Instruction::new(InstructionType::None, *position)
            }
            Statement::StructStatement(_) => unimplemented!(),
            Statement::DeclareStatement(DeclareStatement { name, data_type, position }) => {
                self.declares.set(name.value.clone(), data_type.clone());

                Instruction::new(InstructionType::None, *position)
            }
            Statement::ExpressionStatement(ExpressionStatement { expression, position }) => {
                Instruction::new(InstructionType::Expression(self.compile_expression(expression)?), *position)
            }
        })
    }

    pub fn compile_expression(&mut self, expression: &Expression) -> CompileResult<IrExpression> {
        let expression = match expression {
            Expression::Identifier(Identifier { value, .. }) => IrExpression::Identifier(value.clone()),
            Expression::BlockExpression(BlockExpression { statements, .. }) => {
                let mut instructions = Vec::new();

                for statement in statements.iter() {
                    instructions.push(self.compile_statement(statement)?);

                    if let Statement::ReturnStatement(_) = statement {
                        break;
                    }
                }

                IrExpression::Block(instructions)
            }
            Expression::PrefixExpression(PrefixExpression { operator, right, .. }) => {
                IrExpression::Prefix(operator.clone(), Box::new(self.compile_expression(right)?))
            }
            Expression::InfixExpression(InfixExpression { operator, left, right, .. }) => IrExpression::Infix(
                operator.clone(),
                Box::new(self.compile_expression(left)?),
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
            Expression::CallExpression(CallExpression {
                function,
                arguments,
                position,
            }) => {
                let mut compiled_arguments = Vec::new();
                let function = self.compile_expression(function)?;
                let function_type = match Checker::new(None, &self.declares, &self.customs, *position)?
                    .get_type_from_ir_expression(&function)?
                    .data_type
                {
                    DataTypeKind::Fn(function_type) => function_type,
                    _ => unreachable!(),
                };

                for (index, (argument, (_, kind))) in arguments.iter().zip(function_type.parameters.iter()).enumerate() {
                    if let ParameterKind::Spread = kind {
                        compiled_arguments.push(self.compile_expression(&Expression::ArrayLiteral(ArrayLiteral {
                            elements: arguments[index..].to_vec(),
                            position: *position,
                        }))?);

                        break;
                    }

                    compiled_arguments.push(self.compile_expression(argument)?);
                }

                IrExpression::Call(Box::new(function), compiled_arguments)
            }
            Expression::TypeofExpression(TypeofExpression { expression, position }) => {
                let expression = self.compile_expression(expression)?;

                IrExpression::Literal(LiteralValue::String(
                    Checker::new(None, &self.declares, &self.customs, *position)?
                        .get_type_from_ir_expression(&expression)?
                        .to_string(),
                ))
            }
            Expression::IndexExpression(IndexExpression { left, index, .. }) => {
                IrExpression::Index(Box::new(self.compile_expression(left)?), Box::new(self.compile_expression(index)?))
            }
            _ => self.compile_literal(expression)?,
        };

        Ok(expression)
    }

    fn compile_literal(&mut self, expression: &Expression) -> CompileResult<IrExpression> {
        let expression = match expression {
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
            Expression::FunctionLiteral(FunctionLiteral {
                parameters,
                body,
                return_type,
                ..
            }) => {
                let mut new_parameters = Vec::new();

                for (
                    index,
                    parameter @ Parameter {
                        name,
                        data_type,
                        kind,
                        position,
                    },
                ) in parameters.iter().enumerate()
                {
                    let data_type = custom_data_type(data_type, &self.customs)?;

                    match kind {
                        ParameterKind::Normal => {
                            self.declares.set(name.value.clone(), data_type);
                            new_parameters.push(parameter.clone());
                        }
                        ParameterKind::Spread => {
                            if index != parameters.len() - 1 {
                                return Err(TypeError::new(
                                    TypeErrorKind::SpreadParameterMustBeLast,
                                    Some("Spread parameter must be last".to_string()),
                                    *position,
                                ));
                            }

                            self.declares.set(
                                name.value.clone(),
                                DataType::new(DataTypeKind::Array(Box::new(data_type.clone())), *position),
                            );

                            new_parameters.push(Parameter {
                                name: name.clone(),
                                data_type,
                                kind: kind.clone(),
                                position: *position,
                            });

                            break;
                        }
                    };
                }
                IrExpression::Literal(LiteralValue::Function(
                    new_parameters,
                    match self.compile_expression(&Expression::BlockExpression(body.clone()))? {
                        IrExpression::Block(instructions) => instructions,
                        _ => unreachable!(),
                    },
                    custom_data_type(return_type, &self.customs).map(|data_type| data_type.data_type)?,
                    None,
                ))
            }
            Expression::StructLiteral(_) => todo!(),
            _ => unreachable!(),
        };

        Ok(expression)
    }
}
