use crate::{
    types::{
        IrExpression, IrExpressionStatement, IrLetStatement, IrProgram, IrReturnStatement,
        IrStatement,
    },
    CompilingError, CompilingErrorKind,
};
use parser::ast::*;

#[derive(Debug)]
pub struct Compiler(pub Program);

pub type CompileResult<T> = Result<T, Vec<CompilingError>>;

impl Compiler {
    pub fn compile_program(&mut self) -> CompileResult<IrProgram> {
        if !self.0.errors.is_empty() {
            return Err(self
                .0
                .errors
                .iter()
                .map(|error| {
                    CompilingError::new(
                        CompilingErrorKind::ParsingError(error.clone()),
                        Position::default(),
                    )
                })
                .collect());
        }

        let mut ir_program = IrProgram::default();

        for statement in self.0.statements.clone().iter() {
            ir_program.0.push(self.compile_statement(statement)?);
        }

        Ok(ir_program)
    }

    fn compile_statement(&mut self, statement: &Statement) -> CompileResult<IrStatement> {
        Ok(match statement {
            Statement::LetStatement(statement) => {
                IrStatement::LetStatement(self.compile_let_statement(statement)?)
            }
            Statement::AutoStatement(statement) => {
                IrStatement::LetStatement(self.compile_auto_statement(statement)?)
            }
            Statement::ReturnStatement(statement) => {
                IrStatement::ReturnStatement(self.compile_return_statement(statement)?)
            }
            Statement::TypeStatement(_) => todo!(),
            Statement::DeclareStatement(_) => todo!(),
            Statement::StructStatement(_) => todo!(),
            Statement::ExpressionStatement(statement) => {
                IrStatement::IrExpressionStatement(IrExpressionStatement::new(
                    self.compile_expression(&statement.expression)?,
                    statement.position,
                ))
            }
        })
    }

    fn compile_let_statement(&mut self, statement: &LetStatement) -> CompileResult<IrLetStatement> {
        let LetStatement {
            identifier,
            value,
            position,
            ..
        } = statement;

        Ok(IrLetStatement::new(
            identifier.clone().into(),
            self.compile_expression(value)?,
            *position,
        ))
    }

    fn compile_return_statement(
        &mut self,
        statement: &ReturnStatement,
    ) -> CompileResult<IrReturnStatement> {
        let ReturnStatement { value, position } = statement;

        Ok(IrReturnStatement::new(
            self.compile_expression(value)?,
            *position,
        ))
    }

    fn compile_auto_statement(
        &mut self,
        statement: &AutoStatement,
    ) -> CompileResult<IrLetStatement> {
        let AutoStatement {
            identifier,
            value,
            position,
        } = statement;

        Ok(IrLetStatement::new(
            identifier.clone().into(),
            self.compile_expression(value)?,
            *position,
        ))
    }

    fn compile_expression(&mut self, expression: &Expression) -> CompileResult<IrExpression> {
        Ok(match expression {
            Expression::Literal(literal) => self.compile_literal(literal)?,
            _ => todo!(),
        })
    }

    fn compile_literal(&mut self, expression: &Literal) -> CompileResult<IrExpression> {
        Ok(match expression {
            Literal::Identifier(identifier) => IrExpression::Identifier(identifier.clone().into()),
            _ => todo!(),
        })
    }
}
