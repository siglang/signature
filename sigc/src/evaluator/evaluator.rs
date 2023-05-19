use super::{
    enviroment::Environment, object::Object, EvaluateError, EvaluateErrorKind, EvaluateResult,
};
use parser::ast::{
    DataType, DataTypeKind, Expression, LetStatement, Literal, Position, Program, Statement,
};

#[derive(Debug)]
pub struct Evaluator {
    pub program: Program,
    pub environment: Environment,
}

impl Evaluator {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            environment: Environment::new(None),
        }
    }

    pub fn evaluate(&mut self) -> EvaluateResult<()> {
        for statement in &self.program.clone() {
            self.eval_statement(statement)?;
        }

        Ok(())
    }

    fn eval_statement(&mut self, statement: &Statement) -> EvaluateResult<()> {
        match statement {
            Statement::LetStatement(statement) => self.eval_let_statement(statement),
            Statement::AutoStatement(statement) => {
                let statement = statement.clone();
                self.eval_let_statement(&LetStatement {
                    identifier: statement.identifier,
                    value: statement.value,
                    data_type: DataType::new(DataTypeKind::Unknown, Position(0, 0)),
                    position: statement.position,
                })
            }
            Statement::ReturnStatement(_) => todo!(),
            Statement::ReturnExpressionStatement(_) => todo!(),
            Statement::TypeStatement(_) => todo!(),
            Statement::DeclareStatement(_) => todo!(),
            Statement::StructStatement(_) => todo!(),
            Statement::ExpressionStatement(statement) => {
                self.eval_expression(&statement.expression, statement.position)?;

                Ok(())
            }
        }
    }

    fn eval_let_statement(&mut self, statement: &LetStatement) -> EvaluateResult<()> {
        let identifier = statement.identifier.value.clone();
        let value = self.eval_expression(&statement.value, statement.position)?;

        self.environment.insert(&identifier, value).ok_or_else(|| {
            EvaluateError::new(
                EvaluateErrorKind::IdentifierAlreadyDefined(identifier),
                statement.identifier.position,
            )
        })?;

        Ok(())
    }

    fn eval_expression(
        &mut self,
        expression: &Expression,
        position: Position,
    ) -> EvaluateResult<Object> {
        match expression {
            Expression::BlockExpression(_) => todo!(),
            Expression::PrefixExpression(_) => todo!(),
            Expression::InfixExpression(_) => todo!(),
            Expression::IfExpression(_) => todo!(),
            Expression::CallExpression(_) => todo!(),
            Expression::TypeofExpression(_) => todo!(),
            Expression::IndexExpression(_) => todo!(),
            Expression::Literal(literal) => self.eval_literal(literal, position),
            Expression::Debug(expression, position) => {
                let value = self.eval_expression(expression, *position)?;
                println!("[E:DEBUG:{position}]: {:?}", value);
                Ok(value)
            }
        }
    }

    fn eval_literal(&mut self, literal: &Literal, _position: Position) -> EvaluateResult<Object> {
        match literal {
            Literal::Identifier(identifier) => {
                self.environment.get(&identifier.value).ok_or_else(|| {
                    EvaluateError::new(
                        EvaluateErrorKind::IdentifierNotDefined(identifier.value.clone()),
                        identifier.position,
                    )
                })
            }
            Literal::NumberLiteral(literal) => Ok(Object::Number(literal.value)),
            Literal::StringLiteral(literal) => Ok(Object::String(literal.value.clone())),
            Literal::BooleanLiteral(literal) => Ok(Object::Boolean(literal.value)),
            Literal::FunctionLiteral(_) => todo!(),
            Literal::ArrayLiteral(_) => todo!(),
            Literal::StructLiteral(_) => todo!(),
        }
    }
}
