pub mod enviroment;
pub mod object;

use enviroment::Environment;
use object::Object;
use parser::ast::{
    Expression, InfixExpression, InfixOperator, LetStatement, Literal, Position, PrefixExpression,
    PrefixOperator, Program, Statement,
};
use std::fmt;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub struct EvaluateError {
    pub kind: EvaluateErrorKind,
    pub position: Position,
}

impl fmt::Display for EvaluateError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.position, self.kind)
    }
}

impl EvaluateError {
    pub fn new(kind: EvaluateErrorKind, position: Position) -> Self {
        Self { kind, position }
    }
}

#[derive(Debug, Clone, Error, PartialEq)]
#[rustfmt::skip]
pub enum EvaluateErrorKind {
    #[error("Identifier `{0}` already defined")] IdentifierAlreadyDefined(String),
    #[error("Identifier `{0}` not defined")] IdentifierNotDefined(String),
    #[error("Cannot operate `{0}` operator on `{1}``")] InvalidOperator1(String, String),
    #[error("Cannot operate `{0}` operator on `{1}` and `{2}`")] InvalidOperator2(String, String, String),
}

pub type EvaluateResult<T> = Result<T, EvaluateError>;

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
            Expression::PrefixExpression(expression) => self.eval_prefix_expression(expression),
            Expression::InfixExpression(expression) => self.eval_infix_expression(expression),
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

    fn eval_prefix_expression(&mut self, expression: &PrefixExpression) -> EvaluateResult<Object> {
        let right = self.eval_expression(&expression.right, expression.position)?;

        macro_rules! operate {
            ($type:ident, $operator:tt) => {{
                Ok(match right {
                    Object::$type(right) => Object::$type($operator right),
                    _ => {
                        return Err(EvaluateError::new(
                            EvaluateErrorKind::InvalidOperator1(
                                expression.operator.to_string(),
                                right.to_string(),
                            ),
                            expression.position,
                        ));
                    }
                })
            }}
        }

        match expression.operator {
            PrefixOperator::Minus => operate!(Number, -),
            PrefixOperator::Not => operate!(Boolean, !),
        }
    }

    fn eval_infix_expression(&mut self, expression: &InfixExpression) -> EvaluateResult<Object> {
        let left = self.eval_expression(&expression.left, expression.position)?;
        let right = self.eval_expression(&expression.right, expression.position)?;

        macro_rules! operate {
            ($type:ident, $ret:ident, $operator:tt) => {{
                Ok(match left {
                    Object::$type(left) => {
                        match right {
                            Object::$type(right) => Object::$ret(left $operator right),
                            _ => {
                                return Err(EvaluateError::new(
                                    EvaluateErrorKind::InvalidOperator2(
                                        expression.operator.to_string(),
                                        left.to_string(),
                                        right.to_string(),
                                    ),
                                    expression.position,
                                ));
                            }
                        }
                    },
                    _ => {
                        return Err(EvaluateError::new(
                            EvaluateErrorKind::InvalidOperator1(
                                expression.operator.to_string(),
                                right.to_string(),
                            ),
                            expression.position,
                        ));
                    }
                })
            }}
        }

        match expression.operator {
            InfixOperator::Dot => todo!(),
            InfixOperator::Plus => operate!(Number, Number, +),
            InfixOperator::Minus => operate!(Number, Number, -),
            InfixOperator::Asterisk => operate!(Number, Number, *),
            InfixOperator::Slash => operate!(Number, Number, /),
            InfixOperator::Percent => operate!(Number, Number, %),
            InfixOperator::EQ => Ok(Object::Boolean(left == right)),
            InfixOperator::NEQ => Ok(Object::Boolean(left != right)),
            InfixOperator::GT => operate!(Number, Boolean, >),
            InfixOperator::GTE => operate!(Number, Boolean, >=),
            InfixOperator::LT => operate!(Number, Boolean, <),
            InfixOperator::LTE => operate!(Number, Boolean, <=),
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
