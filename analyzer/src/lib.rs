pub mod analyzer;
pub mod symbol_table;
pub mod type_checker;

use parser::ast::Position;
use std::fmt;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub struct SemanticError {
    pub message: SemanticErrorKind,
    pub position: Position,
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.position, self.message)
    }
}

impl SemanticError {
    pub fn new(message: SemanticErrorKind, position: Position) -> Self {
        Self { message, position }
    }

    pub fn type_mismatch<T>(left: T, right: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            SemanticErrorKind::TypeMismatch(left.to_string(), right.to_string()),
            position,
        )
    }

    pub fn identifier_not_defined<T>(identifier: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            SemanticErrorKind::IdentifierNotDefined(identifier.to_string()),
            position,
        )
    }

    pub fn identifier_already_defined<T>(identifier: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            SemanticErrorKind::IdentifierAlreadyDefined(identifier.to_string()),
            position,
        )
    }

    pub fn type_alias_not_defined<T>(type_alias: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            SemanticErrorKind::TypeAliasNotDefined(type_alias.to_string()),
            position,
        )
    }

    pub fn type_alias_already_defined<T>(type_alias: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            SemanticErrorKind::TypeAliasAlreadyDefined(type_alias.to_string()),
            position,
        )
    }

    pub fn operator_not_supported<O, D>(operator: O, data_type: D, position: Position) -> Self
    where
        O: ToString,
        D: ToString,
    {
        Self::new(
            SemanticErrorKind::OperatorNotSupported(operator.to_string(), data_type.to_string()),
            position,
        )
    }
}

#[derive(Debug, Clone, Error, PartialEq)]
#[rustfmt::skip]
pub enum SemanticErrorKind {
    #[error("Type mismatch: `{0}` is not `{1}`")] TypeMismatch(String, String),
    #[error("Identifier `{0}` is not defined")] IdentifierNotDefined(String),
    #[error("Type alias `{0}` is not defined")] TypeAliasNotDefined(String),
    #[error("Identifier `{0}` is already defined")] IdentifierAlreadyDefined(String),
    #[error("Type alias `{0}` is already defined")] TypeAliasAlreadyDefined(String),
    #[error("Operator `{0}` is not supported for type `{1}`")] OperatorNotSupported(String, String),
}

pub type SemanticResult<T> = Result<T, SemanticError>;
