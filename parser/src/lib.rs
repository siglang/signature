pub mod ast;
pub mod helpers;
pub mod parser;
pub mod tokenizer;

pub use parser::*;

use ast::Position;
use std::fmt;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub struct ParsingError {
    pub message: ParsingErrorKind,
    pub position: Position,
}

impl fmt::Display for ParsingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.position, self.message)
    }
}

impl ParsingError {
    pub fn new(message: ParsingErrorKind, position: Position) -> Self {
        Self { message, position }
    }

    pub fn expected_next_token<T>(expected: T, got: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            ParsingErrorKind::ExpectedNextToken(expected.to_string(), got.to_string()),
            position,
        )
    }

    pub fn expected_data_type<T>(expected: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            ParsingErrorKind::ExpectedDataType(expected.to_string()),
            position,
        )
    }

    pub fn expected_expression<T>(expected: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            ParsingErrorKind::ExpectedExpression(expected.to_string()),
            position,
        )
    }

    pub fn unexpected_token<T>(token: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            ParsingErrorKind::UnexpectedToken(token.to_string()),
            position,
        )
    }
}

#[derive(Debug, Clone, Error, PartialEq)]
#[rustfmt::skip]
pub enum ParsingErrorKind {
    #[error("Expected next token to be `{0}`, got `{1}` instead")] ExpectedNextToken(String, String),
    #[error("Expected next token to be a data type, got `{0}` instead")] ExpectedDataType(String),
    #[error("Expected next token to be an expression, got `{0}` instead")] ExpectedExpression(String),
    #[error("Unexpected token `{0}`")] UnexpectedToken(String),
}
