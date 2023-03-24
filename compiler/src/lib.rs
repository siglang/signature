pub mod compiler;
pub mod types;

use parser::parser::{ParsingError, Position};
use std::fmt;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub struct CompilingError {
    pub message: CompilingErrorKind,
    pub position: Position,
}

impl fmt::Display for CompilingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.position, self.message)
    }
}

impl CompilingError {
    pub fn new(message: CompilingErrorKind, position: Position) -> Self {
        Self { message, position }
    }
}

#[derive(Debug, Clone, Error, PartialEq)]
#[rustfmt::skip]
pub enum CompilingErrorKind {
    #[error("Parsing error: {0}")] ParsingError(ParsingError),
    #[error("Expected next token to be `{0}`, got `{1}` instead")] ExpectedNextToken(String, String),
    #[error("Expected next token to be a data type, got `{0}` instead")] ExpectedDataType(String),
    #[error("Expected next token to be an expression, got `{0}` instead")] ExpectedExpression(String),
    #[error("Unexpected token `{0}`")] UnexpectedToken(String),
}
