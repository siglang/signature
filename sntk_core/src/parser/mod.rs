use self::ast::Position;
use thiserror::Error;

pub mod ast;
pub mod parser;

#[derive(Debug, Clone)]
pub struct ParsingError {
    pub message: ParsingErrorKind,
    pub position: Position,
}

impl ParsingError {
    pub fn new(message: ParsingErrorKind, position: Position) -> Self {
        Self { message, position }
    }
}

// EXPECTED_NEXT_TOKEN => "expected next token to be {0}, got {1} instead";
// EXPECTED_DATA_TYPE => "expected next token to be a data type, got {0} instead";
// EXPECTED_EXPRESSION => "expected next token to be an expression, got {0} instead";
// UNEXPECTED_TOKEN => "unexpected token {0}";

#[derive(Debug, Clone, Error)]
#[rustfmt::skip]
pub enum ParsingErrorKind {
    #[error("Expected next token to be {0}, got {1} instead")] ExpectedNextToken(String, String),
    #[error("Expected next token to be a data type, got {0} instead")] ExpectedDataType(String),
    #[error("Expected next token to be an expression, got {0} instead")] ExpectedExpression(String),
    #[error("Unexpected token {0}")] UnexpectedToken(String),
}
