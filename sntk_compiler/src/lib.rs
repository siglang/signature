pub mod checker;
pub mod compiler;

use sntk_core::parser::{ast::Position, ParsingError};
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum CompileError {
    ParsingError(Vec<ParsingError>),
    TypeError(TypeError),
}

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: TypeErrorKind,
    pub position: Position,
}

impl TypeError {
    pub fn new(message: TypeErrorKind, position: Position) -> CompileError {
        CompileError::TypeError(Self { message, position })
    }
}

#[derive(Debug, Clone, Error)]
#[rustfmt::skip]
pub enum TypeErrorKind {
    #[error("Expected `{0}` type, got `{1}` instead")] ExpectedDataType(String, String),
    #[error("Expected `{0}` arguments, got `{1}` instead")] ExpectedArguments(usize, usize),
    #[error("Undefined identifier: `{0}`")] UndefinedIdentifier(String),
    #[error("Undefined type: `{0}`")] UndefinedType(String),
    #[error("Unknown type: `{0}`")] UnknownType(String),
    #[error("Unknown array type")] UnknownArrayType,
    #[error("Unexpected parameter length")] UnexpectedParameterLength,
    #[error("`{0}` is not a callable")] NotCallable(String),
    #[error("`{0}` is not a indexable")] NotIndexable(String),
    #[error("Spread parameter must be last")] SpreadParameterMustBeLast,
}
