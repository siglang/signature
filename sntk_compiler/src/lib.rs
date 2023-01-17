pub mod checker;
pub mod compiler;

use sntk_core::parser::{ParsingError, Position};
use std::fmt;
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum CompileError {
    ParsingError(Vec<ParsingError>),
    TypeError(TypeError),
}

impl<'help> fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ParsingError(errors) => write!(f, "{}", errors.iter().map(ToString::to_string).collect::<Vec<_>>().join("\n")),
            Self::TypeError(TypeError { message, position, .. }) => write!(f, "{}: {}", position, message),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: TypeErrorKind,
    pub help: Option<String>,
    pub position: Position,
}

impl TypeError {
    pub fn new<T>(message: TypeErrorKind, help: Option<T>, position: Position) -> CompileError
    where
        T: ToString + 'static,
    {
        let help = help.map(|help| help.to_string());
        CompileError::TypeError(Self { message, help, position })
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
    #[error("`if` expression without alternative")] IfExpressionWithoutAlternative
}
