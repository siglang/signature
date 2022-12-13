pub mod checker;
pub mod compiler;

use sntk_core::parser::{ast::Position, ParsingError};
use std::fmt::{self, write};
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum CompileError {
    ParsingError(Vec<ParsingError>),
    TypeError(TypeError),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut messages = String::new();

        match self {
            CompileError::ParsingError(errors) => {
                for ParsingError { message, position } in errors {
                    write(
                        &mut messages,
                        format_args!("Parsing Error: {} at line {}, column {}\n", message, position.0, position.1),
                    )?;
                }
            }
            CompileError::TypeError(TypeError { message, position }) => {
                write(
                    &mut messages,
                    format_args!("Type Error: {} at line {}, column {}\n", message, position.0, position.1),
                )?;
            }
        }

        write!(f, "{}", messages.trim_end())
    }
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
    #[error("Expected {0} type, got {1} instead")] ExpectedDataType(String, String),
    #[error("Expected {0} arguments, got {1} instead")] ExpectedArguments(usize, usize),
    #[error("Undefined identifier: {0}")] UndefinedIdentifier(String),
    #[error("Unknown type: {0}")] UnknownType(String),
    #[error("Unknown array type")] UnknownArrayType,
    #[error("Unexpected parameter length")] UnexpectedParameterLength,
    #[error("{0} is not a callable")] NotCallable(String),
    #[error("{0} is not a indexable")] NotIndexable(String),
}
