#![allow(clippy::module_inception)]

pub mod enviroment;
pub mod evaluator;
pub mod object;

use parser::ast::Position;
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
}

pub type EvaluateResult<T> = Result<T, EvaluateError>;
