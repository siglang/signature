pub mod analyzer;
pub mod symbol_table;

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
}

#[derive(Debug, Clone, Error, PartialEq)]
#[rustfmt::skip]
pub enum SemanticErrorKind {
    
}
