use parser::parser::Position;
use thiserror::Error;

pub mod builtin;
pub mod instruction;
pub mod interpreter;

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub message: RuntimeErrorKind,
    pub position: Position,
}

impl RuntimeError {
    pub fn new(message: RuntimeErrorKind, position: Position) -> Self {
        Self { message, position }
    }
}

#[derive(Debug, Clone, Error)]
#[rustfmt::skip]
pub enum RuntimeErrorKind {
    #[error("Undefined variable `{0}`")] UndefinedVariable(String),
    #[error("`{0}` is not a function")] NotAFunction(String),
    #[error("`{0}` is not a array")] NotAnArray(String),
    #[error("Invalid operator `{0}`")] InvalidOperator(String),
    #[error("Invalid operands `{0}` and `{1}` for operator `{2}`")] InvalidOperands(String, String, String),
    #[error("Index out of bounds `{0}`")] IndexOutOfBounds(usize),
}
