use sntk_core::parser::ast::Position;
use std::{borrow::Cow, fmt};

pub mod builtin;
pub mod instruction;
pub mod interpreter;

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub message: String,
    pub position: Position,
}

impl RuntimeError {
    pub fn new(message: &str, args: Vec<&str>, position: &Position) -> Self {
        let mut message = message.to_string();

        args.iter().enumerate().for_each(|(i, arg)| {
            message = message.replace(&format!("{{{}}}", i), arg);
        });

        Self {
            message,
            position: position.to_owned(),
        }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "RuntimeError: {} at line {}, column {}",
            self.message, self.position.0, self.position.1
        )
    }
}

macro_rules! messages {
    ($( $name:ident => $message:expr );*;) => {
        $(
            pub const $name: &str = $message;
        )*
    };
}

messages! {
    UNDEFINED_VARIABLE => "Undefined variable `{0}`";
    NOT_A_FUNCTION => "`{0}` is not a function";
    NOT_A_ARRAY => "`{0}` is not a array";
    INVALID_OPERATOR => "Invalid operator `{0}`";
    INVALID_OPERANDS => "Invalid operands `{0}` and `{1}` for operator `{2}`";
    INDEX_OUT_OF_BOUNDS => "Index out of bounds `{0}`";
}

pub fn runtime_error<T>(message: T, replacements: Cow<[&str]>, position: &Position) -> RuntimeError
where
    T: Into<String>,
{
    RuntimeError::new(&message.into(), replacements.into_owned(), position)
}
