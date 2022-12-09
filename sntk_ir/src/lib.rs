use sntk_core::parser::ast::Position;
use sntk_proc::ErrorFormat;
use std::fmt;

pub mod builtin;
pub mod instruction;
pub mod interpreter;

#[derive(Debug, Clone, ErrorFormat)]
pub struct RuntimeError {
    pub message: String,
    pub position: Position,
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
    INVALID_OPERATOR => "Invalid operator `{0}`";
    INVALID_OPERANDS => "Invalid operands `{0}` and `{1}` for operator `{2}`";
}

#[macro_export]
macro_rules! runtime_error {
    ($msg:ident; $( $r:expr ),*; $position:expr) => {
        $crate::RuntimeError::new($msg, vec![$( format!("{}", $r) ),*], &$position)
    };
}
