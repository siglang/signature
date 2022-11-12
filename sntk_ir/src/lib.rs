pub mod builtin;
pub mod environment;
pub mod instruction;
pub mod interpreter;

use sntk_core::parser::ast::Position;
use sntk_proc::ErrorFormat;

#[derive(Debug, Clone, ErrorFormat)]
pub struct RuntimeError {
    pub message: String,
    pub position: Position,
}

macro_rules! messages {
    ($( $name:ident => $message:expr );*;) => {
        $(
            pub const $name: &str = $message;
        )*
    };
}

messages! {
    CANNOT_CALL_NON_FUNCTION => "cannot call non-function: {0}";
    UNDEFINED_IDENTIFIER => "undefined identifier: {0}";
    INDEX_OUT_OF_BOUNDS => "index out of bounds: {0}";
}

#[macro_export]
macro_rules! runtime_error {
    ($msg:ident; $( $r:expr ),*; $position:expr) => {
        $crate::RuntimeError::new($msg, vec![$( format!("{}", $r) ),*], $position.clone())
    };
}
