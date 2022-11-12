pub mod ast;
pub mod parser;

use self::ast::Position;
use sntk_proc::ErrorFormat;

#[derive(Debug, Clone, ErrorFormat)]
pub struct ParsingError {
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
    EXPECTED_NEXT_TOKEN => "expected next token to be {0}, got {1} instead";
    EXPECTED_DATA_TYPE => "expected next token to be a data type, got {0} instead";
    EXPECTED_EXPRESSION => "expected next token to be an expression, got {0} instead";
    UNEXPECTED_TOKEN => "unexpected token {0}";
}
