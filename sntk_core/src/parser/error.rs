use super::ast::*;

#[derive(Debug, PartialEq, Default, Clone)]
pub struct ParsingError {
    pub message: String,
    pub position: Position,
}

impl ParsingError {
    pub fn new(message: &str, args: Vec<String>, position: Position) -> Self {
        let mut message = message.to_string();

        args.iter().enumerate().for_each(|(i, arg)| {
            message = message.replace(&format!("{{{i}}}"), arg);
        });

        ParsingError { message, position }
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
    EXPECTED_NEXT_TOKEN => "expected next token to be {0}, got {1} instead";
    EXPECTED_DATA_TYPE => "expected next token to be a data type, got {0} instead";
    EXPECTED_EXPRESSION => "expected next token to be an expression, got {0} instead";
    UNEXPECTED_TOKEN => "unexpected token {0}";
}
