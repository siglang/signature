use crate::parser::{ast::Position, ParsingError};
use std::borrow::Cow;

pub fn parsing_error<T>(message: T, replacements: Cow<[&str]>, position: &Position) -> ParsingError
where
    T: Into<String>,
{
    ParsingError::new(&message.into(), replacements.into_owned(), position)
}

#[macro_export]
macro_rules! identifier {
    ($self:ident) => {
        match $self.current_token.token_type {
            $crate::tokenizer::token::Tokens::IDENT(ref ident) => ident.clone(),
            _ => {
                return Err(parsing_error(
                    UNEXPECTED_TOKEN,
                    std::borrow::Cow::Borrowed(&[&$self.current_token.token_type.to_string()]),
                    &$self.position,
                ))
            }
        }
    };
}
