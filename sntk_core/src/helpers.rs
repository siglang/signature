#[macro_export]
macro_rules! identifier {
    ($self:ident) => {
        match $self.current_token.token_type {
            $crate::tokenizer::token::Tokens::IDENT(ref ident) => ident.clone(),
            _ => {
                return Err(ParsingError::new(
                    ParsingErrorKind::UnexpectedToken($self.current_token.token_type.to_string()),
                    $self.position,
                ));
            }
        }
    };
}
