#[macro_export]
macro_rules! identifier {
    ($self:ident) => {
        match $self.current_token.kind {
            $crate::tokenizer::token::TokenKind::IDENT(ref ident) => ident.clone(),
            _ => {
                return Err(ParsingError::new(
                    ParsingErrorKind::UnexpectedToken($self.current_token.kind.to_string()),
                    $self.position,
                ));
            }
        }
    };
}
