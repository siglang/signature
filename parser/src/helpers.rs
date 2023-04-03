#[macro_export]
macro_rules! identifier {
    ($self:ident) => {
        match $self.current_token.kind {
            $crate::tokenizer::TokenKind::IDENT(ref ident) => ident.to_string(),
            _ => {
                return Err(ParsingError::new(
                    ParsingErrorKind::UnexpectedToken($self.current_token.kind.to_string()),
                    $self.position,
                ));
            }
        }
    };
}
