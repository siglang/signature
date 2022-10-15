#[macro_export]
macro_rules! ident {
    ($self:ident) => {
        match $self.current_token.token_type {
            Tokens::IDENT(ref ident) => ident.clone(),
            _ => {
                return Err(ParsingError::new(
                    UNEXPECTED_TOKEN,
                    vec![&$self.current_token.token_type.stringify()],
                    position! { $self },
                ))
            }
        }
    };
}

#[macro_export]
macro_rules! position {
    ($self:ident) => {
        $self.position.clone()
    };
}
