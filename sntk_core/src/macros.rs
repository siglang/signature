#[macro_export]
macro_rules! ident {
    ($self:ident) => {
        match $self.current_token.token_type {
            Tokens::IDENT(ref ident) => ident.clone(),
            _ => {
                // return Err(ParsingError::new(
                //     UNEXPECTED_TOKEN,
                //     vec![&$self.current_token.token_type.stringify()],
                //     position! { $self },
                // ))
                return Err(parsing_error! {
                    $self;
                    UNEXPECTED_TOKEN;
                    $self.current_token.token_type
                });
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

#[macro_export]
macro_rules! parsing_error {
    ($self:ident; $msg:ident; $( $r:expr ),*) => {
        ParsingError::new(
            $msg,
            vec![$( format!("{}", $r) ),*],
            position! { $self },
        )
    };
}
