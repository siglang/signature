#[macro_export]
macro_rules! ident {
    ($self:ident) => {
        match $self.current_token.token_type {
            $crate::tokenizer::token::Tokens::IDENT(ref ident) => ident.clone(),
            _ => return Err(parsing_error! { $self; UNEXPECTED_TOKEN; $self.current_token.token_type }),
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
        crate::parser::error::ParsingError::new($msg, vec![$( format!("{}", $r) ),*], position! { $self })
    };
}
