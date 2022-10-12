/// An enumeration of tokens, which are the basic units of that make up a language.
#[derive(Debug, PartialEq, Clone)]
#[rustfmt::skip]
pub enum Tokens {
    ILLEGAL(String), EOF, IDENT(String),

    Number(f64), String(String), Boolean(bool), Comment(String),

    Assign, Plus, Minus, Bang, Asterisk, Slash, Percent,

    Comma, Colon, Semicolon,

    LParen, RParen, LBrace, RBrace, LBracket, RBracket,

    LT, GT, LTE, GTE, EQ, NEQ,

    Let, If, Else, Return, Function,

    NumberType, StringType, BooleanType, /* ArrayType = type[] */ HashType, /* FnType = Function */
}

/// A `Token` structure containing `Tokens` and position (line, column) of the token.
#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: Tokens,
    pub position: (usize, usize),
}

impl Default for Token {
    fn default() -> Self {
        Token::new(Tokens::ILLEGAL(String::from("")), (0, 0))
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token {:?} at {:?}", self.token_type, self.position)
    }
}

impl<T> From<T> for Tokens
where
    T: Into<String>,
{
    fn from(s: T) -> Self {
        match s.into().as_str() {
            "let" => Tokens::Let,
            "if" => Tokens::If,
            "else" => Tokens::Else,
            "return" => Tokens::Return,
            "fn" => Tokens::Function,
            "true" => Tokens::Boolean(true),
            "false" => Tokens::Boolean(false),
            "number" => Tokens::NumberType,
            "string" => Tokens::StringType,
            "boolean" => Tokens::BooleanType,
            "hash" => Tokens::HashType,
            s => Tokens::IDENT(s.to_string()),
        }
    }
}

impl Token {
    /// Creates a new `Token` from a `Tokens` and a position.
    pub fn new(token_type: Tokens, position: (usize, usize)) -> Self {
        Token { token_type, position }
    }

    /// Stringify the token.
    pub fn stringify(&self) -> String {
        macro_rules! to_s {
            ($( $x:ident )*) => {
                match &self.token_type {
                    $( Tokens::$x(x) => x.to_string(), )*
                    _ => format!("{:?}", self.token_type)
                }
            }
        }

        to_s! { IDENT String Number Boolean Comment }
    }
}
