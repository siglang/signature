use std::fmt::*;

/// An enumeration of tokens, which are the basic units of that make up a language.
#[derive(Debug, PartialEq, Clone)]
#[rustfmt::skip]
pub enum Tokens {
    ILLEGAL(String), EOF, IDENT(String),

    Number(f64), String(String), Boolean(bool), Comment(String),

    Assign, Plus, Minus, Bang, Asterisk, Slash, Percent, Arrow,

    Comma, Colon, Semicolon,

    LParen, RParen, LBrace, RBrace, LBracket, RBracket,

    LT, GT, LTE, GTE, EQ, NEQ,

    Let, If, Else, Return, Function, Type,

    NumberType, StringType, BooleanType, /* ArrayType = type[] */ ObjectType, /* FnType = Function */
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
            "type" => Tokens::Type,
            "true" => Tokens::Boolean(true),
            "false" => Tokens::Boolean(false),
            "number" => Tokens::NumberType,
            "string" => Tokens::StringType,
            "boolean" => Tokens::BooleanType,
            "object" => Tokens::ObjectType,
            s => Tokens::IDENT(s.to_string()),
        }
    }
}

impl Display for Tokens {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.stringify())
    }
}

impl Tokens {
    /// Stringify the token.
    pub fn stringify(&self) -> String {
        macro_rules! to_s {
                ($( $x:ident )*) => {
                    match &self {
                        $( Tokens::$x(x) => x.to_string(), )*
                        _ => format!("{:?}", self)
                    }
                }
            }

        to_s! { IDENT String Number Boolean Comment }
    }
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

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "Token {:?} at {:?}", self.token_type, self.position)
    }
}

impl Token {
    /// Creates a new `Token` from a `Tokens` and a position.
    pub fn new(token_type: Tokens, position: (usize, usize)) -> Self {
        Token {
            token_type,
            position,
        }
    }
}
