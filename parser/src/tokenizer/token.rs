use crate::ast::Position;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
#[rustfmt::skip]
pub enum TokenKind {
    ILLEGAL(String), EOF, IDENT(String),

    Number(f64), String(String), Boolean(bool), Comment,

    Assign, Plus, Minus, Bang, Asterisk, Slash, Percent, Arrow, DoubleArrow,

    Dot, Comma, Colon, Semicolon,

    LParen, RParen, LBrace, RBrace, LBracket, RBracket,

    LT, GT, LTE, GTE, EQ, NEQ,

    Let, Mut, If, Else, Return, Function, Type, Declare, Struct, Typeof, Spread,

    NumberType, StringType, BooleanType,

    Debug
}

impl From<String> for TokenKind {
    fn from(s: String) -> Self {
        match s.as_str() {
            "let" => TokenKind::Let,
            "mut" => TokenKind::Mut,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "return" => TokenKind::Return,
            "fn" => TokenKind::Function,
            "type" => TokenKind::Type,
            "declare" => TokenKind::Declare,
            "struct" => TokenKind::Struct,
            "typeof" => TokenKind::Typeof,
            "spread" => TokenKind::Spread,
            "true" => TokenKind::Boolean(true),
            "false" => TokenKind::Boolean(false),
            "number" => TokenKind::NumberType,
            "string" => TokenKind::StringType,
            "boolean" => TokenKind::BooleanType,
            "debug" => TokenKind::Debug,
            s => TokenKind::IDENT(s.to_string()),
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        macro_rules! to_s {
            ($( $x:ident )*) => {
                match &self {
                    $( TokenKind::$x(x) => x.to_string(), )*
                    _ => format!("{:?}", self)
                }
            }
        }

        write!(f, "{}", to_s! { IDENT String Number Boolean })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Position,
}

impl Default for Token {
    fn default() -> Self {
        Token::new(TokenKind::ILLEGAL(String::new()), Position(0, 0))
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token {:?} at {:?}", self.kind, self.position)
    }
}

impl Token {
    pub fn new(kind: TokenKind, position: Position) -> Self {
        Token { kind, position }
    }
}
