use crate::{
    parser::Position,
    tokenizer::{Token, TokenKind},
};

#[derive(Debug, Default)]
pub struct Lexer<'a> {
    pub input: &'a str,
    pub position: usize,
    pub read_position: usize,
    pub current_char: char,
    pub current_position: Position,
}

impl<'a> Lexer<'a> {
    #[inline]
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            ..Default::default()
        };

        lexer.read_char();
        lexer
    }

    pub fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.current_char = '\0';
        } else {
            self.current_char = self.input.chars().nth(self.read_position).unwrap();
        }

        self.position = self.read_position;
        self.read_position += 1;

        self.current_position.1 += 1;
    }

    pub fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.read_position).unwrap()
        }
    }

    pub fn skip_whitespace(&mut self) {
        while self.current_char.is_whitespace() {
            if self.current_char == '\n' {
                self.current_position.0 += 1;
                self.current_position.1 = 0;
            }

            self.read_char();
        }
    }

    pub fn read_identifier(&mut self) -> String {
        let position = self.position;
        while self.current_char.is_alphanumeric() || self.current_char == '_' {
            self.read_char();
        }

        let result = self.input[position..self.position].to_string();

        macro_rules! replace_all {
            ($s:expr, $($t:expr => $r:expr),*) => {{
                let mut s = String::from($s);
                $( s = s.replace($t, $r); )*
                s
            }};
        }

        replace_all! {
            result,
            "\\r" => "\r",
            "\\t" => "\t",
            "\\n" => "\n",
            "\\\"" => "\"",
            "\\\\" => "\\"
        }
    }

    pub fn read_number(&mut self) -> f64 {
        let position = self.position;
        let mut has_dot = false;

        while self.current_char.is_numeric() || self.current_char == '.' {
            if self.current_char == '.' {
                if has_dot {
                    break;
                }

                has_dot = true;
            }

            self.read_char();
        }

        self.input[position..self.position].parse().unwrap_or(0.)
    }

    pub fn read_string(&mut self) -> String {
        let position = self.position + 1;
        while self.peek_char() != '"' && self.current_char != '\0' {
            self.read_char();
        }

        self.read_char();
        self.input[position..self.position].to_string()
    }

    pub fn read_comment(&mut self) {
        if self.current_char == '/' && self.peek_char() == '*' {
            self.read_char();
            self.read_char();

            while self.current_char != '\0' && (self.current_char != '*' || self.peek_char() != '/')
            {
                self.read_char();
            }

            self.read_char();
        }
    }

    pub fn read_inline_comment(&mut self) {
        if self.current_char == '/' && self.peek_char() == '/' {
            self.read_char();
            self.read_char();

            while self.current_char != '\0' && self.current_char != '\n' {
                self.read_char();
            }

            self.skip_whitespace();
        }
    }

    pub fn next_token(&mut self) -> Token {
        use super::token::TokenKind::*;

        self.skip_whitespace();

        macro_rules! match_token {
            ($($token:expr => $token_type:expr),*) => {{
                let position = self.current_position;

                match self.current_char {
                    $( $token => Token::new($token_type, position), )*
                    token => Token::new(TokenKind::ILLEGAL(token.to_string()), position)
                }
            }}
        }

        macro_rules! next {
            (@macro_read $n_token:expr => $t_token:expr; $e_token:expr; $next:expr) => {
                if self.peek_char() == $n_token {
                    if $next {
                        self.read_char();
                    }
                    $t_token
                } else {
                    $e_token
                }
            };
            (@no_read $n_token:expr => $t_token:expr; $e_token:expr) => {
                next!(@macro_read $n_token => $t_token; $e_token; false)
            };
            ($n_token:expr => $t_token:expr; $e_token:expr) => {
                next!(@macro_read $n_token => $t_token; $e_token; true)
            };
        }

        let token = match_token! {
            '+' => Plus,
            '*' => Asterisk,
            '%' => Percent,
            '.' => Dot,
            ',' => Comma,
            ';' => Semicolon,
            ':' => Colon,
            '(' => LParen,
            ')' => RParen,
            '{' => LBrace,
            '}' => RBrace,
            '[' => LBracket,
            ']' => RBracket,

            '-' => next!('>' => Arrow; Minus),

            '=' => next!('=' => EQ; next!('>' => DoubleArrow; Assign)),
            '!' => next!('=' => NEQ; Bang),
            '<' => next!('=' => LTE; LT),
            '>' => next!('=' => GTE; GT),

            '"' => String(self.read_string()),

            '/' => next!(@no_read '*' => {
                self.read_comment();
                self.next_token();

                return self.next_token();
            }; next!(@no_read '/' => {
                self.read_inline_comment();

                return self.next_token();
            }; Slash)),

            '\0' => EOF
        };

        match self.current_char {
            c if c.is_alphabetic() => {
                let position = self.current_position;
                Token::new(TokenKind::from(self.read_identifier()), position)
            }
            c if c.is_numeric() => {
                let position = self.current_position;
                Token::new(TokenKind::Number(self.read_number()), position)
            }
            _ => {
                self.read_char();
                token
            }
        }
    }
}
