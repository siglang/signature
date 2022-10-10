use super::token::*;
use crate::*;

#[derive(Debug)]
pub struct Lexer {
    pub input: String,
    pub position: usize,
    pub read_position: usize,
    pub current_char: char,
    pub current_position: (usize, usize),
}

impl Default for Lexer {
    fn default() -> Self {
        Self {
            input: String::new(),
            position: 0,
            read_position: 0,
            current_char: '\0',
            current_position: (1, 0),
        }
    }
}

impl Lexer {
    pub fn new<T: Into<String>>(input: T) -> Self {
        let mut lexer = Lexer {
            input: input.into(),
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

    pub fn next_token(&mut self) -> Token {
        use super::token::Tokens::*;

        self.skip_whitespace();

        macro_rules! match_token {
            ($($token:expr => $token_type:expr),*) => {
                match self.current_char {
                    $($token => Token::new($token_type, self.current_position),)*
                    _ => Token::new(Tokens::ILLEGAL, self.current_position),
                }
            }
        }

        macro_rules! next {
            ($n_token:expr => $t_token:expr; $e_token:expr) => {
                if self.peek_char() == $n_token {
                    bind! { self.read_char() => $t_token }
                } else {
                    $e_token
                }
            };
        }

        let token = match_token! {
            '+' => Plus,
            '-' => Minus,
            '*' => Asterisk,
            '/' => Slash,
            '<' => LT,
            '>' => GT,
            ',' => Comma,
            ';' => Semicolon,
            ':' => Colon,
            '(' => LParen,
            ')' => RParen,
            '{' => LBrace,
            '}' => RBrace,
            '[' => LBracket,
            ']' => RBracket,
            '"' => String(self.read_string()),
            '=' => next!('=' => EQ; Assign),
            '!' => next!('=' => NEQ; Bang),
            '\0' => EOF
        };

        match self.current_char {
            c if c.is_alphabetic() => Token::new(Tokens::from(self.read_identifier()), self.current_position),
            c if c.is_numeric() => Token::new(Tokens::Number(self.read_number()), self.current_position),
            _ => bind! { self.read_char() => token },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = r#"let five = 5;
let ten = 10;
"#;

        let tests = vec![
            Token::new(Tokens::Let, (1, 4)),
            Token::new(Tokens::IDENT("five".to_string()), (1, 9)),
            Token::new(Tokens::Assign, (1, 10)),
            Token::new(Tokens::Number(5.), (1, 13)),
            Token::new(Tokens::Semicolon, (1, 13)),
            Token::new(Tokens::Let, (2, 4)),
            Token::new(Tokens::IDENT("ten".to_string()), (2, 8)),
            Token::new(Tokens::Assign, (2, 9)),
            Token::new(Tokens::Number(10.), (2, 13)),
            Token::new(Tokens::Semicolon, (2, 13)),
            Token::new(Tokens::EOF, (3, 1)),
        ];

        let mut lexer = Lexer::new(input);

        for test in tests {
            let token = lexer.next_token();
            assert_eq!(token, test);
        }
    }

    #[test]
    fn test_next_token2() {
        let mut z = Lexer::new(r#"let x_32z = y != "Hello, World\n";"#);

        let mut x = Token::new(Tokens::ILLEGAL, (0, 0));

        while x.token_type != Tokens::EOF {
            x = z.next_token();
            println!("{:?}", x);
        }
    }
}
