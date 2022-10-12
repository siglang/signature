use super::ast::*;
use crate::tokenizer::{lexer::*, token::*};

/// **Parses the input string into an AST.**
///
/// for example:
/// ```rust
/// use sntk_core::parser::parser::*;
///
/// let parsed = Parser::from("let x = 10;").parse_program();
/// println!("{parsed:#?}");
/// ```
#[derive(Debug)]
pub struct Parser {
    pub lexer: Lexer,
    pub current_token: Token,
    pub peek_token: Token,
    pub position: Position,
    pub errors: Vec<String>,
}

impl<T> From<T> for Parser
where
    T: Into<String>,
{
    fn from(x: T) -> Self {
        Parser::new(Lexer::new(x))
    }
}

impl Parser {
    /// **Creates a new Parser instance.**
    /// it takes an argument of type `Lexer`.
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: Token::default(),
            peek_token: Token::default(),
            position: Position::default(),
            errors: vec![],
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    /// **Advances the current token and the peek token.**
    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();

        self.position = Position::new(self.current_token.position.0, self.current_token.position.1);
    }

    /// **Parses the input string into an AST.**
    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::default();

        while self.current_token.token_type != Tokens::EOF {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }

            self.next_token();
        }

        program
    }

    /// **Parses a statement.**
    fn parse_statement(&mut self) -> Option<Statement> {
        println!("Parsing statement: {:?}", self.current_token);

        None
    }
}
