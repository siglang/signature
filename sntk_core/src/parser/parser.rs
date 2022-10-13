use super::ast::*;
use crate::tokenizer::{lexer::*, token::*};

pub type ParseResult<T> = Result<T, String>;

pub trait ParserTrait {
    fn new(lexer: Lexer) -> Self;
    fn next_token(&mut self);
    fn expect_token(&mut self, token_type: Tokens) -> bool;
    fn peek_token(&self, token_type: Tokens) -> bool;
    fn get_priority(&self, token_type: Tokens) -> Priority;
    fn peek_priority(&mut self) -> Priority;
    fn current_priority(&self) -> Priority;
    fn parse_program(&mut self) -> Program;
    fn parse_statement(&mut self) -> ParseResult<Statement>;
    fn parse_data_type(&mut self) -> ParseResult<DataType>;
    fn parse_let_statement(&mut self) -> ParseResult<Statement>;
    fn parse_return_statement(&mut self) -> ParseResult<Statement>;
    fn parse_expression_statement(&mut self) -> ParseResult<Statement>;
    fn parse_expression(&mut self, precedence: Priority) -> ParseResult<Expression>;
}

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

impl ParserTrait for Parser {
    /// **Creates a new Lexer instance.**
    /// it takes an argument of type `Lexer`.
    fn new(lexer: Lexer) -> Self {
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
    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();

        self.position = Position::new(self.current_token.position.0, self.current_token.position.1);
    }

    /// **Checks if the current token is of the expected type.**
    fn expect_token(&mut self, token_type: Tokens) -> bool {
        if self.current_token.token_type == token_type {
            self.next_token();

            true
        } else {
            self.errors.push(format!(
                "expected next token to be {token_type:?}, got {:?} instead",
                self.current_token.token_type
            ));

            false
        }
    }

    /// **Checks if the peek token is of the expected type.**
    fn peek_token(&self, token_type: Tokens) -> bool {
        self.peek_token.token_type == token_type
    }

    /// **Gets the priority from the `Tokens`.**
    fn get_priority(&self, token_type: Tokens) -> Priority {
        match token_type {
            Tokens::Assign => Priority::Equals,
            Tokens::Plus | Tokens::Minus => Priority::Sum,
            Tokens::Slash | Tokens::Asterisk => Priority::Product,
            Tokens::LT | Tokens::GT => Priority::LessGreater,
            Tokens::EQ | Tokens::NEQ => Priority::Equals,
            Tokens::LParen => Priority::Call,
            Tokens::LBracket => Priority::Index,
            _ => Priority::Lowest,
        }
    }

    /// **Gets the priority from the peek token.**
    fn peek_priority(&mut self) -> Priority {
        self.get_priority(self.peek_token.token_type.clone())
    }

    /// **Gets the priority from the current token.**
    fn current_priority(&self) -> Priority {
        self.get_priority(self.current_token.token_type.clone())
    }

    /// **Parses the input string into an AST.**
    fn parse_program(&mut self) -> Program {
        let mut program = Program::default();

        while self.current_token.token_type != Tokens::EOF {
            match self.parse_statement() {
                Ok(statement) => program.statements.push(statement),
                Err(error) => self.errors.push(error),
            }

            self.next_token();
        }

        program.errors = self.errors.clone();
        program
    }

    /// **Parses a statement.**
    fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.current_token.token_type {
            Tokens::Let => self.parse_let_statement(),
            Tokens::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    /// **Parses a data type.**
    fn parse_data_type(&mut self) -> ParseResult<DataType> {
        // TODO: Add `array`, `hash`, `fn` types.
        match self.current_token.token_type {
            Tokens::NumberType => Ok(DataType::Number),
            Tokens::StringType => Ok(DataType::String),
            Tokens::BooleanType => Ok(DataType::Boolean),
            _ => Err(format!(
                "expected next token to be a data type, got {:?} instead",
                self.current_token.token_type
            )),
        }
    }

    /// **Parses a let statement.**
    ///
    /// `let ident: type = expr;`
    fn parse_let_statement(&mut self) -> ParseResult<Statement> {
        if !self.expect_token(Tokens::IDENT(String::new())) {
            return Err("expected next token to be an identifier".to_string());
        }

        if let Tokens::IDENT(ident) = self.current_token.token_type.clone() {
            if !self.expect_token(Tokens::Colon) {
                return Err("expected next token to be a colon".to_string());
            }

            if !self.expect_token(Tokens::IDENT(String::new())) {
                return Err("expected next token to be a data type".to_string());
            }

            if let Tokens::IDENT(_) = &self.current_token.token_type {
                let data_type = self.parse_data_type().unwrap(); // TODO: Handle error.

                if !self.expect_token(Tokens::Assign) {
                    return Err("expected next token to be an equals sign".to_string());
                }

                if let Ok(expression) = self.parse_expression(Priority::Lowest) {
                    if self.expect_token(Tokens::Semicolon) {
                        return Ok(Statement::LetStatement(LetStatement::new(
                            data_type,
                            Identifier::new(ident.clone()),
                            expression,
                        )));
                    }
                }
            }
        }

        Err("expected next token to be a semicolon".to_string())
    }

    /// **Parses a return statement.**
    fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        Err("not implemented".to_string())
    }

    /// **Parses an expression statement.**
    fn parse_expression_statement(&mut self) -> ParseResult<Statement> {
        Err("not implemented".to_string())
    }

    /// **Parses an expression.**
    fn parse_expression(&mut self, priority: Priority) -> ParseResult<Expression> {
        let mut left_expression = match self.current_token.token_type.clone() {
            Tokens::IDENT(ident) => Ok(Expression::Identifier(Identifier::new(ident.clone()))),
            Tokens::Number(number) => Ok(Expression::NumberLiteral(NumberLiteral::new(number))),
            Tokens::String(string) => Ok(Expression::StringLiteral(StringLiteral::new(string))),
            Tokens::Boolean(boolean) => Ok(Expression::BooleanLiteral(BooleanLiteral::new(boolean))),
            Tokens::Bang | Tokens::Minus => Ok(Expression::PrefixExpression(PrefixExpression::new(
                self.current_token.token_type.clone(),
                Box::new(self.parse_expression(Priority::Prefix).unwrap()),
            ))),
            Tokens::LParen => {
                self.next_token();

                let expression = self.parse_expression(Priority::Lowest);

                if !self.expect_token(Tokens::RParen) {
                    return Err("expected next token to be a closing parenthesis".to_string());
                }

                expression
            }
            // TODO: Add IfExpression, FunctionExpression, CallExpression, IndexExpression, HashExpression ...
            _ => Err(format!(
                "expected next token to be an expression, got {:?} instead",
                self.current_token.token_type
            )),
        };

        if let Err(e) = left_expression {
            if self.current_token.token_type != Tokens::Semicolon {
                self.errors.push(format!("unexpected token: {:?}", self.current_token.token_type));
            }

            return Err(e);
        }

        while !self.peek_token(Tokens::Semicolon) && priority < self.peek_priority() {
            self.next_token();

            left_expression = match self.current_token.token_type.clone() {
                Tokens::Plus | Tokens::Minus | Tokens::Slash | Tokens::Asterisk | Tokens::EQ | Tokens::NEQ | Tokens::LT | Tokens::GT => {
                    Ok(Expression::InfixExpression(InfixExpression::new(
                        Box::new(left_expression.unwrap()),
                        self.current_token.token_type.clone(),
                        Box::new(self.parse_expression(self.current_priority()).unwrap()),
                    )))
                }
                // Todo: LParen, LBracket ...
                _ => Err(format!(
                    "expected next token to be an infix operator, got {:?} instead",
                    self.current_token.token_type
                )),
            };
        }

        left_expression
    }
}
