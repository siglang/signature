pub mod ast;
pub mod helpers;
pub mod tokenizer;

use crate::{
    ast::*,
    tokenizer::{Lexer, Token, TokenKind},
    Position,
};

#[derive(Debug, Clone, PartialEq)]
pub struct ParsingError {
    pub message: ParsingErrorKind,
    pub position: Position,
}

impl ParsingError {
    pub fn new(message: ParsingErrorKind, position: Position) -> Self {
        Self { message, position }
    }

    pub fn expected_next_token<T>(expected: T, got: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            ParsingErrorKind::ExpectedNextToken(expected.to_string(), got.to_string()),
            position,
        )
    }

    pub fn expected_data_type<T>(expected: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            ParsingErrorKind::ExpectedDataType(expected.to_string()),
            position,
        )
    }

    pub fn expected_expression<T>(expected: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            ParsingErrorKind::ExpectedExpression(expected.to_string()),
            position,
        )
    }

    pub fn unexpected_token<T>(token: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            ParsingErrorKind::UnexpectedToken(token.to_string()),
            position,
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
#[rustfmt::skip]
pub enum ParsingErrorKind {
    ExpectedNextToken(String, String),
    ExpectedDataType(String),
    ExpectedExpression(String),
    UnexpectedToken(String),
}

pub type ParseResult<T> = Result<T, ParsingError>;

/// # Parser
///
/// The parser is responsible for parsing the tokens from the lexer into an AST.
///
/// ### Example
///
/// ```rs
/// use parser::{tokenizer::Lexer, Parser};
///
/// let lexer = Lexer::new("let x: number = 5;");
/// let mut parser = Parser::new(lexer);
/// let ast = parser.parse_program();
///
/// match parser.parse_program() {
///     Ok(ast) => {
///         println!("{ast:#?}");
///     }
///     Err(errors) => {
///         for error in errors {
///             println!("{error}");
///         }
///     }
/// }
#[derive(Debug, Default)]
pub struct Parser<'a> {
    pub lexer: Lexer<'a>,
    pub errors: Vec<ParsingError>,
    current_token: Token,
    peek_token: Token,
    position: Position,
    previous_statement: Option<Statement>,
}

impl<'a> From<&'a str> for Parser<'a> {
    fn from(x: &'a str) -> Self {
        Parser::new(Lexer::new(x))
    }
}

impl<'a> Parser<'a> {
    /// Creates a new `Parser` from a lexer.
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer,
            ..Default::default()
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();

        self.position = Position(self.current_token.position.0, self.current_token.position.1);
    }

    fn expect_token(&mut self, token_type: &TokenKind) -> ParseResult<()> {
        if self.current_token.kind == *token_type {
            self.next_token();

            Ok(())
        } else {
            Err(ParsingError::expected_expression(
                self.current_token.kind.to_string(),
                self.position,
            ))
        }
    }

    fn peek_token(&self, token_type: &TokenKind) -> bool {
        self.peek_token.kind == *token_type
    }

    fn get_priority(&self, token_type: &TokenKind) -> Priority {
        match token_type {
            TokenKind::Dot | TokenKind::Arrow => Priority::Dot,
            TokenKind::Assign | TokenKind::EQ | TokenKind::NEQ => Priority::Equals,
            TokenKind::Plus | TokenKind::Minus => Priority::Sum,
            TokenKind::Slash | TokenKind::Asterisk => Priority::Product,
            TokenKind::LT | TokenKind::GT | TokenKind::LTE | TokenKind::GTE => {
                Priority::LessGreater
            }
            TokenKind::LParen => Priority::Call,
            TokenKind::LBracket => Priority::Index,
            _ => Priority::Lowest,
        }
    }

    fn peek_priority(&mut self) -> Priority {
        self.get_priority(&self.peek_token.kind)
    }

    fn current_priority(&self) -> Priority {
        self.get_priority(&self.current_token.kind)
    }

    /// Parses the tokens from the lexer into an AST.
    ///
    /// if there are any errors, they will be returned as a `Vec<ParsingError>`.
    pub fn parse_program(&mut self) -> Result<Program, Vec<ParsingError>> {
        self.next_token();
        self.next_token();

        let mut program = Program::default();

        while self.current_token.kind != TokenKind::EOF {
            match self.parse_statement() {
                Ok(statement) => program.push(statement),
                Err(error) => self.errors.push(error),
            }

            self.next_token();
        }

        if !self.errors.is_empty() {
            Err(self.errors.clone())
        } else {
            Ok(program)
        }
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        if let Some(Statement::ReturnExpressionStatement(_)) = self.previous_statement {
            return Err(ParsingError::unexpected_token(
                self.current_token.kind.to_string(),
                self.position,
            ));
        }

        let statement = match self.current_token.kind {
            TokenKind::Let => Statement::LetStatement(self.parse_let_statement(false)?),
            TokenKind::Mut => Statement::LetStatement(self.parse_let_statement(true)?),
            TokenKind::Return => Statement::ReturnStatement(self.parse_return_statement()?),
            TokenKind::Type => Statement::TypeStatement(self.parse_type_statement()?),
            TokenKind::Declare => Statement::DeclareStatement(self.parse_declare_statement()?),
            TokenKind::Struct => Statement::StructStatement(self.parse_struct_statement()?),
            _ => self.parse_expression_statement()?,
        };

        self.previous_statement = Some(statement.clone());
        Ok(statement)
    }

    fn parse_let_statement(&mut self, is_mutable: bool) -> ParseResult<LetStatement> {
        self.next_token();

        let ident = Identifier {
            value: ident_token_to_string! { self },
            position: self.position,
        };
        self.next_token();

        let data_type = if self.current_token.kind == TokenKind::Colon {
            self.next_token();
            Some(self.parse_data_type()?)
        } else {
            None
        };

        self.expect_token(&TokenKind::Assign)?;

        if let Ok(expression) = self.parse_expression(&Priority::Lowest) {
            return if self.peek_token(&TokenKind::Semicolon) {
                self.next_token();

                Ok(LetStatement {
                    identifier: ident,
                    value: expression,
                    data_type,
                    is_mutable,
                    position: self.position,
                })
            } else {
                Err(ParsingError::expected_next_token(
                    TokenKind::Semicolon.to_string(),
                    self.current_token.kind.to_string(),
                    self.position,
                ))
            };
        }

        Err(ParsingError::unexpected_token(
            self.current_token.kind.to_string(),
            self.position,
        ))
    }

    fn parse_return_statement(&mut self) -> ParseResult<ReturnStatement> {
        self.next_token();

        if let Ok(expression) = self.parse_expression(&Priority::Lowest) {
            return if self.peek_token(&TokenKind::Semicolon) {
                self.next_token();

                Ok(ReturnStatement {
                    value: expression,
                    position: self.position,
                })
            } else {
                Err(ParsingError::expected_next_token(
                    TokenKind::Semicolon.to_string(),
                    self.current_token.kind.to_string(),
                    self.position,
                ))
            };
        }

        Err(ParsingError::unexpected_token(
            self.current_token.kind.to_string(),
            self.position,
        ))
    }

    fn parse_type_statement(&mut self) -> ParseResult<TypeStatement> {
        self.next_token();

        let ident = ident_token_to_string! { self };
        self.next_token();

        let generics = if self.current_token.kind == TokenKind::LT {
            let result = self.parse_generic_identifier()?;
            self.next_token();
            result
        } else {
            Vec::new()
        };

        self.expect_token(&TokenKind::Assign)?;

        let data_type = self.parse_data_type()?;

        if self.current_token.kind == TokenKind::Semicolon {
            Ok(TypeStatement {
                identifier: Identifier {
                    value: ident,
                    position: self.position,
                },
                data_type,
                generics,
                position: self.position,
            })
        } else {
            Err(ParsingError::expected_next_token(
                TokenKind::Semicolon.to_string(),
                self.current_token.kind.to_string(),
                self.position,
            ))
        }
    }

    fn parse_declare_statement(&mut self) -> ParseResult<DeclareStatement> {
        self.next_token();

        let ident = ident_token_to_string! { self };
        self.next_token();

        self.expect_token(&TokenKind::Assign)?;

        let data_type = self.parse_data_type()?;

        if self.current_token.kind == TokenKind::Semicolon {
            Ok(DeclareStatement {
                identifier: Identifier {
                    value: ident,
                    position: self.position,
                },
                data_type,
                position: self.position,
            })
        } else {
            Err(ParsingError::expected_next_token(
                TokenKind::Semicolon.to_string(),
                self.current_token.kind.to_string(),
                self.position,
            ))
        }
    }

    fn parse_struct_statement(&mut self) -> ParseResult<StructStatement> {
        self.next_token();

        let ident = ident_token_to_string! { self };
        self.next_token();

        let generics = if self.current_token.kind == TokenKind::LT {
            let generic = self.parse_generic_identifier()?;

            self.next_token();

            generic
        } else {
            Vec::new()
        };

        self.expect_token(&TokenKind::LBrace)?;

        let mut fields = Vec::new();

        while self.current_token.kind != TokenKind::RBrace {
            let key = Identifier {
                value: ident_token_to_string! { self },
                position: self.position,
            };
            self.next_token();

            self.expect_token(&TokenKind::Colon)?;

            let value = self.parse_data_type()?;

            fields.push((key, value));

            if self.current_token.kind == TokenKind::RBrace {
                break;
            }

            self.expect_token(&TokenKind::Comma)?;
        }

        self.expect_token(&TokenKind::RBrace)?;

        Ok(StructStatement {
            identifier: Identifier {
                value: ident,
                position: self.position,
            },
            generics,
            fields,
            position: self.position,
        })
    }

    fn parse_expression_statement(&mut self) -> ParseResult<Statement> {
        let expression = self.parse_expression(&Priority::Lowest)?;

        if self.peek_token(&TokenKind::Semicolon) {
            self.next_token();

            Ok(Statement::ExpressionStatement(ExpressionStatement {
                expression,
                position: self.position,
            }))
        } else {
            Ok(Statement::ReturnExpressionStatement(
                ReturnExpressionStatement {
                    value: expression,
                    position: self.position,
                },
            ))
        }
    }

    fn parse_expression(&mut self, priority: &Priority) -> ParseResult<Expression> {
        let left_expression = match self.current_token.kind.clone() {
            TokenKind::IDENT(value) => {
                Some(Ok(Expression::Literal(Literal::Identifier(Identifier {
                    value,
                    position: self.position,
                }))))
            }
            TokenKind::Number(value) => Some(Ok(Expression::Literal(Literal::NumberLiteral(
                NumberLiteral {
                    value,
                    position: self.position,
                },
            )))),
            TokenKind::String(value) => Some(Ok(Expression::Literal(Literal::StringLiteral(
                StringLiteral {
                    value,
                    position: self.position,
                },
            )))),
            TokenKind::Boolean(value) => Some(Ok(Expression::Literal(Literal::BooleanLiteral(
                BooleanLiteral {
                    value,
                    position: self.position,
                },
            )))),
            TokenKind::Bang | TokenKind::Minus => {
                let operator: PrefixOperator = self.current_token.kind.clone().into();
                self.next_token();

                Some(Ok(Expression::PrefixExpression(PrefixExpression {
                    operator,
                    right: Box::new(self.parse_expression(&Priority::Prefix)?),
                    position: self.position,
                })))
            }
            TokenKind::LParen => {
                self.next_token();

                let expression = self.parse_expression(&Priority::Lowest);
                self.next_token();

                if self.current_token.kind != TokenKind::RParen {
                    return Err(ParsingError::expected_next_token(
                        TokenKind::RParen.to_string(),
                        self.current_token.kind.to_string(),
                        self.position,
                    ));
                }

                Some(expression)
            }
            TokenKind::LBrace => Some(Ok(Expression::BlockExpression(
                self.parse_block_expression()?,
            ))),
            TokenKind::LBracket => Some(Ok(Expression::Literal(Literal::ArrayLiteral(
                self.parse_array_literal()?,
            )))),
            TokenKind::Function => Some(Ok(Expression::Literal(Literal::FunctionLiteral(
                self.parse_function_literal()?,
            )))),
            TokenKind::Struct => Some(Ok(Expression::Literal(Literal::StructLiteral(
                self.parse_struct_literal()?,
            )))),
            TokenKind::If => Some(Ok(Expression::IfExpression(self.parse_if_expression()?))),
            TokenKind::Typeof => {
                self.next_token();

                Some(Ok(Expression::TypeofExpression(TypeofExpression {
                    expression: Box::new(self.parse_expression(&Priority::Lowest)?),
                    position: self.position,
                })))
            }
            TokenKind::Debug => {
                self.next_token();

                Some(Ok(Expression::Debug(
                    Box::new(self.parse_expression(&Priority::Lowest)?),
                    self.position,
                )))
            }
            _ => None,
        };

        if left_expression.is_none() && self.current_token.kind != TokenKind::Semicolon {
            return Err(ParsingError::unexpected_token(
                self.current_token.kind.to_string(),
                self.position,
            ));
        }

        let mut left_expression = left_expression.ok_or_else(|| {
            ParsingError::unexpected_token(self.current_token.kind.to_string(), self.position)
        })?;

        if self.peek_token(&TokenKind::Assign) {
            let identifier = Identifier {
                value: ident_token_to_string! { self },
                position: self.position,
            };
            self.next_token();
            self.next_token();

            let expression = self.parse_expression(&Priority::Lowest)?;
            return Ok(Expression::AssignmentExpression(AssignmentExpression {
                identifier,
                value: Box::new(expression),
                position: self.position,
            }));
        }

        while !self.peek_token(&TokenKind::Semicolon) && priority < &self.peek_priority() {
            self.next_token();

            left_expression = match self.current_token.kind {
                TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Dot
                | TokenKind::Slash
                | TokenKind::Asterisk
                | TokenKind::Percent
                | TokenKind::EQ
                | TokenKind::NEQ
                | TokenKind::LT
                | TokenKind::GT
                | TokenKind::LTE
                | TokenKind::GTE => {
                    let operator: InfixOperator = self.current_token.kind.clone().into();

                    let priority = self.current_priority();
                    self.next_token();
                    let right = Box::new(self.parse_expression(&priority)?);

                    Ok(Expression::InfixExpression(InfixExpression {
                        left: Box::new(left_expression?),
                        operator,
                        right,
                        position: self.position,
                    }))
                }
                TokenKind::LParen => {
                    self.next_token();

                    let mut arguments = Vec::new();

                    if self.current_token.kind != TokenKind::RParen {
                        arguments.push(self.parse_expression(&Priority::Lowest)?);
                        self.next_token();

                        if self.current_token.kind == TokenKind::Comma {
                            self.next_token();
                        }

                        while self.current_token.kind != TokenKind::RParen {
                            arguments.push(self.parse_expression(&Priority::Lowest)?);
                            self.next_token();

                            if self.current_token.kind == TokenKind::RParen {
                                break;
                            }

                            self.expect_token(&TokenKind::Comma)?;
                        }

                        if self.current_token.kind != TokenKind::RParen {
                            return Err(ParsingError::expected_next_token(
                                TokenKind::RParen.to_string(),
                                self.current_token.kind.to_string(),
                                self.position,
                            ));
                        }
                    }

                    Ok(Expression::CallExpression(CallExpression {
                        function: Box::new(left_expression?),
                        arguments,
                        position: self.position,
                    }))
                }
                TokenKind::LBracket => {
                    self.next_token();

                    let index = self.parse_expression(&Priority::Lowest)?;
                    self.next_token();

                    if self.current_token.kind != TokenKind::RBracket {
                        return Err(ParsingError::expected_next_token(
                            TokenKind::RBracket.to_string(),
                            self.current_token.kind.to_string(),
                            self.position,
                        ));
                    }

                    Ok(Expression::IndexExpression(IndexExpression {
                        left: Box::new(left_expression?),
                        index: Box::new(index),
                        position: self.position,
                    }))
                }
                _ => Err(ParsingError::unexpected_token(
                    self.current_token.kind.to_string(),
                    self.position,
                )),
            };
        }

        left_expression
    }

    fn parse_block_expression(&mut self) -> ParseResult<BlockExpression> {
        self.next_token();

        let mut statements = Vec::new();

        while self.current_token.kind != TokenKind::RBrace {
            statements.push(self.parse_statement()?);
            self.next_token();
        }

        Ok(BlockExpression {
            statements,
            position: self.position,
        })
    }

    fn parse_array_literal(&mut self) -> ParseResult<ArrayLiteral> {
        self.next_token();

        let mut elements = Vec::new();

        if self.current_token.kind == TokenKind::RBracket {
            return Ok(ArrayLiteral {
                elements,
                position: self.position,
            });
        }

        while self.current_token.kind != TokenKind::RBrace {
            elements.push(self.parse_expression(&Priority::Lowest)?);
            self.next_token();

            if self.current_token.kind == TokenKind::RBracket {
                break;
            }

            self.expect_token(&TokenKind::Comma)?;
        }

        if self.current_token.kind != TokenKind::RBracket {
            return Err(ParsingError::expected_next_token(
                TokenKind::RBracket.to_string(),
                self.current_token.kind.to_string(),
                self.position,
            ));
        }

        Ok(ArrayLiteral {
            elements,
            position: self.position,
        })
    }

    fn parse_struct_literal(&mut self) -> ParseResult<StructLiteral> {
        self.next_token();
        let identifier = ident_token_to_string! { self };

        self.next_token();
        self.next_token();

        let mut fields = Vec::new();

        while self.current_token.kind != TokenKind::RBrace {
            let key = Identifier {
                value: ident_token_to_string! { self },
                position: self.position,
            };
            self.next_token();

            self.expect_token(&TokenKind::Colon)?;

            let value = self.parse_expression(&Priority::Lowest)?;
            self.next_token();

            fields.push((key, value));

            if self.current_token.kind == TokenKind::RBrace {
                break;
            }

            self.expect_token(&TokenKind::Comma)?;
        }

        if self.current_token.kind != TokenKind::RBrace {
            return Err(ParsingError::expected_next_token(
                TokenKind::RBrace.to_string(),
                self.current_token.kind.to_string(),
                self.position,
            ));
        }

        Ok(StructLiteral {
            identifier: Identifier {
                value: identifier,
                position: self.position,
            },
            fields,
            position: self.position,
        })
    }

    fn parse_function_literal(&mut self) -> ParseResult<FunctionLiteral> {
        self.next_token();

        let generics = if self.current_token.kind == TokenKind::LT {
            let result = Some(self.parse_generic_identifier()?);
            self.next_token();

            result
        } else {
            None
        };

        self.expect_token(&TokenKind::LParen)?;

        let mut parameters = Vec::new();

        while self.current_token.kind != TokenKind::RParen {
            let parameter_kind = if self.current_token.kind == TokenKind::Spread {
                self.next_token();
                ParameterKind::Spread
            } else {
                ParameterKind::Normal
            };

            if let TokenKind::IDENT(identifier) = self.current_token.kind.clone() {
                self.next_token();
                self.expect_token(&TokenKind::Colon)?;

                let data_type = self.parse_data_type()?;

                parameters.push(Parameter {
                    identifier: Identifier {
                        value: identifier.to_string(),
                        position: self.position,
                    },
                    data_type,
                    kind: parameter_kind,
                    position: self.position,
                });
            } else {
                return Err(ParsingError::expected_next_token(
                    TokenKind::IDENT(String::new()).to_string(),
                    self.current_token.kind.to_string(),
                    self.position,
                ));
            }

            if self.current_token.kind == TokenKind::RParen {
                break;
            }

            self.expect_token(&TokenKind::Comma)?;
        }

        self.expect_token(&TokenKind::RParen)?;
        self.expect_token(&TokenKind::Arrow)?;

        let return_type = self.parse_data_type()?;

        let body = match self.current_token.kind {
            TokenKind::LBrace => self.parse_block_expression()?,
            TokenKind::DoubleArrow => {
                self.next_token();

                BlockExpression {
                    statements: vec![Statement::ReturnStatement(ReturnStatement {
                        value: self.parse_expression(&Priority::Lowest)?,
                        position: self.position,
                    })],
                    position: self.position,
                }
            }
            _ => {
                return Err(ParsingError::expected_next_token(
                    TokenKind::LBrace.to_string(),
                    self.current_token.kind.to_string(),
                    self.position,
                ))
            }
        };

        Ok(FunctionLiteral {
            generics,
            parameters,
            return_type,
            body,
            position: self.position,
        })
    }

    fn parse_if_expression(&mut self) -> ParseResult<IfExpression> {
        self.next_token();

        let condition = self.parse_expression(&Priority::Lowest)?;
        self.next_token();

        let consequence = self.parse_block_expression()?;

        let alternative = if self.peek_token.kind == TokenKind::Else {
            self.next_token();
            self.next_token();

            if self.current_token.kind == TokenKind::If {
                Some(Box::new(BlockExpression {
                    statements: vec![Statement::ExpressionStatement(ExpressionStatement {
                        expression: Expression::IfExpression(self.parse_if_expression()?),
                        position: self.position,
                    })],
                    position: self.position,
                }))
            } else {
                Some(Box::new(self.parse_block_expression()?))
            }
        } else {
            None
        };

        Ok(IfExpression {
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative,
            position: self.position,
        })
    }

    fn parse_data_type(&mut self) -> ParseResult<DataType> {
        let position = self.position;

        let result = self.parse_data_type_without_next();
        self.next_token();

        result.map(|data_type| DataType {
            kind: data_type,
            position,
        })
    }

    fn parse_data_type_without_next(&mut self) -> ParseResult<DataTypeKind> {
        let mut data_type = match self.current_token.kind {
            TokenKind::NumberType => Ok(DataTypeKind::Number),
            TokenKind::StringType => Ok(DataTypeKind::String),
            TokenKind::BooleanType => Ok(DataTypeKind::Boolean),
            TokenKind::Function => Ok(DataTypeKind::Fn(self.parse_function_type()?)),
            TokenKind::IDENT(ref ident) => Ok(DataTypeKind::Custom(ident.to_string())),
            _ => Err(ParsingError::expected_next_token(
                TokenKind::NumberType.to_string(),
                self.current_token.kind.to_string(),
                self.position,
            )),
        };

        if self.peek_token(&TokenKind::LT) {
            let generics = self.parse_generic()?;

            data_type = Ok(DataTypeKind::Generic(generics));
        }

        while self.peek_token(&TokenKind::LBracket) {
            self.next_token();
            self.next_token();

            if self.current_token.kind != TokenKind::RBracket {
                return Err(ParsingError::expected_next_token(
                    TokenKind::RBracket.to_string(),
                    self.current_token.kind.to_string(),
                    self.position,
                ));
            }

            data_type = data_type.map(|data_type| {
                DataTypeKind::Array(Box::new(DataType {
                    kind: data_type,
                    position: self.position,
                }))
            });
        }

        data_type
    }

    fn parse_function_type(&mut self) -> ParseResult<FunctionType> {
        self.next_token();

        let generics = if self.current_token.kind == TokenKind::LT {
            let result = Some(self.parse_generic_identifier()?);
            self.next_token();

            result
        } else {
            None
        };

        self.expect_token(&TokenKind::LParen)?;

        let mut parameters = Vec::new();

        while self.current_token.kind != TokenKind::RParen {
            let kind = if self.current_token.kind == TokenKind::Spread {
                self.next_token();
                ParameterKind::Spread
            } else {
                ParameterKind::Normal
            };

            parameters.push((self.parse_data_type()?, kind));

            if self.current_token.kind == TokenKind::RParen {
                break;
            }

            self.expect_token(&TokenKind::Comma)?;
        }

        self.expect_token(&TokenKind::RParen)?;
        self.expect_token(&TokenKind::Arrow)?;

        let return_type = self.parse_data_type_without_next()?;

        Ok(FunctionType {
            generics,
            parameters,
            return_type: Box::new(DataType {
                kind: return_type,
                position: self.position,
            }),
            position: self.position,
        })
    }

    fn parse_generic(&mut self) -> ParseResult<Generic> {
        let ident = ident_token_to_string! { self };
        self.next_token();

        let mut generics = Vec::new();

        self.expect_token(&TokenKind::LT)?;

        while self.current_token.kind != TokenKind::GT {
            let data_type = self.parse_data_type()?;

            generics.push(data_type);

            if self.current_token.kind == TokenKind::GT {
                break;
            }

            self.expect_token(&TokenKind::Comma)?;
        }

        Ok(Generic::new(
            DataType {
                kind: DataTypeKind::Custom(ident),
                position: self.position,
            },
            generics,
        ))
    }

    fn parse_generic_identifier(&mut self) -> ParseResult<IdentifierGeneric> {
        let mut generics = Vec::new();

        self.expect_token(&TokenKind::LT)?;

        while self.current_token.kind != TokenKind::GT {
            let ident = ident_token_to_string! { self };
            self.next_token();

            generics.push(Identifier {
                value: ident,
                position: self.position,
            });

            if self.current_token.kind == TokenKind::GT {
                break;
            }

            self.expect_token(&TokenKind::Comma)?;
        }

        Ok(generics)
    }
}
