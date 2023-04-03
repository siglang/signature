use crate::{
    identifier,
    ast::*, ParsingError, ParsingErrorKind, Position,
    tokenizer::{Lexer, Token, TokenKind},
};

pub type ParseResult<T> = Result<T, ParsingError>;

#[derive(Debug, Default)]
pub struct Parser<'a> {
    pub lexer: Lexer<'a>,
    pub current_token: Token,
    pub peek_token: Token,
    pub position: Position,
    pub errors: Vec<ParsingError>,
}

impl<'a> From<&'a str> for Parser<'a> {
    fn from(x: &'a str) -> Self {
        Parser::new(Lexer::new(x))
    }
}

impl<'a> Parser<'a> {
    #[inline]
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer,
            ..Default::default()
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();

        self.position = Position::new(self.current_token.position.0, self.current_token.position.1);
    }

    fn expect_token(&mut self, token_type: &TokenKind) -> ParseResult<()> {
        if self.current_token.kind == *token_type {
            self.next_token();

            Ok(())
        } else {
            Err(ParsingError::new(
                ParsingErrorKind::ExpectedExpression(self.current_token.kind.to_string()),
                self.position,
            ))
        }
    }

    #[inline]
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

    pub fn parse_program(&mut self) -> Program {
        self.next_token();
        self.next_token();

        let mut program = Program::default();

        while self.current_token.kind != TokenKind::EOF {
            match self.parse_statement() {
                Ok(statement) => program.statements.push(statement),
                Err(error) => self.errors.push(error),
            }

            self.next_token();
        }

        program.errors = self.errors.clone();

        if !self.errors.is_empty() {
            program.statements = Vec::new();
        }

        program
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        Ok(match self.current_token.kind {
            TokenKind::Let => Statement::LetStatement(self.parse_let_statement()?),
            TokenKind::Auto => Statement::AutoStatement(self.parse_auto_statement()?),
            TokenKind::Return => Statement::ReturnStatement(self.parse_return_statement()?),
            TokenKind::Type => Statement::TypeStatement(self.parse_type_statement()?),
            TokenKind::Declare => Statement::DeclareStatement(self.parse_declare_statement()?),
            TokenKind::Struct => Statement::StructStatement(self.parse_struct_statement()?),
            _ => Statement::ExpressionStatement(self.parse_expression_statement()?),
        })
    }

    fn parse_let_statement(&mut self) -> ParseResult<LetStatement> {
        self.next_token();

        let ident = Identifier {
            value: identifier! { self },
            position: self.position,
        };
        self.next_token();

        self.expect_token(&TokenKind::Colon)?;

        let data_type = self.parse_data_type()?;

        self.expect_token(&TokenKind::Assign)?;

        if let Ok(expression) = self.parse_expression(&Priority::Lowest) {
            return if self.peek_token(&TokenKind::Semicolon) {
                self.next_token();

                Ok(LetStatement {
                    identifier: ident,
                    data_type,
                    value: expression,
                    position: self.position,
                })
            } else {
                Err(ParsingError::new(
                    ParsingErrorKind::ExpectedNextToken(
                        TokenKind::Semicolon.to_string(),
                        self.current_token.kind.to_string(),
                    ),
                    self.position,
                ))
            };
        }

        Err(ParsingError::new(
            ParsingErrorKind::UnexpectedToken(self.current_token.kind.to_string()),
            self.position,
        ))
    }

    fn parse_auto_statement(&mut self) -> ParseResult<AutoStatement> {
        self.next_token();

        let ident = Identifier {
            value: identifier! { self },
            position: self.position,
        };
        self.next_token();

        self.expect_token(&TokenKind::Assign)?;

        if let Ok(expression) = self.parse_expression(&Priority::Lowest) {
            return if self.peek_token(&TokenKind::Semicolon) {
                self.next_token();

                Ok(AutoStatement {
                    identifier: ident,
                    value: expression,
                    position: self.position,
                })
            } else {
                Err(ParsingError::new(
                    ParsingErrorKind::ExpectedNextToken(
                        TokenKind::Semicolon.to_string(),
                        self.current_token.kind.to_string(),
                    ),
                    self.position,
                ))
            };
        }

        Err(ParsingError::new(
            ParsingErrorKind::UnexpectedToken(self.current_token.kind.to_string()),
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
                Err(ParsingError::new(
                    ParsingErrorKind::ExpectedNextToken(
                        TokenKind::Semicolon.to_string(),
                        self.current_token.kind.to_string(),
                    ),
                    self.position,
                ))
            };
        }

        Err(ParsingError::new(
            ParsingErrorKind::UnexpectedToken(self.current_token.kind.to_string()),
            self.position,
        ))
    }

    fn parse_type_statement(&mut self) -> ParseResult<TypeStatement> {
        self.next_token();

        let ident = identifier! { self };
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
            Err(ParsingError::new(
                ParsingErrorKind::ExpectedNextToken(
                    TokenKind::Semicolon.to_string(),
                    self.current_token.kind.to_string(),
                ),
                self.position,
            ))
        }
    }

    fn parse_declare_statement(&mut self) -> ParseResult<DeclareStatement> {
        self.next_token();

        let ident = identifier! { self };
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
            Err(ParsingError::new(
                ParsingErrorKind::ExpectedNextToken(
                    TokenKind::Semicolon.to_string(),
                    self.current_token.kind.to_string(),
                ),
                self.position,
            ))
        }
    }

    fn parse_struct_statement(&mut self) -> ParseResult<StructStatement> {
        self.next_token();

        let ident = identifier! { self };
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
                value: identifier! { self },
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

    fn parse_expression_statement(&mut self) -> ParseResult<ExpressionStatement> {
        let expression = self.parse_expression(&Priority::Lowest)?;

        if self.peek_token(&TokenKind::Semicolon) {
            self.next_token();

            Ok(ExpressionStatement {
                expression,
                position: self.position,
            })
        } else {
            Err(ParsingError::new(
                ParsingErrorKind::ExpectedNextToken(
                    TokenKind::Semicolon.to_string(),
                    self.current_token.kind.to_string(),
                ),
                self.position,
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
                let operator = self.current_token.kind.clone();

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
                    return Err(ParsingError::new(
                        ParsingErrorKind::ExpectedNextToken(
                            TokenKind::RParen.to_string(),
                            self.current_token.kind.to_string(),
                        ),
                        self.position,
                    ));
                }

                Some(expression)
            }
            TokenKind::LBrace => Some(Ok(Expression::BlockExpression(
                self.parse_block_expression()?,
            ))),
            TokenKind::RBracket => Some(Ok(Expression::Literal(Literal::ArrayLiteral(
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
            _ => None,
        };

        if left_expression.is_none() && self.current_token.kind != TokenKind::Semicolon {
            return Err(ParsingError::new(
                ParsingErrorKind::UnexpectedToken(self.current_token.kind.to_string()),
                self.position,
            ));
        }

        let mut left_expression = left_expression.ok_or_else(|| {
            ParsingError::new(
                ParsingErrorKind::UnexpectedToken(self.current_token.kind.to_string()),
                self.position,
            )
        })?;

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
                | TokenKind::GTE => Ok(Expression::InfixExpression(InfixExpression {
                    left: Box::new(left_expression?),
                    operator: self.current_token.kind.clone(),
                    right: {
                        self.next_token();
                        Box::new(self.parse_expression(&self.current_priority())?)
                    },
                    position: self.position,
                })),
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
                            return Err(ParsingError::new(
                                ParsingErrorKind::ExpectedNextToken(
                                    TokenKind::RParen.to_string(),
                                    self.current_token.kind.to_string(),
                                ),
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
                        return Err(ParsingError::new(
                            ParsingErrorKind::ExpectedNextToken(
                                TokenKind::RBracket.to_string(),
                                self.current_token.kind.to_string(),
                            ),
                            self.position,
                        ));
                    }

                    Ok(Expression::IndexExpression(IndexExpression {
                        left: Box::new(left_expression?),
                        index: Box::new(index),
                        position: self.position,
                    }))
                }
                _ => Err(ParsingError::new(
                    ParsingErrorKind::UnexpectedToken(self.current_token.kind.to_string()),
                    self.position,
                )),
            };
        }

        match left_expression.clone()? {
            Expression::InfixExpression(infix) => {
                if let Some(e) = self.eval_infix_expression(&infix) {
                    return e;
                };
            }
            Expression::PrefixExpression(prefix) => {
                if let Some(e) = self.eval_prefix_expression(&prefix) {
                    return e;
                };
            }
            _ => {}
        };

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
            return Err(ParsingError::new(
                ParsingErrorKind::ExpectedNextToken(
                    TokenKind::RBracket.to_string(),
                    self.current_token.kind.to_string(),
                ),
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
        let identifier = identifier! { self };

        self.next_token();
        self.next_token();

        let mut fields = Vec::new();

        while self.current_token.kind != TokenKind::RBrace {
            let key = Identifier {
                value: identifier! { self },
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
            return Err(ParsingError::new(
                ParsingErrorKind::ExpectedNextToken(
                    TokenKind::RBrace.to_string(),
                    self.current_token.kind.to_string(),
                ),
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
                return Err(ParsingError::new(
                    ParsingErrorKind::ExpectedNextToken(
                        TokenKind::IDENT(String::new()).to_string(),
                        self.current_token.kind.to_string(),
                    ),
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
                return Err(ParsingError::new(
                    ParsingErrorKind::ExpectedNextToken(
                        TokenKind::LBrace.to_string(),
                        self.current_token.kind.to_string(),
                    ),
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
            data_type,
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
            _ => Err(ParsingError::new(
                ParsingErrorKind::ExpectedNextToken(
                    TokenKind::NumberType.to_string(),
                    self.current_token.kind.to_string(),
                ),
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
                return Err(ParsingError::new(
                    ParsingErrorKind::ExpectedNextToken(
                        TokenKind::RBracket.to_string(),
                        self.current_token.kind.to_string(),
                    ),
                    self.position,
                ));
            }

            data_type = data_type.map(|data_type| {
                DataTypeKind::Array(Box::new(DataType {
                    data_type,
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
                data_type: return_type,
                position: self.position,
            }),
            position: self.position,
        })
    }

    fn parse_generic(&mut self) -> ParseResult<Generic> {
        let ident = identifier! { self };
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
                data_type: DataTypeKind::Custom(ident),
                position: self.position,
            },
            generics,
        ))
    }

    fn parse_generic_identifier(&mut self) -> ParseResult<IdentifierGeneric> {
        let mut generics = Vec::new();

        self.expect_token(&TokenKind::LT)?;

        while self.current_token.kind != TokenKind::GT {
            let ident = identifier! { self };
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

    fn eval_expression(&mut self, expression: &Expression) -> Option<ParseResult<Expression>> {
        match expression {
            Expression::InfixExpression(infix) => self.eval_infix_expression(infix),
            Expression::PrefixExpression(prefix) => self.eval_prefix_expression(prefix),
            e => Some(Ok(e.clone())),
        }
    }

    // TODO
    fn eval_infix_expression(
        &mut self,
        infix: &InfixExpression,
    ) -> Option<ParseResult<Expression>> {
        self.eval_infix_expression_opt(infix)
    }

    // TODO
    fn eval_infix_expression_opt(
        &mut self,
        infix: &InfixExpression,
    ) -> Option<ParseResult<Expression>> {
        let InfixExpression {
            left,
            operator,
            right,
            ..
        } = infix;

        macro_rules! f64_ops {
            ($op:tt) => {{
                if let (
                    Some(Ok(Expression::Literal(Literal::NumberLiteral(NumberLiteral { value: left, .. })))),
                    Some(Ok(Expression::Literal(Literal::NumberLiteral(NumberLiteral { value: right, .. })))),
                ) = (self.eval_expression(left), self.eval_expression(right))
                {
                    return Some(Ok(Expression::Literal(Literal::NumberLiteral(
                        NumberLiteral {
                            value: left $op right,
                            position: self.position,
                        }
                    ))));
                }

                return None;
            }}
        }

        match operator {
            TokenKind::Plus => f64_ops! { + },
            TokenKind::Minus => f64_ops! { - },
            TokenKind::Asterisk => f64_ops! { * },
            TokenKind::Slash => f64_ops! { / },
            TokenKind::Percent => f64_ops! { % },
            _ => None,
        }
    }

    fn eval_prefix_expression(
        &mut self,
        prefix: &PrefixExpression,
    ) -> Option<ParseResult<Expression>> {
        let PrefixExpression {
            operator,
            right,
            position,
        } = prefix;

        match operator {
            TokenKind::Minus => {
                if let Expression::Literal(Literal::NumberLiteral(right)) = *right.clone() {
                    return Some(Ok(Expression::Literal(Literal::NumberLiteral(
                        NumberLiteral {
                            value: -right.value,
                            position: *position,
                        },
                    ))));
                }

                None
            }
            TokenKind::Bang => {
                if let Expression::Literal(Literal::BooleanLiteral(right)) = *right.clone() {
                    return Some(Ok(Expression::Literal(Literal::BooleanLiteral(
                        BooleanLiteral {
                            value: !right.value,
                            position: *position,
                        },
                    ))));
                }

                None
            }
            _ => None,
        }
    }
}
