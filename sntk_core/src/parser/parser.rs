use crate::{
    identifier,
    parser::{
        ast::{
            ArrayLiteral, AutoStatement, BlockExpression, BooleanLiteral, CallExpression, DataType, DataTypeKind, DeclareStatement, Expression,
            ExpressionStatement, FunctionLiteral, FunctionType, Generic, Identifier, IdentifierGeneric, IfExpression, IndexExpression,
            InfixExpression, LetStatement, NumberLiteral, Parameter, Position, PrefixExpression, Priority, Program, ReturnStatement, Statement,
            StringLiteral, StructLiteral, StructStatement, TypeStatement, TypeofExpression,
        },
        ParsingError, ParsingErrorKind,
    },
    tokenizer::{
        lexer::Lexer,
        token::{Token, TokenKind},
    },
};

pub type ParseResult<T> = Result<T, ParsingError>;

#[derive(Debug, Default)]
pub struct Parser {
    pub lexer: Lexer,
    pub current_token: Token,
    pub peek_token: Token,
    pub position: Position,
    pub errors: Vec<ParsingError>,
}

impl From<String> for Parser {
    fn from(x: String) -> Self {
        Parser::new(Lexer::new(x))
    }
}

impl Parser {
    #[inline]
    pub fn new(lexer: Lexer) -> Self {
        Parser { lexer, ..Default::default() }
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
            TokenKind::LT | TokenKind::GT | TokenKind::LTE | TokenKind::GTE => Priority::LessGreater,
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

        let ident = Identifier::new(identifier! { self }, self.position);
        self.next_token();

        self.expect_token(&TokenKind::Colon)?;

        let data_type = self.parse_data_type()?;

        self.expect_token(&TokenKind::Assign)?;

        if let Ok(expression) = self.parse_expression(&Priority::Lowest) {
            return if self.peek_token(&TokenKind::Semicolon) {
                self.next_token();

                Ok(LetStatement::new(data_type, ident, expression, self.position))
            } else {
                Err(ParsingError::new(
                    ParsingErrorKind::ExpectedNextToken(TokenKind::Semicolon.to_string(), self.current_token.kind.to_string()),
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

        let ident = Identifier::new(identifier! { self }, self.position);
        self.next_token();

        self.expect_token(&TokenKind::Assign)?;

        if let Ok(expression) = self.parse_expression(&Priority::Lowest) {
            return if self.peek_token(&TokenKind::Semicolon) {
                self.next_token();

                Ok(AutoStatement::new(ident, expression, self.position))
            } else {
                Err(ParsingError::new(
                    ParsingErrorKind::ExpectedNextToken(TokenKind::Semicolon.to_string(), self.current_token.kind.to_string()),
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

                Ok(ReturnStatement::new(expression, self.position))
            } else {
                Err(ParsingError::new(
                    ParsingErrorKind::ExpectedNextToken(TokenKind::Semicolon.to_string(), self.current_token.kind.to_string()),
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
            Ok(TypeStatement::new(
                data_type,
                Identifier::new(ident, self.position),
                generics,
                self.position,
            ))
        } else {
            Err(ParsingError::new(
                ParsingErrorKind::ExpectedNextToken(TokenKind::Semicolon.to_string(), self.current_token.kind.to_string()),
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
            Ok(DeclareStatement::new(data_type, Identifier::new(ident, self.position), self.position))
        } else {
            Err(ParsingError::new(
                ParsingErrorKind::ExpectedNextToken(TokenKind::Semicolon.to_string(), self.current_token.kind.to_string()),
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
            let key = Identifier::new(identifier! { self }, self.position);
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

        Ok(StructStatement::new(
            Identifier::new(ident, self.position),
            generics,
            fields,
            self.position,
        ))
    }

    fn parse_expression_statement(&mut self) -> ParseResult<ExpressionStatement> {
        let expression = self.parse_expression(&Priority::Lowest)?;

        if self.peek_token(&TokenKind::Semicolon) {
            self.next_token();

            Ok(ExpressionStatement::new(expression, self.position))
        } else {
            Err(ParsingError::new(
                ParsingErrorKind::ExpectedNextToken(TokenKind::Semicolon.to_string(), self.current_token.kind.to_string()),
                self.position,
            ))
        }
    }

    fn parse_expression(&mut self, priority: &Priority) -> ParseResult<Expression> {
        let left_expression = match self.current_token.kind.clone() {
            TokenKind::IDENT(ident) => Some(Ok(Expression::Identifier(Identifier::new(ident, self.position)))),
            TokenKind::Number(number) => Some(Ok(Expression::NumberLiteral(NumberLiteral::new(number, self.position)))),
            TokenKind::String(string) => Some(Ok(Expression::StringLiteral(StringLiteral::new(string, self.position)))),
            TokenKind::Boolean(boolean) => Some(Ok(Expression::BooleanLiteral(BooleanLiteral::new(boolean, self.position)))),
            TokenKind::Bang | TokenKind::Minus => {
                let operator = self.current_token.kind.clone();

                self.next_token();

                Some(Ok(Expression::PrefixExpression(PrefixExpression::new(
                    operator,
                    Box::new(self.parse_expression(&Priority::Prefix)?),
                    self.position,
                ))))
            }
            TokenKind::LParen => {
                self.next_token();

                let expression = self.parse_expression(&Priority::Lowest);
                self.next_token();

                if self.current_token.kind != TokenKind::RParen {
                    return Err(ParsingError::new(
                        ParsingErrorKind::ExpectedNextToken(TokenKind::RParen.to_string(), self.current_token.kind.to_string()),
                        self.position,
                    ));
                }

                Some(expression)
            }
            TokenKind::LBrace => Some(Ok(Expression::BlockExpression(self.parse_block_expression()?))),
            TokenKind::LBracket => Some(Ok(Expression::ArrayLiteral(self.parse_array_literal()?))),
            TokenKind::Function => Some(Ok(Expression::FunctionLiteral(self.parse_function_literal()?))),
            TokenKind::Struct => Some(Ok(Expression::StructLiteral(self.parse_struct_literal()?))),
            TokenKind::If => Some(Ok(Expression::IfExpression(self.parse_if_expression()?))),
            TokenKind::Typeof => {
                self.next_token();

                Some(Ok(Expression::TypeofExpression(TypeofExpression::new(
                    Box::new(self.parse_expression(&Priority::Lowest)?),
                    self.position,
                ))))
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
                | TokenKind::GTE => Ok(Expression::InfixExpression(InfixExpression::new(
                    Box::new(left_expression?),
                    self.current_token.kind.clone(),
                    {
                        self.next_token();
                        Box::new(self.parse_expression(&self.current_priority())?)
                    },
                    self.position,
                ))),
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
                                ParsingErrorKind::ExpectedNextToken(TokenKind::RParen.to_string(), self.current_token.kind.to_string()),
                                self.position,
                            ));
                        }
                    }

                    Ok(Expression::CallExpression(CallExpression::new(
                        Box::new(left_expression?),
                        arguments,
                        self.position,
                    )))
                }
                TokenKind::LBracket => {
                    self.next_token();

                    let index = self.parse_expression(&Priority::Lowest)?;
                    self.next_token();

                    if self.current_token.kind != TokenKind::RBracket {
                        return Err(ParsingError::new(
                            ParsingErrorKind::ExpectedNextToken(TokenKind::RBracket.to_string(), self.current_token.kind.to_string()),
                            self.position,
                        ));
                    }

                    Ok(Expression::IndexExpression(IndexExpression::new(
                        Box::new(left_expression?),
                        Box::new(index),
                        self.position,
                    )))
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

        Ok(BlockExpression::new(statements, self.position))
    }

    fn parse_array_literal(&mut self) -> ParseResult<ArrayLiteral> {
        self.next_token();

        let mut elements = Vec::new();

        if self.current_token.kind == TokenKind::RBracket {
            return Ok(ArrayLiteral::new(elements, self.position));
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
                ParsingErrorKind::ExpectedNextToken(TokenKind::RBracket.to_string(), self.current_token.kind.to_string()),
                self.position,
            ));
        }

        Ok(ArrayLiteral::new(elements, self.position))
    }

    fn parse_struct_literal(&mut self) -> ParseResult<StructLiteral> {
        self.next_token();
        let identifier = identifier! { self };

        self.next_token();
        self.next_token();

        let mut fields = Vec::new();

        while self.current_token.kind != TokenKind::RBrace {
            let key = Identifier::new(identifier! { self }, self.position);
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
                ParsingErrorKind::ExpectedNextToken(TokenKind::RBrace.to_string(), self.current_token.kind.to_string()),
                self.position,
            ));
        }

        Ok(StructLiteral::new(Identifier::new(identifier, self.position), fields, self.position))
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
            let is_spread = if self.current_token.kind == TokenKind::Spread {
                self.next_token();
                true
            } else {
                false
            };

            if let TokenKind::IDENT(identifier) = self.current_token.kind.clone() {
                self.next_token();
                self.expect_token(&TokenKind::Colon)?;

                let data_type = self.parse_data_type()?;

                parameters.push(Parameter::new(
                    Identifier::new(identifier.clone(), self.position),
                    data_type,
                    is_spread,
                    self.position,
                ));
            } else {
                return Err(ParsingError::new(
                    ParsingErrorKind::ExpectedNextToken(TokenKind::IDENT("".to_string()).to_string(), self.current_token.kind.to_string()),
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

                BlockExpression::new(
                    vec![Statement::ReturnStatement(ReturnStatement::new(
                        self.parse_expression(&Priority::Lowest)?,
                        self.position,
                    ))],
                    self.position,
                )
            }
            _ => {
                return Err(ParsingError::new(
                    ParsingErrorKind::ExpectedNextToken(TokenKind::LBrace.to_string(), self.current_token.kind.to_string()),
                    self.position,
                ))
            }
        };

        Ok(FunctionLiteral::new(generics, parameters, return_type, body, self.position))
    }

    fn parse_if_expression(&mut self) -> ParseResult<IfExpression> {
        self.next_token();

        let condition = self.parse_expression(&Priority::Lowest)?;
        self.next_token();

        let consequence = self.parse_block_expression()?;
        self.next_token();

        let alternative = if self.current_token.kind == TokenKind::Else {
            self.next_token();

            if self.current_token.kind == TokenKind::If {
                Some(Box::new(BlockExpression {
                    statements: vec![Statement::ExpressionStatement(ExpressionStatement::new(
                        Expression::IfExpression(self.parse_if_expression()?),
                        self.position,
                    ))],
                    position: self.position,
                }))
            } else {
                Some(Box::new(self.parse_block_expression()?))
            }
        } else {
            None
        };

        Ok(IfExpression::new(Box::new(condition), Box::new(consequence), alternative, self.position))
    }

    fn parse_data_type(&mut self) -> ParseResult<DataType> {
        let position = self.position;

        let result = self.parse_data_type_without_next();
        self.next_token();

        result.map(|data_type| DataType::new(data_type, position))
    }

    fn parse_data_type_without_next(&mut self) -> ParseResult<DataTypeKind> {
        let mut data_type = match self.current_token.kind {
            TokenKind::NumberType => Ok(DataTypeKind::Number),
            TokenKind::StringType => Ok(DataTypeKind::String),
            TokenKind::BooleanType => Ok(DataTypeKind::Boolean),
            TokenKind::Function => Ok(DataTypeKind::Fn(self.parse_function_type()?)),
            TokenKind::IDENT(ref ident) => Ok(DataTypeKind::Custom(ident.clone())),
            _ => Err(ParsingError::new(
                ParsingErrorKind::ExpectedNextToken(TokenKind::NumberType.to_string(), self.current_token.kind.to_string()),
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
                    ParsingErrorKind::ExpectedNextToken(TokenKind::RBracket.to_string(), self.current_token.kind.to_string()),
                    self.position,
                ));
            }

            data_type = data_type.map(|t| DataTypeKind::Array(Box::new(DataType::new(t, self.position))));
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
            let spread = if self.current_token.kind == TokenKind::Spread {
                self.next_token();
                true
            } else {
                false
            };

            parameters.push((self.parse_data_type()?, spread));

            if self.current_token.kind == TokenKind::RParen {
                break;
            }

            self.expect_token(&TokenKind::Comma)?;
        }

        self.expect_token(&TokenKind::RParen)?;
        self.expect_token(&TokenKind::Arrow)?;

        let return_type = self.parse_data_type_without_next()?;

        Ok(FunctionType::new(generics, parameters, DataType::new(return_type, self.position)))
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

        Ok(Generic::new(DataType::new(DataTypeKind::Custom(ident), self.position), generics))
    }

    fn parse_generic_identifier(&mut self) -> ParseResult<IdentifierGeneric> {
        let mut generics = Vec::new();

        self.expect_token(&TokenKind::LT)?;

        while self.current_token.kind != TokenKind::GT {
            let ident = identifier! { self };
            self.next_token();

            generics.push(Identifier::new(ident, self.position));

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

    fn eval_infix_expression(&mut self, infix: &InfixExpression) -> Option<ParseResult<Expression>> {
        self.eval_infix_expression_opt_2(infix)
    }

    fn eval_infix_expression_opt_1(&mut self, infix: &InfixExpression) -> Option<ParseResult<Expression>> {
        let InfixExpression { left, operator, right, .. } = infix;

        macro_rules! f64_ops {
            ($op:tt) => {{
                if let (Some(Ok(left)), Some(Ok(right))) = (self.eval_expression(left), self.eval_expression(right)) {
                    if let (Expression::NumberLiteral(left), Expression::NumberLiteral(right)) = (left, right) {
                        return Some(Ok(Expression::NumberLiteral(NumberLiteral::new(
                            left.value $op right.value,
                            self.position,
                        ))));
                    }

                    return None;
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

    fn eval_infix_expression_opt_2(&mut self, infix: &InfixExpression) -> Option<ParseResult<Expression>> {
        let InfixExpression { left, operator, right, .. } = infix;

        macro_rules! f64_ops {
            ($op:tt) => {{
                if let (Some(Ok(left)), Some(Ok(right))) = (self.eval_expression(left), self.eval_expression(right)) {
                    if let (Expression::NumberLiteral(left), Expression::NumberLiteral(right)) = (left, right) {
                        return Some(Ok(Expression::BooleanLiteral(BooleanLiteral::new(
                            left.value $op right.value,
                            self.position,
                        ))));
                    }

                    return None;
                }

                return None;
            }}
        }

        match operator {
            TokenKind::EQ => Some(Ok(Expression::BooleanLiteral(BooleanLiteral::new(left == right, self.position)))),
            TokenKind::NEQ => f64_ops! { != },
            TokenKind::GT => f64_ops! { > },
            TokenKind::LT => f64_ops! { < },
            TokenKind::GTE => f64_ops! { >= },
            TokenKind::LTE => f64_ops! { <= },
            _ => self.eval_infix_expression_opt_1(infix),
        }
    }

    fn eval_prefix_expression(&mut self, prefix: &PrefixExpression) -> Option<ParseResult<Expression>> {
        let PrefixExpression { operator, right, position } = prefix;

        match operator {
            TokenKind::Minus => {
                if let Expression::NumberLiteral(right) = *right.clone() {
                    return Some(Ok(Expression::NumberLiteral(NumberLiteral::new(-right.value, *position))));
                }

                None
            }
            TokenKind::Bang => {
                if let Expression::BooleanLiteral(right) = *right.clone() {
                    return Some(Ok(Expression::BooleanLiteral(BooleanLiteral::new(!right.value, *position))));
                }

                None
            }
            _ => None,
        }
    }
}
