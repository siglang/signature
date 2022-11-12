use crate::{
    ident,
    options::CompilerOptions,
    parser::{
        ast::{
            ArrayLiteral, AutoStatement, BlockExpression, BooleanLiteral, CallExpression, DataType, Expression, ExpressionStatement, FunctionLiteral,
            FunctionType, Generic, Identifier, IdentifierGeneric, IfExpression, IndexExpression, InfixExpression, LetStatement, NumberLiteral,
            Position, PrefixExpression, Priority, Program, ReturnStatement, Statement, StringLiteral, StructLiteral, StructStatement, TypeStatement,
            TypeofExpression,
        },
        error::{ParsingError, EXPECTED_EXPRESSION, EXPECTED_NEXT_TOKEN, UNEXPECTED_TOKEN},
    },
    parsing_error, position,
    tokenizer::{
        lexer::{Lexer, LexerTrait},
        token::{Token, Tokens},
    },
};

pub type ParseResult<T> = Result<T, ParsingError>;

pub trait ParserBase {
    fn new(lexer: Lexer) -> Self;
    fn new_with_options(lexer: Lexer, options: CompilerOptions) -> Self;
    fn next_token(&mut self);
    fn expect_token(&mut self, token_type: &Tokens) -> ParseResult<()>;
    fn peek_token(&self, token_type: &Tokens) -> bool;
    fn get_priority(&self, token_type: &Tokens) -> Priority;
    fn peek_priority(&mut self) -> Priority;
    fn current_priority(&self) -> Priority;
}

pub trait ParserTrait {
    fn parse_program(&mut self) -> Program;
    fn parse_statement(&mut self) -> ParseResult<Statement>;
    fn parse_let_statement(&mut self) -> ParseResult<LetStatement>;
    fn parse_auto_statement(&mut self) -> ParseResult<AutoStatement>;
    fn parse_return_statement(&mut self) -> ParseResult<ReturnStatement>;
    fn parse_type_statement(&mut self) -> ParseResult<TypeStatement>;
    fn parse_struct_statement(&mut self) -> ParseResult<StructStatement>;
    fn parse_expression_statement(&mut self) -> ParseResult<ExpressionStatement>;
    fn parse_expression(&mut self, precedence: &Priority) -> ParseResult<Expression>;
    fn parse_block_expression(&mut self) -> ParseResult<BlockExpression>;
    fn parse_if_expression(&mut self) -> ParseResult<IfExpression>;
    fn parse_array_literal(&mut self) -> ParseResult<ArrayLiteral>;
    fn parse_function_literal(&mut self) -> ParseResult<FunctionLiteral>;
    fn parse_struct_literal(&mut self) -> ParseResult<StructLiteral>;
}

pub trait TypeParser {
    fn parse_data_type(&mut self) -> ParseResult<DataType>;
    fn parse_data_type_without_next(&mut self) -> ParseResult<DataType>;
    fn parse_function_type(&mut self) -> ParseResult<FunctionType>;
    fn parse_generic(&mut self) -> ParseResult<Generic>;
    fn parse_generic_identifier(&mut self) -> ParseResult<IdentifierGeneric>;
}

pub trait EEE {
    fn eval_expression(&mut self, expression: &Expression) -> Option<ParseResult<Expression>>;
    fn eval_infix_expression(&mut self, infix: &InfixExpression) -> Option<ParseResult<Expression>>;
    fn eval_infix_expression_opt_1(&mut self, infix: &InfixExpression) -> Option<ParseResult<Expression>>;
    fn eval_infix_expression_opt_2(&mut self, infix: &InfixExpression) -> Option<ParseResult<Expression>>;
    fn eval_prefix_expression(&mut self, prefix: &PrefixExpression) -> Option<ParseResult<Expression>>;
}

#[derive(Debug, Default)]
pub struct Parser {
    pub lexer: Lexer,
    pub current_token: Token,
    pub peek_token: Token,
    pub position: Position,
    pub errors: Vec<ParsingError>,
    pub options: CompilerOptions,
}

impl From<String> for Parser {
    fn from(x: String) -> Self {
        Parser::new(Lexer::new(x))
    }
}

impl ParserBase for Parser {
    #[inline]
    fn new(lexer: Lexer) -> Self {
        Parser { lexer, ..Default::default() }
    }

    #[inline]
    fn new_with_options(lexer: Lexer, options: CompilerOptions) -> Self {
        Parser {
            lexer,
            options,
            ..Default::default()
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();

        self.position = Position::new(self.current_token.position.0, self.current_token.position.1);
    }

    fn expect_token(&mut self, token_type: &Tokens) -> ParseResult<()> {
        if self.current_token.token_type == *token_type {
            self.next_token();

            Ok(())
        } else {
            Err(parsing_error! { self; EXPECTED_NEXT_TOKEN; token_type, self.current_token.token_type })
        }
    }

    fn peek_token(&self, token_type: &Tokens) -> bool {
        self.peek_token.token_type == *token_type
    }

    fn get_priority(&self, token_type: &Tokens) -> Priority {
        match token_type {
            Tokens::Dot | Tokens::Arrow => Priority::Dot,
            Tokens::Assign | Tokens::EQ | Tokens::NEQ => Priority::Equals,
            Tokens::Plus | Tokens::Minus => Priority::Sum,
            Tokens::Slash | Tokens::Asterisk => Priority::Product,
            Tokens::LT | Tokens::GT | Tokens::LTE | Tokens::GTE => Priority::LessGreater,
            Tokens::LParen => Priority::Call,
            Tokens::LBracket => Priority::Index,
            _ => Priority::Lowest,
        }
    }

    fn peek_priority(&mut self) -> Priority {
        self.get_priority(&self.peek_token.token_type)
    }

    fn current_priority(&self) -> Priority {
        self.get_priority(&self.current_token.token_type)
    }
}

impl ParserTrait for Parser {
    fn parse_program(&mut self) -> Program {
        self.next_token();
        self.next_token();

        let mut program = Program::default();

        while self.current_token.token_type != Tokens::EOF {
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
        Ok(match self.current_token.token_type {
            Tokens::Let => Statement::LetStatement(self.parse_let_statement()?),
            Tokens::Auto => Statement::AutoStatement(self.parse_auto_statement()?),
            Tokens::Return => Statement::ReturnStatement(self.parse_return_statement()?),
            Tokens::Type => Statement::TypeStatement(self.parse_type_statement()?),
            Tokens::Struct => Statement::StructStatement(self.parse_struct_statement()?),
            _ => Statement::ExpressionStatement(self.parse_expression_statement()?),
        })
    }

    fn parse_let_statement(&mut self) -> ParseResult<LetStatement> {
        self.next_token();

        let ident = Identifier::new(ident! { self }, position! { self });
        self.next_token();

        self.expect_token(&Tokens::Colon)?;

        let data_type = self.parse_data_type()?;

        self.expect_token(&Tokens::Assign)?;

        if let Ok(expression) = self.parse_expression(&Priority::Lowest) {
            return if self.peek_token(&Tokens::Semicolon) {
                self.next_token();

                Ok(LetStatement::new(data_type, ident, expression, position! { self }))
            } else {
                Err(parsing_error! { self; EXPECTED_NEXT_TOKEN; Tokens::Semicolon, self.current_token.token_type })
            };
        }

        Err(parsing_error! { self; UNEXPECTED_TOKEN; self.current_token.token_type })
    }

    fn parse_auto_statement(&mut self) -> ParseResult<AutoStatement> {
        self.next_token();

        let ident = Identifier::new(ident! { self }, position! { self });
        self.next_token();

        self.expect_token(&Tokens::Assign)?;

        if let Ok(expression) = self.parse_expression(&Priority::Lowest) {
            return if self.peek_token(&Tokens::Semicolon) {
                self.next_token();

                Ok(AutoStatement::new(ident, expression, position! { self }))
            } else {
                Err(parsing_error! { self; EXPECTED_NEXT_TOKEN; Tokens::Semicolon, self.current_token.token_type })
            };
        }

        Err(parsing_error! { self; UNEXPECTED_TOKEN; self.current_token.token_type })
    }

    fn parse_return_statement(&mut self) -> ParseResult<ReturnStatement> {
        self.next_token();

        if let Ok(expression) = self.parse_expression(&Priority::Lowest) {
            return if self.peek_token(&Tokens::Semicolon) {
                self.next_token();

                Ok(ReturnStatement::new(expression, position! { self }))
            } else {
                Err(parsing_error! { self; EXPECTED_NEXT_TOKEN; Tokens::Semicolon, self.current_token.token_type })
            };
        }

        Err(parsing_error! { self; EXPECTED_EXPRESSION; self.current_token.token_type })
    }

    fn parse_type_statement(&mut self) -> ParseResult<TypeStatement> {
        self.next_token();

        let ident = ident! { self };
        self.next_token();

        let generics = if self.current_token.token_type == Tokens::LT {
            self.parse_generic_identifier()?
        } else {
            Vec::new()
        };
        self.next_token();

        self.expect_token(&Tokens::Assign)?;

        let data_type = self.parse_data_type()?;

        if self.current_token.token_type == Tokens::Semicolon {
            Ok(TypeStatement::new(
                data_type,
                Identifier::new(ident, position! { self }),
                generics,
                position! { self },
            ))
        } else {
            Err(parsing_error! { self; EXPECTED_NEXT_TOKEN; Tokens::Semicolon, self.current_token.token_type })
        }
    }

    fn parse_struct_statement(&mut self) -> ParseResult<StructStatement> {
        self.next_token();

        let ident = ident! { self };
        self.next_token();

        let generics = if self.current_token.token_type == Tokens::LT {
            let generic = self.parse_generic_identifier()?;

            self.next_token();

            generic
        } else {
            Vec::new()
        };

        self.expect_token(&Tokens::LBrace)?;

        let mut fields = Vec::new();

        while self.current_token.token_type != Tokens::RBrace {
            let key = Identifier::new(ident! { self }, position! { self });
            self.next_token();

            self.expect_token(&Tokens::Colon)?;

            let value = self.parse_data_type()?;

            fields.push((key, value));

            if self.current_token.token_type == Tokens::RBrace {
                break;
            }

            self.expect_token(&Tokens::Comma)?;
        }

        self.expect_token(&Tokens::RBrace)?;

        Ok(StructStatement::new(
            Identifier::new(ident, position! { self }),
            generics,
            fields,
            position! { self },
        ))
    }

    fn parse_expression_statement(&mut self) -> ParseResult<ExpressionStatement> {
        let expression = self.parse_expression(&Priority::Lowest)?;

        if self.peek_token(&Tokens::Semicolon) {
            self.next_token();

            Ok(ExpressionStatement::new(expression, position! { self }))
        } else {
            Err(parsing_error! { self; EXPECTED_NEXT_TOKEN; Tokens::Semicolon, self.current_token.token_type })
        }
    }

    fn parse_expression(&mut self, priority: &Priority) -> ParseResult<Expression> {
        let left_expression = match self.current_token.token_type.clone() {
            Tokens::IDENT(ident) => Some(Ok(Expression::Identifier(Identifier::new(ident, position! { self })))),
            Tokens::Number(number) => Some(Ok(Expression::NumberLiteral(NumberLiteral::new(number, position! { self })))),
            Tokens::String(string) => Some(Ok(Expression::StringLiteral(StringLiteral::new(string, position! { self })))),
            Tokens::Boolean(boolean) => Some(Ok(Expression::BooleanLiteral(BooleanLiteral::new(boolean, position! { self })))),
            Tokens::Bang | Tokens::Minus => {
                let operator = self.current_token.token_type.clone();

                self.next_token();

                Some(Ok(Expression::PrefixExpression(PrefixExpression::new(
                    operator,
                    Box::new(self.parse_expression(&Priority::Prefix)?),
                    position! { self },
                ))))
            }
            Tokens::LParen => {
                self.next_token();

                let expression = self.parse_expression(&Priority::Lowest);
                self.next_token();

                if self.current_token.token_type != Tokens::RParen {
                    return Err(parsing_error! { self; EXPECTED_NEXT_TOKEN; Tokens::RParen, self.current_token.token_type });
                }

                Some(expression)
            }
            Tokens::LBrace => Some(Ok(Expression::BlockExpression(self.parse_block_expression()?))),
            Tokens::LBracket => Some(Ok(Expression::ArrayLiteral(self.parse_array_literal()?))),
            Tokens::Function => Some(Ok(Expression::FunctionLiteral(self.parse_function_literal()?))),
            Tokens::Struct => Some(Ok(Expression::StructLiteral(self.parse_struct_literal()?))),
            Tokens::If => Some(Ok(Expression::IfExpression(self.parse_if_expression()?))),
            Tokens::Typeof => {
                self.next_token();

                Some(Ok(Expression::TypeofExpression(TypeofExpression::new(
                    Box::new(self.parse_expression(&Priority::Lowest)?),
                    position! { self },
                ))))
            }
            _ => None,
        };

        if left_expression.is_none() && self.current_token.token_type != Tokens::Semicolon {
            return Err(parsing_error! { self; UNEXPECTED_TOKEN; self.current_token.token_type });
        }

        let mut left_expression = left_expression.ok_or_else(|| parsing_error! { self; UNEXPECTED_TOKEN; self.current_token.token_type })?;

        while !self.peek_token(&Tokens::Semicolon) && priority < &self.peek_priority() {
            self.next_token();

            left_expression = match self.current_token.token_type {
                Tokens::Plus
                | Tokens::Minus
                | Tokens::Dot
                | Tokens::Slash
                | Tokens::Asterisk
                | Tokens::Percent
                | Tokens::Arrow // bind: `foo -> bar` = `foo.bind(bar)`
                | Tokens::EQ
                | Tokens::NEQ
                | Tokens::LT
                | Tokens::GT
                | Tokens::LTE
                | Tokens::GTE => Ok(Expression::InfixExpression(InfixExpression::new(
                    Box::new(left_expression?),
                    self.current_token.token_type.clone(),
                    {
                        self.next_token();
                        Box::new(self.parse_expression(&self.current_priority())?)
                    },
                    position! { self },
                ))),
                Tokens::LParen => {
                    self.next_token();

                    let mut arguments = Vec::new();

                    if self.current_token.token_type != Tokens::RParen {
                        arguments.push(self.parse_expression(&Priority::Lowest)?);
                        self.next_token();

                        if self.current_token.token_type == Tokens::Comma {
                            self.next_token();
                        }

                        while self.current_token.token_type != Tokens::RParen {
                            arguments.push(self.parse_expression(&Priority::Lowest)?);
                            self.next_token();

                            if self.current_token.token_type == Tokens::RParen {
                                break;
                            }

                            self.expect_token(&Tokens::Comma)?;
                        }

                        if self.current_token.token_type != Tokens::RParen {
                            return Err(
                                parsing_error! { self; EXPECTED_NEXT_TOKEN; Tokens::RParen, self.current_token.token_type },
                            );
                        }
                    }

                    Ok(Expression::CallExpression(CallExpression::new(
                        Box::new(left_expression?),
                        arguments,
                        position! { self },
                    )))
                }
                Tokens::LBracket => {
                    self.next_token();

                    let index = self.parse_expression(&Priority::Lowest)?;
                    self.next_token();

                    if self.current_token.token_type != Tokens::RBracket {
                        return Err(parsing_error! { self; EXPECTED_NEXT_TOKEN; Tokens::RBracket, self.current_token.token_type });
                    }

                    Ok(Expression::IndexExpression(IndexExpression::new(
                        Box::new(left_expression?),
                        Box::new(index),
                        position! { self },
                    )))
                }
                _ => Err(parsing_error! { self; UNEXPECTED_TOKEN; self.current_token.token_type }),
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

        while self.current_token.token_type != Tokens::RBrace {
            statements.push(self.parse_statement()?);
            self.next_token();
        }

        Ok(BlockExpression::new(statements, position! { self }))
    }

    fn parse_array_literal(&mut self) -> ParseResult<ArrayLiteral> {
        self.next_token();

        let mut elements = Vec::new();

        if self.current_token.token_type == Tokens::RBracket {
            return Ok(ArrayLiteral::new(elements, position! { self }));
        }

        while self.current_token.token_type != Tokens::RBrace {
            elements.push(self.parse_expression(&Priority::Lowest)?);
            self.next_token();

            if self.current_token.token_type == Tokens::RBracket {
                break;
            }

            self.expect_token(&Tokens::Comma)?;
        }

        if self.current_token.token_type != Tokens::RBracket {
            return Err(parsing_error! { self; EXPECTED_NEXT_TOKEN; Tokens::RBracket, self.current_token.token_type });
        }

        Ok(ArrayLiteral::new(elements, position! { self }))
    }

    fn parse_struct_literal(&mut self) -> ParseResult<StructLiteral> {
        self.next_token();
        let identifier = ident! { self };

        self.next_token();
        self.next_token();

        let mut fields = Vec::new();

        while self.current_token.token_type != Tokens::RBrace {
            let key = Identifier::new(ident! { self }, position! { self });
            self.next_token();

            self.expect_token(&Tokens::Colon)?;

            let value = self.parse_expression(&Priority::Lowest)?;
            self.next_token();

            fields.push((key, value));

            if self.current_token.token_type == Tokens::RBrace {
                break;
            }

            self.expect_token(&Tokens::Comma)?;
        }

        if self.current_token.token_type != Tokens::RBrace {
            return Err(parsing_error! { self; EXPECTED_NEXT_TOKEN; Tokens::RBrace, self.current_token.token_type });
        }

        Ok(StructLiteral::new(
            Identifier::new(identifier, position! { self }),
            fields,
            position! { self },
        ))
    }

    fn parse_function_literal(&mut self) -> ParseResult<FunctionLiteral> {
        self.next_token();

        let generics = if self.current_token.token_type == Tokens::LT {
            let result = Some(self.parse_generic_identifier()?);
            self.next_token();

            result
        } else {
            None
        };

        self.expect_token(&Tokens::LParen)?;

        let mut parameters = Vec::new();

        while self.current_token.token_type != Tokens::RParen {
            if let Tokens::IDENT(identifier) = self.current_token.token_type.clone() {
                self.next_token();
                self.expect_token(&Tokens::Colon)?;

                let data_type = self.parse_data_type()?;

                parameters.push((Identifier::new(identifier.clone(), position! { self }), data_type));
            } else {
                return Err(parsing_error! { self; UNEXPECTED_TOKEN; self.current_token.token_type });
            }

            if self.current_token.token_type == Tokens::RParen {
                break;
            }

            self.expect_token(&Tokens::Comma)?;
        }

        self.expect_token(&Tokens::RParen)?;
        self.expect_token(&Tokens::Arrow)?;

        let return_type = self.parse_data_type()?;

        let body = match self.current_token.token_type {
            Tokens::LBrace => self.parse_block_expression()?,
            Tokens::Arrow => {
                self.next_token();

                BlockExpression::new(
                    vec![Statement::ReturnStatement(ReturnStatement::new(
                        self.parse_expression(&Priority::Lowest)?,
                        position! { self },
                    ))],
                    position! { self },
                )
            }
            _ => return Err(parsing_error! { self; EXPECTED_NEXT_TOKEN; Tokens::LBrace, self.current_token.token_type }),
        };

        Ok(FunctionLiteral::new(generics, parameters, return_type, body, position! { self }))
    }

    fn parse_if_expression(&mut self) -> ParseResult<IfExpression> {
        self.next_token();

        let condition = self.parse_expression(&Priority::Lowest)?;
        self.next_token();

        let consequence = self.parse_block_expression()?;
        self.next_token();

        let alternative = if self.current_token.token_type == Tokens::Else {
            self.next_token();

            if self.current_token.token_type == Tokens::If {
                Some(Box::new(BlockExpression {
                    statements: vec![Statement::ExpressionStatement(ExpressionStatement::new(
                        Expression::IfExpression(self.parse_if_expression()?),
                        position! { self },
                    ))],
                    position: position! { self },
                }))
            } else {
                Some(Box::new(self.parse_block_expression()?))
            }
        } else {
            None
        };

        Ok(IfExpression::new(
            Box::new(condition),
            Box::new(consequence),
            alternative,
            position! { self },
        ))
    }
}

impl TypeParser for Parser {
    fn parse_data_type(&mut self) -> ParseResult<DataType> {
        let result = self.parse_data_type_without_next();
        self.next_token();
        result
    }

    fn parse_data_type_without_next(&mut self) -> ParseResult<DataType> {
        let mut data_type = match self.current_token.token_type {
            Tokens::NumberType => Ok(DataType::Number),
            Tokens::StringType => Ok(DataType::String),
            Tokens::BooleanType => Ok(DataType::Boolean),
            Tokens::Function => Ok(DataType::Fn(self.parse_function_type()?)),
            Tokens::IDENT(ref ident) => Ok(DataType::Custom(ident.clone())),
            _ => Err(parsing_error! { self; UNEXPECTED_TOKEN; self.current_token.token_type }),
        };

        if self.peek_token(&Tokens::LT) {
            let generics = self.parse_generic()?;

            data_type = Ok(DataType::Generic(generics));
        }

        while self.peek_token(&Tokens::LBracket) {
            self.next_token();
            self.next_token();

            if self.current_token.token_type != Tokens::RBracket {
                return Err(parsing_error! { self; EXPECTED_NEXT_TOKEN; Tokens::RBracket, self.current_token.token_type });
            }

            data_type = data_type.map(|t| DataType::Array(Box::new(t)));
        }

        data_type
    }

    fn parse_function_type(&mut self) -> ParseResult<FunctionType> {
        self.next_token();

        let generics = if self.current_token.token_type == Tokens::LT {
            let result = Some(self.parse_generic_identifier()?);
            self.next_token();

            result
        } else {
            None
        };

        self.expect_token(&Tokens::LParen)?;

        let mut parameters = Vec::new();

        while self.current_token.token_type != Tokens::RParen {
            parameters.push(self.parse_data_type()?);

            if self.current_token.token_type == Tokens::RParen {
                break;
            }

            self.expect_token(&Tokens::Comma)?;
        }

        self.expect_token(&Tokens::RParen)?;
        self.expect_token(&Tokens::Arrow)?;

        let return_type = self.parse_data_type_without_next()?;

        Ok(FunctionType::new(generics, parameters, return_type))
    }

    fn parse_generic(&mut self) -> ParseResult<Generic> {
        let ident = ident! { self };
        self.next_token();

        let mut generics = Vec::new();

        self.expect_token(&Tokens::LT)?;

        while self.current_token.token_type != Tokens::GT {
            let data_type = self.parse_data_type()?;

            generics.push(data_type);

            if self.current_token.token_type == Tokens::GT {
                break;
            }

            self.expect_token(&Tokens::Comma)?;
        }

        Ok(Generic::new(DataType::Custom(ident), generics))
    }

    fn parse_generic_identifier(&mut self) -> ParseResult<IdentifierGeneric> {
        let mut generics = Vec::new();

        self.expect_token(&Tokens::LT)?;

        while self.current_token.token_type != Tokens::GT {
            let ident = ident! { self };
            self.next_token();

            generics.push(Identifier::new(ident, position! { self }));

            if self.current_token.token_type == Tokens::GT {
                break;
            }

            self.expect_token(&Tokens::Comma)?;
        }

        Ok(generics)
    }
}

impl EEE for Parser {
    fn eval_expression(&mut self, expression: &Expression) -> Option<ParseResult<Expression>> {
        match expression {
            Expression::InfixExpression(infix) => self.eval_infix_expression(infix),
            Expression::PrefixExpression(prefix) => self.eval_prefix_expression(prefix),
            e => Some(Ok(e.clone())),
        }
    }

    fn eval_infix_expression(&mut self, infix: &InfixExpression) -> Option<ParseResult<Expression>> {
        match self.options.eee_opt_level() {
            1 => self.eval_infix_expression_opt_1(infix),
            2 => self.eval_infix_expression_opt_2(infix),
            _ => None,
        }
    }

    fn eval_infix_expression_opt_1(&mut self, infix: &InfixExpression) -> Option<ParseResult<Expression>> {
        let InfixExpression { left, operator, right, .. } = infix;

        macro_rules! f64_ops {
            ($op:tt) => {{
                if let (Some(Ok(left)), Some(Ok(right))) = (self.eval_expression(left), self.eval_expression(right)) {
                    if let (Expression::NumberLiteral(left), Expression::NumberLiteral(right)) = (left, right) {
                        return Some(Ok(Expression::NumberLiteral(NumberLiteral::new(
                            left.value $op right.value,
                            position! { self },
                        ))));
                    }

                    return None;
                }

                return None;
            }}
        }

        match operator {
            Tokens::Plus => f64_ops! { + },
            Tokens::Minus => f64_ops! { - },
            Tokens::Asterisk => f64_ops! { * },
            Tokens::Slash => f64_ops! { / },
            Tokens::Percent => f64_ops! { % },
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
                            position! { self },
                        ))));
                    }

                    return None;
                }

                return None;
            }}
        }

        match operator {
            Tokens::EQ => Some(Ok(Expression::BooleanLiteral(BooleanLiteral::new(left == right, position! { self })))),
            Tokens::NEQ => f64_ops! { != },
            Tokens::GT => f64_ops! { > },
            Tokens::LT => f64_ops! { < },
            Tokens::GTE => f64_ops! { >= },
            Tokens::LTE => f64_ops! { <= },
            _ => self.eval_infix_expression_opt_1(infix),
        }
    }

    fn eval_prefix_expression(&mut self, prefix: &PrefixExpression) -> Option<ParseResult<Expression>> {
        let PrefixExpression { operator, right, position } = prefix;

        match operator {
            Tokens::Minus => {
                if let Expression::NumberLiteral(right) = *right.clone() {
                    return Some(Ok(Expression::NumberLiteral(NumberLiteral::new(-right.value, position.clone()))));
                }

                None
            }
            Tokens::Bang => {
                if let Expression::BooleanLiteral(right) = *right.clone() {
                    return Some(Ok(Expression::BooleanLiteral(BooleanLiteral::new(!right.value, position.clone()))));
                }

                None
            }
            _ => None,
        }
    }
}
