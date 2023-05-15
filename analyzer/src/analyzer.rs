#![allow(unused_variables)]

use crate::{
    symbol_table::{SymbolAttributes, SymbolEntry, SymbolKind, SymbolTable},
    SemanticError, SemanticResult,
};
use parser::{
    ast::{
        ArrayLiteral, AutoStatement, BlockExpression, DataType, DataTypeKind, DeclareStatement,
        Expression, Identifier, InfixExpression, LetStatement, Literal, Position, PrefixExpression,
        Program, ReturnExpressionStatement, ReturnStatement, Statement, StructStatement,
        TypeStatement,
    },
    tokenizer::TokenKind,
};

/// `Return` - returns a value from top-level function scope.
///
/// ```ignore
/// fn() -> number {
///     let x: number = {
///         let y: number = {
///             return 5;
///         };
///
///         return y + 2; // unreachable code
///     };
///
///     return x + 2; // unreachable code
/// }; // 5
/// ```
///
/// `ReturnExpression` - returns a value from a scope.
///
/// ```ignore
/// fn() -> number {
///     let x: number = {
///         let y: number = {
///             5
///         };
///
///         y + 2 // return 7
///     };
///
///     x + 2 // 9
/// };
/// ```
#[derive(Debug, Clone)]
pub enum AnalyzerReturnKind {
    Return(DataTypeKind),
    ReturnExpression(DataTypeKind),
    Unknown,
}

/// # Analyzer
///
/// The analyzer is responsible for analyzing the AST, type checking and checking for semantic errors.
///
/// ## Example
///
/// ```ignore
/// use analyzer::Analyzer;
///
/// match Analyzer::new(program).analyze() {
///    Ok(_) => println!("No semantic errors found!"),
///    Err(error) => println!("Semantic error: {error}")
/// }
/// ```
#[derive(Debug, Clone)]
pub struct Analyzer {
    pub program: Program,
    pub symbol_table: SymbolTable,
    pub return_type: AnalyzerReturnKind,
}

impl Analyzer {
    /// Creates a new analyzer with an empty symbol table.
    pub fn new(program: Program) -> Self {
        Self {
            program,
            symbol_table: SymbolTable::new(None),
            return_type: AnalyzerReturnKind::Unknown,
        }
    }

    /// Creates a new analyzer with the given symbol table.
    pub fn new_with_symbol_table(program: Program, symbol_table: SymbolTable) -> Self {
        Self {
            program,
            symbol_table,
            return_type: AnalyzerReturnKind::Unknown,
        }
    }

    fn set_return_type(
        &mut self,
        ttype: AnalyzerReturnKind,
        position: Position,
    ) -> SemanticResult<()> {
        match self.return_type {
            AnalyzerReturnKind::Unknown => self.return_type = ttype,
            _ => {
                if let AnalyzerReturnKind::ReturnExpression(return_type) = self.return_type.clone()
                {
                    if let AnalyzerReturnKind::ReturnExpression(ttype) = ttype.clone() {
                        if return_type != ttype {
                            return Err(SemanticError::type_mismatch(return_type, ttype, position));
                        }
                    }
                }

                if let AnalyzerReturnKind::Return(return_type) = self.return_type.clone() {
                    if let AnalyzerReturnKind::Return(ttype) = ttype {
                        if return_type != ttype {
                            return Err(SemanticError::type_mismatch(return_type, ttype, position));
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Analyzes the program and returns a `SemanticResult` with return type of the program.
    pub fn analyze(&mut self) -> SemanticResult<AnalyzerReturnKind> {
        for statement in self.program.clone() {
            self.analyze_statement(&statement)?;

            match self.return_type {
                AnalyzerReturnKind::Return(_) => return Ok(self.return_type.clone()),
                AnalyzerReturnKind::ReturnExpression(_) => break,
                _ => {}
            }
        }

        Ok(self.return_type.clone())
    }

    fn analyze_statement(&mut self, statement: &Statement) -> SemanticResult<()> {
        match statement {
            Statement::LetStatement(statement) => self.analyze_let_statement(statement),
            Statement::AutoStatement(statement) => self.analyze_auto_statement(statement),
            Statement::ReturnStatement(statement) => self.analyze_return_statement(statement),
            Statement::ReturnExpressionStatement(statement) => {
                self.analyze_return_expression_statement(statement)
            }
            Statement::TypeStatement(statement) => self.analyze_type_statement(statement),
            Statement::DeclareStatement(statement) => self.analyze_declare_statement(statement),
            Statement::StructStatement(statement) => self.analyze_struct_statement(statement),
            Statement::ExpressionStatement(statement) => {
                self.analyze_expression(&statement.expression)?;
                Ok(())
            }
        }
    }

    fn analyze_let_statement(&mut self, statement: &LetStatement) -> SemanticResult<()> {
        let type_annotation = self.typeof_data_type(&statement.data_type)?;
        let expression_type = self.analyze_expression_with_provided_type(
            &statement.value,
            type_annotation.clone().kind,
        )?;

        if expression_type != type_annotation {
            return Err(SemanticError::type_mismatch(
                expression_type,
                type_annotation,
                statement.position,
            ));
        }

        self.symbol_table
            .insert(
                &statement.identifier.value,
                SymbolEntry::new(
                    statement.data_type.clone(),
                    SymbolAttributes::default(),
                    SymbolKind::Variable,
                ),
            )
            .ok_or_else(|| {
                SemanticError::identifier_already_defined(
                    statement.identifier.value.clone(),
                    statement.position,
                )
            })?;

        Ok(())
    }

    fn analyze_auto_statement(&mut self, statement: &AutoStatement) -> SemanticResult<()> {
        let expression_type = self.analyze_expression(&statement.value)?;

        self.symbol_table
            .insert(
                &statement.identifier.value,
                SymbolEntry::new(
                    expression_type,
                    SymbolAttributes::default(),
                    SymbolKind::Variable,
                ),
            )
            .ok_or_else(|| {
                SemanticError::identifier_already_defined(
                    statement.identifier.value.clone(),
                    statement.position,
                )
            })?;

        Ok(())
    }

    fn analyze_return_statement(&mut self, statement: &ReturnStatement) -> SemanticResult<()> {
        let expression_type = self.analyze_expression(&statement.value)?;
        self.set_return_type(
            AnalyzerReturnKind::Return(expression_type.kind),
            statement.position,
        )?;

        Ok(())
    }

    fn analyze_return_expression_statement(
        &mut self,
        statement: &ReturnExpressionStatement,
    ) -> SemanticResult<()> {
        let expression_type = self.analyze_expression(&statement.value)?;
        self.set_return_type(
            AnalyzerReturnKind::ReturnExpression(expression_type.kind),
            statement.position,
        )?;

        Ok(())
    }

    fn analyze_type_statement(&mut self, statement: &TypeStatement) -> SemanticResult<()> {
        let ttype = self.typeof_data_type(&statement.data_type)?;

        self.symbol_table
            .insert(
                &statement.identifier.value,
                SymbolEntry::new(ttype, SymbolAttributes::default(), SymbolKind::Named),
            )
            .ok_or_else(|| {
                SemanticError::type_alias_already_defined(
                    statement.identifier.value.clone(),
                    statement.position,
                )
            })?;

        Ok(())
    }

    fn analyze_declare_statement(&mut self, statement: &DeclareStatement) -> SemanticResult<()> {
        let ttype = self.typeof_data_type(&statement.data_type)?;

        self.symbol_table
            .insert(
                &statement.identifier.value,
                SymbolEntry::new(ttype, SymbolAttributes::default(), SymbolKind::Variable),
            )
            .ok_or_else(|| {
                SemanticError::identifier_already_defined(
                    statement.identifier.value.clone(),
                    statement.position,
                )
            })?;

        Ok(())
    }

    fn analyze_struct_statement(&mut self, statement: &StructStatement) -> SemanticResult<()> {
        todo!()
    }

    fn analyze_expression(&mut self, expression: &Expression) -> SemanticResult<DataType> {
        self.typeof_expression(expression)
    }

    fn analyze_expression_with_provided_type(
        &mut self,
        expression: &Expression,
        ttype: DataTypeKind,
    ) -> SemanticResult<DataType> {
        self.typeof_expression_with_provided_type(expression, Some(ttype))
    }

    #[allow(unused_variables)]
    pub fn typeof_expression_with_provided_type(
        &mut self,
        expression: &Expression,
        provided_type: Option<DataTypeKind>,
    ) -> SemanticResult<DataType> {
        let ttype = match expression {
            Expression::BlockExpression(block) => {
                let kind = self.typeof_block_expression(block)?;
                Ok(match kind {
                    AnalyzerReturnKind::Return(ttype) => {
                        self.set_return_type(
                            AnalyzerReturnKind::Return(ttype.clone()),
                            block.position,
                        )?;
                        DataType::new(ttype, block.position)
                    }
                    AnalyzerReturnKind::ReturnExpression(ttype) => {
                        DataType::new(ttype, block.position)
                    }
                    _ => DataType::new(DataTypeKind::Unknown, block.position),
                })
            }
            Expression::PrefixExpression(prefix) => self.typeof_prefix_expression(prefix),
            Expression::InfixExpression(infix) => self.typeof_infix_expression(infix),
            Expression::IfExpression(if_expression) => todo!(),
            Expression::CallExpression(call) => todo!(),
            Expression::TypeofExpression(typeof_expression) => todo!(),
            Expression::IndexExpression(index) => todo!(),
            Expression::Literal(literal) => self.typeof_literal(literal, provided_type),
            Expression::Debug(expression) => {
                println!("DEBUG: {:?}", expression);
                self.typeof_expression(expression)
            }
        };

        ttype.map(|ttype| self.typeof_data_type(&ttype))?
    }

    pub fn typeof_expression(&mut self, expression: &Expression) -> SemanticResult<DataType> {
        self.typeof_expression_with_provided_type(expression, None)
    }

    fn typeof_block_expression(
        &mut self,
        block: &BlockExpression,
    ) -> SemanticResult<AnalyzerReturnKind> {
        let symbol_table = SymbolTable::new(Some(self.symbol_table.clone()));
        Analyzer::new_with_symbol_table(block.statements.clone(), symbol_table).analyze()
    }

    fn typeof_prefix_expression(&mut self, prefix: &PrefixExpression) -> SemanticResult<DataType> {
        let right = self.typeof_expression(&prefix.right)?;

        /*
            !T => boolean
            [-|!]T => number
        */
        match prefix.operator {
            TokenKind::Bang => {
                if right.kind != DataTypeKind::Boolean {
                    Err(SemanticError::operator_not_supported(
                        prefix.operator.clone(),
                        right.kind,
                        prefix.position,
                    ))
                } else {
                    Ok(DataType::new(DataTypeKind::Boolean, prefix.position))
                }
            }
            TokenKind::Minus | TokenKind::Plus => {
                if right.kind != DataTypeKind::Number {
                    Err(SemanticError::operator_not_supported(
                        prefix.operator.clone(),
                        right.kind,
                        prefix.position,
                    ))
                } else {
                    Ok(DataType::new(DataTypeKind::Number, prefix.position))
                }
            }
            _ => unreachable!(),
        }
    }

    fn typeof_infix_expression(&mut self, infix: &InfixExpression) -> SemanticResult<DataType> {
        let left = self.typeof_expression(&infix.left)?;
        let right = self.typeof_expression(&infix.right)?;

        /*
            T [-|*|/|%] T => number
            T + T => number|string
            T [==|!=|<|>|<=|>=] T => boolean
        */
        match infix.operator {
            TokenKind::Plus => match left.kind {
                DataTypeKind::Number | DataTypeKind::String => {
                    if left.kind == right.kind {
                        Ok(DataType::new(left.kind, infix.position))
                    } else {
                        Err(SemanticError::type_mismatch(
                            left.kind,
                            right.kind,
                            infix.position,
                        ))
                    }
                }
                _ => Err(SemanticError::operator_not_supported(
                    infix.operator.clone(),
                    right.kind,
                    infix.position,
                )),
            },
            TokenKind::Minus | TokenKind::Asterisk | TokenKind::Slash | TokenKind::Percent => {
                if left.kind != DataTypeKind::Number {
                    Err(SemanticError::operator_not_supported(
                        infix.operator.clone(),
                        left.kind,
                        infix.position,
                    ))
                } else if left.kind != right.kind {
                    Err(SemanticError::type_mismatch(
                        left.kind,
                        right.kind,
                        infix.position,
                    ))
                } else {
                    Ok(DataType::new(DataTypeKind::Number, infix.position))
                }
            }
            TokenKind::EQ
            | TokenKind::NEQ
            | TokenKind::LT
            | TokenKind::LTE
            | TokenKind::GT
            | TokenKind::GTE => {
                if left.kind != right.kind {
                    Err(SemanticError::type_mismatch(
                        left.kind,
                        right.kind,
                        infix.position,
                    ))
                } else {
                    Ok(DataType::new(DataTypeKind::Boolean, infix.position))
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn typeof_literal(
        &mut self,
        literal: &Literal,
        provided_type: Option<DataTypeKind>,
    ) -> SemanticResult<DataType> {
        Ok(match literal {
            Literal::Identifier(identifier) => self.typeof_identifier_literal(identifier)?,
            Literal::NumberLiteral(literal) => {
                DataType::new(DataTypeKind::Number, literal.position)
            }
            Literal::StringLiteral(literal) => {
                DataType::new(DataTypeKind::String, literal.position)
            }
            Literal::BooleanLiteral(literal) => {
                DataType::new(DataTypeKind::Boolean, literal.position)
            }
            Literal::ArrayLiteral(literal) => self.typeof_array_literal(literal, provided_type)?,
            _ => unimplemented!(),
        })
    }

    fn typeof_identifier_literal(&self, identifier: &Identifier) -> SemanticResult<DataType> {
        Ok(self
            .symbol_table
            .variable(&identifier.value)
            .ok_or_else(|| {
                SemanticError::identifier_not_defined(identifier.value.clone(), identifier.position)
            })?
            .data_type
            .clone())
    }

    fn typeof_array_literal(
        &mut self,
        literal: &ArrayLiteral,
        provided_type: Option<DataTypeKind>,
    ) -> SemanticResult<DataType> {
        let mut data_type: Option<DataType> = None;

        for expression in &literal.elements {
            let ttype = self.typeof_expression(expression)?;

            if let Some(data_type) = data_type.clone() {
                if data_type != ttype {
                    return Err(SemanticError::type_mismatch(
                        data_type.kind,
                        ttype.kind,
                        literal.position,
                    ));
                }
            } else {
                data_type = Some(ttype);
            }
        }

        match data_type {
            Some(data_type) => Ok(DataType::new(
                DataTypeKind::Array(Box::new(data_type)),
                literal.position,
            )),
            None => provided_type
                .ok_or_else(|| SemanticError::type_annotation_needed(literal.position))
                .map(|ttype| DataType::new(ttype, literal.position)),
        }
    }

    pub fn typeof_data_type(&self, data_type: &DataType) -> SemanticResult<DataType> {
        Ok(match data_type.kind.clone() {
            DataTypeKind::Custom(identifier) => self
                .symbol_table
                .named(&identifier)
                .ok_or_else(|| {
                    SemanticError::type_alias_not_defined(identifier, data_type.position)
                })?
                .data_type
                .clone(),
            DataTypeKind::Array(data_type) => DataType::new(
                DataTypeKind::Array(Box::new(self.typeof_data_type(&data_type)?)),
                data_type.position,
            ),
            DataTypeKind::Auto
            | DataTypeKind::Unknown
            | DataTypeKind::Generic(_)
            | DataTypeKind::Fn(_) => {
                unimplemented!()
            }
            _ => data_type.clone(),
        })
    }
}

#[cfg(test)]
mod type_tests {
    use super::*;
    use crate::{symbol_entry, symbol_table};
    use parser::ast::{
        DataType, DataTypeKind, Expression, Identifier, InfixExpression, Literal, NumberLiteral,
        Position, ReturnStatement, Statement, StringLiteral,
    };

    #[test]
    fn test_typeof_block_expression() {
        let parent = {
            let mut symbol_table = SymbolTable::new(None);
            symbol_table
                .insert("x", symbol_entry!(Number, Variable))
                .unwrap();
            symbol_table
        };

        let symbol_table = SymbolTable::new(Some(parent));

        let expression = Expression::BlockExpression(BlockExpression {
            statements: vec![Statement::ReturnStatement(ReturnStatement {
                value: Expression::InfixExpression(InfixExpression {
                    left: Box::new(Expression::Literal(Literal::Identifier(Identifier {
                        value: String::from("x"),
                        position: Position::default(),
                    }))),
                    operator: TokenKind::Plus,
                    right: Box::new(Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                        value: 1.0,
                        position: Position::default(),
                    }))),
                    position: Position::default(),
                }),
                position: Position::default(),
            })],
            position: Position::default(),
        });

        let ttype = Analyzer::new_with_symbol_table(Program::new(), symbol_table)
            .typeof_expression(&expression)
            .unwrap();

        assert_eq!(
            ttype,
            DataType::new(DataTypeKind::Number, Position::default())
        );
    }

    #[test]
    fn test_typeof_infix_expression() {
        let expression = Expression::InfixExpression(InfixExpression {
            left: Box::new(Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                value: 1.0,
                position: Position::default(),
            }))),
            operator: TokenKind::Plus,
            right: Box::new(Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                value: 2.0,
                position: Position::default(),
            }))),
            position: Position::default(),
        });

        let ttype = Analyzer::new(Program::new())
            .typeof_expression(&expression)
            .unwrap();

        assert_eq!(
            ttype,
            DataType::new(DataTypeKind::Number, Position::default())
        );
    }

    #[test]
    fn test_typeof_literal() {
        let expression = Expression::Literal(Literal::StringLiteral(StringLiteral {
            value: String::from("x"),
            position: Position::default(),
        }));

        let ttype = Analyzer::new(Program::new())
            .typeof_expression(&expression)
            .unwrap();

        assert_eq!(
            ttype,
            DataType::new(DataTypeKind::String, Position::default())
        );
    }

    #[test]
    fn test_typeof_literal_2() {
        let symbol_table = symbol_table! {
            x => Variable, Number;
        };

        let expression = Expression::Literal(Literal::Identifier(Identifier {
            value: String::from("x"),
            position: Position::default(),
        }));

        let ttype = Analyzer::new_with_symbol_table(Program::new(), symbol_table)
            .typeof_expression(&expression)
            .unwrap();

        assert_eq!(
            ttype,
            DataType::new(DataTypeKind::Number, Position::default())
        );
    }

    #[test]
    fn test_typeof_array_literal() {
        let expression = Expression::Literal(Literal::ArrayLiteral(ArrayLiteral {
            elements: vec![
                Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                    value: 1.0,
                    position: Position::default(),
                })),
                Expression::Literal(Literal::NumberLiteral(NumberLiteral {
                    value: 2.0,
                    position: Position::default(),
                })),
            ],
            position: Position::default(),
        }));

        let ttype = Analyzer::new(Program::new())
            .typeof_expression(&expression)
            .unwrap();

        assert_eq!(
            ttype,
            DataType::new(
                DataTypeKind::Array(Box::new(DataType::new(
                    DataTypeKind::Number,
                    Position::default()
                ))),
                Position::default()
            )
        );
    }

    #[test]
    fn test_typeof_data_type() {
        let symbol_table = symbol_table! {
            x => Variable, Number;
            X => Named, String;
        };

        let data_type = DataType::new(DataTypeKind::Custom(String::from("X")), Position::default());

        let ttype = Analyzer::new_with_symbol_table(Program::new(), symbol_table)
            .typeof_data_type(&data_type)
            .unwrap();

        assert_eq!(
            ttype,
            DataType::new(DataTypeKind::String, Position::default())
        );
    }
}
