use crate::{analyzer::Analyzer, symbol_table::SymbolTable, SemanticError, SemanticResult};
use parser::{
    ast::{
        ArrayLiteral, BlockExpression, DataType, DataTypeKind, Expression, Identifier,
        InfixExpression, Literal, PrefixExpression,
    },
    tokenizer::TokenKind,
};

#[derive(Debug)]
pub struct TypeChecker {
    pub symbol_table: SymbolTable,
    /// Used when providing a type externally, e.g. `let arr: number[] = [];`
    pub provided_type: Option<DataTypeKind>,
}

impl TypeChecker {
    /// Creates a new type checker with the given symbol table.
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self {
            symbol_table,
            provided_type: None,
        }
    }

    /// Creates a new type checker with the given symbol table and external type.
    pub fn new_with_provided_type(symbol_table: SymbolTable, provided_type: DataTypeKind) -> Self {
        Self {
            symbol_table,
            provided_type: Some(provided_type),
        }
    }

    #[allow(unused_variables)]
    pub fn typeof_expression(&self, expression: &Expression) -> SemanticResult<DataType> {
        let ttype = match expression {
            Expression::BlockExpression(block) => self.typeof_block_expression(block),
            Expression::PrefixExpression(prefix) => self.typeof_prefix_expression(prefix),
            Expression::InfixExpression(infix) => self.typeof_infix_expression(infix),
            Expression::IfExpression(if_expression) => todo!(),
            Expression::CallExpression(call) => todo!(),
            Expression::TypeofExpression(typeof_expression) => todo!(),
            Expression::IndexExpression(index) => todo!(),
            Expression::Literal(literal) => self.typeof_literal(literal),
        };

        ttype.map(|ttype| self.typeof_data_type(&ttype))?
    }

    fn typeof_block_expression(&self, block: &BlockExpression) -> SemanticResult<DataType> {
        let symbol_table = SymbolTable::new(Some(self.symbol_table.clone()));
        let analyzer =
            Analyzer::new_with_symbol_table(block.statements.clone(), symbol_table).analyze()?;

        Ok(DataType::new(analyzer, block.position))
    }

    fn typeof_prefix_expression(&self, prefix: &PrefixExpression) -> SemanticResult<DataType> {
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

    fn typeof_infix_expression(&self, infix: &InfixExpression) -> SemanticResult<DataType> {
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

    pub fn typeof_literal(&self, literal: &Literal) -> SemanticResult<DataType> {
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
            Literal::ArrayLiteral(literal) => self.typeof_array_literal(literal)?,
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

    fn typeof_array_literal(&self, literal: &ArrayLiteral) -> SemanticResult<DataType> {
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
            None => self
                .provided_type
                .clone()
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
mod tests {
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

        let ttype = TypeChecker::new(symbol_table)
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

        let ttype = TypeChecker::new(SymbolTable::new(None))
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

        let ttype = TypeChecker::new(SymbolTable::new(None))
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

        let ttype = TypeChecker::new(symbol_table)
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

        let ttype = TypeChecker::new(SymbolTable::new(None))
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

        let ttype = TypeChecker::new(symbol_table)
            .typeof_data_type(&data_type)
            .unwrap();

        assert_eq!(
            ttype,
            DataType::new(DataTypeKind::String, Position::default())
        );
    }
}
