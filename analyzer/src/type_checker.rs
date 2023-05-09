use crate::{analyzer::Analyzer, symbol_table::SymbolTable, SemanticError, SemanticResult};
use parser::{
    ast::{BlockExpression, DataType, DataTypeKind, Expression, InfixExpression, Literal},
    tokenizer::TokenKind,
};

#[derive(Debug)]
pub struct TypeChecker(pub SymbolTable);

impl TypeChecker {
    #[allow(unused_variables)]
    pub fn typeof_expression(&self, expression: &Expression) -> SemanticResult<DataType> {
        let ttype = match expression {
            Expression::BlockExpression(block) => self.typeof_block_expression(block),
            Expression::PrefixExpression(prefix) => todo!(),
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
        let symbol_table = SymbolTable::new(Some(self.0.clone()));
        let analyzer =
            Analyzer::new_with_symbol_table(block.statements.clone(), symbol_table).analyze()?;

        Ok(DataType::new(analyzer, block.position))
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
            Literal::Identifier(identifier) => self
                .0
                .variable(&identifier.value)
                .ok_or_else(|| {
                    SemanticError::identifier_not_defined(
                        identifier.value.clone(),
                        identifier.position,
                    )
                })?
                .data_type
                .clone(),
            Literal::NumberLiteral(literal) => {
                DataType::new(DataTypeKind::Number, literal.position)
            }
            Literal::StringLiteral(literal) => {
                DataType::new(DataTypeKind::String, literal.position)
            }
            Literal::BooleanLiteral(literal) => {
                DataType::new(DataTypeKind::Boolean, literal.position)
            }
            _ => unimplemented!(),
        })
    }

    pub fn typeof_data_type(&self, data_type: &DataType) -> SemanticResult<DataType> {
        Ok(match data_type.kind.clone() {
            DataTypeKind::Number => DataType::new(DataTypeKind::Number, data_type.position),
            DataTypeKind::String => DataType::new(DataTypeKind::String, data_type.position),
            DataTypeKind::Boolean => DataType::new(DataTypeKind::Boolean, data_type.position),
            DataTypeKind::Custom(identifier) => self
                .0
                .named(&identifier)
                .ok_or_else(|| {
                    SemanticError::type_alias_not_defined(identifier, data_type.position)
                })?
                .data_type
                .clone(),
            _ => unimplemented!(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{symbol_entry, symbol_table};
    use parser::ast::{
        DataType, DataTypeKind, Expression, InfixExpression, Literal, NumberLiteral, Position,
        ReturnStatement, Statement, StringLiteral,
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
                    left: Box::new(Expression::Literal(Literal::Identifier(
                        parser::ast::Identifier {
                            value: String::from("x"),
                            position: Position::default(),
                        },
                    ))),
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

        let ttype = TypeChecker(symbol_table)
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

        let ttype = TypeChecker(SymbolTable::new(None))
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

        let ttype = TypeChecker(SymbolTable::new(None))
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

        let expression = Expression::Literal(Literal::Identifier(parser::ast::Identifier {
            value: String::from("x"),
            position: Position::default(),
        }));

        let ttype = TypeChecker(symbol_table)
            .typeof_expression(&expression)
            .unwrap();

        assert_eq!(
            ttype,
            DataType::new(DataTypeKind::Number, Position::default())
        );
    }

    #[test]
    fn test_typeof_data_type() {
        let symbol_table = symbol_table! {
            x => Variable, Number;
            X => Named, String;
        };

        let data_type = DataType::new(DataTypeKind::Custom(String::from("X")), Position::default());

        let ttype = TypeChecker(symbol_table)
            .typeof_data_type(&data_type)
            .unwrap();

        assert_eq!(
            ttype,
            DataType::new(DataTypeKind::String, Position::default())
        );
    }
}
