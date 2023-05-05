use crate::{symbol_table::SymbolTable, SemanticError, SemanticResult};
use parser::{
    ast::{DataType, DataTypeKind, Expression, InfixExpression, Literal},
    tokenizer::TokenKind,
};

#[derive(Debug)]
pub struct TypeChecker(pub SymbolTable);

impl TypeChecker {
    #[allow(unused_variables)]
    pub fn typeof_expression(&self, expression: &Expression) -> SemanticResult<DataType> {
        let ttype = match expression {
            Expression::BlockExpression(block) => todo!(),
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
