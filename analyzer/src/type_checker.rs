use crate::{symbol_table::SymbolTable, SemanticError, SemanticErrorKind, SemanticResult};
use parser::ast::{DataType, DataTypeKind, Expression, Literal};

#[derive(Debug)]
pub struct TypeChecker(pub SymbolTable);

impl TypeChecker {
    #[allow(unused_variables)]
    pub fn typeof_expression(&self, expression: &Expression) -> SemanticResult<DataType> {
        let ttype = match expression {
            Expression::BlockExpression(block) => todo!(),
            Expression::PrefixExpression(prefix) => todo!(),
            Expression::InfixExpression(infix) => todo!(),
            Expression::IfExpression(if_expression) => todo!(),
            Expression::CallExpression(call) => todo!(),
            Expression::TypeofExpression(typeof_expression) => todo!(),
            Expression::IndexExpression(index) => todo!(),
            Expression::Literal(literal) => self.typeof_literal(literal),
        };

        ttype.map(|ttype| self.typeof_data_type(&ttype))?
    }

    pub fn typeof_literal(&self, literal: &Literal) -> SemanticResult<DataType> {
        Ok(match literal {
            Literal::Identifier(identifier) => self
                .0
                .variable(&identifier.value)
                .ok_or_else(|| {
                    SemanticError::new(
                        SemanticErrorKind::IdentifierNotDefined(identifier.value.clone()),
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
        Ok(match data_type.data_type.clone() {
            DataTypeKind::Number => DataType::new(DataTypeKind::Number, data_type.position),
            DataTypeKind::String => DataType::new(DataTypeKind::String, data_type.position),
            DataTypeKind::Boolean => DataType::new(DataTypeKind::Boolean, data_type.position),
            DataTypeKind::Custom(identifier) => self
                .0
                .lookup(&identifier)
                .ok_or_else(|| {
                    SemanticError::new(
                        SemanticErrorKind::TypeAliasNotDefined(identifier),
                        data_type.position,
                    )
                })?
                .data_type
                .clone(),
            _ => unimplemented!(),
        })
    }
}
