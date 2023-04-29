use crate::{symbol_table::SymbolTable, SemanticResult};
use parser::ast::{DataType, DataTypeKind, Expression, Literal};

#[derive(Debug)]
pub struct TypeChecker(pub SymbolTable);

impl TypeChecker {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self(symbol_table)
    }

    pub fn typeof_expression(&self, expression: &Expression) -> SemanticResult<DataType> {
        match expression {
            Expression::Literal(literal) => self.typeof_literal(literal),
            _ => unimplemented!(),
        }
    }

    pub fn typeof_literal(&self, literal: &Literal) -> SemanticResult<DataType> {
        Ok(match literal {
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
        Ok(match data_type.data_type {
            DataTypeKind::Number => DataType::new(DataTypeKind::Number, data_type.position),
            DataTypeKind::String => DataType::new(DataTypeKind::String, data_type.position),
            DataTypeKind::Boolean => DataType::new(DataTypeKind::Boolean, data_type.position),
            _ => unimplemented!(),
        })
    }
}
