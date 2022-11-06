use crate::{compiler::CompileResult, error::UNKNOWN_TYPE, type_error};
use sntk_core::parser::ast::{DataType, Position};
use sntk_ir::instruction::{Identifier, Instruction, InstructionType, IrExpression, LiteralValue};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct IdentifierTypes {
    pub types: HashMap<Identifier, DataType>,
    pub parent: Option<Box<IdentifierTypes>>,
}

impl IdentifierTypes {
    pub fn new(parent: Option<IdentifierTypes>) -> Self {
        Self {
            types: HashMap::new(),
            parent: parent.map(Box::new),
        }
    }

    pub fn get(&self, name: &Identifier) -> Option<DataType> {
        match self.types.get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.get(name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: Identifier, value: DataType) {
        self.types.insert(name, value);
    }
}

pub fn get_type_from_ir_expression(expression: &IrExpression, types: &IdentifierTypes, position: &Position) -> CompileResult<DataType> {
    match expression.clone() {
        IrExpression::Identifier(identifier) => match types.get(&identifier) {
            Some(data_type) => Ok(data_type),
            None => Err(type_error! { UNKNOWN_TYPE; identifier; position }),
        },
        IrExpression::Literal(literal) => match literal {
            LiteralValue::Number(_) => Ok(DataType::Number),
            LiteralValue::String(_) => Ok(DataType::String),
            LiteralValue::Boolean(_) => Ok(DataType::Boolean),
            LiteralValue::Array(_) | LiteralValue::Function(..) => unimplemented!(),
        },
        IrExpression::Block(_) => todo!(),
        IrExpression::If(_, _, _) => todo!(),
        IrExpression::Call(_, _) => todo!(),
        IrExpression::Index(_, _) => todo!(),
        IrExpression::Prefix(_, _) => todo!(),
        IrExpression::Infix(_, _, _) => todo!(),
    }
}

#[inline(always)]
pub fn get_type_from_literal_value(literal: &LiteralValue, types: &IdentifierTypes, position: &Position) -> CompileResult<DataType> {
    get_type_from_ir_expression(&IrExpression::Literal(literal.clone()), types, position)
}

pub fn get_type_from_instruction(instruction: &Instruction) -> DataType {
    match instruction.instruction {
        InstructionType::StoreName(_, _) => todo!(),
        InstructionType::Return(_) => todo!(),
        InstructionType::Expression(_) => todo!(),
    }
}
