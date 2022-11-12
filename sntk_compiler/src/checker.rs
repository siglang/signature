use crate::{compiler::CompileResult, type_error, EXPECTED_DATA_TYPE, UNDEFINED_IDENTIFIER, UNKNOWN_ARRAY_TYPE};
use sntk_core::parser::ast::{DataType, Position};
use sntk_ir::instruction::{Identifier, Instruction, InstructionType, IrExpression, LiteralValue};
use sntk_proc::with_position;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct IdentifierTypes {
    pub types: HashMap<Identifier, DataType>,
    pub parent: Option<Box<IdentifierTypes>>,
}

impl IdentifierTypes {
    #[inline]
    pub fn new(parent: Option<&IdentifierTypes>) -> Self {
        Self {
            types: HashMap::new(),
            parent: parent.map(|parent| Box::new(parent.clone())),
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

    pub fn set(&mut self, name: &Identifier, value: &DataType) {
        self.types.insert(name.to_string(), value.clone());
    }
}

#[with_position]
pub fn get_type_from_ir_expression(expression: &IrExpression, types: &IdentifierTypes, data_type: Option<&DataType>) -> CompileResult<DataType> {
    match expression.clone() {
        IrExpression::Identifier(identifier) => match types.get(&identifier) {
            Some(data_type) => Ok(data_type),
            None => Err(type_error! { UNDEFINED_IDENTIFIER; identifier; &position }),
        },
        IrExpression::Literal(literal) => get_type_from_literal_value(&literal, types, data_type, &position),
        IrExpression::Block(block) => get_type_from_ir_expression(
            match block.last() {
                Some(instruction) => match instruction.instruction {
                    InstructionType::Return(ref expression) | InstructionType::StoreName(_, ref expression) => expression,
                    _ => return Ok(DataType::Boolean),
                },
                None => return Ok(DataType::Boolean),
            },
            types,
            data_type,
            &position,
        ),
        IrExpression::If(_, _, _) => todo!(),
        IrExpression::Call(_, _) => todo!(),
        IrExpression::Index(_, _) => todo!(),
        IrExpression::Prefix(_, _) => todo!(),
        IrExpression::Infix(_, _, _) => todo!(),
    }
}

#[with_position]
pub fn get_type_from_literal_value(literal: &LiteralValue, types: &IdentifierTypes, data_type: Option<&DataType>) -> CompileResult<DataType> {
    match literal {
        LiteralValue::Number(_) => Ok(DataType::Number),
        LiteralValue::String(_) => Ok(DataType::String),
        LiteralValue::Boolean(_) => Ok(DataType::Boolean),
        LiteralValue::Array(elements) => {
            let mut element_type = DataType::Unknown;

            for element in elements {
                let data_type = get_type_from_ir_expression(&element, types, data_type, position)?;

                if element_type == DataType::Unknown {
                    element_type = data_type;
                } else if element_type != data_type {
                    return Err(type_error! { EXPECTED_DATA_TYPE; element_type, data_type; &position });
                }
            }

            if let Some(data_type) = data_type {
                if element_type == DataType::Unknown {
                    element_type = match data_type {
                        DataType::Array(data_type) => *data_type.clone(),
                        _ => unreachable!(),
                    };
                }

                if *data_type != DataType::Array(Box::new(element_type.clone())) {
                    return Err(type_error! { EXPECTED_DATA_TYPE; data_type, DataType::Array(Box::new(element_type.clone())); &position });
                }
            } else if element_type == DataType::Unknown {
                return Err(type_error! { UNKNOWN_ARRAY_TYPE; ; &position });
            }

            Ok(DataType::Array(Box::new(element_type)))
        }
        LiteralValue::Function(..) => unimplemented!(),
    }
}

#[with_position]
pub fn get_type_from_instruction(instruction: &Instruction, types: &IdentifierTypes, data_type: Option<&DataType>) -> CompileResult<DataType> {
    Ok(match instruction.instruction.clone() {
        InstructionType::StoreName(_, expression) | InstructionType::Return(expression) | InstructionType::Expression(expression) => {
            get_type_from_ir_expression(&expression, types, data_type, position)?
        }
    })
}
