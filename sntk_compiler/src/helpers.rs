use crate::{
    compiler::{CompileResult, Compiler, CompilerTrait},
    error::{CompileError, TypeError, EXPECTED_DATA_TYPE},
    tc::{expand_array_type, expand_function_type, SharedTypeEnvironment, Type, TypeEnvironment, TypeTrait},
    type_error,
};
use sntk_core::parser::ast::{
    ArrayLiteral, BooleanLiteral, DataType, Expression, FunctionLiteral, NumberLiteral, Position, Program, Statement, StringLiteral,
};
use sntk_ir::{
    code::{Block, Instruction},
    value::{LiteralValue, Value},
};
use std::{cell::RefCell, rc::Rc};

pub fn literal_value(expression: Expression, type_environment: SharedTypeEnvironment) -> CompileResult<Value> {
    Ok(match expression {
        Expression::NumberLiteral(NumberLiteral { value, .. }) => Value::LiteralValue(LiteralValue::Number(value)),
        Expression::BooleanLiteral(BooleanLiteral { value, .. }) => Value::LiteralValue(LiteralValue::Boolean(value)),
        Expression::StringLiteral(StringLiteral { value, .. }) => Value::LiteralValue(LiteralValue::String(value)),
        Expression::ArrayLiteral(ArrayLiteral { elements, .. }) => Value::LiteralValue(LiteralValue::Array(
            elements
                .into_iter()
                .map(|x| literal_value(x, Rc::clone(&type_environment)))
                .collect::<CompileResult<Vec<Value>>>()?,
        )),
        Expression::FunctionLiteral(FunctionLiteral {
            parameters,
            body,
            return_type,
            position,
            ..
        }) => {
            let mut statments = Vec::new();

            for statment in body.statements {
                if let Statement::ReturnStatement(_) = statment {
                    statments.push(statment);
                    break;
                } else {
                    statments.push(statment);
                }
            }

            Value::LiteralValue(LiteralValue::Function(
                parameters
                    .iter()
                    .map(|(identifier, parameters)| {
                        type_environment.borrow_mut().set(identifier.value.clone(), parameters.clone());
                        (identifier.value.clone(), parameters.clone())
                    })
                    .collect(),
                compile_block(statments, &position, type_environment)?.0,
                return_type,
            ))
        }
        Expression::Identifier(identifier) => Value::Identifier(identifier.value),
        value => unimplemented!("{:?}", value),
    })
}

#[inline]
pub fn compile_block(statements: Vec<Statement>, position: &Position, type_environment: SharedTypeEnvironment) -> CompileResult<(Block, DataType)> {
    let block = Block(Compiler::new(Program::new(statements)).compile_program()?.instructions);

    Ok((
        block.clone(),
        Type::get_data_type_from_instruction(
            block.0,
            position,
            Rc::new(RefCell::new(TypeEnvironment::new_with_parent(type_environment.clone()))),
        )
        .transpose()?
        .unwrap_or(DataType::Void),
    ))
}

#[inline(always)]
pub fn type_checked_array(expression: &ArrayLiteral, data_type: Option<&DataType>, type_environment: SharedTypeEnvironment) -> CompileResult<Value> {
    let array = literal_value(Expression::ArrayLiteral(expression.clone()), Rc::clone(&type_environment))?;
    let array_type = match literal_value(Expression::ArrayLiteral(expression.clone()), Rc::clone(&type_environment))? {
        Value::LiteralValue(LiteralValue::Array(array)) => expand_array_type(&array, &expression.position, data_type, Rc::clone(&type_environment))?,
        _ => unreachable!(),
    };

    let data_type = match data_type {
        Some(data_type) => data_type.clone(),
        None => Type::get_data_type_from_expression(
            &Expression::ArrayLiteral(expression.clone()),
            &expression.position,
            Rc::clone(&type_environment),
        )?,
    };

    if !Type(data_type.clone()).eq_from_type(&array_type) {
        return Err(type_error! { EXPECTED_DATA_TYPE; array_type.0, data_type; expression.position.clone(); });
    }

    Ok(array)
}

#[inline(always)]
pub fn type_checked_function(
    expression: &FunctionLiteral,
    data_type: Option<&DataType>,
    type_environment: SharedTypeEnvironment,
) -> CompileResult<Value> {
    let function = literal_value(Expression::FunctionLiteral(expression.clone()), Rc::clone(&type_environment))?;
    let function_type = match literal_value(Expression::FunctionLiteral(expression.clone()), Rc::clone(&type_environment))? {
        Value::LiteralValue(LiteralValue::Function(parameters, body, return_type)) => expand_function_type(
            &parameters,
            &body,
            &return_type,
            &expression.position,
            data_type,
            Rc::clone(&type_environment),
        )?,
        _ => unreachable!(),
    };

    let data_type = match data_type {
        Some(data_type) => data_type.clone(),
        None => Type::get_data_type_from_expression(
            &Expression::FunctionLiteral(expression.clone()),
            &expression.position,
            Rc::clone(&type_environment),
        )?,
    };

    if !Type(data_type.clone()).eq_from_type(&function_type) {
        return Err(type_error! { EXPECTED_DATA_TYPE; function_type.0, data_type; expression.position.clone(); });
    }

    Ok(function)
}

#[inline]
pub fn last_instruction_data_type(
    instruction: &Vec<Instruction>,
    position: &Position,
    type_environment: SharedTypeEnvironment,
) -> CompileResult<Option<DataType>> {
    Type::get_data_type_from_instruction(vec![instruction[instruction.len() - 1].clone()], position, type_environment).transpose()
}
