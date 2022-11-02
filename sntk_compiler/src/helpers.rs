use crate::{
    compiler::{CompileResult, Compiler, CompilerTrait},
    error::{CompileError, TypeError, EXPECTED_DATA_TYPE},
    tc::{Type, TypeTrait},
    type_error,
};
use sntk_core::parser::ast::{
    ArrayLiteral, BooleanLiteral, DataType, Expression, FunctionLiteral, NumberLiteral, Position, Program, Statement, StringLiteral,
};
use sntk_ir::{
    code::Block,
    value::{LiteralValue, Value},
};

pub fn literal_value(expression: Expression) -> CompileResult<Value> {
    Ok(match expression {
        Expression::NumberLiteral(NumberLiteral { value, .. }) => Value::LiteralValue(LiteralValue::Number(value)),
        Expression::BooleanLiteral(BooleanLiteral { value, .. }) => Value::LiteralValue(LiteralValue::Boolean(value)),
        Expression::StringLiteral(StringLiteral { value, .. }) => Value::LiteralValue(LiteralValue::String(value)),
        Expression::ArrayLiteral(ArrayLiteral { elements, .. }) => Value::LiteralValue(LiteralValue::Array(
            elements
                .into_iter()
                .map(|element| literal_value(element))
                .collect::<CompileResult<Vec<Value>>>()?,
        )),
        Expression::FunctionLiteral(FunctionLiteral {
            parameters, body, position, ..
        }) => {
            let mut statments = Vec::new();

            for statment in body.statements.clone() {
                if let Statement::ReturnStatement(_) = statment {
                    statments.push(statment);
                    break;
                } else {
                    statments.push(statment);
                }
            }

            Value::LiteralValue(LiteralValue::Function {
                parameters: parameters.iter().map(|p| (p.clone().0.value, p.clone().1)).collect(),
                body: compile_block(statments, &position)?.0,
            })
        }
        value => panic!("Unexpected value: {:?}", value),
    })
}

#[inline]
pub fn compile_block(statements: Vec<Statement>, position: &Position) -> CompileResult<(Block, DataType)> {
    let block = Block(Compiler::new(Program::new(statements)).compile_program()?.instructions);

    Ok((block.clone(), Type::get_data_type_from_instruction(block.0, position)?))
}

#[inline(always)]
pub fn type_checked_array(expression: &ArrayLiteral, data_type: Option<DataType>) -> CompileResult<Value> {
    let ArrayLiteral { elements, position } = expression;

    let array = literal_value(Expression::ArrayLiteral(expression.clone()))?;
    let array_type = Type::get_data_type(&array, position)?;

    match &array_type {
        DataType::Array(array_type) => {
            for element in elements.clone() {
                let element_type = Type::get_data_type_from_expression(&element, position)?;

                if !Type(*array_type.clone()).eq_from_type(&Type(element_type.clone())) {
                    return Err(type_error! { EXPECTED_DATA_TYPE; array_type, element_type; position.clone(); });
                }
            }
        }
        r#type => {
            return Err(type_error! { EXPECTED_DATA_TYPE; match data_type {
                Some(data_type) => data_type,
                None => DataType::Array(Box::new(DataType::Unknown)),
            }, r#type; position.clone(); })
        }
    }

    let data_type = match data_type {
        Some(data_type) => data_type,
        None => Type::get_data_type_from_expression(&Expression::ArrayLiteral(expression.clone()), position)?,
    };

    if !Type(data_type.clone()).eq_from_type(&Type(array_type.clone())) {
        return Err(type_error! { EXPECTED_DATA_TYPE; array_type, data_type; position.clone(); });
    }

    Ok(array)
}

#[inline(always)]
#[allow(unused_variables)]
pub fn type_checked_function(expression: &FunctionLiteral, data_type: Option<DataType>) -> CompileResult<Value> {
    let FunctionLiteral {
        generics,
        parameters,
        return_type,
        body,
        position,
    } = expression;

    let function = literal_value(Expression::FunctionLiteral(expression.clone()))?;
    let function_type = Type::get_data_type(&function, position)?;

    println!("function_type: {:?}", function_type);

    Ok(function)
}
