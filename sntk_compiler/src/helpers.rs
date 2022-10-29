use sntk_core::parser::ast::{ArrayLiteral, BooleanLiteral, Expression, NumberLiteral, ObjectLiteral, StringLiteral};
use sntk_ir::stack::{LiteralValue, Value};
use std::collections::HashMap;

pub fn literal_value(expression: Expression) -> Value {
    match expression {
        Expression::NumberLiteral(NumberLiteral { value, .. }) => Value::LiteralValue(LiteralValue::Number(value)),
        Expression::BooleanLiteral(BooleanLiteral { value, .. }) => Value::LiteralValue(LiteralValue::Boolean(value)),
        Expression::StringLiteral(StringLiteral { value, .. }) => Value::LiteralValue(LiteralValue::String(value)),
        Expression::ArrayLiteral(ArrayLiteral { elements, .. }) => {
            Value::LiteralValue(LiteralValue::Array(elements.into_iter().map(literal_value).collect()))
        }
        Expression::ObjectLiteral(ObjectLiteral { pairs, .. }) => {
            let mut object = HashMap::new();

            for (key, value) in pairs {
                object.insert(key.value, literal_value(value));
            }

            Value::LiteralValue(LiteralValue::Object(object))
        }
        value => panic!("Unexpected value: {:?}", value),
    }
}
