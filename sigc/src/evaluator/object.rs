use parser::ast::{BlockExpression, Parameter};
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub enum Object {
    Number(f64),
    String(String),
    Boolean(bool),
    Function(Vec<Parameter>, BlockExpression),
    Array(Vec<Object>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Number(number) => write!(f, "{number}"),
            Object::String(string) => write!(f, "{string}"),
            Object::Boolean(boolean) => write!(f, "{boolean}"),
            Object::Function(_, _) => write!(f, "Function"),
            Object::Array(_) => write!(f, "Array"),
        }
    }
}
