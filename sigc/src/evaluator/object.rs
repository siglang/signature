use parser::ast::{BlockExpression, Parameter};

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub enum Object {
    Number(f64),
    String(String),
    Boolean(bool),
    Function(Vec<Parameter>, BlockExpression),
    Array(Vec<Object>),
}
