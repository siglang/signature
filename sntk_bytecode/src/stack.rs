use std::*;

/// **The stack on which the interpreter is based.**
#[derive(Debug, PartialEq, Clone)]
pub struct Stack(Vec<Value>);

/// **The value of a stack.**
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    LiteralValue(LiteralValue),
    Identifier(String),
    None,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Value::LiteralValue(literal_value) => write!(f, "{}", literal_value),
            Value::Identifier(name) => write!(f, "{}", name),
            Value::None => write!(f, "None"),
        }
    }
}

/// **The literal value of a stack.**
#[derive(Debug, PartialEq, Clone)]
pub enum LiteralValue {
    Number(f64),
    Boolean(bool),
    String(String),
}

impl fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            LiteralValue::Number(number) => write!(f, "{}", number),
            LiteralValue::Boolean(boolean) => write!(f, "{}", boolean),
            LiteralValue::String(string) => write!(f, "\"{}\"", string),
        }
    }
}

/// **The trait for the stack implementation.**
pub trait StackTrait {
    fn new() -> Self;
    fn push(&mut self, value: Value);
    fn pop(&mut self) -> Option<Value>;
    fn is_empty(&self) -> bool;
    fn len(&self) -> usize;
}

impl StackTrait for Stack {
    fn new() -> Self {
        Stack(Vec::new())
    }

    fn push(&mut self, value: Value) {
        self.0.push(value);
    }

    fn pop(&mut self) -> Option<Value> {
        self.0.pop()
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn len(&self) -> usize {
        self.0.len()
    }
}
