use crate::{
    error::{IrRuntime, POP_EMPTY_STACK},
    runtime_error,
};
use std::{
    collections::HashMap,
    fmt::{self, Write},
};

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Stack(Vec<Value>);

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    LiteralValue(LiteralValue),
    Identifier(String),
    Return(Box<Value>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Value::LiteralValue(literal_value) => write!(f, "{}", literal_value),
            Value::Identifier(name) => write!(f, "identifier ({})", name),
            Value::Return(value) => write!(f, "return ({})", value),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralValue {
    Number(f64),
    Boolean(bool),
    String(String),
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
}

impl fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            LiteralValue::Number(number) => write!(f, "{}", number),
            LiteralValue::Boolean(boolean) => write!(f, "{}", boolean),
            LiteralValue::String(string) => write!(f, "{}", string),
            LiteralValue::Array(array) => {
                let mut string = String::new();

                for value in array {
                    string.write_fmt(format_args!("{}, ", value))?;
                }

                write!(f, "[{}]", string.trim_end_matches(", "))
            }
            LiteralValue::Object(object) => {
                let mut string = String::new();

                for (key, value) in object {
                    string.write_fmt(format_args!("{}: {}, ", key, value))?;
                }

                write!(f, "{{ {} }}", string.trim_end_matches(", "))
            }
        }
    }
}

impl TryFrom<Value> for LiteralValue {
    type Error = String;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::LiteralValue(literal_value) => Ok(literal_value),
            _ => Err(format!("Not a literal value: {}", value)),
        }
    }
}

/// **Environment of the stack.**
#[derive(Debug, PartialEq, Clone, Default)]
pub struct Environment {
    pub values: HashMap<String, Value>,
    pub parent: Option<Box<Environment>>,
}

impl Environment {
    /// Create a new environment.
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            parent: None,
        }
    }

    /// Create a new environment with a parent.
    pub fn new_with_parent(parent: Environment) -> Self {
        Self {
            values: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    /// Get a value from the environment.
    pub fn get(&self, name: &str) -> Option<Value> {
        match self.values.get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.get(name),
                None => None,
            },
        }
    }

    /// Set a value in the environment.
    pub fn set(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }
}

/// **The trait for the stack implementation.**
pub trait StackTrait {
    fn new() -> Self;
    fn push(&mut self, value: Value);
    fn pop(&mut self) -> Value;
    fn is_empty(&self) -> bool;
}

#[rustfmt::skip]
impl StackTrait for Stack {
    fn new() -> Self { Stack(Vec::new()) }
    fn push(&mut self, value: Value) { self.0.push(value); }
    fn pop(&mut self) -> Value {
        self.0.pop().unwrap_or_else(|| runtime_error!(@stack self; POP_EMPTY_STACK;))
    }
    fn is_empty(&self) -> bool { self.0.is_empty() }
}
