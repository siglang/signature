use std::{collections::HashMap, *};

/// **The stack on which the interpreter is based.**
#[derive(Debug, PartialEq, Clone, Default)]
pub struct Stack(Vec<Value>);

/// **The value of a stack.**
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    LiteralValue(LiteralValue),
    Identifier(String),
    Return(Box<Value>),
    None,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Value::LiteralValue(literal_value) => write!(f, "{}", literal_value),
            Value::Identifier(name) => write!(f, "identifier ({})", name),
            Value::Return(value) => write!(f, "return ({})", value),
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
