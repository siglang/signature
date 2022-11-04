use crate::{
    error::{IrRuntime, POP_EMPTY_STACK},
    runtime_error,
    value::Value,
};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Stack(Vec<Value>);

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Environment {
    pub values: HashMap<String, Value>,
    pub parent: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_with_parent(parent: Environment) -> Self {
        Self {
            values: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        match self.values.get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.get(name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }
}

pub trait StackTrait {
    fn new() -> Self;
    fn push(&mut self, value: Value);
    fn pop(&mut self) -> Value;
    fn pop_option(&mut self) -> Option<Value>;
    fn is_empty(&self) -> bool;
}

impl StackTrait for Stack {
    fn new() -> Self {
        Stack(Vec::new())
    }

    fn push(&mut self, value: Value) {
        self.0.push(value);
    }

    fn pop(&mut self) -> Value {
        self.0.pop().unwrap_or_else(|| runtime_error!(@stack self; POP_EMPTY_STACK;))
    }

    fn pop_option(&mut self) -> Option<Value> {
        self.0.pop()
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}
