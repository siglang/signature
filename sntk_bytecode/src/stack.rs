#[derive(Debug, PartialEq, Clone)]
pub struct Stack(Vec<Value>);

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Str(String),
    Identifier(String),
    None,
    // ...
}

impl Stack {
    pub fn new() -> Self {
        Stack(Vec::new())
    }

    pub fn push(&mut self, value: Value) {
        self.0.push(value);
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.0.pop()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn peek(&self) -> Option<&Value> {
        self.0.last()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}
