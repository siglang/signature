use crate::instruction::{Identifier, LiteralValue};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct IrEnvironment {
    pub store: HashMap<Identifier, LiteralValue>,
    pub parent: Option<Box<IrEnvironment>>,
}

impl IrEnvironment {
    #[inline]
    pub fn new(parent: Option<IrEnvironment>) -> Self {
        Self {
            store: HashMap::new(),
            parent: parent.map(Box::new),
        }
    }

    pub fn get(&self, name: &Identifier) -> Option<LiteralValue> {
        match self.store.get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.get(name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: Identifier, value: LiteralValue) {
        self.store.insert(name, value);
    }
}
