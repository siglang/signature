use super::object::Object;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    pub store: HashMap<String, Object>,
    pub parent: Option<Box<Environment>>,
}

impl Environment {
    pub fn new(parent: Option<Environment>) -> Self {
        Self {
            store: HashMap::new(),
            parent: parent.map(Box::new),
        }
    }

    pub fn insert(&mut self, name: &str, object: Object) -> Option<()> {
        if self.store.contains_key(name) {
            return None;
        }

        self.store.insert(name.to_string(), object);
        Some(())
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(object) => Some(object.clone()),
            None => match &self.parent {
                Some(parent) => parent.get(name),
                None => None,
            },
        }
    }
}
