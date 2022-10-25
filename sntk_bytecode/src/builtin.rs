use crate::stack::*;

pub fn get_builtin(name: String) -> Option<impl FnOnce(Option<Vec<Value>>) -> Value> {
    match name.clone().as_str() {
        "print" => Some(Print::call),
        _ => None,
    }
}

/// Built in function based trait.
pub trait BuiltInTrait {
    fn call(arguments: Option<Vec<Value>>) -> Value;
}

pub struct Print;

impl BuiltInTrait for Print {
    fn call(arguments: Option<Vec<Value>>) -> Value {
        if let Some(arguments) = arguments {
            println!("{:?}", arguments);
        }
        Value::None
    }
}
