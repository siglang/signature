use crate::stack::*;

pub fn get_builtin(name: String) -> Option<impl FnOnce(Vec<Value>) -> Value> {
    match name.clone().as_str() {
        "print" => Some(Print::call),
        _ => None,
    }
}

pub trait BuiltIn {
    fn call(arguments: Vec<Value>) -> Value;
}

pub struct Print;

impl BuiltIn for Print {
    fn call(arguments: Vec<Value>) -> Value {
        println!("{}", arguments.iter().map(|x| format!("{} ", x)).collect::<String>());

        Value::None
    }
}
