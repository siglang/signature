use crate::stack::*;

pub fn get_builtin(name: String) -> Option<impl FnOnce(Vec<Value>) -> Value> {
    match name.clone().as_str() {
        "print" => Some(Print::call_return()),
        _ => None,
    }
}

pub trait BuiltIn {
    fn call(arguments: Vec<Value>) -> Value;
    fn call_return() -> Box<dyn FnOnce(Vec<Value>) -> Value> {
        Box::new(move |arguments| Self::call(arguments))
    }
}

pub struct Print;

impl BuiltIn for Print {
    fn call(arguments: Vec<Value>) -> Value {
        println!("{}", arguments.iter().map(|x| format!("{} ", x)).collect::<String>());

        Value::LiteralValue(LiteralValue::Boolean(true))
    }
}
