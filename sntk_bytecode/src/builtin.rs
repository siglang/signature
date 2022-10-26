use crate::stack::*;

pub fn get_builtin(name: String) -> Option<impl FnOnce(Vec<Value>) -> Value> {
    match name.clone().as_str() {
        "print" => Some(Print::call_return()),
        "foo" => Some(Foo::call_return()),
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

        Value::None
    }
}

pub struct Foo;

impl BuiltIn for Foo {
    fn call(arguments: Vec<Value>) -> Value {
        println!("Foo called with {} arguments", arguments.len());

        Value::LiteralValue(LiteralValue::Boolean(true))
    }
}
