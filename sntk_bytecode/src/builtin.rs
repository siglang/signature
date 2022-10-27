use crate::stack::LiteralValue;

pub fn get_builtin(name: String) -> Option<impl FnOnce(Vec<LiteralValue>) -> LiteralValue> {
    match name.as_str() {
        "print" => Some(Print::call_return()),
        _ => None,
    }
}

pub trait BuiltIn {
    fn call(arguments: Vec<LiteralValue>) -> LiteralValue;
    fn call_return() -> Box<dyn FnOnce(Vec<LiteralValue>) -> LiteralValue> {
        Box::new(move |arguments| Self::call(arguments))
    }
}

pub struct Print;

impl BuiltIn for Print {
    fn call(arguments: Vec<LiteralValue>) -> LiteralValue {
        println!("{}", arguments.iter().map(|x| format!("{} ", x)).collect::<String>());

        LiteralValue::Boolean(true)
    }
}
