use crate::instruction::LiteralValue;

pub fn get_builtin_function(name: &str) -> Option<impl FnOnce(Vec<&LiteralValue>) -> LiteralValue> {
    match name {
        "println" => Some(Print::call_function()),
        _ => None,
    }
}

pub trait BuiltIn {
    fn call(arguments: Vec<&LiteralValue>) -> LiteralValue;
    #[inline]
    #[allow(clippy::type_complexity)]
    fn call_function() -> Box<dyn FnOnce(Vec<&LiteralValue>) -> LiteralValue> {
        Box::new(|arguments| Self::call(arguments))
    }
}

pub struct Print;

impl BuiltIn for Print {
    fn call(arguments: Vec<&LiteralValue>) -> LiteralValue {
        println!(
            "{}",
            arguments
                .iter()
                .map(|argument| format!("{}", argument))
                .collect::<Vec<String>>()
                .join(" ")
        );

        LiteralValue::Boolean(true)
    }
}
