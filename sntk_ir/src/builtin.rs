use crate::instruction::LiteralValue;

trait BuiltIn {
    fn call(arguments: Vec<&LiteralValue>) -> LiteralValue;
}

struct Print;
impl BuiltIn for Print {
    fn call(arguments: Vec<&LiteralValue>) -> LiteralValue {
        let arguments = arguments
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(" ");

        println!("{arguments}");

        LiteralValue::Boolean(true)
    }
}

#[allow(clippy::type_complexity)]
#[inline]
fn boxed_call<F>() -> Box<dyn FnOnce(Vec<&LiteralValue>) -> LiteralValue>
where
    F: BuiltIn + ?Sized + 'static,
{
    Box::new(|arguments| <F as BuiltIn>::call(arguments))
}

pub fn builtin_function(name: &str) -> Option<impl FnOnce(Vec<&LiteralValue>) -> LiteralValue> {
    match name {
        "println" => Some(boxed_call::<Print>()),
        _ => None,
    }
}
