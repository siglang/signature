use crate::instruction::LiteralValue;

trait BuiltIn {
    fn call(arguments: Vec<&LiteralValue>) -> LiteralValue;
}

struct Print;
impl BuiltIn for Print {
    fn call(arguments: Vec<&LiteralValue>) -> LiteralValue {
        let arguments = arguments.iter().map(ToString::to_string).collect::<Vec<_>>().join(" ");

        println!("{arguments}");

        LiteralValue::Boolean(true)
    }
}

type BoxedCall = Box<dyn FnOnce(Vec<&LiteralValue>) -> LiteralValue>;

#[allow(clippy::type_complexity)]
#[inline]
fn boxed_call<F>() -> BoxedCall
where
    F: BuiltIn + 'static,
{
    Box::new(F::call)
}

pub fn builtin_function(name: &str) -> Option<BoxedCall> {
    match name {
        "println" => Some(boxed_call::<Print>()),
        _ => None,
    }
}
