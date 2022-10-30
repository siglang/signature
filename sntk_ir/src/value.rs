use crate::code::Block;
use std::fmt::Write;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    LiteralValue(LiteralValue),
    Identifier(String),
    Return(Box<Value>),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::LiteralValue(literal_value) => write!(f, "{}", literal_value),
            Value::Identifier(name) => write!(f, "identifier ({})", name),
            Value::Return(value) => write!(f, "return ({})", value),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralValue {
    Number(f64),
    Boolean(bool),
    String(String),
    Array(Vec<Value>),
    // Record(HashMap<String, Value>),
    Function { parameters: Vec<String>, body: Block },
}

impl std::fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LiteralValue::Number(number) => write!(f, "{}", number),
            LiteralValue::Boolean(boolean) => write!(f, "{}", boolean),
            LiteralValue::String(string) => write!(f, "{}", string),
            LiteralValue::Array(array) => {
                if array.is_empty() {
                    write!(f, "[ ]")
                } else {
                    let mut string = String::new();

                    for value in array {
                        string.write_fmt(format_args!("{}, ", value))?;
                    }

                    write!(f, "[{}]", string.trim_end_matches(", "))
                }
            }
            // LiteralValue::Record(record) => {
            //     if record.is_empty() {
            //         write!(f, "{{ }}")
            //     } else {
            //         let mut string = String::new();

            //         for (key, value) in record {
            //             string.write_fmt(format_args!("{}: {}, ", key, value))?;
            //         }

            //         write!(f, "record {{{}}}", string.trim_end_matches(", "))
            //     }
            // }
            LiteralValue::Function { parameters, .. } => {
                let mut string = String::new();

                for parameter in parameters {
                    string.write_fmt(format_args!("{}, ", parameter))?;
                }

                write!(f, "fn({})", string.trim_end_matches(", "))
            }
        }
    }
}

impl TryFrom<Value> for LiteralValue {
    type Error = String;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::LiteralValue(literal_value) => Ok(literal_value),
            _ => Err(format!("Not a literal value: {}", value)),
        }
    }
}
