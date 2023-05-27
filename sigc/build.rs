use serde_json::Value;
use std::fs;

fn main() {
    let json = fs::read_to_string("error_messages/en.json").unwrap();
    let json = serde_json::from_str(&json).unwrap();

    match json {
        Value::Object(obj) => {
            for (key, value) in obj {
                set_env(&key, &value)
            }
        }
        _ => panic!(),
    }
}

fn set_env(key: &str, value: &Value) {
    match value {
        Value::Object(obj) => {
            for (k, v) in obj {
                set_env(format!("{}.{}", key, k).as_str(), v)
            }
        }
        Value::String(s) => {
            println!("cargo:rustc-env={}={}", key, s);
        }
        Value::Number(n) => {
            println!("cargo:rustc-env={}={}", key, n);
        }
        _ => {}
    }
}
