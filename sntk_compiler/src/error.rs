use sntk_core::parser::error::ParsingError;

#[derive(Debug, Clone)]
pub struct CompileError(pub Vec<ParsingError>);

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut messages = String::new();

        for ParsingError { message, position } in &self.0 {
            messages.push_str(&format!("Parsing error: {} at line {}, column {}\n", message, position.0, position.1));
        }

        write!(f, "{}", messages.trim_end())
    }
}
