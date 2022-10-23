use sntk_core::parser::error::ParsingError;

#[derive(Debug, Clone)]
pub enum CompileError {
    /// The error type for the compiler.
    CompileError(String),
    /// The error type for the parser.
    ParseError(Vec<ParsingError>),
}
