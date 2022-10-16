/// AST Node structures and enumerations such as expression, statements, etc are implemented in this module.
pub mod ast;
/// The error module contains the error handling for the language.
pub mod error;
/// **Parses the input string into an AST.**
///
/// for example:
/// ```rust
/// use sntk_core::parser::parser::*;
///
/// let parsed = Parser::from(r#"type X<T, U> = fn(T, U[]) -> object T: U;"#).parse_program();
/// println!("{parsed:#?}");
/// ```
pub mod parser;
