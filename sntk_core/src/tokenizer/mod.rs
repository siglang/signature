/// **Lexer receives the source code in the from of a string and returns tokenized data.**
///
/// for example:
/// ```rust
/// use sntk_core::tokenizer::{token::*, lexer::*};
///
/// let mut lexer = Lexer::new(r#"let x_32z = y != "Hello, World\n";"#);
/// let mut token = Token::new(Tokens::ILLEGAL(String::new()), (0, 0));
///
/// while token.token_type != Tokens::EOF {
///     token = lexer.next_token();
///     println!("{}", token);
/// }
/// ```
pub mod lexer;
/// An enumeration of tokens, which are the basic units of that make up a language.
/// 
/// the `Token` structure containing `Tokens` and position (line, column) of the token.
pub mod token;
