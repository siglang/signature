use sntk_core::parser::parser::*;

fn main() {
    let parsed = Parser::from("let x: T<U> = 10; 3;").parse_program();
    // TODO: not working. ^ "expected next token to be an expression, got Semicolon instead"

    println!("{parsed:#?}");
}
