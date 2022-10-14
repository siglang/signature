use sntk_core::parser::parser::*;

fn main() {
    let parsed = Parser::from("let x: T<U> = 10; return 3; 2;").parse_program();

    println!("{parsed:#?}");
}
