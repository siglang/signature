use sntk_core::parser::parser::*;

fn main() {
    let parsed = Parser::from("let x: number = 10;").parse_program();

    println!("{parsed:#?}");
}
