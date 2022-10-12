use sntk_core::parser::parser::*;

fn main() {
    let parsed = Parser::from("let x = 10;").parse_program();

    println!("{:#?}", parsed);
}
