use sntk_core::parser::parser::*;

fn main() {
    let parsed = Parser::from("3;").parse_program();

    println!("{parsed:#?}");
}
