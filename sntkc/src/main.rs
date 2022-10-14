use sntk_core::parser::parser::*;

fn main() {
    println!("{:#?}", Parser::from("let x: T<U[]> = 10; return (3); 2;").parse_program());
}
